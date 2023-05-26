// SPDX: CC0-1.0

use anyhow::Context;
use chrono::{DateTime, Local};
use core::num::NonZeroU16;
use slope_field::{
    eval::{self, EvalErrTyp, Ident, Idents, Program},
    lex::{LexErrTyp, Lexer, SubStr, TokTyp},
    parse::{self, ParseErrTyp},
    shell::{self, Command},
    stdlib,
    stdlib::{X, Y},
    {Number, Point, Window},
};
#[cfg(not(debug_assertions))]
use std::process::Stdio;
use std::{
    fs::OpenOptions,
    io::{stdout, BufWriter, Write},
    process::{self, Child, ExitCode},
    sync::Arc,
};

// TODOO: take derivative of functions from user input?
// TODOO: use arbitrary precision float

const OUTPUT_RES: [u32; 2] = [1920, 1080];

fn output_svg_filename(now: DateTime<Local>) -> String {
    format!(
        "{}_output-{}.{}",
        env!("CARGO_PKG_NAME"),
        now.format("%Y-%m-%d_%H-%M-%S"),
        "svg"
    )
}

fn output_gnuplot_filename(now: DateTime<Local>) -> String {
    format!(
        "{}_output-{}.{}",
        env!("CARGO_PKG_NAME"),
        now.format("%Y-%m-%d_%H-%M-%S"),
        "gnuplot"
    )
}

fn output_data_filename(now: DateTime<Local>) -> String {
    format!(
        "{}_output-{}.{}",
        env!("CARGO_PKG_NAME"),
        now.format("%Y-%m-%d_%H-%M-%S"),
        "data"
    )
}

fn main() -> ExitCode {
    match try_main() {
        Ok(()) => ExitCode::SUCCESS,
        Err(err) => {
            eprintln!("unexpected error: {err}");
            let chain = err.chain();
            if chain.len() > 1 {
                eprintln!();
                eprintln!("context:");
                for it in chain.skip(1) {
                    eprintln!("  {it}");
                }
            }
            ExitCode::FAILURE
        }
    }
}

#[derive(Debug)]
struct State {
    expr: Option<Arc<String>>,
    prog: Option<Program>,
    idents: Idents,
    win: Window,
    gnuplot: Option<Child>,
}

fn try_main() -> anyhow::Result<()> {
    let mut state = State {
        expr: Some(Arc::new(String::from("x - y"))),
        prog: None,
        idents: stdlib::standard_idents(),
        win: Window {
            x: -4.0..4.0,
            y: -4.0..4.0,
            density: 20.try_into().unwrap(),
        },
        gnuplot: None,
    };

    let mut stdout = BufWriter::new(stdout());
    loop {
        if let Some(ref expr) = state.expr {
            writeln!(stdout, "dy/dx = {expr}")?;
        } else {
            writeln!(stdout, "dy/dx is not set")?;
        }

        let mut try_cmd = shell::input(&mut stdout, "> ")?;
        try_cmd.make_ascii_lowercase();
        writeln!(stdout)?;

        if let Ok(cmd) = try_cmd.parse::<Command>() {
            match cmd {
                Command::Help => {
                    for c in Command::exhaustive() {
                        writeln!(stdout, "{name}: {help}", name = c.name(), help = c.help())?;
                    }
                }

                Command::Quit => break,

                Command::SetExpr => set_expr(&mut stdout, &mut state)?,

                Command::Plot => plot_expr(&mut stdout, &mut state)?,

                Command::SetWin => set_win(&mut stdout, &mut state)?,

                Command::PrintProg => {
                    if let Some(ref prog) = state.prog {
                        shell::dump_program(&mut stdout, prog, format_args!("program"))?;
                    } else {
                        shell::prog_undefined(&mut stdout)?;
                    }
                }
            }
        } else {
            writeln!(stdout, r#"Unknown command, try "help" for help"#)?;
        }

        writeln!(stdout)?;
    }
    stdout.flush()?;
    Ok(())
}

fn set_win<W: Write>(mut out: W, state: &mut State) -> anyhow::Result<()> {
    writeln!(out, "win = {:#}", state.win)?;
    writeln!(out)?;
    writeln!(out, "note: leave blank to skip")?;

    for (name, dst) in [
        ("x min", &mut state.win.x.start),
        ("x max", &mut state.win.x.end),
        ("y min", &mut state.win.y.start),
        ("y max", &mut state.win.y.end),
    ] {
        match shell::read_fromstr::<_, Number>(
            &mut out,
            format_args!("?{name} (is {cur}) = ", cur = *dst),
            true,
        )? {
            Ok(Some(new)) => *dst = new,
            Ok(None) => {}
            Err(_) => return Ok(()),
        }
    }

    writeln!(out, "note: density must be a nonzero integer")?;
    for (name, dst) in [("density", &mut state.win.density)] {
        match shell::read_fromstr::<_, NonZeroU16>(
            &mut out,
            format_args!("?{name} (is {cur}) = ", cur = *dst),
            true,
        )? {
            Ok(Some(new)) => *dst = new,
            Ok(None) => {}
            Err(_) => return Ok(()),
        }
    }

    Ok(())
}

fn plot_expr<W: Write>(mut out: W, state: &mut State) -> anyhow::Result<()> {
    compile_expr(&mut out, state)?;

    let (expr, prog) = if let Some(ref expr) = state.expr {
        if let Some(ref prog) = state.prog {
            (expr, prog)
        } else {
            shell::prog_undefined(&mut out)?;
            return Ok(());
        }
    } else {
        shell::expr_undefined(&mut out)?;
        return Ok(());
    };

    // set up gnuplot
    if let Some(mut old_child) = state.gnuplot.take() {
        old_child
            .kill()
            .context("failed to kill previous gnuplot child")?;
    }
    let now = Local::now();
    let data_path = output_data_filename(now);
    let gnuplot_path = output_gnuplot_filename(now);
    let svg_path = output_svg_filename(now);
    let mut data = BufWriter::new(
        OpenOptions::new()
            .write(true)
            .create_new(true)
            .open(&data_path)
            .context("failed to open output data file")?,
    );
    let mut gnuplot = BufWriter::new(
        OpenOptions::new()
            .write(true)
            .create_new(true)
            .open(&gnuplot_path)
            .context("failed to open output gnuplot file")?,
    );

    // evaluate program
    let mut stack: Vec<Number> = Vec::new();
    match slope_field::draw(
        |point| {
            // define x & y
            *state.idents.get_mut(&X.into()).unwrap() = Ident::Var(Some(point.x));
            *state.idents.get_mut(&Y.into()).unwrap() = Ident::Var(Some(point.y));

            // evaluate program with given values for x and y to get slope
            eval::eval(prog, &state.idents, &mut stack)
        },
        |Point { x, y }, Point { x: dx, y: dy }| {
            writeln!(data, "{x} {y} {dx} {dy}").context("failed to write to output data file")?;
            Ok::<_, anyhow::Error>(())
        },
        &state.win,
    ) {
        Ok(Ok(step)) => {
            writeln!(out, "evaluation ok")?;

            data.flush()?;
            data.get_mut().sync_data()?;
            drop(data);

            writeln!(gnuplot, "reset")?;
            writeln!(gnuplot, "set term push")?;
            // set output info
            let [width, height] = OUTPUT_RES;
            writeln!(gnuplot, "set terminal svg size {width},{height} enhanced",)?;
            writeln!(gnuplot, "set output '{svg_path}'")?;

            // set window
            let win = &state.win;
            writeln!(
                gnuplot,
                "set xrange[{min}:{max}]",
                min = win.x.start,
                max = win.x.end
            )?;
            writeln!(
                gnuplot,
                "set yrange[{min}:{max}]",
                min = win.y.start,
                max = win.y.end
            )?;
            // (granularity determined by data)

            // configure appearence
            writeln!(gnuplot, r#"set title "{data_path}""#)?;
            writeln!(gnuplot, "set title noenhanced")?;

            writeln!(gnuplot, "set style arrow 2 nohead lc '#27422e'")?;

            writeln!(gnuplot, r#"set xlabel "{X}""#)?;
            writeln!(gnuplot, r#"set ylabel "{Y}""#)?;
            writeln!(gnuplot, "set tics out nomirror")?;

            writeln!(gnuplot, "set key out vertical top right")?;
            writeln!(gnuplot, r#"set key title "Key""#)?;

            // TODOO: color code vectors based on magnitude

            // plot svg
            let damp = 0.9;
            writeln!(gnuplot, r#"plot '{data_path}' \"#)?;
            writeln!(
                gnuplot,
                r#"  using 1:2:({damp}*{xstep}*$3/sqrt(($3)**2+($4)**2)):({damp}*{ystep}*$4/sqrt(($3)**2+($4)**2)) \"#,
                xstep = step.x,
                ystep = step.y,
            )?;
            writeln!(gnuplot, r#"  with vectors arrowstyle 2\"#)?;
            writeln!(gnuplot, r#"  title "dy/dx = {expr}" noenhance"#)?;

            // display window
            writeln!(gnuplot, "set term pop")?;
            writeln!(gnuplot, "replot")?;

            // done with the file
            gnuplot.flush()?;
            gnuplot.get_mut().sync_data()?;
            drop(gnuplot);

            // spawn gnuplot and provide the path to the file
            let mut cmd = process::Command::new("gnuplot");
            cmd.arg("--persist").arg(&gnuplot_path);
            #[cfg(not(debug_assertions))]
            {
                cmd.stdout(Stdio::null())
                    .stderr(Stdio::null())
                    .stdin(Stdio::null());
            }
            let child = cmd
                .spawn()
                .context("failed to spawn gnuplot (is it installed and in ${{PATH}}?)")?;

            state.gnuplot = Some(child);
        }

        Ok(Err(eval_err)) => {
            let err = eval_err;
            let loc = err.op.clone().map(|op| op.loc);
            shell::underline(
                &mut out,
                // NOTE(unicode)
                &loc.clone()
                    .unwrap_or(SubStr::new(Arc::clone(expr), expr.len(), 1)),
            )?;
            writeln!(out, "evalutation error: {}", err)?;

            if loc.is_none() {
                writeln!(
                    out,
                    "note: exactly 1 final value is expected on the stack after evaluation"
                )?;
            }

            match err.typ {
                EvalErrTyp::Empty => {}

                EvalErrTyp::MissingArgs { .. } => {}

                EvalErrTyp::StackMismatch { .. } => {
                    writeln!(
                    out,
                    "note: implicit multiplication is not supported, so for example '5x' would be '5*x'",
                )?;
                }

                EvalErrTyp::NullVar { text: _ } => {}
                EvalErrTyp::UndefinedIdent { text } => {
                    let most_similar = state
                        .idents
                        .iter()
                        .map(|(k, v)| {
                            (
                                strsim::normalized_damerau_levenshtein(
                                    // HACK: this is wasteful but that's not important
                                    &text.get().to_ascii_lowercase(),
                                    &k.get().to_ascii_lowercase(),
                                ),
                                (k, v),
                            )
                        })
                        .reduce(|(acc_sim, acc_kv), (elem_sim, elem_kv)| {
                            if elem_sim > acc_sim {
                                (elem_sim, elem_kv)
                            } else {
                                (acc_sim, acc_kv)
                            }
                        });
                    if let Some((sim, (key, ident))) = most_similar {
                        if sim > 0.3 {
                            let ident_typ = match ident {
                                Ident::Var(_) => "variable",
                                Ident::Const(_) => "constant",
                                Ident::Fun(_) => "function",
                            };
                            writeln!(out, "note: {ident_typ} '{key}' has a similar name")?;
                        }
                    }
                }
            }
        }

        Err(draw_err) => {
            Err(draw_err)?;
        }
    };

    Ok(())
}

fn set_expr<W: Write>(mut out: W, state: &mut State) -> anyhow::Result<()> {
    // read input expression
    let input = shell::input(&mut out, "dy/dx = ")?;
    if input.is_empty() {
        return Ok(());
    }

    state.prog = None;
    state.expr = Some(Arc::new(input));

    // lex, parse, compile & optimize input
    compile_expr(&mut out, state)?;

    Ok(())
}

fn compile_expr<W: Write>(mut out: W, state: &mut State) -> anyhow::Result<()> {
    let input = if let Some(ref expr) = state.expr {
        expr
    } else {
        return Ok(());
    };

    let lexer = Lexer::new(input);

    {
        let old_x = state.idents.insert(X.into(), Ident::Var(None));
        let old_y = state.idents.insert(Y.into(), Ident::Var(None));
        // assert that we're only replacing variables
        for ident in [old_x, old_y].into_iter().flatten() {
            if let Ident::Var(_) = ident {
            } else {
                unreachable!();
            }
        }
    }

    match parse::parse(lexer, &state.idents) {
        Ok(p) => {
            state.prog = Some(p);
        }

        Err(err) => {
            writeln!(out)?;
            shell::underline(&mut out, &err.loc)?;
            writeln!(out, "parse error: {}", err.typ)?;
            match err.typ {
                ParseErrTyp::LexErr(lex_err) => {
                    match lex_err {
                        LexErrTyp::InvalidChar => {
                            writeln!(
                                out,
                                // TODO: this hardcoded list of symbols is that
                                "note: available tokens are numbers, alphabetic identifiers, and symbols +-*/,()"
                            )?;
                        }
                        LexErrTyp::Unsupported(typ) => match typ {
                            TokTyp::Ident
                            | TokTyp::Number
                            | TokTyp::Op(_)
                            | TokTyp::Comma
                            | TokTyp::OpenParen
                            | TokTyp::CloseParen => unreachable!(),

                            TokTyp::XGreater | TokTyp::XLess => {
                                writeln!(
                                    out,
                                    "note: expected an expression but found an inequality"
                                )?;
                            }
                            TokTyp::XEqual => {
                                writeln!(
                                    out,
                                    "note: expected an expression but found an equation"
                                )?;
                            }
                            TokTyp::XPipe => writeln!(
                                out,
                                "note: use the 'abs' function to compute absolute value"
                            )?,
                            TokTyp::XOpenSquareBracket => {}
                            TokTyp::XCloseSquareBracket => {}
                            TokTyp::XOpenCurly => {}
                            TokTyp::XCloseCurly => {}
                        },
                    }
                }

                ParseErrTyp::ParseNum(_) => {
                    writeln!(out, "note: parsing as floating point number")?
                }

                ParseErrTyp::ParenMismatch => {}
            }
        }
    };

    Ok(())
}
