// SPDX: CC0-1.0

use crate::{eval::Program, lex::SubStr};
use anyhow::Context;
use core::fmt;
use std::{
    io::{self, stdin, BufRead, Write},
    sync::Arc,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Command {
    Help,
    Quit,
    SetExpr,
    PrintProg,
    Plot,
    SetWin,
}

impl Command {
    pub const fn exhaustive() -> &'static [Command] {
        &[
            Self::Help,
            Self::Quit,
            Self::SetExpr,
            Self::Plot,
            Self::SetWin,
            Self::PrintProg,
        ]
    }

    pub const fn help(&self) -> &'static str {
        match self {
            Self::Help => "display help for each command",
            Self::Quit => "quit the shell",
            Self::SetExpr => "set expression to visualize",
            Self::PrintProg => "print program compiled from the expression (for debugging)",
            Self::Plot => "plot slope field of expression that has been set",
            Self::SetWin => "set window parameters",
        }
    }

    pub const fn name(&self) -> &'static str {
        match self {
            Self::Help => "help",
            Self::Quit => "quit",
            Self::SetExpr => "set",
            Self::PrintProg => "prog",
            Self::Plot => "plot",
            Self::SetWin => "window",
        }
    }
}

impl core::str::FromStr for Command {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let s: &str = s;
        for c in Self::exhaustive() {
            if s == c.name() {
                return Ok(*c);
            }
        }
        Err(())
    }
}

pub fn input<W: Write>(out: W, prompt: impl fmt::Display) -> anyhow::Result<String> {
    fn inner<W: Write>(mut out: W, prompt: impl fmt::Display) -> io::Result<String> {
        write!(out, "{prompt}")?;
        out.flush()?;
        let mut stdin = stdin().lock();
        let mut s = String::new();
        stdin.read_line(&mut s)?;
        Ok(s.trim().to_string())
    }

    let s = inner(out, prompt).context("read from standard input failed")?;
    Ok(s)
}

pub fn read_fromstr<W: Write, T: core::str::FromStr>(
    mut out: W,
    prompt: impl fmt::Display,
    ignore_empty: bool,
) -> anyhow::Result<Result<Option<T>, <T as core::str::FromStr>::Err>>
where
    <T as core::str::FromStr>::Err: fmt::Display,
{
    let input = Arc::new(input(&mut out, prompt)?);
    if ignore_empty && input.is_empty() {
        return Ok(Ok(None));
    }
    match input.parse::<T>() {
        Ok(new) => Ok(Ok(Some(new))),
        Err(err) => {
            writeln!(out)?;
            underline(&mut out, &SubStr::all(input))?;
            writeln!(out, "parse error: {err}")?;
            Ok(Err(err))
        }
    }
}

pub fn underline<W: Write>(mut out: W, span: &SubStr) -> io::Result<()> {
    writeln!(out, "{}", span.src())?;
    writeln!(
        out,
        "{}{}",
        " ".repeat(span.start()),
        "^".repeat(span.len())
    )?;
    Ok(())
}

pub fn dump_program<W: Write>(
    mut out: W,
    prog: &Program,
    title: core::fmt::Arguments,
) -> io::Result<()> {
    writeln!(out, "{title}: ")?;
    if prog.ops().len() == 0 {
        writeln!(out, "  (empty)")?;
    }
    for op in prog.ops() {
        writeln!(out, "  {op}")?;
    }
    Ok(())
}

pub fn expr_undefined<W: Write>(mut out: W) -> io::Result<()> {
    writeln!(out, "error: no expression is defined")
}

pub fn prog_undefined<W: Write>(mut out: W) -> io::Result<()> {
    writeln!(out, "error: no program is defined")
}
