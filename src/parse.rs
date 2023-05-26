// SPDX: CC0-1.0

// implementation of shunting yard algorithm by dijkstra (see https://en.wikipedia.org/wiki/Shunting_yard_algorithm)

use crate::{
    eval::{Associativity, Ident, Idents, Operation, OperationTyp, OperatorTyp, Program},
    lex::{LexErr, LexErrTyp, Lexer, SubStr, TokTyp},
    Number,
};
use core::{fmt, num::ParseFloatError};

#[derive(Debug)]
pub enum ParseErrTyp {
    LexErr(LexErrTyp),
    ParseNum(ParseFloatError),
    ParenMismatch,
}

impl fmt::Display for ParseErrTyp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::LexErr(err) => write!(f, "{err}"),
            Self::ParseNum(err) => write!(f, "invalid number: {err}"),
            Self::ParenMismatch => write!(f, "mismatched parentheses"),
        }
    }
}

#[derive(Debug)]
pub struct ParseErr {
    pub typ: ParseErrTyp,
    pub loc: SubStr,
}

impl From<LexErr> for ParseErr {
    fn from(err: LexErr) -> Self {
        Self {
            typ: ParseErrTyp::LexErr(err.typ),
            loc: err.loc,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ShuntOpTyp {
    Operator(OperatorTyp),
    Ident,
    OpenParen,
    #[allow(dead_code)]
    CloseParen, // never constructed??
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct ShuntOp {
    typ: ShuntOpTyp,
    loc: SubStr,
}

impl ShuntOp {
    pub fn precedence(&self) -> i8 {
        match self.typ {
            ShuntOpTyp::Operator(op) => op.precedence(),
            ShuntOpTyp::Ident => {
                // see https://softwareengineering.stackexchange.com/questions/290043/precedence-of-function-in-shunting-yard-algorithm
                todo!("trying to take precedence of identifier should be a hard error");
            }
            _ => todo!("unhandled ShuntOp precedence: {self:?}"),
        }
    }

    pub fn into_output(self) -> Operation {
        let typ = match self.typ {
            ShuntOpTyp::Operator(typ) => OperationTyp::Operator(typ),
            ShuntOpTyp::Ident => OperationTyp::Ident,
            ShuntOpTyp::OpenParen | ShuntOpTyp::CloseParen => {
                unreachable!("no parentheses in the output stack")
            }
        };
        Operation { typ, loc: self.loc }
    }
}

pub fn parse(lex: Lexer<'_>, idents: &Idents) -> Result<Program, ParseErr> {
    let mut out: Vec<Operation> = Vec::new(); // output
    let mut ops: Vec<ShuntOp> = Vec::new(); // operator stack

    for tok in lex {
        let tok = tok?;
        match tok.typ {
            TokTyp::Number => {
                let num: Number = match tok.loc.get().parse() {
                    Ok(val) => val,
                    Err(err) => {
                        return Err(ParseErr {
                            typ: ParseErrTyp::ParseNum(err),
                            loc: tok.loc,
                        })?
                    }
                };
                out.push(Operation {
                    typ: OperationTyp::Val(num),
                    loc: tok.loc,
                });
            }

            TokTyp::Ident => {
                let as_var = Operation {
                    typ: OperationTyp::Ident,
                    loc: tok.loc.clone(),
                };
                let as_op = ShuntOp {
                    typ: ShuntOpTyp::Ident,
                    loc: tok.loc.clone(),
                };
                if let Some(op) = idents.get(&tok.loc.into()) {
                    match op {
                        Ident::Const(_) | Ident::Var(_) => out.push(as_var),
                        Ident::Fun(_) => ops.push(as_op),
                    }
                } else {
                    // HACK: if we don't know what the identifier is, assume
                    // it's a variable
                    out.push(as_var);
                }
            }

            TokTyp::Op(o1) => {
                while let Some(o2) = ops.last().cloned() {
                    if (o2.typ != ShuntOpTyp::OpenParen)
                        && ((o2.precedence() > o1.precedence())
                            || ((o1.precedence() == o2.precedence())
                                && (o1.associativity() == Associativity::Left)))
                    {
                        ops.pop().unwrap();
                        out.push(o2.into_output());
                    } else {
                        break;
                    }
                }
                ops.push(ShuntOp {
                    typ: ShuntOpTyp::Operator(o1),
                    loc: tok.loc,
                });
            }

            TokTyp::Comma => {
                while let Some(op) = ops.last() {
                    if op.typ != ShuntOpTyp::OpenParen {
                        let op = ops.pop().unwrap();
                        out.push(op.into_output());
                    } else {
                        break;
                    }
                }
            }

            TokTyp::OpenParen => {
                ops.push(ShuntOp {
                    typ: ShuntOpTyp::OpenParen,
                    loc: tok.loc,
                });
            }

            TokTyp::CloseParen => {
                while let Some(op) = ops.last() {
                    if op.typ != ShuntOpTyp::OpenParen {
                        let op = ops.pop().unwrap();
                        out.push(op.into_output());
                    } else {
                        break;
                    }
                }

                if let Some(op) = ops.pop() {
                    if op.typ != ShuntOpTyp::OpenParen {
                        return Err(ParseErr {
                            typ: ParseErrTyp::ParenMismatch,
                            loc: op.loc,
                        });
                    }
                } else {
                    return Err(ParseErr {
                        typ: ParseErrTyp::ParenMismatch,
                        loc: tok.loc,
                    });
                }

                // handle functions
                if let Some(op) = ops.last() {
                    if let ShuntOpTyp::Ident = op.typ {
                        if let Some(Ident::Fun(_)) = idents.get(&op.loc.clone().into()) {
                            let op = ops.pop().unwrap();
                            out.push(op.into_output());
                        }
                    }
                }
            }

            TokTyp::XGreater
            | TokTyp::XLess
            | TokTyp::XEqual
            | TokTyp::XPipe
            | TokTyp::XOpenSquareBracket
            | TokTyp::XCloseSquareBracket
            | TokTyp::XOpenCurly
            | TokTyp::XCloseCurly => unreachable!("unsupported token survived until parsing"),
        }
    }

    while let Some(op) = ops.pop() {
        if let ShuntOpTyp::OpenParen | ShuntOpTyp::CloseParen = op.typ {
            return Err(ParseErr {
                typ: ParseErrTyp::ParenMismatch,
                loc: op.loc,
            });
        }
        out.push(op.into_output());
    }

    Ok(Program::new(out))
}
