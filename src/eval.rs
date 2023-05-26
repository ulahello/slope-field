// SPDX: CC0-1.0

use crate::{lex::SubStr, stdlib, Number};
use core::fmt;
use std::collections::HashMap;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum OperatorTyp {
    Neg,
    Add,
    Sub,
    Mul,
    Div,
    Exp,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Associativity {
    Left,
    Right,
}

impl OperatorTyp {
    pub const fn precedence(&self) -> i8 {
        match self {
            Self::Add => 2,
            Self::Sub => 2,
            Self::Mul => 3,
            Self::Div => 3,
            Self::Neg => 4,
            Self::Exp => 5,
        }
    }

    pub const fn associativity(&self) -> Associativity {
        use Associativity::{Left, Right};
        match self {
            Self::Neg => Left,
            Self::Add => Left,
            Self::Sub => Left,
            Self::Mul => Left,
            Self::Div => Left,
            Self::Exp => Right,
        }
    }

    pub const fn fun(&self) -> (&'static str, Fun) {
        match self {
            Self::Neg => ("neg", Fun::new(1, stdlib::neg)),
            Self::Add => ("add", Fun::new(2, stdlib::add)),
            Self::Sub => ("sub", Fun::new(2, stdlib::sub)),
            Self::Mul => ("mul", Fun::new(2, stdlib::mul)),
            Self::Div => ("div", Fun::new(2, stdlib::div)),
            Self::Exp => ("exp", Fun::new(2, stdlib::exp)),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum OperationTyp {
    Operator(OperatorTyp),
    Val(Number),
    Ident,
}

#[derive(Clone, Debug)]
pub struct Operation {
    pub typ: OperationTyp,
    pub loc: SubStr,
}

impl fmt::Display for Operation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.typ {
            OperationTyp::Val(val) => write!(f, "push {val}"),
            OperationTyp::Operator(typ) => write!(f, "call '{}'", typ.fun().0),
            OperationTyp::Ident => write!(f, "call '{}'", self.loc.get()),
        }
    }
}

#[derive(Debug)]
pub enum EvalErrTyp {
    Empty,
    MissingArgs {
        name: IdentKey,
        arity: usize,
        found: usize,
    },
    StackMismatch {
        expected: usize,
        found: usize,
    },
    UndefinedIdent {
        text: SubStr,
    },
    NullVar {
        text: SubStr,
    },
}

impl fmt::Display for EvalErr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.typ {
            EvalErrTyp::Empty => write!(f, "cannot evaluate empty program"),

            EvalErrTyp::MissingArgs { name, arity, found } => write!(
                f,
                "function '{name}' requires {arity} argument{s}, but found {found}",
                name = name.get(),
                s = if *arity == 1 { "" } else { "s" }
            ),

            EvalErrTyp::StackMismatch { expected, found } => write!(
                f,
                "expected {expected} operation{s} on the stack but found {found}",
                s = if *expected == 1 { "" } else { "s" }
            ),

            EvalErrTyp::UndefinedIdent { text } => {
                write!(f, "undefined identifier '{}'", text.get())
            }

            EvalErrTyp::NullVar { text } => {
                write!(
                    f,
                    "variable '{}' is declared but its value is not defined",
                    text.get()
                )
            }
        }
    }
}

#[derive(Debug)]
pub struct EvalErr {
    pub typ: EvalErrTyp,
    pub op: Option<Operation>, // if none, associated with end-of-program checking
}

#[derive(Debug)]
pub struct Fun {
    pub arity: usize,
    pub fun: fn(Vec<Number>) -> Number,
}

impl Fun {
    pub const fn new(arity: usize, fun: fn(Vec<Number>) -> Number) -> Self {
        Self { arity, fun }
    }
}

#[derive(Debug)]
pub enum Ident {
    Var(Option<Number>),
    Const(Number),
    Fun(Fun),
}

#[derive(Clone, Debug, Eq)]
pub enum IdentKey {
    Arc(SubStr),
    Static(&'static str),
}

impl PartialEq for IdentKey {
    fn eq(&self, other: &Self) -> bool {
        self.get() == other.get()
    }
}

impl core::hash::Hash for IdentKey {
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
        self.get().hash(state)
    }
}

impl IdentKey {
    pub fn get(&self) -> &str {
        match self {
            Self::Arc(s) => s.get(),
            Self::Static(s) => s,
        }
    }
}

impl fmt::Display for IdentKey {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Arc(s) => write!(f, "{s}"),
            Self::Static(s) => write!(f, "{s}"),
        }
    }
}

impl From<SubStr> for IdentKey {
    fn from(s: SubStr) -> Self {
        Self::Arc(s)
    }
}

impl From<&'static str> for IdentKey {
    fn from(s: &'static str) -> Self {
        Self::Static(s)
    }
}

pub type Idents = HashMap<IdentKey, Ident>;

#[derive(Debug)]
pub struct Program {
    pub(crate) ops: Vec<Operation>,
}

impl Program {
    #[inline]
    pub const fn new(ops: Vec<Operation>) -> Self {
        Self { ops }
    }

    #[inline]
    pub fn ops(&self) -> core::slice::Iter<'_, Operation> {
        self.ops.iter()
    }
}

pub fn eval(prog: &Program, idents: &Idents, stack: &mut Vec<Number>) -> Result<Number, EvalErr> {
    fn expect_fun_args(
        stack: &[Number],
        op: Operation,
        name: impl Into<IdentKey>,
        fun: &Fun,
    ) -> Result<(), EvalErr> {
        let len = stack.len();
        if len < fun.arity {
            Err(EvalErr {
                typ: EvalErrTyp::MissingArgs {
                    arity: fun.arity,
                    found: len,
                    name: name.into(),
                },
                op: Some(op),
            })
        } else {
            Ok(())
        }
    }

    fn expect_exactly_n(
        stack: &[Number],
        op: impl Into<Option<Operation>>,
        n: usize,
    ) -> Result<(), EvalErr> {
        let len = stack.len();
        if len == n {
            Ok(())
        } else {
            Err(EvalErr {
                typ: EvalErrTyp::StackMismatch {
                    expected: n,
                    found: len,
                },
                op: op.into(),
            })
        }
    }

    fn eval_fun(
        stack: &mut Vec<Number>,
        op: Operation,
        name: impl Into<IdentKey>,
        fun: &Fun,
    ) -> Result<Number, EvalErr> {
        expect_fun_args(stack, op, name, fun)?;
        // stack: ...a, b, c, d
        //                 ^^^^ args if arity is 2
        let args: Vec<f64> = Vec::from_iter(stack.iter().rev().take(fun.arity).rev().copied());
        for _ in 0..fun.arity {
            stack.pop().unwrap();
        }
        Ok((fun.fun)(args))
    }

    let mut prog = prog.ops.iter().peekable();

    if prog.peek().is_none() {
        return Err(EvalErr {
            typ: EvalErrTyp::Empty,
            op: None,
        });
    }

    stack.clear();

    for op in prog {
        match op.typ {
            OperationTyp::Operator(typ) => {
                let (name, fun) = typ.fun();
                let val = eval_fun(stack, op.clone(), name, &fun)?;
                stack.push(val);
            }

            OperationTyp::Val(num) => stack.push(num),

            OperationTyp::Ident => {
                let sym = op.loc.clone();
                if let Some(ident) = idents.get(&sym.clone().into()) {
                    let val = match ident {
                        Ident::Var(None) => {
                            return Err(EvalErr {
                                typ: EvalErrTyp::NullVar { text: sym },
                                op: Some(op.clone()),
                            });
                        }
                        Ident::Var(Some(val)) | Ident::Const(val) => *val,
                        Ident::Fun(fun) => eval_fun(stack, op.clone(), sym, fun)?,
                    };
                    stack.push(val);
                } else {
                    return Err(EvalErr {
                        typ: EvalErrTyp::UndefinedIdent { text: sym },
                        op: Some(op.clone()),
                    });
                }
            }
        }
    }

    expect_exactly_n(stack, None, 1)?;
    Ok(stack.pop().unwrap())
}
