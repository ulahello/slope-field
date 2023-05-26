// SPDX: CC0-1.0

use crate::{eval::*, Number};
use core::f64::consts;
use std::collections::HashMap; // assumes Number = f64

pub const X: &str = "x";
pub const Y: &str = "y";

pub fn standard_idents() -> Idents {
    let mut ret = HashMap::new();

    // operators
    for op in [
        OperatorTyp::Neg,
        OperatorTyp::Add,
        OperatorTyp::Sub,
        OperatorTyp::Mul,
        OperatorTyp::Div,
        OperatorTyp::Exp,
    ] {
        let (name, fun) = op.fun();
        ret.insert(name.into(), Ident::Fun(fun));
    }

    ret.insert("abs".into(), Ident::Fun(Fun::new(1, abs)));
    ret.insert("ln".into(), Ident::Fun(Fun::new(1, ln)));
    ret.insert("log".into(), Ident::Fun(Fun::new(2, log)));

    // trig
    ret.insert("sin".into(), Ident::Fun(Fun::new(1, sin)));
    ret.insert("cos".into(), Ident::Fun(Fun::new(1, cos)));
    ret.insert("tan".into(), Ident::Fun(Fun::new(1, tan)));
    ret.insert("asin".into(), Ident::Fun(Fun::new(1, arcsin)));
    ret.insert("acos".into(), Ident::Fun(Fun::new(1, arccos)));
    ret.insert("atan".into(), Ident::Fun(Fun::new(1, arctan)));
    ret.insert("arcsin".into(), Ident::Fun(Fun::new(1, arcsin)));
    ret.insert("arccos".into(), Ident::Fun(Fun::new(1, arccos)));
    ret.insert("arctan".into(), Ident::Fun(Fun::new(1, arctan)));

    ret.insert("pi".into(), Ident::Const(consts::PI));
    ret.insert("tau".into(), Ident::Const(consts::TAU));
    ret.insert("e".into(), Ident::Const(consts::E));
    ret
}

#[track_caller]
fn expect_n<const N: usize>(args: &mut [Number]) -> [Number; N] {
    assert_eq!(args.len(), N);
    args[..N].try_into().unwrap()
}

pub fn neg(mut args: Vec<Number>) -> Number {
    let [x] = expect_n::<1>(&mut args);
    -x
}

pub fn add(mut args: Vec<Number>) -> Number {
    let [x, y] = expect_n::<2>(&mut args);
    x + y
}

pub fn sub(mut args: Vec<Number>) -> Number {
    let [x, y] = expect_n::<2>(&mut args);
    x - y
}

pub fn mul(mut args: Vec<Number>) -> Number {
    let [x, y] = expect_n::<2>(&mut args);
    x * y
}

pub fn div(mut args: Vec<Number>) -> Number {
    let [x, y] = expect_n::<2>(&mut args);
    x / y
}

pub fn exp(mut args: Vec<Number>) -> Number {
    let [x, exp] = expect_n::<2>(&mut args);
    x.powf(exp)
}

pub fn abs(mut args: Vec<Number>) -> Number {
    let [x] = expect_n::<1>(&mut args);
    x.abs()
}

pub fn ln(mut args: Vec<Number>) -> Number {
    let [x] = expect_n::<1>(&mut args);
    x.ln()
}

pub fn log(mut args: Vec<Number>) -> Number {
    let [x, base] = expect_n::<2>(&mut args);
    x.log(base)
}

pub fn sin(mut args: Vec<Number>) -> Number {
    let [x] = expect_n::<1>(&mut args);
    x.sin()
}

pub fn cos(mut args: Vec<Number>) -> Number {
    let [x] = expect_n::<1>(&mut args);
    x.cos()
}

pub fn tan(mut args: Vec<Number>) -> Number {
    let [x] = expect_n::<1>(&mut args);
    x.tan()
}

pub fn arcsin(mut args: Vec<Number>) -> Number {
    let [x] = expect_n::<1>(&mut args);
    x.asin()
}

pub fn arccos(mut args: Vec<Number>) -> Number {
    let [x] = expect_n::<1>(&mut args);
    x.acos()
}

pub fn arctan(mut args: Vec<Number>) -> Number {
    let [x] = expect_n::<1>(&mut args);
    x.atan()
}
