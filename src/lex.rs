// SPDX: CC0-1.0

use crate::eval::OperatorTyp;
use core::{fmt, iter::Peekable, str::CharIndices};
use std::sync::Arc;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct SubStr {
    // yes, silly, but atomic operations are cheap for this use case
    src: Arc<String>,
    start: usize,
    len: usize,
}

impl SubStr {
    #[inline]
    pub const fn new(src: Arc<String>, start: usize, len: usize) -> Self {
        Self { src, start, len }
    }

    #[inline]
    pub fn all(src: Arc<String>) -> Self {
        let len = src.len();
        Self::new(src, 0, len)
    }

    pub fn src(&self) -> Arc<String> {
        Arc::clone(&self.src)
    }

    pub const fn start(&self) -> usize {
        self.start
    }

    pub const fn len(&self) -> usize {
        self.len
    }

    pub const fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn get(&self) -> &str {
        &self.src[self.start..self.start + self.len]
    }

    pub fn shift_right(&mut self, by: usize) {
        self.len += by;
    }

    pub fn split_at(self, idx: usize) -> (Self, Self) {
        let mut l = self.clone();
        let mut r = self;
        l.len = idx;
        r.len -= idx;
        r.start += idx;
        (l, r)
    }
}

impl fmt::Display for SubStr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.get())
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum TokTyp {
    Ident,
    Number,
    Op(OperatorTyp),
    Comma,
    OpenParen,
    CloseParen,

    // unsupported tokens
    XGreater,
    XLess,
    XEqual,
    XPipe,
    XOpenSquareBracket,
    XCloseSquareBracket,
    XOpenCurly,
    XCloseCurly,
}

impl TokTyp {
    pub const fn is_unsupported(&self) -> bool {
        match self {
            Self::Ident
            | Self::Number
            | Self::Op(_)
            | Self::Comma
            | Self::OpenParen
            | Self::CloseParen => false,

            // unsupported tokens
            Self::XGreater
            | Self::XLess
            | Self::XEqual
            | Self::XPipe
            | Self::XOpenSquareBracket
            | Self::XCloseSquareBracket
            | Self::XOpenCurly
            | Self::XCloseCurly => true,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Tok {
    pub typ: TokTyp,
    pub loc: SubStr,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum LexErrTyp {
    InvalidChar,
    Unsupported(TokTyp),
}

impl fmt::Display for LexErrTyp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidChar => write!(f, "invalid character"),
            Self::Unsupported(_) => write!(f, "unsupported character"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct LexErr {
    pub typ: LexErrTyp,
    pub loc: SubStr,
}

#[derive(Debug)]
pub struct Lexer<'src> {
    src: &'src Arc<String>, // contains only ascii characters
    cur: Peekable<CharIndices<'src>>,
    has_errored: bool, // tells iter to yield None after error
}

impl<'src> Lexer<'src> {
    pub fn new(src: &'src Arc<String>) -> Self {
        Self {
            src,
            cur: src.char_indices().peekable(),
            has_errored: false,
        }
    }

    pub fn trim_whitespace(&mut self) {
        while let Some((_, chr)) = self.cur.peek() {
            if chr.is_ascii_whitespace() {
                self.cur.next();
            } else {
                break;
            }
        }
    }

    pub fn consume_unambiguous(&mut self) -> Option<Tok> {
        let (idx, chr) = self.cur.peek().copied()?;
        let typ = match chr {
            '+' => TokTyp::Op(OperatorTyp::Add),
            '*' => TokTyp::Op(OperatorTyp::Mul),
            '/' => TokTyp::Op(OperatorTyp::Div),
            '^' => TokTyp::Op(OperatorTyp::Exp),
            ',' => TokTyp::Comma,
            '(' => TokTyp::OpenParen,
            ')' => TokTyp::CloseParen,

            '>' => TokTyp::XGreater,
            '<' => TokTyp::XLess,
            '=' => TokTyp::XEqual,
            '|' => TokTyp::XPipe,
            '[' => TokTyp::XOpenSquareBracket,
            ']' => TokTyp::XCloseSquareBracket,
            '{' => TokTyp::XOpenCurly,
            '}' => TokTyp::XCloseCurly,
            _ => return None,
        };
        self.cur.next().unwrap(); // consume because we only peeked
        Some(Tok {
            typ,
            // @unicode
            loc: SubStr::new(Arc::clone(self.src), idx, 1),
        })
    }

    pub fn consume_by<P>(
        &mut self,
        next_idx: usize,
        typ: TokTyp,
        predicate: P,
    ) -> Option<Option<Tok>>
    where
        P: Fn(char) -> bool,
    {
        let mut tok = Tok {
            typ,
            // @unicode
            loc: SubStr::new(Arc::clone(self.src), next_idx, 0),
        };
        let while_loop_reached = self.cur.peek().is_some();
        while let Some((_, chr)) = self.cur.peek().copied() {
            if predicate(chr) {
                // @unicode
                tok.loc.shift_right(1);
                self.cur.next().unwrap();
                continue;
            } else {
                break;
            }
        }
        if tok.loc.len == 0 {
            // there is no next token of type `typ`
            None
        } else if !while_loop_reached {
            // we reached the end of `src`
            Some(None)
        } else {
            // we gathered a token of type `typ` and reached a character that
            // isn't part of it
            Some(Some(tok))
        }
    }
}

impl Iterator for Lexer<'_> {
    type Item = Result<Tok, LexErr>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.has_errored {
            return None;
        }

        self.trim_whitespace();

        let (next_idx, next_chr) = self.cur.peek().copied()?;
        let ret = if let Some(tok) = self.consume_unambiguous() {
            Some(Ok(tok))
        } else if next_chr == '-' {
            // distinguish subtraction from negation
            self.cur.next().unwrap();
            let typ = if self
                .cur
                .peek()
                .map(|(_, next_next_chr)| next_next_chr.is_ascii_whitespace())
                .unwrap_or(true)
            {
                TokTyp::Op(OperatorTyp::Sub)
            } else {
                TokTyp::Op(OperatorTyp::Neg)
            };
            Some(Ok(Tok {
                typ,
                // @unicode
                loc: SubStr::new(Arc::clone(self.src), next_idx, 1),
            }))
        } else if let Some(tok_or_end) =
            // parse identifiers
            self.consume_by(next_idx, TokTyp::Ident, |chr| chr.is_ascii_alphabetic())
        {
            tok_or_end.map(Ok)
        } else if let Some(tok_or_end) = self.consume_by(next_idx, TokTyp::Number, |chr| {
            chr.is_ascii_digit() || chr == '.'
        }) {
            // parse numbers
            tok_or_end.map(Ok)
        } else {
            self.has_errored = true;
            Some(Err(LexErr {
                typ: LexErrTyp::InvalidChar,
                // @unicode
                loc: SubStr::new(Arc::clone(self.src), next_idx, 1),
            }))
        };
        if let Some(Ok(ref tok)) = ret {
            if tok.typ.is_unsupported() {
                return Some(Err(LexErr {
                    typ: LexErrTyp::Unsupported(tok.typ),
                    // @unicode
                    loc: SubStr::new(Arc::clone(self.src), next_idx, 1),
                }));
            }
        }
        ret
    }
}
