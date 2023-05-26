// SPDX: CC0-1.0

pub mod eval;
pub mod lex;
pub mod parse;
pub mod shell;
pub mod stdlib;

use core::{fmt, num::NonZeroU16, ops::Range};

pub type Number = f64;

#[derive(Clone, Copy, Debug)]
pub struct Point<T> {
    pub x: T,
    pub y: T,
}

#[derive(Clone, Debug)]
pub struct Window {
    pub x: Range<Number>,
    pub y: Range<Number>,
    pub density: NonZeroU16,
}

impl fmt::Display for Window {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Window")
            .field("x range", &self.x)
            .field("y range", &self.y)
            .field("density", &self.density)
            .finish()
    }
}

pub fn draw<F, D, FErr, DErr>(
    mut f: F,
    mut draw: D,
    window: &Window,
) -> Result<Result</* returns step */ Point<Number>, FErr>, DErr>
where
    F: FnMut(Point<Number>) -> Result<Number, FErr>, // derivative of a function
    D: FnMut(Point<Number>, Point<Number>) -> Result<(), DErr>, // draw slope at given point in (window, space)
{
    let compute_step_nstep = move |range: &Range<Number>| {
        // TODO: better response to if start > end?
        (range.end - range.start).clamp(0.0, Number::MAX)
            / (Number::from(window.density.get()) + 1.0)
    };
    let step = Point {
        x: compute_step_nstep(&window.x),
        y: compute_step_nstep(&window.y),
    };

    let mut p = Point {
        x: Number::NAN,
        y: Number::NAN,
    };

    for y in 0..window.density.get() {
        p.y = window.y.start + step.y * (Number::from(y) + 1.0);
        for x in 0..window.density.get() {
            p.x = window.x.start + step.x * (Number::from(x) + 1.0);
            let dx = step.x;
            let dy = match f(p) {
                Ok(val) => val * step.x,
                Err(ferr) => return Ok(Err(ferr)),
            };
            draw(p, Point { x: dx, y: dy })?;
        }
    }

    Ok(Ok(step))
}
