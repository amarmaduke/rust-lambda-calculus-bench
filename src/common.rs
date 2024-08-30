
use std::sync::Arc;
use std::collections::HashMap;
use std::thread;

use serde_derive::{Serialize, Deserialize};

pub trait TrailingArc {
    fn rced(self) -> Arc<Self>;
}

impl<T> TrailingArc for T {
    #[inline(always)]
    fn rced(self) -> Arc<Self> {
        Arc::new(self)
    }
}

pub trait TrailingBox {
    fn boxed(self) -> Box<Self>;
}

impl<T> TrailingBox for T {
    #[inline(always)]
    fn boxed(self) -> Box<Self> {
        Box::new(self)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Sum<A, B> {
    Left(A),
    Right(B)
}

pub const KB: usize = 1024;
pub const MB: usize = 1024 * KB;
pub const GB: usize = 1024 * MB;

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "kebab-case")]
pub enum Syntax {
    Let(String, Box<Syntax>, Box<Syntax>),
    Var(String),
    Abs(String, Box<Syntax>),
    App(Box<Syntax>, Box<Syntax>)
}

pub fn try_discard_lets(x: &Syntax) -> Option<Syntax> {
    fn inner(x: &Syntax, ctx: &mut HashMap<String, Vec<bool>>) -> Option<Syntax> {
        match x {
            Syntax::Let(x, _, body) => {
                ctx.entry(x.clone()).or_default().push(false);
                let result = inner(body, ctx);
                ctx.entry(x.clone()).or_default().pop();
                result
            }
            Syntax::Var(x) => {
                ctx.get(x)
                    .and_then(|v| v.last())
                    .and_then(|b| if *b { Some(Syntax::Var(x.clone())) } else { None })
            }
            Syntax::Abs(x, body) => {
                ctx.entry(x.clone()).or_default().push(true);
                let body = inner(body, ctx)?;
                ctx.entry(x.clone()).or_default().pop();
                Some(Syntax::Abs(x.clone(), body.boxed()))
            }
            Syntax::App(f, a) => {
                let f = inner(f, ctx)?;
                let a = inner(a, ctx)?;
                Some(Syntax::App(f.boxed(), a.boxed()))
            }
        }
    }
    let mut ctx = HashMap::new();
    inner(x, &mut ctx)
}

pub fn try_numeral(x: &Syntax) -> Option<u64> {
    fn inner(x: &Syntax, counts: &mut HashMap<String, u64>, order: &mut Vec<String>) -> Option<u64> {
        match x {
            Syntax::Let(_, _, _) => return None,
            Syntax::Var(x) => {
                *counts.entry(x.clone()).or_default() += 1;
            }
            Syntax::Abs(x, body) => {
                order.push(x.clone());
                inner(body, counts, order);
            }
            Syntax::App(f, a) => {
                inner(f, counts, order);
                inner(a, counts, order);
            }
        }
        Some(0)
    }
    let mut counts = HashMap::new();
    let mut order = vec![];
    inner(x, &mut counts, &mut order)?;
    if order.len() != 2 { return None; }
    let succ_counts = counts[&order[0]];
    let zero_counts = counts[&order[1]];
    if zero_counts != 1 { return None; }
    Some(succ_counts)
}
