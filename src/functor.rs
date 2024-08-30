
use std::sync::{Arc, OnceLock};

use crate::common::*;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TermF<A> {
    Let(A, A),
    Var(usize),
    Abs(A),
    App(A, A)
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TermFix(TermF<Term>);
pub type Term = Arc<TermFix>;

impl TermFix {
    #[inline]
    fn new(xs: TermF<Term>) -> Term {
        TermFix(xs).rced()
    }

    #[inline]
    fn out_ref(&self) -> &TermF<Term> {
        let TermFix(inner) = self;
        inner
    }
}

pub fn from_syntax(value: Syntax, mut bound: Vec<String>) -> Term {
    match value {
        Syntax::Let(id, def, body) => {
            let def = from_syntax(*def, bound.clone());
            bound.push(id);
            let body = from_syntax(*body, bound);
            TermFix::new(TermF::Let(def, body))
        }
        Syntax::Var(name) => {
            let index = bound.iter().rev().enumerate().find_map(|(i, n)| {
                if *n == name { Some(i) } else { None }
            }).unwrap();
            TermFix::new(TermF::Var(index))
        }
        Syntax::Abs(id, body) => {
            bound.push(id);
            let body = from_syntax(*body, bound);
            TermFix::new(TermF::Abs(body))
        }
        Syntax::App(f, arg) => {
            let f = from_syntax(*f, bound.clone());
            let arg = from_syntax(*arg, bound);
            TermFix::new(TermF::App(f, arg))
        }
    }
}

impl std::fmt::Display for TermFix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.out_ref() {
            TermF::Let(def, body) => {
                let def = def.to_string();
                let body = body.to_string();
                write!(f, "let {} in\n{}", def, body)
            }
            TermF::Var(idx) => idx.fmt(f),
            TermF::Abs(body) => {
                let body = body.to_string();
                write!(f, "λ {}", body)
            }
            TermF::App(fun, arg) => {
                let fun = fun.to_string();
                let arg_str = arg.to_string();
                match arg.as_ref().out_ref() {
                    TermF::Var(_) => write!(f, "{} {}", fun, arg_str),
                    _ => write!(f, "{} ({})", fun, arg_str)
                }
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ListF<T, A> {
    Nil,
    Cons(T, A)
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ListFix<T>(ListF<T, List<T>>);
pub type List<T> = Arc<ListFix<T>>;

impl<T> ListFix<T> {
    #[inline]
    fn new(xs: ListF<T, List<T>>) -> List<T> {
        ListFix(xs).rced()
    }

    #[inline]
    fn out_ref(&self) -> &ListF<T, List<T>> {
        let ListFix(inner) = self;
        inner
    }
}

type Context = List<LazyValue>;

struct Iter<T> {
    context: List<T>
}

trait ListOps<T> {
    fn push(&self, val: T) -> Self;
    fn iter(&self) -> Iter<T>;
}

impl<T: Clone> ListOps<T> for List<T> {
    fn push(&self, val: T) -> Self {
        ListFix::new(ListF::Cons(val, self.clone()))
    }

    fn iter(&self) -> Iter<T> {
        Iter { context: self.clone() }
    }
}

impl<T: Clone> Iterator for Iter<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        let context = self.context.clone();
        match context.out_ref() {
            ListF::Nil => None,
            ListF::Cons(v, tail) => {
                self.context = tail.clone();
                Some(v.clone())
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ValueF<A> {
    Var(usize, List<A>),
    Clos(List<A>, Term)
}

#[derive(Debug, Clone)]
pub struct ValueFix(ValueF<LazyValue>);
pub type Value = Arc<ValueFix>;

impl ValueFix {
    #[inline]
    fn new(xs: ValueF<LazyValue>) -> Value {
        ValueFix(xs).rced()
    }

    #[inline]
    fn out_ref(&self) -> &ValueF<LazyValue> {
        let ValueFix(inner) = self;
        inner
    }
}

#[derive(Debug, Clone)]
pub struct LazyValueF<A> {
    val: OnceLock<Value>,
    env: List<A>,
    arg: Term
}

#[derive(Debug, Clone)]
pub struct LazyValueFix(LazyValueF<LazyValue>);
pub type LazyValue = Arc<LazyValueFix>;

impl LazyValueFix {
    #[inline]
    fn new(xs: LazyValueF<LazyValue>) -> LazyValue {
        LazyValueFix(xs).rced()
    }

    #[inline]
    fn out_ref(&self) -> &LazyValueF<LazyValue> {
        let LazyValueFix(inner) = self;
        inner
    }
}

impl LazyValueFix {
    fn lazy(env: Context, arg: Term) -> LazyValue {
        let data = LazyValueF {
            val: OnceLock::new(),
            env,
            arg
        };
        LazyValueFix::new(data)
    }

    fn computed(val: Value) -> LazyValue {
        let data = LazyValueF {
            val: OnceLock::from(val),
            env: ListFix::new(ListF::Nil),
            arg: TermFix::new(TermF::Var(0))
        };
        LazyValueFix::new(data)
    }

    fn force(&self) -> Value {
        let r = self.out_ref();
        let v = r.val.get();
        match v.as_ref() {
            Some(v) => (*v).clone(),
            None => {
                let new_val = eval(r.env.clone(), r.arg.clone());
                r.val.set(new_val.clone()).ok();
                new_val
            },
        }
    }
}

fn eval(env: Context, term: Term) -> Value {
    stacker::maybe_grow(32 * 1024, 1024 * 1024, || {
        eval_inner(env, term)
    })
}

fn eval_inner(env: Context, term: Term) -> Value {
    match term.out_ref() {
        TermF::Let(def, body) => {
            let def = LazyValueFix::lazy(env.clone(), def.clone());
            let env = env.push(def);
            eval(env, body.clone())
        }
        TermF::Var(x) => {
            let lazy = env.iter().skip(*x).next().unwrap();
            lazy.force()
        }
        TermF::Abs(body) => {
            ValueFix::new(ValueF::Clos(env.clone(), body.clone()))
        }
        TermF::App(fun, arg) => {
            let fun = eval(env.clone(), fun.clone());
            let arg = LazyValueFix::lazy(env, arg.clone());
            match fun.out_ref() {
                ValueF::Var(level, spine) => {
                    let spine = spine.push(arg);
                    ValueFix::new(ValueF::Var(*level, spine))
                }
                ValueF::Clos(env, body) => {
                    let env = env.push(arg);
                    eval(env, body.clone())
                }
            }
        }
    }
}

fn quote(value: Value, level: usize) -> Term {
    stacker::maybe_grow(32 * 1024, 1024 * 1024, || {
        quote_inner(value, level)
    }) 
}

fn quote_inner(value: Value, level: usize) -> Term {
    match value.out_ref() {
        ValueF::Var(idx, spine) => {
            let mut result = TermFix::new(TermF::Var(*idx));
            for arg in spine.iter() {
                let arg = quote(arg.force(), level);
                result = TermFix::new(TermF::App(result, arg));
            }
            result
        }
        ValueF::Clos(env, body) => {
            let nil = ListFix::new(ListF::Nil);
            let var = ValueFix::new(ValueF::Var(level, nil));
            let input = LazyValueFix::computed(var);
            let env = env.push(input);
            let body = eval(env, body.clone());
            let body = quote(body, level + 1);
            TermFix::new(TermF::Abs(body))
        }
    }
}

pub fn normalize(term: Term) -> Term {
    let nil = ListFix::new(ListF::Nil);
    let value = eval(nil, term);
    quote(value, 0)
}
