
use std::sync::{Arc, OnceLock};
use std::hash;

use lazy_static::lazy_static;

use crate::common::*;
use crate::hc::*;

lazy_static! {
    static ref TERM_FACTORY : HcFactory<Term> = {
        HcFactory::with_capacity(1024)
    };
}

// lazy_static! {
//     static ref LAZY_VALUE_FACTORY : HcFactory<LazyValue> = {
//         HcFactory::with_capacity(1024)
//     };
// }

pub fn clear() {
    TERM_FACTORY.clear();
    //LAZY_VALUE_FACTORY.clear();
}

#[derive(Debug, Clone)]
pub struct LazyValue {
    val: OnceLock<Arc<Value>>,
    env: Arc<Context>,
    arg: Hc<Term>
}

impl PartialEq for LazyValue {
    fn eq(&self, other: &Self) -> bool {
        self.env == other.env && self.arg == other.arg
    }
}

impl Eq for LazyValue { }

impl hash::Hash for LazyValue {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.env.hash(state);
        self.arg.hash(state);
    }
}

impl LazyValue {
    fn lazy(env: Arc<Context>, arg: Hc<Term>) -> Arc<LazyValue> {
        LazyValue {
            val: OnceLock::new(),
            env,
            arg
        }.rced()
    }

    fn computed(val: Value) -> Arc<LazyValue> {
        let arg = TERM_FACTORY.make(Term::Var(0));
        LazyValue {
            val: OnceLock::from(val.rced()),
            env: Arc::new(Context::Nil),
            arg
        }.rced()
    }

    fn force(&self) -> Arc<Value> {
        let v = self.val.get();
        match v.as_ref() {
            Some(v) => (*v).clone(),
            None => {
                let new_val = eval(self.env.clone(), self.arg.clone());
                self.val.set(new_val.clone()).ok();
                new_val
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Term {
    Let(Hc<Term>, Hc<Term>),
    Var(usize),
    Abs(Hc<Term>),
    App(Hc<Term>, Hc<Term>)
}

pub fn from_syntax(value: Syntax, mut bound: Vec<String>) -> Hc<Term> {
    match value {
        Syntax::Let(id, def, body) => {
            let def = from_syntax(*def, bound.clone());
            bound.push(id);
            let body = from_syntax(*body, bound);
            TERM_FACTORY.make(Term::Let(def, body))
        }
        Syntax::Var(name) => {
            let index = bound.iter().rev().enumerate().find_map(|(i, n)| {
                if *n == name { Some(i) } else { None }
            }).unwrap();
            TERM_FACTORY.make(Term::Var(index))
        }
        Syntax::Abs(id, body) => {
            bound.push(id);
            let body = from_syntax(*body, bound);
            TERM_FACTORY.make(Term::Abs(body))
        }
        Syntax::App(f, arg) => {
            let f = from_syntax(*f, bound.clone());
            let arg = from_syntax(*arg, bound);
            TERM_FACTORY.make(Term::App(f, arg))
        }
    }
}

impl std::fmt::Display for Term {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Term::Let(def, body) => {
                let def = def.to_string();
                let body = body.to_string();
                write!(f, "let {} in\n{}", def, body)
            }
            Term::Var(idx) => idx.fmt(f),
            Term::Abs(body) => {
                let body = body.to_string();
                write!(f, "λ {}", body)
            }
            Term::App(fun, arg) => {
                let fun = fun.to_string();
                let arg_str = arg.to_string();
                match arg.as_ref() {
                    Term::Var(_) => write!(f, "{} {}", fun, arg_str),
                    _ => write!(f, "{} ({})", fun, arg_str)
                }
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Context {
    Nil,
    Cons(Arc<LazyValue>, Arc<Context>)
}

struct Iter {
    context: Arc<Context>
}

trait ContextOps {
    fn push(&self, val: Arc<LazyValue>) -> Self;
    fn iter(&self) -> Iter;
}

impl Context {
    fn new() -> Arc<Context> {
        Context::Nil.rced()
    }
}

impl ContextOps for Arc<Context> {
    fn push(&self, val: Arc<LazyValue>) -> Self {
        Context::Cons(val, self.clone()).rced()
    }

    fn iter(&self) -> Iter {
        Iter { context: self.clone() }
    }
}

impl Iterator for Iter {
    type Item = Arc<LazyValue>;

    fn next(&mut self) -> Option<Self::Item> {
        let context = self.context.clone();
        match context.as_ref() {
            Context::Nil => None,
            Context::Cons(v, tail) => {
                self.context = tail.clone();
                Some(v.clone())
            }
        }
    }
}


#[derive(Debug, Clone)]
pub enum Value {
    Var(usize, Arc<Context>),
    Clos(Arc<Context>, Hc<Term>)
}

fn eval(env: Arc<Context>, term: Hc<Term>) -> Arc<Value> {
    stacker::maybe_grow(32 * KB, 4 * MB, || {
        eval_inner(env, term)
    })
}

fn eval_inner(mut env: Arc<Context>, term: Hc<Term>) -> Arc<Value> {
    match term.as_ref() {
        Term::Let(def, body) => {
            let def = LazyValue::lazy(env.clone(), def.clone());
            env = env.push(def);
            eval(env, body.clone())
        }
        Term::Var(x) => {
            let lazy = env.iter().skip(*x).next().unwrap();
            lazy.force().clone()
        }
        Term::Abs(body) => {
            Value::Clos(env.clone(), body.clone()).rced()
        }
        Term::App(fun, arg) => {
            let fun = eval(env.clone(), fun.clone());
            let arg = LazyValue::lazy(env, arg.clone());
            match fun.as_ref() {
                Value::Var(level, spine) => {
                    let spine = spine.push(arg);
                    Value::Var(*level, spine).rced()
                }
                Value::Clos(env, body) => {
                    let env = env.push(arg);
                    eval(env, body.clone())
                }
            }
        }
    }
}

fn quote(value: &Value, level: usize) -> Term {
    stacker::maybe_grow(32 * KB, 4 * MB, || {
        quote_inner(value, level)
    }) 
}

fn quote_inner(value: &Value, level: usize) -> Term {
    match value {
        Value::Var(idx, spine) => {
            let mut result = Term::Var(*idx);
            for arg in spine.iter() {
                let arg = TERM_FACTORY.make(quote(&arg.force(), level));
                let f = TERM_FACTORY.make(result);
                result = Term::App(f, arg);
            }
            result
        }
        Value::Clos(env, body) => {
            let mut env = env.clone();
            let input = LazyValue::computed(Value::Var(level, Context::new()));
            env = env.push(input);
            let body = eval(env, body.clone());
            let body = TERM_FACTORY.make(quote(&body, level + 1));
            Term::Abs(body)
        }
    }
}

pub fn normalize(term: Hc<Term>) -> Term {
    let value = eval(Context::new(), term);
    quote(&value, 0)
}
