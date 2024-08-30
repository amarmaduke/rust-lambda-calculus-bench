
use std::sync::{Arc, OnceLock};

use crate::common::*;

pub struct LazyValue {
    val: OnceLock<Arc<Value>>,
    env: Arc<Context>,
    arg: Arc<Term>
}

impl LazyValue {
    fn lazy(env: Arc<Context>, arg: Arc<Term>) -> Arc<LazyValue> {
        Arc::new(LazyValue {
            val: OnceLock::new(),
            env,
            arg
        })
    }

    fn computed(val: Value) -> Arc<LazyValue> {
        Arc::new(LazyValue {
            val: OnceLock::from(val.rced()),
            env: Arc::new(Context::Nil),
            arg: Arc::new(Term::Var(0))
        })
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

#[derive(Debug, PartialEq, Eq)]
pub enum Term {
    Let(Arc<Term>, Arc<Term>),
    Var(usize),
    Abs(Arc<Term>),
    App(Arc<Term>, Arc<Term>)
}

pub fn from_syntax(value: Syntax, mut bound: Vec<String>) -> Arc<Term> {
    match value {
        Syntax::Let(id, def, body) => {
            let def = from_syntax(*def, bound.clone());
            bound.push(id);
            let body = from_syntax(*body, bound);
            Term::Let(def, body).rced()
        }
        Syntax::Var(name) => {
            let index = bound.iter().rev().enumerate().find_map(|(i, n)| {
                if *n == name { Some(i) } else { None }
            }).unwrap();
            Term::Var(index).rced()
        }
        Syntax::Abs(id, body) => {
            bound.push(id);
            let body = from_syntax(*body, bound);
            Term::Abs(body).rced()
        }
        Syntax::App(f, arg) => {
            let f = from_syntax(*f, bound.clone());
            let arg = from_syntax(*arg, bound);
            Term::App(f, arg).rced()
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

#[derive(Clone)]
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
        Arc::new(Context::Nil)
    }
}

impl ContextOps for Arc<Context> {
    fn push(&self, val: Arc<LazyValue>) -> Self {
        Arc::new(Context::Cons(val, self.clone()))
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


#[derive(Clone)]
pub enum Value {
    Var(usize, Arc<Context>),
    Clos(Arc<Context>, Arc<Term>)
}

impl From<Arc<Value>> for Value {
    fn from(val: Arc<Value>) -> Self {
        match val.as_ref() {
            Value::Var(index, env) => {
                Value::Var(*index, env.clone())
            }
            Value::Clos(env, code) => {
                Value::Clos(env.clone(), code.clone())
            }
        }
    }
}

fn eval(env: Arc<Context>, term: Arc<Term>) -> Arc<Value> {
    stacker::maybe_grow(32 * KB, 4 * MB, || {
        eval_inner(env, term)
    })
}

fn eval_inner(mut env: Arc<Context>, term: Arc<Term>) -> Arc<Value> {
    match term.as_ref() {
        Term::Let(def, body) => {
            let def = LazyValue::lazy(env.clone(), def.clone());
            env = env.push(def);
            eval(env, body.clone())
        }
        Term::Var(x) => {
            let lazy = env.iter().nth(*x).unwrap();
            lazy.force()
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

fn quote(value: Arc<Value>, level: usize) -> Term {
    stacker::maybe_grow(32 * KB, 4 * MB, || {
        quote_inner(value, level)
    }) 
}

fn quote_inner(value: Arc<Value>, level: usize) -> Term {
    match value.as_ref() {
        Value::Var(idx, spine) => {
            let mut result = Term::Var(*idx);
            for arg in spine.iter() {
                let arg = quote(arg.force(), level);
                result = Term::App(Arc::new(result), Arc::new(arg));
            }
            result
        }
        Value::Clos(env, body) => {
            let mut env = env.clone();
            let input = LazyValue::computed(Value::Var(level, Context::new()));
            env = env.push(input);
            let body = eval(env, body.clone());
            let body = quote(body, level + 1);
            Term::Abs(Arc::new(body))
        }
    }
}

pub fn normalize(term: Arc<Term>) -> Term {
    let value = eval(Context::new(), term);
    quote(value, 0)
}

pub fn to_syntax(root: Term) -> Syntax {
    fn inner(root: &Term, supply: &mut usize, ctx: &mut Vec<String>) -> Syntax {
        match root {
            Term::Let(def, body) => {
                let name = format!("x{}", supply);
                *supply += 1;
                let def = inner(def, supply, ctx);
                ctx.push(name.clone());
                let body = inner(body, supply, ctx);
                ctx.pop();
                Syntax::Let(name, def.boxed(), body.boxed())
            }
            Term::Var(x) => {
                let i = ctx.len() - 1 - *x;
                let name = ctx[i].clone();
                Syntax::Var(name)
            }
            Term::Abs(body) => {
                let name = format!("x{}", supply);
                *supply += 1;
                ctx.push(name.clone());
                let body = inner(body, supply, ctx);
                ctx.pop();
                Syntax::Abs(name, body.boxed())
            }
            Term::App(f, a) => {
                let f = inner(f, supply, ctx);
                let a = inner(a, supply, ctx);
                Syntax::App(f.boxed(), a.boxed())
            }
        }
    }
    let mut supply = 0;
    let mut ctx = vec![];
    inner(&root, &mut supply, &mut ctx)
}
