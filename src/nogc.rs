
use std::ops;
use std::cell::LazyCell;
use std::sync::OnceLock;

use bumpalo::Bump;

use crate::common::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Exclusive<T> {
    inner: T
}

unsafe impl<T> Sync for Exclusive<T> { }

impl<T> Exclusive<T> {
    const fn new(inner: T) -> Self {
        Self { inner }
    }
    
    fn get(&mut self) -> &mut T {
        let Self { inner } = self;
        inner
    }
}

static mut BUMP : Exclusive<LazyCell<Bump>> = Exclusive::new(LazyCell::new(Bump::new));

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Bp<T : 'static> {
    data: &'static T
}

impl<T> Bp<T> {
    fn new(data: T) -> Self {
        unsafe {
            let data = BUMP.get().alloc(data);
            Bp { data }
        }
    }
}

impl<T> AsRef<T> for Bp<T> {
    fn as_ref(&self) -> &T {
        self.data
    }
}

impl<T> ops::Deref for Bp<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.data
    }
}

pub trait TrailingBp where Self : Sized {
    fn bped(self) -> Bp<Self>;
}

impl<T> TrailingBp for T {
    #[inline(always)]
    fn bped(self) -> Bp<Self> {
        Bp::new(self)
    }
}


#[derive(Clone)]
pub struct LazyValue {
    val: OnceLock<Bp<Value>>,
    env: Bp<Context>,
    arg: Bp<Term>
}

impl LazyValue {
    fn lazy(env: Bp<Context>, arg: Bp<Term>) -> Bp<LazyValue> {
        Bp::new(LazyValue {
            val: OnceLock::new(),
            env,
            arg
        })
    }

    fn computed(val: Value) -> Bp<LazyValue> {
        Bp::new(LazyValue {
            val: OnceLock::from(val.bped()),
            env: Bp::new(Context::Nil),
            arg: Bp::new(Term::Var(0))
        })
    }

    fn force(&self) -> Bp<Value> {
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Term {
    Let(Bp<Term>, Bp<Term>),
    Var(usize),
    Abs(Bp<Term>),
    App(Bp<Term>, Bp<Term>)
}

pub fn from_syntax(value: Syntax, mut bound: Vec<String>) -> Bp<Term> {
    match value {
        Syntax::Let(id, def, body) => {
            let def = from_syntax(*def, bound.clone());
            bound.push(id);
            let body = from_syntax(*body, bound);
            Term::Let(def, body).bped()
        }
        Syntax::Var(name) => {
            let index = bound.iter().rev().enumerate().find_map(|(i, n)| {
                if *n == name { Some(i) } else { None }
            }).unwrap();
            Term::Var(index).bped()
        }
        Syntax::Abs(id, body) => {
            bound.push(id);
            let body = from_syntax(*body, bound);
            Term::Abs(body).bped()
        }
        Syntax::App(f, arg) => {
            let f = from_syntax(*f, bound.clone());
            let arg = from_syntax(*arg, bound);
            Term::App(f, arg).bped()
        }
    }
}

#[derive(Clone)]
pub enum Context {
    Nil,
    Cons(Bp<LazyValue>, Bp<Context>)
}

struct Iter {
    context: Bp<Context>
}

trait ContextOps {
    fn push(&self, val: Bp<LazyValue>) -> Self;
    fn iter(&self) -> Iter;
}

impl Context {
    fn new() -> Bp<Context> {
        Bp::new(Context::Nil)
    }
}

impl ContextOps for Bp<Context> {
    fn push(&self, val: Bp<LazyValue>) -> Self {
        Bp::new(Context::Cons(val, self.clone()))
    }

    fn iter(&self) -> Iter {
        Iter { context: self.clone() }
    }
}

impl Iterator for Iter {
    type Item = Bp<LazyValue>;

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
    Var(usize, Bp<Context>),
    Clos(Bp<Context>, Bp<Term>)
}

impl From<Bp<Value>> for Value {
    fn from(val: Bp<Value>) -> Self {
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

fn eval(env: Bp<Context>, term: Bp<Term>) -> Bp<Value> {
    stacker::maybe_grow(32 * KB, 4 * MB, || {
        eval_inner(env, term)
    })
}

fn eval_inner(mut env: Bp<Context>, term: Bp<Term>) -> Bp<Value> {
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
            Value::Clos(env.clone(), body.clone()).bped()
        }
        Term::App(fun, arg) => {
            let fun = eval(env.clone(), fun.clone());
            let arg = LazyValue::lazy(env, arg.clone());
            match fun.as_ref() {
                Value::Var(level, spine) => {
                    let spine = spine.push(arg);
                    Value::Var(*level, spine).bped()
                }
                Value::Clos(env, body) => {
                    let env = env.push(arg);
                    eval(env, body.clone())
                }
            }
        }
    }
}

fn quote(value: Bp<Value>, level: usize) -> Term {
    stacker::maybe_grow(32 * KB, 4 * MB, || {
        quote_inner(value, level)
    }) 
}

fn quote_inner(value: Bp<Value>, level: usize) -> Term {
    match value.as_ref() {
        Value::Var(idx, spine) => {
            let mut result = Term::Var(*idx);
            for arg in spine.iter() {
                let arg = quote(arg.force(), level);
                result = Term::App(Bp::new(result), Bp::new(arg));
            }
            result
        }
        Value::Clos(env, body) => {
            let mut env = env.clone();
            let input = LazyValue::computed(Value::Var(level, Context::new()));
            env = env.push(input);
            let body = eval(env, body.clone());
            let body = quote(body, level + 1);
            Term::Abs(Bp::new(body))
        }
    }
}

pub fn normalize(term: Bp<Term>) -> Term {
    let value = eval(Context::new(), term);
    quote(value, 0)
}
