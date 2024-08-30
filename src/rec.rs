
use std::sync::{Arc, OnceLock};
use std::cell::RefCell;

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

    #[inline]
    fn forced(&self) -> bool {
        self.out_ref().val.get().is_some()
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

enum EvalState {
    ExpandEval(u32, (Context, Term)),
    CombineEval(u32, TermF<Sum<u32, (Context, Term)>>),
    CombineForce(u32, (u32, LazyValue))
}

const VAL_STACK_SIZE: usize = 32;
const STATE_STACK_SIZE: usize = 128;

// static mut EVAL_VALS : Vec<Option<Value>> = Vec::new();
// static mut EVAL_STACK : Vec<EvalState> = Vec::new();

fn eval(env: Context, term: Term) -> Value {
    let mut vals = Vec::with_capacity(VAL_STACK_SIZE);
    let mut stack = Vec::with_capacity(STATE_STACK_SIZE);
    eval_iter(&mut vals, &mut stack, env, term)
}

fn eval_iter(vals: &mut Vec<Option<Value>>, stack: &mut Vec<EvalState>, env: Context, term: Term) -> Value {
    unsafe {
        vals.push(None);
        stack.push(EvalState::ExpandEval(0, (env, term)));

        while let Some(item) = stack.pop() {
            match item {
                EvalState::ExpandEval(val_idx, (env, term)) => {
                    let term = term.out_ref();
                    let mut seeds = vec![];
                    match term {
                        TermF::Let(def, body) => {
                            vals.push(None);
                            let body_idx = (vals.len() - 1) as u32;
                            let def_lazy = LazyValueFix::lazy(env.clone(), def.clone());
                            let env = env.push(def_lazy);
                            seeds.push(EvalState::ExpandEval(body_idx, (env.clone(), body.clone())));
                            let node = TermF::Let(Sum::Right((env, def.clone())), Sum::Left(body_idx));
                            stack.push(EvalState::CombineEval(val_idx, node));
                        }
                        TermF::Var(x) => {
                            let lazy = env.iter().skip(*x).next().unwrap_unchecked();
                            let lazy_ref = lazy.as_ref().out_ref();
                            vals.push(None);
                            let lazy_idx = (vals.len() - 1) as u32;
                            if !lazy.forced() {
                                seeds.push(EvalState::ExpandEval(lazy_idx, (lazy_ref.env.clone(), lazy_ref.arg.clone())));
                            }
                            let node = (lazy_idx, lazy);
                            stack.push(EvalState::CombineForce(val_idx, node));
                        }
                        TermF::Abs(body) => {
                            let node = TermF::Abs(Sum::Right((env, body.clone())));
                            stack.push(EvalState::CombineEval(val_idx, node));
                        }
                        TermF::App(f, arg) => {
                            vals.push(None);
                            let f_idx = (vals.len() - 1) as u32;
                            seeds.push(EvalState::ExpandEval(f_idx, (env.clone(), f.clone())));
                            let node = TermF::App(Sum::Left(f_idx), Sum::Right((env, arg.clone())));
                            stack.push(EvalState::CombineEval(val_idx, node));
                        }
                    };
                    stack.append(&mut seeds);
                }
                EvalState::CombineEval(val_idx, term) => {
                    match term {
                        TermF::Let(_, body) => {
                            let Sum::Left(body_idx) = body else { unreachable!() };
                            let body_val = vals.get_unchecked_mut(body_idx as usize).take();
                            *vals.get_unchecked_mut(val_idx as usize) = body_val;
                        }
                        TermF::Var(_) => unreachable!(),
                        TermF::Abs(body) => {
                            let Sum::Right((env, body)) = body else { unreachable!() };
                            let result = ValueFix::new(ValueF::Clos(env, body));
                            *vals.get_unchecked_mut(val_idx as usize) = Some(result);
                        }
                        TermF::App(fun, arg) => {
                            let Sum::Left(fun_idx) = fun else { unreachable!() };
                            let Sum::Right((env, arg)) = arg else { unreachable!() };
                            let fun_val = vals.get_unchecked_mut(fun_idx as usize).take().unwrap_unchecked();
                            let arg_lazy = LazyValueFix::lazy(env, arg);
                            match fun_val.out_ref() {
                                ValueF::Var(level, spine) => {
                                    let spine = spine.push(arg_lazy);
                                    let result = ValueFix::new(ValueF::Var(*level, spine));
                                    *vals.get_unchecked_mut(val_idx as usize) = Some(result);
                                }
                                ValueF::Clos(env, body) => {
                                    let env = env.push(arg_lazy);
                                    stack.push(EvalState::ExpandEval(val_idx, (env, body.clone())));
                                }
                            }
                        }
                    }
                }
                EvalState::CombineForce(val_idx, (lazy_idx, lazy)) => {
                    let r = lazy.as_ref().out_ref();
                    let v = r.val.get();
                    let mut result = vals.get_unchecked_mut(lazy_idx as usize).take();
                    result = match v.cloned() {
                        Some(v) => Some(v),
                        None => {
                            r.val.set(result.clone().unwrap()).ok();
                            result
                        }
                    };
                    *vals.get_unchecked_mut(val_idx as usize) = result;
                }
            }
        }

        vals.get_unchecked_mut(0).take().unwrap_unchecked()
    }
}

fn quote_iter(value: Value, level: usize) -> Term {
    enum State {
        Expand(usize, (Value, usize)),
        Combine(usize, Sum<usize, ValueF<usize>>)
    }

    const VAL_STACK_SIZE: usize = 32;
    const STATE_STACK_SIZE: usize = 128;

    let mut vals: Vec<Option<Term>> = Vec::with_capacity(VAL_STACK_SIZE);
    vals.push(None);
    let mut stack = Vec::with_capacity(STATE_STACK_SIZE);
    stack.push(State::Expand(0, (value, level)));

    while let Some(item) = stack.pop() {
        match item {
            State::Expand(val_idx, (value, level)) => {
                let mut seeds = vec![];
                let node: Sum<usize, ValueF<usize>> = match value.out_ref() {
                    ValueF::Var(idx, spine) => {
                        let mut arg_indices = ListFix::new(ListF::Nil);
                        for arg in spine.iter() {
                            vals.push(None);
                            let arg_idx = vals.len() - 1;
                            arg_indices = arg_indices.push(arg_idx);
                            seeds.push(State::Expand(arg_idx, (arg.force(), level)));
                        }
                        Sum::Right(ValueF::Var(*idx, arg_indices))
                    }
                    ValueF::Clos(env, body) => {
                        let nil = ListFix::new(ListF::Nil);
                        let var = ValueFix::new(ValueF::Var(level, nil));
                        let input = LazyValueFix::computed(var);
                        let env = env.push(input);
                        let body = eval(env, body.clone());
                        vals.push(None);
                        let body_idx = vals.len() - 1;
                        seeds.push(State::Expand(body_idx, (body, level + 1)));
                        Sum::Left(body_idx)
                    }
                };
                stack.push(State::Combine(val_idx, node));
                stack.extend(seeds);
            }
            State::Combine(val_idx, node) => {
                match node {
                    Sum::Left(body_idx) => {
                        let body_val = vals[body_idx].take().unwrap();
                        let result = TermFix::new(TermF::Abs(body_val));
                        vals[val_idx] = Some(result);
                    }
                    Sum::Right(ValueF::Var(idx, spine)) => {
                        let mut result = TermFix::new(TermF::Var(idx));
                        for arg_idx in spine.iter() {
                            let arg_val = vals[arg_idx].take().unwrap();
                            result = TermFix::new(TermF::App(result, arg_val));
                        }
                        vals[val_idx] = Some(result);
                    }
                    Sum::Right(ValueF::Clos(_, _)) => unreachable!()
                };
            }
        }
    }

    vals[0].take().unwrap()
}

pub fn normalize(term: Term) -> Term {
    let nil = ListFix::new(ListF::Nil);
    let value = eval(nil, term);
    quote_iter(value, 0)
}
