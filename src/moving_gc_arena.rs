
use std::ops;
use std::hint;
use std::mem::MaybeUninit;
use std::sync::{Arc, OnceLock};

use moving_gc_arena::*;

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

#[derive(Debug, Clone)]
enum Object {
    Term(TermFix),
    Value(ValueFix),
    LazyValue(LazyValueFix),
    Context(ListFix<LazyValue>)
}

impl Object {
    unsafe fn to_term_ref(&self) -> &TermFix {
        match self {
            Object::Term(x) => x,
            _ => hint::unreachable_unchecked()
        }
    }
}

impl HasIx<Object> for Object {
    fn foreach_ix<'b, 'a: 'b, F>(&'a mut self, f: F)
    where
        F: FnMut(&'b mut Ix<Object>) {
        todo!()
    }
}

static mut HEAP : Exclusive<MaybeUninit<Region<Object>>> = Exclusive::new(MaybeUninit::uninit());

pub fn init() {
    unsafe {
        HEAP.get().write(Region::new());
    }
}

unsafe fn heap() -> &'static mut Region<Object> {
    HEAP.get().as_mut_ptr().as_mut().unwrap_unchecked()
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TermF<A> {
    Let(A, A),
    Var(usize),
    Abs(A),
    App(A, A)
}

impl<A> TermF<A> {
    fn map<B>(&self, f : impl Fn(&A) -> B) -> TermF<B> {
        match self {
            TermF::Let(a, b) => TermF::Let(f(a), f(b)),
            TermF::Var(x) => TermF::Var(*x),
            TermF::Abs(b) => TermF::Abs(f(b)),
            TermF::App(t, a) => TermF::App(f(t), f(a)),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TermFix(TermF<Term>);

impl HasIx<Object> for TermFix {
    fn foreach_ix<'b, 'a: 'b, F>(&'a mut self, f: F)
    where
        F: FnMut(&'b mut Ix<Object>) {
        todo!()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Term(Ix<Object>);

impl TermFix {
    #[inline]
    fn out_ref(&self) -> &TermF<Term> {
        let TermFix(inner) = self;
        inner
    }
}

impl Term {
    fn new(xs: TermF<Term>) -> Term {
        unsafe {
            let data = TermFix(xs);
            let obj = heap().alloc(|_| Object::Term(data)).ix();
            Term(obj)
        }
    }
}

impl ops::Deref for Term {
    type Target = TermFix;

    fn deref(&self) -> &Self::Target {
        unsafe {
            let Self(key) = self;
            key.get(heap()).to_term_ref()
        }
    }
}

pub fn from_syntax(value: Syntax, mut bound: Vec<String>) -> Term {
    match value {
        Syntax::Let(id, def, body) => {
            let def = from_syntax(*def, bound.clone());
            bound.push(id);
            let body = from_syntax(*body, bound);
            Term::new(TermF::Let(def, body))
        }
        Syntax::Var(name) => {
            let index = bound.iter().rev().enumerate().find_map(|(i, n)| {
                if *n == name { Some(i) } else { None }
            }).unwrap();
            Term::new(TermF::Var(index))
        }
        Syntax::Abs(id, body) => {
            bound.push(id);
            let body = from_syntax(*body, bound);
            Term::new(TermF::Abs(body))
        }
        Syntax::App(f, arg) => {
            let f = from_syntax(*f, bound.clone());
            let arg = from_syntax(*arg, bound);
            Term::new(TermF::App(f, arg))
        }
    }
}

#[derive(Debug, Clone)]
pub enum ListF<T, A> {
    Nil,
    Cons(T, A)
}

#[derive(Debug, Clone)]
pub struct ListFix<T>(ListF<T, List<T>>);

impl<T : HasIx<Object>> HasIx<Object> for ListFix<T> {
    fn foreach_ix<'b, 'a: 'b, F>(&'a mut self, f: F)
    where
        F: FnMut(&'b mut Ix<Object>) {
        todo!()
    }
}

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

#[derive(Debug, Clone)]
pub enum ValueF<A> {
    Var(usize, List<A>),
    Clos(List<A>, Term)
}

#[derive(Debug, Clone)]
pub struct ValueFix(ValueF<LazyValue>);

impl HasIx<Object> for ValueFix {
    fn foreach_ix<'b, 'a: 'b, F>(&'a mut self, f: F)
    where
        F: FnMut(&'b mut Ix<Object>) {
        todo!()
    }
}

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

impl HasIx<Object> for LazyValueFix {
    fn foreach_ix<'b, 'a: 'b, F>(&'a mut self, f: F)
    where
        F: FnMut(&'b mut Ix<Object>) {
        todo!()
    }
}

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
            arg: Term::new(TermF::Var(0))
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
            let lazy = env.iter().nth(*x).unwrap();
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
            let mut result = Term::new(TermF::Var(*idx));
            for arg in spine.iter() {
                let arg = quote(arg.force(), level);
                result = Term::new(TermF::App(result, arg));
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
            Term::new(TermF::Abs(body))
        }
    }
}

pub fn normalize(term: Term) -> Term {
    let nil = ListFix::new(ListF::Nil);
    let value = eval(nil, term);
    quote(value, 0)
}
