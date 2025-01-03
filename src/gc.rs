
use std::mem::MaybeUninit;
use std::ops::DerefMut;
use std::sync::{Arc, LazyLock, Mutex, MutexGuard};
use std::cell::Cell;
use std::marker::PhantomData;

use crate::common::*;

pub type PhantomUnsync = PhantomData<Cell<()>>;
pub type PhantomUnsend = PhantomData<MutexGuard<'static, ()>>;

unsafe trait GcType {
    fn size() -> usize;
    fn serialize(&self) -> &[u32];
    fn deserialize(data: &[u32]) -> Self;
}

const GC_HEAP_SIZE: usize = 64_000_000; // 256 MB

static GC_HEAP: LazyLock<Mutex<GcHeap>> = LazyLock::new(|| Mutex::new(GcHeap::new()));

fn heap() -> MutexGuard<'static, GcHeap> {
    GC_HEAP.lock().unwrap()
}

struct GcHeap {
    to: Box<[u32; GC_HEAP_SIZE]>,
    from: Box<[u32; GC_HEAP_SIZE]>,
    next: usize,
    roots: Vec<Cell<u32>>,
    free: Vec<usize>,
    regions: Vec<RegionData>
}

impl GcHeap {
    fn new() -> GcHeap {
        GcHeap {
            to: Box::new([0; GC_HEAP_SIZE]),
            from: Box::new([0; GC_HEAP_SIZE]),
            next: 0,
            roots: Vec::with_capacity(128),
            free: Vec::with_capacity(32),
            regions: Vec::with_capacity(4)
        }
    }

    fn alloc<T : GcType>(&mut self, t: T) -> u32 {
        let data = T::serialize(&t);
        let len = T::size();
        for i in 0..len {
            self.from[self.next] = data[i];
            self.next += 1;
        }
        self.next as u32
    }

    fn get<T : GcType>(&self, index: usize) -> T {
        let data = &self.from[index..(index + T::size())];
        T::deserialize(data)
    }

    fn update<T: GcType>(&mut self, index: usize, t: T) {
        todo!()
    }
}

#[derive(Debug)]
struct RegionData {
    roots: Vec<u32>
}

#[derive(Debug)]
struct Region {
    index: usize,
    not_send: PhantomUnsend,
    not_sync: PhantomUnsync
}

impl Region {
    fn new() -> Region {
        let data = RegionData { roots: Vec::with_capacity(8) };
        let index = heap().regions.len();
        heap().regions.push(data);
        Region {
            index,
            not_send: PhantomData,
            not_sync: PhantomData
        }
    }

    fn push(&self, root: u32) {
        let Region { index, .. } = self;
        heap().regions[*index].roots.push(root);
    }

    fn alloc<'a, T : GcType>(&'a self, t : T) -> Gc<'a, T> {
        let index = heap().alloc(t);
        self.push(index);
        Gc {
            index,
            phantom: PhantomData
        }
    }

    fn root<'a, T : GcType>(&'a self, ptr: &Gc<'a, T>) -> Root<T> {
        let index = heap().roots.len();
        heap().roots.push(Cell::new(ptr.index));
        Root {
            index,
            phantom: PhantomData
        }
    }

    fn downgrade<'a, T : GcType>(&'a self, root: &Root<T>) -> Gc<'a, T> {
        todo!()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct Root<T> {
    index: usize,
    phantom: PhantomData<T>
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Gc<'a, T> {
    index: u32,
    phantom: PhantomData<&'a T>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TermF<A> {
    Let(A, A),
    Var(usize),
    Abs(A),
    App(A, A)
}

// #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
// pub struct TermFix(TermF<Term>);

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Term<'a>(TermF<Gc<'a, Term<'a>>>);

// pub struct LazyValue {
//     val: OnceLock<Arc<Value>>,
//     env: Arc<Context>,
//     arg: Arc<Term>
// }

// impl LazyValue {
//     fn lazy(env: Arc<Context>, arg: Arc<Term>) -> Arc<LazyValue> {
//         Arc::new(LazyValue {
//             val: OnceLock::new(),
//             env,
//             arg
//         })
//     }

//     fn computed(val: Value) -> Arc<LazyValue> {
//         Arc::new(LazyValue {
//             val: OnceLock::from(val.rced()),
//             env: Arc::new(Context::Nil),
//             arg: Arc::new(Term::Var(0))
//         })
//     }

//     fn force(&self) -> Arc<Value> {
//         let v = self.val.get();
//         match v.as_ref() {
//             Some(v) => (*v).clone(),
//             None => {
//                 let new_val = eval(self.env.clone(), self.arg.clone());
//                 self.val.set(new_val.clone()).ok();
//                 new_val
//             },
//         }
//     }
// }

// #[derive(Debug, PartialEq, Eq)]
// pub enum Term {
//     Let(Arc<Term>, Arc<Term>),
//     Var(usize),
//     Abs(Arc<Term>),
//     App(Arc<Term>, Arc<Term>)
// }

// pub fn from_syntax(value: Syntax, mut bound: Vec<String>) -> Arc<Term> {
//     match value {
//         Syntax::Let(id, def, body) => {
//             let def = from_syntax(*def, bound.clone());
//             bound.push(id);
//             let body = from_syntax(*body, bound);
//             Term::Let(def, body).rced()
//         }
//         Syntax::Var(name) => {
//             let index = bound.iter().rev().enumerate().find_map(|(i, n)| {
//                 if *n == name { Some(i) } else { None }
//             }).unwrap();
//             Term::Var(index).rced()
//         }
//         Syntax::Abs(id, body) => {
//             bound.push(id);
//             let body = from_syntax(*body, bound);
//             Term::Abs(body).rced()
//         }
//         Syntax::App(f, arg) => {
//             let f = from_syntax(*f, bound.clone());
//             let arg = from_syntax(*arg, bound);
//             Term::App(f, arg).rced()
//         }
//     }
// }

// impl std::fmt::Display for Term {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self {
//             Term::Let(def, body) => {
//                 let def = def.to_string();
//                 let body = body.to_string();
//                 write!(f, "let {} in\n{}", def, body)
//             }
//             Term::Var(idx) => idx.fmt(f),
//             Term::Abs(body) => {
//                 let body = body.to_string();
//                 write!(f, "λ {}", body)
//             }
//             Term::App(fun, arg) => {
//                 let fun = fun.to_string();
//                 let arg_str = arg.to_string();
//                 match arg.as_ref() {
//                     Term::Var(_) => write!(f, "{} {}", fun, arg_str),
//                     _ => write!(f, "{} ({})", fun, arg_str)
//                 }
//             }
//         }
//     }
// }

// #[derive(Clone)]
// pub enum Context {
//     Nil,
//     Cons(Arc<LazyValue>, Arc<Context>)
// }

// struct Iter {
//     context: Arc<Context>
// }

// trait ContextOps {
//     fn push(&self, val: Arc<LazyValue>) -> Self;
//     fn iter(&self) -> Iter;
// }

// impl Context {
//     fn new() -> Arc<Context> {
//         Arc::new(Context::Nil)
//     }
// }

// impl ContextOps for Arc<Context> {
//     fn push(&self, val: Arc<LazyValue>) -> Self {
//         Arc::new(Context::Cons(val, self.clone()))
//     }

//     fn iter(&self) -> Iter {
//         Iter { context: self.clone() }
//     }
// }

// impl Iterator for Iter {
//     type Item = Arc<LazyValue>;

//     fn next(&mut self) -> Option<Self::Item> {
//         let context = self.context.clone();
//         match context.as_ref() {
//             Context::Nil => None,
//             Context::Cons(v, tail) => {
//                 self.context = tail.clone();
//                 Some(v.clone())
//             }
//         }
//     }
// }


// #[derive(Clone)]
// pub enum Value {
//     Var(usize, Arc<Context>),
//     Clos(Arc<Context>, Arc<Term>)
// }

// impl From<Arc<Value>> for Value {
//     fn from(val: Arc<Value>) -> Self {
//         match val.as_ref() {
//             Value::Var(index, env) => {
//                 Value::Var(*index, env.clone())
//             }
//             Value::Clos(env, code) => {
//                 Value::Clos(env.clone(), code.clone())
//             }
//         }
//     }
// }

// fn eval(env: Arc<Context>, term: Arc<Term>) -> Arc<Value> {
//     stacker::maybe_grow(32 * KB, 4 * MB, || {
//         eval_inner(env, term)
//     })
// }

// fn eval_inner(mut env: Arc<Context>, term: Arc<Term>) -> Arc<Value> {
//     match term.as_ref() {
//         Term::Let(def, body) => {
//             let def = LazyValue::lazy(env.clone(), def.clone());
//             env = env.push(def);
//             eval(env, body.clone())
//         }
//         Term::Var(x) => {
//             let lazy = env.iter().nth(*x).unwrap();
//             lazy.force()
//         }
//         Term::Abs(body) => {
//             Value::Clos(env.clone(), body.clone()).rced()
//         }
//         Term::App(fun, arg) => {
//             let fun = eval(env.clone(), fun.clone());
//             let arg = LazyValue::lazy(env, arg.clone());
//             match fun.as_ref() {
//                 Value::Var(level, spine) => {
//                     let spine = spine.push(arg);
//                     Value::Var(*level, spine).rced()
//                 }
//                 Value::Clos(env, body) => {
//                     let env = env.push(arg);
//                     eval(env, body.clone())
//                 }
//             }
//         }
//     }
// }

// fn quote(value: Arc<Value>, level: usize) -> Term {
//     stacker::maybe_grow(32 * KB, 4 * MB, || {
//         quote_inner(value, level)
//     }) 
// }

// fn quote_inner(value: Arc<Value>, level: usize) -> Term {
//     match value.as_ref() {
//         Value::Var(idx, spine) => {
//             let mut result = Term::Var(*idx);
//             for arg in spine.iter() {
//                 let arg = quote(arg.force(), level);
//                 result = Term::App(Arc::new(result), Arc::new(arg));
//             }
//             result
//         }
//         Value::Clos(env, body) => {
//             let mut env = env.clone();
//             let input = LazyValue::computed(Value::Var(level, Context::new()));
//             env = env.push(input);
//             let body = eval(env, body.clone());
//             let body = quote(body, level + 1);
//             Term::Abs(Arc::new(body))
//         }
//     }
// }

// pub fn normalize(term: Arc<Term>) -> Term {
//     let value = eval(Context::new(), term);
//     quote(value, 0)
// }

// pub fn to_syntax(root: Term) -> Syntax {
//     fn inner(root: &Term, supply: &mut usize, ctx: &mut Vec<String>) -> Syntax {
//         match root {
//             Term::Let(def, body) => {
//                 let name = format!("x{}", supply);
//                 *supply += 1;
//                 let def = inner(def, supply, ctx);
//                 ctx.push(name.clone());
//                 let body = inner(body, supply, ctx);
//                 ctx.pop();
//                 Syntax::Let(name, def.boxed(), body.boxed())
//             }
//             Term::Var(x) => {
//                 let i = ctx.len() - 1 - *x;
//                 let name = ctx[i].clone();
//                 Syntax::Var(name)
//             }
//             Term::Abs(body) => {
//                 let name = format!("x{}", supply);
//                 *supply += 1;
//                 ctx.push(name.clone());
//                 let body = inner(body, supply, ctx);
//                 ctx.pop();
//                 Syntax::Abs(name, body.boxed())
//             }
//             Term::App(f, a) => {
//                 let f = inner(f, supply, ctx);
//                 let a = inner(a, supply, ctx);
//                 Syntax::App(f.boxed(), a.boxed())
//             }
//         }
//     }
//     let mut supply = 0;
//     let mut ctx = vec![];
//     inner(&root, &mut supply, &mut ctx)
// }
