use std::ops;
use std::hint;
use std::mem::MaybeUninit;
use std::collections::{HashSet, HashMap};

use crate::common::*;

#[derive(Debug)]
struct Heap<T> {
    data: Vec<T>,
    free: Vec<usize>
}

impl<T> Heap<T> {
    fn new() -> Heap<T> {
        Heap {
            data: Vec::with_capacity(1024),
            free: Vec::with_capacity(128)
        }
    }

    fn alloc(&mut self, data: T) -> usize {
        if let Some(idx) = self.free.pop() {
            self.data[idx] = data;
            idx
        } else {
            let idx = self.data.len();
            self.data.push(data);
            idx
        }
    }

    fn get(&self, index: usize) -> &T {
        &self.data[index]
    }

    fn get_mut(&mut self, index: usize) -> &mut T {
        &mut self.data[index]
    }

    fn delete(&mut self, idx: usize) {
        self.free.push(idx);
    }

    fn iter(&self) -> HeapIterator<'_, T> {
        HeapIterator {
            heap: self,
            index: 1
        }
    }
}

struct HeapIterator<'a, T> {
    heap: &'a Heap<T>,
    index: usize
}

impl<'a, T> Iterator for HeapIterator<'a, T> {
    type Item = usize;

    fn next(&mut self) -> Option<Self::Item> {
        let index = self.index;
        let mut offset = 0;
        while self.heap.free.contains(&(index + offset)) {
            offset += 1;
        }
        self.index += offset;
        let result = self.index;
        self.index += 1;
        if result < self.heap.data.len() { Some(result) }
        else { None }
    }
}

static mut HEAP : MaybeUninit<Heap<Node>> = MaybeUninit::uninit();

pub unsafe fn init() {
    HEAP.write(Heap::new());
    HEAP.assume_init_mut().alloc(Node::new_null());
}

unsafe fn heap() -> &'static mut Heap<Node> {
    HEAP.assume_init_mut()
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Kind {
    Null,
    Root,
    Lam,
    Def,
    Apply,
    Dup,
    Scope,
    Erase
}

mod kind {
    pub const PRINCIPAL: usize = 0;
    pub mod null {
        pub const PRINCIPAL: usize = 0;
        pub const AUX1: usize = 1;
        pub const AUX2: usize = 2;
        pub const AUX3: usize = 3;
    }
    pub mod root {
        pub const ROOT: usize = 1;
    }
    pub mod lam {
        pub const ROOT: usize = 0;
        pub const BODY: usize = 1;
        pub const BIND: usize = 2;
    }
    pub mod def {

    }
    pub mod apply {
        pub const FUNCTION: usize = 0;
        pub const ARGUMENT: usize = 1;
        pub const ROOT: usize = 2;
    }
    pub mod dup {
        pub const INPUT: usize = 0;
        pub const AUX1: usize = 1;
        pub const AUX2: usize = 2;
        pub const LEVEL: usize = 3;
    }
    pub mod scope {
        pub const INPUT: usize = 0;
        pub const BODY: usize = 1;
        pub const LEVEL: usize = 2;
        pub const VALUE: usize = 3;
    }
}

impl Kind {
    fn value(&self) -> u8 {
        match self {
            Kind::Null => 0,
            Kind::Root => 1,
            Kind::Lam => 2,
            Kind::Def => 3,
            Kind::Apply => 4,
            Kind::Dup => 5,
            Kind::Scope => 6,
            Kind::Erase => 7,
        }
    }

    fn ports(&self) -> &[usize] {
        match self {
            Kind::Null => &[0, 1, 2, 3],
            Kind::Root => &[1],
            Kind::Lam => &[0, 1, 2],
            Kind::Def => &[0, 1, 2, 3],
            Kind::Apply => &[0, 1, 2],
            Kind::Dup => &[0, 1, 2],
            Kind::Scope => &[0, 1],
            Kind::Erase => &[0],
        }
    }

    fn auxiliary_ports(&self) -> &[usize] {
        match self {
            Kind::Null => &[1, 2, 3],
            Kind::Root => &[1],
            Kind::Lam => &[1, 2],
            Kind::Def => &[1, 2, 3],
            Kind::Apply => &[1, 2],
            Kind::Dup => &[1, 2],
            Kind::Scope => &[1],
            Kind::Erase => &[],
        }
    }
}

impl From<u8> for Kind {
    fn from(value: u8) -> Self {
        use Kind::*;
        match value {
            0 => Null,
            1 => Root,
            2 => Lam,
            3 => Def,
            4 => Apply,
            5 => Dup,
            6 => Scope,
            7 => Erase,
            _ => unreachable!()
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Node([u32; 4]);

impl Node {
    const HEADER_MASK: u32 = 0xC0000000;
    const PORT_MASK: u32 = 0x30000000;
    const AGENT_MASK: u32 = 0x0FFFFFFF;

    #[inline]
    fn inner(&self) -> &[u32; 4] {
        let Node(inner) = self;
        inner
    }

    #[inline]
    fn inner_mut(&mut self) -> &mut [u32; 4] {
        let Node(inner) = self;
        inner
    }

    #[inline]
    fn new_null() -> Node {
        Node([0, 0, 0, 0])
    }

    fn header(&self) -> u8 {
        let slots = self.inner();
        let b0 = (slots[0] & Node::HEADER_MASK) >> (32 - 2);
        let b1 = (slots[1] & Node::HEADER_MASK) >> (32 - 4);
        let b2 = (slots[2] & Node::HEADER_MASK) >> (32 - 6);
        let b3 = (slots[3] & Node::HEADER_MASK) >> (32 - 8);
        let data = b3 | b2 | b1 | b0;
        data as _
    }

    fn set_header(&mut self, header: u8) {
        let slots = (*self.inner()).map(|x| x & !Node::HEADER_MASK);
        let inner = self.inner_mut();
        let b0 = ((header & 0x03) as u32) << (32 - 2);
        let b1 = ((header & 0x0C) as u32) << (32 - 4);
        let b2 = ((header & 0x30) as u32) << (32 - 6);
        let b3 = ((header & 0xC0) as u32) << (32 - 8);
        inner[0] = b0 ^ slots[0];
        inner[1] = b1 ^ slots[1];
        inner[2] = b2 ^ slots[2];
        inner[3] = b3 ^ slots[3];
    }

    #[inline]
    fn port(&self, id: usize) -> u32 {
        (self.inner()[id] & Node::PORT_MASK) >> (32 - 4)
    }

    #[inline]
    fn set_port(&mut self, id: usize, data: u32) {
        let data = (data & 3) << (32 - 4);
        let slots = self.inner_mut();
        slots[id] = (slots[id] & !Node::PORT_MASK) | data;
    }

    #[inline]
    fn agent(&self, id: usize) -> u32 {
        self.inner()[id] & Node::AGENT_MASK
    }

    #[inline]
    fn set_agent(&mut self, id: usize, data: u32) {
        let slots = self.inner_mut();
        slots[id] = (slots[id] & !Node::AGENT_MASK) | (data & Node::AGENT_MASK);
    }

    #[inline]
    fn wire(&self, id: usize) -> u32 {
        self.inner()[id] & !Node::HEADER_MASK
    }

    #[inline]
    fn set_wire(&mut self, id: usize, port: u32, agent: u32) {
        self.set_port(id, port);
        self.set_agent(id, agent);
    }
}

impl std::fmt::Debug for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let header: Kind = dbg!(self.header()).into();
        let wires = [self.agent(0), self.agent(1), self.agent(2), self.agent(3)];
        f.debug_tuple("Node").field(&header).field(&wires).finish()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Wire(u32);

impl Wire {
    #[inline]
    fn inner(&self) -> u32 {
        let Wire(inner) = self;
        *inner
    }

    #[inline]
    fn agent(&self) -> usize {
        let data = self.inner() & Node::AGENT_MASK;
        data as _
    }

    #[inline]
    fn port(&self) -> usize {
        ((self.inner() & Node::PORT_MASK) >> (32 - 4)) as _
    }

    unsafe fn with(&self, port: usize) -> Wire {
        let agent = self.agent() as u32;
        let port = (port as u32) & 3;
        let data = (port << (32 - 4)) | agent;
        Wire(data)
    }

    unsafe fn alloc(k : Kind) -> Wire {
        let mut node = Node::new_null();
        node.set_header(k.value());
        let data: u32 = heap().alloc(node) as _;
        Wire(data)
    }

    unsafe fn kind(&self) -> Kind {
        let node = heap().get(self.agent());
        node.header().into()
    }

    unsafe fn delete(&self) {
        let index = self.inner() as usize;
        heap().delete(index);
    }

    unsafe fn load(&self, port: usize) -> u32 {
        let node = heap().get(self.agent());
        node.agent(port)
    }

    unsafe fn store(&self, port: usize, other_port: usize, data: usize) {
        let node = heap().get_mut(self.agent());
        node.set_agent(port, data as _);
        node.set_port(port, other_port as _);
    }

    unsafe fn flip(&self) -> Wire {
        let node = heap().get(self.agent());
        let data = node.wire(self.port());
        Wire(data)
    }

    unsafe fn active(&self) -> bool {
        self.port() == kind::PRINCIPAL
        && self.flip().port() == kind::PRINCIPAL
    }

    unsafe fn connect(&self, other: Wire) {
        self.store(self.port(), other.port() as _, other.agent() as _);
        other.store(other.port(), self.port() as _, self.agent() as _);
    }

    unsafe fn connect_many(&self, wires: &[Wire]) {
        let mut total = 1;
        let mut dups: Vec<Wire> = vec![*self];
        while total < wires.len() {
            if total * 2 < wires.len() {
                let mut next_dups: Vec<Wire> = vec![];
                for dup in dups.drain(..) {
                    let new_dup = Wire::alloc(Kind::Dup).agent();
                    dup.connect((kind::dup::INPUT, new_dup).into());
                    next_dups.push((kind::dup::AUX1, new_dup).into());
                    next_dups.push((kind::dup::AUX2, new_dup).into());
                }
                dups = next_dups;
                total *= 2;
            } else {
                let remaining = wires.len() - total;
                let suffix = dups.split_off(remaining);
                let mut prefix: Vec<Wire> = vec![];
                for dup in dups.drain(..) {
                    let new_dup = Wire::alloc(Kind::Dup).agent();
                    dup.connect((kind::dup::INPUT, new_dup).into());
                    prefix.push((kind::dup::AUX1, new_dup).into());
                    prefix.push((kind::dup::AUX2, new_dup).into());
                }
                dups = prefix;
                dups.extend(suffix);
                total += remaining;
            }
        }
        for (w1, w2) in dups.drain(..).zip(wires.iter()) {
            w1.connect(*w2);
        }
    }
}

impl From<(usize, usize)> for Wire {
    fn from(value: (usize, usize)) -> Self {
        let port = ((value.0 & 3) << (32 - 4)) as u32;
        let agent = value.1 as u32;
        Wire(port | agent)
    }
}

pub fn from_syntax(syntax: Syntax) -> Wire {
    unsafe fn fix_nulls(data: &mut Vec<Wire>) {
        for wire in data.iter_mut() {
            if wire.kind() == Kind::Null {
                let next_wire = wire.flip();
                wire.delete();
                *wire = next_wire;
            }
        }
    }
    fn inner(syntax: Syntax, root: Wire, bound: &mut Vec<(String, Vec<Wire>)>) -> Wire {
        unsafe {
            match syntax {
                Syntax::Let(name, def, body) => {
                    let f = Syntax::Abs(name, body);
                    let t = Syntax::App(f.boxed(), def);
                    inner(t, root, bound)
                }
                Syntax::Var(name) => {
                    let (index, vars) = bound
                        .iter_mut()
                        .rev()
                        .enumerate()
                        .find_map(|(i, (n, v))|
                            { if *n == name { Some((i, v)) } else { None } })
                        .unwrap();
                    if index == 0 {
                        let var = Wire::alloc(Kind::Null);
                        var.store(kind::null::PRINCIPAL, root.port(), root.agent());
                        vars.push(var.with(kind::null::AUX1));
                        var.with(kind::null::PRINCIPAL)
                    } else {
                        let var = Wire::alloc(Kind::Scope);
                        var.store(kind::scope::INPUT, root.port(), root.agent());
                        var.store(kind::scope::LEVEL, 0, 0);
                        var.store(kind::scope::VALUE, 0, (index - 1) as _);
                        vars.push(var.with(kind::scope::BODY));
                        var.with(kind::scope::INPUT)
                    }
                }
                Syntax::Abs(name, body) => {
                    let lam = Wire::alloc(Kind::Lam);
                    lam.store(kind::lam::ROOT, root.port(), root.agent());
                    bound.push((name, vec![]));
                    let body = inner(*body, (kind::lam::BODY, lam.agent()).into(), bound);
                    let Some((_, mut vars)) = bound.pop() else { panic!() };
                    lam.store(kind::lam::BODY, body.port(), body.agent());
                    fix_nulls(&mut vars);
                    lam.with(kind::lam::BIND).connect_many(&vars);
                    lam.with(kind::lam::ROOT)
                }
                Syntax::App(f, a) => {
                    let app = Wire::alloc(Kind::Apply);
                    app.store(kind::apply::ROOT, root.port(), root.agent());
                    let fun = inner(*f, app.with(kind::apply::FUNCTION), bound);
                    let arg = inner(*a, app.with(kind::apply::ARGUMENT), bound);
                    app.store(kind::apply::FUNCTION, fun.port(), fun.agent());
                    app.store(kind::apply::ARGUMENT, arg.port(), arg.agent());
                    app.with(kind::apply::ROOT)
                }
            }
        }
    }
    unsafe {
        let root = Wire::alloc(Kind::Root).with(kind::root::ROOT);
        let mut bound = vec![];
        let body = inner(syntax, root, &mut bound);
        root.store(kind::root::ROOT, body.port(), body.agent());
        root
    }
}

pub unsafe fn to_dot() -> String {
    unsafe fn dot_metadata(port: usize, wire: Wire) -> String {
        let (id, attr) = match wire.kind() {
            Kind::Null => todo!(),
            Kind::Root => {
                let id = "r".to_string();
                let attr = match port {
                    kind::PRINCIPAL => "s",
                    _ => unreachable!()
                };
                (id, attr)
            }
            Kind::Lam => {
                let id = "l".to_string();
                let attr = match port {
                    kind::lam::ROOT => "n",
                    kind::lam::BODY => "e",
                    kind::lam::BIND => "s",
                    _ => unreachable!()
                };
                (id, attr)
            }
            Kind::Def => todo!(),
            Kind::Apply => {
                let id = "a".to_string();
                let attr = match port {
                    kind::apply::ROOT => "n",
                    kind::apply::FUNCTION => "s",
                    kind::apply::ARGUMENT => "e",
                    _ => unreachable!()
                };
                (id, attr)
            }
            Kind::Dup => {
                let level = wire.load(kind::dup::LEVEL);
                let id = format!("d{}", level);
                let attr = match port {
                    kind::dup::INPUT => "n",
                    kind::dup::AUX1 => "sw",
                    kind::dup::AUX2 => "se",
                    _ => unreachable!()
                };
                (id, attr)
            }
            Kind::Scope => {
                let level = wire.load(kind::scope::LEVEL);
                let value = wire.load(kind::scope::VALUE);
                let id = format!("s{}v{}", level, value);
                let attr = match port {
                    kind::scope::INPUT => "n",
                    kind::scope::BODY => "s",
                    _ => unreachable!()
                };
                (id, attr)
            }
            Kind::Erase => {
                let id = "e".to_string();
                let attr = match port {
                    kind::PRINCIPAL => "s",
                    _ => unreachable!()
                };
                (id, attr)
            }
        };
        format!("g{}{}:{}", wire.inner(), id, attr)
    }
    let mut result = "".to_string();
    let mut duplicate = HashSet::new();
    for index in heap().iter() {
        let agent = Wire(index as u32);
        for port in agent.kind().ports() {
            let other = agent.with(*port).flip();
            let lhs = dot_metadata(*port, agent);
            let rhs = dot_metadata(other.port(), other);
            if !duplicate.contains(&(lhs.clone(), rhs.clone())) {
                let edge = format!("    {} -- {}\n", lhs.clone(), rhs.clone());
                result.push_str(edge.as_str());
                duplicate.insert((lhs.clone(), rhs.clone()));
                duplicate.insert((rhs.clone(), lhs.clone()));
            }
        }
    }
    format!("graph {{\n{}}}", result)
}

pub fn to_syntax(agent: Wire) -> Syntax {
    unsafe fn next_structural_from(mut agent: Wire, mut port: usize) -> (usize, Wire) {
        // loop {
        //     wire = agent.wire(port);
        //     match agent.kind() {
        //         Kind::Null => unreachable!(),
        //         Kind::Root => unreachable!(),
        //         Kind::Lam => break,
        //         Kind::Def => unreachable!(),
        //         Kind::Apply => break,
        //         Kind::Dup => {
        //             port = kind::dup::INPUT;
        //         }
        //         Kind::Scope => {
        //             if port == kind::scope::INPUT {
        //                 port = kind::scope::BODY;
        //             } else {
        //                 port = kind::scope::INPUT;
        //             }
        //         }
        //         Kind::Erase => unreachable!(),
        //     }
        // }
        // (port, agent)
        todo!()
    }
    unsafe fn handle_variable(agent: Wire, port: usize, context: &mut Vec<Wire>) -> Option<Syntax> {
        todo!()
    }
    unsafe fn inner(agent: Wire, supply: &mut usize, context: &mut Vec<Wire>) -> Syntax {
        todo!()
        // match agent.kind() {
        //     Kind::Null => unreachable!(),
        //     Kind::Root => {
        //         let wire = agent.wire(kind::PRINCIPAL);
        //         inner(wire.agent(), supply, context)
        //     }
        //     Kind::Lam => {
        //         context.push(agent);
        //         let (port, body) = next_structural_from(agent, kind::lam::BODY);
        //         let body = handle_variable(body, port, context).unwrap_or_else(|| inner(body, supply, context));
        //         context.pop();
        //         let name = format!("x{}", supply);
        //         *supply += 1;
        //         Syntax::Abs(name, body.boxed())
        //     }
        //     Kind::Def => unreachable!(),
        //     Kind::Apply => {
        //         let (fun_port, fun) = next_structural_from(agent, kind::apply::FUNCTION);
        //         let fun = handle_variable(fun, fun_port, context).unwrap_or_else(|| inner(fun, supply, context));
        //         let (arg_port, arg) = next_structural_from(agent, kind::apply::ARGUMENT);
        //         let arg = handle_variable(arg, arg_port, context).unwrap_or_else(|| inner(arg, supply, context));
        //         Syntax::App(fun.boxed(), arg.boxed())
        //     }
        //     Kind::Dup => unreachable!(),
        //     Kind::Scope => unreachable!(),
        //     Kind::Erase => unreachable!(),
        // }
    }
    unsafe {
        let mut supply = 0;
        let mut context = vec![];
        inner(agent, &mut supply, &mut context)
    }
}

unsafe fn beta(app: Wire, lam: Wire) -> (Wire, Wire) {
    todo!()
    // let var_scope = Agent::alloc(Kind::Scope);
    // let body_scope = Agent::alloc(Kind::Scope);

    // // Setup var_scope
    // let (root_port, root) = app.wire(kind::apply::ROOT);
    // var_scope.connect(kind::PRINCIPAL, root_port, root);
    // let (var_port, var) = lam.wire(kind::lam::BIND);
    // var_scope.connect(kind::scope::BODY, var_port, var);

    // // Setup body_scope
    // let (arg_port, arg) = app.wire(kind::apply::ARGUMENT);
    // body_scope.connect(kind::PRINCIPAL, arg_port, arg);
    // let (body_port, body) = lam.wire(kind::lam::BODY);
    // body_scope.connect(kind::scope::BODY, body_port, body);

    // app.delete();
    // lam.delete();
    // (var_scope, body_scope)
}

unsafe fn commute(top: Wire, bot: Wire) -> (Vec<Wire>, Vec<Wire>) {
    todo!()
    // let top_kind = top.kind();
    // let bot_kind = bot.kind();
    // let top_ports = top_kind.auxiliary_ports();
    // let bot_ports = bot_kind.auxiliary_ports();
    // let top_copies: Vec<_> = bot_ports.iter().map(|_| Agent::alloc(top.kind())).collect();
    // let bot_copies: Vec<_> = top_ports.iter().map(|_| Agent::alloc(bot.kind())).collect();

    // for (i, copy) in top_copies.iter().enumerate() {
    //     for port in top_ports.iter() {
    //         copy.set(*port, bot_copies[*port]);
    //     }
    //     let (j, root) = bot.get(i);
    //     copy.set(kind::PRINCIPAL, root);
    //     root.set(j, *copy);
    // }

    // for (i, copy) in bot_copies.iter().enumerate() {
    //     for port in bot_ports.iter() {
    //         copy.set(*port, top_copies[*port]);
    //     }
    //     let (j, root) = bot.get(i);
    //     copy.set(kind::PRINCIPAL, root);
    //     root.set(j, *copy);
    // }

    // top.delete();
    // bot.delete();
    // (top_copies, bot_copies)
}

unsafe fn annihilate(agent1: Wire, agent2: Wire) {
    // let kind = agent1.kind();
    // let iter = kind.auxiliary_ports()
    //     .iter()
    //     .zip(kind.auxiliary_ports().iter().rev());
    // for (top, bot) in iter {
    //     let (top_port, top_agent) = agent1.wire(*top);
    //     let (bot_port, bot_agent) = agent2.wire(*bot);
    //     top_agent.connect(top_port, bot_port, bot_agent);
    // }
    // agent1.delete();
    // agent2.delete();
}

unsafe fn active(agent: Wire) -> Vec<Wire> {
    todo!()
    // let mut active = vec![];
    // let mut traversal = vec![agent];
    // let mut visited = HashSet::new();
    // while let Some(agent) = traversal.pop() {
    //     if visited.contains(&agent) { continue; }
    //     visited.insert(agent);
    //     let (port, _) = agent.wire(kind::PRINCIPAL);
    //     if port == kind::PRINCIPAL { active.push(agent); }
    //     for p in agent.kind().ports() {
    //         let (_, next_agent) = agent.wire(*p);
    //         traversal.push(next_agent);
    //     }
    // }
    // active
}

unsafe fn reduce(agent: Wire) {
    todo!()
    // let mut active = active(agent);
    // while let Some(agent) = active.pop() {
    //     let (other_port, other_agent) = agent.wire(kind::PRINCIPAL);
    //     if other_port != kind::PRINCIPAL { continue; }
    //     match (agent.kind(), other_agent.kind()) {
    //         | (Kind::Apply, Kind::Lam)
    //         | (Kind::Lam, Kind::Apply) => { 
    //             let (lam, app) =
    //                 if agent.kind() == Kind::Lam { (agent, other_agent) }
    //                 else { (other_agent, agent) };
    //             let (scope1, scope2) = beta(app, lam);
    //             active.push(scope1);
    //             active.push(scope2);
    //         }
    //         (f, g) if f == g => {
    //             annihilate(agent, other_agent)
    //         }
    //         _ => {
    //             let (top_copies, bot_copies) = commute(agent, other_agent);
    //             active.extend(top_copies);
    //             active.extend(bot_copies);
    //         }
    //     }
    // }
}

pub fn normalize(agent: Wire) -> Wire {
    unsafe {
        reduce(agent);
        agent
    }
}

mod tests {
    pub use super::*;

    #[test]
    fn test_basic() {
        unsafe {
            init();
            let var_x = "x".to_string();
            let id_syntax = Syntax::Abs(var_x.clone(), Syntax::Var(var_x.clone()).boxed()).boxed();
            let delta_syntax = Syntax::Abs(var_x.clone(),
                Syntax::App(
                    Syntax::Var(var_x.clone()).boxed(),
                    Syntax::Var(var_x.clone()).boxed()
                ).boxed()
            ).boxed();
            let example1 = Syntax::App(id_syntax.clone(), id_syntax.clone()).boxed();

            let agent = from_syntax(*example1);
            let agent = normalize(agent);
            dbg!(heap());
            println!("{}", to_dot())
        }
    }
}
