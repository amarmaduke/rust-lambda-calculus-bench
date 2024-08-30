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
}

static mut HEAP : MaybeUninit<Heap<Node>> = MaybeUninit::uninit();

pub unsafe fn init() {
    HEAP.write(Heap::new());
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
            Kind::Root => &[0],
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
            Kind::Root => &[],
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Node([u32; 4]);

impl Node {
    const HEADER_MASK: u32 = 0xC0000000;
    const SLOT_MASK: u32 = 0x3FFFFFFF;

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
        let b0 = slots[0] & Node::HEADER_MASK >> (32 - 8);
        let b1 = slots[1] & Node::HEADER_MASK >> (32 - 6);
        let b2 = slots[2] & Node::HEADER_MASK >> (32 - 4);
        let b3 = slots[3] & Node::HEADER_MASK >> (32 - 2);
        let data = b3 | b2 | b1 | b0;
        data as _
    }

    fn set_header(&mut self, header: u8) {
        let slots = (*self.inner()).map(|x| x & Node::SLOT_MASK);
        let inner = self.inner_mut();
        let b0 = ((header & 0x03) as u32) << (32 - 2);
        let b1 = ((header & 0x0C) as u32) << (32 - 4);
        let b2 = ((header & 0x30) as u32) << (32 - 6);
        let b3 = ((header & 0xC0) as u32) << (32 - 8);
        inner[0] = b0 & slots[0];
        inner[1] = b1 & slots[1];
        inner[2] = b2 & slots[2];
        inner[3] = b3 & slots[3];
    }

    #[inline]
    fn slot(&self, id: usize) -> u32 {
        self.inner()[id] & Node::SLOT_MASK
    }

    #[inline]
    fn set_slot(&mut self, id: usize, data: u32) {
        let slots = self.inner_mut();
        slots[id] = (slots[id] & Node::HEADER_MASK) | (data & Node::SLOT_MASK)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Agent(u32);

impl From<usize> for Agent {
    #[inline]
    fn from(value: usize) -> Self {
        Agent(value as _)
    }
}

impl From<&Agent> for usize {
    #[inline]
    fn from(value: &Agent) -> Self {
        value.inner() as _
    }
}

impl Agent {
    #[inline]
    fn inner(&self) -> u32 {
        let Agent(inner) = self;
        *inner
    }

    unsafe fn kind(&self) -> Kind {
        let node = heap().get(self.into());
        node.header().into()
    }

    unsafe fn alloc(k : Kind) -> Agent {
        let mut node = Node::new_null();
        node.set_header(k.value());
        heap().alloc(node).into()
    }

    unsafe fn get(&self, port: usize) -> (usize, Agent) {
        let node = heap().get(self.into());
        let other_agent = node.slot(port);
        let other_node = heap().get(other_agent as _);
        let other_kind: Kind = other_node.header().into();
        let other_port = other_kind
            .ports()
            .iter()
            .find(|p| other_node.slot(**p) == self.inner())
            .unwrap();
        (*other_port, Agent(other_agent))
    }

    unsafe fn delete(&self) {
        let index = self.inner() as usize;
        heap().delete(index);
    }

    unsafe fn store(&self, port: usize, data: u32) {
        let node = heap().get_mut(self.into());
        node.set_slot(port, data)
    }

    #[inline(always)]
    unsafe fn set(&self, port: usize, agent: Agent) {
        self.store(port, agent.inner())
    }

    unsafe fn connect(&self, port: usize, wires: &[(usize, Agent)]) {
        let mut total = 1;
        let mut dups: Vec<(usize, Agent)> = vec![];
        while total < wires.len() {
            if total == 1 {
                let dup = Agent::alloc(Kind::Dup);
                dup.set(kind::dup::INPUT, *self);
                self.set(port, dup);
                dups.push((kind::dup::AUX1, dup));
                dups.push((kind::dup::AUX2, dup));
                total += 1;
            }
            else if total * 2 < wires.len() {
                let mut next_dups: Vec<(usize, Agent)> = vec![];
                for (port, dup) in dups.drain(..) {
                    let new_dup = Agent::alloc(Kind::Dup);
                    new_dup.set(kind::dup::INPUT, dup);
                    dup.set(port, new_dup);
                    next_dups.push((kind::dup::AUX1, new_dup));
                    next_dups.push((kind::dup::AUX2, new_dup));
                }
                dups = next_dups;
                total *= 2;
            } else {
                let remaining = wires.len() - total;
                let suffix = dups.split_off(remaining);
                let mut prefix: Vec<(usize, Agent)> = vec![];
                for (port, dup) in dups.drain(..) {
                    let new_dup = Agent::alloc(Kind::Dup);
                    new_dup.set(kind::dup::INPUT, dup);
                    dup.set(port, new_dup);
                    prefix.push((kind::dup::AUX1, new_dup));
                    prefix.push((kind::dup::AUX2, new_dup));
                }
                dups = prefix;
                dups.extend(suffix);
                total += remaining;
            }
        }
        for ((p1, a1), (p2, a2)) in dups.drain(..).zip(wires.iter()) {
            a1.set(p1, *a2);
            a2.set(*p2, a1);
        }
    }
}

pub fn from_syntax(syntax: Syntax) -> Agent {
    unsafe fn fix_nulls(data: &mut Vec<(usize, Agent)>) {
        for (port, agent) in data.iter_mut() {
            if agent.kind() == Kind::Null {
                let (next_port, next_agent) = agent.get(kind::PRINCIPAL);
                agent.delete();
                *port = next_port;
                *agent = next_agent;
            }
        }
    }
    fn inner(syntax: Syntax, root: Agent, bound: &mut Vec<(String, Vec<(usize, Agent)>)>) -> Agent {
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
                        let var = Agent::alloc(Kind::Null);
                        var.set(kind::null::PRINCIPAL, root);
                        vars.push((kind::null::AUX1, var));
                        var
                    } else {
                        let var = Agent::alloc(Kind::Scope);
                        var.set(kind::scope::INPUT, root);
                        var.store(kind::scope::LEVEL, 0);
                        var.store(kind::scope::VALUE, (index - 1) as _);
                        vars.push((kind::scope::BODY, var));
                        var
                    }
                }
                Syntax::Abs(name, body) => {
                    let lam = Agent::alloc(Kind::Lam);
                    lam.set(kind::lam::ROOT, root);
                    bound.push((name, vec![]));
                    let body = inner(*body, lam, bound);
                    let Some((_, mut vars)) = bound.pop() else { panic!() };
                    lam.set(kind::lam::BODY, body);
                    fix_nulls(&mut vars);
                    lam.connect(kind::lam::BIND, &vars);
                    lam
                }
                Syntax::App(f, a) => {
                    let app = Agent::alloc(Kind::Apply);
                    app.set(kind::apply::ROOT, root);
                    let f = inner(*f, app, bound);
                    let a = inner(*a, app, bound);
                    app.set(kind::apply::FUNCTION, f);
                    app.set(kind::apply::ARGUMENT, a);
                    app
                }
            }
        }
    }
    unsafe {
        let root = Agent::alloc(Kind::Root);
        let mut bound = vec![];
        let body = inner(syntax, root, &mut bound);
        root.set(kind::PRINCIPAL, body);
        root
    }
}

unsafe fn beta(app: Agent, lam: Agent) -> (Agent, Agent) {
    let var_scope = Agent::alloc(Kind::Scope);
    let body_scope = Agent::alloc(Kind::Scope);

    // Setup var_scope
    let (root_port, root) = app.get(kind::apply::ROOT);
    var_scope.set(kind::PRINCIPAL, root);
    root.set(root_port, var_scope);
    let (var_port, var) = lam.get(kind::lam::BIND);
    var_scope.set(kind::scope::BODY, var);
    var.set(var_port, var_scope);

    // Setup body_scope
    let (arg_port, arg) = app.get(kind::apply::ARGUMENT);
    body_scope.set(kind::PRINCIPAL, arg);
    arg.set(arg_port, body_scope);
    let (body_port, body) = lam.get(kind::lam::BODY);
    body_scope.set(kind::scope::BODY, body);
    body.set(body_port, body_scope);

    app.delete();
    lam.delete();
    (var_scope, body_scope)
}

unsafe fn commute(top: Agent, bot: Agent) -> (Vec<Agent>, Vec<Agent>) {
    let top_kind = top.kind();
    let bot_kind = bot.kind();
    let top_ports = top_kind.auxiliary_ports();
    let bot_ports = bot_kind.auxiliary_ports();
    let top_copies: Vec<_> = bot_ports.iter().map(|_| Agent::alloc(top.kind())).collect();
    let bot_copies: Vec<_> = top_ports.iter().map(|_| Agent::alloc(bot.kind())).collect();

    for (i, copy) in top_copies.iter().enumerate() {
        for port in top_ports.iter() {
            copy.set(*port, bot_copies[*port]);
        }
        let (j, root) = bot.get(i);
        copy.set(kind::PRINCIPAL, root);
        root.set(j, *copy);
    }

    for (i, copy) in bot_copies.iter().enumerate() {
        for port in bot_ports.iter() {
            copy.set(*port, top_copies[*port]);
        }
        let (j, root) = bot.get(i);
        copy.set(kind::PRINCIPAL, root);
        root.set(j, *copy);
    }

    top.delete();
    bot.delete();
    (top_copies, bot_copies)
}

unsafe fn annihilate(agent1: Agent, agent2: Agent) {
    let kind = agent1.kind();
    let iter = kind.auxiliary_ports()
        .iter()
        .zip(kind.auxiliary_ports().iter().rev());
    for (top, bot) in iter {
        let (top_port, top_agent) = agent1.get(*top);
        let (bot_port, bot_agent) = agent2.get(*bot);
        top_agent.set(top_port, bot_agent);
        bot_agent.set(bot_port, top_agent);
    }
    agent1.delete();
    agent2.delete();
}

unsafe fn active(agent: Agent) -> Vec<Agent> {
    let mut active = vec![];
    let mut traversal = vec![agent];
    let mut visited = HashSet::new();
    while let Some(agent) = traversal.pop() {
        if visited.contains(&agent) { continue; }
        visited.insert(agent);
        let (port, _) = agent.get(kind::PRINCIPAL);
        if port == kind::PRINCIPAL { active.push(agent); }
        for p in agent.kind().ports() {
            let (_, next_agent) = agent.get(*p);
            traversal.push(next_agent);
        }
    }
    active
}

unsafe fn reduce(agent: Agent) {
    let mut active = active(agent);
    while let Some(agent) = active.pop() {
        let (other_port, other_agent) = agent.get(kind::PRINCIPAL);
        if other_port != kind::PRINCIPAL { continue; }
        match (agent.kind(), other_agent.kind()) {
            | (Kind::Apply, Kind::Lam)
            | (Kind::Lam, Kind::Apply) => { 
                let (lam, app) =
                    if agent.kind() == Kind::Lam { (agent, other_agent) }
                    else { (other_agent, agent) };
                let (scope1, scope2) = beta(app, lam);
                active.push(scope1);
                active.push(scope2);
            }
            (f, g) if f == g => {
                annihilate(agent, other_agent)
            }
            _ => {
                let (top_copies, bot_copies) = commute(agent, other_agent);
                active.extend(top_copies);
                active.extend(bot_copies);
            }
        }
    }
}

pub fn normalize(agent: Agent) -> Agent {
    unsafe {
        reduce(agent);
        agent
    }
}

pub fn to_value(root: usize) -> Option<u64> {
    todo!()
}
