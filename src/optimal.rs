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
            index: 0
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
        pub const ROOT: usize = 0;
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
        let header: Kind = self.header().into();
        let wires = [
            format!("{}:{}", self.port(0), self.agent(0)),
            format!("{}:{}", self.port(1), self.agent(1)),
            format!("{}:{}", self.port(2), self.agent(2)),
            format!("{}:{}", self.port(3), self.agent(3)),];
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

    #[inline]
    unsafe fn delete(&self) {
        *heap().get_mut(self.agent()) = Node::new_null();
        heap().delete(self.agent());
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

    unsafe fn scope_level(&self) -> Option<usize> {
        match self.kind() {
            Kind::Scope => Some(self.load(kind::scope::LEVEL) as _),
            Kind::Dup => Some(self.load(kind::dup::LEVEL) as _),
            _ => None
        }
    }

    unsafe fn set_scope_level(&self, level: usize) {
        match self.kind() {
            Kind::Scope => self.store(kind::scope::LEVEL, 0, level),
            Kind::Dup => self.store(kind::dup::LEVEL, 0, level),
            _ => { }
        }
    }

    unsafe fn repetition_count(&self) -> Option<usize> {
        match self.kind() {
            Kind::Scope => Some(self.load(kind::scope::VALUE) as _),
            _ => None
        }
    }

    unsafe fn set_repetition_count(&self, value: usize) {
        if self.kind() == Kind::Scope { 
            self.store(kind::scope::VALUE, 0, value)
        }
    }

    unsafe fn flip(&self) -> Wire {
        let node = heap().get(self.agent());
        let data = node.wire(self.port());
        Wire(data)
    }

    unsafe fn structural(&self) -> bool {
        use Kind::*;
        matches!(self.kind(), Root | Lam | Def | Apply)
    }

    unsafe fn is_lam_bind(&self) -> bool {
        self.kind() == Kind::Lam && self.port() == kind::lam::BIND
    }

    #[inline]
    unsafe fn active(&self) -> bool {
        let other = self.flip();
        if (self.kind() == Kind::Root && other.structural())
            || (other.kind() == Kind::Root && self.structural())
        {
            false
        } else {
            let eraser_opt =
                (self.kind() == Kind::Erase && !other.structural())
                || (other.kind() == Kind::Erase && !self.structural());
            let principal_pairs =
                self.port() == kind::PRINCIPAL && other.port() == kind::PRINCIPAL;
            eraser_opt || principal_pairs
        }
    }

    #[inline]
    unsafe fn is_loop(&self) -> bool {
        self.agent() == self.flip().agent()
    }

    #[inline]
    unsafe fn alive(&self) -> bool {
        !heap().free.contains(&self.agent())
        && self.kind() != Kind::Null
    }

    unsafe fn increment_scope_level(&self, other: Wire) {
        match other.kind() {
            Kind::Lam | Kind::Scope => {
                let value = match other.kind() {
                    Kind::Lam => 1,
                    Kind::Scope => {
                        self.load(kind::scope::VALUE) + 1
                    }
                    _ => unreachable!()
                };
                match self.kind() {
                    Kind::Dup => {
                        let level = self.load(kind::dup::LEVEL);
                        let level: usize = (level + value) as _;
                        self.store(kind::dup::LEVEL, 0, level);
                    }
                    Kind::Scope => {
                        let level = self.load(kind::scope::LEVEL);
                        let level: usize = (level + value) as _;
                        self.store(kind::scope::LEVEL, 0, level);
                    }
                    _ => { }
                }
            }
            _ => { }
        }
    }

    unsafe fn connect(&self, other: Wire) {
        self.store(self.port(), other.port() as _, other.agent() as _);
        other.store(other.port(), self.port() as _, self.agent() as _);
    }

    unsafe fn connect_many(&self, wires: &[Wire]) {
        if wires.is_empty() {
            let eraser = Wire::alloc(Kind::Erase).with(kind::PRINCIPAL);
            self.connect(eraser);
        } else {
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
                let next_wire = wire.with(kind::PRINCIPAL).flip();
                wire.delete();
                *wire = next_wire;
            }
        }
    }
    fn inner(syntax: Syntax, root: Wire, bound: &mut Vec<(String, Vec<Wire>)>) -> Wire {
        dbg!("from_syntax");
        stacker::maybe_grow(32 * KB, 4 * MB, || {
            inner_helper(syntax, root, bound)
        }) 
    }
    fn inner_helper(syntax: Syntax, root: Wire, bound: &mut Vec<(String, Vec<Wire>)>) -> Wire {
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
                        var.store(kind::scope::BODY, root.port(), root.agent());
                        var.store(kind::scope::LEVEL, 0, 0);
                        var.store(kind::scope::VALUE, 0, (index - 1) as _);
                        vars.push(var.with(kind::scope::INPUT));
                        var.with(kind::scope::BODY)
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

unsafe fn dot_metadata(wire: Wire) -> String {
    let (id, attr) = match wire.kind() {
        Kind::Null => todo!(),
        Kind::Root => {
            let id = "r".to_string();
            let attr = "s";
            (id, attr)
        }
        Kind::Lam => {
            let id = "l".to_string();
            let attr = match wire.port() {
                kind::lam::ROOT => "n",
                kind::lam::BODY => "se",
                kind::lam::BIND => "sw",
                _ => unreachable!()
            };
            (id, attr)
        }
        Kind::Def => todo!(),
        Kind::Apply => {
            let id = "a".to_string();
            let attr = match wire.port() {
                kind::apply::ROOT => "n",
                kind::apply::FUNCTION => "sw",
                kind::apply::ARGUMENT => "se",
                _ => unreachable!()
            };
            (id, attr)
        }
        Kind::Dup => {
            let level = wire.load(kind::dup::LEVEL);
            let id = format!("d{}", level);
            let attr = match wire.port() {
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
            let attr = match wire.port() {
                kind::scope::INPUT => "n",
                kind::scope::BODY => "s",
                _ => unreachable!()
            };
            (id, attr)
        }
        Kind::Erase => {
            let id = "e".to_string();
            let attr = match wire.port() {
                kind::PRINCIPAL => "s",
                _ => unreachable!()
            };
            (id, attr)
        }
    };
    format!("g{}{}:{}", wire.agent(), id, attr)
}



pub unsafe fn to_dot(start: Wire, length: usize) -> String {
    let mut result = "".to_string();
    let mut duplicate = HashSet::new();
    let mut traverse = vec![(start, 0)];
    while let Some((wire, dist)) = traverse.pop() {
        if dist >= length { continue }
        for &port in wire.kind().ports() {
            let other = wire.with(port).flip();
            let lhs = dot_metadata(wire.with(port));
            let rhs = dot_metadata(other);
            if !duplicate.contains(&(lhs.clone(), rhs.clone())) {
                let edge = format!("    {} -- {}\n", lhs.clone(), rhs.clone());
                result.push_str(edge.as_str());
                duplicate.insert((lhs.clone(), rhs.clone()));
                duplicate.insert((rhs.clone(), lhs.clone()));
            }
            traverse.push((wire.with(port).flip(), dist + 1));
        }
    }
    format!("graph {{\n{}}}", result)
}

pub fn to_syntax(agent: Wire) -> Syntax {
    unsafe fn next_structural_from(mut wire: Wire) -> Wire {
        loop {
            wire = wire.flip();
            match wire.kind() {
                Kind::Null => unreachable!(),
                Kind::Root => unreachable!(),
                Kind::Lam => break,
                Kind::Def => unreachable!(),
                Kind::Apply => break,
                Kind::Dup => {
                    wire = wire.with(kind::dup::INPUT);
                }
                Kind::Scope => {
                    if wire.port() == kind::scope::INPUT {
                        wire = wire.with(kind::scope::BODY);
                    } else {
                        wire = wire.with(kind::scope::INPUT);
                    }
                }
                Kind::Erase => unreachable!(),
            }
        }
        wire
    }
    unsafe fn handle_variable(wire: Wire, context: &mut HashMap<usize, String>) -> Option<Syntax> {
        if wire.kind() == Kind::Lam && wire.port() == kind::lam::BIND {
            let name = context.get(&wire.agent()).cloned()?;
            Some(Syntax::Var(name))
        } else {
            None
        }
    }
    unsafe fn inner(wire: Wire, supply: &mut usize, context: &mut HashMap<usize, String>) -> Syntax {
        stacker::maybe_grow(32 * KB, 4 * MB, || {
            inner_helper(wire, supply, context)
        }) 
    }
    unsafe fn inner_helper(wire: Wire, supply: &mut usize, context: &mut HashMap<usize, String>) -> Syntax {
        match wire.kind() {
            Kind::Null => unreachable!(),
            Kind::Root => {
                let wire = wire.with(kind::root::ROOT).flip();
                inner(wire, supply, context)
            }
            Kind::Lam => {
                let name = format!("x{}", supply);
                *supply += 1;
                context.insert(wire.agent(), name.clone());
                let body = next_structural_from(wire.with(kind::lam::BODY));
                let body = handle_variable(body, context).unwrap_or_else(|| inner(body, supply, context));
                Syntax::Abs(name, body.boxed())
            }
            Kind::Def => unreachable!(),
            Kind::Apply => {
                let fun = next_structural_from(wire.with(kind::apply::FUNCTION));
                let fun = handle_variable(fun, context).unwrap_or_else(|| inner(fun, supply, context));
                let arg = next_structural_from(wire.with(kind::apply::ARGUMENT));
                let arg = handle_variable(arg, context).unwrap_or_else(|| inner(arg, supply, context));
                Syntax::App(fun.boxed(), arg.boxed())
            }
            Kind::Dup => unreachable!(),
            Kind::Scope => unreachable!(),
            Kind::Erase => unreachable!(),
        }
    }
    unsafe {
        let mut supply = 0;
        let mut context = HashMap::new();
        inner(agent, &mut supply, &mut context)
    }
}

unsafe fn beta(app: Wire, lam: Wire) -> (Wire, Wire) {
    let var_scope = Wire::alloc(Kind::Scope);
    let body_scope = Wire::alloc(Kind::Scope);

    // Connect app wires
    let root = app.with(kind::apply::ROOT).flip();
    body_scope.with(kind::scope::INPUT).connect(root);
    let arg = app.with(kind::apply::ARGUMENT).flip();
    var_scope.with(kind::scope::INPUT).connect(arg);

    // Lambda wires may have a self-loop
    if lam.with(kind::lam::BIND).is_loop() {
        body_scope.with(kind::scope::BODY).connect(var_scope.with(kind::scope::BODY));
    } else {
        let body = lam.with(kind::lam::BODY).flip();
        body_scope.with(kind::scope::BODY).connect(body);
        let bind = lam.with(kind::lam::BIND).flip();
        var_scope.with(kind::scope::BODY).connect(bind);
    }

    app.delete();
    lam.delete();
    (var_scope, body_scope)
}

unsafe fn commute(top: Wire, bot: Wire) -> (Vec<Wire>, Vec<Wire>) {
    if top.kind() == Kind::Root || bot.kind() == Kind::Root {
        let (root, other) = if top.kind() == Kind::Root { (top, bot) } else { (bot, top) };
        commute_root(root, other)
    } else {
        commute_general(top, bot)
    }
}

unsafe fn commute_erase(eraser: Wire, other: Wire) -> Vec<Wire> {
    let mut result = vec![];
    match other.kind() {
        Kind::Null => todo!(),
        Kind::Root => todo!(),
        Kind::Lam => todo!(),
        Kind::Def => todo!(),
        Kind::Apply => {
            match other.port() {
                kind::apply::ARGUMENT => {
                    let eraser2 = Wire::alloc(Kind::Erase).with(kind::PRINCIPAL);
                    let root = other.with(kind::apply::ROOT).flip();
                    let fun = other.with(kind::apply::FUNCTION).flip();
                    eraser2.connect(root);
                    eraser.connect(fun);
                    other.delete();
                    result.push(eraser);
                    result.push(eraser2);
                }
                kind::apply::FUNCTION => unreachable!(),
                kind::apply::ROOT => {
                    let eraser2 = Wire::alloc(Kind::Erase).with(kind::PRINCIPAL);
                    let arg = other.with(kind::apply::ARGUMENT).flip();
                    let fun = other.with(kind::apply::FUNCTION).flip();
                    eraser2.connect(arg);
                    eraser.connect(fun);
                    other.delete();
                    result.push(eraser);
                    result.push(eraser2);
                }
                _ => unreachable!()
            }
        }
        Kind::Dup => {
            match other.port() {
                kind::dup::INPUT => unreachable!(),
                kind::dup::AUX1 => {
                    let input = other.with(kind::dup::INPUT).flip();
                    let aux2 = other.with(kind::dup::AUX2).flip();
                    input.connect(aux2);
                    eraser.delete();
                    other.delete();
                    result.push(input);
                    result.push(aux2);
                }
                kind::dup::AUX2 => {
                    let input = other.with(kind::dup::INPUT).flip();
                    let aux1 = other.with(kind::dup::AUX1).flip();
                    input.connect(aux1);
                    eraser.delete();
                    other.delete();
                    result.push(input);
                    result.push(aux1);
                }
                _ => unreachable!()
            }
        }
        Kind::Scope => {
            match other.port() {
                kind::scope::INPUT => unreachable!(),
                kind::scope::BODY => {
                    let other = other.with(kind::scope::INPUT);
                    eraser.connect(other.flip());
                    other.delete();
                    result.push(eraser);
                }
                _ => unreachable!()
            }
        }
        Kind::Erase => unreachable!()
    }
    result
}

unsafe fn commute_root(root: Wire, other: Wire) -> (Vec<Wire>, Vec<Wire>) {
    match other.kind() {
        Kind::Null => todo!(),
        Kind::Root => todo!(),
        Kind::Lam => todo!(),
        Kind::Def => todo!(),
        Kind::Apply => todo!(),
        Kind::Dup => todo!(),
        Kind::Scope => {
            root.connect(other.with(kind::scope::BODY).flip());
            other.delete();
            (vec![root], vec![])
        }
        Kind::Erase => todo!(),
    }
}

unsafe fn commute_general(top: Wire, bot: Wire) -> (Vec<Wire>, Vec<Wire>) {
    let mut loops = vec![];
    let top_kind = top.kind();
    let bot_kind = bot.kind();
    let top_ports = top_kind.auxiliary_ports();
    let bot_ports = bot_kind.auxiliary_ports();

    let top_copies: Vec<_> = bot_ports.iter()
        .map(|&port| {
            let wire = Wire::alloc(top.kind()).with(kind::PRINCIPAL);
            if let Some(level) = top.scope_level() { wire.set_scope_level(level); }
            if let Some(value) = top.repetition_count() { wire.set_repetition_count(value); }
            if bot.with(port).is_loop() {
                let temp = Wire::alloc(Kind::Null);
                bot.with(port).flip().connect(temp.with(kind::null::AUX1));
                bot.with(port).connect(temp.with(kind::null::AUX2));
                loops.push(temp);
            }
            wire.increment_scope_level(bot);
            wire
        })
        .collect();

    for (i, &port) in bot_ports.iter().enumerate() {
        let wire = top_copies[i];
        let other = bot.with(port).flip();
        wire.connect(other);
    }

    let bot_copies: Vec<_> = top_ports.iter()
        .map(|&port| {
            let wire = Wire::alloc(bot.kind()).with(kind::PRINCIPAL);
            if let Some(level) = bot.scope_level() { wire.set_scope_level(level); }
            if let Some(value) = bot.repetition_count() { wire.set_repetition_count(value); }
            if top.with(port).is_loop() {
                let temp = Wire::alloc(Kind::Null);
                top.with(port).flip().connect(temp.with(kind::null::AUX1));
                top.with(port).connect(temp.with(kind::null::AUX2));
                loops.push(temp);
            }
            wire.increment_scope_level(top);
            wire
        })
        .collect();

    for (i, &port) in top_ports.iter().enumerate() {
        let wire = bot_copies[i];
        let other = top.with(port).flip();
        wire.connect(other);
    }

    // Fix loops
    for temp in loops.drain(..) {
        let start = temp.with(kind::null::AUX1).flip();
        let end = temp.with(kind::null::AUX2).flip();
        start.connect(end);
        temp.delete();
    }

    for (i, top) in top_copies.iter().enumerate() {
        let bot_port = i + 1;
        for (j, &port) in top_ports.iter().enumerate() {
            let bot = bot_copies[j].with(bot_port);
            top.with(port).connect(bot);
        }
    }

    top.delete();
    bot.delete();
    (top_copies, bot_copies)
}

unsafe fn annihilate(wire1: Wire, wire2: Wire) {
    let kind = wire1.kind();
    if kind == Kind::Scope {
        let value1 = wire1.load(kind::scope::VALUE) + 1;
        let value2 = wire2.load(kind::scope::VALUE) + 1;
        match value1.cmp(&value2) {
            std::cmp::Ordering::Less => {
                let value2 = value2 - value1 - 1;
                wire2.store(kind::scope::VALUE, 0, value2 as _);
                wire2.connect(wire1.with(kind::scope::BODY).flip());
                wire1.delete();
            }
            std::cmp::Ordering::Equal => {
                wire1.with(kind::scope::BODY).flip()
                    .connect(wire2.with(kind::scope::BODY).flip());
                wire1.delete();
                wire2.delete();
            }
            std::cmp::Ordering::Greater => {
                let value1 = value1 - value2 - 1;
                wire1.store(kind::scope::VALUE, 0, value1 as _);
                wire1.connect(wire1.with(kind::scope::BODY).flip());
                wire2.delete();
            }
        }
    } else {
        for &port in kind.auxiliary_ports().iter() {
            let top = wire1.with(port).flip();
            let bot = wire2.with(port).flip();
            top.connect(bot);
        }
        wire1.delete();
        wire2.delete();
    }
}

unsafe fn active(wire: Wire) -> Vec<Wire> {
    let mut active = vec![];
    let mut traversal = vec![wire];
    let mut visited = HashSet::new();
    while let Some(wire) = traversal.pop() {
        let agent = wire.agent();
        if visited.contains(&agent) { continue; }
        visited.insert(agent);
        for &p in wire.kind().ports() {
            if wire.with(p).active() && wire.alive() { active.push(wire.with(p)); }
            let next_wire = wire.with(p).flip();
            traversal.push(next_wire);
        }
    }
    active
}

unsafe fn reduce(mut active: Vec<Wire>, gas: &mut Option<usize>) {
    while let Some(wire) = active.pop() {
        if !wire.active() || !wire.alive() { continue; }
        if gas.is_some() && (*gas).unwrap() == 0 { break; }
        *gas = gas.map(|g| g - 1);
        println!("{}", to_dot(wire, 2));
        dbg!((wire.kind(), wire.agent(), wire.port()));
        dbg!((wire.flip().kind(), wire.flip().agent(), wire.flip().port()));
        match (wire.kind(), wire.flip().kind()) {
            | (Kind::Apply, Kind::Lam)
            | (Kind::Lam, Kind::Apply) => { 
                let (lam, app) =
                    if wire.kind() == Kind::Lam { (wire, wire.flip()) }
                    else { (wire.flip(), wire) };
                let (scope1, scope2) = beta(app, lam);
                active.push(scope1);
                active.push(scope2);
            }
            (f, g)
            if f == g
            && wire.scope_level() == wire.flip().scope_level()
            => {
                annihilate(wire, wire.flip());
            }
            | (Kind::Erase, _)
            | (_, Kind::Erase)
            if wire.port() != kind::PRINCIPAL || wire.flip().port() != kind::PRINCIPAL
            => {
                let (eraser, other) =
                    if wire.kind() == Kind::Erase { (wire, wire.flip()) }
                    else { (wire.flip(), wire) };
                let new = commute_erase(eraser, other);
                active.extend(new);
            }
            _ => {
                let (top_copies, bot_copies) = commute(wire, wire.flip());
                active.extend(top_copies);
                active.extend(bot_copies);
            }
        }
    }
}

pub fn normalize(agent: Wire, mut gas: Option<usize>) -> Wire {
    unsafe {
        loop {
            if gas.is_some() && gas.unwrap() == 0 { break; }
            let active = active(agent);
            if active.is_empty() { break; }
            reduce(active, &mut gas);
        }
        agent
    }
}

// mod tests {
//     pub use super::*;

//     #[test]
//     fn test_basic() {
//         unsafe {
//             init();
//             let var_x = "x".to_string();
//             let var_y = "y".to_string();
//             let var_z = "z".to_string();
//             let id_syntax = Syntax::Abs(var_x.clone(), Syntax::Var(var_x.clone()).boxed()).boxed();
//             let id_id = Syntax::App(
//                 id_syntax.clone(),
//                 id_syntax.clone()
//             ).boxed();
//             let delta_syntax = Syntax::Abs(var_x.clone(),
//                 Syntax::App(
//                     Syntax::Var(var_x.clone()).boxed(),
//                     Syntax::Var(var_x.clone()).boxed()
//                 ).boxed()
//             ).boxed();
//             let example1 = Syntax::App(id_syntax.clone(), id_syntax.clone()).boxed();
//             let one = Syntax::Abs(var_x.clone(),
//                 Syntax::Abs(var_y.clone(),
//                     Syntax::App(
//                         Syntax::Var(var_y.clone()).boxed(),
//                         Syntax::Var(var_x.clone()).boxed()
//                     ).boxed()
//                 ).boxed()
//             ).boxed();
//             let simple_let = 
//                 Syntax::Let("b0".to_string(), id_syntax.clone(),
//                     Syntax::Let(var_z.clone(),
//                             Syntax::App(delta_syntax.clone(),
//                                 Syntax::Var("b0".to_string()).boxed()
//                             ).boxed(),
//                         Syntax::Let("b1".to_string(), 
//                             Syntax::App(delta_syntax.clone(),
//                                 Syntax::Var(var_z.clone()).boxed()
//                             ).boxed(),
//                             Syntax::App(
//                                 Syntax::Var("b1".to_string()).boxed(),
//                                 Syntax::Var("b1".to_string()).boxed(),
//                             ).boxed()
//                         ).boxed()
//                     ).boxed()
//                 ).boxed();

//             let agent = from_syntax(*simple_let);
//             let agent = normalize(agent, None);
//             dbg!(heap());
//             println!("{}", to_dot());
//             //let normal_syntax = to_syntax(agent);
//             //dbg!(normal_syntax);
//         }
//     }
// }
