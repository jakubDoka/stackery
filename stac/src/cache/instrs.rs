use mini_alloc::InternedStr;

use crate::*;

#[derive(Copy, Clone)]
pub struct Label(u16);

pub struct Labels {
    counter: u16,
}

impl Labels {
    pub fn new() -> Self {
        Self { counter: 0 }
    }

    pub fn next(&mut self) -> Label {
        let label = Label(self.counter);
        self.counter = self.counter.checked_add(1).expect("too many labels");
        label
    }
}

#[derive(Copy, Clone)]
pub struct Local(u16);

pub struct Locals {
    counter: u16,
}

impl Locals {
    pub fn new() -> Self {
        Self { counter: 0 }
    }

    pub fn next(&mut self) -> Local {
        let local = Local(self.counter);
        self.counter = self.counter.checked_add(1).expect("too many locals");
        local
    }

    pub fn start_frame(&mut self) -> LocalFrame {
        LocalFrame(self.counter)
    }

    pub fn end_frame(&mut self, frame: LocalFrame) {
        self.counter = frame.0;
    }
}

pub struct LocalFrame(u16);

#[derive(Copy, Clone)]
pub enum Instr {
    Lit(CacheRef<LiteralKindAst>),
    Ident(Sym),
    Import(ModuleRef),
    Str(CacheRef<InternedStr>),
    ScopeStart,
    ScopeEnd,
    Unary(OpCode),
    Binary(OpCode),
    ArrayStart,
    ArrayEnd(bool),
    TupleStart,
    TupleEnd,
    Enum,
    Call,
    Decl(Sym),
    Index,
    Label(Label),
    Field(CacheRef<InternedStr>),
    Jump(Label),
    JumpIf(Label),
    Ret,
    Unknown,
}
