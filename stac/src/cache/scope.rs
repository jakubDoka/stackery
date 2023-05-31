use mini_alloc::InternedStr;

pub type Repr = u16;

#[derive(Default)]
pub struct ScopeCache {
    inner: crate::CacheVec<InternedStr>,
}

impl ScopeCache {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn view(&self, range: ScopeRange) -> &ScopeView {
        unsafe { std::mem::transmute(self.inner.view(range.inner)) }
    }
}

pub struct ScopeView {
    inner: [InternedStr],
}

impl ScopeView {
    pub fn name(&self, sym: Sym) -> InternedStr {
        self.inner[sym.0 as usize]
    }

    pub fn resolve(&self, name: InternedStr) -> Option<Sym> {
        self.inner
            .iter()
            .rposition(|&sym| sym == name)
            .map(|index| Sym::new(index))
    }

    pub fn all(&self) -> &[InternedStr] {
        &self.inner
    }
}

#[derive(Clone, Copy, Default)]
pub struct ScopeRange {
    inner: crate::CacheVecRange<InternedStr>,
}

#[derive(Default)]
pub struct Scope {
    pub sym_stack: Vec<InternedStr>,
}

impl Scope {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn push(&mut self, sym: InternedStr) -> Sym {
        let sym_id = self.sym_stack.len();
        self.sym_stack.push(sym);
        Sym::new(sym_id)
    }

    pub fn resolve(&self, name: InternedStr) -> Option<Sym> {
        self.sym_stack
            .iter()
            .rposition(|&sym| sym == name)
            .map(|index| Sym::new(index))
    }

    pub fn start_frame(&mut self) -> ScopeFrame {
        ScopeFrame {
            sym_stack_len: self.sym_stack.len(),
        }
    }

    pub fn end_frame(&mut self, frame: ScopeFrame) {
        self.sym_stack.truncate(frame.sym_stack_len);
    }

    pub fn cache(&mut self, cache: &mut ScopeCache) -> ScopeRange {
        let mut push = cache.inner.push();
        push.extend(self.sym_stack.drain(..));
        ScopeRange {
            inner: push.finish(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Sym(Repr);

impl Sym {
    pub fn new(index: usize) -> Self {
        Self(index.try_into().expect("exceeded sym count limit"))
    }

    pub fn index(self) -> usize {
        self.0 as usize
    }
}

pub struct ScopeFrame {
    sym_stack_len: usize,
}
