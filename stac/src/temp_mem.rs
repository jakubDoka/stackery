use std::{cell::UnsafeCell, iter::TrustedLen};

use mini_alloc::{ArenaBase, ArenaScope, Diver, DiverBase};

#[derive(Default)]
pub struct TempMemBase {
    arena: ArenaBase,
    diver: DiverBase,
}

impl TempMemBase {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn scope(&mut self) -> TempMem {
        TempMem {
            arena: self.arena.scope(),
            diver: self.diver.untyped_dive().into(),
        }
    }

    pub fn diver_and_arena(&mut self) -> (Diver, ArenaScope) {
        let arena = self.arena.scope();
        let diver = self.diver.untyped_dive();
        (diver, arena)
    }
}

pub struct TempMem<'a> {
    arena: ArenaScope<'a>,
    diver: UnsafeCell<Diver<'a>>,
}

impl<'a> TempMem<'a> {
    pub fn collect_trusted_len<I>(&self, iter: I) -> &mut [I::Item]
    where
        I: IntoIterator,
        I::IntoIter: ExactSizeIterator + TrustedLen,
    {
        self.arena.alloc_iter(iter)
    }

    pub fn collect<I>(&self, iter: I) -> &mut [I::Item]
    where
        I: IntoIterator,
    {
        let mut dive = unsafe { (*self.diver.get()).dive::<I::Item>() };
        dive.extend(iter);
        self.arena.alloc_rev_iter(dive)
    }
}
