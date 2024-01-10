use std::{
    fmt::Debug,
    sync::{Arc, RwLock, Weak},
};

use either::Either;
use sharded_slab::Slab;
use smol_str::SmolStr;

use crate::util::Map;

use super::Value;

#[derive(Clone)]
pub struct Scope {
    slab: Either<Arc<Slab<RwLock<Value>>>, Weak<Slab<RwLock<Value>>>>,
    map: Map<SmolStr, usize>,
}

impl Debug for Scope {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut lala = Map::new();
        for (k, v) in self.map.iter() {
            lala.insert(
                k,
                match &self.slab {
                    Either::Left(a) => Some(a.get(*v).unwrap().read().unwrap().clone()),
                    Either::Right(_) => None,
                },
            );
        }
        lala.fmt(f)
    }
}

impl Scope {
    /// Creates a new scope.
    pub fn new() -> Self {
        Self {
            slab: Either::Left(Default::default()),
            map: Default::default(),
        }
    }

    /// "Weakens" the handle to the heap, so that it holds on to the heap weakly rather than keeping it alive.
    pub fn weaken(&mut self) {
        let new_slab = match &self.slab {
            Either::Left(inner) => Either::Right(Arc::downgrade(inner)),
            Either::Right(inner) => Either::Right(inner.clone()),
        };
        self.slab = new_slab;
    }

    fn slab(&self) -> Arc<Slab<RwLock<Value>>> {
        match &self.slab {
            Either::Left(slab) => slab.clone(),
            Either::Right(slab) => slab.upgrade().unwrap().clone(),
        }
    }

    /// Inserts a new variable into the scope.
    pub fn insert(&mut self, var: SmolStr, value: Value) {
        let id = self.slab().insert(value.into()).unwrap();
        self.map.insert(var, id);
    }

    /// Mutates *the data that this variable points to*, without rebinding the variable. Returns None if there is no such variable.
    pub fn mutate(&self, var: &str, value: Value) -> Option<()> {
        let id = *self.map.get(var)?;
        let slab = self.slab();
        let cell = slab.get(id)?;
        *cell.write().unwrap() = value;
        Some(())
    }

    /// Looks up a variable.
    pub fn get(&self, var: &str) -> Option<Value> {
        let id = self.map.get(var)?;
        Some(self.slab().get(*id)?.read().unwrap().clone())
    }
}
