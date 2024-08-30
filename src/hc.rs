
use std::hash;
use std::ops::Deref;
use std::ptr;
use std::sync::{Arc, Weak};

use dashmap::DashMap;

#[derive(Debug, Clone)]
pub struct Hc<T>(Arc<T>);

impl<T> Hc<T> {
    pub fn inner(&self) -> &Arc<T> {
        let Hc(inner) = self;
        inner
    }

    pub fn as_ptr(&self) -> *const T {
        let Hc(inner) = self;
        Arc::as_ptr(inner)
    }
}

impl<T> PartialEq for Hc<T> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(self.inner(), other.inner())
    }
}
impl<T> Eq for Hc<T> { }

impl<T> hash::Hash for Hc<T> {
    #[inline]
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        ptr::hash(self.as_ptr(), state);
    }
}

impl<T> Deref for Hc<T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &Self::Target {
        self.inner().deref()
    }
}

impl<T> AsRef<T> for Hc<T> {
    #[inline]
    fn as_ref(&self) -> &T {
        self.inner().as_ref()
    }
}

impl<T> Hc<T> {
    pub fn to_weak(&self) -> WeakHc<T> {
        let weak = Arc::downgrade(self.inner());
        WeakHc(weak)
    }
}

#[derive(Debug, Clone)]
pub struct WeakHc<T>(Weak<T>);

impl<T> WeakHc<T> {
    pub fn inner(&self) -> &Weak<T> {
        let WeakHc(inner) = self;
        inner
    }

    pub fn upgrade(self) -> Option<Hc<T>> {
        self.inner()
            .upgrade()
            .map(|rc| Hc(rc))
    }
}

pub struct HcFactory<T: hash::Hash + Eq + Clone> {
    table: DashMap<T, WeakHc<T>>
}

impl<T: hash::Hash + Eq + Clone> HcFactory<T> {
    pub fn with_capacity(capacity: usize) -> HcFactory<T> {
        HcFactory {
            table: DashMap::with_capacity(capacity)
        }
    }

    pub fn get(&self, element: &T) -> Option<Hc<T>> {
       self.table
            .get(element)
            .and_then(|x| x.value().clone().upgrade())
    }

    pub fn make(&self, element: T) -> Hc<T> {
        if let Some(hc) = self.get(&element) {
            hc
        } else {
            let result = Hc(Arc::new(element.clone()));
            self.table.insert(element, result.to_weak());
            result
        }
    }

    pub fn clear(&self) {
        self.table.clear()
    }
}
