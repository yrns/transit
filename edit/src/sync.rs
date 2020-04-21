use indexmap::{IndexMap, IndexSet};
use std::hash::Hash;

// A collection that synchronizes with an external iterator of a
// different type. The collection can be indexed by the external type
// and has its own order so that the elements can be reordered for
// painting and event handling.

pub struct SyncMap<T, U> {
    insert: Box<dyn Fn(T) -> U>,
    map: IndexMap<T, U>,
}

impl<T, U> SyncMap<T, U>
where
    T: Copy + Hash + Eq,
{
    pub fn new(f: impl Fn(T) -> U + 'static) -> Self {
        Self {
            insert: Box::new(f),
            map: IndexMap::new(),
        }
    }

    pub fn sync(&mut self, iter: impl Iterator<Item = T>) -> bool {
        let c = self.map.len();
        let mut set = IndexSet::new();

        for t in iter {
            set.insert(t);

            // insert new elements if missing
            if !self.map.contains_key(&t) {
                self.map.insert(t, (self.insert)(t));
            }
        }

        // remove elements not in the set
        self.map.retain(|t, _| set.contains(t));

        c != self.map.len()
    }

    // remove the element and place it last
    pub fn raise(&mut self, t: T) {
        if let Some(u) = self.map.shift_remove(&t) {
            self.map.insert(t, u);
        }
    }

    pub fn clear(&mut self) {
        self.map.clear()
    }

    pub fn len(&self) -> usize {
        self.map.len()
    }

    pub fn iter(&self) -> impl Iterator<Item = (&T, &U)> {
        self.map.iter()
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = (&T, &mut U)> {
        self.map.iter_mut()
    }

    pub fn values(&self) -> impl DoubleEndedIterator<Item = &U> {
        self.map.values()
    }

    pub fn values_mut(&mut self) -> impl DoubleEndedIterator<Item = &mut U> {
        self.map.values_mut()
    }
}
