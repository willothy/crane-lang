use slotmap::{Key, SlotMap};
use std::{collections::HashMap, hash::Hash};

pub struct Interner<K, V>
where
    K: Key,
    V: Hash + PartialEq + Eq,
{
    inner: SlotMap<K, V>,
    map: HashMap<V, K>,
}

impl<K, V> Interner<K, V>
where
    K: Key,
    V: Hash + PartialEq + Eq + Clone,
{
    pub fn new() -> Self {
        Self {
            inner: SlotMap::with_key(),
            map: HashMap::new(),
        }
    }

    pub fn get_or_intern(&mut self, value: V) -> K {
        if let Some(key) = self.map.get(&value) {
            *key
        } else {
            let key = self.inner.insert(value);
            self.map.insert(self.inner.get(key).unwrap().clone(), key);
            key
        }
    }

    pub fn get(&self, key: K) -> Option<&V> {
        self.inner.get(key)
    }

    pub fn get_mut(&mut self, key: K) -> Option<&mut V> {
        self.inner.get_mut(key)
    }
}
