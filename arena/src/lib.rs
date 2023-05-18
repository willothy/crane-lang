//! A thread-safe, id-based arena allocator

use boxcar::Vec;
use crossbeam_queue::SegQueue;
use std::fmt::Debug;
use std::sync::atomic::{AtomicU32, Ordering};
use std::{
    num::NonZeroU32,
    sync::{Arc, RwLock},
};

#[derive(Debug, Clone, Copy)]
pub struct KeyData {
    index: u32,
    /// The version of the key. This is incremented every time the key is reused.
    /// An even value means the slot is occupied, an odd value means the slot is vacant.
    version: NonZeroU32,
}

impl KeyData {
    pub fn as_ffi(&self) -> u64 {
        (self.index as u64) | ((self.version.get() as u64) << 32)
    }

    pub fn from_ffi(data: u64) -> Self {
        Self {
            index: data as u32,
            version: unsafe { NonZeroU32::new_unchecked((data >> 32) as u32) },
        }
    }
}

#[derive(Debug)]
pub struct Slot<T> {
    inner: Option<Arc<RwLock<T>>>,
    /// The version of the slot. This is incremented every time the slot is reused.
    /// An even value means the slot is occupied, an odd value means the slot is vacant.
    version: AtomicU32,
}

impl<T> Default for Slot<T> {
    fn default() -> Self {
        Self {
            inner: None,
            version: AtomicU32::new(1),
        }
    }
}

#[derive(Debug)]
pub struct Arena<T> {
    inner: Vec<RwLock<Slot<T>>>,
    free_queue: SegQueue<u32>,
}

// https://codereview.stackexchange.com/a/141215
fn next_power_of_two(mut n: u32) -> u32 {
    n -= 1;

    n |= n >> 1;
    n |= n >> 2;
    n |= n >> 4;
    n |= n >> 8;
    n |= n >> 16;

    n + 1
}

impl<T> Arena<T> {
    pub fn new() -> Self {
        let new = Self {
            inner: Vec::with_capacity(128),
            free_queue: SegQueue::new(),
        };
        new.resize(128);
        new
    }

    pub fn with_capacity(capacity: usize) -> Self {
        let new = Self {
            inner: Vec::with_capacity(capacity),
            free_queue: SegQueue::new(),
        };
        new.resize(capacity);
        new
    }

    pub fn resize(&self, new_len: usize) {
        if new_len <= self.inner.len() {
            return;
        }

        for _ in self.inner.len()..=new_len {
            self.free_queue
                .push(self.inner.push(RwLock::new(Slot::default())) as u32);
        }
    }

    pub fn insert(&self, value: T) -> KeyData {
        let index = loop {
            match self.free_queue.pop() {
                Some(index) => break index,
                None => self.resize(next_power_of_two(self.inner.len() as u32 * 2) as usize),
            }
        };
        let mut slot = self.inner[index as usize].write().unwrap();
        let old_version = slot.version.fetch_add(1, Ordering::SeqCst);
        slot.inner.replace(Arc::new(RwLock::new(value)));

        KeyData {
            index,
            version: unsafe { NonZeroU32::new_unchecked(old_version + 1) },
        }
    }

    pub fn get(&self, key: KeyData) -> Option<Arc<RwLock<T>>> {
        let slot = self.inner.get(key.index as usize).unwrap().read().unwrap();
        if slot.version.load(Ordering::SeqCst) == key.version.get() {
            slot.inner.clone()
        } else {
            None
        }
    }

    pub fn remove(&self, key: KeyData) -> Option<Arc<RwLock<T>>> {
        let mut slot = self.inner.get(key.index as usize).unwrap().write().unwrap();
        if slot.version.load(Ordering::SeqCst) == key.version.get() {
            let old = slot.inner.take();
            slot.version.fetch_add(1, Ordering::SeqCst);
            self.free_queue.push(key.index);
            old
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::{
        sync::atomic::{AtomicBool, Ordering},
        thread,
    };

    #[test]
    fn test_get_before_remove() {
        let arena: Arena<u32> = Arena::new();
        let value = 42;
        let key = arena.insert(value);

        let retrieved_value = arena.get(key);
        assert!(retrieved_value.is_some());
        assert_eq!(*retrieved_value.clone().unwrap().read().unwrap(), value);

        let removed_value = arena.remove(key);
        assert!(removed_value.is_some());

        assert_eq!(*retrieved_value.unwrap().read().unwrap(), value);

        let retrieved_value = arena.get(key);
        assert!(retrieved_value.is_none());
    }

    #[test]
    fn test_multithreaded_access() {
        let arena: Arc<Arena<u32>> = Arc::new(Arena::new());
        let num_threads = 4;
        let num_iterations = 100;

        let handles = Vec::new();
        let stop_flag = Arc::new(AtomicBool::new(false));

        for t in 0..num_threads {
            let arena_clone = Arc::clone(&arena);
            let stop_flag_clone = Arc::clone(&stop_flag);

            let handle = thread::spawn(move || {
                for i in 0..num_iterations {
                    let value = t + i;
                    let key = arena_clone.insert(value);
                    let result = arena_clone.get(key);

                    assert!(result.is_some(), "Result was none for key {key:?}",);
                    assert_eq!(*result.unwrap().read().unwrap(), value);

                    if stop_flag_clone.load(Ordering::SeqCst) {
                        break;
                    }
                }
            });

            handles.push(handle);
        }

        // Wait for all threads to finish
        for handle in handles {
            handle.join().unwrap();
        }
    }

    #[test]
    fn test_insert_and_get() {
        let arena: Arena<u32> = Arena::new();
        let value = 42;
        let key = arena.insert(value);

        let result = arena.get(key);
        assert!(result.is_some());
        assert_eq!(*result.unwrap().read().unwrap(), value);
    }

    #[test]
    fn test_insert_and_remove() {
        let arena: Arena<u32> = Arena::new();
        let value = 42;
        let key = arena.insert(value);

        let result = arena.remove(key);
        assert!(result.is_some());
        assert_eq!(*result.unwrap().read().unwrap(), value);
    }

    #[test]
    fn it_works() {
        use super::*;
        let arena = Arena::<u32>::new();
        let key1 = arena.insert(42);
        let key2 = arena.insert(43);
        let key3 = arena.insert(44);

        let v1 = arena.get(key1).unwrap();
        assert_eq!(*v1.read().unwrap(), 42);
        let v2 = arena.get(key2).unwrap();
        assert_eq!(*v2.read().unwrap(), 43);
        let v3 = arena.get(key3).unwrap();
        assert_eq!(*v3.read().unwrap(), 44);

        // Remove the value in key1 slot
        let value = arena.remove(key1).unwrap();
        assert_eq!(*value.read().unwrap(), 42);

        // This should replace the value in key1 slot
        let key4 = arena.insert(45);

        let k1_v = arena.get(key1);
        assert!(k1_v.is_none());

        let k4_v = arena.get(key4);
        assert!(k4_v.is_some());
        assert_eq!(*k4_v.unwrap().read().unwrap(), 45);
    }

    #[test]
    fn test_insert_and_get_multiple() {
        let arena: Arena<u32> = Arena::new();
        let value1 = 42;
        let key1 = arena.insert(value1);

        let value2 = 99;
        let key2 = arena.insert(value2);

        let result1 = arena.get(key1);
        assert_eq!(*result1.unwrap().read().unwrap(), value1);

        let result2 = arena.get(key2);
        assert_eq!(*result2.unwrap().read().unwrap(), value2);
    }
}
