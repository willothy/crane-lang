use std::{
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc,
    },
    thread,
};

use ::crane_arena::Arena;

pub fn main() {
    let arena: Arc<Arena<u32>> = Arc::new(Arena::new());
    let num_threads = 4;
    let num_iterations = 100;

    let mut last_10 = vec![];
    for i in 0..num_iterations {
        let value = i;
        let key = arena.insert(value);
        let result = arena.get(key);
        if i % 10 == 0 {
            last_10.drain(..).for_each(|k| {
                arena.remove(k);
            });
        } else {
            last_10.push(key);
        }

        assert!(result.is_some(), "Result was none for key {key:?}",);
        assert_eq!(*result.unwrap().read().unwrap(), value);
    }
    // println!("{arena:#?}");
}
