use ::criterion::{black_box, criterion_group, criterion_main, Criterion};
use crane_arena::Arena;

fn insert_get_bench(c: &mut Criterion) {
    let arena = Arena::<u32>::new();

    c.bench_function("insert and get", |b| {
        b.iter(|| {
            let value = 42;
            let key = arena.insert(value as u32);
            arena.get(key).unwrap();
        })
    });
}

criterion_group!(benches, insert_get_bench);

criterion_main!(benches);
