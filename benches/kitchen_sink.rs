#[macro_use]
extern crate criterion;

use criterion::{AxisScale, BenchmarkId, Criterion, PlotConfiguration};
use rand::prelude::*;
use std::ops::Range;

fn kitchen_sink(kvs: &[(Range<i32>, bool)]) {
    use rangemap::RangeMap;

    let mut range_map: RangeMap<i32, bool> = RangeMap::new();
    // Remove every second range.
    let mut remove = false;
    for (range, value) in kvs {
        if remove {
            range_map.clear_range(range.clone());
        } else {
            range_map.set(range.clone(), *value);
        }
        remove = !remove;
    }
}

fn old_kitchen_sink(kvs: &[(Range<i32>, bool)]) {
    use base::RangeMap;

    let mut range_map: RangeMap<i32, bool> = RangeMap::new();
    // Remove every second range.
    let mut remove = false;
    for (range, value) in kvs {
        if remove {
            range_map.remove(range.clone());
        } else {
            range_map.insert(range.clone(), *value);
        }
        remove = !remove;
    }
}

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("kitchen sink", |b| {
        let mut rng = thread_rng();
        let kvs: Vec<(Range<i32>, bool)> = (0..1000)
            .map(|_| {
                let start = rng.gen_range(0..1000);
                let end = start + rng.gen_range(1..100);
                let value: bool = random();
                (start..end, value)
            })
            .collect();
        b.iter(|| kitchen_sink(&kvs))
    });

    let mut group = c.benchmark_group("comparison");
    let mut rng = thread_rng();
    group.plot_config(PlotConfiguration::default().summary_scale(AxisScale::Logarithmic));
    group.sample_size(100);
    group.measurement_time(std::time::Duration::from_secs(60));

    for elems in (1..=6u32).map(|exp| 10i32.pow(exp)) {
        let kvs: Vec<(Range<i32>, bool)> = (0..elems)
            .map(|_| {
                let start = rng.gen_range(0..1000);
                let end = start + rng.gen_range(1..100);
                let value: bool = random();
                (start..end, value)
            })
            .collect();

        group.bench_with_input(BenchmarkId::new("Published", elems), &kvs, |b, kvs| {
            b.iter(|| old_kitchen_sink(kvs))
        });
        group.bench_with_input(BenchmarkId::new("Forked", elems), &kvs, |b, kvs| {
            b.iter(|| kitchen_sink(kvs))
        });
    }
    group.finish()
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
