//! Bounded workers for corpus checks that would otherwise occupy one
//! libtest thread long after the individual tests have finished.

use std::sync::atomic::{AtomicUsize, Ordering};

/// Check every input once and return results in input order. Cap each
/// corpus at eight workers because other tests and the tools they launch
/// also run concurrently. RUST_TEST_THREADS can lower that limit.
pub fn map<T: Sync, R: Send>(inputs: &[T], check: impl Fn(&T) -> R + Sync) -> Vec<R> {
    let cpus = std::thread::available_parallelism().map_or(1, usize::from);
    let limit = std::env::var("RUST_TEST_THREADS")
        .ok()
        .and_then(|s| s.parse::<usize>().ok())
        .unwrap_or(cpus);
    map_with_workers(inputs, check, cpus.min(limit).clamp(1, 8))
}

fn map_with_workers<T: Sync, R: Send>(
    inputs: &[T],
    check: impl Fn(&T) -> R + Sync,
    workers: usize,
) -> Vec<R> {
    if workers <= 1 || inputs.len() <= 1 {
        return inputs.iter().map(check).collect();
    }
    let next = AtomicUsize::new(0);
    let mut results = std::thread::scope(|scope| {
        let handles: Vec<_> = (0..workers.min(inputs.len()))
            .map(|_| {
                scope.spawn(|| {
                    let mut results = Vec::new();
                    loop {
                        let i = next.fetch_add(1, Ordering::Relaxed);
                        let Some(input) = inputs.get(i) else { break };
                        results.push((i, check(input)));
                    }
                    results
                })
            })
            .collect();
        handles
            .into_iter()
            .flat_map(|handle| {
                handle
                    .join()
                    .unwrap_or_else(|panic| std::panic::resume_unwind(panic))
            })
            .collect::<Vec<_>>()
    });
    results.sort_unstable_by_key(|(i, _)| *i);
    results.into_iter().map(|(_, result)| result).collect()
}

#[test]
fn every_input_is_checked_once_and_results_keep_their_order() {
    for len in [0, 1, 3, 37] {
        for workers in [1, 4, 64] {
            let inputs: Vec<_> = (0..len).collect();
            let visits: Vec<_> = (0..len).map(|_| AtomicUsize::new(0)).collect();
            let results = map_with_workers(
                &inputs,
                |&i| {
                    visits[i].fetch_add(1, Ordering::Relaxed);
                    std::thread::yield_now();
                    i * i
                },
                workers,
            );
            assert_eq!(results, inputs.iter().map(|i| i * i).collect::<Vec<_>>());
            assert!(visits.iter().all(|n| n.load(Ordering::Relaxed) == 1));
        }
    }
}

#[test]
#[should_panic(expected = "a corpus failure must fail the test")]
fn a_worker_failure_is_not_lost() {
    map_with_workers(
        &[0, 1, 2],
        |&i| {
            assert_ne!(i, 1, "a corpus failure must fail the test");
        },
        2,
    );
}
