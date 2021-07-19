use alloc::collections::BTreeMap;
use core::ops::RangeBounds;

use super::{bounds::Bound, RangeMap};

// A simple but infeasibly slow and memory-hungry
// version of `RangeMap` for testing.
#[derive(Eq, PartialEq, Debug)]
pub struct StupidU32RangeMap<V> {
    // Inner B-Tree map. Stores values and their keys
    // directly rather than as ranges.
    btm: BTreeMap<u32, V>,
}

impl<V> StupidU32RangeMap<V>
where
    V: Eq + Clone,
{
    pub fn new() -> StupidU32RangeMap<V> {
        StupidU32RangeMap {
            btm: BTreeMap::new(),
        }
    }

    pub fn insert<R: RangeBounds<u32>>(&mut self, range: R, value: V) {
        let start = match range.start_bound() {
            Bound::Unbounded => u32::MIN,
            Bound::Included(&t) => t,
            Bound::Excluded(&t) => t + 1,
        };
        let end = match range.end_bound() {
            Bound::Unbounded => u32::MAX,
            Bound::Included(&t) => t,
            Bound::Excluded(&t) => t - 1,
        };
        for k in start..=end {
            self.btm.insert(k, value.clone());
        }
    }
}

impl<V> From<RangeMap<u32, V>> for StupidU32RangeMap<V>
where
    V: Eq + Clone,
{
    fn from(range_map: RangeMap<u32, V>) -> Self {
        let mut stupid = Self::new();
        for (range, value) in range_map.iter() {
            stupid.insert(range, value.clone());
        }
        stupid
    }
}
