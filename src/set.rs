use core::{
    fmt::{self, Debug},
    ops::RangeBounds,
};
use core::{ops::Bound, prelude::v1::*};

use crate::{bounds::StartBound, map::Key, Range, RangeMap};

pub mod iterators;
// pub mod ops;

// TODO docs

#[derive(Clone)]
/// A set whose items are stored as (half-open) ranges bounded
/// inclusively below and exclusively above `(start..end)`.
///
/// See [`RangeMap`]'s documentation for more details.
///
/// [`RangeMap`]: struct.RangeMap.html
pub struct RangeSet<T> {
    map: RangeMap<T, ()>,
}

impl<T> RangeSet<T> {
    /// Makes a new empty `RangeSet`.
    pub fn new() -> Self
    where
        T: Clone + Ord,
    {
        RangeSet {
            map: RangeMap::new(),
        }
    }

    pub fn clear(&mut self) {
        self.map.clear()
    }

    /// Returns a reference to the range covering the given key, if any.
    pub fn get_range_for(&self, value: &T) -> Option<&Range<T>>
    where
        T: Clone + Ord,
    {
        self.map.get_key_value(value).map(|(range, _)| range)
    }

    /// Returns `true` if any range in the set covers the specified value.
    pub fn contains(&self, value: &T) -> bool
    where
        T: Clone + Ord,
    {
        self.map.contains_point(value)
    }

    pub fn is_disjoint(&self, other: &Self) -> bool {
        // self.intersection(other).next().is_none()
        todo!()
        // TODO
    }

    pub fn is_subset(&self, other: &Self) -> bool {
        todo!()
        // TODO
    }

    pub fn is_superset(&self, other: &Self) -> bool {
        other.is_subset(self)
    }

    /// Insert a range into the set.
    ///
    /// If the inserted range either overlaps or is immediately adjacent
    /// any existing range, then the ranges will be coalesced into
    /// a single contiguous range.
    ///
    /// # Panics
    ///
    /// Panics if range `start >= end`.
    pub fn insert<R>(&mut self, range: R) -> bool
    where
        R: RangeBounds<T>,
        T: Clone + Ord,
    {
        // TODO: return options?
        self.map.insert(range, ()).is_none()
    }

    pub fn set<R>(&mut self, range: R)
    where
        R: RangeBounds<T>,
        T: Clone + Ord,
    {
        self.map.set(range, ())
    }

    // Replace? Doesn't seem meaningful

    /// Removes a range from the set, if all or any of it was present.
    ///
    /// If the range to be removed _partially_ overlaps any ranges
    /// in the set, then those ranges will be contracted to no
    /// longer cover the removed range.
    ///
    /// # Panics
    ///
    /// Panics if range `start >= end`.
    pub fn remove<R>(&mut self, range: R)
    where
        R: RangeBounds<T>,
        T: Clone + Ord,
    {
        // TODO: return whether anything was removed? BTreeSet::remove() -> bool
        self.map.clear_range(range);
    }

    pub fn take<R>(&mut self, range: R) -> Self
    where
        R: RangeBounds<T>,
        T: Clone + Ord,
    {
        if let Some(map) = self.map.remove_range(range) {
            map.into()
        } else {
            Self::new()
        }
    }

    pub fn retain<F>(&mut self, mut f: F)
    where
        T: Ord,
        F: FnMut(&Range<T>) -> bool,
    {
        self.map.retain(|r, _| f(r))
    }

    // TODO: note clone needs
    pub fn append(&mut self, other: &mut Self)
    where
        T: Clone + Ord,
    {
        self.map.append(&mut other.map)
    }

    pub fn split_off(&mut self, at: Bound<T>) -> Self
    where
        T: Clone + Ord,
    {
        Self {
            map: self.map.split_off(at),
        }
    }

    pub fn complement(&self) -> Self
    where
        T: Clone + Ord,
    {
        // TODO
        todo!()
        // // RangeMap::iter_complement MUST return disjoint ranges,
        // // so we know we can just insert them without extra checking
        // self.map
        //     .iter_complement()
        //     .fold(Self::new(), |mut set, range| {
        //         set.map.map.insert(Key(range), ());
        //         set
        //     })
    }

    // TODO: subset(&self, range: R) -> Self; slightly faster than ranges(..).filter().collect() because it doesn't need to check insertions
}

impl<T> Default for RangeSet<T>
where
    T: Clone + Ord,
{
    fn default() -> Self {
        RangeSet::new()
    }
}

impl<K, V> From<RangeMap<K, V>> for RangeSet<K>
where
    K: Ord,
{
    fn from(map: RangeMap<K, V>) -> Self {
        let RangeMap { map, store } = map;
        RangeSet {
            map: RangeMap {
                map: map.into_iter().map(|(k, _)| (k, ())).collect(),
                store,
            },
        }
    }
}

// TODO
// pub struct IntoIter<T> {
//     inner: super::map::IntoIter<T, ()>,
// }
// impl<T> IntoIterator for RangeSet<T> {
//     type Item = Range<T>;
//     type IntoIter = IntoIter<T>;
//     fn into_iter(self) -> Self::IntoIter {
//         IntoIter {
//             inner: self.map.into_iter(),
//         }
//     }
// }
// impl<T> Iterator for IntoIter<T> {
//     type Item = Range<T>;
//     fn next(&mut self) -> Option<Range<T>> {
//         self.inner.next().map(|(range, _)| range)
//     }
//     fn size_hint(&self) -> (usize, Option<usize>) {
//         self.inner.size_hint()
//     }
// }

// We can't just derive this automatically, because that would
// expose irrelevant (and private) implementation details.
// Instead implement it in the same way that the underlying BTreeSet does.
impl<T: Debug> Debug for RangeSet<T>
where
    T: Ord + Clone,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_set().entries(self.iter()).finish()
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    use alloc::{format, vec, vec::Vec};

    trait RangeSetExt<T> {
        fn to_vec(&self) -> Vec<Range<T>>;
    }

    impl<T> RangeSetExt<T> for RangeSet<T>
    where
        T: Ord + Clone,
    {
        fn to_vec(&self) -> Vec<Range<T>> {
            self.iter().cloned().collect()
        }
    }

    #[test]
    fn empty_set_is_empty() {
        let range_set: RangeSet<u32> = RangeSet::new();
        assert_eq!(range_set.to_vec(), Vec::<Range<u32>>::new());
    }

    #[test]
    fn insert_into_empty_map() {
        let mut range_set: RangeSet<u32> = RangeSet::new();
        range_set.insert(0..50);
        assert_eq!(range_set.to_vec(), vec![0..50]);
    }

    #[test]
    fn remove_partially_overlapping() {
        let mut range_set: RangeSet<u32> = RangeSet::new();
        range_set.insert(0..50);
        range_set.remove(25..75);
        assert_eq!(range_set.to_vec(), vec![0..25]);
    }

    // TODO: gaps_in
    // #[test]
    // fn gaps_between_items_floating_inside_outer_range() {
    //     let mut range_set: RangeSet<u32> = RangeSet::new();
    //     // 0 1 2 3 4 5 6 7 8 9
    //     // ◌ ◌ ◌ ◌ ◌ ●-◌ ◌ ◌ ◌
    //     range_set.insert(5..6);
    //     // 0 1 2 3 4 5 6 7 8 9
    //     // ◌ ◌ ◌ ●-◌ ◌ ◌ ◌ ◌ ◌
    //     range_set.insert(3..4);
    //     // 0 1 2 3 4 5 6 7 8 9
    //     // ◌ ◆-------------◇ ◌
    //     let outer_range = 1..8;
    //     let mut gaps = range_set.gaps_in(&outer_range);
    //     // Should yield gaps at start, between items,
    //     // and at end.
    //     assert_eq!(gaps.next(), Some(1..3));
    //     assert_eq!(gaps.next(), Some(4..5));
    //     assert_eq!(gaps.next(), Some(6..8));
    //     assert_eq!(gaps.next(), None);
    //     // Gaps iterator should be fused.
    //     assert_eq!(gaps.next(), None);
    //     assert_eq!(gaps.next(), None);
    // }
    ///
    /// impl Debug
    ///

    #[test]
    fn set_debug_repr_looks_right() {
        let mut set: RangeSet<u32> = RangeSet::new();

        // Empty
        assert_eq!(format!("{:?}", set), "{}");

        // One entry
        set.insert(2..5);
        assert_eq!(format!("{:?}", set), "{2..5}");

        // Many entries
        set.insert(7..8);
        set.insert(10..11);
        assert_eq!(format!("{:?}", set), "{2..5, 7..8, 10..11}");
    }
}
