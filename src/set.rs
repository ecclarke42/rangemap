use core::fmt::{self, Debug};

use crate::{
    map::Key,
    Bound::{self, *},
    Range, RangeBounds, RangeMap,
};

pub mod iterators;
// pub mod ops;

/// # RangeSet
///
/// A set based on a [`RangeMap`]. Like [`RangeMap`], adjacent ranges will be
/// merged into a single range.
///
/// See [`RangeMap`]'s documentation for more details on implementation. The
/// internal representation of this `struct` is is a `RangeMap<T, ()>`
///
/// # Examples
///
/// ```
/// use rangemap::{Range, RangeSet};
///
/// let mut ranges = RangeSet::new();
///
/// // Add some ranges
/// ranges.insert(0..5);
/// ranges.insert(5..10); // Note, this will be merged with 0..5!
/// ranges.insert(20..25);
///
/// // Check if a point is covered
/// assert!(ranges.contains(&7));
/// assert!(!ranges.contains(&12));
///
/// // Remove a range (or parts of some ranges)
/// assert!(ranges.contains(&5));
/// assert!(ranges.contains(&24));
/// ranges.remove(3..6);
/// ranges.remove(22..);
/// assert!(!ranges.contains(&5));
/// assert!(!ranges.contains(&24));
///
/// // Check which ranges are covered
/// assert!(ranges.into_iter().eq(vec![
///     Range::from(0..3),
///     Range::from(6..10),
///     Range::from(20..22),
/// ]));
/// ```
///
#[derive(Clone)]
pub struct RangeSet<T> {
    map: RangeMap<T, ()>,
}

impl<T> RangeSet<T> {
    /// Makes a new empty `RangeSet`.
    pub fn new() -> Self
    where
        T: Ord,
    {
        RangeSet {
            map: RangeMap::new(),
        }
    }

    /// Make a new `RangeSet` with a single range present, representing all
    /// possible values.
    ///
    /// ```
    /// use rangemap::RangeSet;
    ///
    /// // Thus, this
    /// let full = RangeSet::<u32>::full();
    ///
    /// // Is equivalent to
    /// let mut manual = RangeSet::new();
    /// manual.insert(..);
    ///
    /// assert_eq!(full, manual);
    /// ```
    pub fn full() -> Self
    where
        T: Ord,
    {
        let mut set = Self::new();
        set.map
            .map
            .insert(Key(Range::new(Unbounded, Unbounded)), ());
        set
    }

    /// Clears the set, removing all elements.
    ///
    /// # Examples
    ///
    /// ```
    /// use rangemap::RangeSet;
    ///
    /// let mut set = RangeSet::new();
    /// set.insert(0..1);
    /// set.clear();
    /// assert!(set.is_empty());
    /// ```
    pub fn clear(&mut self) {
        self.map.clear()
    }

    /// Returns `true` if any range in the set covers the specified value.
    ///
    /// # Examples
    ///
    /// ```
    /// use rangemap::RangeSet;
    ///
    /// let mut set = RangeSet::new();
    /// set.insert(0..5);
    /// assert!(set.contains(&3));
    /// ```
    pub fn contains(&self, value: &T) -> bool
    where
        T: Clone + Ord,
    {
        self.map.contains(value)
    }

    /// Returns a reference to the range covering the given value, if any.
    ///
    /// # Examples
    ///
    /// ```
    /// use rangemap::{Range, RangeSet};
    ///
    /// let mut set = RangeSet::new();
    /// set.insert(0..5);
    ///
    /// assert_eq!(set.get_range_for(&3), Some(&Range::from(0..5)));
    /// assert!(set.get_range_for(&6).is_none());
    /// ```
    pub fn get_range_for(&self, value: &T) -> Option<&Range<T>>
    where
        T: Clone + Ord,
    {
        self.map.get_range_value(value).map(|(range, _)| range)
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
        if let Some(map) = self.map.remove(range) {
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

    // TODO: split_off_range

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

impl<T: PartialEq> PartialEq for RangeSet<T> {
    fn eq(&self, other: &Self) -> bool {
        self.map == other.map
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
        assert_eq!(format!("{:?}", set), "{[2, 5)}");

        // Many entries
        set.insert(7..=8);
        set.insert(10..11);
        assert_eq!(format!("{:?}", set), "{[2, 5), [7, 8], [10, 11)}");
    }
}
