use core::fmt::{self, Debug};

use crate::{
    map::{Key, MaybeMap},
    Bound::{self, *},
    Range, RangeBounds, RangeMap,
};

pub mod iterators;
// pub mod ops;

#[cfg(test)]
mod tests;

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
    /// # Examples
    ///
    /// ```
    /// use rangemap::{Range, RangeSet};
    ///
    /// let mut set = RangeSet::new();
    /// set.insert(0..5);
    /// assert!(!set.is_empty())
    /// ```
    ///
    /// # See Also
    ///
    /// - [`RangeMap::insert`] and [`RangeMap::set`] for the internal map's
    /// insertion semantics. Because values are always `()` and returning
    /// overwritten values is not necessary, this method uses `set`.
    ///
    pub fn insert<R>(&mut self, range: R)
    where
        R: RangeBounds<T>,
        T: Clone + Ord,
    {
        self.map.set(range, ())
    }

    /// Removes a range from the set returning if all or any of it was present.
    ///
    /// # Examples
    ///
    /// ```
    /// use rangemap::{Range, RangeSet};
    ///
    /// let mut set = RangeSet::new();
    /// set.insert(0..5);
    /// assert!(set.remove(0..2));
    /// ```
    ///
    /// # See Also
    ///
    /// - [`RangeMap::remove`] and [`RangeMap::clear_range`] for the internal map's
    /// removal semantics. However, this method will not allocate anything to
    /// return.
    /// - [`RangeSet::take`] if you want the removed elements
    ///
    pub fn remove<R>(&mut self, range: R) -> bool
    where
        R: RangeBounds<T>,
        T: Clone + Ord,
    {
        let mut removed_ranges = MaybeMap::None;
        self.map
            .remove_internal(Range::from(&range), &mut removed_ranges);
        removed_ranges.into()
    }

    /// Removes a range from the set, returning a set containing the removed
    /// elements
    ///
    /// # Examples
    ///
    /// ```
    /// use rangemap::{Range, RangeSet};
    ///
    /// let mut set = RangeSet::new();
    /// set.insert(0..5);
    /// let removed = set.take(0..2);
    ///
    ///
    /// ```
    ///
    /// # See Also
    ///
    /// - [`RangeMap::remove`] and [`RangeMap::clear_range`] for the internal map's
    /// removal semantics. However, this method will not allocate anything to
    /// return.
    /// - [`RangeSet::remove`] if you don't want the removed elements
    ///
    pub fn take<R>(&mut self, range: R) -> Self
    where
        R: RangeBounds<T>,
        T: Clone + Ord,
    {
        Self {
            map: self.map.remove(range).unwrap_or_default(),
        }
    }

    /// Retains only the elements specified by the predicate.
    ///
    /// In other words, remove all ranges `f(v)` returns `false`.
    ///
    /// # Examples
    ///
    /// ```
    /// use rangemap::{RangeSet, Range};
    ///
    /// let mut set = RangeSet::new();
    /// set.insert(0..4);
    /// set.insert(5..9);
    /// set.insert(10..14);
    /// set.insert(15..19);
    /// set.insert(20..24);
    ///
    /// // Keep only the ranges with even numbered starts
    /// set.retain(|r| r.start_value().unwrap() % 2 == 0);
    ///
    /// assert!(set.contains(&0));
    /// assert!(set.contains(&10));
    /// assert!(set.contains(&12));
    /// assert!(set.contains(&20));
    /// assert!(set.contains(&23));
    ///
    /// assert!(!set.contains(&15));
    /// ```
    ///
    /// # See Also
    ///
    /// - [`RangeMap::retain`], which is called internally
    ///
    pub fn retain<F>(&mut self, mut f: F)
    where
        T: Ord,
        F: FnMut(&Range<T>) -> bool,
    {
        self.map.retain(|r, _| f(r))
    }

    /// Moves all elements from `other` into `Self`, leaving `other` empty.
    ///
    /// # Examples
    ///
    /// ```
    /// use rangemap::{Range, RangeSet};
    ///
    /// let mut a = RangeSet::new();
    /// a.insert(0..1);
    /// a.insert(1..2);
    /// a.insert(2..3);
    ///
    /// let mut b = RangeSet::new();
    /// b.insert(2..3);
    /// b.insert(3..4);
    /// b.insert(4..5);
    ///
    /// a.append(&mut b);
    ///
    /// // Ranges in a should all be coalesced to 0..5
    /// assert!(a.into_iter().eq(vec![
    ///     Range::from(0..5)
    /// ]));
    /// assert!(b.is_empty());
    /// ```
    pub fn append(&mut self, other: &mut Self)
    where
        T: Clone + Ord,
    {
        self.map.append(&mut other.map)
    }

    /// Split the set into two at the given bound. Returns everything including
    /// and after that bound.
    ///
    /// # Examples
    ///
    /// # Basic Usage
    ///
    /// ```
    /// use rangemap::{RangeSet, Range, Bound};
    ///
    /// let mut a = RangeSet::new();
    /// a.insert(0..1);
    /// a.insert(2..3);
    /// a.insert(4..5);
    /// a.insert(6..7);
    ///
    /// let b = a.split_off(Bound::Included(4));
    ///
    /// assert!(a.into_iter().eq(vec![
    ///     Range::from(0..1),
    ///     Range::from(2..3),
    /// ]));
    /// assert!(b.into_iter().eq(vec![
    ///     Range::from(4..5),
    ///     Range::from(6..7),
    /// ]));
    /// ```
    ///
    /// ## Mixed Bounds
    ///
    /// ```
    /// use rangemap::{RangeSet, Range, Bound};
    ///
    /// let mut a = RangeSet::new();
    /// a.insert(0..7);
    ///
    /// let c = a.split_off(Bound::Excluded(4));
    /// let b = a.split_off(Bound::Included(2));
    ///
    /// assert!(a.into_iter().eq(vec![
    ///     Range::from(0..2)
    /// ]));
    /// assert!(b.into_iter().eq(vec![
    ///     Range::from(2..=4)
    /// ]));
    /// assert!(c.into_iter().eq(vec![
    ///     Range::new(Bound::Excluded(4), Bound::Excluded(7))
    /// ]));
    /// ```
    ///
    pub fn split_off(&mut self, at: Bound<T>) -> Self
    where
        T: Clone + Ord,
    {
        Self {
            map: self.map.split_off(at),
        }
    }

    // TODO: split_off_range
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
