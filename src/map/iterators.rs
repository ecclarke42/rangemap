use core::{
    fmt::{self, Debug},
    iter::{FromIterator, FusedIterator},
    ops::Bound::*,
};

use super::Key;
use crate::{
    bounds::{EndBound, StartBound},
    Range, RangeBounds, RangeMap, RangeSet,
};

// TODO: all doctests

impl<K, V> RangeMap<K, V> {
    /// Returns the number of ranges in the map.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// use std::collections::BTreeMap;
    ///
    /// let mut a = BTreeMap::new();
    /// assert_eq!(a.len(), 0);
    /// a.insert(1, "a");
    /// assert_eq!(a.len(), 1);
    /// ```
    pub fn len(&self) -> usize {
        self.map.len()
    }

    /// Returns `true` if the map contains no ranges.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// use std::collections::BTreeMap;
    ///
    /// let mut a = BTreeMap::new();
    /// assert!(a.is_empty());
    /// a.insert(1, "a");
    /// assert!(!a.is_empty());
    /// ```
    pub fn is_empty(&self) -> bool {
        self.map.is_empty()
    }

    /// Gets an iterator over the sorted ranges in the map.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// use rangemap::{Range, RangeMap};
    ///
    /// let mut map = RangeMap::new();
    /// map.insert(0..1, "a");
    /// map.insert(1..2, "b");
    /// map.insert(2..3, "c");
    ///
    /// let (first_range, first_value) = map.iter().next().unwrap();
    /// assert_eq!((*first_range, *first_value), (Range::from(0..1), "a"));
    /// ```
    pub fn iter(&self) -> Iter<'_, K, V> {
        Iter(self.map.iter())
    }

    /// Gets an iterator over a subset of the sorted ranges in the map, bounded
    /// by `range`.
    pub fn iter_in<R>(&self, range: R) -> IterIn<'_, K, V>
    where
        R: RangeBounds<K>,
        K: Clone + Ord,
    {
        IterIn {
            iter: self.iter(),
            range: Range::from(&range),
        }
    }

    /// Gets an iterator over the sorted ranges in the map, with mutable values
    ///
    /// Ranges are used as keys and therefore cannot be mutable. To manipulate
    /// the bounds of stored ranges, they must be removed and re-inserted to
    /// ensure bound integrity.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// use std::collections::BTreeMap;
    ///
    /// let mut map = BTreeMap::new();
    /// map.insert("a", 1);
    /// map.insert("b", 2);
    /// map.insert("c", 3);
    ///
    /// // add 10 to the value if the key isn't "a"
    /// for (key, value) in map.iter_mut() {
    ///     if key != &"a" {
    ///         *value += 10;
    ///     }
    /// }
    /// ```
    pub fn iter_mut(&mut self) -> IterMut<'_, K, V> {
        IterMut(self.map.iter_mut())
    }

    /// Gets an iterator over the range keys of the map (similar to `BTreeMap::keys()`)
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// use std::collections::BTreeMap;
    ///
    /// let mut a = BTreeMap::new();
    /// a.insert(2, "b");
    /// a.insert(1, "a");
    ///
    /// let keys: Vec<_> = a.keys().cloned().collect();
    /// assert_eq!(keys, [1, 2]);
    /// ```
    // pub fn keys(&self) -> Keys<'_, K, V> {
    //     Keys(self.iter())
    // }
    pub fn ranges(&self) -> Ranges<'_, K, V> {
        Ranges(self.iter())
    }

    /// Gets an iterator over the values of the map, in order by their range.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// use std::collections::BTreeMap;
    ///
    /// let mut a = BTreeMap::new();
    /// a.insert(1, "hello");
    /// a.insert(2, "goodbye");
    ///
    /// let values: Vec<&str> = a.values().cloned().collect();
    /// assert_eq!(values, ["hello", "goodbye"]);
    /// ```
    pub fn values(&self) -> Values<'_, K, V> {
        Values(self.iter())
    }

    /// Gets a mutable iterator over the values of the map, in order by their
    /// range.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// use std::collections::BTreeMap;
    ///
    /// let mut a = BTreeMap::new();
    /// a.insert(1, String::from("hello"));
    /// a.insert(2, String::from("goodbye"));
    ///
    /// for value in a.values_mut() {
    ///     value.push_str("!");
    /// }
    ///
    /// let values: Vec<String> = a.values().cloned().collect();
    /// assert_eq!(values, [String::from("hello!"),
    ///                     String::from("goodbye!")]);
    /// ```
    pub fn values_mut(&mut self) -> ValuesMut<'_, K, V> {
        ValuesMut(self.iter_mut())
    }

    // fn range_bounds(&self) -> R?

    // TODO: remove K: Clone?
    pub fn iter_subset<R>(&self, range: R) -> IterSubset<'_, K, V>
    where
        R: RangeBounds<K>,
        K: Clone + Ord,
    {
        let range = Range::new(range.start_bound(), range.end_bound()).cloned();
        IterSubset(Some(match (&range.start, &range.end) {
            (StartBound(Unbounded), EndBound(Unbounded)) => IterSubsetInner::Full(self.iter()),
            (StartBound(Unbounded), bounded_end) => IterSubsetInner::Partial {
                before: None,
                iter: self.map.range(..bounded_end.after().unwrap().cloned()),
                range,
            },
            (bounded_start, EndBound(Unbounded)) => IterSubsetInner::Partial {
                before: Some(self.map.range(..bounded_start.clone())),
                iter: self.map.range(bounded_start.clone()..),
                range,
            },
            (bounded_start, bounded_end) => IterSubsetInner::Partial {
                before: Some(self.map.range(..bounded_start.clone())),
                iter: self
                    .map
                    .range(bounded_start.clone()..bounded_end.after().unwrap().cloned()),
                range,
            },
        }))
    }

    /// Create a `RangeMap` referencing a subset range in `self`
    pub fn subset<R>(&self, range: R) -> RangeMap<K, &V>
    where
        R: RangeBounds<K>,
        K: Clone + Ord,
    {
        RangeMap {
            map: self.iter_subset(range).map(|(r, v)| (Key(r), v)).collect(),
            store: alloc::vec::Vec::with_capacity(self.store.len()),
        }
    }

    pub fn iter_complement(&self) -> IterComplement<'_, K, V> {
        IterComplement(Some(ComplementInner::Before {
            first: self.ranges().next(),
            iter: self.iter(),
        }))
    }

    pub fn complement(&self) -> RangeSet<&K>
    where
        K: Ord,
    {
        RangeSet {
            map: RangeMap {
                map: self.iter_complement().map(|r| (Key(r), ())).collect(),
                store: alloc::vec::Vec::with_capacity(self.store.len()),
            },
        }
    }

    /// Gets an iterator over all maximally-sized gaps between ranges in the map
    ///
    /// NOTE: Empty regions before and after those stored in this map (i.e.
    /// before the first range and after the last range) will not be included
    /// in this iterator
    pub fn iter_gaps(&self) -> Gaps<'_, K, V> {
        Gaps {
            iter: self.iter(),
            prev: None,
        }
    }

    pub fn gaps(&self) -> RangeSet<&K>
    where
        K: Ord,
    {
        RangeSet {
            map: RangeMap {
                map: self.iter_gaps().map(|r| (Key(r), ())).collect(),
                store: alloc::vec::Vec::with_capacity(self.store.len()),
            },
        }
    }

    // /// Gets an iterator over all maximally-sized gaps between ranges in the map,
    // /// further bounded by an outer range
    // ///
    // /// NOTE: Unlike [`gaps`], the iterator here WILL include regions before and
    // /// after those stored in the map, so long as they are included in the outer
    // /// range
    // pub fn gaps_in<'a, R: 'a + core::ops::RangeBounds<K>>(
    //     &'a self,
    //     range: R,
    // ) -> GapsIn<'a, K, V, R> {
    //     // TODO: why can't we borrow start/end and make `bounds` a Range<&'a T>?
    //     GapsIn {
    //         iter: self.iter(),
    //         prev: None,
    //         bounds: range,
    //     }
    // }
}

impl<K, V> IntoIterator for RangeMap<K, V> {
    type Item = (Range<K>, V);
    type IntoIter = IntoIter<K, V>;
    fn into_iter(self) -> Self::IntoIter {
        IntoIter(self.map.into_iter())
    }
}
impl<'a, K, V> IntoIterator for &'a RangeMap<K, V> {
    type Item = (&'a Range<K>, &'a V);
    type IntoIter = Iter<'a, K, V>;
    fn into_iter(self) -> Iter<'a, K, V> {
        self.iter()
    }
}
impl<'a, K, V> IntoIterator for &'a mut RangeMap<K, V> {
    type Item = (&'a Range<K>, &'a mut V);
    type IntoIter = IterMut<'a, K, V>;
    fn into_iter(self) -> IterMut<'a, K, V> {
        self.iter_mut()
    }
}

impl<R: core::ops::RangeBounds<K>, K: Clone + Ord, V: Clone + Eq> FromIterator<(R, V)>
    for RangeMap<K, V>
{
    fn from_iter<T: IntoIterator<Item = (R, V)>>(iter: T) -> Self {
        let mut map = Self::new();
        map.extend(iter);
        map
    }
}

impl<R, K, V> Extend<(R, V)> for RangeMap<K, V>
where
    R: core::ops::RangeBounds<K>,
    K: Clone + Ord,
    V: Clone + Eq,
{
    #[inline]
    fn extend<T: IntoIterator<Item = (R, V)>>(&mut self, iter: T) {
        iter.into_iter().for_each(move |(k, v)| {
            self.set(k, v);
        });
    }
}

/// An iterator over the entries of a `RangeMap`.
///
/// This `struct` is created by the [`iter`] method on [`RangeMap`]. See its
/// documentation for more.
///
/// [`iter`]: RangeMap::iter
pub struct Iter<'a, K, V>(alloc::collections::btree_map::Iter<'a, Key<K>, V>);

impl<K: Debug, V: Debug> Debug for Iter<'_, K, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_list().entries(self.clone()).finish()
    }
}

impl<'a, K: 'a, V: 'a> Iterator for Iter<'a, K, V> {
    type Item = (&'a Range<K>, &'a V);
    fn next(&mut self) -> Option<(&'a Range<K>, &'a V)> {
        self.0.next().map(|(wrapper, v)| (&wrapper.0, v))
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
    fn last(mut self) -> Option<(&'a Range<K>, &'a V)> {
        self.next_back()
    }
    fn min(mut self) -> Option<(&'a Range<K>, &'a V)> {
        self.next()
    }
    fn max(mut self) -> Option<(&'a Range<K>, &'a V)> {
        self.next_back()
    }
}
impl<K, V> FusedIterator for Iter<'_, K, V> {}

impl<'a, K: 'a, V: 'a> DoubleEndedIterator for Iter<'a, K, V> {
    fn next_back(&mut self) -> Option<(&'a Range<K>, &'a V)> {
        self.0.next_back().map(|(wrapper, v)| (&wrapper.0, v))
    }
}
impl<K, V> ExactSizeIterator for Iter<'_, K, V> {
    fn len(&self) -> usize {
        self.0.len()
    }
}
impl<K, V> Clone for Iter<'_, K, V> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

/// An iterator over the entries of a `RangeMap`.
///
/// This `struct` is created by the [`iter`] method on [`RangeMap`]. See its
/// documentation for more.
///
/// [`iter`]: RangeMap::iter
pub struct IterIn<'a, K, V> {
    iter: Iter<'a, K, V>,
    range: Range<K>,
}

impl<K: Clone + Ord + Debug, V: Debug> Debug for IterIn<'_, K, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_list().entries(self.clone()).finish()
    }
}

impl<'a, K: 'a + Ord, V: 'a> Iterator for IterIn<'a, K, V> {
    type Item = (&'a Range<K>, &'a V);
    fn next(&mut self) -> Option<(&'a Range<K>, &'a V)> {
        loop {
            let next = self.iter.next()?;
            if next.0.overlaps(&self.range) {
                return Some(next);
            }
        }
    }
    fn last(mut self) -> Option<(&'a Range<K>, &'a V)> {
        self.next_back()
    }
    fn min(mut self) -> Option<(&'a Range<K>, &'a V)> {
        self.next()
    }
    fn max(mut self) -> Option<(&'a Range<K>, &'a V)> {
        self.next_back()
    }
}
impl<K: Ord, V> FusedIterator for IterIn<'_, K, V> {}

impl<'a, K: 'a + Ord, V: 'a> DoubleEndedIterator for IterIn<'a, K, V> {
    fn next_back(&mut self) -> Option<(&'a Range<K>, &'a V)> {
        loop {
            let next = self.iter.next_back()?;
            if next.0.overlaps(&self.range) {
                return Some(next);
            }
        }
    }
}
impl<K: Ord, V> ExactSizeIterator for IterIn<'_, K, V> {
    fn len(&self) -> usize {
        self.iter.len()
    }
}
impl<K: Clone, V> Clone for IterIn<'_, K, V> {
    fn clone(&self) -> Self {
        Self {
            iter: self.iter.clone(),
            range: self.range.clone(),
        }
    }
}

/// A mutable iterator over the entries of a `RangeMap`.
///
/// This `struct` is created by the [`iter_mut`] method on [`RangeMap`]. See its
/// documentation for more.
///
/// [`iter_mut`]: RangeMap::iter_mut
pub struct IterMut<'a, K: 'a, V: 'a>(alloc::collections::btree_map::IterMut<'a, Key<K>, V>);

impl<K: Debug, V: Debug> Debug for IterMut<'_, K, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl<'a, K: 'a, V: 'a> Iterator for IterMut<'a, K, V> {
    type Item = (&'a Range<K>, &'a mut V);

    fn next(&mut self) -> Option<(&'a Range<K>, &'a mut V)> {
        self.0.next().map(|(wrapper, v)| (&wrapper.0, v))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }

    fn last(mut self) -> Option<(&'a Range<K>, &'a mut V)> {
        self.next_back()
    }

    fn min(mut self) -> Option<(&'a Range<K>, &'a mut V)> {
        self.next()
    }

    fn max(mut self) -> Option<(&'a Range<K>, &'a mut V)> {
        self.next_back()
    }
}

impl<'a, K: 'a, V: 'a> DoubleEndedIterator for IterMut<'a, K, V> {
    fn next_back(&mut self) -> Option<(&'a Range<K>, &'a mut V)> {
        self.0.next_back().map(|(wrapper, v)| (&wrapper.0, v))
    }
}

impl<K, V> ExactSizeIterator for IterMut<'_, K, V> {
    fn len(&self) -> usize {
        self.0.len()
    }
}

impl<K, V> FusedIterator for IterMut<'_, K, V> {}

// impl<'a, K, V> IterMut<'a, K, V> {
//     /// Returns an iterator of references over the remaining items.
//     #[inline]
//     pub(super) fn iter(&self) -> Iter<'_, K, V> {
//         Iter(self.0.iter())
//     }
// }

/// An owning iterator over the entries of a `RangeMap`.
///
/// This `struct` is created by the [`into_iter`] method on [`RangeMap`]
/// (provided by the `IntoIterator` trait). See its documentation for more.
///
/// [`into_iter`]: IntoIterator::into_iter
pub struct IntoIter<K, V>(alloc::collections::btree_map::IntoIter<Key<K>, V>);
impl<K: Debug, V: Debug> Debug for IntoIter<K, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}
impl<K, V> Iterator for IntoIter<K, V> {
    type Item = (Range<K>, V);
    fn next(&mut self) -> Option<(Range<K>, V)> {
        self.0.next().map(|(wrapper, v)| (wrapper.0, v))
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}
impl<K, V> DoubleEndedIterator for IntoIter<K, V> {
    fn next_back(&mut self) -> Option<(Range<K>, V)> {
        self.0.next_back().map(|(wrapper, v)| (wrapper.0, v))
    }
}
impl<K, V> ExactSizeIterator for IntoIter<K, V> {
    fn len(&self) -> usize {
        self.0.len()
    }
}
impl<K, V> FusedIterator for IntoIter<K, V> {}
// impl<K, V> IntoIter<K, V> {
//     #[inline]
//     pub(super) fn iter(&self) -> Iter<'_, K, V> {
//         Iter(self.)
//     }
// }

/// An iterator over the keys of a `RangeMap`.
///
/// This `struct` is created by the [`keys`] method on [`RangeMap`]. See its
/// documentation for more.
///
/// [`keys`]: RangeMap::keys
pub struct Ranges<'a, K: 'a, V: 'a>(Iter<'a, K, V>);
impl<K: Debug, V> Debug for Ranges<'_, K, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_list().entries(self.clone()).finish()
    }
}
impl<'a, K, V> Iterator for Ranges<'a, K, V> {
    type Item = &'a Range<K>;
    fn next(&mut self) -> Option<&'a Range<K>> {
        self.0.next().map(|(k, _)| k)
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
    fn last(mut self) -> Option<&'a Range<K>> {
        self.next_back()
    }
    fn min(mut self) -> Option<&'a Range<K>> {
        self.next()
    }
    fn max(mut self) -> Option<&'a Range<K>> {
        self.next_back()
    }
}
impl<'a, K, V> DoubleEndedIterator for Ranges<'a, K, V> {
    fn next_back(&mut self) -> Option<&'a Range<K>> {
        self.0.next_back().map(|(k, _)| k)
    }
}
impl<K, V> ExactSizeIterator for Ranges<'_, K, V> {
    fn len(&self) -> usize {
        self.0.len()
    }
}
impl<K, V> FusedIterator for Ranges<'_, K, V> {}

impl<K, V> Clone for Ranges<'_, K, V> {
    fn clone(&self) -> Self {
        Ranges(self.0.clone())
    }
}

/// An iterator over the values of a `RangeMap`.
///
/// This `struct` is created by the [`values`] method on [`RangeMap`]. See its
/// documentation for more.
///
/// [`values`]: RangeMap::values
#[derive(Clone)]
pub struct Values<'a, K: 'a, V: 'a>(Iter<'a, K, V>);

// TODO: Debug Impl
// impl<K, V: Debug> Debug for Values<'_, K, V> {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         f.debug_list().entries(self.clone()).finish()
//     }
// }

impl<'a, K, V> Iterator for Values<'a, K, V> {
    type Item = &'a V;
    fn next(&mut self) -> Option<&'a V> {
        self.0.next().map(|(_, v)| v)
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
    fn last(mut self) -> Option<&'a V> {
        self.next_back()
    }
}
impl<'a, K, V> DoubleEndedIterator for Values<'a, K, V> {
    fn next_back(&mut self) -> Option<&'a V> {
        self.0.next_back().map(|(_, v)| v)
    }
}
impl<K, V> ExactSizeIterator for Values<'_, K, V> {
    fn len(&self) -> usize {
        self.0.len()
    }
}
impl<K, V> FusedIterator for Values<'_, K, V> {}

/// A mutable iterator over the values of a `RangeMap`.
///
/// This `struct` is created by the [`values_mut`] method on [`RangeMap`]. See its
/// documentation for more.
///
/// [`values_mut`]: RangeMap::values_mut
pub struct ValuesMut<'a, K: 'a, V: 'a>(IterMut<'a, K, V>);

// TODO: Debug Impl
// impl<K, V: Debug> Debug for ValuesMut<'_, K, V> {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         f.debug_list()
//             .entries(self.iter().map(|(_, val)| val))
//             .finish()
//     }
// }

impl<'a, K, V> Iterator for ValuesMut<'a, K, V> {
    type Item = &'a mut V;
    fn next(&mut self) -> Option<&'a mut V> {
        self.0.next().map(|(_, v)| v)
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
    fn last(mut self) -> Option<&'a mut V> {
        self.next_back()
    }
}
impl<'a, K, V> DoubleEndedIterator for ValuesMut<'a, K, V> {
    fn next_back(&mut self) -> Option<&'a mut V> {
        self.0.next_back().map(|(_, v)| v)
    }
}
impl<K, V> ExactSizeIterator for ValuesMut<'_, K, V> {
    fn len(&self) -> usize {
        self.0.len()
    }
}
impl<K, V> FusedIterator for ValuesMut<'_, K, V> {}

pub struct IterSubset<'a, K, V>(Option<IterSubsetInner<'a, K, V>>);

enum IterSubsetInner<'a, K, V> {
    Full(Iter<'a, K, V>),

    Partial {
        before: Option<alloc::collections::btree_map::Range<'a, Key<K>, V>>,
        iter: alloc::collections::btree_map::Range<'a, Key<K>, V>,
        range: Range<K>,
    },
}

impl<'a, K: Clone + Ord, V> Iterator for IterSubset<'a, K, V> {
    type Item = (Range<K>, &'a V);
    fn next(&mut self) -> Option<Self::Item> {
        match self.0.take()? {
            IterSubsetInner::Full(mut iter) => {
                let next = iter.next().map(|(r, v)| (r.clone(), v));
                if next.is_some() {
                    self.0.insert(IterSubsetInner::Full(iter));
                }
                next
            }
            IterSubsetInner::Partial {
                mut before,
                mut iter,
                range,
            } => {
                // Check the first previous range from start to see if it
                // overlaps the given outer range, consuming `before` as there
                // will only be 1 options there
                if let Some((Key(r), v)) = before.take().map(|mut x| x.next_back()).flatten() {
                    let mut r = r.clone();

                    // Check if this overlaps the outer range
                    // (range iterator means this must start before range start)
                    if r.end.cmp_start(&range.start).is_gt() {
                        if r.start < range.start {
                            r.start = range.start.clone();
                        };

                        self.0.insert(IterSubsetInner::Partial {
                            before: None,
                            iter,
                            range,
                        });
                        return Some((r, v));
                    }
                }

                // Otherwise, continue marching through `iter` until we reach
                // `range.end`
                let (Key(r), v) = iter.next()?;
                let mut r = r.clone();

                if r.start.as_ref() > range.end.after().unwrap() {
                    // Finished!
                    None
                } else {
                    if r.end > range.end {
                        // If this extends past the end, it must be our last item
                        r.end = range.end;
                    } else {
                        // Otherwise, save everything for next iteration
                        self.0.insert(IterSubsetInner::Partial {
                            before: None,
                            iter,
                            range,
                        });
                    }

                    Some((r, v))
                }
            }
        }
    }
}

pub struct Gaps<'a, K, V> {
    iter: Iter<'a, K, V>,
    prev: Option<Range<&'a K>>,
}

impl<'a, K, V> Iterator for Gaps<'a, K, V>
where
    K: Ord,
{
    type Item = Range<&'a K>;
    fn next(&mut self) -> Option<Self::Item> {
        let next = self.iter.next()?.0.as_ref();

        if let Some(prev) = self.prev.take() {
            // Get the adjacent bound to the end of the previous range

            let start = prev.bound_after()?.cloned(); // If none, no more gaps (this extends forwards to infinity)
            let end = next
                .bound_before()
                .expect("Unbounded internal range in RangeMap")
                .cloned();
            self.prev.insert(next);
            Some(Range { start, end })
        } else {
            // No previous bound means first gap

            // Get the adjacent bound to the end of the first range
            // If none, no more gaps (this extends forwards to infinity)
            let start = next.borrow_bound_after()?;

            // Check if we have another range, otherwise only one item (no gaps)
            let next = self.iter.next()?.0.as_ref();

            // Store the end of the next segment for next iteration
            // This bound should always exist, because this is not the first
            // range
            let end = next.borrow_bound_before().unwrap();

            // Hold on to next
            self.prev = Some(next);
            Some(Range { start, end })
        }
    }
}

impl<K: Ord, V> FusedIterator for Gaps<'_, K, V> {}

pub struct IterComplement<'a, K, V>(Option<ComplementInner<'a, K, V>>);
enum ComplementInner<'a, K, V> {
    Before {
        first: Option<&'a Range<K>>,
        iter: Iter<'a, K, V>,
    },
    Gaps(Gaps<'a, K, V>), // TODO: make gaps generic over iterator? Then Before can use Peekable
}

impl<'a, K, V> Iterator for IterComplement<'a, K, V>
where
    K: Ord,
{
    type Item = Range<&'a K>;
    fn next(&mut self) -> Option<Self::Item> {
        match self.0.take()? {
            ComplementInner::Before { first, iter } => {
                if let Some(first) = first {
                    let mut gaps = Gaps { iter, prev: None };
                    let out = first
                        .bound_before()
                        .map(|end| Range {
                            start: StartBound(Unbounded),
                            end,
                        })
                        .or_else(|| gaps.next());

                    self.0.insert(ComplementInner::Gaps(gaps));
                    out
                } else {
                    None
                }
            }

            // Use Gaps iterator to iterate all inner gaps
            ComplementInner::Gaps(mut gaps) => {
                if let Some(next) = gaps.next() {
                    // In the gaps iterator
                    self.0.insert(ComplementInner::Gaps(gaps));
                    Some(next)
                } else {
                    // After the last item in gaps, try to use the `prev`
                    // element to get the end bound, otherwise no more gaps!
                    gaps.prev
                        .map(|p| {
                            p.borrow_bound_after().map(|start| Range {
                                start,
                                end: EndBound(Unbounded),
                            })
                        })
                        .flatten()
                }
            }
        }
    }
}

// pub struct GapsIn<'a, K, V, R> {
//     iter: Iter<'a, K, V>,
//     prev: Option<&'a Range<K>>,
//     bounds: R,
// }

// impl<'a, K, V, R> Iterator for GapsIn<'a, K, V, R>
// where
//     K: Ord,
//     R: core::ops::RangeBounds<K>,
// {
//     type Item = Range<&'a K>;
//     fn next(&mut self) -> Option<Self::Item> {
//         todo!();

//         // if let Some((next, _)) = self.iter.next() {
//         //     if let Some(prev) = self.prev {
//         //         // Get the adjacent bound to the end of the previous range

//         //         let start = prev.bound_after()?.cloned(); // If none, no more gaps (this extends forwards to infinity)
//         //         let end = next
//         //             .bound_before()
//         //             .expect("Unbounded internal range in RangeMap")
//         //             .cloned();
//         //         self.prev = Some(next);
//         //         Some(Range { start, end })
//         //     } else {
//         //         // No previous bound means first gap

//         //         // Get the adjacent bound to the end of the first range
//         //         let start = next.bound_after()?.cloned(); // If none, no more gaps (this extends forwards to infinity)

//         //         // Check if we have another range
//         //         if let Some((next, _)) = self.iter.next() {
//         //             // Store the end of the next segment for next iteration
//         //             let end = next
//         //                 .bound_before()
//         //                 .expect("Unbounded internal range in RangeMap")
//         //                 .cloned();

//         //             self.prev = Some(next);
//         //             Some(Range { start, end })
//         //         } else {
//         //             // Only one item (no gaps)
//         //             None
//         //         }
//         //     }
//         // } else {
//         //     None
//         // }
//     }
// }

// impl<K: Clone + Ord, V, R: core::ops::RangeBounds<K>> FusedIterator for GapsIn<'_, K, V, R> {}
