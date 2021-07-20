use alloc::{collections::BTreeMap, vec::Vec};
use core::{
    cmp::{max, Ordering},
    fmt::Debug,
    hash::{Hash, Hasher},
    ops::{Index, RangeBounds},
};

use crate::{
    bounds::{Bound, StartBound},
    Range,
};
pub(crate) use key::Key;

pub mod iterators;
mod key;

#[cfg(test)]
mod tests;

/// # RangeMap
///
/// A map of non-overlapping ranges to values. Inserted ranges will be merged
/// with adjacent ranges if they have the same value.
///
/// Internally, [`RangeMap`] is represented by a [`BTreeMap`] in which the keys
/// are represented by a concrete [`Range`] type, sorted by their start values.
///
/// # Examples
///
/// TODO
///
/// # Entry API
///
/// TODO
///
#[derive(Clone)]
pub struct RangeMap<K, V> {
    pub(crate) map: BTreeMap<Key<K>, V>,

    /// Reuseable storage for working set of keys
    /// (many insertions/deletions will allocate less)
    ///
    /// TODO Performance Improvement:
    ///     This (and successor key collection) could be more streamlined with a
    ///     few strategically placed `unsafe` blocks
    pub(crate) store: Vec<Key<K>>,
}

impl<K, V> RangeMap<K, V> {
    /// Makes a new, empty `RangeMap`.
    ///
    /// Does not allocate anything on its own.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// use rangemap::RangeMap;
    ///
    /// let mut map = RangeMap::new();
    ///
    /// // entries can now be inserted into the empty map
    /// map.insert(0..1, "a");
    /// ```
    pub fn new() -> Self
    where
        K: Ord,
    {
        RangeMap {
            map: BTreeMap::new(),
            store: Vec::new(),
        }
    }

    /// Makes a new, empty [`RangeMap`] with a value for the full range.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// use rangemap::RangeMap;
    ///
    /// let mut map = RangeMap::with_value(true);
    ///
    /// // All values will return something
    /// assert_eq!(map[&0], true);
    /// assert_eq!(map[&10], true);
    /// assert_eq!(map[&12345678], true);
    /// ```
    pub fn with_value(value: V) -> Self
    where
        K: Ord,
    {
        let mut inner = BTreeMap::new();
        inner.insert(Key(Range::full()), value);
        RangeMap {
            map: inner,
            store: Vec::new(),
        }
    }

    /// Clears the map, removing all elements.
    ///
    /// This also resets the capacity of the internal `store`.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// use rangemap::RangeMap;
    ///
    /// let mut a = RangeMap::new();
    /// a.insert(0..1, "a");
    /// a.clear();
    /// assert!(a.is_empty());
    /// ```
    pub fn clear(&mut self) {
        self.map.clear();
        self.store = Vec::new(); // Reset capacity
    }

    /// Resets the capacity of `store`
    pub fn shrink_to_fit(&mut self) {
        self.store = Vec::new(); // Reset capacity
    }

    /// Returns a reference to the value corresponding to the given point,
    /// if the point is covered by any range in the map.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// use rangemap::RangeMap;
    ///
    /// let mut map = RangeMap::new();
    /// map.insert(0..1, "a");
    /// assert_eq!(map.get(&0), Some(&"a"));
    /// assert_eq!(map.get(&2), None);
    /// ```
    pub fn get(&self, at: &K) -> Option<&V>
    where
        K: Clone + Ord,
    {
        self.get_range_value(at).map(|(_range, value)| value)
    }

    /// Returns the range-value pair (as a pair of references) corresponding
    /// to the given point, if the point is covered by any range in the map.
    ///
    /// # Examples
    ///
    /// ```
    /// use rangemap::{Range, RangeMap};
    ///
    /// let mut map = RangeMap::new();
    /// map.insert(0..1, "a");
    /// assert_eq!(map.get_range_value(&0), Some((&Range::from(0..1), &"a")));
    /// assert_eq!(map.get_range_value(&2), None);
    /// ```
    pub fn get_range_value(&self, at: &K) -> Option<(&Range<K>, &V)>
    where
        K: Clone + Ord,
    {
        // The only stored range that could contain the given key is the
        // last stored range whose start is less than or equal to this key.
        self.map
            .range(..=(StartBound(Bound::Included(at.clone()))))
            .rev()
            .map(|(w, v)| (&w.0, v))
            .next()
            .filter(|(range, _)| range.contains(at))
    }

    // TODO: entry api for get_range_value_mut (since we can't control when &mut V is changed to do a merge/coalesce)

    /// Returns `true` if any range in the map covers the specified point.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// use rangemap::RangeMap;
    ///
    /// let mut map = RangeMap::new();
    /// map.insert(0..1, "a");
    /// assert_eq!(map.contains(&0), true);
    /// assert_eq!(map.contains(&2), false);
    /// ```
    pub fn contains(&self, point: &K) -> bool
    where
        K: Clone + Ord,
    {
        self.get_range_value(point).is_some()
    }

    /// Get the widest bounds covered by the ranges in this map
    ///
    /// **NOTE**: This is not necessarily (or likely) a contiguous range!
    /// # Examples
    ///
    /// ```
    /// use rangemap::{RangeMap, Range, Bound};
    ///
    /// let mut map = RangeMap::new();
    /// map.insert(0..9, "a");
    /// map.insert(15..30, "b");
    /// map.insert(35..90, "c");
    ///
    /// assert_eq!(map.bounds(), Some(Range::from(0..90).as_ref()));
    /// ```
    pub fn bounds(&self) -> Option<Range<&K>> {
        let mut iter = self.map.iter();
        iter.next().map(|(first, _)| {
            if let Some((last, _)) = iter.next_back() {
                // 2 or more items, use widest possible bounds
                Range {
                    start: first.0.start.as_ref(),
                    end: last.0.end.as_ref(),
                }
            } else {
                // 1 item, use it's bounds
                first.0.as_ref()
            }
        })
    }

    /// Get the lowest bound covered by the ranges in this map
    ///
    /// # Examples
    ///
    /// ```
    /// use rangemap::{RangeMap, Range, Bound};
    ///
    /// let mut map = RangeMap::new();
    /// map.insert(0..9, "a");
    /// map.insert(15..30, "b");
    /// map.insert(35..90, "c");
    ///
    /// assert_eq!(map.lower_bound(), Some(&Bound::Included(0)));
    /// ```
    pub fn lower_bound(&self) -> Option<&Bound<K>> {
        self.map.iter().next().map(|(range, _)| &range.0.start.0)
    }

    /// Get the highest bound covered by the ranges in this map
    ///
    /// # Examples
    ///
    /// ```
    /// use rangemap::{RangeMap, Range, Bound};
    ///
    /// let mut map = RangeMap::new();
    /// map.insert(0..9, "a");
    /// map.insert(15..30, "b");
    /// map.insert(35..90, "c");
    ///
    /// assert_eq!(map.upper_bound(), Some(&Bound::Excluded(90)));
    /// ```
    pub fn upper_bound(&self) -> Option<&Bound<K>> {
        self.map.iter().next_back().map(|(range, _)| &range.0.end.0)
    }

    /// Retains only the elements specified by the predicate.
    ///
    /// In other words, remove all pairs `(k, v)` such that `f(&k, &mut v)`
    /// returns `false`.
    ///
    /// # Examples
    ///
    /// ```
    /// use rangemap::{RangeMap, Range, Bound};
    ///
    /// let mut map = RangeMap::new();
    /// map.set(0..5, true);
    /// map.set(5..10, false);
    /// map.set(10..15, true);
    /// map.set(15..20, false);
    /// map.set(20..25, true);
    ///
    /// // Keep only the ranges with even numbered starts
    /// map.retain(|k, _| k.start_value().unwrap() % 2 == 0);
    ///
    /// assert!(map[&0] == true);
    /// assert!(map[&10] == true);
    /// assert!(map[&12] == true);
    /// assert!(map[&20] == true);
    /// assert!(map[&24] == true);
    ///
    /// assert!(map.get(&15).is_none());
    /// ```
    ///
    pub fn retain<F>(&mut self, mut f: F)
    where
        K: Ord,
        F: FnMut(&Range<K>, &mut V) -> bool,
    {
        self.map.retain(|k, v| f(&k.0, v))
    }

    /// Insert a value for the specified range
    ///
    /// If the inserted range completely overlaps any existing range in the map,
    /// the existing range (or ranges) will be replaced by the inserted range.
    ///
    /// If the inserted range partially overlaps any existing range in the map,
    /// the existing ranges will be truncated to non-overlapping regions.
    ///
    /// If the inserted range overlaps or is touching an existing range that
    /// maps to the same value, the ranges will be merged into one contiguous
    /// range
    ///
    /// # Returns
    ///
    /// Much like other maps ([`BTreeMap::insert`] or [`HashMap::insert`]),
    /// insert returns the overwritten values (if any existed). Because multiple
    /// ranges might be overwritten, another map will be constructed with those
    /// values.
    ///
    /// **Note**: This will allocate a new underlying [`RangeMap`], though, so an
    /// option is used in case no ranges were overwritten. If you don't care
    /// about the overwritten values, use [`RangeMap::set_range`] instead.
    ///
    /// # Examples
    ///
    /// ## Basic Usage
    ///
    /// ```
    /// use rangemap::RangeMap;
    ///
    /// let mut map = RangeMap::new();
    /// map.insert(0..4, "a");
    /// assert_eq!(map.is_empty(), false);
    ///
    /// map.insert(2..6, "b");
    /// assert_eq!(map[&1], "a");
    /// assert_eq!(map[&3], "b");
    /// ```
    ///
    /// ## Overwriting
    ///
    /// ```
    /// use rangemap::{RangeMap, Range};
    ///
    /// let mut map = RangeMap::new();
    /// map.insert(0..10, "a");
    /// let out = map.insert(3..6, "b").unwrap();
    ///
    /// assert_eq!(map[&2], "a");
    /// assert_eq!(map[&4], "b");
    /// assert_eq!(map[&7], "a");
    /// assert!(out.into_iter().eq(vec![(Range::from(3..6), "a")]));
    ///
    /// ```
    ///
    /// ## Coalescing
    ///
    /// ```
    /// use rangemap::{RangeMap, Range};
    ///
    /// let mut map = RangeMap::new();
    /// map.insert(0..10, "a");
    /// map.insert(10..20, "a");
    ///
    /// assert!(map.into_iter().eq(vec![(Range::from(0..20), "a")]));
    ///
    /// ```
    ///
    /// # See Also
    ///
    /// - [`RangeMap::set`] if you don't want to return the values
    /// overwritten
    /// - [`RangeMap::insert_if_empty`] if you only want to insert the value if
    /// no overlaps occur
    /// - [`RangeMap::insert_in_gaps`] if you only want to insert the value for
    /// the empty parts of the range, not overwriting any values.
    ///
    pub fn insert<R>(&mut self, range: R, value: V) -> Option<Self>
    where
        R: core::ops::RangeBounds<K>,
        K: Clone + Ord,
        V: Clone + Eq,
    {
        // assert!(range.start_bound() <= range.end_bound());
        let range = Range::from(&range);
        let mut removed_ranges = MaybeMap::Uninitialized;
        self.insert_internal(range, value, &mut removed_ranges);
        removed_ranges.into()
    }

    /// Set a value for the specified range, overwriting any existing subset
    /// ranges. This is the same as [`RangeMap::insert`], but without a return
    /// value, so overlapping ranges will be truncated and adjacent ranges with
    /// the same value will be merged.
    ///
    /// # Examples
    ///
    /// ## Basic Usage
    ///
    /// ```
    /// use rangemap::RangeMap;
    ///
    /// let mut map = RangeMap::new();
    /// map.set(0..4, "a");
    /// assert_eq!(map.is_empty(), false);
    ///
    /// map.set(2..6, "b");
    /// assert_eq!(map[&1], "a");
    /// assert_eq!(map[&3], "b");
    /// ```
    ///
    /// ## Overwriting
    ///
    /// ```
    /// use rangemap::RangeMap;
    ///
    /// let mut map = RangeMap::new();
    /// map.set(0..10, "a");
    /// map.set(3..6, "b");
    ///
    /// assert_eq!(map[&2], "a");
    /// assert_eq!(map[&4], "b");
    /// assert_eq!(map[&7], "a");
    ///
    /// ```
    ///
    /// ## Coalescing
    ///
    /// ```
    /// use rangemap::{RangeMap, Range};
    ///
    /// let mut map = RangeMap::new();
    /// map.set(0..10, "a");
    /// map.set(10..20, "a");
    ///
    /// assert!(map.into_iter().eq(vec![(Range::from(0..20), "a")]))
    ///
    /// ```
    ///
    /// # See Also
    ///
    /// - [`RangeMap::insert`] if you want the value overwritten
    /// - [`RangeMap::insert_if_empty`] if you only want to insert the value if
    /// no overlaps occur
    /// - [`RangeMap::insert_in_gaps`] if you only want to insert the value for
    /// the empty parts of the range, not overwriting any values.
    ///
    pub fn set<R>(&mut self, range: R, value: V)
    where
        R: core::ops::RangeBounds<K>,
        K: Clone + Ord,
        V: Clone + Eq,
    {
        let range = Range::from(&range);
        let mut removed_ranges = MaybeMap::Never;
        self.insert_internal(range, value, &mut removed_ranges);
    }

    /// Insert a value into the map, only if there are no existing overlapping
    /// ranges. Returns the given value if it wasn't inserted.
    ///
    /// # Examples
    ///
    /// ```
    /// use rangemap::RangeMap;
    ///
    /// let mut map = RangeMap::new();
    /// assert!(map.insert_if_empty(0..5, true).is_none());
    /// assert!(map.insert_if_empty(5..10, true).is_none());
    /// assert!(map.insert_if_empty(3..6, true).is_some());
    ///
    /// ```
    ///
    /// # See Also
    ///
    /// - [`RangeMap::insert`] or [`RangeMap::set`] if you want to overwrite
    /// existing values
    /// - [`RangeMap::insert_in_gaps`] if you only want to insert the value for
    /// the empty parts of the range
    ///
    pub fn insert_if_empty<R>(&mut self, range: R, value: V) -> Option<V>
    where
        R: core::ops::RangeBounds<K>,
        K: Clone + Ord,
        V: Clone + Eq,
    {
        // Get the ranges before and after this one
        let range = Range::from(&range);

        // Check for any overlaps
        let overlapped = if let Some(upper_bound) = range.end.after() {
            self.map.range(..=upper_bound.cloned())
        } else {
            self.map.range::<Key<K>, _>(..)
        }
        .rev()
        .take_while(|(k, _)| k.0.overlaps(&range))
        .next()
        .is_none();

        // If no overlaps, we can insert directly into the map
        if overlapped {
            self.map.insert(Key(range), value);
            None
        } else {
            Some(value)
        }
    }

    /// Insert a value for empty regions (gaps) in the specified range. If
    /// values exist for ranges overlapping this range, those values will be
    /// preserved. As such, this method may add multiple ranges for the given
    /// value.
    ///
    /// # Examples
    ///
    /// ```
    /// use rangemap::{RangeMap, Range};
    ///
    /// let mut map = RangeMap::new();
    /// map.set(5..10, "a");
    /// map.set(15..20, "a");
    /// map.insert_in_gaps(0..30, "b");
    ///
    /// assert!(map.into_iter().eq(vec![
    ///     (Range::from(0..5), "b"),
    ///     (Range::from(5..10), "a"),
    ///     (Range::from(10..15), "b"),
    ///     (Range::from(15..20), "a"),
    ///     (Range::from(20..30), "b"),
    /// ]));
    ///
    /// ```
    ///
    /// # See Also
    ///
    /// - [`RangeMap::insert`] or [`RangeMap::set`] if you want to overwrite
    /// existing values
    /// - [`RangeMap::insert_if_empty`] if you only want to insert the value if
    /// no overlaps occur
    /// - [`RangeMap::with_value`] if you'd instead like to construct your map
    /// with a default value for all possible ranges
    ///
    pub fn insert_in_gaps<R>(&mut self, range: R, value: V)
    where
        R: core::ops::RangeBounds<K>,
        K: Clone + Ord,
        V: Clone + Eq,
    {
        let mut range = Range::from(&range);

        // In case this is an empty map, exit early
        if self.map.is_empty() {
            self.map.insert(Key(range), value);
            return;
        }

        // Similar to insert, we need to see if any preceeding ranges overlap
        // or touch this one

        let leftmost = self
            .map
            .range(..=range.start.clone())
            .rev()
            .take_while(|(r, _)| r.0.touches(&range))
            .last()
            .map(|(k, v)| (k.clone(), v));

        if let Some((leftmost_touching_range, leftmost_touching_value)) = leftmost {
            // And merge if they have the same value
            if value.eq(leftmost_touching_value) {
                range.start = leftmost_touching_range.0.start.clone(); // Min is implied
                if leftmost_touching_range.0.end > range.end {
                    range.end = leftmost_touching_range.0.end.clone();
                }
                self.map.remove(&leftmost_touching_range);
            } else if leftmost_touching_range.0.end < range.end {
                // If this range extends past the end of the previous range,
                // truncate this range.
                range.start = leftmost_touching_range.0.bound_after().unwrap().cloned();
            } else {
                // Otherwise, we've exhausted the insertion range and don't need
                // to add anything
                return;
            }
        }

        // Get successors of this insertion range. Both are treated the same
        // (unlike in insert)
        self.store.clear();
        self.store.extend(
            if let Some(bound_after) = range.bound_after().map(|b| b.cloned()) {
                self.map.range(range.start.clone()..=bound_after)
            } else {
                self.map.range(range.start.clone()..)
            }
            .map(|(k, _)| k.clone()),
        );

        // Keep marching along the insertion range and insert gaps as we find them
        for successor in self.store.drain(..) {
            let successor_value = self.map.get(&successor).unwrap();
            // If we can merge ranges, do so
            if value.eq(successor_value) {
                let (removed_range, _) = self.map.remove_entry(&successor).unwrap();
                range.end = max(removed_range.0.end, range.end);
            } else {
                // Otherwise, we may need to insert a gap. We can only
                // insert if the range starts before the successor
                // (it shouldn't ever by greater, but could be equal)
                if successor.0.start > range.start {
                    self.map.insert(
                        Key(Range {
                            start: range.start.clone(),
                            end: successor
                                .0
                                .bound_before()
                                .expect("Unexpected unbounded start")
                                .cloned(),
                        }),
                        value.clone(),
                    );
                }

                // After inserting the gap, move the start of the range to
                // the end of this successor, if it exists. If not, this
                // successor extends to the end and we're done.
                if let Some(next_gap_start) = successor.0.bound_after() {
                    range.start = next_gap_start.cloned();
                } else {
                    // Shouldn't actually be necessary, as it would be the last itme
                    break;
                }
            }
        }

        // Any leftover range can then be inserted as a "last gap"
        self.map.insert(Key(range), value);
    }

    /// Remove all values in a given range, returning the removed values.
    /// Overlapping ranges will be truncated at the bounds of this range.
    ///
    /// **Note**: This will allocate another [`RangeMap`] for returning the
    /// removed ranges, so if you don't care about them, use
    /// [`RangeMap::clear_range`] instead
    ///
    /// # Examples
    ///
    /// ```
    /// use rangemap::RangeMap;
    ///
    /// let mut map = RangeMap::new();
    /// map.insert(0..=10, 5);
    /// let removed = map.remove(2..4).unwrap();
    ///
    /// assert_eq!(map[&0], 5);
    /// assert!(map.get(&2).is_none());
    /// assert!(map.get(&3).is_none());
    /// assert_eq!(map[&4], 5);
    /// assert_eq!(map[&10], 5);
    ///
    /// assert_eq!(removed[&2], 5);
    /// assert_eq!(removed[&3], 5);
    /// ```
    ///
    /// # See Also
    ///
    /// - [`RangeMap::clear_range`] if you don't care about the removed values
    ///
    pub fn remove<R: core::ops::RangeBounds<K>>(&mut self, range: R) -> Option<Self>
    where
        K: Clone + Ord,
        V: Clone,
    {
        let mut removed_ranges = MaybeMap::Uninitialized;
        self.remove_internal(Range::from(&range), &mut removed_ranges);
        removed_ranges.into()
    }

    // Unset all values in a given range. Overlapping ranges will be truncated at the bounds of this range

    /// Remove all values in a given range. Overlapping ranges will be truncated
    /// at the bounds of this range.
    ///
    /// # Examples
    ///
    /// ```
    /// use rangemap::RangeMap;
    ///
    /// let mut map = RangeMap::new();
    /// map.insert(0..=10, 5);
    /// map.clear_range(2..4);
    ///
    /// assert_eq!(map[&0], 5);
    /// assert!(map.get(&2).is_none());
    /// assert!(map.get(&3).is_none());
    /// assert_eq!(map[&4], 5);
    /// assert_eq!(map[&10], 5);
    /// ```
    ///
    /// # See Also
    ///
    /// - [`RangeMap::remove`] if you want the removed values
    ///
    pub fn clear_range<R: core::ops::RangeBounds<K>>(&mut self, range: R)
    where
        K: Clone + Ord,
        V: Clone,
    {
        let mut removed_ranges = MaybeMap::Never;
        self.remove_internal(Range::from(&range), &mut removed_ranges);
    }

    /// Moves all elements from `other` into `Self`, leaving `other` empty.
    ///
    /// Note thate `V` must be `Clone` in case any ranges need to be split
    ///
    /// # Examples
    ///
    /// ```
    /// use rangemap::RangeMap;
    ///
    /// let mut a = RangeMap::new();
    /// a.insert(0..1, "a");
    /// a.insert(1..2, "b");
    /// a.insert(2..3, "c");
    ///
    /// let mut b = RangeMap::new();
    /// b.insert(2..3, "d");
    /// b.insert(3..4, "e");
    /// b.insert(4..5, "f");
    ///
    /// a.append(&mut b);
    ///
    /// assert_eq!(a.len(), 5);
    /// assert_eq!(b.len(), 0);
    ///
    /// assert_eq!(a[&0], "a");
    /// assert_eq!(a[&1], "b");
    /// assert_eq!(a[&2], "d");
    /// assert_eq!(a[&3], "e");
    /// assert_eq!(a[&4], "f");
    /// ```
    pub fn append(&mut self, other: &mut Self)
    where
        K: Clone + Ord,
        V: Clone + Eq,
    {
        // self.bounds().is_none() implies an empty map
        match (self.bounds(), other.bounds()) {
            // Other is empty, nothing to append
            (_, None) => {
                // NoOp
            }

            // Self is empty, swap it with other
            (None, _) => core::mem::swap(self, other),

            // Overlapping ranges, we must insert each range in other
            (Some(a), Some(b)) if a.overlaps(&b) => {
                for (range, value) in core::mem::take(&mut other.map) {
                    self.set(range.0, value)
                }
            }

            // If there isn't any overlap, we can safely insert all of the
            // items in other directly into the inner map
            (Some(_), Some(_)) => {
                for (range, value) in core::mem::take(&mut other.map) {
                    self.map.insert(range, value);
                }
            }
        }
    }

    /// Split the map into two at the given bound. Returns everything including
    /// and after that bound.
    ///
    /// # Examples
    ///
    /// # Basic Usage
    ///
    /// ```
    /// use rangemap::{RangeMap, Range, Bound};
    ///
    /// let mut a = RangeMap::new();
    /// a.insert(0..1, "a");
    /// a.insert(1..2, "b");
    /// a.insert(2..3, "c");
    /// a.insert(3..4, "d");
    ///
    /// let b = a.split_off(Bound::Included(2));
    ///
    /// assert!(a.into_iter().eq(vec![
    ///     (Range::from(0..1), "a"),
    ///     (Range::from(1..2), "b"),
    /// ]));
    /// assert!(b.into_iter().eq(vec![
    ///     (Range::from(2..3), "c"),
    ///     (Range::from(3..4), "d"),
    /// ]));
    /// ```
    ///
    /// ## Mixed Bounds
    ///
    /// ```
    /// use rangemap::{RangeMap, Bound};
    ///
    /// let mut a = RangeMap::new();
    /// a.insert(0..1, "a");
    /// a.insert(1..2, "b");
    /// a.insert(2..3, "c");
    /// a.insert(3..4, "d");
    /// a.insert(4..5, "e");
    /// a.insert(5..6, "f");
    /// a.insert(6..7, "g");
    ///
    /// let c = a.split_off(Bound::Excluded(4));
    /// let b = a.split_off(Bound::Included(2));
    ///
    /// assert_eq!(a.len(), 2);
    /// assert_eq!(b.len(), 3);
    /// assert_eq!(c.len(), 3);
    ///
    /// assert_eq!(a[&0], "a");
    /// assert_eq!(a[&1], "b");
    /// assert!(a.get(&2).is_none());
    ///
    /// assert!(b.get(&1).is_none());
    /// assert_eq!(b[&2], "c");
    /// assert_eq!(b[&3], "d");
    /// assert_eq!(b[&4], "e");
    /// assert!(b.get(&5).is_none());
    ///
    /// // `c` also has a a range (4, 5), which is inaccessible with integers.
    /// // if we were using floats, `c[4.5]` would be `"e"`.
    /// assert!(c.get(&4).is_none());
    /// assert_eq!(c[&5], "f");
    /// assert_eq!(c[&6], "g");
    /// assert!(c.get(&7).is_none());
    /// ```
    ///
    pub fn split_off(&mut self, at: Bound<K>) -> Self
    where
        K: Clone + Ord,
        V: Clone,
    {
        let at = StartBound(at);
        if self.is_empty() {
            return Self::new();
        }

        // Split non-overlapping items
        let mut other = self.map.split_off(&at);

        // If there are still items in the lower map, check if the last range
        // crosses the boundary into the upper map
        // No split should be necessary if `at` is unbounded
        if let Some(split_range) = self.map.iter().next_back().map(|(k, _)| k.clone()) {
            // These should all be together and available if there's a split
            if let Some((left_end, at_value)) = at.before().zip(at.value()) {
                if split_range.0.contains(at_value) {
                    // This should always unwrap, because we know the key exists
                    let value = self.map.remove(&split_range).unwrap();

                    // Reinsert truncated range in each
                    self.map.insert(
                        Key(Range {
                            start: split_range.0.start.clone(),
                            end: left_end.cloned(),
                        }),
                        value.clone(),
                    );
                    other.insert(
                        Key(Range {
                            start: at.clone(),
                            end: split_range.0.end,
                        }),
                        value,
                    );
                }
            }
        }

        Self {
            map: other,
            store: Vec::new(),
        }
    }

    // TODO: split_off_range

    /// Internal implementation for [`insert`], [`set`], and similar
    fn insert_internal(
        &mut self,
        mut range: Range<K>,
        value: V,
        removed_ranges: &mut MaybeMap<K, V>,
    ) where
        K: Clone + Ord,
        V: Clone + Eq,
    {
        // In case this is an empty map, exit early
        if self.map.is_empty() {
            self.map.insert(Key(range), value);
            return;
        }

        // Get ranges starting at or before the new range that touch it. The
        // iterator here should yeild:
        // - None if no ranges touch the new range
        // - The first previous range that touches or overlaps the new range
        // - The range two previous if the new range starts right at a previous
        // range (overlapping at the start) and touches one more previous range
        // (like 0..3, 3..5, when inserting 3..4)
        //
        // We want to have the leftmost touching range (rather than just
        // overlapping) in case we can combine the ranges when they have equal
        // values
        let leftmost = self
            .map
            .range(..=range.start.clone())
            .rev()
            .take_while(|(r, _)| r.0.touches(&range))
            .last()
            .map(|(k, v)| (k.clone(), v));

        if let Some((leftmost_touching_range, leftmost_touching_value)) = leftmost {
            if value.eq(leftmost_touching_value) {
                // Remove the touching range and use it's start value (and maybe
                // it's end, in the case of an overlap)
                range.start = leftmost_touching_range.0.start.clone(); // Min is implied
                if leftmost_touching_range.0.end > range.end {
                    range.end = leftmost_touching_range.0.end.clone();
                }
                self.map.remove(&leftmost_touching_range);
            } else if range.overlaps(&leftmost_touching_range.0) {
                // Split an overlapping range to preserve non-overlapped values
                self.split_key(&leftmost_touching_range, &range, removed_ranges);
            }
            // Otherwise, touches (with a different value) but doesn't overlap,
            // leave the existing range alone.
        }

        // After making the adjustment above, are there any following ranges
        // that overlap with the new range?
        //
        // Or, following ranges that touch the end of this range (thus, bound_after)
        //
        // If there is no bound after the inserted range (i.e. the new range is
        // unbounded on the right), all successor ranges can just be removed.
        if let Some(bound_after) = range.bound_after().map(|b| b.cloned()) {
            // Just store keys, so we don't clone values
            self.store.clear();
            self.store.extend(
                self.map
                    .range(range.start.clone()..=bound_after.clone())
                    .map(|(k, _)| k.clone()),
            );

            for successor in self.store.drain(..) {
                if let alloc::collections::btree_map::Entry::Occupied(entry) =
                    self.map.entry(successor)
                {
                    if entry.get().eq(&value) {
                        // If values are the same, merge the ranges (and don't
                        // consider the successor part removed).
                        //
                        // For merging, we don't care if this is a touching or
                        // overlapping range, just that we may need to extend the
                        // end of the inserted range to merge with it.
                        let (successor, _) = entry.remove_entry();
                        if successor.0.end > range.end {
                            range.end = successor.0.end;
                        }
                    } else if entry.key().0.start < bound_after {
                        // Otherwise, if the range is overlapping (not just
                        // touching), it will need to be partially or fully removed
                        let (mut successor, successor_value) = entry.remove_entry();

                        // If overlapping, we need to split and reinsert it
                        if successor.0.end > range.end {
                            self.map.insert(
                                Key(Range {
                                    start: bound_after,
                                    end: core::mem::replace(
                                        &mut successor.0.end,
                                        range.end.clone(),
                                    ),
                                }),
                                successor_value.clone(),
                            );
                            removed_ranges.insert(successor, successor_value);
                            break;
                        } else {
                            // Store the removed portion
                            removed_ranges.insert(successor, successor_value);
                        }
                    }
                    // Otherwise (touching and different value), leave the successor alone
                }
            }
        } else {
            // No upper bound, all following ranges are removed or merged
            let successors = self
                .map
                .range(range.start.clone()..)
                .map(|(k, _)| k.clone())
                .collect::<alloc::vec::Vec<_>>();

            for successor in successors {
                let v = self.map.remove(&successor).unwrap();
                if value != v {
                    removed_ranges.insert(successor, v);
                }
            }
        }

        // Finally, insert the new range and return the removed ranges
        self.map.insert(Key(range), value);
    }

    /// Remove a specified range (`range_to_remove`) from an area of the map
    /// overlapped by the range defined by `key`.
    ///
    /// This is a helper function designed for use in `insert_internal`
    /// and `remove_internal`. This method does no checking for overlaps, and
    /// assumes that the ranges do!
    fn split_key(
        &mut self,
        key: &Key<K>,
        range_to_remove: &Range<K>,
        removed_ranges: &mut MaybeMap<K, V>,
    ) where
        K: Clone + Ord,
        V: Clone,
    {
        // Unwrap here is fine, since the callers of this should have already
        // determined that the key exists
        let (mut removed_range, value) = self.map.remove_entry(key).unwrap();

        // Insert a split of the range to the left (if necessary)
        if removed_range.0.start < range_to_remove.start {
            self.map.insert(
                Key(Range {
                    start: core::mem::replace(
                        &mut removed_range.0.start,
                        range_to_remove.start.clone(),
                    ),
                    end: range_to_remove.bound_before().unwrap().cloned(), // From above inequality, this must not be unbound
                }),
                value.clone(),
            );
        }

        // Insert a split of the range to the right (if necessary)
        if removed_range.0.end > range_to_remove.end {
            self.map.insert(
                Key(Range {
                    start: range_to_remove.bound_after().unwrap().cloned(), // same as above
                    end: core::mem::replace(&mut removed_range.0.end, range_to_remove.end.clone()),
                }),
                value.clone(),
            );
        }
        removed_ranges.insert(removed_range, value);
    }

    pub(crate) fn remove_internal(&mut self, range: Range<K>, removed_ranges: &mut MaybeMap<K, V>)
    where
        K: Clone + Ord,
        V: Clone,
    {
        // Return early if we can
        if self.map.is_empty() {
            return;
        }

        // Get the first range before this one
        let previous_range = self
            .map
            .range(..=range.start.clone())
            .rev()
            .next()
            .map(|(k, _)| k.clone());
        if let Some(previous_range) = previous_range {
            // Split an overlapping range to preserve non-overlapped values
            if range.overlaps(&previous_range.0) {
                self.split_key(&previous_range, &range, removed_ranges);
            }
        }

        // Check if there are any ranges starting inside the range to remove.
        // Unlike insert, we don't care about touching ranges because we
        // won't be merging them.
        self.store.clear();
        self.store.extend(
            if let Some(after) = range.bound_after().map(|b| b.cloned()) {
                self.map.range(range.start.clone()..=after)
            } else {
                self.map.range(range.start.clone()..)
            }
            .map(|(k, _)| k.clone()),
        );

        for mut successor in self.store.drain(..) {
            let value = self.map.remove(&successor).unwrap();

            // Must be the last range
            if successor.0.end > range.end {
                self.map.insert(
                    Key(Range {
                        start: range.bound_after().unwrap().cloned(), // Implicitly not none due to less than successor end
                        end: successor.0.end.clone(),
                    }),
                    value.clone(),
                );
                successor.0.end = range.end;
                removed_ranges.insert(successor, value);
                break;
            } else {
                removed_ranges.insert(successor, value);
            }
        }
    }
}

// We can't just derive this automatically, because that would
// expose irrelevant (and private) implementation details.
// Instead implement it in the same way that the underlying BTreeMap does.
impl<K: Debug, V: Debug> Debug for RangeMap<K, V>
where
    K: Ord + Clone,
    V: Eq + Clone,
{
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_map().entries(self.iter()).finish()
    }
}

impl<K: Hash, V: Hash> Hash for RangeMap<K, V> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        for elt in self {
            elt.hash(state);
        }
    }
}

impl<K: Ord, V> Default for RangeMap<K, V> {
    fn default() -> Self {
        Self::new()
    }
}

impl<K: PartialEq, V: PartialEq> PartialEq for RangeMap<K, V> {
    fn eq(&self, other: &Self) -> bool {
        self.len() == other.len() && self.iter().zip(other).all(|(a, b)| a == b)
    }
}
impl<K: Eq, V: Eq> Eq for RangeMap<K, V> {}
impl<K: PartialOrd + Ord, V: PartialOrd> PartialOrd for RangeMap<K, V> {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.map.iter().partial_cmp(other.map.iter())
    }
}
impl<K: Ord, V: Ord> Ord for RangeMap<K, V> {
    #[inline]
    fn cmp(&self, other: &Self) -> Ordering {
        self.map.iter().cmp(other.map.iter())
    }
}

impl<K, V> Index<&K> for RangeMap<K, V>
where
    K: Clone + Ord,
    V: Eq + Clone,
{
    type Output = V;

    /// Returns a reference to the value corresponding to the supplied key.
    ///
    /// # Panics
    ///
    /// Panics if the key is not present in the `RangeMap`.
    #[inline]
    fn index(&self, key: &K) -> &V {
        self.get(key).expect("no entry found for key")
    }
}

pub(crate) enum MaybeMap<K, V> {
    // Never do anything
    Never,

    // Hold whether the map would contain elements
    None,
    Some,

    // Hold actual elements
    Uninitialized,
    Map(BTreeMap<Key<K>, V>),
}
impl<K: Ord, V> MaybeMap<K, V> {
    fn insert(&mut self, key: Key<K>, value: V) {
        match self {
            MaybeMap::Never | MaybeMap::Some => {} //NoOp
            MaybeMap::None => *self = MaybeMap::Some,
            MaybeMap::Uninitialized => {
                let mut map = BTreeMap::new();
                map.insert(key, value);
                *self = MaybeMap::Map(map);
            }
            MaybeMap::Map(map) => {
                map.insert(key, value);
            }
        }
    }
}

impl<K, V> From<MaybeMap<K, V>> for Option<RangeMap<K, V>> {
    fn from(map: MaybeMap<K, V>) -> Self {
        if let MaybeMap::Map(map) = map {
            Some(RangeMap {
                map,
                store: Vec::new(),
            })
        } else {
            None
        }
    }
}
impl<K, V> From<MaybeMap<K, V>> for bool {
    fn from(map: MaybeMap<K, V>) -> Self {
        matches!(map, MaybeMap::Some | MaybeMap::Map(_))
    }
}
