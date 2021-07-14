use core::{cmp::max, ops::RangeBounds};

use alloc::collections::BTreeMap;

use crate::{range::Key, range::StartBound, Bound, Range};

// TODO: docs like BTreeMap

pub struct RangeMap<K, V>(pub(crate) BTreeMap<Key<K>, V>);

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
        RangeMap(BTreeMap::new())
    }

    // TODO: docs
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
    pub fn with_value(value: V) -> Self
    where
        K: Ord,
    {
        let mut inner = BTreeMap::new();
        inner.insert(Key(Range::full()), value);
        RangeMap(inner)
    }

    /// Clears the map, removing all elements.
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
        self.0.clear()
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
    pub fn get(&self, key: &K) -> Option<&V>
    where
        K: Clone + Ord,
    {
        self.get_key_value(key).map(|(_range, value)| value)
    }
    // get_at_point(&self, point: &K) -> Option<&V>

    // TODO: Note no get_mut because it is ambiguous (change value or range?)

    /// Returns the range-value pair (as a pair of references) corresponding
    /// to the given point, if the point is covered by any range in the map.
    ///
    /// # Examples
    ///
    /// ```
    /// use rangemap::RangeMap;
    ///
    /// let mut map = RangeMap::new();
    /// map.insert(0..1, "a");
    /// assert_eq!(map.get_key_value(&0), Some((&(0..1), &"a")));
    /// assert_eq!(map.get_key_value(&2), None);
    /// ```
    pub fn get_key_value(&self, k: &K) -> Option<(&Range<K>, &V)>
    where
        K: Clone + Ord,
    {
        // The only stored range that could contain the given key is the
        // last stored range whose start is less than or equal to this key.
        // TODO: add tests
        self.0
            .range(..=(StartBound(Bound::Included(k.clone()))))
            .rev()
            .map(|(w, v)| (&w.0, v))
            .next()
            .filter(|(range, _)| range.contains(k))
    }

    /// Returns the range-value pair (as a pair of references) corresponding
    /// to the given point, if the point is covered by any range in the map.
    ///
    /// # Examples
    ///
    /// ```
    /// use rangemap::RangeMap;
    ///
    /// let mut map = RangeMap::new();
    /// map.insert(0..1, "a");
    /// assert_eq!(map.get_key_value(&0), Some((&(0..1), &"a")));
    /// assert_eq!(map.get_key_value(&2), None);
    /// ```
    pub fn get_key_value_mut(&mut self, k: &K) -> Option<(&Range<K>, &mut V)>
    where
        K: Clone + Ord,
    {
        // The only stored range that could contain the given key is the
        // last stored range whose start is less than or equal to this key.
        // TODO: add tests
        self.0
            .range_mut(..=(StartBound(Bound::Included(k.clone()))))
            .rev()
            .map(|(w, v)| (&w.0, v))
            .next()
            .filter(|(range, _)| range.contains(k))
    }

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
    /// assert_eq!(map.contains_key(&0), true);
    /// assert_eq!(map.contains_key(&2), false);
    /// ```
    pub fn contains_point(&self, key: &K) -> bool
    where
        K: Clone + Ord,
    {
        self.get_key_value(key).is_some()
    }

    /// Get the widest bounds covered by the ranges in this map
    ///
    /// NOTE: This is not necessarily (or likely) a contiguous range!
    pub fn bounds(&self) -> Option<Range<&K>> {
        let mut iter = self.0.iter();
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
    pub fn lower_bound(&self) -> Option<&Bound<K>> {
        self.0.iter().next().map(|(range, _)| &range.0.start.0)
    }
    /// Get the highest bound covered by the ranges in this map
    pub fn upper_bound(&self) -> Option<&Bound<K>> {
        self.0.iter().next_back().map(|(range, _)| &range.0.end.0)
    }
}

/// Insertion/Deletion methods
impl<K: Clone + Ord, V: Clone + Eq> RangeMap<K, V> {
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
    /// ## Returns
    ///
    /// Much like other maps ([`BTreeMap::insert`] or [`HashMap::insert`]),
    /// insert returns the overwritten values (if any existed). Because multiple
    /// ranges might be overwritten, another map will be constructed with those
    /// values.
    ///
    /// Note: This will allocate a new underlying `BTreeMap`, though, so an
    /// option is used in case no ranges were overwritten. If you don't care
    /// about the overwritten values, use [`RangeMap::set_range`] instead.
    ///
    /// ## Panics
    ///
    /// Panics if range `start > end`.
    ///
    /// ## Examples
    ///
    /// Basic usage:
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
    /// ## See Also
    ///
    /// - [`RangeMap::set_range`] if you don't want to return the values
    /// overwritten
    /// - [`RangeMap::set_gaps`] if you only want to insert values for the empty
    /// parts of the range (don't overwrite any values)
    ///
    /// [`BTreeMap::insert`]: std::collections::BTreeMap::insert
    /// [`HashMap::insert`]: std::collections::HashMap::insert
    pub fn insert<R: core::ops::RangeBounds<K>>(&mut self, range: R, value: V) -> Option<Self>
    where
        K: Clone + Ord,
        V: Clone + Eq,
    {
        // assert!(range.start_bound() <= range.end_bound());
        let range = Range::new(range);
        let mut removed_ranges = MaybeMap::Uninitialized;
        self.insert_internal(range, value, &mut removed_ranges);
        removed_ranges.into()
    }

    // Set a value for the specified range, overwriting any existing subset ranges (overlapping ranges will be truncated at the bounds of this range)
    fn set<R: core::ops::RangeBounds<K>>(&mut self, range: R, value: V)
    where
        K: Clone + Ord,
        V: Clone + Eq,
    {
        let range = Range::new(range);
        let mut removed_ranges = MaybeMap::Never;
        self.insert_internal(range, value, &mut removed_ranges);
    }

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
        if self.0.is_empty() {
            self.0.insert(Key(range), value);
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
        if let Some((leftmost_touching_range, leftmost_touching_value)) = self
            .0
            .range(..=range.start.clone())
            .rev()
            .take_while(|(r, _)| r.0.touches(&range))
            .last()
            .clone()
        {
            if value.eq(leftmost_touching_value) {
                // Remove the touching range and use it's start value (and maybe
                // it's end, in the case of an overlap)
                range.start = leftmost_touching_range.0.start.clone(); // Min is implied
                range.end = max(&leftmost_touching_range.0.end, &range.end).clone();
                self.0.remove(leftmost_touching_range);
            } else if range.overlaps(&leftmost_touching_range.0) {
                // Split an overlapping range to preserve non-overlapped values
                self.split_key(leftmost_touching_range, &range, removed_ranges);
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
        if let Some(bound_after) = range.bound_after() {
            for (successor_range, successor_value) in
                self.0.range(range.start.clone()..=bound_after)
            {
                let (mut removed_range, removed_value) =
                    self.0.remove_entry(successor_range).unwrap();
                if value.eq(successor_value) {
                    // If values are the same, merge the ranges (and don't
                    // consider the successor part removed).
                    //
                    // For merging, we don't care if this is a touching or
                    // overlapping range, just that we may need to extend the
                    // end of the inserted range to merge with it.
                    range.end = max(successor_range.0.end, range.end);
                } else if successor_range.0.start < bound_after {
                    // Otherwise, if the range is overlapping (not just
                    // touching), it will need to be partially or fully removed

                    // If overlapping, we need to split it
                    if successor_range.0.end > range.end {
                        removed_range.0.end = range.end;
                        self.0.insert(
                            Key(Range {
                                start: bound_after,
                                end: successor_range.0.end.clone(),
                            }),
                            removed_value.clone(),
                        );
                    }

                    // Store the removed portion
                    removed_ranges.insert(removed_range, removed_value);
                }
                // Otherwise (touching and different value), leave the successor alone
            }
        } else {
            // No upper bound, all following ranges are removed or merged
            for (r, v) in self.0.range(range.start.clone()..) {
                let (mut removed_range, removed_value) = self.0.remove_entry(r).unwrap();
                if value.ne(v) {
                    removed_ranges.insert(removed_range, removed_value);
                }
            }
        }

        // Finally, insert the new range and return the removed ranges
        self.0.insert(Key(range), value);
    }

    // Insert a value for empty regions (gaps) in the specified range (i.e. `set_range` but without overwrite)
    // if values are already set for ranges overlapping this range, those values will be preserved. As such,
    // this method may add multiple ranges for the given value.
    fn set_gaps<R: core::ops::RangeBounds<K>>(&mut self, range: R, value: V) {
        let range = Range::new(range);

        // In case this is an empty map, exit early
        if self.0.is_empty() {
            self.0.insert(Key(range), value);
        }

        // Similar to insert, we need to see if any preceeding ranges overlap
        // or touch this one
        if let Some((leftmost_touching_range, leftmost_touching_value)) = self
            .0
            .range(..=range.start.clone())
            .rev()
            .take_while(|(r, _)| r.0.touches(&range))
            .last()
        {
            // And merge if they have the same value
            if value.eq(leftmost_touching_value) {
                range.start = leftmost_touching_range.0.start.clone(); // Min is implied
                range.end = max(leftmost_touching_range.0.end, range.end);
                self.0.remove(leftmost_touching_range);
            } else if leftmost_touching_range.0.end < range.end {
                // If this range extends past the end of the previous range,
                // truncate this range.
                range.start = leftmost_touching_range.0.bound_after().unwrap();
            } else {
                // Otherwise, we've exhausted the insertion range and don't need
                // to add anything
                return;
            }
        }

        // Get successors of this insertion range. Both are treated the same
        // (unlike in insert)
        let successors = if let Some(bound_after) = range.bound_after() {
            self.0.range(range.start.clone()..=bound_after)
        } else {
            self.0.range(range.start.clone()..)
        };

        // Keep marching along the insertion range and insert gaps as we find them
        for (successor_range, successor_value) in successors {
            // If we can merge ranges, do so
            if value.eq(successor_value) {
                let (removed_range, _) = self.0.remove_entry(successor_range).unwrap();
                range.end = max(removed_range.0.end, range.end);
            } else {
                // Otherwise, we may need to insert a gap. We can only
                // insert if the range starts before the successor
                // (it shouldn't ever by greater, but could be equal)
                if successor_range.0.start > range.start {
                    self.0.insert(
                        Key(Range {
                            start: range.start.clone(),
                            end: successor_range
                                .0
                                .bound_before()
                                .expect("Unexpected unbounded start"),
                        }),
                        value,
                    );
                }

                // After inserting the gap, move the start of the range to
                // the end of this successor, if it exists. If not, this
                // successor extends to the end and we're done.
                if let Some(next_gap_start) = successor_range.0.bound_after() {
                    range.start = next_gap_start;
                } else {
                    // Shouldn't actually be necessary, as it would be the last itme
                    break;
                }
            }
        }

        // Any leftover range can then be inserted as a "last gap"
        self.0.insert(Key(range), value);
    }

    // Unset all values in a given range. Overlapping ranges will be truncated at the bounds of this range
    fn clear_range<R: core::ops::RangeBounds<K>>(&mut self, range: R) {
        let mut range = Range::new(range);
        let mut removed_ranges = MaybeMap::Never;
        self.remove_internal(range, &mut removed_ranges);
    }

    // TODO: more docs
    /// Remove all values in a given range, returning the removed values. Overlapping ranges will be truncated at the bounds of this range
    ///
    /// ```
    /// use rangemap::RangeMap;
    ///
    /// let mut map = RangeMap::new();
    /// map.insert(0..=10, 5);
    /// map.remove(2..4);
    ///
    /// assert_eq!(map[0], &5);
    /// assert!(map.get(2).is_none());
    /// assert!(map.get(3).is_none());
    /// assert_eq!(map[4], &5);
    /// assert_eq!(map[10], &5);
    /// ```
    fn remove_range<R: core::ops::RangeBounds<K>>(&mut self, range: R) -> Option<Self> {
        let mut range = Range::new(range);
        let mut removed_ranges = MaybeMap::Uninitialized;
        self.remove_internal(range, &mut removed_ranges);
        removed_ranges.into()
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
    ) {
        // Unwrap here is fine, since the callers of this should have already
        // determined that the key exists
        let (mut removed_range, value) = self.0.remove_entry(key).unwrap();

        // Insert a split of the range to the left (if necessary)
        if removed_range.0.start < range_to_remove.start {
            removed_range.0.start = range_to_remove.start.clone();
            self.0.insert(
                Key(Range {
                    start: removed_range.0.start.clone(),
                    end: range_to_remove.bound_before().unwrap(), // From above inequality, this must not be unbound
                }),
                value.clone(),
            );
        }

        // Insert a split of the range to the right (if necessary)
        if removed_range.0.end > range_to_remove.end {
            removed_range.0.end = range_to_remove.end.clone();
            self.0.insert(
                Key(Range {
                    start: range_to_remove.bound_after().unwrap(), // same as above
                    end: removed_range.0.end.clone(),
                }),
                value.clone(),
            );
        }
        removed_ranges.insert(removed_range, value);
    }

    fn remove_internal(&mut self, range: Range<K>, removed_ranges: &mut MaybeMap<K, V>) {
        // Return early if we can
        if self.0.is_empty() {
            return;
        }

        // Get the first range before this one
        if let Some((previous_range, _)) = self.0.range(..=range.start.clone()).rev().next() {
            // Split an overlapping range to preserve non-overlapped values
            if range.overlaps(&previous_range.0) {
                self.split_key(previous_range, &range, removed_ranges);
            }
        }

        // Check if there are any ranges starting inside the range to remove.
        // Unlike insert, we don't care about touching ranges because we
        // won't be merging them.
        let successors = if let Some(after) = range.bound_after() {
            self.0.range(range.start..=after)
        } else {
            self.0.range(range.start..)
        };
        for (successor, _) in successors {
            let (removed, value) = self.0.remove_entry(successor).unwrap();

            // Must be the last range
            if successor.0.end > range.end {
                removed.0.end = range.end;
                self.0.insert(
                    Key(Range {
                        start: range.bound_after().unwrap(), // Implicitly not none due to less than successor end
                        end: successor.0.end.clone(),
                    }),
                    value.clone(),
                );
            }

            removed_ranges.insert(removed, value);
        }
    }

    /// Moves all elements from `other` into `Self`, leaving `other` empty.
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
        K: Ord,
    {
        // Do we have to append anything at all?
        if other.is_empty() {
            return;
        }

        // We can just swap `self` and `other` if `self` is empty.
        if self.is_empty() {
            core::mem::swap(self, other);
            return;
        }

        // Otherwise, insert everything from other into self
        let other_iter = core::mem::take(other).into_iter();
        for (range, value) in other_iter {
            self.set(range, value)
        }
    }

    // TODO
    // /// a.insert(0..1, "a");
    // /// a.insert(1..2, "b");
    // /// a.insert(2..3, "c");
    // /// a.insert(3..4, "d");
    // ///
    // /// let b = a.split_off(&2);
    // ///
    // /// assert_eq!(a.len(), 2);
    // /// assert_eq!(b.len(), 3);
    // ///
    // /// assert_eq!(a[&0], "a");
    // /// assert_eq!(a[&1], "b");
    // /// assert!(a.get(&2).is_none());
    // ///
    // /// assert_eq!(b[&2], "c");
    // /// assert_eq!(b[&3], "d");
    // /// assert!(b.get(&1).is_none());
    // /// ```
    // pub fn split_off(&mut self, key: &K) -> Self {
    //     if self.is_empty() {
    //         return Self::new();
    //     }

    //     // Split non-overlapping items
    //     let other = self.btm.split_off(&RangeStartWrapper::point(key.clone()));

    //     // If there are still items in the lower map (self.btm), check if the
    //     // last range crosses the boundary into the upper map
    //     if let Some((last_range_wrapper, _)) = self.btm.iter().next_back() {
    //         if last_range_wrapper.range.contains(key) {
    //             // This should always unwrap, because we know the key exists
    //             let value = self.btm.remove(last_range_wrapper).unwrap();

    //             // Reinsert truncated range in each
    //             self.0.insert(
    //                 RangeStartWrapper::new(last_range_wrapper.range.start..key.clone()),
    //                 value.clone(),
    //             );
    //             other.0.insert(
    //                 RangeStartWrapper::new(key.clone()..last_range_wrapper.range.end),
    //                 value,
    //             );
    //         }
    //     }

    //     Self(other)
    // }
}
// TODO: set_range_x and variants for each concrete range type (not requiring clone)

// TODO: disengenious because it can be a non-contiguous range?
// impl<K, V> core::ops::RangeBounds<K> for RangeMap<K, V> {
//     fn start_bound(&self) -> core::ops::Bound<&K> {

//     }
//     fn end_bound(&self) -> core::ops::Bound<&K> {

//     }
//     fn contains<U>(&self, item: &U) -> bool
//     where
//             K: PartialOrd<U>,
//             U: ?Sized + PartialOrd<K>, {

//     }
// }

impl<K: Ord, V> Default for RangeMap<K, V> {
    fn default() -> Self {
        Self::new()
    }
}

pub enum MaybeMap<K, V> {
    Never,
    Uninitialized,
    Map(BTreeMap<Key<K>, V>),
}
impl<K: Ord, V> MaybeMap<K, V> {
    fn insert(&mut self, key: Key<K>, value: V) {
        match self {
            MaybeMap::Never => {} //NoOp
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
            Some(RangeMap(map))
        } else {
            None
        }
    }
}
