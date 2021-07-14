use self::entry::{Entry, OccupiedEntry, VacantEntry};

use super::range_wrapper::RangeStartWrapper;
use crate::std_ext::*;
use alloc::collections::BTreeMap;
use core::cmp::Ordering;
use core::fmt::{self, Debug};
use core::hash::{Hash, Hasher};
use core::ops::{Index, Range};
use core::prelude::v1::*;
use core::{borrow::Borrow, ops::Bound};

pub mod entry;
pub mod iterators;
#[cfg(test)]
mod tests;

/// A map whose keys are stored as (half-open) ranges bounded
/// inclusively below and exclusively above `(start..end)`.
///
/// Contiguous and overlapping ranges that map to the same value
/// are coalesced into a single range.
#[derive(Clone)]
pub struct RangeMap<K, V> {
    // Wrap ranges so that they are `Ord`.
    // See `range_wrapper.rs` for explanation.
    btm: BTreeMap<RangeStartWrapper<K>, V>,
}

impl<K, V> RangeMap<K, V>
where
    K: Ord + Clone,
    V: Eq + Clone,
{


    


    // TODO: Implement: Should this mutate the entire containing range (the
    // implementation as a map might see it), or should it add a new point
    // range?). Because of this ambiguity, maybe this function *shouldn't* be
    // implemented, in favor of get_range() or insert() or update_range(), etc?

    // /// Returns a mutable reference to the value corresponding to the key.
    // ///
    // /// The key may be any borrowed form of the map's key type, but the ordering
    // /// on the borrowed form *must* match the ordering on the key type.
    // ///
    // /// # Examples
    // ///
    // /// Basic usage:
    // ///
    // /// ```
    // /// use std::collections::BTreeMap;
    // ///
    // /// let mut map = BTreeMap::new();
    // /// map.insert(1, "a");
    // /// if let Some(x) = map.get_mut(&1) {
    // ///     *x = "b";
    // /// }
    // /// assert_eq!(map[&1], "b");
    // /// ```
    // BTreeMap Implementation
    // pub fn get_mut<Q: ?Sized>(&mut self, key: &Q) -> Option<&mut V>
    // where
    //     K: Borrow<Q> + Ord,
    //     Q: Ord,
    // {
    //     let root_node = self.root.as_mut()?.borrow_mut();
    //     match root_node.search_tree(key) {
    //         Found(handle) => Some(handle.into_val_mut()),
    //         GoDown(_) => None,
    //     }
    // }

    // TODO: for BTreeMap parity, return value on overlap? What to do with multiple overlaps?
    // Maybe return another RangeMap with the removed segments? But that would allocate,
    // so that could be opt-in via a similar function (insert_returning, or something)?

    pub fn insert(&mut self, range: Range<K>, value: V) {
        // We don't want to have to make empty ranges make sense;
        // they don't represent anything meaningful in this structure.
        assert!(range.start < range.end);

        // Wrap up the given range so that we can "borrow"
        // it as a wrapper reference to either its start or end.
        // See `range_wrapper.rs` for explanation of these hacks.
        let mut new_range_start_wrapper: RangeStartWrapper<K> = RangeStartWrapper::new(range);
        let new_value = value;

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
        if let Some((prev_range_start_wrapper, prev_range_value)) = self
            .btm
            .range((
                Bound::Unbounded,
                Bound::Included(new_range_start_wrapper.clone()),
            ))
            .rev()
            .take_while(|(r, _)| r.range.touches(&range))
            .last()
        {
            // TODO: clean up
            self.adjust_touching_ranges_for_insert(
                prev_range_start_wrapper.clone(),
                prev_range_value.clone(),
                &mut new_range_start_wrapper.range,
                &new_value,
            );
        }

        // Are there any stored ranges whose heads overlap or immediately
        // follow the range to insert?
        //
        // If there are any such stored ranges (that weren't already caught above),
        // their starts will fall somewhere after the start of the range to insert,
        // and on or before its end.
        //
        // This time around, if the latter holds, it also implies
        // the former so we don't need to check here if they touch.
        //
        // REVISIT: Possible micro-optimisation: `impl Borrow<T> for RangeStartWrapper<T>`
        // and use that to search here, to avoid constructing another `RangeStartWrapper`.
        let new_range_end = RangeStartWrapper::point(new_range_start_wrapper.range.end.clone());
        for (stored_range_start_wrapper, stored_value) in self.btm.range((
            Bound::Included(&new_range_start_wrapper),
            Bound::Included(&new_range_end),
        )) {
            // One extra exception: if we have different values,
            // and the stored range starts at the end of the range to insert,
            // then we don't want to keep looping forever trying to find more!
            #[allow(clippy::suspicious_operation_groupings)]
            if stored_range_start_wrapper.range.start == new_range_start_wrapper.range.end
                && *stored_value != new_value
            {
                // We're beyond the last stored range that could be relevant.
                // Avoid wasting time on irrelevant ranges, or even worse, looping forever.
                // (`adjust_touching_ranges_for_insert` below assumes that the given range
                // is relevant, and behaves very poorly if it is handed a range that it
                // shouldn't be touching.)
                break;
            }

            let stored_range_start_wrapper = stored_range_start_wrapper.clone();
            let stored_value = stored_value.clone();

            self.adjust_touching_ranges_for_insert(
                stored_range_start_wrapper,
                stored_value,
                &mut new_range_start_wrapper.range,
                &new_value,
            );
        }

        // Insert the (possibly expanded) new range, and we're done!
        self.btm.insert(new_range_start_wrapper, new_value);
    }

    // TODO: similar to insert(), can we return values? overlaps?
    /// Removes a range from the map, if all or any of it was present.
    ///
    /// If the range to be removed _partially_ overlaps any ranges
    /// in the map, then those ranges will be contracted to no
    /// longer cover the removed range.
    ///
    ///
    /// # Panics
    ///
    /// Panics if range `start >= end`.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// use rangemap::RangeMap;
    ///
    /// let mut map = RangeMap::new();
    /// map.insert(0..10, "a");
    /// map.remove(3..5);
    /// assert_eq!(map.get(&0), Some(&"a"));
    /// assert_eq!(map.get(&3), None);
    /// assert_eq!(map.get(&5), Some(&"a"));
    /// assert_eq!(map.get(&8), Some(&"a"));
    /// ```
    pub fn remove(&mut self, range: Range<K>) {
        // We don't want to have to make empty ranges make sense;
        // they don't represent anything meaningful in this structure.
        assert!(range.start < range.end);

        let range_start_wrapper: RangeStartWrapper<K> = RangeStartWrapper::new(range);
        let range = &range_start_wrapper.range;

        // Is there a stored range overlapping the start of
        // the range to insert?
        //
        // If there is any such stored range, it will be the last
        // whose start is less than or equal to the start of the range to insert.
        if let Some((stored_range_start_wrapper, stored_value)) = self
            .btm
            .range((Bound::Unbounded, Bound::Included(&range_start_wrapper)))
            .next_back()
            .filter(|(stored_range_start_wrapper, _stored_value)| {
                // Does the only candidate range overlap
                // the range to insert?
                stored_range_start_wrapper.range.overlaps(&range)
            })
            .map(|(stored_range_start_wrapper, stored_value)| {
                (stored_range_start_wrapper.clone(), stored_value.clone())
            })
        {
            self.adjust_overlapping_ranges_for_remove(
                stored_range_start_wrapper,
                stored_value,
                &range,
            );
        }

        // Are there any stored ranges whose heads overlap the range to insert?
        //
        // If there are any such stored ranges (that weren't already caught above),
        // their starts will fall somewhere after the start of the range to insert,
        // and before its end.
        //
        // REVISIT: Possible micro-optimisation: `impl Borrow<T> for RangeStartWrapper<T>`
        // and use that to search here, to avoid constructing another `RangeStartWrapper`.
        let new_range_end_as_start = RangeStartWrapper::new(range.end.clone()..range.end.clone());
        while let Some((stored_range_start_wrapper, stored_value)) = self
            .btm
            .range((
                Bound::Excluded(&range_start_wrapper),
                Bound::Excluded(&new_range_end_as_start),
            ))
            .next()
            .map(|(stored_range_start_wrapper, stored_value)| {
                (stored_range_start_wrapper.clone(), stored_value.clone())
            })
        {
            self.adjust_overlapping_ranges_for_remove(
                stored_range_start_wrapper,
                stored_value,
                &range,
            );
        }
    }

    // TODO: Implement remove_entry()? It could just be folded into remove, returning the
    // original range of the items removed... But we get the same overlap issue with insert



    fn adjust_touching_ranges_for_insert(
        &mut self,
        stored_range_start_wrapper: RangeStartWrapper<K>,
        stored_value: V,
        new_range: &mut Range<K>,
        new_value: &V,
    ) {
        use core::cmp::{max, min};

        if stored_value == *new_value {
            // The ranges have the same value, so we can "adopt"
            // the stored range.
            //
            // This means that no matter how big or where the stored range is,
            // we will expand the new range's bounds to subsume it,
            // and then delete the stored range.
            new_range.start =
                min(&new_range.start, &stored_range_start_wrapper.range.start).clone();
            new_range.end = max(&new_range.end, &stored_range_start_wrapper.range.end).clone();
            self.btm.remove(&stored_range_start_wrapper);
        } else {
            // The ranges have different values.
            if new_range.overlaps(&stored_range_start_wrapper.range) {
                // The ranges overlap. This is a little bit more complicated.
                // Delete the stored range, and then add back between
                // 0 and 2 subranges at the ends of the range to insert.
                self.btm.remove(&stored_range_start_wrapper);
                if stored_range_start_wrapper.range.start < new_range.start {
                    // Insert the piece left of the range to insert.
                    self.btm.insert(
                        RangeStartWrapper::new(
                            stored_range_start_wrapper.range.start..new_range.start.clone(),
                        ),
                        stored_value.clone(),
                    );
                }
                if stored_range_start_wrapper.range.end > new_range.end {
                    // Insert the piece right of the range to insert.
                    self.btm.insert(
                        RangeStartWrapper::new(
                            new_range.end.clone()..stored_range_start_wrapper.range.end,
                        ),
                        stored_value,
                    );
                }
            } else {
                // No-op; they're not overlapping,
                // so we can just keep both ranges as they are.
            }
        }
    }

    fn adjust_overlapping_ranges_for_remove(
        &mut self,
        stored_range_start_wrapper: RangeStartWrapper<K>,
        stored_value: V,
        range_to_remove: &Range<K>,
    ) {
        // Delete the stored range, and then add back between
        // 0 and 2 subranges at the ends of the range to insert.
        self.btm.remove(&stored_range_start_wrapper);
        let stored_range = stored_range_start_wrapper.range;
        if stored_range.start < range_to_remove.start {
            // Insert the piece left of the range to insert.
            self.btm.insert(
                RangeStartWrapper::new(stored_range.start..range_to_remove.start.clone()),
                stored_value.clone(),
            );
        }
        if stored_range.end > range_to_remove.end {
            // Insert the piece right of the range to insert.
            self.btm.insert(
                RangeStartWrapper::new(range_to_remove.end.clone()..stored_range.end),
                stored_value,
            );
        }
    }

    // TODO: Remove
    // fn get_segment_containing(&self, k: &K) -> Option<(&Range<K>, &V)> {
    //     // Get the first range starting before this point (if one exists)
    //     // and check if it contains this point
    //     self.get_segments_starting_before(k)
    //         .next()
    //         .filter(|(range, _)| range.contains(k))
    // }
    // fn get_segment_containing_mut(&mut self, k: &K) -> Option<(&Range<K>, &mut V)> {
    //     // Get the first range starting before this point (if one exists)
    //     // and check if it contains this point
    //     self.get_segments_starting_before_mut(k)
    //         .next()
    //         .filter(|(range, _)| range.contains(k))
    // }
    // /// Get an iterator of keys and values starting before a specified point
    // /// (decreasing / moving to the left)
    // fn get_segments_starting_before(&self, k: &K) -> impl Iterator<Item = (&Range<K>, &V)> {
    //     self.btm
    //         .range((
    //             Bound::Unbounded,
    //             Bound::Included(RangeStartWrapper::new(k.clone()..k.clone())),
    //         ))
    //         .rev()
    //         .map(|(w, v)| (&w.range, v))
    // }
    // /// Get an iterator of mutable keys and values starting before a specified
    // /// point (decreasing / moving to the left)
    // fn get_segments_starting_before_mut(
    //     &mut self,
    //     k: &K,
    // ) -> impl Iterator<Item = (&Range<K>, &mut V)> {
    //     self.btm
    //         .range_mut((
    //             Bound::Unbounded,
    //             Bound::Included(RangeStartWrapper::new(k.clone()..k.clone())),
    //         ))
    //         .rev()
    //         .map(|(w, v)| (&w.range, v))
    // }
}

impl<K: Hash, V: Hash> Hash for RangeMap<K, V> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        for elt in self {
            elt.hash(state);
        }
    }
}

impl<K, V> Default for RangeMap<K, V>
where
    K: Ord + Clone,
    V: Eq + Clone,
{
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
        self.btm.iter().partial_cmp(other.btm.iter())
    }
}
impl<K: Ord, V: Ord> Ord for RangeMap<K, V> {
    #[inline]
    fn cmp(&self, other: &Self) -> Ordering {
        self.btm.iter().cmp(other.btm.iter())
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
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_map().entries(self.iter()).finish()
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
