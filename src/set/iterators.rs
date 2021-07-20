use core::{
    cmp::Ordering::*,
    fmt::{self, Debug},
    iter::{FromIterator, FusedIterator},
    ops::RangeBounds,
};

use crate::{Range, RangeSet};
// TODO: all doctests

/// See [`alloc::collections::btree_set::ITER_PERFORMANCE_TIPPING_SIZE_DIFF`]
///
/// The std library uses `ITER_PERFORMANCE_TIPPING_SIZE_DIFF` to split whether
/// to iterate two items in tandem or search through one (the larger) while
/// iterating through the smaller. Let's trust they know what they're doing,
/// though that tipping point may be different for ranges that require an extra
/// step in the search (to find the last range starting BEFORE a point).
const ITER_PERFORMANCE_TIPPING_SIZE_DIFF: usize = 16;

impl<T> RangeSet<T> {
    pub fn iter(&self) -> Iter<'_, T> {
        Iter(self.map.ranges())
    }

    pub fn len(&self) -> usize {
        self.map.len()
    }

    pub fn is_empty(&self) -> bool {
        self.map.is_empty()
    }

    // range?

    /// Check if two sets have overlapping outer ranges
    /// this is mostly useful for iterator shortcuts
    // fn overlaps(&self, other: &Self) -> bool
    // where
    //     T: Ord,
    // {
    //     if let Some((self_range, other_range)) = self
    //         .map
    //         .bounds()
    //         .map(|s| other.map.bounds().map(|o| (s, o)))
    //         .flatten()
    //     {
    //         // If both ranges are bounded, check that they overlap
    //         self_range.overlaps(&other_range)
    //     } else {
    //         // Otherwise, one or both are empty and they do not intersect
    //         false
    //     }
    // }

    pub fn difference<'a>(&'a self, other: &'a Self) -> Difference<'a, T>
    where
        T: Clone + Ord,
    {
        if let Some((self_range, other_range)) = self
            .map
            .bounds()
            .map(|s| other.map.bounds().map(|o| (s, o)))
            .flatten()
        {
            // If both ranges are bounded and overlap, perform a difference
            if self_range.overlaps(&other_range) {
                // // TODO: Use the tipping point, but only for the overlapping region?
                // if self.len() <= (other.len() / ITER_PERFORMANCE_TIPPING_SIZE_DIFF) {
                //     return Difference(DifferenceInner::Search {
                //         iter: self.iter(),
                //         prev: None,
                //         other,
                //     });
                // } else {
                return Difference(DifferenceInner::Stitch {
                    self_iter: self.iter(),
                    prev_self: None,
                    other_iter: other.iter(),
                    prev_other: None,
                });
                // }
            }
        }

        // If either range is empty or they don't overlap, the difference is
        // strictly `self`.
        Difference(DifferenceInner::Iterate(self.iter()))
    }

    pub fn symmetric_difference<'a>(&'a self, other: &'a Self) -> SymmetricDifference<'a, T> {
        SymmetricDifference {
            iter_a: self.iter(),
            prev_a: None,
            iter_b: other.iter(),
            prev_b: None,
        }
    }

    pub fn intersection<'a>(&'a self, other: &'a Self) -> Intersection<'a, T> {
        todo!()
    }

    pub fn union<'a>(&'a self, other: &'a Self) -> Union<'a, T> {
        todo!()
    }

    // TODO: names?
    // pub fn iter_complement(&self) -> impl Iterator<Item = Range<T>> {
    //     self.map.iter_complement()
    // }
    // pub fn gaps(&self) -> impl Iterator<Item = Range<T>>
    // where
    //     T: Ord + Clone,
    // {
    //     self.map.gaps()
    // }

    // /// Gets an iterator over all the maximally-sized ranges
    // /// contained in `outer_range` that are not covered by
    // /// any range stored in the set.
    // ///
    // /// The iterator element type is `Range<T>`.
    // ///
    // /// NOTE: Calling `gaps` eagerly finds the first gap,
    // /// even if the iterator is never consumed.
    // pub fn gaps_in<R: RangeBounds<T>>(&self, outer_range: R) -> impl Iterator<Item = Range<T>> {
    //     self.map.gaps_in(outer_range).map(|(k, v)| k)
    // }

    // TODO
    // pub fn extend_into_gaps(&mut self)
}

// impl<T: Clone + Ord> FromIterator<Range<T>> for RangeSet<T> {
//     fn from_iter<I: IntoIterator<Item = Range<T>>>(iter: I) -> RangeSet<T> {
//         let mut set = Self::new();
//         set.extend(iter);
//         set
//     }
// }

/// TODO: document use of Clone, and set ordering
impl<Item: RangeBounds<T>, T: Clone + Ord> FromIterator<Item> for RangeSet<T> {
    fn from_iter<I: IntoIterator<Item = Item>>(iter: I) -> Self {
        let mut set = Self::new();
        for item in iter {
            set.set(item);
        }
        set
    }
}

impl<T: Clone + Ord> Extend<Range<T>> for RangeSet<T> {
    /// Insert all the items from `iter` into `self`.
    ///
    /// **NOTE**: Inserted items will overwrite existing ranges in `self` if
    /// they overlap. If you don't want to overwrite existing ranges, use
    /// [`extend_into_gaps`].
    ///
    /// Clone is required for insertion, since we can't guarantee elements in `iter`
    /// are ordered or non-overlapping, so ranges may need to be split.
    #[inline]
    fn extend<Iter: IntoIterator<Item = Range<T>>>(&mut self, iter: Iter) {
        // self.map.extend(iter.into_iter().map(|t| (t, ())))
        iter.into_iter().for_each(move |range| {
            self.set(range);
        });
    }

    // #[inline]
    // fn extend_one(&mut self, elem: T) {
    //     self.insert(elem);
    // }
}

impl<'a, T: 'a + Ord + Copy> Extend<&'a Range<T>> for RangeSet<T> {
    fn extend<I: IntoIterator<Item = &'a Range<T>>>(&mut self, iter: I) {
        self.extend(iter.into_iter().cloned());
    }

    // #[inline]
    // fn extend_one(&mut self, &elem: &'a T) {
    //     self.insert(elem);
    // }
}

#[derive(Clone)]
pub struct Iter<'a, T: 'a>(crate::map::iterators::Ranges<'a, T, ()>);

impl<T: fmt::Debug> fmt::Debug for Iter<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Iter").field(&self.0.clone()).finish()
    }
}
impl<'a, T> Iterator for Iter<'a, T> {
    type Item = &'a Range<T>;

    fn next(&mut self) -> Option<&'a Range<T>> {
        self.0.next()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }

    fn last(mut self) -> Option<&'a Range<T>> {
        self.next_back()
    }

    fn min(mut self) -> Option<&'a Range<T>> {
        self.next()
    }

    fn max(mut self) -> Option<&'a Range<T>> {
        self.next_back()
    }
}

impl<T> FusedIterator for Iter<'_, T> {}

impl<'a, T> DoubleEndedIterator for Iter<'a, T> {
    fn next_back(&mut self) -> Option<&'a Range<T>> {
        self.0.next_back()
    }
}

impl<T> ExactSizeIterator for Iter<'_, T> {
    fn len(&self) -> usize {
        self.0.len()
    }
}

impl<T> IntoIterator for RangeSet<T> {
    type Item = Range<T>;
    type IntoIter = IntoIter<T>;

    // TODO: docs
    fn into_iter(self) -> Self::IntoIter {
        IntoIter(self.map.into_iter())
    }
}

pub struct IntoIter<T>(crate::map::iterators::IntoIter<T, ()>);
impl<T: fmt::Debug> fmt::Debug for IntoIter<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}
impl<T> Iterator for IntoIter<T> {
    type Item = Range<T>;
    fn next(&mut self) -> Option<Range<T>> {
        self.0.next().map(|(range, _)| range)
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}
impl<T> DoubleEndedIterator for IntoIter<T> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.0.next_back().map(|(range, _)| range)
    }
}
impl<T> FusedIterator for IntoIter<T> {}

// TODO: Implement Difference Search method

pub struct Difference<'a, T: Ord>(DifferenceInner<'a, T>);
#[derive(Debug)]
enum DifferenceInner<'a, T: 'a + Ord> {
    /// Iterate all of `self` and some of `other`, spotting matches along the
    /// way. The std lib uses Peekable here, which doesn't quite work for us,
    /// since we need to store a possibly split range (not the original range)
    Stitch {
        self_iter: Iter<'a, T>,
        prev_self: Option<Range<&'a T>>,

        other_iter: Iter<'a, T>,
        prev_other: Option<Range<&'a T>>,
    },

    // /// If other is large, we search through it instead of iterating
    // /// Trusting the stdlib knows what it's doing here...
    // ///
    // /// TODO: bench if Search mode is useful at all. Defaulting to include it
    // /// because stdlib does.
    // Search {
    //     iter: Iter<'a, T>,
    //     other: &'a RangeSet<T>,

    //     /// Stored item, along with references to any overlapping ranges already
    //     /// queried (since they'll be immediately needed in the next iteration)
    //     prev: Option<(Range<&'a T>, alloc::vec::Vec<&'a Range<T>>)>,
    // },
    /// For non-overlapping sets, just produce everything in self
    ///
    /// This is also the case if `self` or `other` is empty
    Iterate(Iter<'a, T>),
}

// impl<T: Ord + fmt::Debug> fmt::Debug for Difference<'_, T> {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         f.debug_tuple("Difference").field(&self.0.clone()).finish()
//     }
// }

// TODO: document why Range<&'a T> instead of &'a Range<T>
// TODO: combine Stitch and Search inner loop logic?
impl<'a, T: Ord> Iterator for Difference<'a, T> {
    type Item = Range<&'a T>;

    fn next(&mut self) -> Option<Range<&'a T>> {
        match &mut self.0 {
            DifferenceInner::Iterate(iter) => iter.next().map(|x| x.as_ref()),
            DifferenceInner::Stitch {
                self_iter,
                prev_self,

                other_iter,
                prev_other,
            } => {
                // If we stored a previous range, it's a split (so take it)
                // If there aren't any left, we're finished
                let mut range = prev_self
                    .take()
                    .or_else(|| self_iter.next().map(|x| x.as_ref()))?;

                // Look for items in `other` until we run out
                loop {
                    if let Some(other) = prev_other
                        .take()
                        .or_else(|| other_iter.next().map(|x| x.as_ref()))
                    {
                        // If `range` is still fully before `other`, use it (and
                        // hold on to `other`)
                        if range.end.cmp_start(&other.start).is_gt() {
                            prev_other.insert(other);
                            return Some(range);
                        }

                        // If `range` is fully after `other`, grab the next
                        // `other` (and loop again)
                        if range.start.cmp_end(&other.end).is_gt() {
                            continue;
                        }

                        // Otherwise, we have some overlap
                        //
                        // The `borrow_bound_x().unwrap()` calls below should be
                        // fine, since they only occur where the match precludes
                        // an unbounded start/end.
                        match (range.start.cmp(&other.start), range.end.cmp(&other.end)) {
                            // Partial overlap, but `left` doesn't extend beyond `right`
                            // We can use part of `left` and forget the rest
                            (Less, Less) => {
                                range.end = other.borrow_bound_before().unwrap();
                                prev_other.insert(other);
                                return Some(range);
                            }

                            // Partial overlap where `left` extends just to the
                            // end of `right` (don't save `right`)
                            (Less, Equal) => {
                                range.end = other.borrow_bound_before().unwrap();
                                return Some(range);
                            }

                            // Fully overlapped, but with some excess `right`
                            // Keep it and loop again with a new `left`.
                            (Greater | Equal, Less) => {
                                range = self_iter.next()?.as_ref();
                                prev_other.insert(other);
                                continue;
                            }

                            // Fully overlapped but with no more `right`, loop
                            // again with a new one of each
                            (Greater | Equal, Equal) => {
                                range = self_iter.next()?.as_ref();
                                continue;
                            }

                            // Partial overlap, but some `left` past `right`
                            // Keep part of `left` and look for a new `right`
                            (Greater | Equal, Greater) => {
                                range.start = other.borrow_bound_after().unwrap();
                                continue;
                            }

                            // `left` extends beyond `right` in both directions.
                            // Use the part of `left` before `right` and store
                            // the part after.
                            (Less, Greater) => {
                                prev_self.insert(Range {
                                    start: other.borrow_bound_after().unwrap(),
                                    end: core::mem::replace(
                                        &mut range.end,
                                        other.borrow_bound_before().unwrap(),
                                    ),
                                });
                                return Some(range);
                            }
                        }
                    } else {
                        return Some(range);
                    }
                }
            } // DifferenceInner::Search { iter, other, prev } => {

              //     // Get either the next item in `iter`, or the previously stored
              //     // state. Short circuit out if there are no more items in iter.
              //     let (next, mut overlaps) = prev.take().or_else(|| {
              //         let next = iter.next()?.as_ref();
              //         let overlaps = if let Some(after) = next.end.borrow_after() {
              //             other.map.map.range(..=after.cloned())
              //         } else {
              //             other.map.map.iter()
              //         }
              //         .rev().filter_map(|(k, _)| if &k.0.overlaps(other) )
              //         .collect();

              //         Some((next, overlaps))
              //     })?;

              //     if overlaps.is_empty() {
              //         return Some(next);
              //     }

              //     // Get next segment from overlaps
              //     loop {

              //     }
              // }
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let (self_len, other_len) = match &self.0 {
            DifferenceInner::Stitch {
                self_iter,
                other_iter,
                ..
            } => (self_iter.len(), other_iter.len()),
            // DifferenceInner::Search { iter, other, .. } => (iter.len(), other.len()),
            DifferenceInner::Iterate(iter) => (iter.len(), 0),
        };
        (self_len.saturating_sub(other_len), Some(self_len))
    }

    fn min(mut self) -> Option<Range<&'a T>> {
        self.next()
    }
}

#[derive(Debug, Clone)]
pub struct SymmetricDifference<'a, T> {
    iter_a: Iter<'a, T>,
    prev_a: Option<Range<&'a T>>,
    iter_b: Iter<'a, T>,
    prev_b: Option<Range<&'a T>>,
}

// impl<T: fmt::Debug> fmt::Debug for SymmetricDifference<'_, T> {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         f.debug_tuple("SymmetricDifference").field(&self.).finish()
//     }
// }

// impl<T> Clone for SymmetricDifference<'_, T> {
//     fn clone(&self) -> Self {
//         Self {

//         }
//     }
// }

impl<'a, T: Ord> Iterator for SymmetricDifference<'a, T> {
    type Item = Range<&'a T>;

    fn next(&mut self) -> Option<Range<&'a T>> {
        let next_a = self
            .prev_a
            .take()
            .or_else(|| self.iter_a.next().map(|x| x.as_ref()));

        let next_b = self
            .prev_b
            .take()
            .or_else(|| self.iter_b.next().map(|x| x.as_ref()));

        // If one ran out, use the other
        let mut next_a = match next_a {
            Some(a) => a,
            None => return next_b,
        };
        let mut next_b = match next_b {
            Some(b) => b,
            None => return Some(next_a),
        };

        // Otherwise, look for mutually exclusive items
        loop {
            // If `next_a` is fully before `next_b`, use it
            // (and hold on to `next_b`)
            if next_a.end.cmp_start(&next_b.start).is_gt() {
                self.prev_b.insert(next_b);
                return Some(next_b);
            }

            // Likewise the other way around
            if next_a.start.cmp_end(&next_b.end).is_gt() {
                self.prev_a.insert(next_a);
                return Some(next_a);
            }

            // Otherwise, we have some overlap
            //
            // Similar to Difference impl
            match (next_a.start.cmp(&next_b.start), next_a.end.cmp(&next_b.end)) {
                // Partial overlap, but `a` doesn't extend beyond `b`.
                // Use the non-overlapped part of `a` and remember to remove
                // the overlap from `b` for the next iteration.
                (Less, Less) => {
                    // When removing the overlapped region, use `replace` to
                    // make sure we do it in order.
                    next_b.start =
                        core::mem::replace(&mut next_a.end, next_b.borrow_bound_before().unwrap())
                            .borrow_after()
                            .unwrap();
                    self.prev_b.insert(next_b);
                    return Some(next_a);
                }

                // Partial overlap where `a` extends just to the
                // end of `b` (don't save `b`)
                (Less, Equal) => {
                    next_a.end = next_b.borrow_bound_before().unwrap();
                    return Some(next_a);
                }

                // `a` extends beyond `b` in both directions.
                // Use the part of `a` before `b` and store
                // the part after.
                (Less, Greater) => {
                    self.prev_a.insert(Range {
                        start: next_b.borrow_bound_after().unwrap(),
                        end: next_a.end.clone(),
                    });
                    next_a.end = next_b.borrow_bound_before().unwrap();
                    return Some(next_a);
                }

                // `b` extends past `a`. Remove the overlap and loop
                // (if necessary)
                (Equal, Less) => {
                    next_b.start = next_a.borrow_bound_after().unwrap();
                    if let Some(a) = self.iter_a.next() {
                        next_a = a.as_ref();
                        continue;
                    } else {
                        // No more `a`s, just return this `b` part
                        return Some(next_b);
                    }
                }

                // Both exactly overlap each other. loop!
                // (or return early because we're out of items in one)
                (Equal, Equal) => {
                    if let Some(a) = self.iter_a.next().map(|x| x.as_ref()) {
                        next_a = a;
                    } else {
                        // But no more `a`s
                        return Some(next_b);
                    }
                    if let Some(b) = self.iter_b.next().map(|x| x.as_ref()) {
                        next_b = b;
                    } else {
                        // But no more `b`s
                        return Some(next_a);
                    }
                    continue;
                }

                // Partial overlap, but some `b` past `a`
                // Keep part of `a` and look for a new `b`
                (Equal, Greater) => {
                    next_a.start = next_b.borrow_bound_after().unwrap();
                    if let Some(b) = self.iter_b.next().map(|x| x.as_ref()) {
                        next_b = b;
                    } else {
                        return Some(next_b);
                    }
                    continue;
                }

                // `b` extends beyond `a` in both directions.
                // Use the part of `b` before `a` and store
                // the part after.
                (Greater, Less) => {
                    self.prev_b.insert(Range {
                        start: next_a.borrow_bound_after().unwrap(),
                        end: next_b.end.clone(),
                    });
                    next_b.end = next_a.borrow_bound_before().unwrap();
                    return Some(next_b);
                }

                // Partial overlap, where `b` extends before `a`, but they
                // end together.
                (Greater, Equal) => {
                    next_b.end = next_a.borrow_bound_before().unwrap();
                    return Some(next_b);
                }

                // Partial overlap, but `b` doesn't extend beyond `a`.
                // Use the non-overlapped part of `b` and remember to remove
                // the overlap from `a` for the next iteration.
                (Greater, Greater) => {
                    // When removing the overlapped region, use `replace` to
                    // make sure we do it in order. (Similar to (Less, Less))
                    next_a.start =
                        core::mem::replace(&mut next_b.end, next_a.borrow_bound_before().unwrap())
                            .borrow_after()
                            .unwrap();
                    self.prev_a.insert(next_a);
                    return Some(next_b);
                }
            }
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (0, Some(self.iter_a.len() + self.iter_b.len()))
    }

    fn min(mut self) -> Option<Range<&'a T>> {
        self.next()
    }
}

impl<T: Ord> FusedIterator for SymmetricDifference<'_, T> {}

pub struct Intersection<'a, T> {
    left: Iter<'a, T>,
    right: Iter<'a, T>,
}
pub struct Union<'a, T> {
    left: Iter<'a, T>,
    right: Iter<'a, T>,
}

// TODO: Debug difference
