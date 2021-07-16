use core::{
    cmp::Ordering::*,
    fmt::{self, Debug},
    iter::{FromIterator, FusedIterator, Peekable},
    ops::RangeBounds,
};

use crate::{Range, RangeMap, RangeSet};
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
                // TODO: Use the tipping point, but only for the overlapping region?
                if self.len() <= (other.len() / ITER_PERFORMANCE_TIPPING_SIZE_DIFF) {
                    return Difference(DifferenceInner::Search {
                        self_iter: self.iter(),
                        other_set: other,
                    });
                } else {
                    return Difference(DifferenceInner::Stitch {
                        self_iter: self.iter().peekable(),
                        other_iter: other.iter().peekable(),
                    });
                }
            }
        }

        // If either range is empty or they don't overlap, the difference is
        // strictly `self`.
        Difference(DifferenceInner::Iterate(self.iter()))
    }

    pub fn symmetric_difference<'a>(&'a self, other: &'a Self) -> SymmetricDifference<'a, T> {
        todo!()
    }

    pub fn intersection<'a>(&'a self, other: &'a Self) -> Intersection<'a, T> {
        todo!()
    }

    pub fn union<'a>(&'a self, other: &'a Self) -> Union<'a, T> {
        todo!()
    }

    // TODO: names?
    pub fn iter_complement(&self) -> impl Iterator<Item = Range<T>> {
        self.map.iter_complement()
    }
    pub fn gaps(&self) -> impl Iterator<Item = Range<T>>
    where
        T: Ord + Clone,
    {
        self.map.gaps()
    }
    /// Gets an iterator over all the maximally-sized ranges
    /// contained in `outer_range` that are not covered by
    /// any range stored in the set.
    ///
    /// The iterator element type is `Range<T>`.
    ///
    /// NOTE: Calling `gaps` eagerly finds the first gap,
    /// even if the iterator is never consumed.
    pub fn gaps_in<R: RangeBounds<T>>(&self, outer_range: R) -> impl Iterator<Item = Range<T>> {
        self.map.gaps_in(outer_range).map(|(k, v)| k)
    }
}

impl<T: Clone + Ord> FromIterator<T> for RangeSet<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> RangeSet<T> {
        let mut set = Self::new();
        set.extend(iter);
        set
    }
}

impl<T: Clone + Ord> Extend<T> for RangeSet<T> {
    #[inline]
    fn extend<Iter: IntoIterator<Item = T>>(&mut self, iter: Iter) {
        // self.map.extend(iter.into_iter().map(|t| (t, ())))
        iter.into_iter().for_each(move |t| {
            self.insert(t);
        });
    }

    // #[inline]
    // fn extend_one(&mut self, elem: T) {
    //     self.insert(elem);
    // }
}

impl<'a, T: 'a + Ord + Copy> Extend<&'a T> for RangeSet<T> {
    fn extend<I: IntoIterator<Item = &'a T>>(&mut self, iter: I) {
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

pub struct IntoIter<T>(crate::map::iterators::IntoIter<T, ()>);

impl<T: fmt::Debug> fmt::Debug for IntoIter<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}
// Range?

#[derive(Clone)]
pub struct Difference<'a, T: Clone + Ord>(DifferenceInner<'a, T>);
#[derive(Debug, Clone)]
enum DifferenceInner<'a, T: 'a + Clone + Ord> {
    Stitch {
        // iterate all of `self` and some of `other`, spotting matches along the way
        self_iter: Peekable<Iter<'a, T>>,
        other_iter: Peekable<Iter<'a, T>>,
    },
    // Trusting the stdlib knows what it's doing here...
    // TODO: bench if Search mode is useful at all
    Search {
        // iterate `self`, look up in `other`
        self_iter: Iter<'a, T>,
        other_set: &'a RangeSet<T>,
    },
    Iterate(Iter<'a, T>), // simply produce all values in `self`
}

impl<T: Clone + Ord + fmt::Debug> fmt::Debug for Difference<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Difference").field(&self.0).finish()
    }
}

impl<'a, T: Clone + Ord> Iterator for Difference<'a, T> {
    type Item = &'a Range<T>;

    fn next(&mut self) -> Option<&'a Range<T>> {
        match &mut self.0 {
            DifferenceInner::Stitch {
                self_iter,
                other_iter,
            } => {
                // If no more in self, we're done
                let mut self_next = self_iter.peek()?;

                // Look for items in other until we run out
                loop {
                    if let Some(other_next) = other_iter.peek() {
                        // match (self_next.start.cmp(other_next.start), self_next.end.cmp(other_next.end)) {
                        //     ()
                        // }

                        // If start is still before other, use it
                        if self_next.end.cmp_start(&other_next.start).is_gt() {
                            return self_iter.next();
                        }

                        // If self has passed other, grab the next other]
                        if self_next.start.cmp_end(&other_next.end).is_gt() {
                            other_iter.next();
                            continue;
                        }

                        // Otherwise, we have some overlap
                        // If `other_next` ends before the end of start, we'll need to loop
                        match self_next.end.cmp(&other_next.end) {

                            // More of `other_next` left, leave it around for next iteration
                            Less => {

                            }

                            // Self and other end at the same place, pop both
                            Equal => {

                            }

                            // Self extends past other, so keep it around for the next iteration
                            Greater => {

                                // TODO: need to implement own Peekable type...
                                // We need to store PART of the peeked range, but not all of it (since we will return part of it here)

                            }


                        }
                    } else {
                        return self_iter.next();
                    }
                }
            }
            DifferenceInner::Search {
                self_iter,
                other_set,
            } => loop {
                let self_next = self_iter.next()?;

                todo!();
                // TODO: implement
                if !other_set.contains(&self_next) {
                    return Some(self_next);
                }
            },
            DifferenceInner::Iterate(iter) => iter.next(),
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let (self_len, other_len) = match &self.0 {
            DifferenceInner::Stitch {
                self_iter,
                other_iter,
            } => (self_iter.len(), other_iter.len()),
            DifferenceInner::Search {
                self_iter,
                other_set,
            } => (self_iter.len(), other_set.len()),
            DifferenceInner::Iterate(iter) => (iter.len(), 0),
        };
        (self_len.saturating_sub(other_len), Some(self_len))
    }

    fn min(mut self) -> Option<&'a Range<T>> {
        self.next()
    }
}

pub struct SymmetricDifference<'a, T> {
    left: Iter<'a, T>,
    right: Iter<'a, T>,
}
pub struct Intersection<'a, T> {
    left: Iter<'a, T>,
    right: Iter<'a, T>,
}
pub struct Union<'a, T> {
    left: Iter<'a, T>,
    right: Iter<'a, T>,
}

// TODO: Debug difference
