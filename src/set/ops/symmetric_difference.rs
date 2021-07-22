use core::{cmp::Ordering::*, fmt::Debug, iter::FusedIterator};

use crate::{map::Key, set::iterators::Iter, Range, RangeMap, RangeSet};

impl<T> RangeSet<T> {
    // TODO: into_difference_iter

    pub fn symmetric_difference_iter<'a>(&'a self, other: &'a Self) -> SymmetricDifference<'a, T> {
        SymmetricDifference {
            iter_a: self.iter(),
            prev_a: None,
            iter_b: other.iter(),
            prev_b: None,
        }
    }

    // TODO: into_symmetric_difference

    pub fn symmetric_difference<'a>(&'a self, other: &'a Self) -> RangeSet<&'a T>
    where
        T: Ord,
    {
        // Don't need to insert, since we know ranges produced by the iterator
        // aren't overlapping
        RangeSet {
            map: RangeMap {
                map: self
                    .symmetric_difference_iter(other)
                    .map(|r| (Key(r), ()))
                    .collect(),
                store: alloc::vec::Vec::new(),
            },
        }
    }
}

/// Set Symmetric Difference
impl<'a, T: Ord + Clone> core::ops::BitXor<&'a RangeSet<T>> for &'a RangeSet<T> {
    type Output = RangeSet<&'a T>;

    // TODO: docs

    /// Returns the symmetric difference of `self` and `rhs` as a new `BTreeSet<T>`.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::BTreeSet;
    ///
    /// let a: BTreeSet<_> = vec![1, 2, 3].into_iter().collect();
    /// let b: BTreeSet<_> = vec![2, 3, 4].into_iter().collect();
    ///
    /// let result = &a ^ &b;
    /// let result_vec: Vec<_> = result.into_iter().collect();
    /// assert_eq!(result_vec, [1, 4]);
    /// ```
    fn bitxor(self, rhs: &'a RangeSet<T>) -> RangeSet<&'a T> {
        self.symmetric_difference(rhs)
    }
}

// TODO: BitXorAssign for symmetric difference? Maybe omit, unless a good use case comes up
// /// Set in-place symmetric difference  // TODO: self.into_symmetric_difference() may be quicker for these?
// impl<T: Ord + Clone> core::ops::BitXorAssign<&RangeSet<T>> for RangeSet<T> {
//     fn bitxor_assign(&mut self, rhs: &RangeSet<T>) {

//     }
// }
// impl<T: Ord + Clone> core::ops::BitXorAssign<RangeSet<T>> for RangeSet<T> {
//     fn sub_assign(&mut self, rhs: RangeSet<T>) {
//         for range in rhs.iter() {
//             self.remove(range);
//         }
//     }
// }

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
                        end: next_a.end,
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
                        end: next_b.end,
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
