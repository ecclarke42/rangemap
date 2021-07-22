use core::cmp::Ordering::*;

use crate::{map::Key, set::iterators::Iter, Range, RangeMap, RangeSet};

impl<T> RangeSet<T> {
    // TODO: into_intersection_iter

    pub fn iter_intersection<'a>(&'a self, other: &'a Self) -> Intersection<'a, T> {
        Intersection {
            iter_a: self.iter(),
            prev_a: None,
            iter_b: other.iter(),
            prev_b: None,
        }
    }

    // TODO: into_intersection

    pub fn intersection<'a>(&'a self, other: &'a Self) -> RangeSet<&'a T>
    where
        T: Ord,
    {
        // Don't need to insert, since we know ranges produced by the iterator
        // aren't overlapping
        RangeSet {
            map: RangeMap {
                map: self
                    .iter_intersection(other)
                    .map(|r| (Key(r), ()))
                    .collect(),
                store: alloc::vec::Vec::new(),
            },
        }
    }
}

/// Set Intersection A & B
impl<'a, T: Ord + Clone> core::ops::BitAnd<&'a RangeSet<T>> for &'a RangeSet<T> {
    type Output = RangeSet<&'a T>;

    // TODO: docs

    /// Returns the intersection of `self` and `rhs` as a new `BTreeSet<T>`.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::BTreeSet;
    ///
    /// let a: BTreeSet<_> = vec![1, 2, 3].into_iter().collect();
    /// let b: BTreeSet<_> = vec![2, 3, 4].into_iter().collect();
    ///
    /// let result = &a & &b;
    /// let result_vec: Vec<_> = result.into_iter().collect();
    /// assert_eq!(result_vec, [2, 3]);
    /// ```
    fn bitand(self, rhs: &'a RangeSet<T>) -> RangeSet<&'a T> {
        self.intersection(rhs)
    }
}

// TODO: into_intersection
impl<'a, T: Ord + Clone> core::ops::BitAnd<RangeSet<T>> for RangeSet<T> {
    type Output = RangeSet<T>;
    fn bitand(self, rhs: RangeSet<T>) -> RangeSet<T> {
        self.intersection(&rhs).cloned()
    }
}

/// Set in-place intersection
// impl<T: Ord + Clone> core::ops::BitAndAssign<&RangeSet<T>> for RangeSet<T> {
//     fn bitxor_assign(&mut self, rhs: &RangeSet<T>) {}
// }
// impl<T: Ord + Clone> core::ops::BitAndAssign<RangeSet<T>> for RangeSet<T> {
//     fn sub_assign(&mut self, rhs: RangeSet<T>) {
//         for range in rhs.iter() {
//             self.remove(range);
//         }
//     }
// }

pub struct Intersection<'a, T> {
    iter_a: Iter<'a, T>,
    prev_a: Option<Range<&'a T>>,

    iter_b: Iter<'a, T>,
    prev_b: Option<Range<&'a T>>,
}

impl<'a, T: Ord> Iterator for Intersection<'a, T> {
    type Item = Range<&'a T>;
    fn next(&mut self) -> Option<Self::Item> {
        // Get the next values. If either ran out, we're done
        let mut next_a = self
            .prev_a
            .take()
            .or_else(|| self.iter_a.next().map(|x| x.as_ref()))?;

        let mut next_b = self
            .prev_b
            .take()
            .or_else(|| self.iter_b.next().map(|x| x.as_ref()))?;

        // Otherwise, find the next common item
        loop {
            // If `next_a` is fully before `next_b`, grab another and loop
            if next_a.end.cmp_start(&next_b.start).is_gt() {
                next_a = self.iter_a.next()?.as_ref();
                continue;
            }

            // Likewise the other way around
            if next_a.start.cmp_end(&next_b.end).is_gt() {
                next_b = self.iter_b.next()?.as_ref();
                continue;
            }

            // Otherwise, we have some overlap
            match (next_a.start.cmp(&next_b.start), next_a.end.cmp(&next_b.end)) {
                // Partial overlap, but `a` doesn't extend beyond `b`.
                // Use the overlapped part of `a` and remember to remove it from
                // `b` for the next iteration.
                (Less, Less) => {
                    next_a.start =
                        core::mem::replace(&mut next_b.start, next_a.borrow_bound_after().unwrap());
                    self.prev_b.insert(next_b);
                    return Some(next_a);
                }

                // Partial overlap where `a` extends just to the
                // end of `b` (just use `b`)
                (Less, Equal) => return Some(next_b),

                // `a` extends beyond `b` in both directions.
                // Return `b` but keep the last part of `a`
                (Less, Greater) => {
                    next_a.start = next_b.borrow_bound_after().unwrap();
                    self.prev_a.insert(next_a);
                    return Some(next_b);
                }

                // Partial overlap where `a` extends just to the
                // end of `b`. Use `a` and hold on to the end of `b`
                (Equal, Less) => {
                    next_b.start = next_a.borrow_bound_after().unwrap();
                    self.prev_b.insert(next_b);
                    return Some(next_a);
                }

                // Both exactly overlap each other
                (Equal, Equal) => return Some(next_a),

                // Partial overlap, but some `b` past `a`
                // Keep part of `a` and look for a new `b`
                (Equal, Greater) => {
                    next_a.start = next_b.borrow_bound_after().unwrap();
                    self.prev_a.insert(next_a);
                    return Some(next_b);
                }

                // `b` extends beyond `a` in both directions.
                // Use `a` and keep the end of `b`
                (Greater, Less) => {
                    next_b.start = next_a.borrow_bound_after().unwrap();
                    self.prev_b.insert(next_b);
                    return Some(next_a);
                }

                // Partial overlap, where `b` extends before `a`, but they
                // end together. Just return `a`
                (Greater, Equal) => return Some(next_a),

                // Partial overlap, but `b` doesn't extend beyond `a`.
                // Use the overlapped part of `b` and keep `a` for the next iteration.
                (Greater, Greater) => {
                    next_b.start =
                        core::mem::replace(&mut next_a.start, next_b.borrow_bound_after().unwrap());
                    self.prev_a.insert(next_a);
                    return Some(next_b);
                }
            }
        }
    }
}
