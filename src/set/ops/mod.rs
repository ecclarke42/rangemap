//! Common Set operations for RangeSet

use crate::{map::Key, RangeBounds, RangeMap, RangeSet};

pub mod difference;
pub mod intersection;
pub mod symmetric_difference;
pub mod union;

impl<T: Ord> RangeSet<T> {
    /// Check whether `self` and `other` are disjoint sets
    ///
    /// That is, the intersection between `self` and `other` is empty
    pub fn is_disjoint(&self, other: &Self) -> bool {
        self.iter_intersection(other).next().is_none()
    }

    /// Check whether `self` is a subset of `other`
    ///
    /// That is, all elements of `self` exist in `other`, or (as implemented)
    /// `self.difference(other)` is empty
    pub fn is_subset(&self, other: &Self) -> bool {
        self.iter_difference(other).next().is_none()
    }

    pub fn is_superset(&self, other: &Self) -> bool {
        other.is_subset(self)
    }

    // TODO: No Clone
    // TODO: subset(&self, range: R) -> Self; slightly faster than ranges(..).filter().collect() because it doesn't need to check insertions
    pub fn subset<R: RangeBounds<T>>(&self, range: R) -> RangeSet<T>
    where
        T: Clone + Ord,
    {
        RangeSet {
            map: RangeMap {
                map: self
                    .map
                    .iter_subset(range)
                    .map(|(r, _)| (Key(r), ()))
                    .collect(),
                store: alloc::vec::Vec::new(),
            },
        }
    }

    // as_complement / into_complement?
    pub fn complement(&self) -> RangeSet<&T>
    where
        T: Ord,
    {
        self.map.complement()
    }
}

/// Set Complement
impl<'a, T: Ord + Clone> core::ops::Not for &'a RangeSet<T> {
    type Output = RangeSet<&'a T>;

    // TODO: docs
    fn not(self) -> Self::Output {
        self.complement()
    }
}

impl<T: Ord + Clone> core::ops::Not for RangeSet<T> {
    type Output = RangeSet<T>;

    fn not(self) -> Self::Output {
        self.complement().cloned()
    }
}
