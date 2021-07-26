use crate::{map::Key, set::iterators::Iter, Segment, SegmentMap, SegmentSet};

impl<T> SegmentSet<T> {
    // TODO: into_union_iter

    pub fn union_iter<'a>(&'a self, other: &'a Self) -> Union<'a, T> {
        Union {
            iter_a: self.iter(),
            prev_a: None,
            iter_b: other.iter(),
            prev_b: None,
        }
    }

    // TODO: into_union

    pub fn union<'a>(&'a self, other: &'a Self) -> SegmentSet<&'a T>
    where
        T: Ord,
    {
        // Don't need to insert, since we know ranges produced by the iterator
        // aren't overlapping
        SegmentSet {
            map: SegmentMap {
                map: self.union_iter(other).map(|r| (Key(r), ())).collect(),
                store: alloc::vec::Vec::new(),
            },
        }
    }
}
/// Set Union
impl<T: Ord + Clone> core::ops::BitOr<&SegmentSet<T>> for &SegmentSet<T> {
    type Output = SegmentSet<T>;

    /// Returns the union of `self` and `rhs` as a new `BTreeSet<T>`.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::BTreeSet;
    ///
    /// let a: BTreeSet<_> = vec![1, 2, 3].into_iter().collect();
    /// let b: BTreeSet<_> = vec![3, 4, 5].into_iter().collect();
    ///
    /// let result = &a | &b;
    /// let result_vec: Vec<_> = result.into_iter().collect();
    /// assert_eq!(result_vec, [1, 2, 3, 4, 5]);
    /// ```
    fn bitor(self, rhs: &SegmentSet<T>) -> SegmentSet<T> {
        self.union(rhs).cloned()
    }
}

/// Set Union
impl<T: Ord + Clone> core::ops::Add<&SegmentSet<T>> for &SegmentSet<T> {
    type Output = SegmentSet<T>;

    fn add(self, rhs: &SegmentSet<T>) -> SegmentSet<T> {
        self.union(rhs).cloned()
    }
}

// TODO: Set in-place union (AddAssign and BitOrAssign)

pub struct Union<'a, T> {
    iter_a: Iter<'a, T>,
    prev_a: Option<Segment<&'a T>>,

    iter_b: Iter<'a, T>,
    prev_b: Option<Segment<&'a T>>,
}

impl<'a, T: Ord> Iterator for Union<'a, T> {
    type Item = Segment<&'a T>;
    fn next(&mut self) -> Option<Self::Item> {
        let next_a = self
            .prev_a
            .take()
            .or_else(|| self.iter_a.next().map(|x| x.as_ref()));

        let next_b = self
            .prev_b
            .take()
            .or_else(|| self.iter_b.next().map(|x| x.as_ref()));

        // If one ran out, use the other
        let next_a = match next_a {
            Some(a) => a,
            None => return next_b,
        };
        let next_b = match next_b {
            Some(b) => b,
            None => return Some(next_a),
        };

        // If `a` is fully before `b` return it and hold on to `b`
        if next_a.end.cmp_start(&next_b.start).is_gt() {
            self.prev_b.insert(next_b);
            return Some(next_a);
        }

        // Likewise the other way around
        if next_a.start.cmp_end(&next_b.end).is_gt() {
            self.prev_a.insert(next_a);
            return Some(next_b);
        }

        // Otherwise, `a` must overlap `b`. Store the outer bounds
        let mut outer = Segment {
            start: core::cmp::min(next_a.start, next_b.start),
            end: core::cmp::max(next_a.end, next_b.end),
        };

        // Increase the outer end bound until we have no overlap
        loop {
            // Get the next end_bound for a touching `a` (holding on to it if
            // not touching)
            let next_a_end = if let Some(r) = self
                .prev_a
                .take()
                .or_else(|| self.iter_a.next().map(|r| r.as_ref()))
            {
                if outer.touches(&r) {
                    Some(r.end)
                } else {
                    self.prev_a.insert(r);
                    None
                }
            } else {
                None
            };

            // likewise for `b`
            let next_b_end = if let Some(r) = self
                .prev_b
                .take()
                .or_else(|| self.iter_b.next().map(|r| r.as_ref()))
            {
                if outer.touches(&r) {
                    Some(r.end)
                } else {
                    self.prev_b.insert(r);
                    None
                }
            } else {
                None
            };

            match (next_a_end, next_b_end) {
                // If no extensions to make, return
                (None, None) => return Some(outer),

                // If we only have one, apply it and loop
                (Some(end), None) | (None, Some(end)) => outer.end = end,

                // If we have both, use the greater (and loop)
                (Some(a), Some(b)) => outer.end = core::cmp::max(a, b),
            }
        }
    }
}
