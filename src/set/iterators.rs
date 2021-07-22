use core::{
    fmt,
    iter::{FromIterator, FusedIterator},
    ops::RangeBounds,
};

use crate::{Range, RangeSet};
// TODO: all doctests

// /// See [`alloc::collections::btree_set::ITER_PERFORMANCE_TIPPING_SIZE_DIFF`]
// ///
// /// The std library uses `ITER_PERFORMANCE_TIPPING_SIZE_DIFF` to split whether
// /// to iterate two items in tandem or search through one (the larger) while
// /// iterating through the smaller. Let's trust they know what they're doing,
// /// though that tipping point may be different for ranges that require an extra
// /// step in the search (to find the last range starting BEFORE a point).
// const ITER_PERFORMANCE_TIPPING_SIZE_DIFF: usize = 16;

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

impl<Item: RangeBounds<T>, T: Clone + Ord> FromIterator<Item> for RangeSet<T> {
    fn from_iter<I: IntoIterator<Item = Item>>(iter: I) -> Self {
        let mut set = Self::new();
        for item in iter {
            set.insert(item);
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
            self.insert(range);
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
