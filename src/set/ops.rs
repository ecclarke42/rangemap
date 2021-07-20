//! Common Set operations for RangeSet

use super::RangeSet;

// TODO
// pub fn is_disjoint(&self, other: &Self) -> bool {
//     // self.intersection(other).next().is_none()
//     todo!()
//     // TODO
// }

// pub fn is_subset(&self, other: &Self) -> bool {
//     todo!()
//     // TODO
// }

// pub fn is_superset(&self, other: &Self) -> bool {
//     other.is_subset(self)
// }

// pub fn complement(&self) -> Self
// where
//     T: Clone + Ord,
// {
//     // TODO
//     todo!()
//     // // RangeMap::iter_complement MUST return disjoint ranges,
//     // // so we know we can just insert them without extra checking
//     // self.map
//     //     .iter_complement()
//     //     .fold(Self::new(), |mut set, range| {
//     //         set.map.map.insert(Key(range), ());
//     //         set
//     //     })
// }

// TODO: subset(&self, range: R) -> Self; slightly faster than ranges(..).filter().collect() because it doesn't need to check insertions

/// Set Difference
impl<T: Ord + Clone> core::ops::Sub<&RangeSet<T>> for &RangeSet<T> {
    type Output = RangeSet<T>;

    /// Returns the difference of `self` and `rhs` as a new `BTreeSet<T>`.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::BTreeSet;
    ///
    /// let a: BTreeSet<_> = vec![1, 2, 3].into_iter().collect();
    /// let b: BTreeSet<_> = vec![3, 4, 5].into_iter().collect();
    ///
    /// let result = &a - &b;
    /// let result_vec: Vec<_> = result.into_iter().collect();
    /// assert_eq!(result_vec, [1, 2]);
    /// ```
    fn sub(self, rhs: &RangeSet<T>) -> RangeSet<T> {
        self.difference(rhs).cloned().collect()
    }
}
impl<T: Ord + Clone> core::ops::SubAssign<&RangeSet<T>> for RangeSet<T> {
    // TODO: docs
    // TODO: more efficient in place operation?
    fn sub_assign(&mut self, rhs: &RangeSet<T>) {
        for range in rhs.iter() {
            self.clear_range(range)
        }
    }
}
impl<T: Ord + Clone> core::ops::SubAssign<RangeSet<T>> for RangeSet<T> {
    // TODO: docs
    // TODO: more efficient in place operation?
    fn sub_assign(&mut self, rhs: &RangeSet<T>) {
        for range in rhs.iter() {
            self.clear_range(range)
        }
    }
}

/// Set Symmetric Difference
impl<T: Ord + Clone> core::ops::BitXor<&RangeSet<T>> for &RangeSet<T> {
    type Output = RangeSet<T>;

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
    fn bitxor(self, rhs: &RangeSet<T>) -> RangeSet<T> {
        self.symmetric_difference(rhs).cloned().collect()
    }
}

/// Set Intersection A & B
impl<T: Ord + Clone> core::ops::BitAnd<&RangeSet<T>> for &RangeSet<T> {
    type Output = RangeSet<T>;

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
    fn bitand(self, rhs: &RangeSet<T>) -> RangeSet<T> {
        self.intersection(rhs).cloned().collect()
    }
}

/// Set Union
impl<T: Ord + Clone> core::ops::BitOr<&RangeSet<T>> for &RangeSet<T> {
    type Output = RangeSet<T>;

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
    fn bitor(self, rhs: &RangeSet<T>) -> RangeSet<T> {
        self.union(rhs).cloned().collect()
    }
}

/// Set Union
impl<T: Ord + Clone> core::ops::Add<&RangeSet<T>> for &RangeSet<T> {
    type Output = RangeSet<T>;

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
    /// let result = &a + &b;
    /// let result_vec: Vec<_> = result.into_iter().collect();
    /// assert_eq!(result_vec, [1, 2, 3, 4, 5]);
    /// ```
    fn add(self, rhs: &RangeSet<T>) -> RangeSet<T> {
        self.union(rhs).cloned().collect()
    }
}

/// Set Complement
impl<T: Ord + Clone> core::ops::Not for &RangeSet<T> {
    type Output = RangeSet<T>;

    // TODO: docs
    fn not(self) -> Self::Output {
        self.complement()
    }
}
