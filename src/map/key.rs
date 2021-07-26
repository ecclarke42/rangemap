use crate::segment::{Segment, Start};
use core::{cmp::Ordering, fmt::Debug, ops::Bound};

/// Wrapper type for items the map (range should only ever be increasing)
#[derive(Clone)]
pub(crate) struct Key<T>(pub(crate) Segment<T>);

impl<T: Clone> Key<&T> {
    pub(crate) fn cloned(&self) -> Key<T> {
        Key(self.0.cloned())
    }
}

impl<T: Copy> Copy for Key<T> {}
impl<T: Debug> Debug for Key<T> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

impl<T> core::borrow::Borrow<Start<T>> for Key<T> {
    fn borrow(&self) -> &Start<T> {
        &self.0.start
    }
}
// impl<T> core::borrow::Borrow<Start<&T>> for Key<T> {
//     fn borrow(&self) -> &Start<&T> {
//         &self.0.start.as_ref()
//     }
// }
// impl<'a, T> core::borrow::Borrow<Start<&'a T>> for Key<T> {
//     fn borrow(&self) -> &Start<&'a T> {
//         match &self.0.start.0 {
//             Bound::Included(x) => Bound::Included(x),
//             Bound::Excluded(x) => Bound::Excluded(x),
//             Bound::Unbounded => Bound::Unbounded,
//         }
//     }
// }
impl<T> core::borrow::Borrow<Bound<T>> for Key<T> {
    fn borrow(&self) -> &Bound<T> {
        &self.0.start.0
    }
}

// impl<'a, T> core::borrow::Borrow<Bound<&'a T>> for Key<&'a T> {
//     fn borrow(&self) -> &Bound<&'a T> {
//         // self.0.start.as_bound_inner_ref();

//         &self.0.start.0
//         // match &self.0.start.0 {
//         //     Bound::Included(x) => &Bound::Included(x),
//         //     Bound::Excluded(x) => &Bound::Excluded(x),
//         //     Bound::Unbounded => &Bound::Unbounded,
//         // }
//     }
// }

impl<T: PartialEq> PartialEq for Key<T> {
    fn eq(&self, other: &Key<T>) -> bool {
        self.0.start == other.0.start
    }
}
impl<T: PartialEq> PartialEq<Bound<T>> for Key<T> {
    fn eq(&self, other: &Bound<T>) -> bool {
        self.0.start.0.eq(other)
    }
}
impl<T: PartialEq> PartialEq<T> for Key<T> {
    fn eq(&self, other: &T) -> bool {
        if let Bound::Included(start) = &self.0.start.0 {
            start == other
        } else {
            false
        }
    }
}
impl<T: Eq> Eq for Key<T> {}
impl<T: Ord> Ord for Key<T> {
    fn cmp(&self, other: &Key<T>) -> Ordering {
        self.0.start.cmp(&other.0.start)
    }
}
impl<T> PartialOrd for Key<T>
where
    T: Ord,
{
    fn partial_cmp(&self, other: &Key<T>) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
