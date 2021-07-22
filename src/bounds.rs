pub use core::ops::Bound::{self, *};
use core::{
    cmp::Ordering::{self, *},
    fmt::Debug,
};

/// Directional bound for the start of a monotonically increasing segement
#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub(crate) struct StartBound<T>(pub(crate) Bound<T>);
impl<T: Debug> Debug for StartBound<T> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        self.0.fmt(f)
    }
}

impl<T> StartBound<T> {
    pub fn as_ref(&self) -> StartBound<&T> {
        StartBound(self.as_bound_inner_ref())
    }

    pub fn as_bound_inner_ref(&self) -> Bound<&T> {
        match &self.0 {
            Included(x) => Included(x),
            Excluded(x) => Excluded(x),
            Unbounded => Unbounded,
        }
    }

    pub fn value(&self) -> Option<&T> {
        match &self.0 {
            Included(x) | Excluded(x) => Some(x),
            Unbounded => None,
        }
    }
    // pub fn value_mut(&mut self) -> Option<&mut T> {
    //     match &mut self.0 {
    //         Included(x) | Excluded(x) => Some(x),
    //         Unbounded => None,
    //     }
    // }
    pub fn before(&self) -> Option<EndBound<&T>> {
        match &self.0 {
            Excluded(t) => Some(EndBound(Included(t))),
            Included(t) => Some(EndBound(Excluded(t))),
            Unbounded => None,
        }
    }

    /// PartialOrd workaround
    pub fn cmp_end(&self, end: &EndBound<T>) -> Ordering
    where
        T: Ord,
    {
        match (&self.0, &end.0) {
            (Unbounded, _) | (_, Unbounded) => Less,

            // When a start and end are on the same point and both included,
            // they CAN be equal
            (Included(a), Included(b)) => a.cmp(b),

            // When one or both are excluded at the same point, start will
            // be greater, due to increasing ranges (i.e. it's pointing right)
            (Included(a) | Excluded(a), Included(b) | Excluded(b)) => match a.cmp(b) {
                Equal => Greater,
                other => other,
            },
        }
    }
}

impl<'a, T> StartBound<&'a T> {
    // pub fn into_before(self) -> Option<EndBound<&'a T>> {
    //     match self.0 {
    //         Excluded(t) => Some(EndBound(Included(t))),
    //         Included(t) => Some(EndBound(Excluded(t))),
    //         Unbounded => None,
    //     }
    // }
    pub fn borrow_before(&self) -> Option<EndBound<&'a T>> {
        match self.0 {
            Excluded(t) => Some(EndBound(Included(t))),
            Included(t) => Some(EndBound(Excluded(t))),
            Unbounded => None,
        }
    }
}

impl<T: Clone> StartBound<&T> {
    pub fn cloned(&self) -> StartBound<T> {
        StartBound(match self.0 {
            Excluded(t) => Excluded(t.clone()),
            Included(t) => Included(t.clone()),
            Unbounded => Unbounded,
        })
    }
}

impl<T: Ord> PartialOrd for StartBound<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<T: Ord> Ord for StartBound<T> {
    fn cmp(&self, other: &StartBound<T>) -> Ordering {
        match (&self.0, &other.0) {
            // Unbounded is always less than bounded, for purposes of
            // comparison, Unbounded is equal to itself
            (Unbounded, Unbounded) => Ordering::Equal,
            (Unbounded, _) => Ordering::Less,
            (_, Unbounded) => Ordering::Greater,

            // Same bound type, just compare values
            (Included(a), Included(b)) | (Excluded(a), Excluded(b)) => a.cmp(b),

            // Different bound types
            // For the start of a strictly increasing range, included is less
            // than excluded (at the same value)
            (Included(a), Excluded(b)) => match a.cmp(b) {
                Ordering::Equal => Ordering::Less,
                not_equal => not_equal,
            },
            (Excluded(a), Included(b)) => match a.cmp(b) {
                Ordering::Equal => Ordering::Greater,
                not_equal => not_equal,
            },
        }
    }
}

/// Directional bound for the end of a monotonically increasing segement
#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub(crate) struct EndBound<T>(pub(crate) Bound<T>);
impl<T: Debug> Debug for EndBound<T> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        self.0.fmt(f)
    }
}
impl<T> From<EndBound<T>> for Bound<T> {
    fn from(b: EndBound<T>) -> Self {
        b.0
    }
}
impl<T> EndBound<T> {
    pub fn as_ref(&self) -> EndBound<&T> {
        EndBound(self.as_bound_inner_ref())
    }
    pub fn as_bound_inner_ref(&self) -> Bound<&T> {
        match &self.0 {
            Included(x) => Included(x),
            Excluded(x) => Excluded(x),
            Unbounded => Unbounded,
        }
    }
    pub fn value(&self) -> Option<&T> {
        match &self.0 {
            Included(x) | Excluded(x) => Some(x),
            Unbounded => None,
        }
    }
    // pub fn value_mut(&mut self) -> Option<&mut T> {
    //     match &mut self.0 {
    //         Included(x) | Excluded(x) => Some(x),
    //         Unbounded => None,
    //     }
    // }
    pub fn after(&self) -> Option<StartBound<&T>> {
        match &self.0 {
            Excluded(t) => Some(StartBound(Included(t))),
            Included(t) => Some(StartBound(Excluded(t))),
            Unbounded => None,
        }
    }
    /// PartialOrd workaround
    pub fn cmp_start(&self, start: &StartBound<T>) -> Ordering
    where
        T: Ord,
    {
        start.cmp_end(self).reverse()
    }
}

impl<'a, T> EndBound<&'a T> {
    // pub fn into_after(self) -> Option<StartBound<&'a T>> {
    //     match self.0 {
    //         Excluded(t) => Some(StartBound(Included(t))),
    //         Included(t) => Some(StartBound(Excluded(t))),
    //         Unbounded => None,
    //     }
    // }
    pub fn borrow_after(&self) -> Option<StartBound<&'a T>> {
        match self.0 {
            Excluded(t) => Some(StartBound(Included(t))),
            Included(t) => Some(StartBound(Excluded(t))),
            Unbounded => None,
        }
    }
}

impl<T: Clone> EndBound<&T> {
    pub fn cloned(&self) -> EndBound<T> {
        EndBound(match self.0 {
            Excluded(t) => Excluded(t.clone()),
            Included(t) => Included(t.clone()),
            Unbounded => Unbounded,
        })
    }
}

impl<T: Ord> PartialOrd for EndBound<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl<T: Ord> Ord for EndBound<T> {
    fn cmp(&self, other: &EndBound<T>) -> Ordering {
        match (&self.0, &other.0) {
            // Unbounded is always greater than bounded, for purposes of
            // comparison, Unbounded is equal to itself
            (Unbounded, Unbounded) => Ordering::Equal,
            (Unbounded, _) => Ordering::Greater,
            (_, Unbounded) => Ordering::Less,

            // Same bound type, just compare values
            (Included(a), Included(b)) | (Excluded(a), Excluded(b)) => a.cmp(b),

            // Different bound types
            // For the end of a strictly increasing range, included is greater
            // than excluded (at the same value)
            (Included(a), Excluded(b)) => match a.cmp(b) {
                Ordering::Equal => Ordering::Greater,
                not_equal => not_equal,
            },
            (Excluded(a), Included(b)) => match a.cmp(b) {
                Ordering::Equal => Ordering::Less,
                not_equal => not_equal,
            },
        }
    }
}
