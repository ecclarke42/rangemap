pub use core::ops::Bound::{self, *};
use core::{
    cmp::Ordering::{self, *},
    fmt::Debug,
};

/// Directional bound for the start of a monotonically increasing segement
#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub(crate) struct Start<T>(pub(crate) Bound<T>);
impl<T: Debug> Debug for Start<T> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        self.0.fmt(f)
    }
}

impl<T> Start<T> {
    pub fn as_ref(&self) -> Start<&T> {
        Start(self.as_bound_inner_ref())
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
    pub fn before(&self) -> Option<End<&T>> {
        match &self.0 {
            Excluded(t) => Some(End(Included(t))),
            Included(t) => Some(End(Excluded(t))),
            Unbounded => None,
        }
    }

    /// PartialOrd workaround
    pub fn cmp_end(&self, end: &End<T>) -> Ordering
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

impl<'a, T> Start<&'a T> {
    // pub fn into_before(self) -> Option<End<&'a T>> {
    //     match self.0 {
    //         Excluded(t) => Some(End(Included(t))),
    //         Included(t) => Some(End(Excluded(t))),
    //         Unbounded => None,
    //     }
    // }
    pub fn borrow_before(&self) -> Option<End<&'a T>> {
        match self.0 {
            Excluded(t) => Some(End(Included(t))),
            Included(t) => Some(End(Excluded(t))),
            Unbounded => None,
        }
    }
}

impl<T: Clone> Start<&T> {
    pub fn cloned(&self) -> Start<T> {
        Start(match self.0 {
            Excluded(t) => Excluded(t.clone()),
            Included(t) => Included(t.clone()),
            Unbounded => Unbounded,
        })
    }
}

impl<T: Ord> PartialOrd for Start<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<T: Ord> Ord for Start<T> {
    fn cmp(&self, other: &Start<T>) -> Ordering {
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
pub(crate) struct End<T>(pub(crate) Bound<T>);
impl<T: Debug> Debug for End<T> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        self.0.fmt(f)
    }
}
impl<T> From<End<T>> for Bound<T> {
    fn from(b: End<T>) -> Self {
        b.0
    }
}
impl<T> End<T> {
    pub fn as_ref(&self) -> End<&T> {
        End(self.as_bound_inner_ref())
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
    pub fn after(&self) -> Option<Start<&T>> {
        match &self.0 {
            Excluded(t) => Some(Start(Included(t))),
            Included(t) => Some(Start(Excluded(t))),
            Unbounded => None,
        }
    }
    /// PartialOrd workaround
    pub fn cmp_start(&self, start: &Start<T>) -> Ordering
    where
        T: Ord,
    {
        start.cmp_end(self).reverse()
    }
}

impl<'a, T> End<&'a T> {
    // pub fn into_after(self) -> Option<Start<&'a T>> {
    //     match self.0 {
    //         Excluded(t) => Some(Start(Included(t))),
    //         Included(t) => Some(Start(Excluded(t))),
    //         Unbounded => None,
    //     }
    // }
    pub fn borrow_after(&self) -> Option<Start<&'a T>> {
        match self.0 {
            Excluded(t) => Some(Start(Included(t))),
            Included(t) => Some(Start(Excluded(t))),
            Unbounded => None,
        }
    }
}

impl<T: Clone> End<&T> {
    pub fn cloned(&self) -> End<T> {
        End(match self.0 {
            Excluded(t) => Excluded(t.clone()),
            Included(t) => Included(t.clone()),
            Unbounded => Unbounded,
        })
    }
}

impl<T: Ord> PartialOrd for End<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl<T: Ord> Ord for End<T> {
    fn cmp(&self, other: &End<T>) -> Ordering {
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
