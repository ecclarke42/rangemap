pub use core::ops::Bound;
use core::{
    cmp::{max, min, Ordering},
    fmt::Debug,
};

// TODO: how to express an empty range? just Option<Range<T>>?
/// Monotonically increasing segment, for use in [`RangeMap`]
#[derive(Debug, Clone, Copy)]
pub struct Range<T> {
    pub(crate) start: StartBound<T>,
    pub(crate) end: EndBound<T>,
}

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
    pub fn as_bound(&self) -> &Bound<T> {
        &self.0
    }
    pub fn as_bound_inner_ref(&self) -> Bound<&T> {
        match &self.0 {
            Bound::Included(x) => Bound::Included(x),
            Bound::Excluded(x) => Bound::Excluded(x),
            Bound::Unbounded => Bound::Unbounded,
        }
    }
    pub fn value(&self) -> Option<&T> {
        match &self.0 {
            Bound::Included(x) | Bound::Excluded(x) => Some(x),
            Bound::Unbounded => None,
        }
    }
    pub fn value_mut(&mut self) -> Option<&mut T> {
        match &mut self.0 {
            Bound::Included(x) | Bound::Excluded(x) => Some(x),
            Bound::Unbounded => None,
        }
    }
}

impl<T: Clone> StartBound<&T> {
    pub fn cloned(&self) -> StartBound<T> {
        StartBound(match self.0 {
            Bound::Excluded(t) => Bound::Excluded(t.clone()),
            Bound::Included(t) => Bound::Included(t.clone()),
            Bound::Unbounded => Bound::Unbounded,
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
            (Bound::Unbounded, Bound::Unbounded) => Ordering::Equal,
            (Bound::Unbounded, _) => Ordering::Less,
            (_, Bound::Unbounded) => Ordering::Greater,

            // Same bound type, just compare values
            (Bound::Included(a), Bound::Included(b)) | (Bound::Excluded(a), Bound::Excluded(b)) => {
                a.cmp(b)
            }

            // Different bound types
            // For the start of a strictly increasing range, included is less
            // than excluded (at the same value)
            (Bound::Included(a), Bound::Excluded(b)) => match a.cmp(b) {
                Ordering::Equal => Ordering::Less,
                not_equal => not_equal,
            },
            (Bound::Excluded(a), Bound::Included(b)) => match a.cmp(b) {
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
    pub fn as_bound(&self) -> &Bound<T> {
        &self.0
    }
    pub fn as_bound_inner_ref(&self) -> Bound<&T> {
        match &self.0 {
            Bound::Included(x) => Bound::Included(x),
            Bound::Excluded(x) => Bound::Excluded(x),
            Bound::Unbounded => Bound::Unbounded,
        }
    }
    pub fn value(&self) -> Option<&T> {
        match &self.0 {
            Bound::Included(x) | Bound::Excluded(x) => Some(x),
            Bound::Unbounded => None,
        }
    }
    pub fn value_mut(&mut self) -> Option<&mut T> {
        match &mut self.0 {
            Bound::Included(x) | Bound::Excluded(x) => Some(x),
            Bound::Unbounded => None,
        }
    }
}

impl<T: Clone> EndBound<&T> {
    pub fn cloned(&self) -> EndBound<T> {
        EndBound(match self.0 {
            Bound::Excluded(t) => Bound::Excluded(t.clone()),
            Bound::Included(t) => Bound::Included(t.clone()),
            Bound::Unbounded => Bound::Unbounded,
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
            (Bound::Unbounded, Bound::Unbounded) => Ordering::Equal,
            (Bound::Unbounded, _) => Ordering::Greater,
            (_, Bound::Unbounded) => Ordering::Less,

            // Same bound type, just compare values
            (Bound::Included(a), Bound::Included(b)) | (Bound::Excluded(a), Bound::Excluded(b)) => {
                a.cmp(b)
            }

            // Different bound types
            // For the end of a strictly increasing range, included is greater
            // than excluded (at the same value)
            (Bound::Included(a), Bound::Excluded(b)) => match a.cmp(b) {
                Ordering::Equal => Ordering::Greater,
                not_equal => not_equal,
            },
            (Bound::Excluded(a), Bound::Included(b)) => match a.cmp(b) {
                Ordering::Equal => Ordering::Less,
                not_equal => not_equal,
            },
        }
    }
}

impl<T> core::ops::RangeBounds<T> for Range<T> {
    fn start_bound(&self) -> Bound<&T> {
        self.start.as_bound_inner_ref()
    }
    fn end_bound(&self) -> Bound<&T> {
        self.end.as_bound_inner_ref()
    }
}

// TODO: Range From's

impl<T> Range<T> {
    /// Construct a new segment from range bounds
    ///
    /// If the range given is backwards (decreasing), it will be reversed
    ///
    /// ## Weird Ranges
    ///
    /// Oddly constructed ranges will be coerced to "sane" structure:
    /// - Point ranges (where both start and end are bounded to the same value
    /// and at least one is included) will be coerced to (Included, Included)
    ///
    /// ## Panics
    ///
    /// Only one range is too strange to be included here. If you pass a point
    /// range with both the start and end excluded, this will panic, as the
    /// range is impossible to evaluate
    pub fn new<R: core::ops::RangeBounds<T>>(r: R) -> Self
    where
        T: Ord + Clone,
    {
        let start = bound_cloned(r.start_bound());
        let end = bound_cloned(r.end_bound());
        match (&start, &end) {
            (Bound::Included(s), Bound::Included(e))
            | (Bound::Included(s), Bound::Excluded(e))
            | (Bound::Excluded(s), Bound::Included(e))
            | (Bound::Excluded(s), Bound::Excluded(e)) => match s.cmp(e) {
                // If equal, coerce to included
                Ordering::Equal => Self {
                    start: StartBound(Bound::Included(s.clone())),
                    end: EndBound(Bound::Included(e.clone())),
                },
                Ordering::Less => Self {
                    start: StartBound(start),
                    end: EndBound(end),
                },
                // If backwards, flip bounds
                Ordering::Greater => Self {
                    start: StartBound(end),
                    end: EndBound(start),
                },
            },

            // Otherwise, only one side has a value
            _ => Self {
                start: StartBound(start),
                end: EndBound(end),
            },
        }
    }

    // TODO: names

    // TODO: docs
    pub fn full() -> Self {
        Self {
            start: StartBound(Bound::Unbounded),
            end: EndBound(Bound::Unbounded),
        }
    }

    /// Constructs a new point segement at the given value
    pub fn point(value: T) -> Self
    where
        T: Clone,
    {
        Self {
            start: StartBound(Bound::Included(value.clone())),
            end: EndBound(Bound::Included(value)),
        }
    }

    /// Shift the entire range by some value
    pub fn shift(&mut self, by: T)
    where
        T: Clone + core::ops::AddAssign,
    {
        self.start.value_mut().map(|s| *s += by.clone());
        self.end.value_mut().map(|e| *e += by);
    }

    /// Shift the entire range to the left by some value (useful if using an
    /// unsigned type, where [`shift`] isn't useful)
    pub fn shift_left(&mut self, by: T)
    where
        T: Clone + core::ops::SubAssign,
    {
        self.start.value_mut().map(|s| *s -= by.clone());
        self.end.value_mut().map(|e| *e -= by);
    }

    pub fn shift_right(&mut self, by: T)
    where
        T: Clone + core::ops::AddAssign,
    {
        self.start.value_mut().map(|s| *s += by.clone());
        self.end.value_mut().map(|e| *e += by);
    }

    /// Adjust the start of a range to a new lower bound.
    pub fn adjust_left(&mut self, new_start: Bound<T>) -> Self {
        todo!()
    }

    /// Adjust the end of a range to a new upper bound.
    pub fn adjust_right(&mut self, new_end: Bound<T>) -> Self {
        todo!()
    }

    /// Converts from `&Range<T>` to `Range<&T>`.
    #[inline]
    pub fn as_ref(&self) -> Range<&T> {
        Range {
            start: self.start.as_ref(),
            end: self.end.as_ref(),
        }
    }

    pub(crate) fn bound_before(&self) -> Option<EndBound<&T>> {
        match &self.start.0 {
            Bound::Excluded(t) => Some(EndBound(Bound::Included(t))),
            Bound::Included(t) => Some(EndBound(Bound::Excluded(t))),
            Bound::Unbounded => None,
        }
    }
    pub(crate) fn bound_after(&self) -> Option<StartBound<&T>> {
        match &self.end.0 {
            Bound::Excluded(t) => Some(StartBound(Bound::Included(t))),
            Bound::Included(t) => Some(StartBound(Bound::Excluded(t))),
            Bound::Unbounded => None,
        }
    }

    /// Check whether the range overlaps with another (i.e. the intersection is
    /// not null)
    pub fn overlaps(&self, other: &Self) -> bool
    where
        T: Ord,
    {
        match (
            core::cmp::max(self.start.as_ref(), other.start.as_ref()).0,
            core::cmp::min(self.end.as_ref(), other.end.as_ref()).0,
        ) {
            // Both starts and/or both ends are unbounded, must overlap
            (Bound::Unbounded, _) | (_, Bound::Unbounded) => true,

            // If both are included, check less than or equal (overlap on same value)
            (Bound::Included(a), Bound::Included(b)) => a <= b,

            // Otherwise, no overlap on equal
            (Bound::Included(a) | Bound::Excluded(a), Bound::Included(b) | Bound::Excluded(b)) => {
                a < b
            }
        }
    }

    /// Check whether the range touches another, either overlapping it or coming
    /// directly up to it (with no gap)
    ///
    /// See also: [`Range::overlaps`]
    pub fn touches(&self, other: &Self) -> bool
    where
        T: Ord,
    {
        match (
            core::cmp::max(self.start.as_ref(), other.start.as_ref()).0,
            core::cmp::min(self.end.as_ref(), other.end.as_ref()).0,
        ) {
            // Both starts and/or both ends are unbounded, must overlap
            (Bound::Unbounded, _) | (_, Bound::Unbounded) => true,

            // If both are excluded, ranges don't touch on equal
            // (there is a point gap when a==b)
            (Bound::Excluded(a), Bound::Excluded(b)) => a < b,

            // If either are included, check less than or equal
            // (overlap on both included, touching when only one is)
            (Bound::Included(a) | Bound::Excluded(a), Bound::Included(b) | Bound::Excluded(b)) => {
                a <= b
            }
        }
    }

    pub fn up_to_and_including_start(&self) -> core::ops::RangeTo<Bound<&T>> {
        ..self.start.as_bound_inner_ref()
    }
}

// Utility, since it's messy to match everwhere
fn bound_cloned<T: Clone>(b: Bound<&T>) -> Bound<T> {
    match b {
        Bound::Unbounded => Bound::Unbounded,
        Bound::Included(x) => Bound::Included(x.clone()),
        Bound::Excluded(x) => Bound::Excluded(x.clone()),
    }
}
fn bound_value<T>(b: Bound<T>) -> Option<T> {
    match b {
        Bound::Unbounded => None,
        Bound::Included(t) => Some(t),
        Bound::Excluded(t) => Some(t),
    }
}
