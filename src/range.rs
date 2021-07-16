pub use core::ops::Bound::{self, *};
use core::{
    cmp::Ordering::{self, *},
    fmt::Debug,
};

// TODO: how to express an empty range? just Option<Range<T>>?
/// Monotonically increasing segment, for use in [`RangeMap`]
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
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
    pub fn value_mut(&mut self) -> Option<&mut T> {
        match &mut self.0 {
            Included(x) | Excluded(x) => Some(x),
            Unbounded => None,
        }
    }
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
            (Unbounded, _) => Less,

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
    // pub fn gt_end(&self)
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
    pub fn as_bound(&self) -> &Bound<T> {
        &self.0
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
    pub fn value_mut(&mut self) -> Option<&mut T> {
        match &mut self.0 {
            Included(x) | Excluded(x) => Some(x),
            Unbounded => None,
        }
    }
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
        // TODO: use start.cmp_end(end)?
        let start = bound_cloned(r.start_bound());
        let end = bound_cloned(r.end_bound());
        match (&start, &end) {
            (Included(s), Included(e))
            | (Included(s), Excluded(e))
            | (Excluded(s), Included(e))
            | (Excluded(s), Excluded(e)) => match s.cmp(e) {
                // If equal, coerce to included
                Ordering::Equal => Self {
                    start: StartBound(Included(s.clone())),
                    end: EndBound(Included(e.clone())),
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
            start: StartBound(Unbounded),
            end: EndBound(Unbounded),
        }
    }

    /// Constructs a new point segement at the given value
    pub fn point(value: T) -> Self
    where
        T: Clone,
    {
        Self {
            start: StartBound(Included(value.clone())),
            end: EndBound(Included(value)),
        }
    }

    /// Shift the entire range by some value
    pub fn shift(&mut self, by: T)
    where
        T: Clone + core::ops::AddAssign,
    {
        if let Some(value) = self.start.value_mut() {
            *value += by.clone();
        }
        if let Some(value) = self.end.value_mut() {
            *value += by;
        }
    }

    /// Shift the entire range to the left by some value (useful if using an
    /// unsigned type, where [`shift`] isn't useful)
    pub fn shift_left(&mut self, by: T)
    where
        T: Clone + core::ops::SubAssign,
    {
        if let Some(value) = self.start.value_mut() {
            *value -= by.clone();
        }
        if let Some(value) = self.end.value_mut() {
            *value -= by;
        }
    }

    pub fn shift_right(&mut self, by: T)
    where
        T: Clone + core::ops::AddAssign,
    {
        if let Some(value) = self.start.value_mut() {
            *value += by.clone();
        }
        if let Some(value) = self.end.value_mut() {
            *value += by;
        }
    }

    /// Adjust the start of a range to a new lower bound.
    pub fn adjust_left(&mut self, _new_start: Bound<T>) -> Self {
        // TODO: panic if empty range
        todo!()
    }

    /// Adjust the end of a range to a new upper bound.
    pub fn adjust_right(&mut self, _new_end: Bound<T>) -> Self {
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
        self.start.before()
    }
    pub(crate) fn bound_after(&self) -> Option<StartBound<&T>> {
        self.end.after()
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
            (Unbounded, _) | (_, Unbounded) => true,

            // If both are included, check less than or equal (overlap on same value)
            (Included(a), Included(b)) => a <= b,

            // Otherwise, no overlap on equal
            (Included(a) | Excluded(a), Included(b) | Excluded(b)) => a < b,
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
            (Unbounded, _) | (_, Unbounded) => true,

            // If both are excluded, ranges don't touch on equal
            // (there is a point gap when a==b)
            (Excluded(a), Excluded(b)) => a < b,

            // If either are included, check less than or equal
            // (overlap on both included, touching when only one is)
            (Included(a) | Excluded(a), Included(b) | Excluded(b)) => a <= b,
        }
    }

    pub fn up_to_and_including_start(&self) -> core::ops::RangeTo<Bound<&T>> {
        ..self.start.as_bound_inner_ref()
    }
}

// Utility, since it's messy to match everwhere
fn bound_cloned<T: Clone>(b: Bound<&T>) -> Bound<T> {
    match b {
        Unbounded => Unbounded,
        Included(x) => Included(x.clone()),
        Excluded(x) => Excluded(x.clone()),
    }
}
fn bound_value<T>(b: Bound<T>) -> Option<T> {
    match b {
        Unbounded => None,
        Included(t) => Some(t),
        Excluded(t) => Some(t),
    }
}
