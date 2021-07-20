use crate::bounds::{EndBound, StartBound};
use core::{
    cmp::Ordering,
    fmt::Debug,
    ops::Bound::{self, *},
};

/// Monotonically increasing segment, for use as a concrete range type in
/// [`RangeMap`].
#[derive(Clone, Copy, Hash, Eq)]
pub struct Range<T> {
    pub(crate) start: StartBound<T>,
    pub(crate) end: EndBound<T>,
}

impl<T> core::ops::RangeBounds<T> for Range<T> {
    fn start_bound(&self) -> Bound<&T> {
        self.start.as_bound_inner_ref()
    }
    fn end_bound(&self) -> Bound<&T> {
        self.end.as_bound_inner_ref()
    }
}

impl<T> core::ops::RangeBounds<T> for &Range<T> {
    fn start_bound(&self) -> Bound<&T> {
        self.start.as_bound_inner_ref()
    }
    fn end_bound(&self) -> Bound<&T> {
        self.end.as_bound_inner_ref()
    }
}

impl<T: Debug> Debug for Range<T> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match &self.start.0 {
            Unbounded => write!(f, "(-∞, ")?,
            Excluded(t) => write!(f, "({:?}, ", t)?,
            Included(t) => write!(f, "[{:?}, ", t)?,
        }
        match &self.end.0 {
            Unbounded => write!(f, "∞)"),
            Excluded(t) => write!(f, "{:?})", t),
            Included(t) => write!(f, "{:?}]", t),
        }
    }
}

impl<T> Range<T> {
    /// Construct a new segment from range bounds
    ///
    /// If the range given is backwards (decreasing), it will be reversed
    ///
    /// # Weird Ranges
    ///
    /// Oddly constructed ranges will be coerced to "sane" structure:
    /// - Point ranges (where both start and end are bounded to the same value
    /// and at least one is included) will be coerced to (Included, Included)
    ///
    /// # Panics
    ///
    /// Only one range is too strange to be included here. If you pass a point
    /// range with both the start and end excluded, this will panic, as the
    /// range is impossible to evaluate
    ///
    /// ```
    /// use rangemap::{Range, Bound};
    /// use core::ops::RangeBounds;
    ///
    /// let r = Range::new(Bound::Included(0), Bound::Excluded(5));
    /// assert_eq!(r.start_bound(), Bound::Included(&0));
    /// assert_eq!(r.end_bound(), Bound::Excluded(&5));
    /// ```
    ///
    /// # See Also
    ///
    /// `Range` also implements `From` for all of the `core::ops` range types,
    /// so you may find it more convenient to construct a range like
    /// `Range::from(a..b)`
    ///
    pub fn new(start: Bound<T>, end: Bound<T>) -> Self
    where
        T: Ord,
    {
        match (&start, &end) {
            (Included(s), Included(e))
            | (Included(s), Excluded(e))
            | (Excluded(s), Included(e))
            | (Excluded(s), Excluded(e)) => match s.cmp(e) {
                // If equal, coerce to included
                Ordering::Equal => Self {
                    start: StartBound(Included(bound_value(start).unwrap())),
                    end: EndBound(Included(bound_value(end).unwrap())),
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

    /// Construct a new [`Range`] that spans all possible values
    ///
    /// This is the same as `Range::from(..)`.
    ///
    /// # Examples
    ///
    /// ```
    /// use rangemap::{Range, Bound, RangeBounds};
    ///
    /// let r = Range::<u32>::full();
    /// assert_eq!(r.start_bound(), Bound::Unbounded);
    /// assert_eq!(r.end_bound(), Bound::Unbounded);
    /// ```
    pub fn full() -> Self {
        Self {
            start: StartBound(Unbounded),
            end: EndBound(Unbounded),
        }
    }

    /// Constructs a new point segement at the given value
    ///
    /// # Examples
    ///
    /// ```
    /// use rangemap::{Range, RangeBounds, Bound};
    ///
    /// let r = Range::point(5);
    /// assert_eq!(r.start_bound(), Bound::Included(&5));
    /// assert_eq!(r.end_bound(), Bound::Included(&5));
    /// ```
    pub fn point(value: T) -> Self
    where
        T: Clone,
    {
        Self {
            start: StartBound(Included(value.clone())),
            end: EndBound(Included(value)),
        }
    }

    /// Get the start value of the range (if it is bounded)
    ///
    /// # Examples
    ///
    /// ```
    /// use rangemap::{Range, RangeBounds, Bound};
    ///
    /// let bounded = Range::from(5..10);
    /// assert_eq!(bounded.start_value(), Some(&5));
    ///
    /// let unbounded = Range::from(..10);
    /// assert_eq!(unbounded.start_value(), None);
    /// ```
    pub fn start_value(&self) -> Option<&T> {
        self.start.value()
    }

    /// Get the end value of the range (if it is bounded)
    ///
    /// # Examples
    ///
    /// ```
    /// use rangemap::{Range, RangeBounds, Bound};
    ///
    /// let bounded = Range::from(5..10);
    /// assert_eq!(bounded.end_value(), Some(&10));
    ///
    /// let unbounded = Range::from(5..);
    /// assert_eq!(unbounded.end_value(), None);
    /// ```
    pub fn end_value(&self) -> Option<&T> {
        self.end.value()
    }

    /// Converts from `Range<T>` to `Range<&T>`.
    ///
    /// Many iterators from this crate return a `Range<&T>` instead of a
    /// `&Range<T>` since bounds need to be constructed or adjusted. This allows
    /// us to avoid `clone`ing `T`.
    ///
    #[inline]
    pub fn as_ref(&self) -> Range<&T> {
        Range {
            start: self.start.as_ref(),
            end: self.end.as_ref(),
        }
    }

    /// Check whether the range overlaps with another
    ///
    /// # Examples
    ///
    /// ```
    /// use rangemap::Range;
    ///
    /// // Overlapping Ranges
    /// let a = Range::from(0..10);
    /// let b = Range::from(5..15);
    /// assert!(a.overlaps(&b));
    ///
    /// // Touching Ranges
    /// let a = Range::from(0..10);
    /// let b = Range::from(10..20);
    /// assert!(!a.overlaps(&b));
    ///
    /// // Separate Ranges
    /// let a = Range::from(0..10);
    /// let b = Range::from(15..20);
    /// assert!(!a.overlaps(&b));
    ///
    /// ```
    ///
    /// # See Also
    ///
    /// - [`Range::touches`] for overlapping OR adjacent ranges
    ///
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

    /// Check whether the range touches another, either overlapping it or
    /// directly adjacent to it
    ///
    /// # Examples
    ///
    /// ```
    /// use rangemap::Range;
    ///
    /// // Overlapping Ranges
    /// let a = Range::from(0..10);
    /// let b = Range::from(5..15);
    /// assert!(a.touches(&b));
    ///
    /// // Touching Ranges
    /// let a = Range::from(0..10);
    /// let b = Range::from(10..20);
    /// assert!(a.touches(&b));
    ///
    /// // Separate Ranges
    /// let a = Range::from(0..10);
    /// let b = Range::from(15..20);
    /// assert!(!a.touches(&b));
    ///
    /// ```
    ///
    /// # See Also
    ///
    /// - [`Range::overlaps`] for only overlapping ranges
    ///
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

    // TODO: Range convenience methods

    // /// Shift the entire range by some value
    // pub fn shift(&mut self, by: T)
    // where
    //     T: Clone + core::ops::AddAssign,
    // {
    //     if let Some(value) = self.start.value_mut() {
    //         *value += by.clone();
    //     }
    //     if let Some(value) = self.end.value_mut() {
    //         *value += by;
    //     }
    // }

    // /// Shift the entire range to the left by some value (useful if using an
    // /// unsigned type, where [`shift`] isn't useful)
    // pub fn shift_left(&mut self, by: T)
    // where
    //     T: Clone + core::ops::SubAssign,
    // {
    //     if let Some(value) = self.start.value_mut() {
    //         *value -= by.clone();
    //     }
    //     if let Some(value) = self.end.value_mut() {
    //         *value -= by;
    //     }
    // }

    // pub fn shift_right(&mut self, by: T)
    // where
    //     T: Clone + core::ops::AddAssign,
    // {
    //     if let Some(value) = self.start.value_mut() {
    //         *value += by.clone();
    //     }
    //     if let Some(value) = self.end.value_mut() {
    //         *value += by;
    //     }
    // }

    // /// Adjust the start of a range to a new lower bound.
    // pub fn adjust_left(&mut self, _new_start: Bound<T>) -> Option<Self> {
    //     // panic if empty range, or just none?
    // }

    // /// Adjust the end of a range to a new upper bound.
    // pub fn adjust_right(&mut self, _new_end: Bound<T>) -> Option<Self> {
    // }
}

/// Crate Methods
impl<T> Range<T> {
    /// Obtain the adjacent end bound before the start of this range (if it
    /// exists)
    pub(crate) fn bound_before(&self) -> Option<EndBound<&T>> {
        self.start.before()
    }
    /// Obtain the adjacent start bound after the end of this range (if it
    /// exists)
    pub(crate) fn bound_after(&self) -> Option<StartBound<&T>> {
        self.end.after()
    }
}

impl<'a, T> Range<&'a T> {
    pub(crate) fn borrow_bound_before(&self) -> Option<EndBound<&'a T>> {
        self.start.borrow_before()
    }
    pub(crate) fn borrow_bound_after(&self) -> Option<StartBound<&'a T>> {
        self.end.borrow_after()
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

impl<T: Ord> From<core::ops::Range<T>> for Range<T> {
    fn from(r: core::ops::Range<T>) -> Self {
        Range::new(Included(r.start), Excluded(r.end))
    }
}

impl<T: Ord> From<core::ops::RangeFrom<T>> for Range<T> {
    fn from(r: core::ops::RangeFrom<T>) -> Self {
        Range::new(Included(r.start), Unbounded)
    }
}

impl<T: Ord> From<core::ops::RangeTo<T>> for Range<T> {
    fn from(r: core::ops::RangeTo<T>) -> Self {
        Range::new(Unbounded, Excluded(r.end))
    }
}

impl<T: Ord> From<core::ops::RangeFull> for Range<T> {
    fn from(_: core::ops::RangeFull) -> Self {
        Range::new(Unbounded, Unbounded)
    }
}

impl<T: Ord> From<core::ops::RangeInclusive<T>> for Range<T> {
    fn from(r: core::ops::RangeInclusive<T>) -> Self {
        let (start, end) = r.into_inner();
        Range::new(Included(start), Included(end))
    }
}

impl<T: Ord> From<core::ops::RangeToInclusive<T>> for Range<T> {
    fn from(r: core::ops::RangeToInclusive<T>) -> Self {
        Range::new(Unbounded, Included(r.end))
    }
}

impl<T: Ord + Clone, R: core::ops::RangeBounds<T>> From<&R> for Range<T> {
    fn from(r: &R) -> Self {
        Range::new(bound_cloned(r.start_bound()), bound_cloned(r.end_bound()))
    }
}

impl<T: PartialEq, R: core::ops::RangeBounds<T>> PartialEq<R> for Range<T> {
    fn eq(&self, other: &R) -> bool {
        (match (&self.start.0, other.start_bound()) {
            (Unbounded, Unbounded) => true,
            (Included(t), Included(u)) | (Excluded(t), Excluded(u)) => t.eq(u),
            _ => false,
        }) && (match (&self.end.0, other.end_bound()) {
            (Unbounded, Unbounded) => true,
            (Included(t), Included(u)) | (Excluded(t), Excluded(u)) => t.eq(u),
            _ => false,
        })
    }
}
