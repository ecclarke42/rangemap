
use super::*;
use alloc::{format, vec, vec::Vec};

trait RangeSetExt<T> {
    fn to_vec(&self) -> Vec<Range<T>>;
}

impl<T> RangeSetExt<T> for RangeSet<T>
where
    T: Ord + Clone,
{
    fn to_vec(&self) -> Vec<Range<T>> {
        self.iter().cloned().collect()
    }
}

#[test]
fn empty_set_is_empty() {
    let range_set: RangeSet<u32> = RangeSet::new();
    assert_eq!(range_set.to_vec(), Vec::<Range<u32>>::new());
}

#[test]
fn insert_into_empty_map() {
    let mut range_set: RangeSet<u32> = RangeSet::new();
    range_set.insert(0..50);
    assert_eq!(range_set.to_vec(), vec![0..50]);
}

#[test]
fn remove_partially_overlapping() {
    let mut range_set: RangeSet<u32> = RangeSet::new();
    range_set.insert(0..50);
    range_set.remove(25..75);
    assert_eq!(range_set.to_vec(), vec![0..25]);
}

// TODO: gaps_in
// #[test]
// fn gaps_between_items_floating_inside_outer_range() {
//     let mut range_set: RangeSet<u32> = RangeSet::new();
//     // 0 1 2 3 4 5 6 7 8 9
//     // ◌ ◌ ◌ ◌ ◌ ●-◌ ◌ ◌ ◌
//     range_set.insert(5..6);
//     // 0 1 2 3 4 5 6 7 8 9
//     // ◌ ◌ ◌ ●-◌ ◌ ◌ ◌ ◌ ◌
//     range_set.insert(3..4);
//     // 0 1 2 3 4 5 6 7 8 9
//     // ◌ ◆-------------◇ ◌
//     let outer_range = 1..8;
//     let mut gaps = range_set.gaps_in(&outer_range);
//     // Should yield gaps at start, between items,
//     // and at end.
//     assert_eq!(gaps.next(), Some(1..3));
//     assert_eq!(gaps.next(), Some(4..5));
//     assert_eq!(gaps.next(), Some(6..8));
//     assert_eq!(gaps.next(), None);
//     // Gaps iterator should be fused.
//     assert_eq!(gaps.next(), None);
//     assert_eq!(gaps.next(), None);
// }
///
/// impl Debug
///

#[test]
fn set_debug_repr_looks_right() {
    let mut set: RangeSet<u32> = RangeSet::new();

    // Empty
    assert_eq!(format!("{:?}", set), "{}");

    // One entry
    set.insert(2..5);
    assert_eq!(format!("{:?}", set), "{[2, 5)}");

    // Many entries
    set.insert(7..=8);
    set.insert(10..11);
    assert_eq!(format!("{:?}", set), "{[2, 5), [7, 8], [10, 11)}");
}
