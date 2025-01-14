use chrono::offset::TimeZone;
use chrono::{Duration, Utc};
use segmap::SegmentMap;

fn main() {
    let people = ["Alice", "Bob", "Carol"];
    let mut roster = SegmentMap::new();

    // Set up initial roster.
    let start_of_roster = Utc.ymd(2019, 1, 7);
    let mut week_start = start_of_roster;
    for _ in 0..3 {
        for person in people {
            let next_week = week_start + Duration::weeks(1);
            roster.insert(week_start..next_week, person);
            week_start = next_week;
        }
    }

    // Bob is covering Alice's second shift (the fourth shift overall).
    let fourth_shift_start = start_of_roster + Duration::weeks(3);
    let fourth_shift_end = fourth_shift_start + Duration::weeks(1);
    roster.insert(fourth_shift_start..fourth_shift_end, "Bob");

    // Print out the roster, and observe that
    // the fourth and fifth shifts have been coalesced
    // into one range.
    for (range, &person) in roster.iter() {
        let start = *range.start_value().unwrap();
        let duration = *range.end_value().unwrap() - start;
        println!("{} ({}): {}", start, duration, person);
    }
}
