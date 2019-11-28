
use crate::unicode_tables::general_category::BY_NAME;
use std::cmp::Ordering;
pub fn validate_name_value(name: &str, value: &str) -> bool {
    let list = if let Ok(idx) = BY_NAME.binary_search_by(|(n, _)| {
        name.cmp(n)
    }) {
        &BY_NAME[idx].1
    } else {
        return false;
    };
    value.chars().all(|c| {
        list.binary_search_by(|(lhs, rhs)| {
            let first = lhs.cmp(&c);
            match first {
                Ordering::Equal
                | Ordering::Less => return first,
                _ => (),
            }
            rhs.cmp(&c)
        }).is_ok()
    })
}