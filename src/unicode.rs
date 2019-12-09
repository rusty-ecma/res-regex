
use crate::unicode_tables::{
    general_category::GC,
    script_values::SCRIPT,
    binary_props::BINARY,
};

pub fn validate_name_or_value(name: &str) -> bool {
    if let Ok(_) = GC.binary_search(&name) {
        true
    } else if let Ok(_) = BINARY.binary_search(&name) {
        true
    } else {
        false
    }
}

pub fn validate_name_and_value(name: &str, value: &str) -> bool {
    if let Some(set) = validate_name(name) {
        set.binary_search(&value).is_ok()
    } else {
        false
    }
}


pub fn validate_name(name: &str) -> Option<&[&str]> {
    if name == "General_Category" || name == "gc" {
        Some(GC)
    } else if name == "Script" || name == "sc" || name == "Script_Extensions" || "scx" {
        Some(SCRIPT)
    } else {
        None
    }
}

pub fn validate_value(value: &str) -> bool {
    SCRIPT.binary_search(&value).is_ok()
}