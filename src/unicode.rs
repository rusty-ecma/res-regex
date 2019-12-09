use crate::unicode_tables::{binary_props::BINARY, general_category::GC, script_values::SCRIPT};

/// Validate a `LoneUnicodePropertyNameOrValue`
/// is a valid name or value
///
/// ex:
/// ```js
/// let re = /\p{White_Space}\p{Alphabetic}/;
/// ```
///
/// This function will first search the General_Category
/// names and aliases and then the Binary Property
/// names and aliases
pub fn validate_name_or_value(name: &str) -> bool {
    if let Ok(_) = GC.binary_search(&name) {
        true
    } else if let Ok(_) = BINARY.binary_search(&name) {
        true
    } else {
        false
    }
}
/// Validate a `UnicodePropertyName` and `UnicodePropertyValue`
/// are correct
///
///
/// ex:
/// ```js
/// let re = /\p{Script=Greek}\p{gc=Lm}/
/// ```
///
/// valid names include `General_Category`, `gc`, `Script`,
/// `Script_Extensions`, `sc` and `scx`
///  any other names will return false
pub fn validate_name_and_value(name: &str, value: &str) -> bool {
    if let Some(set) = validate_name(name) {
        set.binary_search(&value).is_ok()
    } else {
        false
    }
}

/// Validate a name is `General_Category`, `gc`, `Script`,
/// `Script_Extensions`, `sc` or `scx`. This will return
/// Some with the correct list of possible values
/// None, otherwise
pub fn validate_name(name: &str) -> Option<&[&str]> {
    if name == "General_Category" || name == "gc" {
        Some(GC)
    } else if name == "Script" || name == "sc" || name == "Script_Extensions" || name == "scx" {
        Some(SCRIPT)
    } else {
        None
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn name_and_value() {
        for value in GC {
            assert!(validate_name_and_value("General_Category", value));
            assert!(validate_name_and_value("gc", value));
        }
        assert!(!validate_name_and_value("General_Category", "junk"));
        assert!(!validate_name_and_value("gc", "junk"));
        for value in SCRIPT {
            assert!(validate_name_and_value("Script", value));
            assert!(validate_name_and_value("Script_Extensions", value));
            assert!(validate_name_and_value("sc", value));
            assert!(validate_name_and_value("scx", value));
        }
        assert!(!validate_name_and_value("Script", "junk"));
        assert!(!validate_name_and_value("Script_Extensions", "junk"));
        assert!(!validate_name_and_value("sc", "junk"));
        assert!(!validate_name_and_value("scx", "junk"));
    }
    #[test]
    fn name_or_value() {
        for value in GC {
            assert!(validate_name_or_value(value));
        }
        for value in BINARY {
            assert!(validate_name_or_value(value));
        }
        assert!(!validate_name_or_value("junk"));
    }
}
