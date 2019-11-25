use log::trace;
use ress::prelude::RegEx;
use std::{iter::Peekable, str::Chars};

#[derive(Debug)]
pub struct Error {
    pub msg: String,
    pub idx: usize,
}
impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{} at {}", self.msg, self.idx)
    }
}

impl std::error::Error for Error {}

impl Error {
    fn new(idx: usize, msg: &str) -> Self {
        Self {
            idx,
            msg: msg.to_string(),
        }
    }
}

pub struct RegexParser<'a> {
    pattern: &'a str,
    chars: Peekable<Chars<'a>>,
    flag_str: &'a str,
    flags: RegExFlags,
    state: State<'a>,
}

impl<'a> RegexParser<'a> {
    pub fn new(js: &'a str) -> Result<Self, Error> {
        trace!("new parser: {:?}", js);
        if !js.starts_with('/') {
            return Err(Error::new(
                0,
                "regular expression literals must start with a /",
            ));
        }
        let pat_end_idx = if let Some(end_idx) = js.rfind('/') {
            if end_idx == 0 {
                return Err(Error::new(0, "regular expression literals must have 2 `/`"));
            } else {
                end_idx
            }
        } else {
            return Err(Error::new(0, "regular expression literals must have 2 `/`"));
        };
        let pattern = if let Some(pattern) = js.get(1..pat_end_idx) {
            pattern
        } else {
            return Err(Error::new(0, "Invalid regular expression"));
        };
        let (flags, flag_str) = if let Some(flag_str) = js.get(pat_end_idx + 1..) {
            let mut flags = RegExFlags::default();
            for (i, c) in flag_str.chars().enumerate() {
                flags.add_flag(c, pat_end_idx + i + 1)?;
            }
            (flags, flag_str)
        } else {
            return Err(Error::new(pat_end_idx, "invalid flags"));
        };
        Ok(Self {
            pattern,
            flag_str,
            chars: pattern.chars().peekable(),
            state: State::new(pattern.len(), flags.unicode),
            flags,
        })
    }

    pub fn parse(&mut self) -> Result<RegEx<&'a str>, Error> {
        trace!("parse");
        self.pattern()?;
        if !self.state.n && !self.state.group_names.is_empty() {
            self.pattern()?;
        }
        let flags = if !self.flag_str.is_empty() {
            Some(self.flag_str)
        } else {
            None
        };
        Ok(RegEx {
            body: self.pattern,
            flags,
        })
    }

    fn pattern(&mut self) -> Result<(), Error> {
        trace!("pattern");
        if self.state.pos > 0 {
            self.chars = self.pattern.chars().peekable();
            self.state.reset();
        }
        self.disjunction()?;
        if self.state.pos != self.state.len {
            if self.eat(')') {
                return Err(Error::new(self.state.pos, "Unmatched `)`"));
            }
            if self.eat('[') || self.eat('}') {
                return Err(Error::new(self.state.pos, "Lone quantifier brackets"));
            }
        }
        if self.state.max_back_refs > self.state.num_capturing_parens {
            return Err(Error::new(self.state.pos, "Invalid escape"));
        }
        for name in &self.state.back_ref_names {
            if !self.state.group_names.contains(name) {
                return Err(Error::new(
                    self.state.pos,
                    "Invalid named capture referenced",
                ));
            }
        }
        Ok(())
    }

    fn disjunction(&mut self) -> Result<(), Error> {
        trace!("disjunction");
        self.alternative()?;
        while self.eat('|') {
            self.alternative()?;
        }
        if self.eat_quantifier(true)? {
            return Err(Error::new(self.state.pos, "Nothing to repeat"));
        }
        if self.eat('{') {
            return Err(Error::new(self.state.pos, "lone quantifier brackets"));
        }
        Ok(())
    }

    fn alternative(&mut self) -> Result<(), Error> {
        trace!("alternative");
        while self.state.pos < self.state.len && self.eat_term()? {}
        Ok(())
    }

    fn eat_quantifier(&mut self, no_error: bool) -> Result<bool, Error> {
        trace!("eat_quantifier");
        Ok(if self.eat_quantifier_prefix(no_error)? {
            self.eat('?');
            true
        } else {
            false
        })
    }

    fn eat_quantifier_prefix(&mut self, no_error: bool) -> Result<bool, Error> {
        trace!("eat_quantifier_prefix");
        let ret = self.eat('*')
            || self.eat('+')
            || self.eat('?')
            || self.eat_braced_quantifier(no_error)?;

        Ok(ret)
    }

    fn eat_braced_quantifier(&mut self, no_error: bool) -> Result<bool, Error> {
        let start = self.state.pos;
        if self.eat('{') {
            if self.eat_digits(10) {

                let min = self.state.last_int_value;
                let max = if self.eat(',') && self.eat_digits(10) {
                    self.state.last_int_value
                } else {
                    None
                };
                if self.eat('}') {
                    if let (Some(max), Some(min)) = (max, min) {
                        if max < min && !no_error {
                            return Err(Error::new(
                                self.state.pos,
                                &format!("numbers out of order in {{{},{}}}", min, max),
                            ));
                        }
                    }
                    return Ok(true);
                }
            }
            if self.state.u && !no_error {
                return Err(Error::new(self.state.pos, "Incomplete quantifier"));
            }
            self.reset_to(start);
        }
        Ok(false)
    }

    fn eat_term(&mut self) -> Result<bool, Error> {
        trace!("eat_term");
        if self.eat_assertion()? {
            if self.state.last_assert_is_quant && self.eat_quantifier(false)? {
                if self.state.n {
                    return Err(Error::new(self.state.pos, "Invalid quantifier"));
                }
            }
            return Ok(true);
        }
        if self.state.u {
            if self.eat_atom()? {
                self.eat_quantifier(false)?;
                return Ok(true);
            }
        } else if self.eat_extended_atom()? {
            self.eat_quantifier(false)?;
            return Ok(true);
        }
        Ok(false)
    }

    fn eat_atom(&mut self) -> Result<bool, Error> {
        let ret = self.eat_pattern_characters()
            || self.eat('.')
            || self.eat_reverse_solidus_atom_escape()?
            || self.eat_character_class()?
            || self.eat_uncapturing_group()?
            || self.eat_capturing_group()?;
        Ok(ret)
    }

    fn eat_extended_atom(&mut self) -> Result<bool, Error> {
        let ret = self.eat('.')
            || self.eat_reverse_solidus_atom_escape()?
            || self.eat_character_class()?
            || self.eat_uncapturing_group()?
            || self.eat_capturing_group()?
            || self.eat_invalid_braced_quantifier()?
            || self.eat_extended_pattern_character();
        Ok(ret)
    }

    fn eat_invalid_braced_quantifier(&mut self) -> Result<bool, Error> {
        if self.eat_braced_quantifier(true)? {
            return Err(Error::new(self.state.pos, "Nothing to repeat"));
        }
        Ok(false)
    }

    fn eat_extended_pattern_character(&mut self) -> bool {
        if let Some(ch) = self.chars.peek() {
            if *ch != '$'
                && !(*ch >= '(' && *ch <= '+')
                && *ch != '.'
                && *ch != '?'
                && *ch != '['
                && *ch != '^'
                && *ch != '|'
            {
                self.advance();
                return true;
            }
        }
        false
    }

    fn eat_pattern_characters(&mut self) -> bool {
        let start = self.state.pos;
        while let Some(next) = self.chars.peek() {
            if !Self::is_syntax_ch(*next) {
                self.advance();
            }
        }
        self.state.pos != start
    }

    fn is_syntax_ch(ch: char) -> bool {
        ch == '$'
            || ch >= '(' && ch <= '+'
            || ch == '.'
            || ch == '?'
            || ch >= '[' && ch <= ']'
            || ch >= '{' && ch <= '}'
    }

    fn eat_reverse_solidus_atom_escape(&mut self) -> Result<bool, Error> {
        let start = self.state.pos;
        if self.eat('\\') {
            if self.eat_atom_escape()? {
                return Ok(true);
            }
            self.reset_to(start);
        }
        Ok(false)
    }

    fn eat_atom_escape(&mut self) -> Result<bool, Error> {
        if self.eat_back_ref()
            || self.eat_character_class_escape()?
            || self.eat_character_escape()?
            || self.state.n && self.eat_k_group_name()?
        {
            return Ok(true);
        }
        if self.state.u {
            if let Some(next) = self.chars.peek() {
                if *next == 'c' {
                    return Err(Error::new(self.state.pos, "Invalid unicode escape"));
                }
                return Err(Error::new(self.state.pos, "Invalid escape"));
            }
        }
        Ok(false)
    }

    fn eat_back_ref(&mut self) -> bool {
        let start = self.state.pos;
        if self.eat_decimal_escape() {
            let n = if let Some(n) = self.state.last_int_value {
                n
            } else {
                return true;
            };
            if self.state.u {
                if n > self.state.max_back_refs {
                    self.state.max_back_refs = n;
                }
                return true;
            }
            if n <= self.state.num_capturing_parens {
                return true;
            }
            self.reset_to(start);
        }
        false
    }

    fn eat_decimal_escape(&mut self) -> bool {
        self.state.last_int_value = Some(0);
        if let Some(next) = self.chars.peek() {
            if *next >= '1' && *next <= '9' {
                let n: u32 = (*next).into();
                let last_int_value = self.state.last_int_value.unwrap_or(0);
                self.state.last_int_value = Some(10 * last_int_value + n);
                self.advance();
                while let Some(next) = self.chars.peek() {
                    if *next >= '1' && *next <= '9' {
                        let n: u32 = (*next).into();
                        let last_int_value = self.state.last_int_value.unwrap_or(0);
                        self.state.last_int_value = Some(10 * last_int_value + n);
                        self.advance();
                    } else {
                        break;
                    }
                }
            }
            return true;
        }
        false
    }

    fn eat_character_class_escape(&mut self) -> Result<bool, Error> {
        if let Some(next) = self.chars.peek() {
            if Self::is_character_class_escape(*next) {
                self.state.last_int_value = None;
                self.advance();
                return Ok(true);
            }
            if self.state.u && (*next == 'P' || *next == 'p') {
                self.state.last_int_value = None;
                self.advance();
                if self.eat('{') && self.eat_unicode_property_value_expression()? && self.eat('}') {
                    return Ok(true);
                }
                return Err(Error::new(self.state.pos, "Invalid property name"));
            }
        }
        Ok(false)
    }

    fn eat_unicode_property_value_expression(&mut self) -> Result<bool, Error> {
        let start = self.state.pos;
        if self.eat_unicode_property_name() && self.eat('=') {
            let name = self.state.last_string_value.clone();
            if self.eat_unicode_property_value() {
                self.validate_unicode_property_name_and_value(
                    &name,
                    &self.state.last_string_value,
                )?;
                return Ok(true);
            }
        }
        self.reset_to(start);
        if self.eat_lone_unicode_property_name_or_value() {
            self.validate_unicode_property_name_or_value(&self.state.last_string_value)?;
            return Ok(true);
        }
        Ok(false)
    }

    fn eat_unicode_property_name(&mut self) -> bool {
        let start = self.state.pos;
        self.state.last_string_value = None;
        while let Some(ch) = self.chars.peek() {
            if Self::is_unicode_property_name_character(*ch) {
                self.advance();
            } else {
                break;
            }
        }
        if self.state.pos != start {
            self.state.last_string_value = self.pattern.get(start..self.state.pos)
        }
        self.state.last_string_value.is_some()
    }

    fn eat_unicode_property_value(&mut self) -> bool {
        let start = self.state.pos;
        while let Some(next) = self.chars.peek() {
            if Self::is_unicode_property_value_character(*next) {
                self.advance();
            } else {
                break;
            }
        }
        if start != self.state.pos {
            self.state.last_string_value = self.pattern.get(start..self.state.pos);
        }
        self.state.last_string_value.is_some()
    }

    fn eat_lone_unicode_property_name_or_value(&mut self) -> bool {
        self.eat_unicode_property_value()
    }

    fn validate_unicode_property_name_and_value(
        &self,
        name: &Option<&'a str>,
        value: &Option<&'a str>,
    ) -> Result<(), Error> {
        Err(
            Error {
                idx: self.state.pos,
                msg: format!("Unable to validate unicode property name and value ({:?} and {:?}), this is not yet implemented", name, value),
            }
        )
    }
    
    fn validate_unicode_property_name_or_value(
        &self,
        name_or_value: &Option<&'a str>,
    ) -> Result<(), Error> {
        Err(
            Error {
                idx: self.state.pos,
                msg: format!("Unable to validate unicode property name or value ({:?}), this is not yet implemented", name_or_value),
            }
        )
    }

    fn is_unicode_property_name_character(ch: char) -> bool {
        Self::is_control_letter(ch) || ch == '_'
    }

    fn is_unicode_property_value_character(ch: char) -> bool {
        Self::is_unicode_property_name_character(ch) || ch.is_digit(10)
    }

    fn is_control_letter(ch: char) -> bool {
        (ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z')
    }

    fn is_character_class_escape(ch: char) -> bool {
        ch == 'd' || ch == 'D' || ch == 's' || ch == 'S' || ch == 'w' || ch == 'W'
    }

    fn eat_character_escape(&mut self) -> Result<bool, Error> {
        let ret = self.eat_control_escape()
        || self.eat_c_control_letter()
        || self.eat_zero()
        || self.eat_hex_escape_sequence()?
        || self.eat_regex_unicode_escape_sequence()?
        || (!self.state.u && self.eat_legacy_octal_escape_sequence())
        || self.eat_identity_escape();
        Ok(ret)
    }

    fn eat_control_escape(&mut self) -> bool {
        if let Some(ch) = self.chars.peek() {
            match ch {
                't' | 'n' | 'v' | 'f' | 'r' => {
                    self.state.last_int_value = Some((*ch).into());
                    self.advance();
                    return true;
                }
                _ => return false,
            }
        }
        false
    }

    fn eat_c_control_letter(&mut self) -> bool {
        let start = self.state.pos;
        if self.eat('c') {
            if self.eat_control_letter() {
                return true;
            }
            self.reset_to(start);
        }
        false
    }

    fn eat_control_letter(&mut self) -> bool {
        if let Some(next) = self.chars.peek() {
            if Self::is_control_letter(*next) {
                let n: u32 = (*next).into();
                self.state.last_int_value = Some(n % 0x20);
                self.advance();
                return true;
            }
        }
        false
    }

    fn eat_zero(&mut self) -> bool {
        if let Some(zero) = self.chars.peek() {
            if *zero == '0' {
                self.state.last_int_value = Some(0);
                self.advance();
                return true;
            }
        }
        false
    }
    fn eat_hex_escape_sequence(&mut self) -> Result<bool, Error> {
        let start = self.state.pos;
        if self.eat('x') {
            if self.eat_fixed_hex_digits(2) {
                return Ok(true);
            }
            if self.state.u {
                return Err(Error::new(start, "Invalid escape"))
            }
            self.reset_to(start)
        }
        Ok(false)
    }

    fn eat_fixed_hex_digits(&mut self, len: usize) -> bool {
        let start = self.state.pos;
        self.state.last_int_value = Some(0);
        for _ in 0..len {
            if let Some(n) = self.eat_digit(16) {
                let last_int_value = self.state.last_int_value.unwrap_or(0);
                self.state.last_int_value = Some(16 * last_int_value + n);
            } else {
                self.reset_to(start);
                return false;
            }
        }
        true
    }

    fn eat_legacy_octal_escape_sequence(&mut self) -> bool {
        let mut last_int_value = 0;
        if let Some(n1) = self.eat_digit(8) {
            if let Some(n2) = self.eat_digit(8) {
                if n1 <= 3 {
                    if let Some(n3) = self.eat_digit(8) {
                        last_int_value = n1 * 64 + n2 * 8 + n3;
                    } else {
                        last_int_value = n1 * 8 + n2;
                    }
                } else {
                    last_int_value = n1 * 8 + n2;
                }
            } else {
                last_int_value = n1;
            }
            self.state.last_int_value = Some(last_int_value);
            return true;
        }
        false
    }
    fn eat_digit(&mut self, radix: u32) -> Option<u32> {
        if let Some(next) = self.chars.peek() {
            if next.is_digit(radix) {
                let n = next.to_digit(radix);
                self.advance();
                return n
            }
        }
        None
    }
    fn eat_identity_escape(&mut self) -> bool {
        if self.state.u {
            if self.eat_syntax_character() {
                return true;
            }
            if self.eat('/') {
                self.state.last_int_value = Some(0x2f);
                return true;
            }
        }
        false
    }
    fn eat_syntax_character(&mut self) -> bool {
        if let Some(ch) = self.chars.peek() {
            if Self::is_syntax_ch(*ch) {
                self.state.last_int_value = Some((*ch).into());
                self.advance();
                return true;
            }
        }
        false
    }

    fn eat_regex_unicode_escape_sequence(&mut self) -> Result<bool, Error> {
        let start = self.state.pos;
        if self.eat('u') {
            if self.eat_fixed_hex_digits(4) {
                let lead = self.state.last_int_value.unwrap_or(0);
                if self.state.u && lead >= 0xD800 && lead <= 0xDBFF {
                    let lead_end = self.state.pos;
                    if self.eat('\\') && self.eat('u') && self.eat_fixed_hex_digits(4) {
                        let tail = self.state.last_int_value.unwrap_or(0);
                        if tail >= 0xDC00
                        && tail <= 0xDFFF {
                            self.state.last_int_value = Some((lead - 0xD800) * 0x400 + (tail - 0xDC00) + 0x10000);
                            return Ok(true);
                        }
                    }
                    self.reset_to(lead_end);
                    self.state.last_int_value = Some(lead);
                }
                return Ok(true);
            }
            if self.state.u 
                && self.eat('{')
                && self.eat_digits(16)
                && self.eat('}')
                && self.state.last_int_value.map(|v| v <= 0x10FFFF).unwrap_or(true) {
                return Ok(true);
            }

            if self.state.u {
                return Err(Error::new(self.state.pos, "Invalid unicode escape"));
            }

            self.reset_to(start)
        }
        Ok(false)
    }
   
    fn eat_character_class(&mut self) -> Result<bool, Error> {
        if self.eat('[') {
            self.eat('^');
            self.class_ranges()?;
            if self.eat(']') {
                Ok(true)
            } else {
                Err(Error::new(self.state.pos, "Unterminated character class"))
            }
            
        } else {
            Ok(false)
        }
    }

    fn class_ranges(&mut self) -> Result<(), Error> {
        while self.eat_class_atom()? {
            let left = self.state.last_int_value;
            if self.eat('-') && self.eat_class_atom()? {
                let right = self.state.last_int_value;
                if self.state.u && (left.is_none() || right.is_none()) {
                    return Err(Error::new(self.state.pos, "Invalid character class"))
                }
                if let (Some(left), Some(right)) = (left, right) {
                    if left > right {
                        return Err(Error::new(self.state.pos, &format!("Rage out of order in character class ({} > {})", left, right)));
                    }
                }
            }
        }
        Ok(())
    }

    fn eat_class_atom(&mut self) -> Result<bool, Error> {
        let start = self.state.pos;
        if self.eat('\\') {
            if self.eat_class_escape()? {
                return Ok(true)
            }
            if self.state.u {
                if let Some(ch) = self.chars.peek() {
                    if *ch == 'c' || ch.is_digit(8) {
                        return Err(Error::new(self.state.pos, "Invalid class escape"));
                    }
                    return Err(Error::new(self.state.pos, "Invalid escape"));
                }
            }
            self.reset_to(start);
        }
        if let Some(ch) = self.chars.peek() {
            if *ch != ']' {
                self.state.last_int_value = Some((*ch).into());
                self.advance();
                return Ok(true)
            }
        }
        Ok(false)
    }

    fn eat_class_escape(&mut self) -> Result<bool, Error> {
        let start = self.state.pos;
        if self.eat('b') {
            self.state.last_int_value = Some(0x08);
            return Ok(true);
        }
        if self.state.u && self.eat('-') {
            self.state.last_int_value = Some(0x2D);
            return Ok(true);
        }
        if self.state.u && self.eat('c') {
            if self.eat_class_control_letter() {
                return Ok(true);
            }
            self.reset_to(start);
        }
        let ret = self.eat_character_class_escape()?
            || self.eat_character_escape()?;
        Ok(ret)
    }

    fn eat_class_control_letter(&mut self) -> bool {
        if let Some(ch) = self.chars.peek() {
            if ch.is_digit(10) || *ch == '_' {
                let n: u32 = (*ch).into();
                self.state.last_int_value = Some(n % 0x20);
                self.advance();
                return true;
            }
        }
        false
    }

    fn eat_k_group_name(&mut self) -> Result<bool, Error> {
        if self.eat('k') {
            if self.eat_group_name()? {
                if let Some(name) = self.state.last_string_value {
                    self.state.back_ref_names.push(name.clone());
                    return Ok(true);
                }
            }   
            return Err(Error::new(self.state.pos, "Invalid named reference"));
        }
        Ok(false)
    }

    fn eat_group_name(&mut self) -> Result<bool, Error> {
        self.state.last_string_value = None;
        if self.eat('<') {
            if self.eat_regex_identifier_name()? && self.eat('>') {
                return Ok(true);
            }
            return Err(Error::new(self.state.pos, "Invalid capture group name"));
        }
        Ok(false)
    }

    fn eat_regex_identifier_name(&mut self) -> Result<bool, Error> {
        let start = self.state.pos;
        self.state.last_string_value = None;
        if self.eat_ident_start()? {
            while self.eat_ident_part()? {

            }
            self.state.last_string_value = Some(&self.pattern[start..self.state.pos]);
            return Ok(true);
        }
        Ok(false)
    }

    fn eat_ident_start(&mut self) -> Result<bool, Error> {
        let start = self.state.pos;
        self.state.last_string_value = None;
        let mut ch = if let Some(ch) = self.chars.peek() {
            *ch
        } else {
            return Ok(false);
        };
        self.advance();
        if ch == '\\' && self.eat_regex_unicode_escape_sequence()? {
            if let Some(n) = self.state.last_int_value {
                if let Some(n) = std::char::from_u32(n) {
                    ch = n;
                }
            }
        }
        if Self::is_id_start(ch) {
            self.state.last_int_value = Some(ch.into());
            return Ok(true)
        }
        self.reset_to(start);
        Ok(false)
    }

    fn eat_ident_part(&mut self) -> Result<bool, Error> {
        let start = self.state.pos;
        let mut ch = if let Some(ch) = self.chars.peek() {
            *ch
        } else {
            return Ok(false);
        };
        self.advance();
        if ch == '\\' && self.eat_regex_unicode_escape_sequence()? {
            if let Some(n) = self.state.last_int_value {
                if let Some(n) = std::char::from_u32(n) {
                    ch = n;
                }
            }
        }
        if Self::is_id_continue(ch) {
            self.state.last_int_value = Some(ch.into());
            return Ok(true)
        }
        self.reset_to(start);
        Ok(false)
    }

    fn is_id_start(ch: char) -> bool {
        (ch >= 'A' && ch <= 'Z')
        || (ch >= 'a' && ch <= 'z')
        || ch == '$'
        || ch == '_'
        || unic_ucd_ident::is_id_start(ch)
    }

    fn is_id_continue(ch: char) -> bool {
        (ch >= 'A' && ch <= 'Z')
        || (ch >= 'a' && ch <= 'z')
        || (ch >= '0' && ch <= '9')
        || ch == '$'
        || ch == '_'
        || unic_ucd_ident::is_id_continue(ch)
    }

    fn eat_uncapturing_group(&mut self) -> Result<bool, Error> {
        let start = self.state.pos;
        if self.eat('(') {
            if self.eat('?') && self.eat(':') {
                self.disjunction()?;
                if self.eat(')') {
                    return Ok(true)
                }
                return Err(Error::new(start, "Unterminated group"));
            }
            self.reset_to(start)
        }
        Ok(false)
    }

    fn eat_capturing_group(&mut self) -> Result<bool, Error> {
        if self.eat('(') {
            self.group_specifier()?;
            self.disjunction()?;
            if self.eat(')') {
                self.state.num_capturing_parens += 1;
                Ok(true)
            } else {
                Err(Error::new(self.state.pos, "Unterminated group"))
            }
        } else {
            Ok(false)
        }
    }

    fn group_specifier(&mut self) -> Result<(), Error> {
        if self.eat('?') {
            if self.eat_group_name()? {
                if let Some(name) = self.state.last_string_value {
                    if self.state.group_names.contains(&name) {
                        return Err(Error::new(self.state.pos, "Duplicate capture group name"));
                    } else {
                        self.state.group_names.push(name.clone());
                    }
                }
            }
            return Err(Error::new(self.state.pos, "Invalid group"));
        }
        Ok(())
    }

    fn eat_assertion(&mut self) -> Result<bool, Error> {
        trace!("eat_assertion");
        let start = self.state.pos;
        self.state.last_assert_is_quant = false;
        if self.eat('^') || self.eat('$') {
            return Ok(true);
        }
        if self.eat('\\') {
            if self.eat('B') || self.eat('b') {
                return Ok(true);
            }
            self.reset_to(start);
        }
        if self.eat('(') && self.eat('?') {
            let look_behind = self.eat('<');
            if self.eat('=') || self.eat('!') {
                self.disjunction()?;
                if !self.eat(')') {
                    return Err(Error::new(self.state.pos, "Unterminated group"));
                }
                self.state.last_assert_is_quant = !look_behind;
                return Ok(true);
            }
        }
        self.reset_to(start);
        Ok(false)
    }

    fn eat_digits(&mut self, radix: u32) -> bool {
        trace!("eat_digits");
        let start = self.state.pos;
        while let Some(next) = self.chars.peek() {
            if let Some(n) = next.to_digit(radix) {
                let last_int_value = self.state.last_int_value.unwrap_or(0);
                self.state.last_int_value = Some(radix * last_int_value + n);
                self.advance();
            } else {
                break;
            }
        }
        self.state.pos != start
    }

    fn eat(&mut self, ch: char) -> bool {
        trace!("eat, {:?}", ch);
        if let Some(next) = self.chars.peek() {
            if *next == ch {
                self.advance();
                return true;
            }
        }
        false
    }

    fn advance(&mut self) {
        trace!("advance, {}", self.state.pos);
        if let Some(ch) = self.chars.next() {
            self.state.pos += ch.len_utf8();
        }
    }

    fn reset_to(&mut self, idx: usize) {
        trace!("reset_to {}", idx);
        let remaining = &self.pattern[idx..];
        self.chars = remaining.chars().peekable();
        self.state.pos = idx;
    }
}

struct State<'a> {
    pos: usize,
    len: usize,
    last_int_value: Option<u32>,
    last_string_value: Option<&'a str>,
    last_assert_is_quant: bool,
    num_capturing_parens: u32,
    max_back_refs: u32,
    group_names: Vec<&'a str>,
    back_ref_names: Vec<&'a str>,
    n: bool,
    u: bool,
}

impl<'a> State<'a> {
    pub fn new(len: usize, u: bool) -> Self {
        Self {
            pos: 0,
            len,
            last_int_value: None,
            last_string_value: None,
            last_assert_is_quant: false,
            num_capturing_parens: 0,
            max_back_refs: 0,
            group_names: Vec::new(),
            back_ref_names: Vec::new(),
            n: u,
            u,
        }
    }
    pub fn reset(&mut self) {
        self.pos = 0;
        self.last_int_value = None;
        self.last_string_value = None;
        self.num_capturing_parens = 0;
        self.max_back_refs = 0;
        self.group_names.clear();
        self.back_ref_names.clear();
    }
}

#[derive(Debug)]
struct RegExFlags {
    case_insensitive: bool,
    multi_line: bool,
    dot_matches_new_line: bool,
    unicode: bool,
    global: bool,
    sticky: bool,
}

impl Default for RegExFlags {
    fn default() -> Self {
        RegExFlags {
            case_insensitive: false,
            multi_line: false,
            dot_matches_new_line: false,
            unicode: false,
            global: false,
            sticky: false,
        }
    }
}

impl RegExFlags {
    fn add_flag(&mut self, c: char, pos: usize) -> Result<(), Error> {
        match c {
            'g' => {
                if self.global {
                    Err(Error::new(pos, "duplicate g flag"))
                } else {
                    self.global = true;
                    Ok(())
                }
            }
            'i' => {
                if self.case_insensitive {
                    Err(Error::new(pos, "duplicate i flag"))
                } else {
                    self.case_insensitive = true;
                    Ok(())
                }
            }
            'm' => {
                if self.multi_line {
                    Err(Error::new(pos, "duplicate m flag"))
                } else {
                    self.multi_line = true;
                    Ok(())
                }
            }
            's' => {
                if self.dot_matches_new_line {
                    Err(Error::new(pos, "duplicate s flag"))
                } else {
                    self.dot_matches_new_line = true;
                    Ok(())
                }
            }
            'u' => {
                if self.unicode {
                    Err(Error::new(pos, "duplicate u flag"))
                } else {
                    self.unicode = true;
                    Ok(())
                }
            }
            'y' => {
                if self.sticky {
                    Err(Error::new(pos, "duplicate y flag"))
                } else {
                    self.sticky = true;
                    Ok(())
                }
            }
            _ => Err(Error::new(pos, &format!("invalid flag {:?}", c))),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn lots_of_regexes() {
        let _ = pretty_env_logger::try_init();
        let simple = "/asdf|fdsa/g";
        let mut parser = RegexParser::new(simple).expect("unable to create parser");
        parser.parse().expect("failed to parse");
    }
}
