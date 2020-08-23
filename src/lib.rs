use log::trace;
use std::{iter::Peekable, str::Chars};

mod unicode;
mod unicode_tables;

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
    state: State<'a>,
}

impl<'a> RegexParser<'a> {
    pub fn new(js: &'a str) -> Result<Self, Error> {
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
        let flags = if let Some(flag_str) = js.get(pat_end_idx + 1..) {
            let mut flags = RegExFlags::default();
            for (i, c) in flag_str.chars().enumerate() {
                flags.add_flag(c, pat_end_idx + i + 1)?;
            }
            flags
        } else {
            return Err(Error::new(pat_end_idx, "invalid flags"));
        };
        Ok(Self {
            pattern,
            chars: pattern.chars().peekable(),
            state: State::new(pattern.len(), flags.unicode),
        })
    }

    pub fn validate(&mut self) -> Result<(), Error> {
        trace!("parse {:?}", self.current());
        self.pattern()?;
        if !self.state.n && !self.state.group_names.is_empty() {
            self.pattern()?;
        }
        Ok(())
    }
    /// The primary entry point, `Pattern` is technically
    /// the target for all the characters inbetween the `/`s
    /// ```js
    /// let re = /pattern/
    /// ```
    fn pattern(&mut self) -> Result<(), Error> {
        trace!("pattern {:?}", self.current(),);
        if self.state.pos > 0 {
            self.chars = self.pattern.chars().peekable();
            self.state.reset();
        }
        self.disjunction()?;
        if self.state.pos != self.state.len {
            if self.eat(')') {
                return Err(Error::new(self.state.pos, "Unmatched `)`"));
            }
            if self.eat(']') || self.eat('}') {
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
    /// A disjunction will be items separated by a `|`
    /// ```js
    /// let re = /dis|junction/
    /// ```
    fn disjunction(&mut self) -> Result<(), Error> {
        trace!("disjunction {:?}", self.current(),);
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
    /// An alternative is either side of a `disjunction`
    /// ```js
    /// let re = /alt1|alt2/;
    /// ```
    fn alternative(&mut self) -> Result<(), Error> {
        trace!("alternative {:?}", self.current(),);
        while self.state.pos < self.state.len && self.eat_term()? {}
        Ok(())
    }
    /// a quantifier is appended to an item to say how
    /// many of that item should exist, this includes `*` (0 or more)
    /// `+` (1 or more), `?` (0 or 1) or `{1}`, `{1,2}`
    ///
    /// ```js
    /// let re = /s*p+q?a{1}b{1,2}/;
    /// ```
    fn eat_quantifier(&mut self, no_error: bool) -> Result<bool, Error> {
        trace!("eat_quantifier {:?}", self.current(),);
        Ok(if self.eat_quantifier_prefix(no_error)? {
            self.eat('?');
            true
        } else {
            false
        })
    }
    /// A prefix is either then characer `*`, `+`, `?` or
    /// the full braced quantifier `{1} or `{1,2}`
    fn eat_quantifier_prefix(&mut self, no_error: bool) -> Result<bool, Error> {
        trace!("eat_quantifier_prefix {:?}", self.current(),);
        let ret = self.eat('*')
            || self.eat('+')
            || self.eat('?')
            || self.eat_braced_quantifier(no_error)?;
        Ok(ret)
    }
    /// A braced quantifier either 1 or two numbers wrapped in
    /// curly braces separated by a comma. The first number
    /// refers to the minimum number of repeated items and the
    /// second number refers to the maximum. The second number
    /// is optional
    ///
    /// ```js
    /// let re = /a{1,100}/;
    /// if (re.text('a'.repeat(101))) {
    ///     throw new Error('re will only match up to 100 repeated `a`s');
    /// }
    /// ```
    fn eat_braced_quantifier(&mut self, no_error: bool) -> Result<bool, Error> {
        trace!("eat_braced_quantifier {:?}", self.current(),);
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
    /// A term is the body of an `alternate`
    /// it may include an `assertion` or an `atom`
    /// or an `atom` followed by a `quantifier`
    ///
    /// ```js
    /// let re = /term/
    /// ```
    fn eat_term(&mut self) -> Result<bool, Error> {
        trace!("eat_term {:?}", self.current(),);
        if self.eat_assertion()? {
            if self.state.last_assert_is_quant && self.eat_quantifier(false)? && self.state.n {
                return Err(Error::new(self.state.pos, "Invalid quantifier"));
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
    /// An atom is a single character or representative
    /// set of characters. This includes things like
    /// groups and classes
    /// ```js
    /// let re = /a(b)[a-b]/;
    /// ```
    fn eat_atom(&mut self) -> Result<bool, Error> {
        trace!("eat_atom {:?}", self.current(),);
        let ret = self.eat_pattern_characters()
            || self.eat('.')
            || self.eat_reverse_solidus_atom_escape()?
            || self.eat_character_class()?
            || self.eat_uncapturing_group()?
            || self.eat_capturing_group()?;
        Ok(ret)
    }
    /// An extended version of the normal `atom`, this includes
    /// exotic classes and groups
    fn eat_extended_atom(&mut self) -> Result<bool, Error> {
        trace!("eat_extended_atom {:?}", self.current(),);
        let ret = self.eat('.')
            || self.eat_reverse_solidus_atom_escape()?
            || self.eat_character_class()?
            || self.eat_uncapturing_group()?
            || self.eat_capturing_group()?
            || self.eat_invalid_braced_quantifier()?
            || self.eat_extended_pattern_character();
        Ok(ret)
    }
    /// attempts to consume a braced quantifier
    /// in an invalid position.
    fn eat_invalid_braced_quantifier(&mut self) -> Result<bool, Error> {
        trace!("eat_invalid_braced_quantifier {:?}", self.current(),);
        if self.eat_braced_quantifier(true)? {
            return Err(Error::new(self.state.pos, "Nothing to repeat"));
        }
        Ok(false)
    }
    /// extended pattern characters include symbols
    /// like `(` or `|`
    fn eat_extended_pattern_character(&mut self) -> bool {
        trace!("eat_extended_pattern_character {:?}", self.current(),);
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
    /// A pattern character is any non-syntax
    /// character
    fn eat_pattern_characters(&mut self) -> bool {
        trace!("eat_pattern_characters {:?}", self.current(),);
        let start = self.state.pos;
        while let Some(next) = self.chars.peek() {
            if !Self::is_syntax_ch(*next) {
                self.advance();
            } else {
                break;
            }
        }
        self.state.pos != start
    }
    /// Syntax characters are operators
    /// that have special meanin in a regular expression
    /// like `?` or `.`
    fn is_syntax_ch(ch: char) -> bool {
        ch == '$'
            || ch >= '(' && ch <= '+'
            || ch == '.'
            || ch == '?'
            || ch >= '[' && ch <= '^'
            || ch >= '{' && ch <= '}'
    }

    /// a reverse solidus is a really fancy name for `\`
    fn eat_reverse_solidus_atom_escape(&mut self) -> Result<bool, Error> {
        trace!("eat_reverse_solidus_atom_escape {:?}", self.current(),);
        let start = self.state.pos;
        if self.eat('\\') {
            if self.eat_atom_escape()? {
                return Ok(true);
            }
            self.reset_to(start);
        }
        Ok(false)
    }
    /// Picking up after a `\`
    fn eat_atom_escape(&mut self) -> Result<bool, Error> {
        trace!("eat_atom_escape {}", self.state.u,);
        if self.eat_back_ref()
            || self.eat_character_class_escape()?
            || self.eat_character_escape()?
            || self.state.n && self.eat_k_group_name()?
        {
            return Ok(true);
        }
        trace!("previous check failed, {}", self.state.u);
        if self.state.u {
            trace!("previous all failed, with unicode flag");
            if let Some(next) = self.current() {
                if *next == 'c' {
                    return Err(Error::new(self.state.pos, "Invalid unicode escape"));
                }
            }
            trace!("returning error");
            return Err(Error::new(self.state.pos, "Invalid escape"));
        }
        Ok(false)
    }
    /// A back reference is a reference to a
    /// previous capture group
    /// ```js
    /// let re = /(abc)\1/;
    /// ```
    ///
    /// in the above, we would match "abcabc" only
    fn eat_back_ref(&mut self) -> bool {
        trace!("eat_back_ref {:?}", self.current(),);
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
    /// an escaped decimal number
    fn eat_decimal_escape(&mut self) -> bool {
        trace!("eat_decimal_escape {:?}", self.current(),);
        let start = self.state.pos;
        let mut last_int_value = 0;
        while let Some(next) = self.chars.peek() {
            if let Some(n) = next.to_digit(10) {
                last_int_value = 10 * last_int_value + n;
                self.advance()
            } else {
                break;
            }
        }
        self.state.last_int_value = Some(last_int_value);
        self.state.pos != start
    }
    /// An escaped character class
    /// this include `\d`, `\s`, and `\w`
    /// if the regex has the `u` flag, it would also
    /// include `\p{General_Category=Greek}`
    fn eat_character_class_escape(&mut self) -> Result<bool, Error> {
        trace!("eat_character_class_escape {:?}", self.current(),);
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
    /// After an escaped p (`\p{`), with unicode enabled would
    /// allow for unicode category classes
    fn eat_unicode_property_value_expression(&mut self) -> Result<bool, Error> {
        trace!("eat_unicode_property_value_expression {:?}", self.current(),);
        let start = self.state.pos;
        if self.eat_unicode_property_name() && self.eat('=') {
            let name = self.state.last_string_value;
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
    /// This will be one of the following
    ///  * `General_Category`
    ///  * `gc`
    ///  * `Script`
    ///  * `sc`
    ///  * `Script_Extensions`
    ///  * `scx`
    fn eat_unicode_property_name(&mut self) -> bool {
        trace!("eat_unicode_property_name {:?}", self.current(),);
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
    /// This should match a value in the corresponding
    /// category lists
    fn eat_unicode_property_value(&mut self) -> bool {
        trace!("eat_unicode_property_value {:?}", self.current(),);
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
    /// This could be any General_Category or Binary Property
    /// entry
    fn eat_lone_unicode_property_name_or_value(&mut self) -> bool {
        trace!(
            "eat_lone_unicode_property_name_or_value {:?}",
            self.current(),
        );
        self.eat_unicode_property_value()
    }
    /// Validates that the name and value
    /// are valid
    fn validate_unicode_property_name_and_value(
        &self,
        name: &Option<&'a str>,
        value: &Option<&'a str>,
    ) -> Result<(), Error> {
        if let (Some(name), Some(value)) = (name, value) {
            if !unicode::validate_name_and_value(name, value) {
                Err(Error {
                    idx: self.state.pos,
                    msg: format!(
                        "Unable to validate unicode property name and value ({:?} and {:?})",
                        name, value
                    ),
                })
            } else {
                Ok(())
            }
        } else {
            Err(Error {
                idx: self.state.pos,
                msg: "Invalid unicode property name & value provided".to_string(),
            })
        }
    }
    /// Validates that a lone name or value
    /// is valid
    fn validate_unicode_property_name_or_value(
        &self,
        name_or_value: &Option<&'a str>,
    ) -> Result<(), Error> {
        if let Some(name) = name_or_value {
            if !unicode::validate_name_or_value(name) {
                Err(Error {
                    idx: self.state.pos,
                    msg: format!(
                        "Unable to validate unicode property name or value ({:?})",
                        name_or_value
                    ),
                })
            } else {
                Ok(())
            }
        } else {
            Err(Error {
                idx: self.state.pos,
                msg: "Invalid unicoe property name or value".to_string(),
            })
        }
    }
    /// This will be any control letter plus `_`
    fn is_unicode_property_name_character(ch: char) -> bool {
        Self::is_control_letter(ch) || ch == '_'
    }
    /// This will be any name character plus and decimal digit
    fn is_unicode_property_value_character(ch: char) -> bool {
        Self::is_unicode_property_name_character(ch) || ch.is_digit(10)
    }
    /// Any capital or lowercase english character
    fn is_control_letter(ch: char) -> bool {
        (ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z')
    }
    /// `d`, `D`, `s`, `S`, `w`, `W`
    fn is_character_class_escape(ch: char) -> bool {
        ch == 'd' || ch == 'D' || ch == 's' || ch == 'S' || ch == 'w' || ch == 'W'
    }
    /// This would consume any valid character after a `\`
    fn eat_character_escape(&mut self) -> Result<bool, Error> {
        trace!("eat_character_escape {:?}", self.current(),);
        let ret = self.eat_control_escape()
            || self.eat_c_control_letter()
            || self.eat_zero()
            || self.eat_hex_escape_sequence()?
            || self.eat_unicode_escape_sequence()?
            || (!self.state.u && self.eat_legacy_octal_escape_sequence())
            || self.eat_identity_escape();
        Ok(ret)
    }
    /// Peek at the current look ahead token
    fn current(&mut self) -> Option<&char> {
        self.chars.peek()
    }
    /// control escapes include `\t`, `\n`, `\v`, `\f` and `\r`
    ///
    /// ```js
    /// let re = /\n\t/;
    /// ```
    fn eat_control_escape(&mut self) -> bool {
        trace!("eat_control_escape {:?}", self.current(),);
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
    /// An escaped control character is any `\c` followed
    /// by a single english letter (upper or lower)
    ///
    /// ```js
    /// let re = /\cI/;
    /// ```
    /// These characters represent an old
    /// form of control escapes like \t (in the example above)
    ///
    /// (wikipedia)[https://en.wikipedia.org/wiki/Control_character]
    fn eat_c_control_letter(&mut self) -> bool {
        trace!("eat_c_control_letter {:?}", self.current(),);
        let start = self.state.pos;
        if self.eat('c') {
            if self.eat_control_letter() {
                return true;
            }
            self.reset_to(start);
        }
        false
    }
    /// Eat a letter after a `\c`
    fn eat_control_letter(&mut self) -> bool {
        trace!("eat_control_letter {:?}", self.current(),);
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
    /// Eat a zero character
    fn eat_zero(&mut self) -> bool {
        trace!("eat_zero {:?}", self.current(),);
        if let Some(zero) = self.chars.peek() {
            if *zero == '0' {
                self.state.last_int_value = Some(0);
                self.advance();
                return true;
            }
        }
        false
    }
    /// eat a hexidecimal number escape sequence
    fn eat_hex_escape_sequence(&mut self) -> Result<bool, Error> {
        trace!("eat_hex_escape_sequence {:?}", self.current(),);
        let start = self.state.pos;
        if self.eat('x') {
            if self.eat_fixed_hex_digits(2) {
                return Ok(true);
            }
            if self.state.u {
                return Err(Error::new(start, "Invalid escape"));
            }
            self.reset_to(start)
        }
        Ok(false)
    }
    /// Attempt to consume a fixed number of hexidecimal
    /// characters in a row
    fn eat_fixed_hex_digits(&mut self, len: usize) -> bool {
        trace!("eat_fixed_hex_digits {:?}", self.current(),);
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
    /// Eat a sequence of numbers starting with 0, all below 8
    fn eat_legacy_octal_escape_sequence(&mut self) -> bool {
        trace!("eat_legacy_octal_escape_sequence {:?}", self.current(),);
        let last_int_value;
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
    /// Attempt to consume a digit of the provided
    /// radix
    fn eat_digit(&mut self, radix: u32) -> Option<u32> {
        trace!("eat_digit {:?}", self.current(),);
        if let Some(next) = self.chars.peek() {
            if next.is_digit(radix) {
                let n = next.to_digit(radix);
                self.advance();
                return n;
            }
        }
        None
    }

    fn eat_identity_escape(&mut self) -> bool {
        trace!("eat_identity_escape {:?}", self.current(),);
        if self.state.u {
            if self.eat_syntax_character() {
                return true;
            }
            if self.eat('/') {
                self.state.last_int_value = Some(0x2f);
                return true;
            }
            return false;
        }
        if let Some(ch) = self.chars.peek() {
            if *ch != 'c' && (!self.state.n || *ch != 'k') {
                let n = (*ch).into();
                self.state.last_int_value = Some(n);
                self.advance();
                true
            } else {
                false
            }
        } else {
            true
        }
    }
    /// Attempt to consume a syntax character like `{`
    fn eat_syntax_character(&mut self) -> bool {
        trace!("eat_syntax_character {:?}", self.current(),);
        if let Some(ch) = self.chars.peek() {
            if Self::is_syntax_ch(*ch) {
                self.state.last_int_value = Some((*ch).into());
                self.advance();
                return true;
            }
        }
        false
    }
    /// A fixed 4 digit or curly brace unicode escape character
    /// ```js
    /// let re = /\u{61}\u0062/;
    /// ```
    fn eat_unicode_escape_sequence(&mut self) -> Result<bool, Error> {
        trace!("eat_regex_unicode_escape_sequence {:?}", self.current(),);
        let start = self.state.pos;
        if self.eat('u') {
            if self.eat_fixed_hex_digits(4) {
                let lead = self.state.last_int_value.unwrap_or(0);
                if self.state.u && lead >= 0xD800 && lead <= 0xDBFF {
                    let lead_end = self.state.pos;
                    if self.eat('\\') && self.eat('u') && self.eat_fixed_hex_digits(4) {
                        let tail = self.state.last_int_value.unwrap_or(0);
                        if tail >= 0xDC00 && tail <= 0xDFFF {
                            self.state.last_int_value =
                                Some((lead - 0xD800) * 0x400 + (tail - 0xDC00) + 0x10000);
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
                && self
                    .state
                    .last_int_value
                    .map(|v| v <= 0x10_FFFF)
                    .unwrap_or(true)
            {
                return Ok(true);
            }

            if self.state.u {
                return Err(Error::new(self.state.pos, "Invalid unicode escape"));
            }

            self.reset_to(start)
        }
        Ok(false)
    }
    /// Attempt to consume a character class
    /// ```js
    /// let re = /[clas]/;
    /// ```
    fn eat_character_class(&mut self) -> Result<bool, Error> {
        trace!("eat_character_class {:?}", self.current(),);
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
    /// Attempt to consume a class range
    /// ```js
    /// let re = /[c-r]/;
    /// ```
    fn class_ranges(&mut self) -> Result<(), Error> {
        trace!("class_ranges {:?}", self.current(),);
        while self.eat_class_atom()? {
            let left = self.state.last_int_value;
            if self.eat('-') && self.eat_class_atom()? {
                let right = self.state.last_int_value;
                if self.state.u && (left.is_none() || right.is_none()) {
                    return Err(Error::new(self.state.pos, "Invalid character class"));
                }
                if let (Some(left), Some(right)) = (left, right) {
                    if left > right {
                        return Err(Error::new(
                            self.state.pos,
                            &format!(
                                "Range out of order in character class ({} > {})",
                                left, right
                            ),
                        ));
                    }
                }
            }
        }
        Ok(())
    }
    /// Attempt to consume a single part of a class
    fn eat_class_atom(&mut self) -> Result<bool, Error> {
        trace!("eat_class_atom {:?}", self.current(),);
        let start = self.state.pos;
        if self.eat('\\') {
            if self.eat_class_escape()? {
                return Ok(true);
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
                return Ok(true);
            }
        }
        Ok(false)
    }
    /// attempt to consume an escaped part of a class
    fn eat_class_escape(&mut self) -> Result<bool, Error> {
        trace!("eat_class_escape {:?}", self.current(),);
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
        let ret = self.eat_character_class_escape()? || self.eat_character_escape()?;
        Ok(ret)
    }
    /// attempt to consume a control letter
    fn eat_class_control_letter(&mut self) -> bool {
        trace!("eat_class_control_letter {:?}", self.current(),);
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
    /// attempt to consume a `\k` group
    fn eat_k_group_name(&mut self) -> Result<bool, Error> {
        trace!("eat_k_group_name {:?}", self.current(),);
        if self.eat('k') {
            if self.eat_group_name()? {
                if let Some(name) = self.state.last_string_value {
                    self.state.back_ref_names.push(name);
                    return Ok(true);
                }
            }
            return Err(Error::new(self.state.pos, "Invalid named reference"));
        }
        Ok(false)
    }
    /// attempt to consume a named group
    fn eat_group_name(&mut self) -> Result<bool, Error> {
        trace!("eat_group_name {:?}", self.current(),);
        self.state.last_string_value = None;
        if self.eat('<') {
            if self.eat_regex_identifier_name()? && self.eat('>') {
                return Ok(true);
            }
            return Err(Error::new(self.state.pos, "Invalid capture group name"));
        }
        Ok(false)
    }
    /// Attempt to consume an identifier name
    fn eat_regex_identifier_name(&mut self) -> Result<bool, Error> {
        trace!("eat_regex_identifier_name {:?}", self.current(),);
        let start = self.state.pos;
        self.state.last_string_value = None;
        if self.eat_ident_start()? {
            while self.eat_ident_part()? {}
            self.state.last_string_value = Some(&self.pattern[start..self.state.pos]);
            return Ok(true);
        }
        Ok(false)
    }
    /// attempt to consume an identifer start
    fn eat_ident_start(&mut self) -> Result<bool, Error> {
        trace!("eat_ident_start {:?}", self.current(),);
        let start = self.state.pos;
        self.state.last_string_value = None;
        let mut ch = if let Some(ch) = self.chars.peek() {
            *ch
        } else {
            return Ok(false);
        };
        self.advance();
        if ch == '\\' && self.eat_unicode_escape_sequence()? {
            if let Some(n) = self.state.last_int_value {
                if let Some(n) = std::char::from_u32(n) {
                    ch = n;
                }
            }
        }
        if Self::is_id_start(ch) {
            self.state.last_int_value = Some(ch.into());
            return Ok(true);
        }
        self.reset_to(start);
        Ok(false)
    }

    fn eat_ident_part(&mut self) -> Result<bool, Error> {
        trace!("eat_ident_part {:?}", self.current(),);
        let start = self.state.pos;
        let mut ch = if let Some(ch) = self.chars.peek() {
            *ch
        } else {
            return Ok(false);
        };
        self.advance();
        if ch == '\\' && self.eat_unicode_escape_sequence()? {
            if let Some(n) = self.state.last_int_value {
                if let Some(n) = std::char::from_u32(n) {
                    ch = n;
                }
            }
        }
        if Self::is_id_continue(ch) {
            self.state.last_int_value = Some(ch.into());
            return Ok(true);
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
        trace!("eat_uncapturing_group {:?}", self.current(),);
        let start = self.state.pos;
        if self.eat('(') {
            if self.eat('?') && self.eat(':') {
                self.disjunction()?;
                if self.eat(')') {
                    return Ok(true);
                }
                return Err(Error::new(start, "Unterminated group"));
            }
            self.reset_to(start)
        }
        Ok(false)
    }

    fn eat_capturing_group(&mut self) -> Result<bool, Error> {
        trace!("eat_capturing_group {:?}", self.current(),);
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
        trace!("group_specifier {:?}", self.current(),);
        if self.eat('?') {
            if self.eat_group_name()? {
                if let Some(name) = self.state.last_string_value {
                    if self.state.group_names.contains(&name) {
                        return Err(Error::new(self.state.pos, "Duplicate capture group name"));
                    } else {
                        self.state.group_names.push(name);
                        return Ok(())
                    }
                }
            }
            return Err(Error::new(self.state.pos, "Invalid group"));
        }
        Ok(())
    }

    fn eat_assertion(&mut self) -> Result<bool, Error> {
        trace!("eat_assertion {:?}", self.current(),);
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
        trace!("eat_digits {:?}", self.current(),);
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
        if let Some(next) = self.chars.peek() {
            if *next == ch {
                self.advance();
                return true;
            }
        }
        false
    }

    fn advance(&mut self) {
        if let Some(ch) = self.chars.next() {
            self.state.pos += ch.len_utf8();
        }
    }

    fn reset_to(&mut self, idx: usize) {
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
        run_test("/asdf|fdsa/g").unwrap();
    }
    #[test]
    #[should_panic = "Invalid escape"]
    fn decimal_escape_with_u() {
        run_test(r"/\1/u").unwrap()
    }

    #[test]
    #[should_panic = "invalid flag"]
    fn invalid_regex_flag() {
        run_test("/./G").unwrap();
    }

    #[test]
    #[should_panic = "Nothing to repeat"]
    fn bad_look_behind() {
        run_test(r"/.(?<=.)?/").unwrap();
    }

    #[test]
    #[should_panic]
    fn bad_quant() {
        run_test(r"/{2}/").unwrap();
    }

    #[test]
    #[should_panic]
    fn id_continue_u() {
        run_test(r"/\M/u").unwrap();
    }

    #[test]
    #[should_panic]
    fn cant_start_with_star() {
        run_test("/*/").unwrap();
    }

    #[test]
    fn unicode_name_and_value() {
        for value in unicode_tables::general_category::GC {
            run_test(&format!(r"/\p{{General_Category={}}}/u", value))
                .expect(&format!("failed at General_category={}", value));
            run_test(&format!(r"/\p{{gc={}}}/u", value)).expect(&format!("failed at gc={}", value));
        }
        for value in unicode_tables::script_values::SCRIPT {
            run_test(&format!(r"/\p{{Script={}}}/u", value))
                .expect(&format!("failed at Script={}", value));
            run_test(&format!(r"/\p{{sc={}}}/u", value)).expect(&format!("failed at sc={}", value));
            run_test(&format!(r"/\p{{Script_Extensions={}}}/u", value))
                .expect(&format!("failed at Script_Extensions={}", value));
            run_test(&format!(r"/\p{{scx={}}}/u", value))
                .expect(&format!("failed at scx={}", value));
        }
    }
    #[test]
    #[should_panic]
    fn unicode_name_and_value_bad_name() {
        run_test(r"/\p{junk=Greek}/u").unwrap();
    }
    #[test]
    #[should_panic]
    fn unicode_name_and_value_bad_value() {
        run_test(r"/\p{General_Category=Geek}/u").unwrap();
    }
    #[test]
    #[should_panic]
    fn unicode_name_or_value_bad_value() {
        run_test(r"/\p{junk}/u").unwrap();
    }
    #[test]
    fn unicode_name_or_value() {
        for value in unicode_tables::GC_AND_BP {
            run_test(&format!(r"/\p{{{}}}/u", value)).unwrap();
        }
    }

    #[test]
    fn named_group() {
        run_test(r"/(?<x>a)|b/").unwrap();
    }

    fn run_test(regex: &str) -> Result<(), Error> {
        let _ = pretty_env_logger::try_init();
        let mut parser = RegexParser::new(regex)?;
        parser.validate()?;
        Ok(())
    }
}
