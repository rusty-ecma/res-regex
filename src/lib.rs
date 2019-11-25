
use ress::prelude::RegEx;
use std::{
    iter::Peekable,
    str::Chars,
};
use log::trace;

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
            return Err(Error::new(0, "regular expression literals must start with a /"))
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
            if self.eat('[') {
                return Err(Error::new(self.state.pos, "Lone quantifier brackets"));
            }
        }
        if self.state.max_back_refs > self.state.num_capturing_parens {
            return Err(Error::new(self.state.pos, "Invalid escape"));
        }
        for name in &self.state.back_ref_names {
            if !self.state.group_names.contains(name) {
                return Err(Error::new(self.state.pos, "Invalid named capture referenced"));
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
        while self.state.pos < self.state.len && self.eat_term()? {

        }
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
                    Some(self.state.last_int_value) 
                } else { 
                    None 
                };
                if self.eat('}') {
                    if let Some(max) = max {
                        if max < min && !no_error {
                            return Err(Error::new(self.state.pos, &format!("numbers out of order in {{{},{}}}", min, max)));
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
            || self.eat_character_class()
            || self.eat_uncapturing_group()
            || self.eat_capturing_group();
        Ok(ret)
    }

    fn eat_extended_atom(&mut self) -> Result<bool, Error> {
        let ret = self.eat('.')
            || self.eat_reverse_solidus_atom_escape()?
            || self.eat_character_class()
            || self.eat_uncapturing_group()
            || self.eat_capturing_group()
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
            if *ch != '$' && !(*ch >= '(' && *ch <= '+')
                && *ch != '.'
                && *ch != '?'
                && *ch != '['
                && *ch != '^'
                && *ch != '|' {
                    self.advance();
                    return true
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
            || self.eat_character_escape()
            || self.state.n && self.eat_k_group_name() {
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
            let n = self.state.last_int_value;
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
        self.state.last_int_value = 0;
        if let Some(next) = self.chars.peek() {
            if *next >= '1' && *next <= '9' {
                let n: u32 = (*next).into();
                self.state.last_int_value = 10 * self.state.last_int_value + n;
                self.advance();
                while let Some(next) = self.chars.peek() {
                    if *next >= '1' && *next <= '9' {
                        let n: u32 = (*next).into();
                        self.state.last_int_value = 10 * self.state.last_int_value + n;
                        self.advance();
                    } else {
                        break;
                    }
                }
            }
            return true
        }
        false
    }

    fn eat_character_class_escape(&mut self) -> Result<bool, Error> {
        if let Some(next) = self.chars.peek() {
            if Self::is_character_class_escape(*next) {
                self.state.last_int_value = 0;
                self.advance();
                return Ok(true);
            }
            if self.state.u && (*next == 'P' || *next == 'p') {
                self.state.last_int_value = 0;
                self.advance();
                if self.eat('{') && self.eat_unicode_property_value_expression() && self.eat('}') {
                    return Ok(true);
                }
            }
        }
        Err(Error::new(self.state.pos, "Invalid property name"))
    }

    fn eat_unicode_property_value_expression(&mut self) -> bool {
        unimplemented!()
    }

    fn is_character_class_escape(ch: char) -> bool {
        ch == 'd'
        || ch == 'D'
        || ch == 's'
        || ch == 'S'
        || ch == 'w'
        || ch == 'W'
    }

    fn eat_character_escape(&mut self) -> bool {
        unimplemented!()
    }

    fn eat_character_class(&mut self) -> bool {
        unimplemented!()
    }

    fn eat_k_group_name(&mut self) -> bool {
        unimplemented!()
    }

    fn eat_uncapturing_group(&mut self) -> bool {
        unimplemented!()
    }

    fn eat_capturing_group(&mut self) -> bool {
        unimplemented!()
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
        Ok(false)
    }

    fn eat_digits(&mut self, radix: u32) -> bool {
        trace!("eat_digits");
        let start = self.state.pos;
        while let Some(next) = self.chars.peek() {
            if next.is_digit(radix) {
                let n: u32 = (*next).into();
                self.state.last_int_value = radix * self.state.last_int_value + n;
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
    last_int_value: u32,
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
            last_int_value: 0,
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
        self.last_int_value = 0;
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
