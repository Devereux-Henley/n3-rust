use super::iri;
use super::factory;
use core::{fmt, str};
use core::str::{Chars};
use core::iter::{FromIterator, Iterator};
use heapless::{Vec};
use heapless::consts;

/// This type represents all possible errors that can occur when deserializing Turtle data
#[derive(Debug, PartialEq)]
pub enum Error {
    /// Lexer buffer wasn't large enough to parse input data.
    FailedAllocation,

    /// Invalid format for blank node
    InvalidBlankNode,

    /// Invalid format for variable
    InvalidVariable,

    /// Invalid format for keyword
    InvalidKeyword,

    /// Invalid format for a boolean
    InvalidBoolean,

    /// Invalid number.
    InvalidNumber,

    /// Encountered end of file before closing string character was encountered.
    EofWhileParsingString,

    /// Turtle has non-whitespace trailing characters after the value.
    TrailingCharacters,

    #[doc(hidden)]
    __Extensible,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Error::FailedAllocation => "Token buffer for Lexer was not allocated enough space",
                Error::InvalidBlankNode => "Blank node was not correctly formatted.",
                Error::InvalidVariable => "Variable was not correctly formatted",
                Error::InvalidKeyword => "Keyword was not correctly formatted",
                Error::InvalidBoolean => "Invalid boolean.",
                Error::InvalidNumber => "Invalid number.",
                Error::EofWhileParsingString => "File ended before ending \" was found.",
                Error::TrailingCharacters => {
                    "Turtle has non-whitespace trailing characters after \
                     the \
                     value."
                },
                _ => "Invalid Turtle",
            }
        )
    }
}

pub(crate) struct LexerOptions {
    pub line_mode: bool,
    pub comments: bool,
    pub n3: bool,
}

struct Lexer<'b> {
    options: LexerOptions,
    iter: Chars<'b>,
}

pub(crate) struct LexerTokenData<'b> {
    pub line: u32,
    pub value: &'b str,
    pub prefix: &'b str,
    pub token_type: &'b str
}

pub(crate) enum LexerToken<'b> {
    Comment(LexerTokenData<'b>),
    EoF(LexerTokenData<'b>),
    Path(LexerTokenData<'b>),
    IRI(LexerTokenData<'b>),
    Inverse(LexerTokenData<'b>),
    Blank(LexerTokenData<'b>),
    Literal(LexerTokenData<'b>),
    Keyword(LexerTokenData<'b>),
    Variable(LexerTokenData<'b>),
    LangCode(LexerTokenData<'b>),
    Period(LexerTokenData<'b>),
    Abbreviation(LexerTokenData<'b>),
    Type(LexerTokenData<'b>),
    TypeIRI(LexerTokenData<'b>),
}

impl<'b> Lexer<'b> {
    fn new(iter: Chars<'b>) -> Lexer<'b> {
        Lexer { iter, options: LexerOptions { line_mode: false, comments: false, n3: true } }
    }

    fn new_with_options(iter: Chars<'b>, options: LexerOptions) -> Lexer<'b> {
        Lexer { iter, options }
    }

    fn parse_expected(&mut self, expected: &str) -> bool {
        let mut chars = expected.chars();
        loop {
            let next_char = chars.next();

            if next_char.is_none() {
                return true;
            }

            match (next_char, self.iter.next()) {
                (Some(v1), Some(v2)) => if v1 != v2 { return false },
                (Some(_), None) => return false,
                _ => continue
            }
        }
    }

    fn parse_bool(&mut self, val: bool) -> Result<&'b str, Error> {
        let boolean_to_match = if val { "rue" } else { "alse" };
        let matched_boolean = self.parse_expected(boolean_to_match);
        if matched_boolean {
            match self.iter.next() {
                Some('.')
                    | Some(',')
                    | Some(';')
                    | Some(' ')
                    | Some('\r')
                    | Some('\n')
                    | Some('#')
                    | Some('(')
                    | Some(')')
                    | Some('[')
                    | Some(']')
                    | Some('{')
                    | Some('}')
                    | Some('"')
                    | Some('\'')
                    | Some('<') => return Ok(if val { "true" } else { "false" }),
                _ => return Err(Error::InvalidBoolean)
            };
        } {
            return Err(Error::InvalidBoolean)
        }
    }

    fn parse_str(&mut self) -> Result<&'b str, Error> {
        let mut elements_traversed = 0;
        let slice = self.iter.as_str();
        loop {
            match self.iter.next() {
                Some('"') => {
                    return Ok(&slice[..elements_traversed]);
                }
                Some(_) => elements_traversed += 1,
                None => return Err(Error::EofWhileParsingString),
            }
        }
    }

    fn parse_whitespace(&mut self) -> Option<char> {
        loop {
            match self.iter.next() {
                Some(' ') | Some('\n') | Some('\t') | Some('\r') => continue,
                Some(character) => {
                    return Some(character);
                },
                None => return None
            }
        }
    }

    fn parse_comment(&mut self) -> Result<&'b str, Error> {
        let mut elements_traversed = 0;
        let slice = self.iter.as_str();
        loop {
            match self.iter.next() {
                Some('\n') | Some('\r') => {
                    return Ok(&slice[..elements_traversed]);
                },
                None => {
                    return Ok(&slice[..elements_traversed]);
                },
                Some(_) => elements_traversed += 1
            }
        }
    }

    fn parse_blank_node(&mut self) -> Result<&'b str, Error> {
        let mut elements_traversed = 0;
        let slice = self.iter.as_str();

        // TODO Add support for surrogate unicode point prefix. [\u{d800} ..= \u{db7f}] followed by [\u{dc00} ..= \u{dfff}]
        // Match valid starting characters.
        match self.iter.next() {
            Some('0' ..= '9')
                | Some('A' ..= 'Z')
                | Some('a' ..= 'z')
                | Some('_')
                | Some('\u{c0}' ..= '\u{d6}')
                | Some('\u{d8}' ..= '\u{f6}')
                | Some('\u{f8}' ..= '\u{02ff}')
                | Some('\u{0370}' ..= '\u{037d}')
                | Some('\u{037f}' ..= '\u{1fff}')
                | Some('\u{200c}' ..= '\u{200d}')
                | Some('\u{2070}' ..= '\u{218f}')
                | Some('\u{2c00}' ..= '\u{2fef}')
                | Some('\u{3001}' ..= '\u{d7ff}')
                | Some('\u{f900}' ..= '\u{fdcf}')
                | Some('\u{fdf0}' ..= '\u{fffd}') => elements_traversed += 1,
            _ => return Err(Error::InvalidBlankNode)
        }

        loop {
            match self.iter.next() {
                Some('\t')
                    | Some('.')
                    | Some(',')
                    | Some(';')
                    | Some(':')
                    | Some(' ')
                    | Some('\n')
                    | Some('\r')
                    | Some('#')
                    | Some('(')
                    | Some(')')
                    | Some('[')
                    | Some(']')
                    | Some('{')
                    | Some('}')
                    | Some('"')
                    | Some('\'')
                    | Some('<') => {
                        return Ok(&slice[..elements_traversed]);
                    },
                None => {
                    return Ok(&slice[..elements_traversed]);
                }
                Some('0' ..= '9')
                    | Some('A' ..= 'Z')
                    | Some('a' ..= 'z')
                    | Some('_')
                    | Some('-')
                    | Some('\u{b7}')
                    | Some('\u{c0}' ..= '\u{d6}')
                    | Some('\u{d8}' ..= '\u{f6}')
                    | Some('\u{f8}' ..= '\u{02ff}')
                    | Some('\u{0370}' ..= '\u{037d}')
                    | Some('\u{037f}' ..= '\u{1fff}')
                    | Some('\u{200c}' ..= '\u{200d}')
                    | Some('\u{2070}' ..= '\u{218f}')
                    | Some('\u{2c00}' ..= '\u{2fef}')
                    | Some('\u{3001}' ..= '\u{d7ff}')
                    | Some('\u{f900}' ..= '\u{fdcf}')
                    | Some('\u{fdf0}' ..= '\u{fffd}') => elements_traversed += 1,
                _ => return Err(Error::InvalidBlankNode)
            }
        }
    }

    fn parse_keyword(&mut self) -> Result<&'b str, Error> {
        let mut elements_traversed = 0;
        let slice = self.iter.as_str();
        loop {
            match self.iter.next() {
                Some('a' ..= 'z')
                    | Some('A' ..= 'Z') => elements_traversed += 1,
                Some(' ')
                    | Some('\r')
                    | Some('\n')
                    | Some('#')
                    | Some('<') => {
                        return Ok(&slice[..elements_traversed]);
                    }
                _ => return Err(Error::InvalidKeyword)
            }
        }
    }

    fn parse_variable(&mut self) -> Result<&'b str, Error> {
        let mut elements_traversed = 0;
        let slice = self.iter.as_str();

        // TODO Add support for surrogate unicode point prefix. [\u{d800} ..= \u{db7f}] followed by [\u{dc00} ..= \u{dfff}]
        // Match valid starting characters.
        match self.iter.next() {
            Some('A' ..= 'Z')
                | Some('a' ..= 'z')
                | Some('_')
                | Some('\u{c0}' ..= '\u{d6}')
                | Some('\u{d8}' ..= '\u{f6}')
                | Some('\u{f8}' ..= '\u{02ff}')
                | Some('\u{0370}' ..= '\u{037d}')
                | Some('\u{037f}' ..= '\u{1fff}')
                | Some('\u{200c}' ..= '\u{200d}')
                | Some('\u{2070}' ..= '\u{218f}')
                | Some('\u{2c00}' ..= '\u{2fef}')
                | Some('\u{3001}' ..= '\u{d7ff}')
                | Some('\u{f900}' ..= '\u{fdcf}')
                | Some('\u{fdf0}' ..= '\u{fffd}') => elements_traversed += 1,
            _ => return Err(Error::InvalidVariable)
        }

        loop {
            match self.iter.next() {
                Some('\t')
                    | Some('.')
                    | Some(',')
                    | Some(';')
                    | Some('!')
                    | Some('^')
                    | Some(' ')
                    | Some('\n')
                    | Some('\r')
                    | Some('#')
                    | Some('(')
                    | Some(')')
                    | Some('[')
                    | Some(']')
                    | Some('{')
                    | Some('}')
                    | Some('"')
                    | Some('\'')
                    | Some('<') => return Ok(&slice[..elements_traversed]),
                None => return Ok(&slice[..elements_traversed]),
                Some('0' ..= ':')
                    | Some('A' ..= 'Z')
                    | Some('a' ..= 'z')
                    | Some('_')
                    | Some('-')
                    | Some('\u{b7}')
                    | Some('\u{c0}' ..= '\u{d6}')
                    | Some('\u{d8}' ..= '\u{f6}')
                    | Some('\u{f8}' ..= '\u{02ff}')
                    | Some('\u{0370}' ..= '\u{037d}')
                    | Some('\u{037f}' ..= '\u{1fff}')
                    | Some('\u{200c}' ..= '\u{200d}')
                    | Some('\u{2070}' ..= '\u{218f}')
                    | Some('\u{2c00}' ..= '\u{2fef}')
                    | Some('\u{3001}' ..= '\u{d7ff}')
                    | Some('\u{f900}' ..= '\u{fdcf}')
                    | Some('\u{fdf0}' ..= '\u{fffd}') => elements_traversed += 1,
                _ => return Err(Error::InvalidVariable)
            }
        }
    }
}

impl <'b> Iterator for Lexer<'b> {
    type Item = Result<LexerToken<'b>, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        // Loop to allow fallthrough when options prevent token emission.
        loop {
            let next = self.parse_whitespace();
            match next {
                Some('#') => {
                    let comment_result = self.parse_comment();

                    if self.options.comments {
                        return Some(comment_result.map(|comment| LexerToken::Comment(LexerTokenData { prefix: "", token_type: "comment", value: comment, line: 0 })))
                    }
                },
                Some('_') => {
                    match self.iter.next() {
                        Some(':') => {
                            return Some(self.parse_blank_node().map(|node| LexerToken::Blank(LexerTokenData { prefix: "_", token_type: "blank", value: node, line: 0 })));
                        },
                        _ => return Some(Err(Error::InvalidBlankNode))
                    }
                },
                Some('@') => {
                    // TODO Add langCode support.
                    return Some(self.parse_keyword().map(|keyword| LexerToken::Keyword(LexerTokenData { prefix: "@", token_type: keyword, value: keyword, line: 0})));
                },
                Some('?') => {
                    return Some(self.parse_variable().map(|variable| LexerToken::Variable(LexerTokenData { prefix: "?", token_type: "var", value: variable, line: 0})));
                },
                Some('f') => {
                    return Some(self.parse_bool(false).map(|boolean| LexerToken::Literal(LexerTokenData { prefix: iri::xsd::BOOLEAN, token_type: "literal", value: boolean, line: 0})));
                },
                Some('t') => {
                    return Some(self.parse_bool(true).map(|boolean| LexerToken::Literal(LexerTokenData { prefix: iri::xsd::BOOLEAN, token_type: "literal", value: boolean, line: 0})));
                },
                Some(_) => return Some(Err(Error::TrailingCharacters)),
                None => return None
            }
        }
    }
}

pub(crate) fn tokenize_input<'b, S>(slice: &'b str) -> Result<heapless::Vec<LexerToken, S>, Error> where S: heapless::ArrayLength<LexerToken<'b>> {
    let indices = slice.chars();
    let lexer: Lexer<'b> = Lexer::new(indices);
    let result: Result<heapless::Vec<LexerToken, S>, Error> = lexer.collect();
    return result;
}

pub(crate) fn tokenize_input_with_options<'b, S>(slice: &'b str, options: LexerOptions) -> Result<heapless::Vec<LexerToken, S>, Error> where S: heapless::ArrayLength<LexerToken<'b>> {
    let indices = slice.chars();
    let lexer: Lexer<'b> = Lexer::new_with_options(indices, options);
    let result: Result<heapless::Vec<LexerToken, S>, Error> = lexer.collect();
    return result;
}

#[cfg(test)]
mod tests {
    use heapless::consts;

    #[test]
    fn parse_no_comment() {
        let slice = "# Hello World!";
        let vec: heapless::Vec<super::LexerToken, consts::U64> = super::tokenize_input(slice).unwrap();
        assert_eq!(vec.len(), 0);
    }

    #[test]
    fn parse_comment() {
        let slice = "# Hello World!";
        let vec: heapless::Vec<super::LexerToken, consts::U64> = super::tokenize_input_with_options(slice, super::LexerOptions { comments: true, n3: true, line_mode: false }).unwrap();
        assert_eq!(vec.len(), 1);

        let parse_result = match &vec[0] {
            super::LexerToken::Comment(data) => data,
            _ => panic!("First token was not a comment.")
        };

        assert_eq!(parse_result.value, " Hello World!")
    }

    #[test]
    fn parse_comments() {
        let slice = "# Hello \n # World!\n";
        let vec: heapless::Vec<super::LexerToken, consts::U64> = super::tokenize_input_with_options(slice, super::LexerOptions { comments: true, n3: true, line_mode: false }).unwrap();
        assert_eq!(vec.len(), 2);

        let parse_result_one = match &vec[0] {
            super::LexerToken::Comment(data) => data,
            _ => panic!("First token was not a comment.")
        };

        assert_eq!(parse_result_one.value, " Hello ");

        let parse_result_two = match &vec[1] {
            super::LexerToken::Comment(data) => data,
            _ => panic!("First token was not a comment.")
        };

        assert_eq!(parse_result_two.value, " World!");
    }

    #[test]
    fn parse_blank_node() {
        let slice = "_:test";
        let vec: heapless::Vec<super::LexerToken, consts::U64> = super::tokenize_input(slice).unwrap();

        let parse_result = match &vec[0] {
            super::LexerToken::Blank(data) => data,
            _ => panic!("First token was not a blank node.")
        };

        assert_eq!(parse_result.value, "test");
    }

    #[test]
    fn parse_keyword() {
        let slice = "@test ";
        let vec: heapless::Vec<super::LexerToken, consts::U64> = super::tokenize_input(slice).unwrap();

        let parse_result = match &vec[0] {
            super::LexerToken::Keyword(data) => data,
            _ => panic!("First token was not a keyword.")
        };

        assert_eq!(parse_result.value, "test");
    }

    #[test]
    fn parse_keyword_invalid_content() {
        let slice = "@test123 ";
        let result: Result<heapless::Vec<super::LexerToken, consts::U64>, super::Error> = super::tokenize_input(slice);

        assert!(result.is_err());
    }

    #[test]
    fn parse_variable() {
        let slice = "?test";
        let vec: heapless::Vec<super::LexerToken, consts::U64> = super::tokenize_input(slice).unwrap();

        let parse_result = match &vec[0] {
            super::LexerToken::Variable(data) => data,
            _ => panic!("First token was not a variable.")
        };

        assert_eq!(parse_result.value, "test");
    }

    #[test]
    fn parse_variable_no_content() {
        let slice = "?";
        let result: Result<heapless::Vec<super::LexerToken, consts::U64>, super::Error> = super::tokenize_input(slice);

        assert!(result.is_err());
    }

    #[test]
    fn parse_variable_invalid_content() {
        let slice = "?0";
        let result: Result<heapless::Vec<super::LexerToken, consts::U64>, super::Error> = super::tokenize_input(slice);

        assert!(result.is_err());
    }

    #[test]
    fn parse_blank_node_invalid_semicolon() {
        let slice = "_test";
        let result: Result<heapless::Vec<super::LexerToken, consts::U64>, super::Error> = super::tokenize_input(slice);

        assert!(result.is_err());
    }

    #[test]
    fn parse_blank_node_invalid_content() {
        let slice = "_:\u{b7}";
        let result: Result<heapless::Vec<super::LexerToken, consts::U64>, super::Error> = super::tokenize_input(slice);

        assert!(result.is_err());
    }

    #[test]
    fn parse_blank_node_no_content() {
        let slice_one = "_:";
        let result_one: Result<heapless::Vec<super::LexerToken, consts::U64>, super::Error> = super::tokenize_input(slice_one);

        assert!(result_one.is_err());

        let slice_two = "_";
        let result_two: Result<heapless::Vec<super::LexerToken, consts::U64>, super::Error> = super::tokenize_input(slice_two);

        assert!(result_two.is_err());
    }

    #[test]
    fn parse_boolean() {
        let slice_one = "false;";
        let result_one: heapless::Vec<super::LexerToken, consts::U64> = super::tokenize_input(slice_one).unwrap();

        let parse_result_one = match &result_one[0] {
            super::LexerToken::Literal(data) => data,
            _ => panic!("First token was not a literal")
        };

        assert_eq!(parse_result_one.value, "false");

        let slice_two = "true;";
        let result_two: heapless::Vec<super::LexerToken, consts::U64> = super::tokenize_input(slice_two).unwrap();

        let parse_result_two = match &result_two[0] {
            super::LexerToken::Literal(data) => data,
            _ => panic!("First token was not a literal")
        };

        assert_eq!(parse_result_two.value, "true");
    }
}
