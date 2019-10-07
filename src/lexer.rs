use super::iri;
use super::factory;
use core::{fmt, str};
use heapless::{Vec};
use heapless::consts;

/// This type represents all possible errors that can occur when deserializing Turtle data
#[derive(Debug, PartialEq)]
pub enum Error {
    /// EOF while parsing a list.
    EofWhileParsingList,

    /// EOF while parsing an object.
    EofWhileParsingObject,

    /// EOF while parsing a string.
    EofWhileParsingString,

    /// EOF while parsing a Turtle value.
    EofWhileParsingValue,

    /// Expected this character to be a `':'`.
    ExpectedColon,

    /// Expected this character to be either a `','` or a `']'`.
    ExpectedListCommaOrEnd,

    /// Expected this character to be either a `','` or a `'}'`.
    ExpectedObjectCommaOrEnd,

    /// Expected to parse either a `true`, `false`, or a `null`.
    ExpectedSomeIdent,

    /// Expected this character to start a Turtle value.
    ExpectedSomeValue,

    /// Lexer buffer wasn't large enough to parse input data.
    FailedAllocation,

    /// Invalid number.
    InvalidNumber,

    /// Invalid type
    InvalidType,

    /// Invalid unicode code point.
    InvalidUnicodeCodePoint,

    /// Object key is not a string.
    KeyMustBeAString,

    /// Turtle has non-whitespace trailing characters after the value.
    TrailingCharacters,

    /// Turtle has a comma after the last value in an array or map.
    TrailingComma,

    #[doc(hidden)]
    __Extensible,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Error::EofWhileParsingList => "EOF while parsing a list.",
                Error::EofWhileParsingObject => "EOF while parsing an object.",
                Error::EofWhileParsingString => "EOF while parsing a string.",
                Error::EofWhileParsingValue => "EOF while parsing a Turtle value.",
                Error::ExpectedColon => "Expected this character to be a `':'`.",
                Error::ExpectedListCommaOrEnd => {
                    "Expected this character to be either a `','` or\
                     a \
                     `']'`."
                }
                Error::ExpectedObjectCommaOrEnd => {
                    "Expected this character to be either a `','` \
                     or a \
                     `'}'`."
                }
                Error::ExpectedSomeIdent => {
                    "Expected to parse either a `true`, `false`, or a \
                     `null`."
                }
                Error::ExpectedSomeValue => "Expected this character to start a Turtle value.",
                Error::FailedAllocation => "Token buffer for Lexer was not allocated enough space",
                Error::InvalidNumber => "Invalid number.",
                Error::InvalidType => "Invalid type",
                Error::InvalidUnicodeCodePoint => "Invalid unicode code point.",
                Error::KeyMustBeAString => "Object key is not a string.",
                Error::TrailingCharacters => {
                    "Turtle has non-whitespace trailing characters after \
                     the \
                     value."
                }
                Error::TrailingComma => "Turtle has a comma after the last value in an array or map.",
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

pub(crate) struct Lexer<'b> {
    options: LexerOptions,
    slice: &'b [u8],
    index: usize,
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
    Variable(LexerTokenData<'b>),
    LangCode(LexerTokenData<'b>),
    Period(LexerTokenData<'b>),
    Abbreviation(LexerTokenData<'b>),
    Type(LexerTokenData<'b>),
    TypeIRI(LexerTokenData<'b>),
}

impl<'b> Lexer<'b> {
    fn new(slice: &'b [u8]) -> Lexer<'_> {
        Lexer { slice, index: 0, options: LexerOptions { line_mode: false, comments: false, n3: true } }
    }

    fn new_with_options(slice: &'b [u8], options: LexerOptions) -> Lexer<'_> {
        Lexer { slice, index: 0, options }
    }

    pub fn tokenize_to_end<T>(&mut self) -> Result<Vec<LexerToken<'b>, T>, Error> where T: heapless::ArrayLength<LexerToken<'b>> {
        let mut result: Vec<LexerToken, T> = Vec::new();

        loop {
            match self.parse_whitespace() {
                Some(b'#') => {
                    self.eat_char();
                    let comment = self.parse_comment()?;

                    if self.options.comments {
                        result.push(LexerToken::Comment(LexerTokenData { prefix: "", token_type: "comment", value: comment, line: 0 })).or(Err(Error::FailedAllocation))?
                    }
                },
                Some(_) => return Err(Error::TrailingCharacters),
                None => return Ok(result)
            }
        }
    }

    fn eat_char(&mut self) {
        self.index += 1;
    }

    fn end(&mut self) -> Result<(), Error> {
        match self.parse_whitespace() {
            Some(_) => Err(Error::TrailingCharacters),
            None => Ok(()),
        }
    }

    fn next_char(&mut self) -> Option<u8> {
        let character = self.slice.get(self.index);

        if character.is_some() {
            self.index += 1;
        }

        character.cloned()
    }

    fn parse_str(&mut self) -> Result<&'b str, Error> {
        let start = self.index;
        loop {
            match self.peek() {
                Some(b'"') => {
                    let end = self.index;
                    self.eat_char();
                    return str::from_utf8(&self.slice[start..end])
                        .map_err(|_| Error::InvalidUnicodeCodePoint);
                }
                Some(_) => self.eat_char(),
                None => return Err(Error::EofWhileParsingString),
            }
        }
    }

    fn parse_ident(&mut self, ident: &[u8]) -> Result<(), Error> {
        for c in ident {
            if Some(*c) != self.next_char() {
                return Err(Error::ExpectedSomeIdent);
            }
        }

        Ok(())
    }

    fn parse_whitespace(&mut self) -> Option<u8> {
        loop {
            match self.peek() {
                Some(b' ') | Some(b'\n') | Some(b'\t') | Some(b'\r') => {
                    self.eat_char();
                }
                other => {
                    return other;
                }
            }
        }
    }

    fn parse_comment(&mut self) -> Result<&'b str, Error> {
        let start = self.index;
        loop {
            match self.peek() {
                Some(b'\n') | Some(b'\r') | None => {
                    let end = self.index;
                    self.eat_char();
                    return str::from_utf8(&self.slice[start..end])
                        .map_err(|_| Error::InvalidUnicodeCodePoint);
                }
                Some(_) => self.eat_char()
            }
        }
    }

    fn peek(&mut self) -> Option<u8> {
        self.slice.get(self.index).cloned()
    }
}

#[cfg(test)]
mod tests {
    use heapless::consts;

    #[test]
    fn parse_no_comment() {
        let slice = "# Hello World!".as_bytes();
        let mut lexer = super::Lexer::new_with_options(slice, super::LexerOptions { n3: true, comments: false, line_mode: false });

        let vec: heapless::Vec<super::LexerToken, consts::U64> = lexer.tokenize_to_end().unwrap();
        assert_eq!(vec.len(), 0);
    }

    #[test]
    fn parse_comment() {
        let slice = "# Hello World!".as_bytes();
        let mut lexer = super::Lexer::new_with_options(slice, super::LexerOptions { n3: true, comments: true, line_mode: false });

        let vec: heapless::Vec<super::LexerToken, consts::U64> = lexer.tokenize_to_end().unwrap();
        assert_eq!(vec.len(), 1);

        let parse_result = match &vec[0] {
            super::LexerToken::Comment(data) => data,
            _ => panic!("First token was not a comment.")
        };

        assert_eq!(parse_result.value, " Hello World!")
    }

    #[test]
    fn parse_comments() {
        let slice = "# Hello \n # World!\n".as_bytes();
        let mut lexer = super::Lexer::new_with_options(slice, super::LexerOptions { n3: true, comments: true, line_mode: false });

        let vec: heapless::Vec<super::LexerToken, consts::U64> = lexer.tokenize_to_end().unwrap();
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
}
