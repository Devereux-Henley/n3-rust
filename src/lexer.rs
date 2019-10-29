use super::iri;
use super::factory;
use core::{fmt, str};
use heapless::{Vec};
use heapless::consts;

/// This type represents all possible errors that can occur when deserializing Turtle data
#[derive(Debug, PartialEq)]
pub enum Error {
    /// Lexer buffer wasn't large enough to parse input data.
    FailedAllocation,

    /// Invalid format for blank node
    InvalidBlankNode,

    /// Invalid number.
    InvalidNumber,

    /// Invalid unicode code point.
    InvalidUnicodeCodePoint,

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
                Error::InvalidNumber => "Invalid number.",
                Error::InvalidUnicodeCodePoint => "Invalid unicode code point.",
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
                Some(b'_') => {
                    self.eat_char();
                    match self.peek() {
                        Some(b':') => {
                            self.eat_char();
                            let node = self.parse_blank_node()?;

                            result.push(LexerToken::Blank(LexerTokenData { prefix: "_", token_type: "blank", value: node, line: 0 })).or(Err(Error::FailedAllocation))?
                        },
                        _ => return Err(Error::InvalidBlankNode)
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

    fn parse_blank_node(&mut self) -> Result<&'b str, Error> {
        let start = self.index;
        loop {
            match self.peek() {
                None
                    | Some(b'\t')
                    | Some(b'.')
                    | Some(b',')
                    | Some(b';')
                    | Some(b':')
                    | Some(b' ')
                    | Some(b'\n')
                    | Some(b'\r')
                    | Some(b'#')
                    | Some(b'(')
                    | Some(b')')
                    | Some(b'[')
                    | Some(b']')
                    | Some(b'{')
                    | Some(b'}')
                    | Some(b'"')
                    | Some(b'\'')
                    | Some(b'<') => {
                        let end = self.index;

                        // If first character found was end delimiter, blank node is invalid.
                        if start == end {
                            return Err(Error::InvalidBlankNode);
                        }

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

    #[test]
    fn parse_blank_node() {
        let slice = "_:test".as_bytes();
        let mut lexer = super::Lexer::new(slice);

        let vec: heapless::Vec<super::LexerToken, consts::U64> = lexer.tokenize_to_end().unwrap();

        let parse_result = match &vec[0] {
            super::LexerToken::Blank(data) => data,
            _ => panic!("First token was not a blank node.")
        };

        assert_eq!(parse_result.value, "test");
    }

    #[test]
    fn parse_blank_node_invalid_semicolon() {
        let slice = "_test".as_bytes();
        let mut lexer = super::Lexer::new(slice);

        let result: Result<heapless::Vec<super::LexerToken, consts::U64>, super::Error> = lexer.tokenize_to_end();

        assert!(result.is_err());
    }

    #[test]
    fn parse_blank_node_invalid_content() {
        let slice = "_:".as_bytes();
        let mut lexer = super::Lexer::new(slice);

        let result: Result<heapless::Vec<super::LexerToken, consts::U64>, super::Error> = lexer.tokenize_to_end();

        assert!(result.is_err());
    }
}
