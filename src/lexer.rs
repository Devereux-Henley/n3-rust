use super::iri;
use super::factory;
use core::{fmt, str};
use core::str::{Chars, CharIndices};
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

struct Lexer<'b> {
    options: LexerOptions,
    slice: &'b str,
    iter: CharIndices<'b>,
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
    fn new(slice: &'b str, iter: CharIndices<'b>) -> Lexer<'b> {
        Lexer { slice, iter, options: LexerOptions { line_mode: false, comments: false, n3: true } }
    }

    fn new_with_options(slice: &'b str, iter: CharIndices<'b>, options: LexerOptions) -> Lexer<'b> {
        Lexer { slice, iter, options }
    }

    pub fn tokenize_to_end<T>(&mut self) -> Result<Vec<LexerToken<'b>, T>, Error> where T: heapless::ArrayLength<LexerToken<'b>> {
        let mut result: Vec<LexerToken, T> = Vec::new();

        loop {
            match self.parse_whitespace() {
                Some('#') => {
                    let comment = self.parse_comment()?;

                    if self.options.comments {
                        result.push(LexerToken::Comment(LexerTokenData { prefix: "", token_type: "comment", value: comment, line: 0 })).or(Err(Error::FailedAllocation))?
                    }
                },
                Some('_') => {
                    match self.iter.next() {
                        Some((_, ':')) => {
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

    fn parse_str(&mut self) -> Result<&'b str, Error> {
        let mut elements_traversed = 0;
        loop {
            match self.iter.next() {
                Some((end, '"')) => {
                    let start = end - elements_traversed;

                    return Ok(&self.slice[start..end]);
                }
                Some((_, _)) => elements_traversed += 1,
                None => return Err(Error::EofWhileParsingString),
            }
        }
    }

    fn parse_whitespace(&mut self) -> Option<char> {
        loop {
            match self.iter.next() {
                Some((_, ' ')) | Some((_, '\n')) | Some((_, '\t')) | Some((_, '\r')) => continue,
                Some((_, character)) => {
                    return Some(character);
                },
                None => return None
            }
        }
    }

    fn parse_comment(&mut self) -> Result<&'b str, Error> {
        let mut elements_traversed = 0;
        loop {
            match self.iter.next() {
                Some((end, '\n')) | Some((end, '\r')) => {
                    let start = end - elements_traversed;
                    return Ok(&self.slice[start..end]);
                },
                None => {
                    let end = self.slice.len();
                    let start = end - elements_traversed;
                    return Ok(&self.slice[start..end]);
                },
                Some(_) => elements_traversed += 1
            }
        }
    }

    fn parse_blank_node(&mut self) -> Result<&'b str, Error> {
        let mut elements_traversed = 0;
        loop {
            match self.iter.next() {
                    Some((end, '\t'))
                    | Some((end, '.'))
                    | Some((end, ','))
                    | Some((end, ';'))
                    | Some((end, ':'))
                    | Some((end, ' '))
                    | Some((end, '\n'))
                    | Some((end, '\r'))
                    | Some((end, '#'))
                    | Some((end, '('))
                    | Some((end, ')'))
                    | Some((end, '['))
                    | Some((end, ']'))
                    | Some((end, '{'))
                    | Some((end, '}'))
                    | Some((end, '"'))
                    | Some((end, '\''))
                    | Some((end, '<')) => {
                        let start = end - elements_traversed;

                        // If first character found was end delimiter, blank node is invalid.
                        if start == end {
                            return Err(Error::InvalidBlankNode);
                        }

                        return Ok(&self.slice[start..end]);
                    },
                None => {
                    let end = self.slice.len();
                    let start = end - elements_traversed;
                    // If first character found was end delimiter, blank node is invalid.
                    if start == end {
                        return Err(Error::InvalidBlankNode);
                    }

                    return Ok(&self.slice[start..end]);
                }
                Some(_) => elements_traversed += 1
            }
        }
    }
}

pub(crate) fn tokenize_input<'b, S>(slice: &'b str) -> Result<heapless::Vec<LexerToken, S>, Error> where S: heapless::ArrayLength<LexerToken<'b>> {
    let indices = slice.char_indices();
    let mut lexer: Lexer<'b> = Lexer::new(slice, indices);
    let result: Result<heapless::Vec<LexerToken, S>, Error> = lexer.tokenize_to_end();
    return result;
}

pub(crate) fn tokenize_input_with_options<'b, S>(slice: &'b str, options: LexerOptions) -> Result<heapless::Vec<LexerToken, S>, Error> where S: heapless::ArrayLength<LexerToken<'b>> {
    let indices = slice.char_indices();
    let mut lexer: Lexer<'b> = Lexer::new_with_options(slice, indices, options);
    let result: Result<heapless::Vec<LexerToken, S>, Error> = lexer.tokenize_to_end();
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
    fn parse_blank_node_invalid_semicolon() {
        let slice = "_test";
        let result: Result<heapless::Vec<super::LexerToken, consts::U64>, super::Error> = super::tokenize_input(slice);

        assert!(result.is_err());
    }

    #[test]
    fn parse_blank_node_invalid_content() {
        let slice = "_:";
        let result: Result<heapless::Vec<super::LexerToken, consts::U64>, super::Error> = super::tokenize_input(slice);

        assert!(result.is_err());
    }
}
