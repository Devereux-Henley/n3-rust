//! N3-rust implementations of the RDF/JS core data types
//! See http://rdf.js.org/data-model-spec/#data-interfaces

use core::convert::TryFrom;
use super::iri;
use heapless::String;
use heapless::consts::{U0, U32};

pub struct NamedNode;
pub struct Literal;
pub struct BlankNode;
pub struct Variable;

/// An RDF Term, that may be of the kinds defined in [`TermType`].
#[derive(Clone)]
pub struct Term<T, S> where S: heapless::ArrayLength<u8> {
    /// The identifier for the given term.
    pub id: String<S>,

    /// The kind of the given term. This determines how `id` and `value` are constructed.
    pub kind: T,

    /// The constructed value of the term. Parsed from `id` based on the `kind`.
    pub value: String<S>,

    /// Hidden secret compilation unit. Prevents external manual construction of Term, forcing external users to go through the [`Term::new`] constructor.
    _secret: ()
}

impl<S> Term<Literal, S> where S: heapless::ArrayLength<u8> {
    fn data_type_string(&self) -> &str {
        let last_quote_position = self.id.rfind('"');
        match last_quote_position {
            Some(index) => {
                let seperator = self.id.chars().nth(index);

                if seperator == Some('^') {
                    &self.id[index + 2..]
                } else if seperator == Some('@') {
                    iri::xsd::STRING
                } else {
                    iri::rdf::LANG_STRING
                }
            },
            None => ""
        }
    }

    pub fn data_type(&self) -> Term<NamedNode, S> {
        named_node(self.data_type_string())
    }

    pub fn language(&self) -> &str {
        let last_quote_position = self.id.rfind('"');

        match last_quote_position {
            Some(position) => {
                if position < self.id.len() + 2 && self.id.chars().nth(position + 1).unwrap_or(' ') == '@' {
                    &self.id[position + 2..]
                } else {
                    ""
                }
            },
            None => ""
        }
    }
}

pub enum Subject<S> where S: heapless::ArrayLength<u8> {
    NamedNode(Term<NamedNode, S>),
    BlankNode(Term<BlankNode, S>),
    Variable(Term<Variable, S>)
}

pub enum Predicate<S> where S: heapless::ArrayLength<u8> {
    NamedNode(Term<NamedNode, S>),
    Variable(Term<Variable, S>)
}

pub enum Object<S> where S: heapless::ArrayLength<u8> {
    NamedNode(Term<NamedNode, S>),
    Literal(Term<Literal, S>),
    BlankNode(Term<BlankNode, S>),
    Variable(Term<Variable, S>)
}

pub enum Graph<S> where S: heapless::ArrayLength<u8> {
    DefaultGraph,
    NamedNode(Term<NamedNode, S>),
    BlankNode(Term<BlankNode, S>),
    Variable(Term<Variable, S>)
}

pub struct Quad<S, P, O, G> where S: heapless::ArrayLength<u8>, P: heapless::ArrayLength<u8>, O: heapless::ArrayLength<u8>, G: heapless::ArrayLength<u8> {
    pub subject: Subject<S>,

    pub predicate: Predicate<P>,

    pub object: Object<O>,

    pub graph: Graph<G>
}

pub enum ParsedTerm<S> where S: heapless::ArrayLength<u8> {
    NamedNode(Term<NamedNode, S>),
    Literal(Term<Literal, S>),
    BlankNode(Term<BlankNode, S>),
    Variable(Term<Variable, S>),
    DefaultGraph
}

fn find_quoted_content(value: &str) -> &str {
    if value.len() < 1 {
        return "";
    }

    let quote_index = &value[1..].find('"');

    match quote_index {
        Some(index) => &value[1..index+1],
        None => ""
    }
}

pub fn named_node<'a, S>(id: &'a str) -> Term<NamedNode, S> where S: heapless::ArrayLength<u8> {
    Term { id: String::from(id), kind: NamedNode, value: String::from(id), _secret: () }
}

pub fn boolean_literal<S>(value: bool) -> Result<Term<Literal, S>, &'static str> where S: heapless::ArrayLength<u8> {
    let mut new_id: String<S> = String::from(if value {  "\"true\"^^" } else { "\"false\"^^" });

    match new_id.push_str(iri::xsd::BOOLEAN) {
        Ok(_) => {
            let new_value = String::from(find_quoted_content(&new_id));

            Ok(Term { id: new_id, kind: Literal, value: new_value, _secret: () })
        },
        Err(_) => Err("Literal id was not allocated enough space")
    }
}

pub fn literal<'a, S>(id: &'a str) -> Term<Literal, S> where S: heapless::ArrayLength<u8> {
    Term { id: String::from(id), kind: Literal, value: String::from(find_quoted_content(id)), _secret: () }
}

pub fn blank_node<'a, S>(id: &'a str) -> Result<Term<BlankNode, S>, &'static str> where S: heapless::ArrayLength<u8> {
    static mut BLANK_NODE_COUNTER: i32 = 0;

    if id.is_empty() {
        let mut new_id: String<S> = String::from(":_n3-");

        unsafe {
            let blank_node_str: String<U32> = String::from(BLANK_NODE_COUNTER);
            BLANK_NODE_COUNTER += 1;

            return match new_id.push_str(&blank_node_str) {
                Ok(_) =>  {
                    let new_value = String::from(&new_id[2..]);

                    Ok(Term { id: new_id, kind: BlankNode, value: new_value, _secret: () })
                },
                Err(_) => Err("Blank Node id was not allocated enough space.")
            }
        }
    }

    if id.len() < 2usize {
        return Err("Blank node id must be longer than 2 characters.");
    }


    let mut new_id: String<S> = String::from(":_");

    match new_id.push_str(id) {
        Ok(_) => {
            let new_value = String::from(&new_id[2..]);

            Ok(Term { id: new_id, kind: BlankNode, value: new_value, _secret: () })
        },
        Err(_) => Err("Blank Node id was not allocated enough space.")
    }
}

pub fn variable<'a, S>(id: &'a str) -> Result<Term<Variable, S>, &'static str> where S: heapless::ArrayLength<u8> {
    let mut new_id: String<S> = String::from("?");

    match new_id.push_str(id) {
        Ok(_) => {
            let new_value = String::from(&new_id[1..]);

            Ok(Term { id: new_id, kind: Variable, value: new_value, _secret: () })
        },
        Err(_) => Err("Variable id was not allocated enough space.")
    }
}

pub fn triple<S, P, O>(subject: Subject<S>, predicate: Predicate<P>, object: Object<O>) -> Quad<S, P, O, U0>
  where S: heapless::ArrayLength<u8>, P: heapless::ArrayLength<u8>, O: heapless::ArrayLength<u8> {
    Quad { subject, predicate, object, graph: Graph::DefaultGraph }
}

pub fn quad<S, P, O, G>(subject: Subject<S>, predicate: Predicate<P>, object: Object<O>, graph: Graph<G>) -> Quad<S, P, O, G>
  where S: heapless::ArrayLength<u8>, P: heapless::ArrayLength<u8>, O: heapless::ArrayLength<u8>, G: heapless::ArrayLength<u8> {
    Quad { subject, predicate, object, graph }
}

impl<'a, S> TryFrom<&'a str> for Term<NamedNode, S> where S: heapless::ArrayLength<u8> {
    type Error = &'static str;

    fn try_from(id: &'a str) -> Result<Self, Self::Error> {
        match id.chars().nth(0usize) {
            Some(_) => Ok(named_node(id)),
            None => Err("NamedNode cannot be constructed from an empty id.")
        }
    }
}

impl<'a, S> TryFrom<&'a str> for Term<BlankNode, S> where S: heapless::ArrayLength<u8> {
    type Error = &'static str;

    fn try_from(id: &'a str) -> Result<Self, Self::Error> {
        match id.chars().nth(0usize) {
            Some('_') => blank_node(id),
            Some(_) => Err("BlankNode must begin with '_'."),
            None => Err("BlankNode cannot be constructed from an empty id.")
        }
    }
}

impl<'a, S> TryFrom<&'a str> for Term<Literal, S> where S: heapless::ArrayLength<u8> {
    type Error = &'static str;

    fn try_from(id: &'a str) -> Result<Self, Self::Error> {
        match id.chars().nth(0usize) {
            Some('?') => Ok(literal(id)),
            Some(_) => Err("Literal must begin with '?'."),
            None => Err("Literal cannot be constructed from an empty id.")
        }
    }
}

impl<'a, S> TryFrom<&'a str> for Term<Variable, S> where S: heapless::ArrayLength<u8> {
    type Error = &'static str;

    fn try_from(id: &'a str) -> Result<Self, Self::Error> {
        match id.chars().nth(0usize) {
            Some('"') => variable(id),
            Some(_) => Err("Variable must begin with '\"'."),
            None => Err("Variable cannot be constructed from an empty id.")
        }
    }
}

pub fn from_id<'a, S>(id: &'a str) -> Result<ParsedTerm<S>, &'static str> where S: heapless::ArrayLength<u8> {
    match id.chars().nth(0usize) {
        Some('_') => {
            match blank_node(id) {
                Ok(node) => Ok(ParsedTerm::BlankNode(node)),
                Err(message) => Err(message)
            }
        },
        Some('?') => {
            match variable(id) {
                Ok(node) => Ok(ParsedTerm::Variable(node)),
                Err(message) => Err(message)
            }
        },
        Some('"') => Ok(ParsedTerm::Literal(literal(id))),
        Some(_) => Ok(ParsedTerm::NamedNode(named_node(id))),
        None => Ok(ParsedTerm::DefaultGraph)
    }
}

// Tests

#[cfg(test)]
mod tests {
    use super::super::iri;
    use super::{Term, NamedNode, Literal, BlankNode, Variable};
    use heapless::consts::{U8, U16, U32, U64};

    #[test]
    fn id() {
        let named_node: Term<NamedNode, U8> = super::named_node("abc");
        let literal: Term<Literal, U16> = super::literal("\"abc\"@en-us");
        let blank_node: Result<Term<BlankNode, U8>, &'static str> = super::blank_node("abc");
        let variable: Result<Term<Variable, U8>, &'static str> = super::variable("abc");

        assert_eq!(blank_node.is_ok(), true);
        assert_eq!(variable.is_ok(), true);

        assert_eq!(named_node.id, "abc");
        assert_eq!(literal.id, "\"abc\"@en-us");
        assert_eq!(blank_node.unwrap().id, ":_abc");
        assert_eq!(variable.unwrap().id, "?abc");
    }

    #[test]
    fn value() {
        let named_node: Term<NamedNode, U8> = super::named_node("abc");
        let literal: Term<Literal, U16> = super::literal("\"abc\"@en-us");
        let blank_node: Result<Term<BlankNode, U8>, &'static str> = super::blank_node("abc");
        let variable: Result<Term<Variable, U8>, &'static str> = super::variable("abc");

        assert_eq!(blank_node.is_ok(), true);
        assert_eq!(variable.is_ok(), true);

        assert_eq!(named_node.value, "abc");
        assert_eq!(literal.value, "abc");
        assert_eq!(blank_node.unwrap().value, "abc");
        assert_eq!(variable.unwrap().value, "abc");
    }

    #[test]
    fn boolean_literal() {
        let literal: Result<Term<Literal, U64>, &'static str> = super::boolean_literal(true);

        assert_eq!(literal.is_ok(), true);

        let ok_literal = literal.unwrap();
        assert_eq!(ok_literal.id, "\"true\"^^http://www.w3.org/2001/XMLSchema#boolean");
        assert_eq!(ok_literal.value, "true");
    }

    #[test]
    fn literal_language() {
        let literal: Term<Literal, U16> = super::literal("\"abc\"@en-us");

        assert_eq!(literal.language(), "en-us");
    }

    #[test]
    fn blank_node_id() {
        let blank_node_0: Result<Term<BlankNode, U8>, &'static str> = super::blank_node("");
        let blank_node_1: Result<Term<BlankNode, U8>, &'static str> = super::blank_node("");

        assert_eq!(blank_node_0.is_ok(), true);
        assert_eq!(blank_node_1.is_ok(), true);

        let bn0 = blank_node_0.unwrap();
        let bn1 = blank_node_1.unwrap();

        assert_eq!(bn0.id, ":_n3-0");
        assert_eq!(bn1.id, ":_n3-1");

        assert_eq!(bn0.value, "n3-0");
        assert_eq!(bn1.value, "n3-1");
    }
}
