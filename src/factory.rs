use super::iri;
use heapless::String;
use heapless::consts::U8;
use uuid::Uuid;

pub enum TermType {
    NamedNode,
    Literal,
    BlankNode,
    Variable,
    DefaultGraph
}

pub struct Term<S> where S: heapless::ArrayLength<u8> {
    pub id: String<S>,
    pub kind: TermType,
    pub value: String<S>,
    _secret: ()
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

impl<'a, S> Term<S> where S: heapless::ArrayLength<u8> {
    pub fn new(id: &'a str, kind: TermType) -> Term<S> {
        match kind {
            TermType::NamedNode => Term { id: String::from(id), kind: kind, value: String::from(id), _secret: () },
            TermType::Literal => Term { id: String::from(id), kind: kind, value: String::from(find_quoted_content(id)), _secret: () },
            TermType::BlankNode => Term { id: String::from(id), kind: kind, value: String::from(&id[2..]), _secret: () },
            TermType::Variable => Term { id: String::from(id), kind: kind, value: String::from(&id[1..]), _secret: () },
            TermType::DefaultGraph => Term { id: String::from(id), kind: kind, value: String::from(id), _secret: () },
        }
    }

    pub fn named_node(iri: &'a str) -> Term<S> {
        Term::new(iri, TermType::NamedNode)
    }
}

impl<S> PartialEq for Term<S> where S: heapless::ArrayLength<u8> {
    fn eq(&self, other: &Self) -> bool {
        match (&self.kind, &other.kind) {
            (TermType::Literal, TermType::Literal) => self.id == other.id,
            (TermType::Literal, _) => false,
            _ => self.id == other.id
        }
    }
}

pub fn get_literal_data_type_string<'a>(id: &'a str) -> &'a str {
    let last_quote_position = id.rfind('"');
    match last_quote_position {
        Some(index) => {
            let seperator = id.chars().nth(index);

            if seperator == Some('^') {
                &id[index + 2..]
            } else if seperator == Some('@') {
                iri::xsd::STRING
            } else {
                iri::rdf::LANG_STRING
            }
        },
        None => ""
    }
}

pub fn get_literal_data_type<'a, S>(id: &'a str) -> Term<S> where S: heapless::ArrayLength<u8> {
    Term::new(get_literal_data_type_string(id), TermType::NamedNode)
}

pub fn from_id<'a, S>(id: &'a str) -> Term<S> where S: heapless::ArrayLength<u8> {
    match id.chars().nth(0usize) {
        Some('_') => Term::new(id, TermType::BlankNode),
        Some('?') => Term::new(id, TermType::Variable),
        Some('"') => Term::new(id, TermType::Literal),
        Some(_) => Term::new(id, TermType::NamedNode),
        None => Term::new("", TermType::DefaultGraph)
    }
}

pub static DEFAULT_GRAPH: Term<U8> = Term { id: String(heapless::i::String::new()), value: String(heapless::i::String::new()), kind: TermType::DefaultGraph, _secret: () };

// Tests

#[cfg(test)]
mod tests {
    use super::{Term, TermType, DEFAULT_GRAPH};
    use heapless::consts::{U8, U16};

    #[test]
    fn value() {
        let named_node: Term<U8> = Term::new("abc", TermType::NamedNode);
        let literal: Term<U16> = Term::new("\"abc\"@123", TermType::Literal);
        let blank_node: Term<U8> = Term::new("12abc", TermType::BlankNode);
        let variable: Term<U8> = Term::new("1abc", TermType::Variable);
        let default_graph: Term<U8> = Term::new("abc", TermType::DefaultGraph);

        assert_eq!(named_node.value, "abc");
        assert_eq!(literal.value, "abc");
        assert_eq!(blank_node.value, "abc");
        assert_eq!(variable.value, "abc");
        assert_eq!(default_graph.value, "abc");
    }

    #[test]
    fn eq() {
        let node1: Term<U8> = Term::new("abc", TermType::Literal);
        let node2: Term<U8> = Term::new("abc", TermType::Literal);
        let node3: Term<U8> = Term::new("abc", TermType::NamedNode);

        assert_eq!(node1 == node2, true);
        assert_eq!(node1 == node3, false);
    }

    #[test]
    fn default_graph_exists() {
        assert_eq!(DEFAULT_GRAPH.id, "");
    }
}
