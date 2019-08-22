pub enum TermType {
    NamedNode,
    Literal,
    BlankNode,
    Variable,
    DefaultGraph
}

pub struct Term<'a> {
    pub id: &'a str,
    pub kind: TermType
}

fn find_quoted_content(value: &str) -> &str {
    let quote_index = value[1..].find('"');

    match quote_index {
        Some(index) => &value[1..index+1],
        None => ""
    }
}

impl<'a> Term<'a> {
    pub fn value(&self) -> &'a str {
        match &self.kind {
            TermType::NamedNode => self.id,
            TermType::Literal => find_quoted_content(self.id),
            TermType::BlankNode => &self.id[2..],
            TermType::Variable => &self.id[1..],
            TermType::DefaultGraph => self.id,
        }
    }
}

impl<'a> PartialEq for Term<'a> {
    fn eq(&self, other: &Self) -> bool {
        match (&self.kind, &other.kind) {
            (TermType::Literal, TermType::Literal) => self.id == other.id,
            (TermType::Literal, _) => false,
            _ => self.id == other.id
        }
    }
}

pub static DEFAULT_GRAPH: Term<'static> = Term {
    id: "",
    kind: TermType::DefaultGraph
};

// Tests

#[cfg(test)]
mod tests {
    #[test]
    fn value() {
        let named_node = super::Term { id: "abc", kind: super::TermType::NamedNode };
        let literal = super::Term { id: "\"abc\"123", kind: super::TermType::Literal };
        let blank_node = super::Term { id: "12abc", kind: super::TermType::BlankNode };
        let variable = super::Term { id: "1abc", kind: super::TermType::Variable };
        let default_graph = super::Term { id: "abc", kind: super::TermType::DefaultGraph };

        assert_eq!(named_node.value(), "abc");
        assert_eq!(literal.value(), "abc");
        assert_eq!(blank_node.value(), "abc");
        assert_eq!(variable.value(), "abc");
        assert_eq!(default_graph.value(), "abc");
    }

    #[test]
    fn eq() {
        let node1 = super::Term { id: "abc", kind: super::TermType::Literal };
        let node2 = super::Term { id: "abc", kind: super::TermType::Literal };
        let node3 = super::Term { id: "abc", kind: super::TermType::NamedNode };

        assert_eq!(node1 == node2, true);
        assert_eq!(node1 == node3, false);
    }
}
