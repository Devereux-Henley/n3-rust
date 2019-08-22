
pub enum Term<'a> {
    NamedNode { id: &'a str },
    Literal { id: &'a str },
    BlankNode { id: &'a str },
    Variable { id: &'a str },
    DefaultGraph { id: &'a str }
}

fn find_quoted_content(value: &str) -> &str {
    let quote_index = value[1..].find('"');

    match quote_index {
        Some(index) => &value[1..index],
        None => ""
    }
}

impl<'a> Term<'a> {
    fn value(&self) -> &'a str {
        match &self {
            Term::NamedNode { id } => id,
            Term::Literal { id } => find_quoted_content(id),
            Term::BlankNode { id } => id,
            Term::Variable { id } => id,
            Term::DefaultGraph { id } => id,
        }
    }
}

pub static DEFAULT_GRAPH: Term<'static> = Term::DefaultGraph {
    id: "",
};
