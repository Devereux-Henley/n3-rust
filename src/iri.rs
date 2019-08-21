#[macro_export]
macro_rules! RDF_IRI_PREFIX { () => { "http://www.w3.org/1999/02/22-rdf-syntax-ns#" }; }

#[macro_export]
macro_rules! XSD_IRI_PREFIX { () => { "http://www.w3.org/2001/XMLSchema#" }; }

#[macro_export]
macro_rules! SWAP_IRI_PREFIX { () => { "http://www.w3.org/2000/10/swap/" }; }

pub mod xsd {
    pub const DECIMAL: &str = concat!(XSD_IRI_PREFIX!(), "decimal");
    pub const BOOLEAN: &str = concat!(XSD_IRI_PREFIX!(), "boolean");
    pub const DOUBLE: &str = concat!(XSD_IRI_PREFIX!(), "double");
    pub const INTEGER: &str = concat!(XSD_IRI_PREFIX!(), "integer");
    pub const STRING: &str = concat!(XSD_IRI_PREFIX!(), "string");
}

pub mod rdf {
    pub const TYPE: &str = concat!(RDF_IRI_PREFIX!(), "type");
    pub const NIL: &str = concat!(RDF_IRI_PREFIX!(), "nil");
    pub const FIRST: &str = concat!(RDF_IRI_PREFIX!(), "first");
    pub const REST: &str = concat!(RDF_IRI_PREFIX!(), "rest");
    pub const LANG_STRING: &str = concat!(RDF_IRI_PREFIX!(), "langString");
}

pub mod owl {
    pub const SAME_AS: &str = "http://www.w3.org/2002/07/owl#sameAs";
}

pub mod r {
    pub const FOR_SOME: &str = concat!(SWAP_IRI_PREFIX!(), "reify#forSome");
    pub const FOR_ALL: &str = concat!(SWAP_IRI_PREFIX!(), "reify#forAll");
}

pub mod log {
    pub const IMPLIES: &str = concat!(SWAP_IRI_PREFIX!(), "log#implies");
}
