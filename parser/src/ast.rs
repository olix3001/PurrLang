use common::{FileRange, PurrSource};

#[derive(Debug, Clone, PartialEq)]
pub struct Statement {
    pub kind: StatementKind,
    pub pos: FileRange
}

#[derive(Debug, Clone, PartialEq)]
pub enum StatementKind {
    // Module can change source files after being inlined
    // for compilation.
    Module(Vec<Statement>, Attributes, PurrSource)
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Attributes {
    pub tags: Vec<AttributeTag>
}

#[derive(Debug, Clone, PartialEq)]
pub enum AttributeTag {
    Marker(String)
}
