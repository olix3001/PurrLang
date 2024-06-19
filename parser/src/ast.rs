use common::{FileRange, PurrSource};

#[derive(Debug, Clone, PartialEq)]
pub struct Statement {
    pub kind: StatementKind,
    pub pos: FileRange,
    pub attributes: Attributes
}

#[derive(Debug, Clone, PartialEq)]
pub enum StatementKind {
    // Module can change source files after being inlined
    // for compilation.
    Module(Vec<Statement>, Attributes, PurrSource),

    Expr(Expression),
    ExprNoSemi(Expression),

    LetDefinition(LetDefinition),
    Trigger(Trigger),

    Break,
    Continue,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LetDefinition {
    pub symbol: String,
    pub value: Option<Expression>,
    pub ty: Ty
}

#[derive(Debug, Clone, PartialEq)]
pub struct Trigger {
    pub name: String,
    pub pos: FileRange,
    pub body: Vec<Statement>
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expression {
    pub kind: ExpressionKind,
    pub pos: FileRange,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExpressionKind {

}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Attributes {
    pub tags: Vec<AttributeTag>
}

#[derive(Debug, Clone, PartialEq)]
pub enum AttributeTag {
    Marker(String)
}

#[derive(Debug, Clone, PartialEq)]
pub struct Ty {
    pub kind: TyKind,
    pub pos: FileRange,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TyKind {
    Never,
    Infer,
    Void,

    Number,
    Text,

    Path(PurrPath)
}

#[derive(Debug, Clone, PartialEq)]
pub struct PurrPath {
    pub segments: Vec<PurrPathSegment>,
    pub pos: FileRange,
}

#[derive(Debug, Clone, PartialEq)]
pub struct PurrPathSegment {
    pub ident: String,
    pub args: Option<Box<GenericArgs>>,
    pub pos: FileRange,
}

#[derive(Debug, Clone, PartialEq)]
pub struct GenericArgs {
    pub args: Vec<Ty>,
    pub pos: FileRange
}
