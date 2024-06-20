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
    BlockDefinition(BlockDefinition),

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
    pub body: Vec<Statement>
}

#[derive(Debug, Clone, PartialEq)]
pub struct BlockDefinition {
    pub name: String,
    pub opcode: String,
    pub fields: Vec<ValueField>
}

#[derive(Debug, Clone, PartialEq)]
pub struct ValueField {
    pub name: String,
    pub value: Expression,
    pub pos: FileRange
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeField {
    pub name: String,
    pub ty: Ty,
    pub pos: FileRange
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expression {
    pub kind: ExpressionKind,
    pub pos: FileRange,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExpressionKind {
    AnonStruct(Vec<ValueField>),
    Path(PurrPath),

    Unary(UnaryOp, Box<Expression>),
    Binary(Box<Expression>, BinaryOp, Box<Expression>),
    Paren(Box<Expression>),

    Field(Box<Expression>, String),
    Call(Box<Expression>, Vec<Expression>),

    Number(f64),
    String(String),
    Bool(bool)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Neg,
    Not
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    Add, Sub, Mul, Div,
    Mod, Pow, And, Or,
    Eq, Ne, Gt, Ge,
    Lt, Le
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
    Ptr,

    Number,
    Text,

    AnonStruct(Vec<TypeField>),
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
