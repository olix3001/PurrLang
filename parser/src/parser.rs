use error::SyntaxError;
use logos::{Logos, Lexer};
use common::{FileRange, PurrSource};

use crate::ast;

macro_rules! expected {
    ($expected:expr, $tokens:expr, $notes:expr, $a:expr) => {
        Err(SyntaxError::ExpectedToken {
            expected: $expected,
            found: format!(
                "{}: \"{}\"",
                match $a {
                    Some(t) => t.typ(),
                    None => "EOF"
                },
                $tokens.text().unwrap()
            ),
            pos: $tokens.position().unwrap(),
            file: $notes.file.clone()
        })
    };
}

// ==< Lexer >==
#[derive(Logos, Debug, PartialEq, Copy, Clone)]
#[logos(skip r"[ \t\r\n\f]+")]
#[logos(skip r"//[^\n]*")]
pub enum Token {
    // ==< Operators >==
    #[token("|")]  Pipe,
    #[token("&")]  Ampersand,
    #[token("||")] Or,
    #[token("&&")] And,
    #[token("==")] Eq,
    #[token("!=")] Ne,
    #[token(">=")] Ge,
    #[token(">")]  Gt,
    #[token("<=")] Le,
    #[token("<")]  Lt,
    #[token("*")]  Star,
    #[token("/")]  Slash,
    #[token("%")]  Percent,
    #[token("^")]  Hat,
    #[token("+")]  Plus,
    #[token("-")]  Minus,
    #[token("!")]  Bang,
    #[token("=")]  Assign,
    #[token("+=")] AddAssign,
    #[token("-=")] SubAssign,
    #[token("*=")] MulAssign,
    #[token("/=")] DivAssign,
    #[token("%=")] ModAssign,
    #[token("++")] Inc,
    #[token("--")] Dec,
    #[token("#")]  Hash,
    #[token("@")]  At,
    
    // ==< Terminators >==
    #[token(";")]  Semi,
    #[token(":")]  Colon,
    #[token(",")]  Comma,
    #[token("{")]  LCurly,
    #[token("}")]  RCurly,
    #[token("[")]  LSquare,
    #[token("]")]  RSquare,
    #[token("(")]  LParen,
    #[token(")")]  RParen,
    #[token("::")] DColon,
    #[token(".")]  Period,
    #[token("->")] Arrow,
    #[token("=>")] ThickArrow,

    // ==< Keywords >==
    #[token("block")] Block,
    #[token("const")] Const,
    #[token("let")] Let,
    #[token("cloud")] Cloud,
    #[token("global")] Global,
    #[token("enum")] Enum,
    #[token("struct")] Struct,
    #[token("impl")] Impl,
    #[token("for")] For,
    #[token("while")] While,
    #[token("loop")] Loop,
    #[token("if")] If,
    #[token("else")] Else,
    #[token("return")] Return,
    #[token("def")] Define,
    #[token("comptime")] Comptime,
    #[token("match")] Match,
    #[token("mod")] Module,
    #[token("break")] Break,
    #[token("continue")] Continue,
    #[token("ptr")] Ptr,
    #[token("import")] Import,

    // ==< Primitive types >==
    #[token("void")] Void,
    #[token("number")] Number,
    #[token("text")] Text,

    // ==< Values >==
    #[regex(r"([a-zA-Z_][a-zA-Z0-9_]*)")]
    Ident,
    #[regex(r"[0-9][_0-9]*(\.[0-9][_0-9]*)?")]
    NumberLit,
    #[regex(r"0x[a-fA-F0-9][_a-fA-F0-9]*")]
    HexNumberLit,
    #[regex(r#"[a-z0-9]*("(?:\\.|[^\\"])*"|'(?:\\.|[^\\'])*')"#)]
    StringLit,

    #[token("true")]  True,
    #[token("false")] False,
}

impl<'src> Token {
    fn typ(&self) -> &'static str {
        use Token::*;
        match self {
            Pipe | Ampersand | Or | And | Eq | Ne | Ge | Gt | Le | Lt |
            Star | Slash | Percent | Hat | Plus | Minus | Bang | Assign |
            AddAssign | SubAssign | MulAssign | DivAssign | ModAssign |
            Inc | Dec | Hash | At => "operator",

            Ident => "identifier",
            
            Semi | Colon | Comma | LCurly | RCurly | LSquare | RSquare |
            LParen | RParen | DColon | Period | Arrow | ThickArrow
                => "terminator",

            Block | Const | Let | Cloud | Global | Enum | Struct |
            Impl | For | While | Loop | If | Else | Return | Define |
            Comptime | Match | Module | Break | Continue | Import => "keyword",

            NumberLit | HexNumberLit => "number literal",
            StringLit => "string literal",
            True | False => "boolean literal",

            Void | Number | Text | Ptr => "type",
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct ParseNotes {
    pub attributes: ast::Attributes,
    pub file: PurrSource
}

impl ParseNotes {
    pub fn new(path: PurrSource) -> Self {
        Self {
            attributes: ast::Attributes::default(),
            file: path
        }
    }
}

#[derive(Clone)]
pub struct Tokens<'src> {
    iter: Lexer<'src, Token>,
    stack: Vec<(Option<Token>, String, FileRange)>,
    index: usize
}

impl<'src> Tokens<'src> {
    fn new(iter: Lexer<'src, Token>) -> Self {
        Self {
            iter,
            stack: Vec::new(),
            index: 0
        }
    }

    fn next(&mut self) -> Option<Token> {
        if self.index == self.stack.len() {
            let next_elem = self.iter.next();
            if let Some(Err(_)) = next_elem { return None; }
            let next_elem = next_elem.map(|e| e.unwrap());

            let slice = self.iter.slice().to_string();
            let range = self.iter.span();

            self.stack.push((next_elem, slice, range));
            self.index += 1;
            next_elem
        } else {
            if self.index >= self.stack.len() {
                return None;
            }
            self.index += 1;
            self.stack[self.index - 1].0
        }
    }

    fn current(&mut self) -> Option<Token> {
        if self.index < 1 { return None; }
        if self.index-1 > self.stack.len() { return None; }
        self.stack[self.index-1].0
    }

    fn back(&mut self) -> bool {
        if self.index < 1 { return false }
        self.index -= 1;
        true
    }

    fn peek(&mut self) -> Option<Token> {
        let next = self.next();
        if next.is_some() {
            self.index -= 1;
        }
        next
    }

    fn check(&mut self, token: Token) -> bool {
        let next = self.next();
        if next != Some(token) {
            self.back();
            false
        } else { true }
    }

    fn position(&self) -> Option<FileRange> {
        if self.index < 1 { return None; }
        if self.index-1 > self.stack.len()  {
            return None;
        }
        Some(self.stack[self.index-1].2.clone())
    }

    fn text(&self) -> Option<&str> {
        if self.index < 1 { return None; }
        if self.index-1 > self.stack.len()  {
            return None;
        }
        Some(&self.stack[self.index-1].1)
    }
}

// ==< Parser >==

pub fn parse_purr(
    unparsed: String,
    source: PurrSource,
) -> Result<(Vec<ast::Statement>, ParseNotes), SyntaxError> {
    // Initialize basic structures.
    let tokens_iter = Token::lexer(&unparsed);
    let mut tokens = Tokens::new(tokens_iter);
    let mut statements = Vec::<ast::Statement>::new();
    let mut notes = ParseNotes::new(source);

    notes.attributes = parse_attributes(&mut tokens, &mut notes, true)?;

    loop {
        match tokens.peek() {
            Some(_) =>
                statements.push(
                    parse_statement(&mut tokens, &mut notes)?
                ),
            None => break,
        }
    }

    Ok((statements, notes))
}

pub fn parse_statements_until(
    tokens: &mut Tokens,
    notes: &mut ParseNotes,
    end: Token
) -> Result<Vec<ast::Statement>, SyntaxError> {
    let mut statements = Vec::<ast::Statement>::new();

    loop {
        if tokens.check(end) { break; }
        match tokens.peek() {
            Some(_) => 
                statements.push(parse_statement(tokens, notes)?),
            None => { expect(tokens, notes, end)?; },
        }
    }

    Ok(statements)
}

pub fn parse_statement(
    tokens: &mut Tokens,
    notes: &mut ParseNotes
) -> Result<ast::Statement, SyntaxError> {
    let attributes = parse_attributes(tokens, notes, false)?;

    let first = tokens.next();
    let start_pos = tokens.position().unwrap().start;
    let kind: ast::StatementKind = match first {
        Some(Token::Let) => {
            // Let definition statement.
            // This handles local variable creation.
            let name = expect_ident(tokens, notes)?;

            let ty = if tokens.check(Token::Colon) {
                parse_ty(tokens, notes)?
            } else {
                ast::Ty {
                    kind: ast::TyKind::Infer,
                    pos: 0..0
                }
            };

            let value = if tokens.check(Token::Assign) {
                Some(parse_expression(tokens, notes)?)
            } else { None };

            // This always has to be followed by semicolon.
            expect(tokens, notes, Token::Semi)?;

            ast::StatementKind::LetDefinition(
                ast::LetDefinition {
                    symbol: name,
                    ty,
                    value
                }
            )
        }

        Some(Token::At) => {
            // Triggers
            let name = expect_ident(tokens, notes)?;
            let arguments = if tokens.check(Token::LParen) {
                if !tokens.check(Token::RParen) {
                    let args = separated(tokens, notes, Token::Comma, parse_expression)?;
                    expect(tokens, notes, Token::RParen)?;
                    args
                } else { Vec::new() }
            } else { Vec::new() };
            expect(tokens, notes, Token::LCurly)?;
            let body = parse_statements_until(tokens, notes, Token::RCurly)?;
            ast::StatementKind::Trigger(ast::Trigger {
                name, body, arguments
            })
        }

        Some(Token::Block) => ast::StatementKind::BlockDefinition(
            parse_block_definition(tokens, notes)?
        ),

        Some(Token::Define) => {
            let name = expect_ident(tokens, notes)?;
            let generics = parse_optional_generics(tokens, notes)?;
            let signature = parse_signature(tokens, notes, ast::TyKind::Void)?;
            expect(tokens, notes, Token::LCurly)?;
            let body = parse_statements_until(tokens, notes, Token::RCurly)?;
            ast::StatementKind::FunctionDefinition(
                ast::FunctionDefinition {
                    name,
                    generics,
                    signature,
                    body
                }
            )
        },

        Some(Token::Struct) => {
            let name = expect_ident(tokens, notes)?;
            let generics = parse_optional_generics(tokens, notes)?;
            let fields = parse_ty_struct(tokens, notes, true)?;
            ast::StatementKind::StructDefinition(
                ast::StructDefinition {
                    name,
                    generics,
                    fields
                }
            )
        }

        Some(Token::Import) => {
            let import = ast::StatementKind::Import(
                parse_import(tokens, notes)?
            );
            expect(tokens, notes, Token::Semi)?;
            import
        },

        Some(Token::Module) => {
            let name = expect_ident(tokens, notes)?;
            expect(tokens, notes, Token::LCurly)?;
            let body = parse_statements_until(tokens, notes, Token::RCurly)?;
            ast::StatementKind::Module(ast::ModuleDefinition {
                name,
                body,
                source: notes.file.clone()
            })
        },

        Some(Token::Break) => ast::StatementKind::Break,
        Some(Token::Continue) => ast::StatementKind::Continue,

        Some(_) => {
            tokens.back();
            let expression = parse_expression(tokens, notes)?;
            if tokens.check(Token::Semi) {
                ast::StatementKind::Expr(expression)
            } else {
                ast::StatementKind::ExprNoSemi(expression)
            }
        },
        None => expected!("statement".to_string(), tokens, notes, first)?,
    };

    let end_pos = tokens.position().unwrap().end;
    Ok(ast::Statement {
        kind,
        attributes,
        pos: start_pos..end_pos,
    })
}

pub fn parse_expression(
    tokens: &mut Tokens,
    notes: &mut ParseNotes,
) -> Result<ast::Expression, SyntaxError> {
    parse_binary_expression_or(tokens, notes)
}

macro_rules! impl_binary_expressions {
    ($($name:ident use $fun:ident where $($kind:ident => $ty:ident),+);+;) => {
        $(
            pub fn $name(
                tokens: &mut Tokens,
                notes: &mut ParseNotes
            ) -> Result<ast::Expression, SyntaxError> {
                let mut lhs = $fun(tokens, notes)?;
                
                while $(tokens.check(Token::$kind))||* {
                    let op = tokens.current().unwrap();
                    let kind = match op {
                        $(Token::$kind => ast::BinaryOp::$ty),+,
                        _ => { unreachable!() }
                    };
                    let rhs = $fun(tokens, notes)?;
                    let pos = lhs.pos.start..rhs.pos.end;
                    lhs = ast::Expression {
                        kind: ast::ExpressionKind::Binary(
                            Box::new(lhs),
                            kind,
                            Box::new(rhs)
                        ),
                        pos
                    }
                }

                Ok(lhs)
            }
        )+
    };
}

impl_binary_expressions! {
    parse_binary_expression_or use parse_binary_expression_and
        where Or => Or;
    parse_binary_expression_and use parse_binary_expression_equality
        where And => And;
    parse_binary_expression_equality use parse_binary_expression_comparison 
        where Eq => Eq, Ne => Ne;
    parse_binary_expression_comparison use parse_binary_expression_term 
        where Gt => Gt, Ge => Ge, Lt => Lt, Le => Le;
    parse_binary_expression_term use parse_binary_expression_mod
        where Plus => Add, Minus => Sub;
    parse_binary_expression_mod use parse_binary_expression_factor
        where Percent => Mod;
    parse_binary_expression_factor use parse_binary_expression_pow
        where Slash => Div, Star => Mul;
    parse_binary_expression_pow use parse_unary_expression
        where Hat => Pow;
}

pub fn parse_unary_expression(
    tokens: &mut Tokens,
    notes: &mut ParseNotes
) -> Result<ast::Expression, SyntaxError> {
    if tokens.check(Token::Bang) {
        let start_pos = tokens.position().unwrap().start;
        let rhs = parse_unary_expression(tokens, notes)?;
        let end_pos = tokens.position().unwrap().end;
        Ok(ast::Expression {
            kind: ast::ExpressionKind::Unary(ast::UnaryOp::Not, Box::new(rhs)),
            pos: start_pos..end_pos
        })
    } else if tokens.check(Token::Minus) {
        let start_pos = tokens.position().unwrap().start;
        let rhs = parse_unary_expression(tokens, notes)?;
        let end_pos = tokens.position().unwrap().end;
        Ok(ast::Expression {
            kind: ast::ExpressionKind::Unary(ast::UnaryOp::Neg, Box::new(rhs)),
            pos: start_pos..end_pos
        })
    } else { parse_expression_call_or_field(tokens, notes) }
}

pub fn parse_expression_call_or_field(
    tokens: &mut Tokens,
    notes: &mut ParseNotes
) -> Result<ast::Expression, SyntaxError> {
    let mut subject = parse_primary_expression(tokens, notes)?;

    while tokens.check(Token::Period) || tokens.check(Token::LParen) {
        match tokens.current() {
            Some(Token::Period) => {
                let field_name = expect_ident(tokens, notes)?;
                let end_pos = tokens.position().unwrap().end;
                subject = ast::Expression {
                    pos: subject.pos.start..end_pos,
                    kind: ast::ExpressionKind::Field(Box::new(subject), field_name),
                };
            },
            Some(Token::LParen) => {
                let arguments = if tokens.peek() != Some(Token::RParen) {
                    separated(tokens, notes, Token::Comma, parse_expression)?
                } else { Vec::new() };
                expect(tokens, notes, Token::RParen)?;
                let end_pos = tokens.position().unwrap().end;
                subject = ast::Expression {
                    pos: subject.pos.start..end_pos,
                    kind: ast::ExpressionKind::Call(Box::new(subject), arguments)
                };
            },
            Some(_) | None => break
        }
    }

    Ok(subject)
}

pub fn parse_primary_expression(
    tokens: &mut Tokens,
    notes: &mut ParseNotes
) -> Result<ast::Expression, SyntaxError> {
    // Parenthesised expression
    if tokens.check(Token::LParen) {
        let expr = parse_expression(tokens, notes)?;
        expect(tokens, notes, Token::RParen)?;
        return Ok(expr);
    }

    // Anonymous struct
    if tokens.check(Token::Period) && tokens.check(Token::LCurly) {
        let start_pos = tokens.position().unwrap().start;
        let struct_ = parse_values_struct(tokens, notes, false)?;
        expect(tokens, notes, Token::RCurly)?;
        let end_pos = tokens.position().unwrap().start;
        return Ok(ast::Expression {
            kind: ast::ExpressionKind::AnonStruct(struct_),
            pos: start_pos..end_pos
        });
    }

    // Literals
    let literal_value = tokens.next();
    let value = match literal_value {
        Some(Token::NumberLit) =>
            Some(ast::ExpressionKind::Number(
                tokens.text().unwrap().parse().unwrap()
            )),
        Some(Token::HexNumberLit) =>
            Some(ast::ExpressionKind::Number(
                i64::from_str_radix(
                    &tokens.text().unwrap()[2..],
                    16
                ).unwrap() as _
            )),
        Some(Token::StringLit) =>
            Some(ast::ExpressionKind::String(
                text_literal_to_string(
                    tokens.text().unwrap()
                )
            )),
        Some(Token::True) => Some(ast::ExpressionKind::Bool(true)),
        Some(Token::False) => Some(ast::ExpressionKind::Bool(false)),
        Some(_) | None => { tokens.back(); None }
    };

    if let Some(value) = value {
        return Ok(ast::Expression {
            pos: tokens.position().unwrap().clone(),
            kind: value
        });
    }
    
    // Path/Struct expression
    let path = parse_path(tokens, notes, false)?;
    
    if tokens.check(Token::LCurly) {
        let fields = if !tokens.check(Token::RCurly) {
            let fields = parse_values_struct(tokens, notes, false)?;
            expect(tokens, notes, Token::RCurly)?;
            fields
        } else { Vec::new() };
        let end_pos = tokens.position().unwrap().end;

        return Ok(ast::Expression {
            pos: path.pos.start..end_pos,
            kind: ast::ExpressionKind::StructLiteral(path, fields)
        });
    }

    Ok(ast::Expression {
        pos: path.pos.clone(),
        kind: ast::ExpressionKind::Path(path),
    })
}

pub fn parse_values_struct(
    tokens: &mut Tokens,
    notes: &mut ParseNotes,
    delimiters: bool
) -> Result<Vec<ast::ValueField>, SyntaxError> {
    if delimiters { expect(tokens, notes, Token::LCurly)?; }
    let fields = separated(tokens, notes, Token::Comma, |tokens, notes| {
        let name = expect_ident(tokens, notes)?;
        let start_pos = tokens.position().unwrap().start;
        expect(tokens, notes, Token::Colon)?;
        let value = parse_expression(tokens, notes)?;
        let end_pos = tokens.position().unwrap().end;
        Ok(ast::ValueField {
            name,
            value,
            pos: start_pos..end_pos
        })
    })?;
    if delimiters { expect(tokens, notes, Token::RCurly)?; }
    Ok(fields)
}

pub fn parse_ty_struct(
    tokens: &mut Tokens,
    notes: &mut ParseNotes,
    delimiters: bool
) -> Result<Vec<ast::TypeField>, SyntaxError> {
    if delimiters { expect(tokens, notes, Token::LCurly)?; }
    let fields = separated(tokens, notes, Token::Comma, |tokens, notes| {
        let name = expect_ident(tokens, notes)?;
        let start_pos = tokens.position().unwrap().start;
        expect(tokens, notes, Token::Colon)?;
        let ty = parse_ty(tokens, notes)?;
        let end_pos = tokens.position().unwrap().end;
        Ok(ast::TypeField {
            name,
            ty,
            pos: start_pos..end_pos
        })
    })?;
    if delimiters { expect(tokens, notes, Token::RCurly)?; }
    Ok(fields)
}

pub fn parse_optional_generics(
    tokens: &mut Tokens,
    notes: &mut ParseNotes
) -> Result<Vec<ast::TypeVariable>, SyntaxError> {
    if !tokens.check(Token::Lt) { return Ok(Vec::new()); }
    if tokens.check(Token::Gt) { return Ok(Vec::new()); }

    let arguments = separated(tokens, notes, Token::Comma, |tokens, notes| {
        // TODO: Add constraints when traits are added.
        let name = expect_ident(tokens, notes)?;
        Ok(ast::TypeVariable {
            name
        })
    })?;
    expect(tokens, notes, Token::Gt)?;

    Ok(arguments)
}

pub fn parse_attributes(
    tokens: &mut Tokens,
    notes: &mut ParseNotes,
    is_top_level: bool
) -> Result<ast::Attributes, SyntaxError> {
    let mut attributes = ast::Attributes::default();

    loop {
        let next = tokens.next();
        match next {
            Some(Token::Hash) => {
                // If we're expecting top level attributes
                // expect additional bang symbol.
                if is_top_level {
                    if tokens.next() != Some(Token::Bang) {
                        tokens.back();
                        return Ok(attributes);
                    }
                }

                expect(tokens, notes, Token::LSquare)?;

                // Marker attributes only for now.
                expect(tokens, notes, Token::Ident)?;
                attributes.tags.push(ast::AttributeTag::Marker(
                    tokens.text().unwrap().to_string()
                ));
                expect(tokens, notes, Token::RSquare)?;
            },
            _ => {
                tokens.back();
                return Ok(attributes)
            }
        }
    }
}

pub fn parse_ty(
    tokens: &mut Tokens,
    notes: &mut ParseNotes,
) -> Result<ast::Ty, SyntaxError> {
    let first = tokens.next();
    let start_pos = tokens.position().unwrap().start;

    let kind: ast::TyKind = match first {
        Some(Token::Void) => ast::TyKind::Void,
        Some(Token::Number) => ast::TyKind::Number,
        Some(Token::Text) => ast::TyKind::Text,
        Some(Token::Bang) => ast::TyKind::Never,
        Some(Token::Ptr) => ast::TyKind::Ptr,
        Some(Token::Period) => ast::TyKind::AnonStruct(
            parse_ty_struct(tokens, notes, true)?
        ),
        Some(Token::Ident) => { // Path variant
            tokens.back();
            let path = parse_path(tokens, notes, false)?;
            ast::TyKind::Path(path)
        },
        found @ Some(_) => expected!("any type".to_string(), tokens, notes, found)?,
        None => expected!("any type".to_string(), tokens, notes, None::<Token>)?,
    };

    let end_pos = tokens.position().unwrap().end;
    Ok(ast::Ty {
        kind,
        pos: start_pos..end_pos
    })
}

pub fn parse_path(
    tokens: &mut Tokens,
    notes: &mut ParseNotes,
    allow_trailing_dcolon: bool
) -> Result<ast::PurrPath, SyntaxError> {
    let mut segments = Vec::<ast::PurrPathSegment>::new();

    tokens.next();
    let start_pos = tokens.position().unwrap().start;
    tokens.back();

    loop {
        let segment_name = expect_ident(tokens, notes)?;
        let start_pos = tokens.position().unwrap().start;

        // Optional generic arguments.
        let generic_args = if tokens.check(Token::Lt) {
            let start_pos = tokens.position().unwrap().start+1;
            let args = separated(
                tokens,
                notes,
                Token::Comma,
                parse_ty
            )?;
            expect(tokens, notes, Token::Gt)?;
            let end_pos = tokens.position().unwrap().end;
            Some(ast::GenericArgs {
                args,
                pos: start_pos..end_pos
            })
        } else { None };

        let end_pos = tokens.position().unwrap().end;

        segments.push(ast::PurrPathSegment {
            ident: segment_name,
            args: generic_args.map(Box::new),
            pos: start_pos..end_pos
        });

        if !tokens.check(Token::DColon) { break; }
        if allow_trailing_dcolon &&
            tokens.peek() != Some(Token::Ident)
        { break; }
    }

    let end_pos = tokens.position().unwrap().end;

    Ok(ast::PurrPath {
        segments,
        pos: start_pos..end_pos
    })
}

fn parse_block_definition(
    tokens: &mut Tokens,
    notes: &mut ParseNotes
) -> Result<ast::BlockDefinition, SyntaxError> {
    let name = expect_ident(tokens, notes)?;
    let signature = parse_signature(tokens, notes, ast::TyKind::Ptr)?;
    let opcode = expect_ident(tokens, notes)?;
    let body = parse_values_struct(tokens, notes, true)?;

    Ok(ast::BlockDefinition {
        name,
        signature,
        opcode,
        body
    })
}

fn parse_import(
    tokens: &mut Tokens,
    notes: &mut ParseNotes
) -> Result<ast::ImportTree, SyntaxError> {
    let prefix = parse_path(tokens, notes, true)?;
    let is_current_dcolon = tokens.current() == Some(Token::DColon);
    if !is_current_dcolon {
        return Ok(ast::ImportTree { prefix, kind: ast::ImportKind::Name });
    }

    if tokens.check(Token::Star) {
        return Ok(ast::ImportTree {
            prefix,
            kind: ast::ImportKind::Glob
        });
    }

    expect(tokens, notes, Token::LCurly)?;
    if tokens.check(Token::RCurly) {
        expect(tokens, notes, Token::Semi)?;
        return Ok(ast::ImportTree {
            prefix,
            kind: ast::ImportKind::Nested(Vec::new())
        });
    }

    let subimports = separated(tokens, notes, Token::Comma, parse_import)?;
    expect(tokens, notes, Token::RCurly)?;

    return Ok(ast::ImportTree {
        prefix,
        kind: ast::ImportKind::Nested(subimports)
    });
}

fn parse_signature(
    tokens: &mut Tokens,
    notes: &mut ParseNotes,
    default_return_ty: ast::TyKind
) -> Result<ast::Signature, SyntaxError> {
    expect(tokens, notes, Token::LParen)?;

    let arguments = if !tokens.check(Token::RParen) {
        let args = separated(tokens, notes, Token::Comma, |tokens, notes| {
            let name = expect_ident(tokens, notes)?;
            let start_pos = tokens.position().unwrap().start;
            expect(tokens, notes, Token::Colon)?;
            let ty = parse_ty(tokens, notes)?;
            let end_pos = tokens.position().unwrap().end;
            Ok(ast::TypeField {
                name, ty, pos: start_pos..end_pos
            })
        })?;
        expect(tokens, notes, Token::RParen)?;
        args
    } else { Vec::new() };

    let return_type = if tokens.check(Token::Arrow) {
        parse_ty(tokens, notes)?
    } else { ast::Ty { kind: default_return_ty, pos: 0..0 } };

    Ok(ast::Signature { arguments, return_type })
}

fn separated<T>(
    tokens: &mut Tokens,
    notes: &mut ParseNotes,
    separator: Token,
    parser: impl Fn(&mut Tokens, &mut ParseNotes) -> Result<T, SyntaxError>
) -> Result<Vec<T>, SyntaxError> {
    let mut result = Vec::<T>::new();

    loop {
        let value = parser(tokens, notes)?;
        result.push(value);

        if !tokens.check(separator) { break }
    }

    Ok(result)
}

fn expect(
    tokens: &mut Tokens,
    notes: &mut ParseNotes,
    token: Token
) -> Result<Token, SyntaxError> {
    let next = tokens.next();
    if next == Some(token) {
        return Ok(next.unwrap());
    }
    expected!(
        format!("{:?}", token), // TODO: Replace with textual representation.
        tokens, notes, next
    )
}

#[inline(always)]
fn expect_ident(
    tokens: &mut Tokens,
    notes: &mut ParseNotes
) -> Result<String, SyntaxError> {
    expect(tokens, notes, Token::Ident)?;
    Ok(tokens.text().unwrap().to_string())
}

fn text_literal_to_string(value: &str) -> String {
    if value.len() == 2 { return "".to_string() } // Empty string.
    // TODO: Replace escape characters
    value[1..value.len()-1].to_string()
}

#[cfg(test)]
mod tests {
    // Those tests mostly check only whether given syntax parses or not,
    // and DO NOT check whether output is correct.
    // Checking correctness of the output is on TODO list.
    use common::PurrSource;

    use crate::{ast, parser::ParseNotes};

    use super::parse_purr;

    #[test]
    fn parse_top_level_attributes() {
        let parse_result = parse_purr("
            #![hello]
            #![world]
        ".to_string(), PurrSource::Unknown);

        assert_eq!(
            parse_result.unwrap().1,
            ParseNotes {
                attributes: ast::Attributes {
                    tags: vec![
                        ast::AttributeTag::Marker("hello".to_string()),
                        ast::AttributeTag::Marker("world".to_string())
                    ]
                },
                file: PurrSource::Unknown,
            }
        )
    }

    #[test]
    fn parse_let_without_value() {
        parse_purr(
            "let hello_world: Lorem<Ipsum>::Dolor;".to_string(),
            PurrSource::Unknown
        ).unwrap(); // If It does not panic then should be fine
    }

    #[test]
    fn parse_let_with_expr() {
        parse_purr(
            "let hello: number; let world = hello;".to_string(),
            PurrSource::Unknown
        ).unwrap(); // If It does not panic then should be fine
    }

    #[test]
    fn parse_anon_struct() {
        parse_purr(
            "let hello: .{ a: number, b: text } = .{
                a: 1,
                b: \"Hello\"
            };".to_string(),
            PurrSource::Unknown
        ).unwrap(); // If It does not panic then should be fine
    }

    #[test]
    fn parse_trigger() {
        parse_purr(
            "
            @green_flag {
                let a: number;
                let b: number = a;
            }
            @key_pressed(\"space\") {
                let n = 1;
            }
            ".to_string(),
            PurrSource::Unknown
        ).unwrap(); // If It does not panic then should be fine
    }

    #[test]
    fn parse_literals() {
        parse_purr(
            "1;2;3;4.2;123.456;0xef;\"Hello world\";true;false;".to_string(),
            PurrSource::Unknown
        ).unwrap(); // If It does not panic then should be fine
    }

    #[test]
    fn parse_unary_expression() {
        parse_purr(
            "-128;!true".to_string(),
            PurrSource::Unknown
        ).unwrap(); // If It does not panic then should be fine
    }

    #[test]
    fn parse_binary_expression() {
        parse_purr(
            "1 + 4%2 - 5*6^7 > 10 && 1 == 2/2".to_string(),
            PurrSource::Unknown
        ).unwrap(); // If It does not panic then should be fine
    }

    #[test]
    fn parse_call_and_field() {
        parse_purr(
            "hello.world().my.result(lorem, ipsum)(1, 2, true)".to_string(),
            PurrSource::Unknown
        ).unwrap(); // If It does not panic then should be fine
    }

    #[test]
    fn parse_parenthesised_expression() {
        parse_purr(
            "2 * (2 + 2)".to_string(),
            PurrSource::Unknown
        ).unwrap(); // If It does not panic then should be fine
    }

    #[test]
    fn parse_block_definition() {
        parse_purr(
            "
            block add(a: number, b: number) -> number operator_add {
                inputs: .{ NUM1: a, NUM2: b }
            }
            ".to_string(),
            PurrSource::Unknown
        ).unwrap(); // If It does not panic then should be fine
    }

    #[test]
    fn parse_module_definition() {
        parse_purr(
            "
            mod hello {
                @green_flag {
                    let a = 1;
                    let b = 2;
                    let c = a + b;
                }
            }
            ".to_string(),
            PurrSource::Unknown
        ).unwrap(); // If It does not panic then should be fine
    }

    #[test]
    fn parse_import() {
        parse_purr(
            "
            import hello::world;
            import lorem::ipsum::{dolor::sit::*, amet};
            ".to_string(),
            PurrSource::Unknown
        ).unwrap(); // If It does not panic then should be fine
    }

    #[test]
    fn parse_function_definition() {
        parse_purr(
            "
            def move_to_postion(x: number, y: number) {
                goto_position(x, y);
            }
            ".to_string(),
            PurrSource::Unknown
        ).unwrap(); // If It does not panic then should be fine
    }

    #[test]
    fn parse_struct_definition() {
        parse_purr(
            "
            struct Labelled<T> {
                value: T,
                label: text
            }
            ".to_string(),
            PurrSource::Unknown
        ).unwrap(); // If It does not panic then should be fine
    }

    #[test]
    fn parse_struct_literal() {
        parse_purr(
            "
            let position = Vec2 {
                x: 1, y: 2
            };
            ".to_string(),
            PurrSource::Unknown
        ).unwrap(); // If It does not panic then should be fine
    }
}
