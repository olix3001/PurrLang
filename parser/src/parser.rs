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
            Comptime | Match | Module | Break | Continue => "keyword",

            NumberLit | HexNumberLit => "number literal",
            StringLit => "string literal",
            True | False => "boolean literal",

            Void | Number | Text => "type"
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

    fn previous(&mut self) -> Option<Token> {
        if self.index < 1 { return None; }
        self.index -= 1;
        self.next()
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
            expect(tokens, notes, Token::LCurly)?;
            let body = parse_statements_until(tokens, notes, Token::RCurly)?;
            ast::StatementKind::Trigger(ast::Trigger {
                name, body
            })
        }

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
    parse_primary_expression(tokens, notes)
}

pub fn parse_primary_expression(
    tokens: &mut Tokens,
    notes: &mut ParseNotes
) -> Result<ast::Expression, SyntaxError> {
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
    
    // Path expression
    let path = parse_path(tokens, notes)?;
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
        Some(Token::Ident) => { // Path variant
            tokens.back();
            let path = parse_path(tokens, notes)?;
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
    notes: &mut ParseNotes
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
) -> Result<(), SyntaxError> {
    todo!("Implement block syntax")
    /*
    * block move(n: number) "motion_movesteps" stack {
    *     inputs: .{ STEPS: n },
    *     fields: .{}
    * }
    */
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
            ".{
                a: lorem,
                b: ipsum
            }".to_string(),
            PurrSource::Unknown
        ).unwrap(); // If It does not panic then should be fine
    }

    #[test]
    fn parse_trigger() {
        parse_purr(
            "@green_flag {
                let a: number;
                let b: number = a;
            }".to_string(),
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
}
