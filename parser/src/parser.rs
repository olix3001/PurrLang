use error::SyntaxError;
use logos::{Logos, Lexer};
use common::PurrSource;

use crate::ast;

// ==< Lexer >==

#[derive(Logos, Debug, PartialEq, Copy, Clone)]
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

    // ==< Values >==
    #[regex(r"([a-zA-Z_][a-zA-Z0-9_]*)")]
    Ident,
    #[regex(r"[0-9][_0-9]*(\.[0-9][_0-9]*)?")]
    Number,
    #[regex(r"0x[a-fA-F0-9][_a-fA-F0-9]*")]
    HexNumber,
    #[regex(r#"[a-z0-9]*("(?:\\.|[^\\"])*"|'(?:\\.|[^\\'])*')"#)]
    StringLiteral,

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
            Comptime | Match => "keyword",

            Number | HexNumber => "number literal",
            StringLiteral => "string literal",
            True | False => "boolean literal",
        }
    }
}

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
    stack: Vec<(Option<Token>, String, core::ops::Range<usize>)>,
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

    fn _next(&mut self) -> Option<Token> {
        if self.index == 0 {
            let next_elem = self.iter.next();
            if let Some(Err(_)) = next_elem { return None; }
            let next_elem = next_elem.map(|e| e.unwrap());

            let slice = self.iter.slice().to_string();
            let range = self.iter.span();

            self.stack.push((next_elem, slice, range));
            next_elem
        } else {
            self.index -= 1;
            self.stack[self.stack.len() - self.index - 1].0
        }
    }

    fn next(&mut self, skip_separator: bool) -> Option<Token> {
        let next_elem = self._next();

        if !skip_separator && next_elem == Some(Token::Semi) {
            self.next(skip_separator)
        } else {
            next_elem
        }
    }

    fn _previous(&mut self, skip_separator: bool) -> Option<Token> {
        self.index += 1;
        let len = self.stack.len();
        if len < self.index { return None }

        self.next(skip_separator)
    }

    fn previous(&mut self) -> Option<Token> { self._previous(false) }

    fn current(&self) -> Option<Token> {
        let len = self.stack.len();
        if len == 0 || len - self.index < 1 {
            None
        } else {
            self.stack[len - self.index - 1].0
        }
    }

    fn position(&self) -> core::ops::Range<usize> {
        if self.stack.len() - self.index == 0 {
            return 0..0;
        }
        self.stack[self.stack.len() - self.index - 1].2.clone()
    }
}

pub fn parse_purr(
    mut unparsed: String,
    source: PurrSource,
) -> Result<(Vec<ast::Statement>, ParseNotes), SyntaxError> {
    todo!("Parser is not yet implemented")
}
