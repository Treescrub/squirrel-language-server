use core::fmt;
use std::{str::Chars, iter::Peekable};

#[derive(Debug)]
pub enum TokenType {
    Identifier(String),
    StringLiteral(String),
    IntegerLiteral(u32),
    FloatLiteral(f32),
    Class,                  // class
    Base,                   // base
    Extends,                // extends
    Constructor,            // constructor
    Instanceof,             // instanceof
    Static,                 // static
    This,                   // this
    Function,               // function
    LeftCurly,              // {
    RightCurly,             // }
    LeftSquare,             // [
    RightSquare,            // ]
    Delete,                 // delete
    Switch,                 // switch
    Arrow,                  // <-
    If,                     // if
    Else,                   // else
    While,                  // while
    Break,                  // break
    For,                    // for
    Do,                     // do
    Null,                   // null
    Foreach,                // foreach
    In,                     // in
    Newslot,                // newslot
    Local,                  // local
    Clone,                  // clone
    Return,                 // return
    Typeof,                 // typeof
    Equal,                  // ==
    NotEqual,               // !=
    LessOrEqual,            // <=
    GreaterOrEqual,         // >=
    ThreeWayCompare,        // <=>
    LogicalAnd,             // &&
    LogicalOr,              // ||
    BitwiseAnd,             // &
    BitwiseOr,              // |
    BitwiseXor,             // ^
    Modulo,                 // %
    Multiply,               // *
    Divide,                 // /
    Plus,                   // +
    Minus,                  // -
    PlusEqual,              // +=
    MinusEqual,             // -=
    MultiplyEqual,          // *=
    DivideEqual,            // /=
    ModuloEqual,            // %=
    AndEqual,               // &=
    OrEqual,                // |=
    XorEqual,               // ^=
    ShiftLeft,              // <<
    ShiftRight,             // >>
    PlusPlus,               // ++
    MinusMinus,             // --
    Continue,               // continue
    Yield,                  // yield
    Try,                    // try
    Catch,                  // catch
    Throw,                  // throw
    Resume,                 // resume
    DoubleColon,            // ::
    Case,                   // case
    Default,                // default
    UnsignedShiftRight,     // >>>
    Varargs,                // ...
    LineInfo,               // __LINE__
    FileInfo,               // __FILE__
    True,                   // true
    False,                  // false
    AttributeOpen,          // </
    AttributeClose,         // />
    Enum,                   // enum
    Const,                  // const
    Rawcall,                // rawcall
    Dot,                    // .
    Invalid                 // (invalid token)
}

#[derive(Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub value: String,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.token_type)
    }
}

pub struct Lexer<'a> {
    iter: Peekable<Chars<'a>>,
    cur_char: char,
    cur_token_value: String,
    pub tokens: Vec<Token/*<'a>*/>,
}

impl<'a,'b> Lexer<'b> {
    pub fn new(data: &'a str) -> Self where 'a: 'b {
        Self {
            iter: data.chars().peekable(),
            cur_char: '\0',
            cur_token_value: String::from(""),
            tokens: Vec::<Token>::new()
        }
    }

    pub fn start_token(&mut self) {
        self.cur_token_value = String::from("");
    }

    pub fn end_token(&mut self, token_type: TokenType) {
        self.tokens.push(Token {
            token_type,
            value: self.cur_token_value.clone(),
        });
    }

    pub fn next(&mut self) -> char {
        self.cur_token_value.push(self.cur_char);
        self.cur_char = self.iter.next().unwrap_or('\0');

        return self.cur_char;
    }

    pub fn peek(&mut self) -> char {
        return *self.iter.peek().unwrap_or(&'\0');
    }

    pub fn lex(&mut self) {
        let mut line: u32 = 1;

        self.next();

        loop {
            if self.cur_char == '\0' {
                break;
            }
            if self.cur_char == ' ' || self.cur_char == '\t' || self.cur_char == '\r' {
                continue;
            }
            if self.cur_char == '\n' {
                line += 1;
                continue;
            }

            self.start_token();
            
            match self.cur_char {
                '.' => {
                    if self.next() != '.' {
                        self.end_token(TokenType::Dot);
                        continue;
                    }
                    if self.next() != '.' {
                        self.end_token(TokenType::Invalid);
                        continue;
                    }
                    self.next();
                    self.end_token(TokenType::Varargs);
                }
                _ => {
                    self.next();
                    self.end_token(TokenType::Invalid);
                    continue;
                }
            }
        }
    }
}