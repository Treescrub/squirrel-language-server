use core::fmt;
use std::{str::Chars, iter::Peekable, collections::HashMap};

#[derive(Copy, Clone, Debug)]
pub enum TokenType {
    Identifier,
    StringLiteral,
    IntegerLiteral,
    FloatLiteral,
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
    LeftParen,              // (
    RightParen,             // )
    Delete,                 // delete
    Switch,                 // switch
    If,                     // if
    Else,                   // else
    While,                  // while
    Break,                  // break
    For,                    // for
    Do,                     // do
    Null,                   // null
    Foreach,                // foreach
    In,                     // in
    Newslot,                // <-
    Local,                  // local
    Clone,                  // clone
    Return,                 // return
    Typeof,                 // typeof
    Assign,                 // =
    Equal,                  // ==
    NotEqual,               // !=
    LessThan,               // <
    LessOrEqual,            // <=
    GreaterOrEqual,         // >=
    GreaterThan,            // >
    ThreeWayCompare,        // <=>
    LogicalNot,             // !
    LogicalAnd,             // &&
    LogicalOr,              // ||
    BitwiseAnd,             // &
    BitwiseOr,              // |
    BitwiseXor,             // ^
    BitwiseNot,             // ~
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
    BitwiseNotEqual,        // ~=
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
    Ternary,                // ?
    Colon,                  // :
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
        write!(f, "{:?} ({:?})", self.token_type, self.value)
    }
}

pub struct Lexer<'a> {
    iter: Peekable<Chars<'a>>,
    cur_char: char,
    cur_token_value: String,
    pub tokens: Vec<Token>,
    keywords: HashMap<String, TokenType>,
}

impl<'a,'b> Lexer<'b> {
    pub fn new(data: &'a str) -> Self where 'a: 'b {
        Self {
            iter: data.chars().peekable(),
            cur_char: '\0',
            cur_token_value: String::from(""),
            tokens: Vec::<Token>::new(),
            keywords: Self::init_keywords(),
        }
    }

    pub fn init_keywords() -> HashMap<String, TokenType> {
        return HashMap::from([
            (String::from("while"), TokenType::While),
            (String::from("do"), TokenType::Do),
            (String::from("if"), TokenType::If),
            (String::from("else"), TokenType::Else),
            (String::from("break"), TokenType::Break),
            (String::from("continue"), TokenType::Continue),
            (String::from("return"), TokenType::Return),
            (String::from("null"), TokenType::Null),
            (String::from("function"), TokenType::Function),
            (String::from("local"), TokenType::Local),
            (String::from("for"), TokenType::For),
            (String::from("foreach"), TokenType::Foreach),
            (String::from("in"), TokenType::In),
            (String::from("typeof"), TokenType::Typeof),
            (String::from("base"), TokenType::Base),
            (String::from("delete"), TokenType::Delete),
            (String::from("try"), TokenType::Try),
            (String::from("catch"), TokenType::Catch),
            (String::from("throw"), TokenType::Throw),
            (String::from("clone"), TokenType::Clone),
            (String::from("yield"), TokenType::Yield),
            (String::from("resume"), TokenType::Resume),
            (String::from("switch"), TokenType::Switch),
            (String::from("case"), TokenType::Case),
            (String::from("default"), TokenType::Default),
            (String::from("this"), TokenType::This),
            (String::from("class"), TokenType::Class),
            (String::from("extends"), TokenType::Extends),
            (String::from("constructor"), TokenType::Constructor),
            (String::from("instanceof"), TokenType::Instanceof),
            (String::from("true"), TokenType::True),
            (String::from("false"), TokenType::False),
            (String::from("static"), TokenType::Static),
            (String::from("enum"), TokenType::Enum),
            (String::from("const"), TokenType::Const),
            (String::from("__LINE__"), TokenType::LineInfo),
            (String::from("__FILE__"), TokenType::FileInfo),
            (String::from("rawcall"), TokenType::Rawcall),
        ]);
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
                self.next();
                continue;
            }
            if self.cur_char == '\n' {
                self.next();
                line += 1;
                continue;
            }

            self.start_token();
            
            match self.cur_char {
                'a'..='z' | 'A'..='Z' => {
                    // lex identifier/keyword
                    loop {
                        self.next();
                        match self.cur_char {
                            'a'..='z' | 'A'..='Z' | '_' | '1'..='9' => (),
                            _ => {
                                if self.keywords.contains_key(&self.cur_token_value) {
                                    self.end_token(self.keywords[&self.cur_token_value]);
                                } else {
                                    self.end_token(TokenType::Identifier);
                                }
                                break;
                            }
                        }
                    }
                }
                '\'' | '"' => {
                    // lex string
                    let delimiter = self.cur_char;
                    loop {
                        self.next();
                        if self.cur_char == delimiter {
                            self.next();
                            self.end_token(TokenType::StringLiteral);
                            break;
                        }
                        if self.cur_char == '\0' {
                            self.end_token(TokenType::Invalid);
                            break;
                        }
                    }
                }
                '=' => {
                    if self.next() != '=' {
                        self.end_token(TokenType::Assign);
                    } else {
                        self.next();
                        self.end_token(TokenType::Equal);
                    }
                }
                '!' => {
                    if self.next() != '=' {
                        self.end_token(TokenType::LogicalNot);
                    } else {
                        self.next();
                        self.end_token(TokenType::NotEqual);
                    }
                }
                '*' => {
                    match self.next() {
                        '=' => {
                            self.next();
                            self.end_token(TokenType::MultiplyEqual);
                        }
                        _ => {
                            self.end_token(TokenType::Multiply);
                        }
                    }
                }
                '/' => {
                    match self.next() {
                        '=' => {
                            self.next();
                            self.end_token(TokenType::DivideEqual);
                        }
                        '>' => {
                            self.next();
                            self.end_token(TokenType::AttributeClose);
                        }
                        _ => {
                            self.end_token(TokenType::Divide);
                        }
                    }
                }
                '%' => {
                    match self.next() {
                        '=' => {
                            self.next();
                            self.end_token(TokenType::ModuloEqual);
                        }
                        _ => {
                            self.end_token(TokenType::Modulo);
                        }
                    }
                }
                '&' => {
                    match self.next() {
                        '=' => {
                            self.next();
                            self.end_token(TokenType::AndEqual);
                        }
                        '&' => {
                            self.next();
                            self.end_token(TokenType::LogicalAnd);
                        }
                        _ => {
                            self.end_token(TokenType::BitwiseAnd);
                        }
                    }
                }
                '|' => {
                    match self.next() {
                        '=' => {
                            self.next();
                            self.end_token(TokenType::OrEqual);
                        }
                        '|' => {
                            self.next();
                            self.end_token(TokenType::LogicalOr);
                        }
                        _ => {
                            self.end_token(TokenType::BitwiseOr);
                        }
                    }
                }
                '^' => {
                    match self.next() {
                        '=' => {
                            self.next();
                            self.end_token(TokenType::XorEqual);
                        }
                        _ => {
                            self.end_token(TokenType::BitwiseXor);
                        }
                    }
                }
                '~' => {
                    match self.next() {
                        '=' => {
                            self.next();
                            self.end_token(TokenType::BitwiseNotEqual);
                        }
                        _ => {
                            self.end_token(TokenType::BitwiseNot);
                        }
                    }
                }
                '<' => {
                    match self.next() {
                        '=' => {
                            if self.next() == '>' {
                                self.end_token(TokenType::ThreeWayCompare);
                            } else {
                                self.end_token(TokenType::LessOrEqual);
                            }
                        }
                        '-' => {
                            self.next();
                            self.end_token(TokenType::Newslot);
                        }
                        '<' => {
                            self.next();
                            self.end_token(TokenType::ShiftLeft);
                        }
                        '/' => {
                            self.next();
                            self.end_token(TokenType::AttributeOpen);
                        }
                        _ => {
                            self.next();
                            self.end_token(TokenType::LessThan);
                        }
                    }
                }
                '>' => {
                    match self.next() {
                        '=' => {
                            self.next();
                            self.end_token(TokenType::GreaterOrEqual);
                        }
                        '>' => {
                            if self.next() == '>' {
                                self.next();
                                self.end_token(TokenType::UnsignedShiftRight);
                            } else {
                                self.end_token(TokenType::ShiftRight);
                            }
                        }
                        _ => {
                            self.next();
                            self.end_token(TokenType::GreaterThan);
                        }
                    }
                }
                '+' => {
                    match self.next() {
                        '=' => {
                            self.next();
                            self.end_token(TokenType::PlusEqual);
                        }
                        '+' => {
                            self.next();
                            self.end_token(TokenType::PlusPlus);
                        }
                        _ => {
                            self.end_token(TokenType::Plus);
                        }
                    }
                }
                '-' => {
                    match self.next() {
                        '=' => {
                            self.next();
                            self.end_token(TokenType::MinusEqual);
                        }
                        '-' => {
                            self.next();
                            self.end_token(TokenType::MinusMinus);
                        }
                        _ => {
                            self.end_token(TokenType::Minus);
                        }
                    }
                }
                ':' => {
                    match self.next() {
                        ':' => {
                            self.next();
                            self.end_token(TokenType::DoubleColon);
                        }
                        _ => {
                            self.end_token(TokenType::Colon);
                        }
                    }
                }
                '?' => {
                    self.next();
                    self.end_token(TokenType::Ternary);
                }
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
                '(' => {
                    self.next();
                    self.end_token(TokenType::LeftParen);
                }
                ')' => {
                    self.next();
                    self.end_token(TokenType::RightParen);
                }
                '[' => {
                    self.next();
                    self.end_token(TokenType::LeftSquare);
                }
                ']' => {
                    self.next();
                    self.end_token(TokenType::RightSquare);
                }
                '{' => {
                    self.next();
                    self.end_token(TokenType::LeftCurly);
                }
                '}' => {
                    self.next();
                    self.end_token(TokenType::RightCurly);
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