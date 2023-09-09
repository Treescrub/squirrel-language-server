use core::fmt;
use std::{str::Chars, iter::Peekable, collections::HashMap};

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
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
    Semicolon,              // ;
    Comma,                  // ,
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
    pub nvalue: Option<i32>,
    pub fvalue: Option<f32>,
    pub line: u32,
    pub column: u32,
}

impl Default for Token {
    fn default() -> Self {
        Self {
            token_type: TokenType::Invalid,
            value: String::new(),
            nvalue: None,
            fvalue: None,
            line: 0,
            column: 0,
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?} ({:?}) at line {:?} column {:?}", self.token_type, self.value, self.line, self.column)?;
        if self.nvalue.is_some() {
            write!(f, ", nval {:?}", self.nvalue.unwrap())?;
        }
        if self.fvalue.is_some() {
            write!(f, ", fval {:?}", self.fvalue.unwrap())?;
        }
        
        Ok(())
    }
}

pub struct Lexer<'a> {
    iter: Peekable<Chars<'a>>,
    line: u32,
    column: u32,
    cur_char: char,
    cur_token_value: String,
    pub tokens: Vec<Token>,
    keywords: HashMap<String, TokenType>,
}

impl<'a,'b> Lexer<'b> {
    pub fn new(data: &'a str) -> Self where 'a: 'b {
        Self {
            iter: data.chars().peekable(),
            line: 1,
            column: 0,
            cur_char: '\0',
            cur_token_value: String::from(""),
            tokens: Vec::<Token>::new(),
            keywords: Self::init_keywords(),
        }
    }

    fn init_keywords() -> HashMap<String, TokenType> {
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

    fn start_token(&mut self) {
        self.cur_token_value = String::from("");
    }

    fn end_token(&mut self, token_type: TokenType) {
        self.end_token_full(token_type, None, None);
    }

    fn end_token_full(&mut self, token_type: TokenType, nvalue: Option<i32>, fvalue: Option<f32>) {
        self.tokens.push(Token {
            token_type,
            value: self.cur_token_value.clone(),
            nvalue: nvalue,
            fvalue: fvalue,
            line: self.line,
            column: self.column,
            ..Default::default()
        });
    }

    fn end_bad_token(&mut self) {
        self.end_token(TokenType::Invalid);
    }

    fn end_token_with_nval(&mut self, token_type: TokenType, nvalue: i32) {
        self.end_token_full(token_type, Some(nvalue), None);
    }

    fn end_token_with_fval(&mut self, token_type: TokenType, fvalue: f32) {
        self.end_token_full(token_type, None, Some(fvalue));
    }
    
    fn end_token_on_next(&mut self, token_type: TokenType) {
        self.next();
        self.end_token(token_type);
    }
    
    fn next(&mut self) -> char {
        self.cur_token_value.push(self.cur_char);
        self.cur_char = self.iter.next().unwrap_or('\0');
        self.column += 1;

        return self.cur_char;
    }

    fn is_digit(char: char) -> bool {
        return char >= '0' && char <= '9';
    }

    fn is_octal(char: char) -> bool {
        return char >= '0' && char <= '7';
    }

    fn is_hex(char: char) -> bool {
        return (char >= '0' && char <= '9') || (char >= 'a' && char <= 'f') || (char >= 'A' && char <= 'F');
    }

    fn is_exponent(char: char) -> bool {
        return char == 'e' || char == 'E';
    }

    fn lex_identifier(&mut self) {
        loop {
            self.next();
            match self.cur_char {
                'a'..='z' | 'A'..='Z' | '_' | '0'..='9' => (),
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

    fn lex_string(&mut self) {
        let delimiter = self.cur_char;
        loop {
            self.next();
            if self.cur_char == delimiter {
                self.end_token_on_next(TokenType::StringLiteral);
                break;
            }
            if self.cur_char == '\0' {
                self.end_bad_token();
                break;
            }
        }
    }

    fn lex_number(&mut self) {
        enum NumberType {
            Int,
            Hex,
            Octal,
            Float,
            Scientific,
        }

        let mut num_type: NumberType = NumberType::Int;
        let first_char = self.cur_char;
        let mut parse_chars = String::new();
        self.next();
        if first_char == '0' && (self.cur_char.to_ascii_uppercase() == 'X' || Self::is_octal(self.cur_char))  { // could be hex or octal
            if self.cur_char.to_ascii_uppercase() == 'X' {
                num_type = NumberType::Hex;

                while Self::is_hex(self.next()) {
                    parse_chars.push(self.cur_char);
                }
            } else if Self::is_octal(self.cur_char) {
                num_type = NumberType::Octal;
                
                parse_chars.push(self.cur_char);
                while Self::is_octal(self.next()) {
                    parse_chars.push(self.cur_char);
                }

                if Self::is_digit(self.cur_char) { // non octal digit encountered
                    self.end_bad_token();
                    return;
                }
            }
        } else {
            parse_chars.push(first_char);

            while self.cur_char == '.' || Self::is_digit(self.cur_char) || Self::is_exponent(self.cur_char) {
                if self.cur_char == '.' || Self::is_exponent(self.cur_char) {
                    num_type = NumberType::Float;
                }

                if Self::is_exponent(self.cur_char) {
                    num_type = NumberType::Scientific;

                    parse_chars.push(self.cur_char);
                    self.next();

                    if self.cur_char == '+' || self.cur_char == '-' {
                        parse_chars.push(self.cur_char);
                        self.next();
                    }

                    if !Self::is_digit(self.cur_char) {
                        self.end_bad_token();
                        return;
                    }
                }

                parse_chars.push(self.cur_char);
                self.next();
            }
        }

        match num_type {
            NumberType::Int => {
                match parse_chars.parse() {
                    Ok(val) => self.end_token_with_nval(TokenType::IntegerLiteral, val),
                    Err(_) => self.end_bad_token()
                }
                
            }
            NumberType::Hex => {
                match i32::from_str_radix(&parse_chars, 16) {
                    Ok(val) => self.end_token_with_nval(TokenType::IntegerLiteral, val),
                    Err(_) => self.end_bad_token()
                }
            }
            NumberType::Octal => {
                match i32::from_str_radix(&parse_chars, 8) {
                    Ok(val) => self.end_token_with_nval(TokenType::IntegerLiteral, val),
                    Err(_) => self.end_bad_token()
                }
            }
            NumberType::Float => {
                match parse_chars.parse() {
                    Ok(val) => self.end_token_with_fval(TokenType::FloatLiteral, val),
                    Err(_) => self.end_bad_token()
                }
            }
            NumberType::Scientific => {
                // TODO: parse scientific notation the same way as C++ strtod
                match parse_chars.parse() {
                    Ok(val) => self.end_token_with_fval(TokenType::FloatLiteral, val),
                    Err(_) => self.end_bad_token()
                }
            }
        }
    }

    pub fn lex(&mut self) {
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
                self.line += 1;
                self.column = 1;
                continue;
            }

            self.start_token();
            
            match self.cur_char {
                'a'..='z' | 'A'..='Z' => {
                    self.lex_identifier();
                }
                '0'..='9' => {
                    self.lex_number();
                }
                '\'' | '"' => {
                    self.lex_string();
                }
                '=' => {
                    if self.next() != '=' {
                        self.end_token(TokenType::Assign);
                    } else {
                        self.end_token_on_next(TokenType::Equal);
                    }
                }
                '!' => {
                    if self.next() != '=' {
                        self.end_token(TokenType::LogicalNot);
                    } else {
                        self.end_token_on_next(TokenType::NotEqual);
                    }
                }
                '*' => {
                    match self.next() {
                        '=' => {
                            self.end_token_on_next(TokenType::MultiplyEqual);
                        }
                        _ => {
                            self.end_token(TokenType::Multiply);
                        }
                    }
                }
                '/' => {
                    match self.next() {
                        '=' => {
                            self.end_token_on_next(TokenType::DivideEqual);
                        }
                        '>' => {
                            self.end_token_on_next(TokenType::AttributeClose);
                        }
                        _ => {
                            self.end_token(TokenType::Divide);
                        }
                    }
                }
                '%' => {
                    match self.next() {
                        '=' => {
                            self.end_token_on_next(TokenType::ModuloEqual);
                        }
                        _ => {
                            self.end_token(TokenType::Modulo);
                        }
                    }
                }
                '&' => {
                    match self.next() {
                        '=' => {
                            self.end_token_on_next(TokenType::AndEqual);
                        }
                        '&' => {
                            self.end_token_on_next(TokenType::LogicalAnd);
                        }
                        _ => {
                            self.end_token(TokenType::BitwiseAnd);
                        }
                    }
                }
                '|' => {
                    match self.next() {
                        '=' => {
                            self.end_token_on_next(TokenType::OrEqual);
                        }
                        '|' => {
                            self.end_token_on_next(TokenType::LogicalOr);
                        }
                        _ => {
                            self.end_token(TokenType::BitwiseOr);
                        }
                    }
                }
                '^' => {
                    match self.next() {
                        '=' => {
                            self.end_token_on_next(TokenType::XorEqual);
                        }
                        _ => {
                            self.end_token(TokenType::BitwiseXor);
                        }
                    }
                }
                '~' => {
                    match self.next() {
                        '=' => {
                            self.end_token_on_next(TokenType::BitwiseNotEqual);
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
                            self.end_token_on_next(TokenType::Newslot);
                        }
                        '<' => {
                            self.end_token_on_next(TokenType::ShiftLeft);
                        }
                        '/' => {
                            self.end_token_on_next(TokenType::AttributeOpen);
                        }
                        _ => {
                            self.end_token(TokenType::LessThan);
                        }
                    }
                }
                '>' => {
                    match self.next() {
                        '=' => {
                            self.end_token_on_next(TokenType::GreaterOrEqual);
                        }
                        '>' => {
                            if self.next() == '>' {
                                self.end_token_on_next(TokenType::UnsignedShiftRight);
                            } else {
                                self.end_token(TokenType::ShiftRight);
                            }
                        }
                        _ => {
                            self.end_token_on_next(TokenType::GreaterThan);
                        }
                    }
                }
                '+' => {
                    match self.next() {
                        '=' => {
                            self.end_token_on_next(TokenType::PlusEqual);
                        }
                        '+' => {
                            self.end_token_on_next(TokenType::PlusPlus);
                        }
                        _ => {
                            self.end_token(TokenType::Plus);
                        }
                    }
                }
                '-' => {
                    match self.next() {
                        '=' => {
                            self.end_token_on_next(TokenType::MinusEqual);
                        }
                        '-' => {
                            self.end_token_on_next(TokenType::MinusMinus);
                        }
                        _ => {
                            self.end_token(TokenType::Minus);
                        }
                    }
                }
                ':' => {
                    match self.next() {
                        ':' => {
                            self.end_token_on_next(TokenType::DoubleColon);
                        }
                        _ => {
                            self.end_token(TokenType::Colon);
                        }
                    }
                }
                ';' => {
                    self.end_token_on_next(TokenType::Semicolon);
                }
                ',' => {
                    self.end_token_on_next(TokenType::Comma);
                }
                '?' => {
                    self.end_token_on_next(TokenType::Ternary);
                }
                '.' => {
                    if self.next() != '.' {
                        self.end_token(TokenType::Dot);
                        continue;
                    }
                    if self.next() != '.' {
                        self.end_bad_token();
                        continue;
                    }
                    self.end_token_on_next(TokenType::Varargs);
                }
                '(' => {
                    self.end_token_on_next(TokenType::LeftParen);
                }
                ')' => {
                    self.end_token_on_next(TokenType::RightParen);
                }
                '[' => {
                    self.end_token_on_next(TokenType::LeftSquare);
                }
                ']' => {
                    self.end_token_on_next(TokenType::RightSquare);
                }
                '{' => {
                    self.end_token_on_next(TokenType::LeftCurly);
                }
                '}' => {
                    self.end_token_on_next(TokenType::RightCurly);
                }
                _ => {
                    self.end_token_on_next(TokenType::Invalid);
                    continue;
                }
            }
        }
    }
}