use core::fmt;
use std::{str::Chars, iter::Peekable, collections::HashMap, fmt::Display};

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

impl TokenType {
    pub fn as_str(&self) -> &str {
        match self {
            TokenType::Identifier => "IDENTIFIER",
            TokenType::StringLiteral => "STRING_LITERAL",
            TokenType::IntegerLiteral => "INTEGER_LITERAL",
            TokenType::FloatLiteral => "FLOAT_LITERAL",
            TokenType::Class => "class",
            TokenType::Base => "base",
            TokenType::Extends => "extends",
            TokenType::Constructor => "constructor",
            TokenType::Instanceof => "instanceof",
            TokenType::Static => "static",
            TokenType::This => "this",
            TokenType::Function => "function",
            TokenType::LeftCurly => "{",
            TokenType::RightCurly => "}",
            TokenType::LeftSquare => "[",
            TokenType::RightSquare => "]",
            TokenType::LeftParen => "(",
            TokenType::RightParen => ")",
            TokenType::Delete => "delete",
            TokenType::Switch => "switch",
            TokenType::If => "if",
            TokenType::Else => "else",
            TokenType::While => "while",
            TokenType::Break => "break",
            TokenType::For => "for",
            TokenType::Do => "do",
            TokenType::Null => "null",
            TokenType::Foreach => "foreach",
            TokenType::In => "in",
            TokenType::Newslot => "->",
            TokenType::Local => "local",
            TokenType::Clone => "clone",
            TokenType::Return => "return",
            TokenType::Typeof => "typeof",
            TokenType::Assign => "assign",
            TokenType::Equal => "==",
            TokenType::NotEqual => "!=",
            TokenType::LessThan => "<",
            TokenType::LessOrEqual => "<=",
            TokenType::GreaterOrEqual => ">=",
            TokenType::GreaterThan => ">",
            TokenType::ThreeWayCompare => "<=>",
            TokenType::LogicalNot => "!",
            TokenType::LogicalAnd => "&&",
            TokenType::LogicalOr => "||",
            TokenType::BitwiseAnd => "&",
            TokenType::BitwiseOr => "|",
            TokenType::BitwiseXor => "^",
            TokenType::BitwiseNot => "~",
            TokenType::Modulo => "%",
            TokenType::Multiply => "*",
            TokenType::Divide => "/",
            TokenType::Plus => "+",
            TokenType::Minus => "-",
            TokenType::PlusEqual => "+=",
            TokenType::MinusEqual => "-=",
            TokenType::MultiplyEqual => "*=",
            TokenType::DivideEqual => "/=",
            TokenType::ModuloEqual => "%=",
            TokenType::ShiftLeft => "<<",
            TokenType::ShiftRight => ">>",
            TokenType::PlusPlus => "++",
            TokenType::MinusMinus => "--",
            TokenType::Continue => "continue",
            TokenType::Yield => "yield",
            TokenType::Try => "try",
            TokenType::Catch => "catch",
            TokenType::Throw => "throw",
            TokenType::Resume => "resume",
            TokenType::DoubleColon => "::",
            TokenType::Ternary => "?",
            TokenType::Colon => ":",
            TokenType::Semicolon => ";",
            TokenType::Comma => ",",
            TokenType::Case => "case",
            TokenType::Default => "default",
            TokenType::UnsignedShiftRight => ">>>",
            TokenType::Varargs => "...",
            TokenType::LineInfo => "__LINE__",
            TokenType::FileInfo => "__FILE__",
            TokenType::True => "true",
            TokenType::False => "false",
            TokenType::AttributeOpen => "</",
            TokenType::AttributeClose => "/>",
            TokenType::Enum => "enum",
            TokenType::Const => "const",
            TokenType::Rawcall => "rawcall",
            TokenType::Dot => ".",
            TokenType::Invalid => "INVALID_TOKEN",
            
        }
    }
}

impl Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        return write!(f, "{}", self.as_str());
    }
}

#[derive(Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub text: String,
    pub nvalue: Option<i32>,
    pub fvalue: Option<f32>,
    pub svalue: Option<String>,
    pub range: TokenRange,
}

impl Default for Token {
    fn default() -> Self {
        Self {
            token_type: TokenType::Invalid,
            text: String::new(),
            nvalue: None,
            fvalue: None,
            svalue: None,
            range: TokenRange::new(),
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?} ({:?}) at [{:?}:{:?}, {:?}:{:?})", 
            self.token_type, self.text,
            self.range.start.line, self.range.start.column,
            self.range.end.line, self.range.end.column)?;
        
        if self.nvalue.is_some() {
            write!(f, ", nval {:?}", self.nvalue.unwrap())?;
        }
        if self.fvalue.is_some() {
            write!(f, ", fval {:?}", self.fvalue.unwrap())?;
        }
        if self.svalue.is_some() {
            write!(f, ", sval {:?}", self.svalue.as_ref().unwrap())?;
        }

        Ok(())
    }
}

#[derive(Debug, Copy, Clone)]
pub struct TokenLocation {
    line: u32,
    column: u32,
}

impl TokenLocation {
    pub fn new() -> Self {
        Self {
            line: 0,
            column: 0,
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct TokenRange {
    start: TokenLocation,
    end: TokenLocation,
}

impl TokenRange {
    pub fn new() -> Self {
        Self {
            start: TokenLocation::new(),
            end: TokenLocation::new(),
        }
    }
}

pub struct Lexer<'a> {
    iter: Peekable<Chars<'a>>,
    line: u32,
    column: u32,
    token_start_location: TokenLocation,
    cur_char: char,
    cur_token_text: String,
    pub tokens: Vec<Token>,
    keywords: HashMap<String, TokenType>,
}

impl<'a,'b> Lexer<'b> {
    pub fn new(data: &'a str) -> Self where 'a: 'b {
        Self {
            iter: data.chars().peekable(),
            line: 1,
            column: 0,
            token_start_location: TokenLocation::new(),
            cur_char: '\0',
            cur_token_text: String::from(""),
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
        self.cur_token_text = String::from("");
        self.token_start_location = TokenLocation {
            line: self.line,
            column: self.column,
        }
    }

    fn end_token(&mut self, token_type: TokenType) {
        self.end_token_full(token_type, None, None, None);
    }

    fn end_token_full(&mut self, token_type: TokenType, nvalue: Option<i32>, fvalue: Option<f32>, svalue: Option<String>) {
        self.tokens.push(Token {
            token_type,
            text: self.cur_token_text.clone(),
            nvalue,
            fvalue,
            svalue,
            range: TokenRange {
                start: self.token_start_location,
                end: TokenLocation {
                    line: self.line, column: self.column
                },
            },
            ..Default::default()
        });
    }

    fn end_bad_token(&mut self) {
        self.end_token(TokenType::Invalid);
    }

    fn end_token_with_nval(&mut self, token_type: TokenType, nvalue: i32) {
        self.end_token_full(token_type, Some(nvalue), None, None);
    }

    fn end_token_with_fval(&mut self, token_type: TokenType, fvalue: f32) {
        self.end_token_full(token_type, None, Some(fvalue), None);
    }

    fn end_token_with_sval(&mut self, token_type: TokenType, svalue: String) {
        self.end_token_full(token_type, None, None, Some(svalue));
    }
    
    fn end_token_on_next(&mut self, token_type: TokenType) {
        self.next();
        self.end_token(token_type);
    }
    
    fn next(&mut self) -> char {
        self.cur_token_text.push(self.cur_char);
        self.cur_char = self.iter.next().unwrap_or('\0');
        if self.cur_char == '\n' {
            self.line += 1;
            self.column = 0;
        }
        self.column += 1;

        return self.cur_char;
    }

    fn is_digit(char: char) -> bool {
        return char.is_ascii_digit();
    }

    fn is_octal(char: char) -> bool {
        return ('0'..='7').contains(&char);
    }

    fn is_hex(char: char) -> bool {
        return char.is_ascii_hexdigit();
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
                    if self.keywords.contains_key(&self.cur_token_text) {
                        self.end_token(self.keywords[&self.cur_token_text]);
                    } else {
                        self.end_token(TokenType::Identifier);
                    }
                    break;
                }
            }
        }
    }

    fn lex_string(&mut self) {
        let mut verbatim = false;
        if self.cur_char == '@' {
            verbatim = true;
            self.next();
            if self.cur_char != '\'' && self.cur_char != '"' {
                self.end_bad_token();
                return;
            }
        }

        let mut fail_at_end = false;
        let mut svalue = String::new();
        let delimiter = self.cur_char;
        loop {
            self.next();
            if self.cur_char == '\n' && !verbatim {
                // still consume tokens up to ending delimiter
                fail_at_end = true;
            }
            if self.cur_char == '\\' {
                if verbatim {
                   svalue.push(self.cur_char);
                   continue; 
                }

                match self.next() {
                    'x' => {
                        let mut hex_digits = String::new();
                        while Self::is_hex(self.next()) {
                            hex_digits.push(self.cur_char);
                        }

                        if let Ok(value) = u32::from_str_radix(&hex_digits, 16) {
                            if let Some(char) = char::from_u32(value) {
                                svalue.push(char);
                            } else {
                                fail_at_end = true;
                            }
                        } else {
                            fail_at_end = true;
                        }
                    }
                    'U' | 'u' => {
                        let mut hex_digits = String::new();
                        while Self::is_hex(self.next()) {
                            hex_digits.push(self.cur_char);
                        }

                        if let Ok(value) = u32::from_str_radix(&hex_digits, 16) {
                            if let Some(char) = char::from_u32(value) {
                                svalue.push(char);
                            } else {
                                fail_at_end = true;
                            }
                        } else {
                            fail_at_end = true;
                        }
                    }
                    't' => svalue.push('\t'),
                    'a' => svalue.push('\x07'),
                    'b' => svalue.push('\x08'),
                    'n' => svalue.push('\n'),
                    'r' => svalue.push('\r'),
                    'v' => svalue.push('\x0b'),
                    'f' => svalue.push('\x0c'),
                    '0' => svalue.push('\0'),
                    '\\' => svalue.push('\\'),
                    '"' => svalue.push('"'),
                    '\'' => svalue.push('\''),
                    _ => fail_at_end = true,
                }

                continue;
            }
            if self.cur_char == delimiter {
                self.next();

                if fail_at_end || (delimiter == '\'' && svalue.len() != 1) {
                    self.end_bad_token();
                } else {
                    self.end_token_with_sval(TokenType::StringLiteral, svalue);
                }

                break;
            }
            if self.cur_char == '\0' {
                self.end_bad_token();
                break;
            }

            svalue.push(self.cur_char);
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
                continue;
            }

            self.start_token();
            
            match self.cur_char {
                'a'..='z' | 'A'..='Z' => self.lex_identifier(),
                '0'..='9' => self.lex_number(),
                '\'' | '"' | '@' => self.lex_string(),
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
                        '=' => self.end_token_on_next(TokenType::MultiplyEqual),
                        _ => self.end_token(TokenType::Multiply),
                    }
                }
                '/' => {
                    match self.next() {
                        '=' => self.end_token_on_next(TokenType::DivideEqual),
                        '>' => self.end_token_on_next(TokenType::AttributeClose),
                        '/' => { // single line comment
                            loop {
                                self.next();
                                if self.cur_char == '\n' || self.cur_char == '\0' {
                                    break;
                                }
                            }
                        }
                        '*' => { // multi line comment
                            loop {
                                self.next();
                                if self.cur_char == '\0' {
                                    break;
                                }
                                if self.cur_char == '*' && self.next() == '/' {
                                    self.next();
                                    break;
                                }
                            }
                        }
                        _ => self.end_token(TokenType::Divide),
                    }
                }
                '#' => {
                    loop {
                        self.next();
                        if self.cur_char == '\n' || self.cur_char == '\0' {
                            break;
                        }
                    }
                }
                '%' => {
                    match self.next() {
                        '=' => self.end_token_on_next(TokenType::ModuloEqual),
                        _ => self.end_token(TokenType::Modulo),
                    }
                }
                '&' => {
                    match self.next() {
                        '&' => self.end_token_on_next(TokenType::LogicalAnd),
                        _ => self.end_token(TokenType::BitwiseAnd),
                    }
                }
                '|' => {
                    match self.next() {
                        '|' => self.end_token_on_next(TokenType::LogicalOr),
                        _ => self.end_token(TokenType::BitwiseOr),
                    }
                }
                '^' => self.end_token_on_next(TokenType::BitwiseXor),
                '~' => self.end_token_on_next(TokenType::BitwiseNot),
                '<' => {
                    match self.next() {
                        '=' => {
                            if self.next() == '>' {
                                self.end_token(TokenType::ThreeWayCompare);
                            } else {
                                self.end_token(TokenType::LessOrEqual);
                            }
                        }
                        '-' => self.end_token_on_next(TokenType::Newslot),
                        '<' => self.end_token_on_next(TokenType::ShiftLeft),
                        '/' => self.end_token_on_next(TokenType::AttributeOpen),
                        _ => self.end_token(TokenType::LessThan),
                    }
                }
                '>' => {
                    match self.next() {
                        '=' => self.end_token_on_next(TokenType::GreaterOrEqual),
                        '>' => {
                            if self.next() == '>' {
                                self.end_token_on_next(TokenType::UnsignedShiftRight);
                            } else {
                                self.end_token(TokenType::ShiftRight);
                            }
                        }
                        _ => self.end_token_on_next(TokenType::GreaterThan),
                    }
                }
                '+' => {
                    match self.next() {
                        '=' => self.end_token_on_next(TokenType::PlusEqual),
                        '+' => self.end_token_on_next(TokenType::PlusPlus),
                        _ => self.end_token(TokenType::Plus),
                    }
                }
                '-' => {
                    match self.next() {
                        '=' => self.end_token_on_next(TokenType::MinusEqual),
                        '-' => self.end_token_on_next(TokenType::MinusMinus),
                        _ => self.end_token(TokenType::Minus),
                    }
                }
                ':' => {
                    match self.next() {
                        ':' => self.end_token_on_next(TokenType::DoubleColon),
                        _ => self.end_token(TokenType::Colon),
                    }
                }
                ';' => self.end_token_on_next(TokenType::Semicolon),
                ',' => self.end_token_on_next(TokenType::Comma),
                '?' => self.end_token_on_next(TokenType::Ternary),
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
                '(' => self.end_token_on_next(TokenType::LeftParen),
                ')' => self.end_token_on_next(TokenType::RightParen),
                '[' => self.end_token_on_next(TokenType::LeftSquare),
                ']' => self.end_token_on_next(TokenType::RightSquare),
                '{' => self.end_token_on_next(TokenType::LeftCurly),
                '}' => self.end_token_on_next(TokenType::RightCurly),
                _ => self.end_token_on_next(TokenType::Invalid),
            }
        }
    }
}