use crate::lexer::Token;

trait AstNode {

}

pub trait Visitor<T> {
    fn visit_statements(&mut self, statements: Statements) -> T;
}

pub struct Statements {
    statements: Vec<Statement>,
}

pub enum Statement {
    If(CommaExpression, IfBlock, Option<IfBlock>),
    While(CommaExpression, Box<Statement>),
    DoWhile(Box<Statement>, CommaExpression),
    For(Option<ForInit>, Option<CommaExpression>, Option<CommaExpression>),
    ForEach(Identifier, Option<Identifier>, Expression, Box<Statement>),
    Switch(CommaExpression, Vec<SwitchCases>),
    LocalDeclare(LocalDeclare),
    Return(Option<CommaExpression>),
    Yield(Option<CommaExpression>),
    Break,
    Continue,
    Function(FunctionIdentifier, Option<Expression>/* static env binding */, Vec<FunctionParam>, Box<Statement>),
    Class(Identifier), // TODO: figure out what the hell is going on here
    Enum(Identifier, Vec<EnumEntry>),
    Statements(Statements),
    TryCatch(Box<Statement>, Identifier, Box<Statement>),
    Throw(CommaExpression),
    Const(Identifier, Scalar),
    CommaExpression(CommaExpression)
}

pub struct EnumEntry {
    key: Identifier,
    value: Option<Scalar>,
}

pub enum Scalar {
    Integer,
    Float,
    StringLiteral,
    True,
    False,
}

pub struct FunctionIdentifier {
    in_root: bool,
    identifiers: Vec<Identifier>,
}

pub enum FunctionParam {
    Normal(Identifier),
    Default(Identifier, Expression),
    VarParams,
}

pub struct SwitchCases {

}

pub enum LocalDeclare {

}

pub enum IfBlock {
    SingleLine(Box<Statement>),
    MultiLine(Statements),
}

pub enum ForInit {
    LocalDeclare(LocalDeclare),
    CommaExpression(CommaExpression),
}

pub struct CommaExpression {

}

pub enum Expression {
    Clone(Box<Expression>),
    Resume(Box<Expression>),
    Delete(Box<Expression>),
    CallFunction,
    DeclareFunction,
    AnonymousFunction,
    Lambda,
    Newslot,
    InitTable,
    Paren,
    Base,
    Identifier(Identifier),
    DotAccess,
    IndexAccess,
    ScopeAccess,
    Literal,
}

pub struct LogicalOrExpression {
    logical_and: LogicalAndExpression,
    logical_or: Box<LogicalOrExpression>,
}

pub struct LogicalAndExpression {
    bitwise_or: BitwiseOrExpression,
    logical_and: Box<LogicalAndExpression>,
}

pub struct BitwiseOrExpression {
    bitwise_xor: BitwiseXorExpression,
    bitwise_or: Box<BitwiseOrExpression>,
}

pub struct BitwiseXorExpression {
    bitwise_and: BitwiseAndExpression,
    bitwise_xor: Box<BitwiseXorExpression>,
}

pub struct BitwiseAndExpression {
    equal1: Equal,
    equal2: Equal,
}

pub struct Equal {
    compare1: Compare,
    operator: Token,
    compare2: Compare,
}

pub struct Compare {
    
}

pub enum PrefixedExpression {
    Clone(Box<PrefixedExpression>),
    Delete(Box<PrefixedExpression>),
}

pub struct Identifier {
    value: String,
}