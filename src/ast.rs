use crate::lexer::{Token, TokenType};

trait AstNode {

}

pub struct Script {
    pub statements: Vec<Statement>,
}

pub struct Statements {
    pub statements: Vec<Statement>,
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

pub enum Factor {
    Scalar(Scalar),
    Base,
    Identifier(Identifier),
    Constructor,
    This,
    // root table access (::)
    Null,
    ArrayInit(Vec<Expression>),
    TableInit, // TODO: params
    DeclareFunction, // TODO: params
    FunctionExpression, // TODO: params
    DeclareClass, // TODO: params
    UnaryOp, // TODO: params
    RawCall, // TODO: params
    Delete(Expression),
    LineInfo,
    FileInfo,
}

pub struct UnaryOp {
    pub operator: TokenType,
    pub expression: PrefixedExpression,
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

pub struct FunctionCallArgs {
    args: Vec<Expression>,
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
    pub expressions: Vec<Expression>,
}

pub struct Expression {
    pub logical_or: Box<LogicalOrExpression>,
    pub expr_type: Option<ExpressionType>
}

pub enum ExpressionType {
    Clone(Box<Expression>),
    Resume(Box<Expression>),
    Delete(Box<Expression>),
    CallFunction,
    DeclareFunction,
    AnonymousFunction,
    Lambda,
    Newslot(Box<Expression>),
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
    pub left: LogicalAndExpression,
    pub right: Option<Box<LogicalOrExpression>>,
}

pub struct LogicalAndExpression {
    pub left: BitwiseOrExpression,
    pub right: Option<Box<LogicalAndExpression>>,
}

pub struct BitwiseOrExpression {
    pub left: BitwiseXorExpression,
    pub right: Option<BitwiseXorExpression>,
}

pub struct BitwiseXorExpression {
    pub left: BitwiseAndExpression,
    pub right: Option<BitwiseAndExpression>,
}

pub struct BitwiseAndExpression {
    pub left: EqualExpression,
    pub right: Option<EqualExpression>,
}

pub struct EqualExpression {
    pub left: CompareExpression,
    pub operator: Option<TokenType>,
    pub right: Option<CompareExpression>,
}

pub struct CompareExpression {
    pub left: ShiftExpression,
    pub operator: Option<TokenType>,
    pub right: Option<ShiftExpression>,
}

pub struct ShiftExpression {
    pub left: PlusExpression,
    pub operator: Option<TokenType>,
    pub right: Option<PlusExpression>,
}

pub struct PlusExpression {
    pub left: MultiplyExpression,
    pub operator: Option<TokenType>,
    pub right: Option<MultiplyExpression>,
}

pub struct MultiplyExpression {
    pub left: PrefixedExpression,
    pub operator: Option<TokenType>,
    pub right: Option<PrefixedExpression>,
}

pub struct PrefixedExpression {
    pub factor: Factor,
    pub expr_type: Option<PrefixedExpressionType>,
}

pub enum PrefixedExpressionType {
    DotAccess(Identifier),
    ArrayStyleAccess(Expression),
    PostIncrement,
    PostDecrement,
    FunctionCall(FunctionCallArgs),
}

pub struct Identifier {
    pub value: String,
}