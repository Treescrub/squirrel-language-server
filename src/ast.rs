use crate::lexer::{TokenType, Token};

pub struct Script {
    pub statements: Vec<Statement>,
}

pub struct Statements {
    pub statements: Vec<Statement>,
}

pub enum Statement {
    If(CommaExpression, Box<Statement>, Option<Box<Statement>>),
    While(CommaExpression, Box<Statement>),
    DoWhile(Box<Statement>, CommaExpression),
    For(Option<ForInit>, Option<CommaExpression>, Option<CommaExpression>, Box<Statement>),
    ForEach(Identifier /* value */, Option<Identifier> /* key */, Expression, Box<Statement>),
    Switch(CommaExpression, Vec<SwitchCase>, Option<Statements> /* default */),
    LocalDeclare(LocalDeclare),
    Return(Option<CommaExpression>),
    Yield(Option<CommaExpression>),
    Break,
    Continue,
    Function(FunctionIdentifier, Option<Expression>/* static env binding */, FunctionParams, Box<Statement>),
    Class(PrefixedExpression, ClassExpression),
    Enum(Identifier, EnumValues),
    StatementBlock(Statements),
    TryCatch(Box<Statement>, Identifier, Box<Statement>),
    Throw(CommaExpression),
    Const(Identifier, Scalar),
    CommaExpression(CommaExpression)
}

pub struct EnumValues {
    pub values: Vec<EnumEntry>,
}

pub struct EnumEntry {
    pub key: Identifier,
    pub value: Option<Scalar>,
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
    DoubleColon(Box<PrefixedExpression>),
    Null,
    ArrayInit(Vec<Expression>),
    TableInit(Table),
    FunctionExpression(Option<Expression> /* bind env */, FunctionParams, Box<Statement>),
    LambdaExpression(Option<Expression> /* bind env */, FunctionParams, Expression),
    ClassExpression(ClassExpression),
    UnaryOp(Box<UnaryOp>),
    RawCall(FunctionCallArgs),
    Delete(Box<PrefixedExpression>),
    ParenExpression(CommaExpression),
    LineInfo,
    FileInfo,
}

pub struct ClassExpression {
    pub base_class: Option<Expression>,
    pub attributes: Option<Table>,
    pub body: Table,
}

pub struct Table {
    pub entries: Vec<TableEntry>,
}

pub enum TableEntry {
    Function(Identifier, FunctionParams, Statement),
    Constructor(FunctionParams, Statement),
    DynamicAssign(CommaExpression, Expression),
    JsonStyle(String, Expression),
    Simple(Identifier, Expression),
    Attributes(Table),
}

pub struct UnaryOp {
    pub operator: TokenType,
    pub expression: PrefixedExpression,
}

pub struct FunctionIdentifier {
    pub identifiers: Vec<Identifier>,
}

pub struct FunctionParams {
    pub params: Vec<FunctionParam>,
}

pub enum FunctionParam {
    Normal(Identifier),
    Default(Identifier, Expression),
    VarParams,
}

pub struct FunctionCallArgs {
    pub args: Vec<Expression>,
    pub post_call_init: Option<PostCallInitialize>,
}

pub struct PostCallInitialize {
    pub entries: Vec<PostCallInitializeEntry>,
}

pub enum PostCallInitializeEntry {
    ArrayStyle(CommaExpression, Expression),
    TableStyle(Identifier, Expression),
}

pub struct SwitchCase {
    pub condition: Expression,
    pub body: Statements,
}

pub enum LocalDeclare {
    Function(Identifier, Option<Expression>/*bind env*/, FunctionParams),
    Assign(Vec<AssignExpression>),
}

pub struct AssignExpression {
    pub identifier: Identifier,
    pub value: Option<Expression>,
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
    Newslot(Box<Expression>),
    Assign(Box<Expression>),
    MinusEqual(Box<Expression>),
    PlusEqual(Box<Expression>),
    MultiplyEqual(Box<Expression>),
    DivideEqual(Box<Expression>),
    Ternary(Box<Expression>, Box<Expression>),
}

pub struct BinaryOpSlice<T> {
    pub operator: TokenType,
    pub right: T,
}

impl<T> BinaryOpSlice<T> {
    pub fn new(operator: TokenType, right: T) -> BinaryOpSlice<T> {
        return BinaryOpSlice {
            operator,
            right,
        }
    }
}

pub struct LogicalOrExpression {
    pub left: LogicalAndExpression,
    pub right: Vec<Box<LogicalOrExpression>>,
}

pub struct LogicalAndExpression {
    pub left: BitwiseOrExpression,
    pub right: Vec<Box<LogicalAndExpression>>,
}

pub struct BitwiseOrExpression {
    pub left: BitwiseXorExpression,
    pub right: Vec<BitwiseXorExpression>,
}

pub struct BitwiseXorExpression {
    pub left: BitwiseAndExpression,
    pub right: Vec<BitwiseAndExpression>,
}

pub struct BitwiseAndExpression {
    pub left: EqualExpression,
    pub right: Vec<EqualExpression>,
}

pub struct EqualExpression {
    pub left: CompareExpression,
    pub slices: Vec<BinaryOpSlice<CompareExpression>>,
}

pub struct CompareExpression {
    pub left: ShiftExpression,
    pub slices: Vec<BinaryOpSlice<ShiftExpression>>,
}

pub struct ShiftExpression {
    pub left: PlusExpression,
    pub slices: Vec<BinaryOpSlice<PlusExpression>>,
}

pub struct PlusExpression {
    pub left: MultiplyExpression,
    pub slices: Vec<BinaryOpSlice<MultiplyExpression>>,
}

pub struct MultiplyExpression {
    pub left: PrefixedExpression,
    pub slices: Vec<BinaryOpSlice<PrefixedExpression>>,
}

pub struct PrefixedExpression {
    pub factor: Factor,
    pub expr_types: Vec<PrefixedExpressionType>,
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

impl From<&Token> for Identifier {
    fn from(value: &Token) -> Self {
        Self {
            value: value.clone_svalue(),
        }
    }
}