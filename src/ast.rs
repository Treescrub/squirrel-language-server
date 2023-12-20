use crate::lexer::{TokenType, Token};

pub struct Script {
    pub statements: Statements,
}

impl Script {
    pub fn new(statements: Vec<Statement>) -> Self {
        Self {
            statements: Statements::new(statements),
        }
    }
}

pub struct Statements {
    pub statements: Vec<Statement>,
}

impl Statements {
    pub fn new(statements: Vec<Statement>) -> Self {
        Self {
            statements,
        }
    }
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

impl EnumValues {
    pub fn new(values: Vec<EnumEntry>) -> Self {
        Self {
            values,
        }
    }
}

pub struct EnumEntry {
    pub key: Identifier,
    pub value: Option<Scalar>,
}

impl EnumEntry {
    pub fn new(key: Identifier, value: Option<Scalar>) -> Self {
        Self {
            key,
            value,
        }
    }
}

pub enum Scalar {
    Integer(i32),
    Float(f32),
    StringLiteral(String),
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

impl ClassExpression {
    pub fn new(base_class: Option<Expression>, attributes: Option<Table>, body: Table) -> Self {
        Self {
            base_class,
            attributes,
            body,
        }
    }
}

pub struct Table {
    pub entries: Vec<TableEntry>,
}

impl Table {
    pub fn new(entries: Vec<TableEntry>) -> Self {
        Self {
            entries,
        }
    }
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

impl UnaryOp {
    pub fn new(operator: TokenType, expression: PrefixedExpression) -> Self {
        Self {
            operator,
            expression,
        }
    }
}

pub struct FunctionIdentifier {
    pub identifiers: Vec<Identifier>,
}

impl FunctionIdentifier {
    pub fn new(identifiers: Vec<Identifier>) -> Self {
        Self {
            identifiers,
        }
    }
}

pub struct FunctionParams {
    pub params: Vec<FunctionParam>,
}

impl FunctionParams {
    pub fn new(params: Vec<FunctionParam>) -> Self {
        Self {
            params,
        }
    }
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

impl FunctionCallArgs {
    pub fn new(args: Vec<Expression>, post_call_init: Option<PostCallInitialize>) -> Self {
        Self {
            args,
            post_call_init,
        }
    }
}

pub struct PostCallInitialize {
    pub entries: Vec<PostCallInitializeEntry>,
}

impl PostCallInitialize {
    pub fn new(entries: Vec<PostCallInitializeEntry>) -> Self {
        Self {
            entries,
        }
    }
}

pub enum PostCallInitializeEntry {
    ArrayStyle(CommaExpression, Expression),
    TableStyle(Identifier, Expression),
}

pub struct SwitchCase {
    pub condition: Expression,
    pub body: Statements,
}

impl SwitchCase {
    pub fn new(condition: Expression, body: Statements) -> Self {
        Self {
            condition,
            body,
        }
    }
}

pub enum LocalDeclare {
    Function(Identifier, Option<Expression>/*bind env*/, FunctionParams, Box<Statement>),
    Assign(Vec<AssignExpression>),
}

pub struct AssignExpression {
    pub identifier: Identifier,
    pub value: Option<Expression>,
}

impl AssignExpression {
    pub fn new(identifier: Identifier, value: Option<Expression>) -> Self {
        Self {
            identifier,
            value,
        }
    }
}

pub enum ForInit {
    LocalDeclare(LocalDeclare),
    CommaExpression(CommaExpression),
}

pub struct CommaExpression {
    pub expressions: Vec<Expression>,
}

impl CommaExpression {
    pub fn new(expressions: Vec<Expression>) -> Self {
        Self {
            expressions,
        }
    }
}

pub struct Expression {
    pub logical_or: Box<LogicalOrExpression>,
    pub expr_type: Option<ExpressionType>
}

impl Expression {
    pub fn new(logical_or: Box<LogicalOrExpression>, expr_type: Option<ExpressionType>) -> Self {
        Self {
            logical_or,
            expr_type,
        }
    }
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

impl LogicalOrExpression {
    pub fn new(left: LogicalAndExpression, right: Vec<Box<LogicalOrExpression>>) -> Self {
        Self {
            left,
            right,
        }
    }
}

pub struct LogicalAndExpression {
    pub left: BitwiseOrExpression,
    pub right: Vec<Box<LogicalAndExpression>>,
}

impl LogicalAndExpression {
    pub fn new(left: BitwiseOrExpression, right: Vec<Box<LogicalAndExpression>>) -> Self {
        Self {
            left,
            right,
        }
    }
}

pub struct BitwiseOrExpression {
    pub left: BitwiseXorExpression,
    pub right: Vec<BitwiseXorExpression>,
}

impl BitwiseOrExpression {
    pub fn new(left: BitwiseXorExpression, right: Vec<BitwiseXorExpression>) -> Self {
        Self {
            left,
            right,
        }
    }
}

pub struct BitwiseXorExpression {
    pub left: BitwiseAndExpression,
    pub right: Vec<BitwiseAndExpression>,
}

impl BitwiseXorExpression {
    pub fn new(left: BitwiseAndExpression, right: Vec<BitwiseAndExpression>) -> Self {
        Self {
            left,
            right,
        }
    }
}

pub struct BitwiseAndExpression {
    pub left: EqualExpression,
    pub right: Vec<EqualExpression>,
}

impl BitwiseAndExpression {
    pub fn new(left: EqualExpression, right: Vec<EqualExpression>) -> Self {
        Self {
            left,
            right,
        }
    }
}

pub struct EqualExpression {
    pub left: CompareExpression,
    pub slices: Vec<BinaryOpSlice<CompareExpression>>,
}

impl EqualExpression {
    pub fn new(left: CompareExpression, slices: Vec<BinaryOpSlice<CompareExpression>>) -> Self {
        Self {
            left,
            slices,
        }
    }
}

pub struct CompareExpression {
    pub left: ShiftExpression,
    pub slices: Vec<BinaryOpSlice<ShiftExpression>>,
}

impl CompareExpression {
    pub fn new(left: ShiftExpression, slices: Vec<BinaryOpSlice<ShiftExpression>>) -> Self {
        Self {
            left,
            slices,
        }
    }
}

pub struct ShiftExpression {
    pub left: PlusExpression,
    pub slices: Vec<BinaryOpSlice<PlusExpression>>,
}

impl ShiftExpression {
    pub fn new(left: PlusExpression, slices: Vec<BinaryOpSlice<PlusExpression>>) -> Self {
        Self {
            left,
            slices,
        }
    }
}

pub struct PlusExpression {
    pub left: MultiplyExpression,
    pub slices: Vec<BinaryOpSlice<MultiplyExpression>>,
}

impl PlusExpression {
    pub fn new(left: MultiplyExpression, slices: Vec<BinaryOpSlice<MultiplyExpression>>) -> Self {
        Self {
            left,
            slices,
        }
    }
}

pub struct MultiplyExpression {
    pub left: PrefixedExpression,
    pub slices: Vec<BinaryOpSlice<PrefixedExpression>>,
}

impl MultiplyExpression {
    pub fn new(left: PrefixedExpression, slices: Vec<BinaryOpSlice<PrefixedExpression>>) -> Self {
        Self {
            left,
            slices,
        }
    }
}

pub struct PrefixedExpression {
    pub factor: Factor,
    pub expr_types: Vec<PrefixedExpressionType>,
}

impl PrefixedExpression {
    pub fn new(factor: Factor, expr_types: Vec<PrefixedExpressionType>) -> Self {
        Self {
            factor,
            expr_types,
        }
    }
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