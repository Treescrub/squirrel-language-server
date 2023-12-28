use crate::{lexer::{TokenType, Token}, source_info::SourceRange};

pub struct AstNode<T> {
    pub range: SourceRange,
    pub value: Box<T>,
}

impl<T> AstNode<T> {
    pub fn new(range: SourceRange, value: T) -> Self {
        Self {
            range,
            value: Box::new(value),
        }
    }
}

pub struct Script {
    pub statements: AstNode<Statements>,
}

impl Script {
    pub fn new(statements: Vec<AstNode<Statement>>) -> Self {
        let mut range = SourceRange::new_empty();

        if !statements.is_empty() {
            range = SourceRange::new(statements.get(0).unwrap().range.start, statements.get(statements.len() - 1).unwrap().range.end);
        }

        Self {
            statements: AstNode::new(range, Statements::new(statements)),
        }
    }
}

pub struct Statements {
    pub statements: Vec<AstNode<Statement>>,
}

impl Statements {
    pub fn new(statements: Vec<AstNode<Statement>>) -> Self {
        Self {
            statements,
        }
    }
}

pub enum Statement {
    If(AstNode<CommaExpression>, AstNode<Statement>, Option<AstNode<Statement>>),
    While(AstNode<CommaExpression>, AstNode<Statement>),
    DoWhile(AstNode<Statement>, AstNode<CommaExpression>),
    For(Option<AstNode<ForInit>>, Option<AstNode<CommaExpression>>, Option<AstNode<CommaExpression>>, AstNode<Statement>),
    ForEach(AstNode<Identifier> /* value */, Option<AstNode<Identifier>> /* key */, AstNode<Expression>, AstNode<Statement>),
    Switch(AstNode<CommaExpression>, Vec<AstNode<SwitchCase>>, Option<AstNode<Statements>> /* default */),
    LocalDeclare(AstNode<LocalDeclare>),
    Return(Option<AstNode<CommaExpression>>),
    Yield(Option<AstNode<CommaExpression>>),
    Break,
    Continue,
    Function(AstNode<FunctionIdentifier>, Option<AstNode<Expression>>/* static env binding */, AstNode<FunctionParams>, AstNode<Statement>),
    Class(AstNode<PrefixedExpression>, AstNode<ClassExpression>),
    Enum(AstNode<Identifier>, AstNode<EnumValues>),
    StatementBlock(AstNode<Statements>),
    TryCatch(AstNode<Statement>, AstNode<Identifier>, AstNode<Statement>),
    Throw(AstNode<CommaExpression>),
    Const(AstNode<Identifier>, AstNode<Scalar>),
    CommaExpression(AstNode<CommaExpression>)
}

pub struct EnumValues {
    pub values: Vec<AstNode<EnumEntry>>,
}

impl EnumValues {
    pub fn new(values: Vec<AstNode<EnumEntry>>) -> Self {
        Self {
            values,
        }
    }
}

pub struct EnumEntry {
    pub key: AstNode<Identifier>,
    pub value: Option<AstNode<Scalar>>,
}

impl EnumEntry {
    pub fn new(key: AstNode<Identifier>, value: Option<AstNode<Scalar>>) -> Self {
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
    Scalar(AstNode<Scalar>),
    Base,
    Identifier(AstNode<Identifier>),
    Constructor,
    This,
    DoubleColon(AstNode<PrefixedExpression>),
    Null,
    ArrayInit(Vec<AstNode<Expression>>),
    TableInit(AstNode<Table>),
    FunctionExpression(Option<AstNode<Expression>> /* bind env */, AstNode<FunctionParams>, AstNode<Statement>),
    LambdaExpression(Option<AstNode<Expression>> /* bind env */, AstNode<FunctionParams>, AstNode<Expression>),
    ClassExpression(AstNode<ClassExpression>),
    UnaryOp(AstNode<UnaryOp>),
    RawCall(AstNode<FunctionCallArgs>),
    Delete(AstNode<PrefixedExpression>),
    ParenExpression(AstNode<CommaExpression>),
    LineInfo,
    FileInfo,
}

pub struct ClassExpression {
    pub base_class: Option<AstNode<Expression>>,
    pub attributes: Option<AstNode<Table>>,
    pub body: AstNode<Table>,
}

impl ClassExpression {
    pub fn new(base_class: Option<AstNode<Expression>>, attributes: Option<AstNode<Table>>, body: AstNode<Table>) -> Self {
        Self {
            base_class,
            attributes,
            body,
        }
    }
}

pub struct Table {
    pub entries: Vec<AstNode<TableEntry>>,
}

impl Table {
    pub fn new(entries: Vec<AstNode<TableEntry>>) -> Self {
        Self {
            entries,
        }
    }
}

pub enum TableEntry {
    Function(AstNode<Identifier>, AstNode<FunctionParams>, AstNode<Statement>),
    Constructor(AstNode<FunctionParams>, AstNode<Statement>),
    DynamicAssign(AstNode<CommaExpression>, AstNode<Expression>),
    JsonStyle(String, AstNode<Expression>),
    Simple(AstNode<Identifier>, AstNode<Expression>),
    Attributes(AstNode<Table>),
}

pub struct UnaryOp {
    pub operator: AstNode<TokenType>,
    pub expression: AstNode<PrefixedExpression>,
}

impl UnaryOp {
    pub fn new(operator: AstNode<TokenType>, expression: AstNode<PrefixedExpression>) -> Self {
        Self {
            operator,
            expression,
        }
    }
}

pub struct FunctionIdentifier {
    pub identifiers: Vec<AstNode<Identifier>>,
}

impl FunctionIdentifier {
    pub fn new(identifiers: Vec<AstNode<Identifier>>) -> Self {
        Self {
            identifiers,
        }
    }
}

pub struct FunctionParams {
    pub params: Vec<AstNode<FunctionParam>>,
}

impl FunctionParams {
    pub fn new(params: Vec<AstNode<FunctionParam>>) -> Self {
        Self {
            params,
        }
    }
}

pub enum FunctionParam {
    Normal(AstNode<Identifier>),
    Default(AstNode<Identifier>, AstNode<Expression>),
    VarParams,
}

pub struct FunctionCallArgs {
    pub args: Vec<AstNode<Expression>>,
    pub post_call_init: Option<AstNode<PostCallInitialize>>,
}

impl FunctionCallArgs {
    pub fn new(args: Vec<AstNode<Expression>>, post_call_init: Option<AstNode<PostCallInitialize>>) -> Self {
        Self {
            args,
            post_call_init,
        }
    }
}

pub struct PostCallInitialize {
    pub entries: Vec<AstNode<PostCallInitializeEntry>>,
}

impl PostCallInitialize {
    pub fn new(entries: Vec<AstNode<PostCallInitializeEntry>>) -> Self {
        Self {
            entries,
        }
    }
}

pub enum PostCallInitializeEntry {
    ArrayStyle(AstNode<CommaExpression>, AstNode<Expression>),
    TableStyle(AstNode<Identifier>, AstNode<Expression>),
}

pub struct SwitchCase {
    pub condition: AstNode<Expression>,
    pub body: AstNode<Statements>,
}

impl SwitchCase {
    pub fn new(condition: AstNode<Expression>, body: AstNode<Statements>) -> Self {
        Self {
            condition,
            body,
        }
    }
}

pub enum LocalDeclare {
    Function(AstNode<Identifier>, Option<AstNode<Expression>>/*bind env*/, AstNode<FunctionParams>, AstNode<Statement>),
    Assign(Vec<AstNode<AssignExpression>>),
}

pub struct AssignExpression {
    pub identifier: AstNode<Identifier>,
    pub value: Option<AstNode<Expression>>,
}

impl AssignExpression {
    pub fn new(identifier: AstNode<Identifier>, value: Option<AstNode<Expression>>) -> Self {
        Self {
            identifier,
            value,
        }
    }
}

pub enum ForInit {
    LocalDeclare(AstNode<LocalDeclare>),
    CommaExpression(AstNode<CommaExpression>),
}

pub struct CommaExpression {
    pub expressions: Vec<AstNode<Expression>>,
}

impl CommaExpression {
    pub fn new(expressions: Vec<AstNode<Expression>>) -> Self {
        Self {
            expressions,
        }
    }
}

pub struct Expression {
    pub logical_or: AstNode<LogicalOrExpression>,
    pub expr_type: Option<AstNode<ExpressionType>>
}

impl Expression {
    pub fn new(logical_or: AstNode<LogicalOrExpression>, expr_type: Option<AstNode<ExpressionType>>) -> Self {
        Self {
            logical_or,
            expr_type,
        }
    }
}

pub enum ExpressionType {
    Newslot(AstNode<Expression>),
    Assign(AstNode<Expression>),
    MinusEqual(AstNode<Expression>),
    PlusEqual(AstNode<Expression>),
    MultiplyEqual(AstNode<Expression>),
    DivideEqual(AstNode<Expression>),
    Ternary(AstNode<Expression>, AstNode<Expression>),
}

pub struct BinaryOpSlice<T> {
    pub operator: AstNode<TokenType>,
    pub right: AstNode<T>,
}

impl<T> BinaryOpSlice<T> {
    pub fn new(operator: AstNode<TokenType>, right: AstNode<T>) -> BinaryOpSlice<T> {
        return BinaryOpSlice {
            operator,
            right,
        }
    }
}

pub struct LogicalOrExpression {
    pub left: AstNode<LogicalAndExpression>,
    pub right: Vec<AstNode<LogicalOrExpression>>,
}

impl LogicalOrExpression {
    pub fn new(left: AstNode<LogicalAndExpression>, right: Vec<AstNode<LogicalOrExpression>>) -> Self {
        Self {
            left,
            right,
        }
    }
}

pub struct LogicalAndExpression {
    pub left: AstNode<BitwiseOrExpression>,
    pub right: Vec<AstNode<LogicalAndExpression>>,
}

impl LogicalAndExpression {
    pub fn new(left: AstNode<BitwiseOrExpression>, right: Vec<AstNode<LogicalAndExpression>>) -> Self {
        Self {
            left,
            right,
        }
    }
}

pub struct BitwiseOrExpression {
    pub left: AstNode<BitwiseXorExpression>,
    pub right: Vec<AstNode<BitwiseXorExpression>>,
}

impl BitwiseOrExpression {
    pub fn new(left: AstNode<BitwiseXorExpression>, right: Vec<AstNode<BitwiseXorExpression>>) -> Self {
        Self {
            left,
            right,
        }
    }
}

pub struct BitwiseXorExpression {
    pub left: AstNode<BitwiseAndExpression>,
    pub right: Vec<AstNode<BitwiseAndExpression>>,
}

impl BitwiseXorExpression {
    pub fn new(left: AstNode<BitwiseAndExpression>, right: Vec<AstNode<BitwiseAndExpression>>) -> Self {
        Self {
            left,
            right,
        }
    }
}

pub struct BitwiseAndExpression {
    pub left: AstNode<EqualExpression>,
    pub right: Vec<AstNode<EqualExpression>>,
}

impl BitwiseAndExpression {
    pub fn new(left: AstNode<EqualExpression>, right: Vec<AstNode<EqualExpression>>) -> Self {
        Self {
            left,
            right,
        }
    }
}

pub struct EqualExpression {
    pub left: AstNode<CompareExpression>,
    pub slices: Vec<AstNode<BinaryOpSlice<CompareExpression>>>,
}

impl EqualExpression {
    pub fn new(left: AstNode<CompareExpression>, slices: Vec<AstNode<BinaryOpSlice<CompareExpression>>>) -> Self {
        Self {
            left,
            slices,
        }
    }
}

pub struct CompareExpression {
    pub left: AstNode<ShiftExpression>,
    pub slices: Vec<AstNode<BinaryOpSlice<ShiftExpression>>>,
}

impl CompareExpression {
    pub fn new(left: AstNode<ShiftExpression>, slices: Vec<AstNode<BinaryOpSlice<ShiftExpression>>>) -> Self {
        Self {
            left,
            slices,
        }
    }
}

pub struct ShiftExpression {
    pub left: AstNode<PlusExpression>,
    pub slices: Vec<AstNode<BinaryOpSlice<PlusExpression>>>,
}

impl ShiftExpression {
    pub fn new(left: AstNode<PlusExpression>, slices: Vec<AstNode<BinaryOpSlice<PlusExpression>>>) -> Self {
        Self {
            left,
            slices,
        }
    }
}

pub struct PlusExpression {
    pub left: AstNode<MultiplyExpression>,
    pub slices: Vec<AstNode<BinaryOpSlice<MultiplyExpression>>>,
}

impl PlusExpression {
    pub fn new(left: AstNode<MultiplyExpression>, slices: Vec<AstNode<BinaryOpSlice<MultiplyExpression>>>) -> Self {
        Self {
            left,
            slices,
        }
    }
}

pub struct MultiplyExpression {
    pub left: AstNode<PrefixedExpression>,
    pub slices: Vec<AstNode<BinaryOpSlice<PrefixedExpression>>>,
}

impl MultiplyExpression {
    pub fn new(left: AstNode<PrefixedExpression>, slices: Vec<AstNode<BinaryOpSlice<PrefixedExpression>>>) -> Self {
        Self {
            left,
            slices,
        }
    }
}

pub struct PrefixedExpression {
    pub factor: AstNode<Factor>,
    pub expr_types: Vec<AstNode<PrefixedExpressionType>>,
}

impl PrefixedExpression {
    pub fn new(factor: AstNode<Factor>, expr_types: Vec<AstNode<PrefixedExpressionType>>) -> Self {
        Self {
            factor,
            expr_types,
        }
    }
}

pub enum PrefixedExpressionType {
    DotAccess(AstNode<Identifier>),
    ArrayStyleAccess(AstNode<Expression>),
    PostIncrement,
    PostDecrement,
    FunctionCall(AstNode<FunctionCallArgs>),
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