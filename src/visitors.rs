use crate::ast::*;

pub trait SimpleVisitor {
    fn visit_script(&self, script: Script) {
        for statement in script.statements {
            self.visit_statement(statement);
        }
    }
    fn visit_statements(&self, statements: Statements) {
        for statement in statements.statements {
            self.visit_statement(statement);
        }
    }
    fn visit_statement(&self, statement: Statement) {
        match statement {
            Statement::If(condition, if_block, else_block) => {
                self.visit_comma_expr(condition);
                self.visit_statement(*if_block);
                if else_block.is_some() {
                    self.visit_statement(*else_block.unwrap());
                }
            }
            Statement::While(condition, loop_body) => {
                self.visit_comma_expr(condition);
                self.visit_statement(*loop_body);
            }
            Statement::DoWhile(loop_body, condition) => {
                self.visit_statement(*loop_body);
                self.visit_comma_expr(condition);
            }
            _ => {todo!()}
        }
    }
    fn visit_local_declare(&self, local_declare: LocalDeclare) {
        match local_declare {
            LocalDeclare::Function(identifier, bind_env, params) => {
                self.visit_identifier(identifier);
                if let Some(bind_env) = bind_env {
                    self.visit_expression(bind_env);
                }
                self.visit_func_params(params);
            }
            LocalDeclare::Assign(assign_exprs) => {
                for assign_expr in assign_exprs {
                    self.visit_assign_expr(assign_expr);
                }
            }
        }
    }
    fn visit_assign_expr(&self, assign_expr: AssignExpression) {
        self.visit_identifier(assign_expr.identifier);
        if let Some(expression) = assign_expr.value {
            self.visit_expression(expression);
        }
    }
    fn visit_for_init(&self, for_init: ForInit) {
        match for_init {
            ForInit::LocalDeclare(local_declare) => {
                self.visit_local_declare(local_declare);
            }
            ForInit::CommaExpression(comma_expr) => {
                self.visit_comma_expr(comma_expr);
            }
        }
    }
    fn visit_unary_op(&self, unary_op: UnaryOp) {
        self.visit_prefixed_exp(unary_op.expression);
    }
    fn visit_func_params(&self, params: FunctionParams) {
        for param in params.params {
            self.visit_func_param(param);
        }
    }
    fn visit_func_param(&self, param: FunctionParam) {
        match param {
            FunctionParam::Normal(identifier) => self.visit_identifier(identifier),
            FunctionParam::Default(identifier, default_val) => {
                self.visit_identifier(identifier);
                self.visit_expression(default_val);
            },
            FunctionParam::VarParams => {},
        }
    }
    fn visit_table(&self, table: Table) {
        for entry in table.entries {
            self.visit_table_entry(entry);
        }
    }
    fn visit_table_entry(&self, entry: TableEntry) {
        match entry {
            TableEntry::Function(name, params, body) => {
                self.visit_identifier(name);
                self.visit_func_params(params);
                self.visit_statement(body);
            },
            TableEntry::Constructor(params, body) => {
                self.visit_func_params(params);
                self.visit_statement(body);
            },
            TableEntry::DynamicAssign(key, value) => {
                self.visit_comma_expr(key);
                self.visit_expression(value);
            },
            TableEntry::JsonStyle(_key, value) => {
                self.visit_expression(value);
            },
            TableEntry::Simple(key, value) => {
                self.visit_identifier(key);
                self.visit_expression(value);
            },
            TableEntry::Attributes(table) => {
                self.visit_table(table);
            },
        }
    }
    fn visit_expression(&self, expression: Expression) {
        self.visit_logical_or_exp(*expression.logical_or);

        if expression.expr_type.is_none() {
            return;
        }

        match expression.expr_type.unwrap() {
            ExpressionType::Newslot(value) => self.visit_expression(*value),
            ExpressionType::Assign(value) => self.visit_expression(*value),
            ExpressionType::MinusEqual(value) => self.visit_expression(*value),
            ExpressionType::PlusEqual(value) => self.visit_expression(*value),
            ExpressionType::MultiplyEqual(value) => self.visit_expression(*value),
            ExpressionType::DivideEqual(value) => self.visit_expression(*value),
            ExpressionType::Ternary(true_case, false_case) => {
                self.visit_expression(*true_case);
                self.visit_expression(*false_case);
            },
        }
    }
    fn visit_class_expr(&self, class_expression: ClassExpression) {
        if let Some(base_class) = class_expression.base_class {
            self.visit_expression(base_class);
        }

        if let Some(attributes) = class_expression.attributes {
            self.visit_table(attributes);
        }

        self.visit_table(class_expression.body);
    }
    fn visit_comma_expr(&self, comma_expression: CommaExpression) {
        for expression in comma_expression.expressions {
            self.visit_expression(expression);
        }
    }
    fn visit_logical_or_exp(&self, logical_or: LogicalOrExpression) {
        self.visit_logical_and_exp(logical_or.left);

        for expression in logical_or.right {
            self.visit_logical_or_exp(*expression);
        }
    }
    fn visit_logical_and_exp(&self, logical_and: LogicalAndExpression) {
        self.visit_bitwise_or_exp(logical_and.left);
        
        for expression in logical_and.right {
            self.visit_logical_and_exp(*expression);
        }
    }
    fn visit_bitwise_or_exp(&self, bitwise_or: BitwiseOrExpression) {
        self.visit_bitwise_xor_exp(bitwise_or.left);

        for expression in bitwise_or.right {
            self.visit_bitwise_xor_exp(expression);
        }
    }
    fn visit_bitwise_xor_exp(&self, bitwise_xor: BitwiseXorExpression) {
        self.visit_bitwise_and_exp(bitwise_xor.left);

        for expression in bitwise_xor.right {
            self.visit_bitwise_and_exp(expression);
        }
    }
    fn visit_bitwise_and_exp(&self, bitwise_and: BitwiseAndExpression) {
        self.visit_equal_exp(bitwise_and.left);

        for expression in bitwise_and.right {
            self.visit_equal_exp(expression);
        }
    }
    fn visit_equal_exp(&self, equal: EqualExpression) {
        self.visit_compare_exp(equal.left);

        for expression in equal.slices {
            self.visit_compare_exp(expression.right);
        }
    }
    fn visit_compare_exp(&self, compare: CompareExpression) {
        self.visit_shift_exp(compare.left);

        for expression in compare.slices {
            self.visit_shift_exp(expression.right);
        }
    }
    fn visit_shift_exp(&self, shift: ShiftExpression) {
        self.visit_plus_exp(shift.left);

        for expression in shift.slices {
            self.visit_plus_exp(expression.right);
        }
    }
    fn visit_plus_exp(&self, plus: PlusExpression) {
        self.visit_multiply_exp(plus.left);

        for expression in plus.slices {
            self.visit_multiply_exp(expression.right);
        }
    }
    fn visit_multiply_exp(&self, multiply: MultiplyExpression) {
        self.visit_prefixed_exp(multiply.left);

        for expression in multiply.slices {
            self.visit_prefixed_exp(expression.right);
        }
    }
    fn visit_prefixed_exp(&self, prefixed: PrefixedExpression) {
        self.visit_factor(prefixed.factor);

        for expression in prefixed.expr_types {
            match expression {
                PrefixedExpressionType::DotAccess(identifier) => self.visit_identifier(identifier),
                PrefixedExpressionType::ArrayStyleAccess(expression) => self.visit_expression(expression),
                PrefixedExpressionType::PostIncrement => {},
                PrefixedExpressionType::PostDecrement => {},
                PrefixedExpressionType::FunctionCall(_) => todo!(),
            }
        }
    }
    fn visit_scalar(&self, _scalar: Scalar) {}
    fn visit_factor(&self, _factor: Factor) {}
    fn visit_identifier(&self, _identifier: Identifier) {}
}

pub trait SimpleVisitorMut {
    fn visit_script(&mut self, script: Script) {
        for statement in script.statements {
            self.visit_statement(statement);
        }
    }
    fn visit_statements(&mut self, statements: Statements) {
        for statement in statements.statements {
            self.visit_statement(statement);
        }
    }
    fn visit_statement(&mut self, statement: Statement) {
        match statement {
            Statement::If(condition, if_block, else_block) => {
                self.visit_comma_expr(condition);
                self.visit_statement(*if_block);
                if else_block.is_some() {
                    self.visit_statement(*else_block.unwrap());
                }
            }
            Statement::While(condition, loop_body) => {
                self.visit_comma_expr(condition);
                self.visit_statement(*loop_body);
            }
            Statement::DoWhile(loop_body, condition) => {
                self.visit_statement(*loop_body);
                self.visit_comma_expr(condition);
            }
            _ => {todo!()}
        }
    }
    fn visit_local_declare(&mut self, local_declare: LocalDeclare) {
        match local_declare {
            LocalDeclare::Function(identifier, bind_env, params) => {
                self.visit_identifier(identifier);
                if let Some(bind_env) = bind_env {
                    self.visit_expression(bind_env);
                }
                self.visit_func_params(params);
            }
            LocalDeclare::Assign(assign_exprs) => {
                for assign_expr in assign_exprs {
                    self.visit_assign_expr(assign_expr);
                }
            }
        }
    }
    fn visit_assign_expr(&mut self, assign_expr: AssignExpression) {
        self.visit_identifier(assign_expr.identifier);
        if let Some(expression) = assign_expr.value {
            self.visit_expression(expression);
        }
    }
    fn visit_for_init(&mut self, for_init: ForInit) {
        match for_init {
            ForInit::LocalDeclare(local_declare) => {
                self.visit_local_declare(local_declare);
            }
            ForInit::CommaExpression(comma_expr) => {
                self.visit_comma_expr(comma_expr);
            }
        }
    }
    fn visit_unary_op(&mut self, unary_op: UnaryOp) {
        self.visit_prefixed_exp(unary_op.expression);
    }
    fn visit_func_params(&mut self, params: FunctionParams) {
        for param in params.params {
            self.visit_func_param(param);
        }
    }
    fn visit_func_param(&mut self, param: FunctionParam) {
        match param {
            FunctionParam::Normal(identifier) => self.visit_identifier(identifier),
            FunctionParam::Default(identifier, default_val) => {
                self.visit_identifier(identifier);
                self.visit_expression(default_val);
            },
            FunctionParam::VarParams => {},
        }
    }
    fn visit_table(&mut self, table: Table) {
        for entry in table.entries {
            self.visit_table_entry(entry);
        }
    }
    fn visit_table_entry(&mut self, entry: TableEntry) {
        match entry {
            TableEntry::Function(name, params, body) => {
                self.visit_identifier(name);
                self.visit_func_params(params);
                self.visit_statement(body);
            },
            TableEntry::Constructor(params, body) => {
                self.visit_func_params(params);
                self.visit_statement(body);
            },
            TableEntry::DynamicAssign(key, value) => {
                self.visit_comma_expr(key);
                self.visit_expression(value);
            },
            TableEntry::JsonStyle(_key, value) => {
                self.visit_expression(value);
            },
            TableEntry::Simple(key, value) => {
                self.visit_identifier(key);
                self.visit_expression(value);
            },
            TableEntry::Attributes(table) => {
                self.visit_table(table);
            },
        }
    }
    fn visit_expression(&mut self, expression: Expression) {
        self.visit_logical_or_exp(*expression.logical_or);

        if expression.expr_type.is_none() {
            return;
        }

        match expression.expr_type.unwrap() {
            ExpressionType::Newslot(value) => self.visit_expression(*value),
            ExpressionType::Assign(value) => self.visit_expression(*value),
            ExpressionType::MinusEqual(value) => self.visit_expression(*value),
            ExpressionType::PlusEqual(value) => self.visit_expression(*value),
            ExpressionType::MultiplyEqual(value) => self.visit_expression(*value),
            ExpressionType::DivideEqual(value) => self.visit_expression(*value),
            ExpressionType::Ternary(true_case, false_case) => {
                self.visit_expression(*true_case);
                self.visit_expression(*false_case);
            },
        }
    }
    fn visit_class_expr(&mut self, class_expression: ClassExpression) {
        if let Some(base_class) = class_expression.base_class {
            self.visit_expression(base_class);
        }

        if let Some(attributes) = class_expression.attributes {
            self.visit_table(attributes);
        }

        self.visit_table(class_expression.body);
    }
    fn visit_comma_expr(&mut self, comma_expression: CommaExpression) {
        for expression in comma_expression.expressions {
            self.visit_expression(expression);
        }
    }
    fn visit_logical_or_exp(&mut self, logical_or: LogicalOrExpression) {
        self.visit_logical_and_exp(logical_or.left);

        for expression in logical_or.right {
            self.visit_logical_or_exp(*expression);
        }
    }
    fn visit_logical_and_exp(&mut self, logical_and: LogicalAndExpression) {
        self.visit_bitwise_or_exp(logical_and.left);
        
        for expression in logical_and.right {
            self.visit_logical_and_exp(*expression);
        }
    }
    fn visit_bitwise_or_exp(&mut self, bitwise_or: BitwiseOrExpression) {
        self.visit_bitwise_xor_exp(bitwise_or.left);

        for expression in bitwise_or.right {
            self.visit_bitwise_xor_exp(expression);
        }
    }
    fn visit_bitwise_xor_exp(&mut self, bitwise_xor: BitwiseXorExpression) {
        self.visit_bitwise_and_exp(bitwise_xor.left);

        for expression in bitwise_xor.right {
            self.visit_bitwise_and_exp(expression);
        }
    }
    fn visit_bitwise_and_exp(&mut self, bitwise_and: BitwiseAndExpression) {
        self.visit_equal_exp(bitwise_and.left);

        for expression in bitwise_and.right {
            self.visit_equal_exp(expression);
        }
    }
    fn visit_equal_exp(&mut self, equal: EqualExpression) {
        self.visit_compare_exp(equal.left);

        for expression in equal.slices {
            self.visit_compare_exp(expression.right);
        }
    }
    fn visit_compare_exp(&mut self, compare: CompareExpression) {
        self.visit_shift_exp(compare.left);

        for expression in compare.slices {
            self.visit_shift_exp(expression.right);
        }
    }
    fn visit_shift_exp(&mut self, shift: ShiftExpression) {
        self.visit_plus_exp(shift.left);

        for expression in shift.slices {
            self.visit_plus_exp(expression.right);
        }
    }
    fn visit_plus_exp(&mut self, plus: PlusExpression) {
        self.visit_multiply_exp(plus.left);

        for expression in plus.slices {
            self.visit_multiply_exp(expression.right);
        }
    }
    fn visit_multiply_exp(&mut self, multiply: MultiplyExpression) {
        self.visit_prefixed_exp(multiply.left);

        for expression in multiply.slices {
            self.visit_prefixed_exp(expression.right);
        }
    }
    fn visit_prefixed_exp(&mut self, prefixed: PrefixedExpression) {
        self.visit_factor(prefixed.factor);

        for expression in prefixed.expr_types {
            match expression {
                PrefixedExpressionType::DotAccess(identifier) => self.visit_identifier(identifier),
                PrefixedExpressionType::ArrayStyleAccess(expression) => self.visit_expression(expression),
                PrefixedExpressionType::PostIncrement => {},
                PrefixedExpressionType::PostDecrement => {},
                PrefixedExpressionType::FunctionCall(_) => todo!(),
            }
        }
    }
    fn visit_scalar(&mut self, _scalar: Scalar) {}
    fn visit_factor(&mut self, _factor: Factor) {}
    fn visit_identifier(&mut self, _identifier: Identifier) {}
}

pub struct PrettyPrinter {
    pub text: String,
    indentation: u32,
}

impl PrettyPrinter {
    pub fn new() -> Self {
        Self {
            text: String::new(),
            indentation: 0,
        }
    }

    fn push_level(&mut self) {
        self.indentation += 1;
    }

    fn pop_level(&mut self) {
        self.indentation -= 1;
    }

    fn add_indents(&mut self) {
        for _ in 0..self.indentation {
            self.text.push_str("  ");
        }
    }

    fn print(&mut self, text: &str) {
        self.add_indents();
        self.text.push_str(text);
        self.text.push('\n');
    }
}

impl SimpleVisitorMut for PrettyPrinter {
    fn visit_script(&mut self, script: Script) {
        self.print(&format!("SCRIPT: {} statement(s)", script.statements.len()));

        for statement in script.statements {
            self.visit_statement(statement);
        }
    }

    fn visit_statements(&mut self, statements: Statements) {
        self.push_level();

        self.print("STATEMENTS");
        for statement in statements.statements {
            self.visit_statement(statement);
        }

        self.pop_level();
    }

    fn visit_statement(&mut self, statement: Statement) {
        self.push_level();
        self.print("STATEMENT");
        match statement {
            Statement::Break => {
                self.push_level();
                self.print("BREAK");
                self.pop_level();
            }
            Statement::Continue => {
                self.push_level();
                self.print("CONTINUE");
                self.pop_level();
            }
            Statement::Const(id, _scalar) => {
                self.push_level();
                self.print("CONST");
                self.visit_identifier(id);
                self.pop_level();
            }
            Statement::StatementBlock(statements) => self.visit_statements(statements),
            Statement::If(condition, if_block, else_block) => {
                self.push_level();
                self.print("IF");
                self.visit_comma_expr(condition);
                self.visit_statement(*if_block);
                if else_block.is_some() {
                    self.visit_statement(*else_block.unwrap());
                }
                self.pop_level();
            }
            Statement::CommaExpression(comma_expression) => {
                self.push_level();
                self.print("COMMA EXPRESSION");
                for expression in comma_expression.expressions {
                    self.visit_expression(expression);
                }
                self.pop_level();
            }
            Statement::Class(name, body) => {
                self.push_level();
                self.print("CLASS");
                self.visit_prefixed_exp(name);
                self.visit_class_expr(body);
                self.pop_level();
            }
            Statement::Function(identifier, bind_env, params, body) => {
                self.push_level();
                self.print("FUNCTION");
                for identifier in identifier.identifiers {
                    self.visit_identifier(identifier);
                }
                if let Some(bind_env) = bind_env {
                    self.visit_expression(bind_env);
                }
                self.visit_func_params(params);
                self.visit_statement(*body);
                self.pop_level();
            }
            Statement::While(condition, body) => {
                self.push_level();
                self.print("WHILE");
                self.visit_comma_expr(condition);
                self.visit_statement(*body);
                self.pop_level();
            },
            Statement::DoWhile(body, condition) => {
                self.push_level();
                self.print("DO WHILE");
                self.visit_statement(*body);
                self.visit_comma_expr(condition);
                self.pop_level();
            },
            Statement::For(init, condition, post, body) => {
                self.push_level();
                self.print("FOR");
                if let Some(init) = init {
                    self.visit_for_init(init);
                }
                if let Some(condition) = condition {
                    self.visit_comma_expr(condition);
                }
                if let Some(post) = post {
                    self.visit_comma_expr(post);
                }
                self.visit_statement(*body);
            },
            Statement::ForEach(value, key, container, body) => {
                self.push_level();
                self.print("FOR EACH");
                self.visit_identifier(value);
                if let Some(key) = key {
                    self.visit_identifier(key);
                }
                self.visit_expression(container);
                self.visit_statement(*body);
                self.pop_level();
            },
            Statement::Switch(value, cases, default) => {
                self.push_level();
                self.print("SWITCH");
                self.visit_comma_expr(value);
                for case in cases {
                    self.visit_expression(case.condition);
                    self.visit_statements(case.body);
                }
                if let Some(default) = default {
                    self.visit_statements(default);
                }
                self.pop_level();
            },
            Statement::LocalDeclare(local_declare) => {
                self.push_level();
                self.print("LOCAL DECLARE");
                self.visit_local_declare(local_declare);
                self.pop_level();
            },
            Statement::Return(value) => {
                self.push_level();
                self.print("RETURN");
                if let Some(value) = value {
                    self.visit_comma_expr(value);
                }
                self.pop_level();
            },
            Statement::Yield(value) => {
                self.push_level();
                self.print("YIELD");
                if let Some(value) = value {
                    self.visit_comma_expr(value);
                }
                self.pop_level();
            },
            Statement::Enum(identifier, values) => {
                self.push_level();
                self.print("ENUM");
                self.visit_identifier(identifier);
                for entry in values.values {
                    self.visit_identifier(entry.key);
                    if let Some(value) = entry.value {
                        self.visit_scalar(value);
                    }
                }
                self.pop_level();
            },
            Statement::TryCatch(try_body, exception_id, catch_body) => {
                self.push_level();
                self.print("TRY CATCH");
                self.visit_statement(*try_body);
                self.visit_identifier(exception_id);
                self.visit_statement(*catch_body);
                self.pop_level();
            },
            Statement::Throw(value) => {
                self.push_level();
                self.print("THROW");
                self.visit_comma_expr(value);
                self.pop_level();
            },
        }

        self.pop_level();
    }

    fn visit_local_declare(&mut self, local_declare: LocalDeclare) {
        match local_declare {
            LocalDeclare::Function(identifier, bind_env, params) => {
                self.push_level();
                self.print("FUNCTION");
                self.visit_identifier(identifier);
                if let Some(bind_env) = bind_env {
                    self.visit_expression(bind_env);
                }
                self.visit_func_params(params);
                self.pop_level();
            }
            LocalDeclare::Assign(assign_exprs) => {
                self.push_level();
                self.print("ASSIGN");
                for assign_expr in assign_exprs {
                    self.visit_assign_expr(assign_expr);
                }
                self.pop_level();
            }
        }
    }

    fn visit_assign_expr(&mut self, assign_expr: AssignExpression) {
        self.push_level();
        self.print("ASSIGN EXPRESSION");
        self.visit_identifier(assign_expr.identifier);
        if let Some(expression) = assign_expr.value {
            self.visit_expression(expression);
        }
        self.pop_level();
    }

    fn visit_for_init(&mut self, for_init: ForInit) {
        self.push_level();
        match for_init {
            ForInit::LocalDeclare(local_declare) => {
                self.print("LOCAL DECLARE");
                self.visit_local_declare(local_declare);
            }
            ForInit::CommaExpression(comma_expr) => {
                self.print("COMMA EXPRESSION");
                self.visit_comma_expr(comma_expr);
            }
        }
        self.pop_level();
    }

    fn visit_unary_op(&mut self, unary_op: UnaryOp) {
        self.push_level();
        self.print("UNARY OP");
        self.visit_prefixed_exp(unary_op.expression);
        self.pop_level();
    }

    fn visit_logical_or_exp(&mut self, logical_or: LogicalOrExpression) {
        if logical_or.right.is_empty() {
            self.visit_logical_and_exp(logical_or.left);
            return;
        }

        self.push_level();
        self.print("LOGICAL OR");
        self.pop_level();
    }

    fn visit_logical_and_exp(&mut self, logical_and: LogicalAndExpression) {
        if logical_and.right.is_empty() {
            self.visit_bitwise_or_exp(logical_and.left);
            return;
        }

        self.push_level();
        self.print("LOGICAL AND");
        self.pop_level();
    }

    fn visit_bitwise_or_exp(&mut self, bitwise_or: BitwiseOrExpression) {
        if bitwise_or.right.is_empty() {
            self.visit_bitwise_xor_exp(bitwise_or.left);
            return;
        }

        self.push_level();
        self.print("BITWISE OR");
        self.pop_level();
    }

    fn visit_bitwise_xor_exp(&mut self, bitwise_xor: BitwiseXorExpression) {
        if bitwise_xor.right.is_empty() {
            self.visit_bitwise_and_exp(bitwise_xor.left);
            return;
        }

        self.push_level();
        self.print("BITWISE XOR");
        self.pop_level();
    }

    fn visit_bitwise_and_exp(&mut self, bitwise_and: BitwiseAndExpression) {
        if bitwise_and.right.is_empty() {
            self.visit_equal_exp(bitwise_and.left);
            return;
        }

        self.push_level();
        self.print("BITWISE AND");
        self.pop_level();
    }

    fn visit_equal_exp(&mut self, equal: EqualExpression) {
        if equal.slices.is_empty() {
            self.visit_compare_exp(equal.left);
            return;
        }

        self.push_level();
        self.print("EQUAL");
        self.pop_level();
    }

    fn visit_compare_exp(&mut self, compare: CompareExpression) {
        if compare.slices.is_empty() {
            self.visit_shift_exp(compare.left);
            return;
        }

        self.push_level();
        self.print("COMPARE");
        self.pop_level();
    }

    fn visit_shift_exp(&mut self, shift: ShiftExpression) {
        if shift.slices.is_empty() {
            self.visit_plus_exp(shift.left);
            return;
        }

        self.push_level();
        self.print("SHIFT");
        self.pop_level();
    }

    fn visit_plus_exp(&mut self, plus: PlusExpression) {
        if plus.slices.is_empty() {
            self.visit_multiply_exp(plus.left);
            return;
        }

        self.push_level();
        self.print("PLUS");
        self.pop_level();
    }

    fn visit_multiply_exp(&mut self, multiply: MultiplyExpression) {
        if multiply.slices.is_empty() {
            self.visit_prefixed_exp(multiply.left);
            return;
        }

        self.push_level();
        self.print("MULTIPLY");
        self.pop_level();
    }

    fn visit_prefixed_exp(&mut self, prefixed: PrefixedExpression) {
        if prefixed.expr_types.is_empty() {
            self.visit_factor(prefixed.factor);
            return;
        }

        self.push_level();
        self.print("PREFIXED");
        self.pop_level();
    }

    fn visit_factor(&mut self, factor: Factor) {
        self.push_level();
        self.print("FACTOR");
        match factor {
            Factor::Scalar(scalar) => {
                self.push_level();
                self.print("SCALAR");
                self.visit_scalar(scalar);
                self.pop_level();
            },
            Factor::Base => self.print("BASE"),
            Factor::Identifier(identifier) => {
                self.push_level();
                self.visit_identifier(identifier);
                self.pop_level();
            },
            Factor::Constructor => self.print("CONSTRUCTOR"),
            Factor::This => self.print("THIS"),
            Factor::DoubleColon(prefixed_expression) => {
                self.push_level();
                self.print("DOUBLE_COLON");
                self.visit_prefixed_exp(*prefixed_expression);
                self.pop_level();
            },
            Factor::Null => self.print("NULL"),
            Factor::ArrayInit(entries) => {
                self.push_level();
                self.print("ARRAY");
                for entry in entries {
                    self.visit_expression(entry);
                }
                self.pop_level();
            },
            Factor::TableInit(table) => {
                self.push_level();
                self.print("TABLE");
                self.visit_table(table);
                self.pop_level();
            },
            Factor::FunctionExpression(bind_env, params, body) => {
                self.push_level();
                self.print("FUNCTION");
                if let Some(bind_env) = bind_env {
                    self.visit_expression(bind_env);
                }

                self.visit_func_params(params);
                self.visit_statement(*body);
                self.pop_level();
            },
            Factor::LambdaExpression(bind_env, params, body) => {
                self.push_level();
                self.print("LAMBDA EXPRESSION");
                if let Some(bind_env) = bind_env {
                    self.visit_expression(bind_env);
                }
                self.visit_func_params(params);
                self.visit_expression(body);
                self.pop_level();
            },
            Factor::ClassExpression(class_expression) => {
                self.push_level();
                self.print("CLASS EXPRESSION");
                self.visit_class_expr(class_expression);
                self.pop_level();
            },
            Factor::UnaryOp(unary_op) => {
                self.push_level();
                self.print("UNARY OP");
                self.visit_unary_op(*unary_op);
                self.pop_level();
            },
            Factor::RawCall(_) => todo!(),
            Factor::Delete(_) => todo!(),
            Factor::ParenExpression(expression) => {
                self.push_level();
                self.print("PAREN EXPRESSION");
                self.visit_comma_expr(expression);
                self.pop_level();
            },
            Factor::LineInfo => todo!(),
            Factor::FileInfo => todo!(),
        }
        self.pop_level();
    }

    fn visit_expression(&mut self, expression: Expression) {
        self.push_level();
        self.print("EXPRESSION");
        self.visit_logical_or_exp(*expression.logical_or);

        if let Some(expr_type) = expression.expr_type {
            match expr_type {
                ExpressionType::Newslot(expression) => {
                    self.push_level();
                    self.print("NEWSLOT");
                    self.visit_expression(*expression);
                    self.pop_level();
                },
                ExpressionType::Assign(expression) => {
                    self.push_level();
                    self.print("ASSIGN");
                    self.visit_expression(*expression);
                    self.pop_level();
                },
                ExpressionType::MinusEqual(expression) => {
                    self.push_level();
                    self.print("MINUS EQUAL");
                    self.visit_expression(*expression);
                    self.pop_level();
                },
                ExpressionType::PlusEqual(expression) => {
                    self.push_level();
                    self.print("PLUS EQUAL");
                    self.visit_expression(*expression);
                    self.pop_level();
                },
                ExpressionType::MultiplyEqual(expression) => {
                    self.push_level();
                    self.print("MULTIPLY EQUAL");
                    self.visit_expression(*expression);
                    self.pop_level();
                },
                ExpressionType::DivideEqual(expression) => {
                    self.push_level();
                    self.print("DIVIDE EQUAL");
                    self.visit_expression(*expression);
                    self.pop_level();
                },
                ExpressionType::Ternary(true_case, false_case) => {
                    self.push_level();
                    self.print("TERNARY");
                    self.visit_expression(*true_case);
                    self.visit_expression(*false_case);
                    self.pop_level();
                },
            }
        }
        self.pop_level();
    }

    fn visit_scalar(&mut self, scalar: Scalar) {
        self.push_level();
        match scalar {
            Scalar::Integer => self.print("INTEGER"),
            Scalar::Float => self.print("FLOAT"),
            Scalar::StringLiteral => self.print("STRING"),
            Scalar::True => self.print("TRUE"),
            Scalar::False => self.print("FALSE"),
        }
        self.pop_level();
    }

    fn visit_identifier(&mut self, _identifier: Identifier) {
        self.print("IDENTIFIER");
    }
}