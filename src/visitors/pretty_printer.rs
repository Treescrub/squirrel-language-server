use crate::analysis::ast::*;
use super::SimpleVisitorMut;


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
    fn visit_script(&mut self, script: &AstNode<Script>) {
        self.print(&script.range.to_string());
        self.print(&format!("SCRIPT: {} statement(s)", script.value.statements.value.statements.len()));

        self.push_level();
        self.walk_script(script);
        self.pop_level();
    }

    fn visit_statements(&mut self, statements: &AstNode<Statements>) {
        self.print(&statements.range.to_string());
        self.print("STATEMENTS");

        self.push_level();
        self.walk_statements(statements);
        self.pop_level();
    }

    fn visit_statement(&mut self, statement: &AstNode<Statement>) {
        self.print(&statement.range.to_string());
        self.print("STATEMENT");

        self.push_level();
        self.walk_statement(statement);
        self.pop_level();
    }

    fn visit_break_statement(&mut self) {
        self.print("BREAK");
    }

    fn visit_continue_statement(&mut self) {
        self.print("CONTINUE");
    }

    fn visit_const_statement(&mut self, name: &AstNode<Identifier>, value: &AstNode<Scalar>) {
        self.print("CONST");

        self.push_level();
        self.walk_const_statement(name, value);
        self.pop_level();
    }

    fn visit_statement_block(&mut self, statements: &AstNode<Statements>) {
        self.print("STATEMENT BLOCK");

        self.push_level();
        self.walk_statement_block(statements);
        self.pop_level();
    }

    fn visit_if_statement(&mut self, condition: &AstNode<CommaExpression>, if_block: &AstNode<Statement>, else_block: &Option<AstNode<Statement>>) {
        self.print("IF");

        self.push_level();
        self.walk_if_statement(condition, if_block, else_block);
        self.pop_level();
    }

    fn visit_comma_expr_statement(&mut self, comma_expr: &AstNode<CommaExpression>) {
        self.print("COMMA EXPRESSION");

        self.push_level();
        self.walk_comma_expr_statement(comma_expr);
        self.pop_level();
    }

    fn visit_class_statement(&mut self, name: &AstNode<PrefixedExpression>, class_expr: &AstNode<ClassExpression>) {
        self.print("CLASS");

        self.push_level();
        self.walk_class_statement(name, class_expr);
        self.pop_level();
    }

    fn visit_function_statement(&mut self, func_identifier: &AstNode<FunctionIdentifier>, env: &Option<AstNode<Expression>>, params: &AstNode<FunctionParams>, body: &AstNode<Statement>) {
        self.print("FUNCTION");

        self.push_level();
        self.walk_function_statement(func_identifier, env, params, body);
        self.pop_level();
    }

    fn visit_while_statement(&mut self, condition: &AstNode<CommaExpression>, loop_body: &AstNode<Statement>) {
        self.print("WHILE");

        self.push_level();
        self.walk_while_statement(condition, loop_body);
        self.pop_level();
    }

    fn visit_do_while_statement(&mut self, loop_body: &AstNode<Statement>, condition: &AstNode<CommaExpression>) {
        self.print("DO WHILE");

        self.push_level();
        self.walk_do_while_statement(loop_body, condition);
        self.pop_level();
    }

    fn visit_for_statement(&mut self, init: &Option<AstNode<ForInit>>, condition: &Option<AstNode<CommaExpression>>, post: &Option<AstNode<CommaExpression>>, body: &AstNode<Statement>) {
        self.print("FOR");

        self.push_level();
        self.walk_for_statement(init, condition, post, body);
        self.pop_level();
    }

    fn visit_foreach_statement(&mut self, value: &AstNode<Identifier>, key: &Option<AstNode<Identifier>>, container: &AstNode<Expression>, body: &AstNode<Statement>) {
        self.print("FOR EACH");

        self.push_level();
        self.walk_foreach_statement(value, key, container, body);
        self.pop_level();
    }

    fn visit_switch_statement(&mut self, value: &AstNode<CommaExpression>, cases: &Vec<AstNode<SwitchCase>>, default: &Option<AstNode<Statements>>) {
        self.print("SWITCH");

        self.push_level();
        self.walk_switch_statement(value, cases, default);
        self.pop_level();
    }

    fn visit_local_declare_statement(&mut self, local_declare: &AstNode<LocalDeclare>) {
        self.print("LOCAL DECLARE");

        self.push_level();
        self.walk_local_declare_statement(local_declare);
        self.pop_level();
    }

    fn visit_return_statement(&mut self, value: &Option<AstNode<CommaExpression>>) {
        self.print("RETURN");

        self.push_level();
        self.walk_return_statement(value);
        self.pop_level();
    }

    fn visit_yield_statement(&mut self, value: &Option<AstNode<CommaExpression>>) {
        self.print("YIELD");

        self.push_level();
        self.walk_yield_statement(value);
        self.pop_level();
    }

    fn visit_enum_statement(&mut self, name: &AstNode<Identifier>, values: &AstNode<EnumValues>) {
        self.print("ENUM");

        self.push_level();
        self.walk_enum_statement(name, values);
        self.pop_level();
    }

    fn visit_trycatch_statement(&mut self, try_body: &AstNode<Statement>, exception_name: &AstNode<Identifier>, catch_body: &AstNode<Statement>) {
        self.print("TRY CATCH");

        self.push_level();
        self.walk_trycatch_statement(try_body, exception_name, catch_body);
        self.pop_level();
    }

    fn visit_throw_statement(&mut self, exception: &AstNode<CommaExpression>) {
        self.print("THROW");

        self.push_level();
        self.walk_throw_statement(exception);
        self.pop_level();
    }

    fn visit_func_params(&mut self, params: &AstNode<FunctionParams>) {
        self.print(&params.range.to_string());
        self.print("FUNC PARAMS");

        self.push_level();
        self.walk_func_params(params);
        self.pop_level();
    }

    fn visit_func_param(&mut self, param: &AstNode<FunctionParam>) {
        self.print(&param.range.to_string());
        self.print("FUNC PARAM");
        
        match *param.value {
            FunctionParam::Normal(_) => {
                self.print("NORMAL");
            },
            FunctionParam::Default(_, _) => {
                self.print("DEFAULT");
            },
            FunctionParam::VarParams => {
                self.print("VARARGS");
            },
        }

        self.push_level();
        self.walk_func_param(param);
        self.pop_level();
    }

    fn visit_local_declare(&mut self, local_declare: &AstNode<LocalDeclare>) {
        self.print(&local_declare.range.to_string());
        match *local_declare.value {
            LocalDeclare::Function(_, _, _, _) => {
                self.print("FUNCTION");
            }
            LocalDeclare::Assign(_) => {
                self.print("ASSIGN");
            }
        }

        self.push_level();
        self.walk_local_declare(local_declare);
        self.pop_level();
    }

    fn visit_assign_expr(&mut self, assign_expr: &AstNode<AssignExpression>) {
        self.print(&assign_expr.range.to_string());
        self.print("ASSIGN EXPRESSION");

        self.push_level();
        self.walk_assign_expr(assign_expr);
        self.pop_level();
    }

    fn visit_for_init(&mut self, for_init: &AstNode<ForInit>) {
        self.print(&for_init.range.to_string());
        self.print("FOR INIT");

        match *for_init.value {
            ForInit::LocalDeclare(_) => {
                self.print("LOCAL DECLARE");
            }
            ForInit::CommaExpression(_) => {
                self.print("COMMA EXPRESSION");
            }
        }

        self.push_level();
        self.walk_for_init(for_init);
        self.pop_level();
    }

    fn visit_unary_op(&mut self, unary_op: &AstNode<UnaryOp>) {
        self.print(&unary_op.range.to_string());
        self.print("UNARY OP");

        self.push_level();
        self.walk_unary_op(unary_op);
        self.pop_level();
    }

    fn visit_logical_or_exp(&mut self, logical_or: &AstNode<LogicalOrExpression>) {
        if logical_or.value.right.is_empty() {
            self.walk_logical_or_exp(logical_or);
        } else {
            self.print(&logical_or.range.to_string());
            self.print("LOGICAL OR");

            self.push_level();
            self.walk_logical_or_exp(logical_or);
            self.pop_level();
        }
    }

    fn visit_logical_and_exp(&mut self, logical_and: &AstNode<LogicalAndExpression>) {
        if logical_and.value.right.is_empty() {
            self.walk_logical_and_exp(logical_and);
        } else {
            self.print(&logical_and.range.to_string());
            self.print("LOGICAL AND");

            self.push_level();
            self.walk_logical_and_exp(logical_and);
            self.pop_level();
        }
    }

    fn visit_bitwise_or_exp(&mut self, bitwise_or: &AstNode<BitwiseOrExpression>) {
        if bitwise_or.value.right.is_empty() {
            self.walk_bitwise_or_exp(bitwise_or);
        } else {
            self.print(&bitwise_or.range.to_string());
            self.print("BITWISE OR");

            self.push_level();
            self.walk_bitwise_or_exp(bitwise_or);
            self.pop_level();
        }
    }

    fn visit_bitwise_xor_exp(&mut self, bitwise_xor: &AstNode<BitwiseXorExpression>) {
        if bitwise_xor.value.right.is_empty() {
            self.walk_bitwise_xor_exp(bitwise_xor);
        } else {
            self.print(&bitwise_xor.range.to_string());
            self.print("BITWISE XOR");

            self.push_level();
            self.walk_bitwise_xor_exp(bitwise_xor);
            self.pop_level();
        }
    }

    fn visit_bitwise_and_exp(&mut self, bitwise_and: &AstNode<BitwiseAndExpression>) {
        if bitwise_and.value.right.is_empty() {
            self.walk_bitwise_and_exp(bitwise_and);
        } else {
            self.print(&bitwise_and.range.to_string());
            self.print("BITWISE AND");

            self.push_level();
            self.walk_bitwise_and_exp(bitwise_and);
            self.pop_level();
        }
    }

    fn visit_equal_exp(&mut self, equal: &AstNode<EqualExpression>) {
        if equal.value.slices.is_empty() {
            self.walk_equal_exp(equal);
        } else {
            self.print(&equal.range.to_string());
            self.print("EQUAL");

            self.push_level();
            self.walk_equal_exp(equal);
            self.pop_level();
        }
    }

    fn visit_compare_exp(&mut self, compare: &AstNode<CompareExpression>) {
        if compare.value.slices.is_empty() {
            self.walk_compare_exp(compare);
        } else {
            self.print(&compare.range.to_string());
            self.print("COMPARE");

            self.push_level();
            self.walk_compare_exp(compare);
            self.pop_level();
        }
    }

    fn visit_shift_exp(&mut self, shift: &AstNode<ShiftExpression>) {
        if shift.value.slices.is_empty() {
            self.walk_shift_exp(shift);
        } else {
            self.print(&shift.range.to_string());
            self.print("SHIFT");

            self.push_level();
            self.walk_shift_exp(shift);
            self.pop_level();
        }
    }

    fn visit_plus_exp(&mut self, plus: &AstNode<PlusExpression>) {
        if plus.value.slices.is_empty() {
            self.walk_plus_exp(plus);
        } else {
            self.print(&plus.range.to_string());
            self.print("PLUS");

            self.push_level();
            self.walk_plus_exp(plus);
            self.pop_level();
        }
    }

    fn visit_multiply_exp(&mut self, multiply: &AstNode<MultiplyExpression>) {
        if multiply.value.slices.is_empty() {
            self.walk_multiply_exp(multiply);
        } else {
            self.print(&multiply.range.to_string());
            self.print("MULTIPLY");

            self.push_level();
            self.walk_multiply_exp(multiply);
            self.pop_level();
        }
    }

    fn visit_prefixed_exp(&mut self, prefixed: &AstNode<PrefixedExpression>) {
        if prefixed.value.expr_types.is_empty() {
            self.walk_prefixed_exp(prefixed);
        } else {
            self.print(&prefixed.range.to_string());
            self.print("PREFIXED");

            self.push_level();
            for expr_type in &prefixed.value.expr_types {
                match expr_type.value.as_ref() {
                    PrefixedExpressionType::DotAccess(_) => {
                        self.print("DOT ACCESS");
                    },
                    PrefixedExpressionType::ArrayStyleAccess(_) => {
                        self.print("ARRAY ACCESS");
                    },
                    PrefixedExpressionType::PostIncrement => {
                        self.print("POST INCREMENT");
                    },
                    PrefixedExpressionType::PostDecrement => {
                        self.print("POST DECREMENT");
                    },
                    PrefixedExpressionType::FunctionCall(_) => {
                        self.print("FUNCTION CALL");
                    },
                }
            }
            self.pop_level();

            self.push_level();
            self.walk_prefixed_exp(prefixed);
            self.pop_level();
        }
    }

    fn visit_table(&mut self, table: &AstNode<Table>) {
        self.print(&table.range.to_string());
        for entry in &table.value.entries {
            self.visit_table_entry(entry);
        }
    }

    fn visit_table_entry(&mut self, entry: &AstNode<TableEntry>) {
        self.print(&entry.range.to_string());
        self.print("TABLE ENTRY");

        self.push_level();
        match *entry.value {
            TableEntry::Function(_, _, _) => {
                self.print("FUNCTION");
            },
            TableEntry::Constructor(_, _) => {
                self.print("CONSTRUCTOR");
            },
            TableEntry::DynamicAssign(_, _) => {
                self.print("DYNAMIC ASSIGN");
            },
            TableEntry::JsonStyle(_, _) => {
                self.print("JSON STYLE");
            },
            TableEntry::Simple(_, _) => {
                self.print("SIMPLE");
            },
            TableEntry::Attributes(_) => {
                self.print("ATTRIBUTES");
            },
        }
        self.pop_level();

        self.push_level();
        self.walk_table_entry(entry);
        self.pop_level();
    }

    fn visit_factor(&mut self, factor: &AstNode<Factor>) {
        self.print(&factor.range.to_string());
        self.print("FACTOR");

        self.push_level();
        match factor.value.as_ref() {
            Factor::Scalar(_) => {
                self.print("SCALAR");
            },
            Factor::Base => self.print("BASE"),
            Factor::Constructor => self.print("CONSTRUCTOR"),
            Factor::This => self.print("THIS"),
            Factor::DoubleColon(_) => {
                self.print("DOUBLE_COLON");
            },
            Factor::Null => self.print("NULL"),
            Factor::ArrayInit(_) => {
                self.print("ARRAY");
            },
            Factor::TableInit(_) => {
                self.print("TABLE");
            },
            Factor::FunctionExpression(_, _, _) => {
                self.print("FUNCTION");
            },
            Factor::LambdaExpression(_, _, _) => {
                self.print("LAMBDA EXPRESSION");
            },
            Factor::ClassExpression(_) => {
                self.print("CLASS EXPRESSION");
            },
            Factor::UnaryOp(_) => {
                self.print("UNARY OP");
            },
            Factor::RawCall(_) => {
                self.print("RAWCALL");
            },
            Factor::Delete(_) => {
                self.print("DELETE");
            },
            Factor::ParenExpression(_) => {
                self.print("PAREN EXPRESSION");
            },
            Factor::LineInfo => {
                self.print("LINE INFO");
            },
            Factor::FileInfo => {
                self.print("FILE INFO");
            },
            _ => {}
        }
        self.pop_level();

        self.push_level();
        self.walk_factor(factor);
        self.pop_level();
    }

    fn visit_expression(&mut self, expression: &AstNode<Expression>) {
        self.print(&expression.range.to_string());
        self.print("EXPRESSION");

        if let Some(expr_type) = &expression.value.expr_type {
            match *expr_type.value {
                ExpressionType::Newslot(_) => {
                    self.print("NEWSLOT");
                },
                ExpressionType::Assign(_) => {
                    self.print("ASSIGN");
                },
                ExpressionType::MinusEqual(_) => {
                    self.print("MINUS EQUAL");
                },
                ExpressionType::PlusEqual(_) => {
                    self.print("PLUS EQUAL");
                },
                ExpressionType::MultiplyEqual(_) => {
                    self.print("MULTIPLY EQUAL");
                },
                ExpressionType::DivideEqual(_) => {
                    self.print("DIVIDE EQUAL");
                },
                ExpressionType::Ternary(_, _) => {
                    self.print("TERNARY");
                },
            }
        }

        self.push_level();
        self.walk_expression(expression);
        self.pop_level();
    }

    fn visit_scalar(&mut self, scalar: &AstNode<Scalar>) {
        self.print(&scalar.range.to_string());
        match scalar.value.as_ref() {
            Scalar::Integer(value) => self.print(&format!("INTEGER: {}", value)),
            Scalar::Float(value) => self.print(&format!("FLOAT: {}", value)),
            Scalar::StringLiteral(value) => self.print(&format!("STRING: {}", value)),
            Scalar::True => self.print("TRUE"),
            Scalar::False => self.print("FALSE"),
        }
    }

    fn visit_identifier(&mut self, identifier: &AstNode<Identifier>) {
        self.print(&identifier.range.to_string());
        self.print(&format!("IDENTIFIER: {}", identifier.value.value));
    }
}