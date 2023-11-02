use tower_lsp::{Client, lsp_types::MessageType};

use crate::ast::*;

pub trait SimpleVisitor {
    fn visit_script(&self, script: Script) {}
    fn visit_statements(&self, statements: Statements) {}
    fn visit_statement(&self, statement: Statement) {}
    fn visit_expression(&self, expression: Expression) {}
    fn visit_comma_expr(&self, comma_expression: CommaExpression) {}
    fn visit_logical_or_exp(&self, logical_or: LogicalOrExpression) {}
    fn visit_logical_and_exp(&self, logical_and: LogicalAndExpression) {}
    fn visit_bitwise_or_exp(&self, bitwise_or: BitwiseOrExpression) {}
    fn visit_bitwise_xor_exp(&self, bitwise_xor: BitwiseXorExpression) {}
    fn visit_bitwise_and_exp(&self, bitwise_and: BitwiseAndExpression) {}
    fn visit_equal_exp(&self, equal: EqualExpression) {}
    fn visit_compare_exp(&self, compare: CompareExpression) {}
    fn visit_shift_exp(&self, shift: ShiftExpression) {}
    fn visit_plus_exp(&self, plus: PlusExpression) {}
    fn visit_multiply_exp(&self, multiply: MultiplyExpression) {}
    fn visit_prefixed_exp(&self, prefixed: PrefixedExpression) {}
    fn visit_factor(&self, factor: Factor) {}
    fn visit_identifier(&self, identifier: Identifier) {}
}

pub trait SimpleVisitorMut {
    fn visit_script(&mut self, script: Script) {}
    fn visit_statements(&mut self, statements: Statements) {}
    fn visit_statement(&mut self, statement: Statement) {}
    fn visit_expression(&mut self, expression: Expression) {}
    fn visit_comma_expr(&mut self, comma_expression: CommaExpression) {}
    fn visit_logical_or_exp(&mut self, logical_or: LogicalOrExpression) {}
    fn visit_logical_and_exp(&mut self, logical_and: LogicalAndExpression) {}
    fn visit_bitwise_or_exp(&mut self, bitwise_or: BitwiseOrExpression) {}
    fn visit_bitwise_xor_exp(&mut self, bitwise_xor: BitwiseXorExpression) {}
    fn visit_bitwise_and_exp(&mut self, bitwise_and: BitwiseAndExpression) {}
    fn visit_equal_exp(&mut self, equal: EqualExpression) {}
    fn visit_compare_exp(&mut self, compare: CompareExpression) {}
    fn visit_shift_exp(&mut self, shift: ShiftExpression) {}
    fn visit_plus_exp(&mut self, plus: PlusExpression) {}
    fn visit_multiply_exp(&mut self, multiply: MultiplyExpression) {}
    fn visit_prefixed_exp(&mut self, prefixed: PrefixedExpression) {}
    fn visit_factor(&mut self, factor: Factor) {}
    fn visit_identifier(&mut self, identifier: Identifier) {}
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
            Statement::Const(id, scalar) => {
                self.push_level();
                self.print("CONST");
                self.pop_level();
            }
            Statement::Statements(statements) => self.visit_statements(statements),
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
            _ => {
                self.push_level();
                self.print("unknown statement");
                self.pop_level();
            },
        }

        self.pop_level();
    }

    fn visit_logical_or_exp(&mut self, logical_or: LogicalOrExpression) {
        if logical_or.right.is_none() {
            self.visit_logical_and_exp(logical_or.left);
            return;
        }

        self.push_level();
        self.print("LOGICAL OR");
        self.pop_level();
    }

    fn visit_logical_and_exp(&mut self, logical_and: LogicalAndExpression) {
        if logical_and.right.is_none() {
            self.visit_bitwise_or_exp(logical_and.left);
            return;
        }

        self.push_level();
        self.print("LOGICAL AND");
        self.pop_level();
    }

    fn visit_bitwise_or_exp(&mut self, bitwise_or: BitwiseOrExpression) {
        if bitwise_or.right.is_none() {
            self.visit_bitwise_xor_exp(bitwise_or.left);
            return;
        }

        self.push_level();
        self.print("BITWISE OR");
        self.pop_level();
    }

    fn visit_bitwise_xor_exp(&mut self, bitwise_xor: BitwiseXorExpression) {
        if bitwise_xor.right.is_none() {
            self.visit_bitwise_and_exp(bitwise_xor.left);
            return;
        }

        self.push_level();
        self.print("BITWISE XOR");
        self.pop_level();
    }

    fn visit_bitwise_and_exp(&mut self, bitwise_and: BitwiseAndExpression) {
        if bitwise_and.right.is_none() {
            self.visit_equal_exp(bitwise_and.left);
            return;
        }

        self.push_level();
        self.print("BITWISE AND");
        self.pop_level();
    }

    fn visit_equal_exp(&mut self, equal: EqualExpression) {
        if equal.right.is_none() {
            self.visit_compare_exp(equal.left);
            return;
        }

        self.push_level();
        self.print("EQUAL");
        self.pop_level();
    }

    fn visit_compare_exp(&mut self, compare: CompareExpression) {
        if compare.right.is_none() {
            self.visit_shift_exp(compare.left);
            return;
        }

        self.push_level();
        self.print("COMPARE");
        self.pop_level();
    }

    fn visit_shift_exp(&mut self, shift: ShiftExpression) {
        if shift.right.is_none() {
            self.visit_plus_exp(shift.left);
            return;
        }

        self.push_level();
        self.print("SHIFT");
        self.pop_level();
    }

    fn visit_plus_exp(&mut self, plus: PlusExpression) {
        if plus.right.is_none() {
            self.visit_multiply_exp(plus.left);
            return;
        }

        self.push_level();
        self.print("PLUS");
        self.pop_level();
    }

    fn visit_multiply_exp(&mut self, multiply: MultiplyExpression) {
        if multiply.right.is_none() {
            self.visit_prefixed_exp(multiply.left);
            return;
        }

        self.push_level();
        self.print("MULTIPLY");
        self.pop_level();
    }

    fn visit_prefixed_exp(&mut self, prefixed: PrefixedExpression) {
        if prefixed.expr_type.is_none() {
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
        self.pop_level();
    }

    fn visit_expression(&mut self, expression: Expression) {
        self.push_level();
        self.print("EXPRESSION");
        self.visit_logical_or_exp(*expression.logical_or);

        if expression.expr_type.is_some() {
            match expression.expr_type.unwrap() {
                ExpressionType::Newslot(expression) => {
                    self.push_level();
                    self.print("NEWSLOT");
                    self.visit_expression(*expression);
                    self.pop_level();
                },
                ExpressionType::Assign(_) => todo!(),
                ExpressionType::MinusEqual(_) => todo!(),
                ExpressionType::PlusEqual(_) => todo!(),
                ExpressionType::MultiplyEqual(_) => todo!(),
                ExpressionType::DivideEqual(_) => todo!(),
                ExpressionType::Ternary(_, _) => todo!(),
            }
        }
        self.pop_level();
    }
}