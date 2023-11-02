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
    }
}

impl SimpleVisitorMut for PrettyPrinter {
    fn visit_script(&mut self, script: Script) {
        self.print(&format!("SCRIPT: {} statement(s)\n", script.statements.len()));
        self.push_level();
        for statement in script.statements {
            self.visit_statement(statement);
        }

        self.pop_level();
    }

    fn visit_statements(&mut self, statements: Statements) {
        self.push_level();

        self.print("STATEMENTS\n");
        for statement in statements.statements {
            self.visit_statement(statement);
        }

        self.pop_level();
    }

    fn visit_statement(&mut self, statement: Statement) {
        self.push_level();
        self.print("STATEMENT\n");
        match statement {
            Statement::Break => {
                self.push_level();
                self.print("BREAK\n");
                self.pop_level();
            }
            Statement::Continue => {
                self.push_level();
                self.print("CONTINUE\n");
                self.pop_level();
            }
            Statement::Const(id, scalar) => {
                self.push_level();
                self.print("CONST\n");
                self.pop_level();
            }
            Statement::Statements(statements) => self.visit_statements(statements),
            Statement::CommaExpression(comma_expression) => {
                self.push_level();
                self.print("COMMA EXPRESSION\n");
                self.pop_level();
            }
            _ => {
                self.push_level();
                self.print("unknown statement\n");
                self.pop_level();
            },
        }

        self.pop_level();
    }

    fn visit_logical_or_exp(&mut self, logical_or: LogicalOrExpression) {
        self.push_level();
    }
}