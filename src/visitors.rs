use tower_lsp::{Client, lsp_types::MessageType};

use crate::ast::*;

pub trait SimpleVisitor {
    fn visit_script(&self, script: Script);
    fn visit_statements(&self, statements: Statements);
    fn visit_statement(&self, statement: Statement);
}

pub trait SimpleVisitorMut {
    fn visit_script(&mut self, script: Script);
    fn visit_statements(&mut self, statements: Statements);
    fn visit_statement(&mut self, statement: Statement);
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
}

impl SimpleVisitorMut for PrettyPrinter {
    fn visit_script(&mut self, script: Script) {
        self.add_indents();
        self.push_level();

        self.text.push_str(&format!("SCRIPT: {} statement(s)\n", script.statements.len()));
        for statement in script.statements {
            self.visit_statement(statement);
        }

        self.pop_level();
    }

    fn visit_statements(&mut self, statements: Statements) {
        self.add_indents();
        self.push_level();

        self.text.push_str("STATEMENTS\n");
        for statement in statements.statements {
            self.visit_statement(statement);
        }

        self.pop_level();
    }

    fn visit_statement(&mut self, statement: Statement) {
        match statement {
            Statement::Break => {
                self.add_indents();
                self.text.push_str("BREAK\n");
            }
            Statement::Continue => {
                self.add_indents();
                self.text.push_str("CONTINUE\n");
            }
            Statement::Const(id, scalar) => {
                self.add_indents();
                self.text.push_str(&format!("CONST\n"));
            }
            Statement::Statements(statements) => self.visit_statements(statements),
            _ => {
                self.add_indents();
                self.text.push_str("unknown statement\n");
            },
        }
    }
}