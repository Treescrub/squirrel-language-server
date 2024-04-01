use super::SimpleVisitorMut;
use crate::analysis::ast::*;



pub struct IdentifierCollector {
    pub identifiers: Vec<AstNode<Identifier>>,
}

impl IdentifierCollector {
    pub fn new() -> Self {
        Self {
            identifiers: Vec::new(),
        }
    }
}

impl SimpleVisitorMut for IdentifierCollector {
    fn visit_identifier(&mut self, identifier: &AstNode<Identifier>) {
        self.identifiers.push(identifier.clone());

        self.walk_identifier(identifier);
    }
}