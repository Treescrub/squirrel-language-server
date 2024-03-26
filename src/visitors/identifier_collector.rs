use std::collections::HashSet;

use super::SimpleVisitorMut;
use crate::analysis::ast::*;



pub struct IdentifierCollector {
    pub identifiers: HashSet<String>,
}

impl IdentifierCollector {
    pub fn new() -> Self {
        Self {
            identifiers: HashSet::new(),
        }
    }
}

impl SimpleVisitorMut for IdentifierCollector {
    fn visit_identifier(&mut self, identifier: &AstNode<Identifier>) {
        let identifier_node: &Identifier = &identifier.value;
        self.identifiers.insert(identifier_node.value.clone());

        self.walk_identifier(identifier);
    }
}