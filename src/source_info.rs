#[derive(Debug, Copy, Clone)]
pub struct SourceRange {
    pub start: SourceLocation,
    pub end: SourceLocation,
}

impl SourceRange {
    pub fn new() -> Self {
        Self {
            start: SourceLocation::new(),
            end: SourceLocation::new(),
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct SourceLocation {
    pub line: u32,
    pub column: u32,
}

impl SourceLocation {
    pub fn new() -> Self {
        Self {
            line: 0,
            column: 0,
        }
    }

    pub fn to_position(&self) -> tower_lsp::lsp_types::Position {
        tower_lsp::lsp_types::Position { line: self.line - 1, character: self.column - 1 }
    }
}