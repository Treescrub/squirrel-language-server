use std::fmt::Display;

#[derive(Debug, Copy, Clone)]
pub struct SourceRange {
    pub start: SourceLocation,
    pub end: SourceLocation,
}

impl Display for SourceRange {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}->{}", self.start, self.end)
    }
}

impl SourceRange {
    pub fn new(start: SourceLocation, end: SourceLocation) -> Self {
        Self {
            start,
            end,
        }
    }

    pub fn new_empty() -> Self {
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

impl Display for SourceLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(l:{}, c:{})", self.line, self.column)
    }
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