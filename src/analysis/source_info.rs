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

/// Represents a range in source code.
/// `start` is inclusive and `end` is exclusive
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

    pub fn contains_location(&self, location: &SourceLocation) -> bool {
        let past_start: bool = (location.line > self.start.line) || (location.line == self.start.line && location.column >= self.start.column);
        let before_end: bool = (location.line < self.end.line) || (location.line == self.end.line && location.column < self.end.column);

        return past_start && before_end;
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

    pub fn to_position(self) -> tower_lsp::lsp_types::Position {
        tower_lsp::lsp_types::Position { line: self.line - 1, character: self.column - 1 }
    }
}