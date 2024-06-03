use super::source_info::{SourceLocation, SourceRange};


#[derive(Debug, Clone)]
struct SymbolEntry {
    name: Option<String>,
    range: SourceRange,
    children: Vec<SymbolEntry>,
}

impl SymbolEntry {
    pub fn new(name: Option<String>, range: SourceRange) -> Self {
        Self {
            name,
            range,
            children: Vec::new(),
        }
    }

    pub fn add_child(&mut self, entry: SymbolEntry) -> &SymbolEntry {
        self.children.push(entry);

        return self.children.last().unwrap();
    }
}

struct SymbolTable {
    root: SymbolEntry,
}

impl SymbolTable {
    pub fn new(range: SourceRange) -> Self {
        Self {
            root: SymbolEntry::new(None, range),
        }
    }

    pub fn get_symbols(&self, location: &SourceLocation) -> Vec<&SymbolEntry> {
        let mut symbols: Vec<&SymbolEntry> = Vec::new();

        let mut visit: Vec<&SymbolEntry> = vec![&self.root];

        while !visit.is_empty() {
            let entry = visit.pop().unwrap();

            if !entry.range.contains_location(location) {
                continue;
            }

            symbols.push(entry);

            for child in &entry.children {
                visit.insert(0, child);
            }
        }

        return symbols;
    }
}