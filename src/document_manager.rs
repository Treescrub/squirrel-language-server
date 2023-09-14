use tower_lsp::lsp_types::{Range as LspRange, Position};

#[derive(Debug)]
pub struct DocumentManager {
    pub contents: String, // only supports one document right now
}

impl DocumentManager {
    pub fn new() -> Self {
        Self {
            contents: String::new(),
        }
    }

    pub fn set(&mut self, text: &str) {
        self.contents = text.to_string();
    }

    pub fn edit(&mut self, text: &str, range: LspRange) {
        let start_index = self.pos_to_index(range.start);
        let end_index = self.pos_to_index(range.end);

        self.contents.replace_range(start_index..end_index, text);
    }

    fn pos_to_index(&self, position: Position) -> usize {
        let target_line = position.line;
        let target_column = position.character;
        let mut char_index: usize = 0;
        let mut line: u32 = 0;
        let mut column: u32 = 0;

        for char in self.contents.chars() {
            if char == '\n' {
                line += 1;
                column = 0;
                char_index += 1;
                continue;
            }
            if line == target_line && column == target_column {
                return char_index;
            }

            char_index += 1;
            column += 1;
        }

        return char_index;
    }
}