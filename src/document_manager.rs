use std::collections::HashMap;

use tower_lsp::lsp_types::{Range as LspRange, Position, Url};

#[derive(Debug)]
pub struct DocumentManager {
    pub files: HashMap<String, String>,
}

impl DocumentManager {
    pub fn new() -> Self {
        Self {
            files: HashMap::new(),
        }
    }

    pub fn open_file(&mut self, text: &str, uri: &Url) {
        self.files.insert(uri.to_string(), text.to_string());
    }

    pub fn edit_file(&mut self, text: &str, uri: &Url, range: Option<LspRange>) {
        if range.is_none() {
            self.files.insert(uri.to_string(), text.to_string());
            return;
        }

        let range = range.unwrap();

        let start_index = self.pos_to_byte_index(uri, range.start);
        let end_index = self.pos_to_byte_index(uri, range.end);

        if self.files.contains_key(&uri.to_string()) {
            let mut contents = self.files[&uri.to_string()].clone();
            contents.replace_range(start_index..end_index, text); // replace_range uses byte indices instead of codepoint indices
            self.files.insert(uri.to_string(), contents);
        }
    }

    pub fn close_file(&mut self, uri: &Url) {
        self.files.remove(&uri.to_string());
    }

    pub fn total_open_files(&self) -> usize {
        return self.files.len();
    }

    pub fn get(&self, uri: &Url) -> Option<&str> {
        if !self.files.contains_key(&uri.to_string()) {
            return None;
        }

        return Some(self.files[&uri.to_string()].as_str());
    }

    // Returns a byte index because replace_range uses byte indices, not codepoint indices
    fn pos_to_byte_index(&self, uri: &Url, position: Position) -> usize {
        if !self.files.contains_key(&uri.to_string()) {
            return 0;
        }

        let target_line = position.line;
        let target_column = position.character;
        let mut byte_index: usize = 0;
        let mut line: u32 = 0;
        let mut column: u32 = 0;

        for char in self.files[&uri.to_string()].chars() {
            if char == '\n' {
                line += 1;
                column = 0;
                byte_index += char.len_utf8();
                continue;
            }
            if line == target_line && column == target_column {
                return byte_index;
            }

            byte_index += char.len_utf8();
            column += 1;
        }

        return byte_index;
    }
}