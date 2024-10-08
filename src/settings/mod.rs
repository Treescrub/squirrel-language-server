use tower_lsp::lsp_types::ConfigurationItem;

pub enum DebugSettings {
    VerbosePrint,
}

impl DebugSettings {
    pub fn as_str(&self) -> &str {
        match self {
            DebugSettings::VerbosePrint => "debug.debugMessages",
        }
    }

    pub fn to_full_name(&self) -> String {
        return "squirrel.".to_owned() + self.as_str();
    }

    fn get_config_item(&self) -> ConfigurationItem {
        return ConfigurationItem {
            scope_uri: None,
            section: Some(self.to_full_name()),
        }
    }
}

impl From<DebugSettings> for ConfigurationItem {
    fn from(value: DebugSettings) -> Self {
        return value.get_config_item();
    }
}