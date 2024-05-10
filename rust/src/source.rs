// TODO!

use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

use edit_egui as edit;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("none")]
    None,
}

//#[cfg_attr(feature = "serde", derive(serde::Deserialize, serde::Serialize))]
#[derive(serde::Deserialize, serde::Serialize)]
pub struct Source {
    pub path: PathBuf,
}

impl From<PathBuf> for Source {
    fn from(path: PathBuf) -> Self {
        Self { path }
    }
}

impl edit::Source for Source {
    type Error = Error;

    fn path(&self) -> &Path {
        self.path.as_path()
    }

    fn normalize_symbol(&self, symbol: &str) -> String {
        symbol.to_owned()
    }

    fn symbols(&self) -> Result<HashMap<String, edit::Locator>, Self::Error> {
        todo!()
    }

    fn template(&self) -> &str {
        todo!()
    }

    fn description(&self) -> &str {
        "Rust one-shot system context for Bevy"
    }

    fn extensions(&self) -> &[&str] {
        &["rs"]
    }
}
