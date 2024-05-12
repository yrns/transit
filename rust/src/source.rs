//! Rust source.

use std::path::{Path, PathBuf};

use edit::SymbolMap;
use tree_sitter::{Parser, Point, Query, QueryCursor, Tree};

use edit_egui as edit;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("{0}")]
    Io(#[from] std::io::Error),
    #[error("{0}")]
    Language(#[from] tree_sitter::LanguageError),
    #[error("{0}")]
    Query(#[from] tree_sitter::QueryError),
}

//#[cfg_attr(feature = "serde", derive(serde::Deserialize, serde::Serialize))]
#[derive(serde::Deserialize, serde::Serialize)]
pub struct Source {
    pub path: PathBuf,
    #[serde(skip)]
    pub parser: Parser,
    #[serde(skip)]
    pub tree: Option<Tree>,
}

impl From<PathBuf> for Source {
    fn from(path: PathBuf) -> Self {
        Self {
            path,
            parser: Parser::new(),
            tree: None,
        }
    }
}

pub const FNS_QUERY: &str = "(function_item name: (identifier) @name) @definition.function";

impl edit::Source for Source {
    type Error = Error;

    fn path(&self) -> &Path {
        self.path.as_path()
    }

    // TODO: valid rust fn name, is this two-way?
    fn normalize_symbol(&self, symbol: &str) -> String {
        symbol.to_owned()
    }

    fn symbols(&mut self) -> Result<SymbolMap, Self::Error> {
        let buf = std::fs::read(&self.path)?;
        let lang = tree_sitter_rust::language();

        // we have to set the language every time since this is deserialized
        self.parser.set_language(&lang)?;
        self.parser.reset();

        let tree = self
            .parser
            .parse(&buf, self.tree.as_ref())
            .expect("parse failed");

        let query = Query::new(&lang, FNS_QUERY)?;

        // warn?
        // if !tree.root_node().has_error() {}

        let mut cursor = QueryCursor::new();
        let matches = cursor.matches(&query, tree.root_node(), buf.as_slice());

        // TODO: this matches *any* function, and does not look for fns that match the template
        let symbols = matches
            .map(|m| {
                let node = m.captures[1].node;
                let Point { row, column } = node.start_position();
                std::str::from_utf8(&buf[node.byte_range()])
                    .map(|name| (name.to_owned(), (self.path.clone(), row, column)))
            })
            // TODO: warn?
            .filter_map(|s| s.ok())
            .collect();

        self.tree = Some(tree);

        Ok(symbols)
    }

    // FIX: escape {}?
    fn template(&self) -> &str {
        r#"fn {}(In((_entity, _event)): In<(Entity, Option<_>)>) {}"#
    }

    fn description(&self) -> &str {
        "Rust one-shot system context for Bevy"
    }

    fn extensions(&self) -> &[&str] {
        &["rs"]
    }
}
