#[cfg(feature = "editor")]
mod edit;
mod graph;
#[cfg(feature = "editor")]
mod undo;

use crate::graph::{Idx, Tdx};
use crate::source::*;

#[cfg(feature = "editor")]
pub use edit::*;
pub use graph::*;
#[cfg(feature = "editor")]
pub use undo::*;

#[cfg_attr(feature = "serde", derive(serde::Deserialize, serde::Serialize))]
#[derive(Debug, Default, Clone)]
pub enum Selection {
    #[default]
    None,
    State(Idx),
    Transition(Tdx),
}

/// Statechart graph editor.
#[cfg_attr(feature = "serde", derive(serde::Deserialize, serde::Serialize))]
//#[derive(Default)]
pub struct Edit<S> {
    /// Source.
    pub source: Option<S>,
    /// Symbols.
    #[cfg(feature = "editor")]
    #[serde(skip)]
    pub symbols: SymbolMap,
    /// Graph structure.
    pub graph: EditGraph,
    /// Pseudo root.
    #[serde(default)]
    narrow: Option<Idx>,
    /// Current selection.
    #[serde(default)]
    pub selection: Selection,
    /// Undo history.
    // TODO: limit history size, maybe move this to persistence
    #[serde(default)]
    pub undo: Undo,
    /// Versioning.
    #[serde(default)]
    pub version: usize,
}

impl<S> Default for Edit<S> {
    fn default() -> Self {
        Self {
            source: None,
            #[cfg(feature = "editor")]
            symbols: Default::default(),
            graph: Default::default(),
            narrow: None,
            selection: Selection::None,
            undo: Undo::default(),
            version: 0,
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum SymbolId {
    Enter(Idx),
    Exit(Idx),
    Guard(Tdx),
}

pub type Symbol = Option<String>;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum ControlPoint {
    C1,
    C2,
}

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("{0}")]
    Io(#[from] std::io::Error),
    #[cfg(feature = "serde")]
    #[error("{0}")]
    Import(#[from] ron::error::SpannedError),
    #[error("{0}")]
    Export(#[from] ron::Error),
    #[cfg(feature = "editor")]
    #[error("{0}")]
    Watch(#[from] WatchError),
    #[error("{0}")]
    Other(String),
}

#[cfg(feature = "serde")]
impl<S> Edit<S>
where
    S: Source,
{
    /// Load from path.
    pub fn load(path: impl AsRef<std::path::Path>) -> Result<Self, Error>
    where
        S: serde::de::DeserializeOwned,
    {
        Ok(ron::de::from_reader(std::fs::File::open(path.as_ref())?)?)
    }
}
