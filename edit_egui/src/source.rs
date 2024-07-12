use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

/// Symbol locator consisting of a path, line no., and column.
pub type Locator = (PathBuf, usize, usize);

/// Maps symbol to source location.
pub type SymbolMap = HashMap<String, Locator>;

/// Provides language-dependent facilities for dealing with source code.
pub trait Source
//: From<PathBuf>
// where
//     Self: TryFrom<PathBuf>,
//     <Self as TryFrom<PathBuf>>::Error: Error,
{
    // /// Type of context this source produces.
    // type Context: Context;

    // /// Runtime context used in [Source::resolve].
    // type RunContext;

    /// Error kind.
    type Error: std::error::Error;

    // /// Create a new source from a path.
    // fn from_path(path: &Path) -> Result<Self, Self::Error>
    // where
    //     Self: Sized;

    /// Source path.
    fn path(&self) -> &Path;

    /// Kebab-case for lisps, etc. TODO: Cow
    fn normalize_symbol(&self, symbol: &str) -> String;

    // /// Returns a symbol locator from source.
    // fn symbol(&self, symbol: &str) -> Option<&Locator>;

    /// Returns a [HashMap] containing all symbols.
    fn symbols(&mut self) -> Result<SymbolMap, Self::Error>;

    /// Insertion template for new symbols.
    fn template(&self) -> &str;

    /// Source file type description.
    fn description(&self) -> &str;

    /// Source file extensions.
    fn extensions() -> &'static [&'static str] {
        &[]
    }

    // Resolve edit graph to a graph usable with [Self::Context].
    // fn resolve(
    //     &self,
    //     edit: &Edit<Self>,
    //     run: &Self::RunContext,
    // ) -> Graph<<Self::Context as Context>::State, <Self::Context as Context>::Transition>
    // where
    //     Self: Sized;
}
