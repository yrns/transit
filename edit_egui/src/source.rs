use std::path::{Path, PathBuf};

/// Symbol locator consisting of a path, line no., and column.
pub type Locator = (PathBuf, usize, usize);

// TODO:
// ✓ Produce symbols from a source file.
// ✓ Watch source file(s) for changes.
// Produce itself from an edit graph (or symbols).
// Produce viewable/editable events for debugging/testing.
// Viewable/editable state.
// Goto symbol?
// ✓ Templating. Insert.
// ✓ Process symbol names (i.e. kebab-case).
// ✓ Extension(s)?

/// Provides language-dependent facilities for dealing with source code.
pub trait Source {
    /// Type of context this source produces.

    //type Context: Context;

    /// Runtime context used in [Source::resolve].

    //type RunContext;

    /// Error kind.
    type Error: std::error::Error;

    /// Create a new source from a path.
    fn from_path(path: &Path) -> Result<Self, Self::Error>
    where
        Self: Sized;

    /// Path to source.
    fn path(&self) -> &Path; // Option?

    /// Kebab-case for lisps, etc.
    fn normalize_symbol(&self, symbol: &str) -> String;

    /// Returns a symbol locator from source.
    fn symbol(&self, symbol: &str) -> Option<&Locator>;

    /// Returns an iterator over all symbols.
    // TODO: impl Trait should not be used in a public trait, but the only consumer is the edit
    // crate... just return a HashMap ref?
    fn symbols(&self) -> impl Iterator<Item = (&String, &Locator)>;

    /// Called every frame; checks source file for changes.
    fn update(&mut self) -> Result<(), Self::Error>;

    /// Insertion template for new symbols.
    fn insert_template(&self) -> &str;

    /// Source file type description.
    fn description(&self) -> &str;

    /// Source file extensions.
    fn extensions(&self) -> &[&str];

    // Resolve edit graph to a graph usable with [Self::Context].
    // fn resolve(
    //     &self,
    //     edit: &Edit<Self>,
    //     run: &Self::RunContext,
    // ) -> Graph<<Self::Context as Context>::State, <Self::Context as Context>::Transition>
    // where
    //     Self: Sized;
}
