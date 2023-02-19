use notify_debouncer_mini::{
    new_debouncer, notify,
    notify::{INotifyWatcher, RecursiveMode},
    DebouncedEvent, Debouncer,
};
use std::{
    path::{Path, PathBuf},
    sync::mpsc::Receiver,
    time::Duration,
};
use tracing::{error, info};
use transit_graph::Context;

/// Symbol locator consisting of a path, line no., and column.
pub type Locator = (PathBuf, usize, usize);

pub type WatchError = notify::Error;

pub type Rx = Receiver<Result<Vec<DebouncedEvent>, Vec<WatchError>>>;

/// Watches source file for changes.
pub struct Watcher(Debouncer<INotifyWatcher>, Rx);

impl Watcher {
    pub fn new(path: impl AsRef<Path>) -> Result<Self, notify::Error> {
        let (tx, rx) = std::sync::mpsc::channel();
        let mut debouncer = new_debouncer(Duration::from_secs(2), None, tx)?;

        debouncer
            .watcher()
            .watch(path.as_ref(), RecursiveMode::NonRecursive)?;

        Ok(Watcher(debouncer, rx))
    }

    /// Call periodically to clear the channel.
    pub fn changed(&mut self) -> bool {
        let mut changed = false;

        for res in self.1.try_iter() {
            match res {
                Ok(events) => {
                    for event in events {
                        info!("watch event: {:?}", event);
                    }
                    changed = true;
                }
                Err(errors) => {
                    for err in errors {
                        error!("watch error: {:?}", err)
                    }
                }
            }
        }

        changed
    }
}

// TODO:
// ✓ Produce symbols from a source file.
// ✓ Watch source file(s) for changes.
// Produce itself from an edit graph (or symbols).
// Produce viewable/editable events for debugging/testing.
// Viewable/editable state.
// Goto symbol?
// Templating. Insert.
// Process symbol names (i.e. kebab-case).
// Extension(s)?

/// Provides language-dependent facilities for dealing with source code.
pub trait Source {
    /// Type of context this source produces.
    type Context: Context;

    /// Iterator over all symbols.
    type Symbols<'a>: Iterator<Item = (&'a String, &'a Locator)>
    where
        Self: 'a;

    /// Error kind.
    type Error: std::error::Error;

    /// Create a new source from a path.
    fn from_path(path: &Path) -> Result<Self, Self::Error>
    where
        Self: Sized;

    /// Path to source.
    fn path(&self) -> &Path; // Option?

    /// Returns a symbol locator from source.
    fn symbol(&self, symbol: &str) -> Option<&Locator>;

    /// Returns an iterator over all symbols.
    fn symbols<'a>(&'a self) -> Self::Symbols<'a>;

    /// Called every frame; checks source file for changes.
    fn update(&mut self) -> Result<(), Self::Error>;
}
