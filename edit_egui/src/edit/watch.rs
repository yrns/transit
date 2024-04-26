use notify_debouncer_mini::{
    new_debouncer, notify,
    notify::{INotifyWatcher, RecursiveMode},
    DebouncedEvent, Debouncer,
};
use std::{
    path::{Path},
    sync::mpsc::Receiver,
    time::Duration,
};
use tracing::{error, info};
//use transit_graph::{Context, Graph};

//use crate::Edit;

pub type WatchError = notify::Error;

pub type Rx = Receiver<Result<Vec<DebouncedEvent>, WatchError>>;

/// Watches source file for changes.
pub struct Watcher(Debouncer<INotifyWatcher>, Rx);

impl Watcher {
    pub fn new(path: impl AsRef<Path>) -> Result<Self, notify::Error> {
        let (tx, rx) = std::sync::mpsc::channel();
        let mut debouncer = new_debouncer(Duration::from_secs(2), tx)?;

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
                Err(err) => {
                    error!("watch error: {:?}", err)
                }
            }
        }

        changed
    }
}
