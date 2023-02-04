use janet::{get_symbols, SymbolMap};
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

pub struct Source {
    path: PathBuf,
    //#[allow(unused)]
    _watcher: Debouncer<INotifyWatcher>,
    rx: Receiver<Result<Vec<DebouncedEvent>, Vec<notify::Error>>>,
    pub symbols: Option<SymbolMap>,
}

#[derive(Debug)]
pub enum Error {
    Notify(notify::Error),
    Janet(janet::Error),
}

// TODO: This needs to be generic over language context.
impl Source {
    pub fn new(path: impl AsRef<Path>) -> Result<Self, Error> {
        let (tx, rx) = std::sync::mpsc::channel();
        let mut debouncer =
            new_debouncer(Duration::from_secs(2), None, tx).map_err(Error::Notify)?;

        debouncer
            .watcher()
            .watch(path.as_ref(), RecursiveMode::NonRecursive)
            .map_err(Error::Notify)?;

        Ok(Self {
            path: path.as_ref().into(),
            _watcher: debouncer,
            rx,
            symbols: get_symbols(path).map_err(Error::Janet)?,
        })
    }

    // Call periodically to clear the channel.
    pub fn update(&mut self) {
        let mut update = false;

        for res in self.rx.try_iter() {
            match res {
                Ok(events) => {
                    for event in events {
                        println!("watch event: {:?}", event);
                    }
                    update = true;
                }
                Err(errors) => {
                    for err in errors {
                        println!("watch error: {:?}", err)
                    }
                }
            }
        }

        // Update symbols if needed.
        if update {
            match get_symbols(&self.path) {
                Ok(symbols) => match symbols {
                    Some(symbols) => {
                        self.symbols = Some(symbols);
                    }
                    None => {
                        println!("no symbols"); // warn?
                    }
                },
                _ => (), // error?
            }
        }
    }

    // Why is this an option?
    // pub fn symbols(&self) -> impl Iterator<Item = &str> {
    //     self.symbols.as_ref().keys()
    // }

    pub fn symbol(&self, symbol: &str) -> Option<&(PathBuf, usize, usize)> {
        self.symbols.as_ref().and_then(|s| s.get(symbol))
    }
}
