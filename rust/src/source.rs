// TODO!

use std::path::PathBuf;

//#[cfg_attr(feature = "serde", derive(serde::Deserialize, serde::Serialize))]
#[derive(serde::Deserialize, serde::Serialize)]
pub struct Source {
    pub path: PathBuf,
    //#[cfg_attr(feature = "serde", serde(skip))]
    //watcher: Option<edit::Watcher>,
    //#[cfg_attr(feature = "serde", serde(skip))]
    //pub symbols: SymbolMap,
}
