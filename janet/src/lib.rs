pub mod from_janet;
pub mod marshal;
pub mod pretty;

use edit_egui as edit;
use from_janet::FromJanet;
use heck::ToKebabCase;
use janetrs::{client::JanetClient, Janet, JanetSymbol, TaggedJanet};
use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};
use tracing::{error, info};
use transit_graph::Graph;

/// Maps symbol to source location.
pub type SymbolMap = HashMap<String, (PathBuf, usize, usize)>;

// TODO: Serialization of the Janet graph with serde is tricky without some kind of state-tracking
// or processing the graph first. Neither the pretty or marshal modules work in a satisfactory
// manner. We want to be able to serialize a running statechart, and later deserialize it and
// continue running...

// TODO: Efficiently handle redefinition of Janet functions (via reloading the source or
// netrepl). Right now we are only storing the function value (as a `Janet`); we would need to save
// the symbol name.

// TODO: Functions are stored in states and transitions which are cloned as locals to the
// statechart. The functions should strictly only be stored with the graph, and only the local
// values cloned (if changed). Right now we are cloning the locals every time a state or transition
// is traversed.

// #[cfg_attr(feature = "serde", derive(serde::Deserialize, serde::Serialize))]
// #[cfg_attr(feature = "serde", serde(transparent))]
// pub struct Symbol<'data>(
//     String,
//     #[cfg_attr(feature = "serde", serde(skip))] Option<JanetFunction<'data>>,
// );

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("watch error")]
    Watch(#[from] edit::WatchError),
    #[error("janet error")]
    Janet(#[from] janetrs::client::Error),
}

#[cfg_attr(feature = "serde", derive(serde::Deserialize, serde::Serialize))]
pub struct Source {
    pub path: PathBuf,
    #[cfg_attr(feature = "serde", serde(skip))]
    watcher: Option<edit::Watcher>,
    #[cfg_attr(feature = "serde", serde(skip))]
    pub symbols: SymbolMap,
}

impl edit::Source for Source {
    type Context = JanetContext;
    type RunContext = JanetClient;
    type Symbols<'a> = std::collections::hash_map::Iter<'a, String, (PathBuf, usize, usize)>;
    type Error = Error;

    fn from_path(path: &Path) -> Result<Self, Self::Error>
    where
        Self: Sized,
    {
        let watcher = edit::Watcher::new(path)?.into();
        let symbols = get_symbols(path)?.unwrap_or_default();

        Ok(Self {
            path: path.to_path_buf(),
            watcher,
            symbols,
        })
    }

    fn path(&self) -> &Path {
        self.path.as_path()
    }

    fn symbol(&self, symbol: &str) -> Option<&edit::Locator> {
        self.symbols.get(symbol)
    }

    fn symbols(&self) -> Self::Symbols<'_> {
        self.symbols.iter()
    }

    fn update(&mut self) -> Result<(), Self::Error> {
        if let Some(watcher) = &mut self.watcher {
            if watcher.changed() {
                if let Some(symbols) = get_symbols(&self.path)? {
                    self.symbols = symbols;
                }
            }
        }

        Ok(())
    }

    fn normalize_symbol(&self, symbol: &str) -> String {
        symbol.to_kebab_case()
    }

    fn insert_template(&self) -> &str {
        "\n\n(defn {} [self ctx ev]\n  )"
    }

    fn description(&self) -> &str {
        "Janet"
    }

    fn extensions(&self) -> &[&str] {
        &["janet"]
    }

    fn resolve(&self, edit: &edit::Edit<Self>, client: &JanetClient) -> Graph<State, Transition> {
        let f = |symbol: &Option<String>| {
            symbol
                .as_deref()
                .map(|symbol| resolve(symbol, client))
                .unwrap_or_else(Janet::nil)
        };

        edit.graph.map(
            |_i, state| State {
                enter: f(&state.enter),
                exit: f(&state.exit),
                ..Default::default()
            },
            |_i, transition| Transition {
                id: transition.id.clone(),
                guard: f(&transition.guard),
                local: Janet::nil(),
            },
        )
    }
}

pub struct JanetContext {
    //client: &'a JanetClient,
    pub context: Janet,
}

//#[cfg_attr(feature = "serde", derive(serde::Deserialize, serde::Serialize))]
#[derive(Clone, Debug)]
pub struct State {
    pub enter: Janet,
    pub exit: Janet,
    pub local: Janet,
}

// Can this be nil instead? This is only used for the root state.
impl Default for State {
    fn default() -> Self {
        Self {
            enter: Janet::nil(),
            exit: Janet::nil(),
            local: Janet::nil(),
        }
    }
}

/// Each event has an `id` which must match the transition(s) it corresponds to. This avoids calling
/// into Janet for every event/transition to see if the guard passes.
#[derive(Debug)]
pub struct Event {
    pub id: String, // TODO: make id generic?
    pub value: Janet,
}

impl Event {
    pub fn id(id: impl Into<String>) -> Self {
        Self {
            id: id.into(),
            value: Janet::nil(),
        }
    }

    pub fn with(mut self, value: impl Into<Janet>) -> Self {
        self.value = value.into();
        self
    }
}

//#[cfg_attr(feature = "serde", derive(serde::Deserialize, serde::Serialize))]
#[derive(Clone, Debug)]
pub struct Transition {
    pub id: String,
    pub guard: Janet,
    pub local: Janet,
}

impl Transition {
    pub fn new(id: &str, symbol: &str, client: &JanetClient, local: Janet) -> Self {
        Self {
            id: id.to_owned(),
            guard: resolve(symbol, client),
            local,
        }
    }
}

impl transit_graph::Context for JanetContext {
    type Event = Event;
    type State = State;
    type Transition = Transition;

    fn dispatch(&mut self, event: &Event) {
        info!("dispatch: event: {:?}", event);
    }

    fn transition(&mut self, source: &State, target: &State) {
        info!("transition: source: {:?} target: {:?}", source, target);
    }
}

// Resolve a symbol and return the result if it's a function.
pub fn resolve<'a>(symbol: impl Into<JanetSymbol<'a>>, client: &JanetClient) -> Janet {
    client
        .env()
        .and_then(|env| env.resolve(symbol))
        .and_then(|value| match value.unwrap() {
            TaggedJanet::Function(_) => Some(value),
            _ => None,
        })
        .unwrap_or_else(Janet::nil)
}

impl transit_graph::State<JanetContext> for State {
    fn enter(&mut self, ctx: &mut JanetContext, event: Option<&Event>) {
        if let TaggedJanet::Function(mut f) = self.enter.unwrap() {
            match f.call([
                self.local,
                ctx.context,
                *event.map(|e| &e.value).unwrap_or(&Janet::nil()),
            ]) {
                Ok(_) => (),
                Err(e) => error!("error in enter: {e:?}"),
            }
        }
    }

    fn exit(&mut self, ctx: &mut JanetContext, event: Option<&Event>) {
        if let TaggedJanet::Function(mut f) = self.exit.unwrap() {
            match f.call([
                self.local,
                ctx.context,
                *event.map(|e| &e.value).unwrap_or(&Janet::nil()),
            ]) {
                Ok(_) => (),
                Err(e) => error!("error in exit: {e:?}"),
            }
        }
    }
}

impl transit_graph::Transition<JanetContext> for Transition {
    fn guard(&mut self, ctx: &mut JanetContext, event: &Event) -> bool {
        if event.id == self.id {
            let value = event.value;
            if let TaggedJanet::Function(mut f) = self.guard.unwrap() {
                match f.call([self.local, ctx.context, value]) {
                    Ok(res) => {
                        info!("guard result: {:?}", res);
                        match res.unwrap() {
                            TaggedJanet::Boolean(b) => b,
                            _ => res.is_truthy(),
                        }
                    }
                    Err(e) => {
                        error!("error in guard: {e:?}");
                        false
                    }
                }
            } else {
                // If there is no guard but the id matches it passes by default.
                info!("no guard");
                true
            }
        } else {
            false
        }
    }
}

pub fn get_symbols(path: impl AsRef<Path>) -> Result<Option<SymbolMap>, Error> {
    let client = JanetClient::init_with_default_env()?;

    // Set the system path to the specified path's directory and drop the extension from the file
    // name. Janet interprets a leading "/" as relative, full paths don't work.
    let path = path.as_ref();
    if let (Some(parent), Some(stem)) = (path.parent(), path.file_stem()) {
        // Requiring a module returns a table w/ symbols. TODO escape? use parser instead?
        let query = format!(
            r#"(setdyn :syspath "{}") (tabseq [[k v] :pairs (require "{}")] k (get v :source-map))"#,
            parent.display(),
            stem.to_string_lossy(),
        );

        //dbg!(&query);

        let res = client.run(query)?;

        Ok(SymbolMap::from_janet(res)
            .map_err(|e| {
                error!("error in SymbolMap::from_janet: {e:?}");
                e
            })
            .ok())
    } else {
        Ok(None)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn symbols() {
        assert_eq!(
            get_symbols("tests/symbols")
                .unwrap()
                .unwrap()
                .remove("a-symbol")
                .unwrap(),
            (
                // If the specified path is relative, the returned paths will be, too.
                PathBuf::from("tests/symbols.janet"),
                1,
                1
            )
        );
    }
}
