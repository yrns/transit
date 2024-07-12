pub mod from_janet;
pub mod marshal;
pub mod pretty;

use edit::SymbolMap;
use edit_egui as edit;
use from_janet::FromJanet;
use heck::ToKebabCase;
use janetrs::{client::JanetClient, Janet, JanetSymbol, TaggedJanet};
use std::path::{Path, PathBuf};
use tracing::{error, info};
use transit_graph::{Context, Graph, Idx, IntMap, Tdx};

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
    //#[error("watch error")]
    //Watch(#[from] edit::WatchError),
    #[error("janet error")]
    Janet(#[from] janetrs::client::Error),
    #[error("unknown extension")]
    UnknownExt,
}

// TODO: replace some of this with bevy_mod_scripting? bevy_asset for watching?
// TODO: build the editor with support for all supported source types, not just one
#[cfg_attr(feature = "serde", derive(serde::Deserialize, serde::Serialize))]
#[cfg_attr(feature = "serde", serde(transparent))]
pub struct Source(pub PathBuf);

impl From<PathBuf> for Source {
    fn from(path: PathBuf) -> Self {
        Self(path)
    }
}

impl edit::Source for Source {
    // type Context = JanetContext;
    // type RunContext = JanetClient;
    type Error = Error;

    const EXT: &'static [&'static str] = &["janet"];

    // fn from_path(path: &Path) -> Result<Self, Self::Error>
    // where
    //     Self: Sized,
    // {
    //     //let watcher = edit::Watcher::new(path)?.into();
    //     let symbols = get_symbols(path)?.unwrap_or_default();

    //     Ok(Self {
    //         path: path.to_path_buf(),
    //         //watcher,
    //         symbols,
    //     })
    // }

    fn path(&self) -> &Path {
        self.0.as_path()
    }

    fn symbols(&mut self) -> Result<SymbolMap, Self::Error> {
        Ok(get_symbols(self.path())?.unwrap_or_default())
    }

    fn normalize_symbol(&self, symbol: &str) -> String {
        symbol.to_kebab_case()
    }

    fn template(&self) -> &str {
        "\n\n(defn {} [self ctx ev]\n  )"
    }

    fn description(&self) -> &str {
        "Janet"
    }
}

pub fn resolve(edit: &edit::Edit<Source>, client: &JanetClient) -> Graph<State, Transition> {
    let f = |symbol: &Option<String>| {
        symbol
            .as_deref()
            .map(|symbol| resolve_symbol(symbol, client))
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

// Resolve a symbol and return the result if it's a function.
pub fn resolve_symbol<'a>(symbol: impl Into<JanetSymbol<'a>>, client: &JanetClient) -> Janet {
    client
        .env()
        .and_then(|env| env.resolve(symbol))
        .and_then(|value| match value.unwrap() {
            TaggedJanet::Function(_) => Some(value),
            _ => None,
        })
        .unwrap_or_else(Janet::nil)
}

//#[cfg_attr(feature = "serde", derive(serde::Deserialize, serde::Serialize))]
pub struct Locals<S, T>(IntMap<usize, S>, IntMap<usize, T>);

impl<S, T> Default for Locals<S, T> {
    fn default() -> Self {
        Self(Default::default(), Default::default())
    }
}

impl<S, T> Locals<S, T>
where
    S: Clone,
    T: Clone,
{
    fn state(&mut self, i: Idx, s: &S) -> &mut S {
        self.0.entry(i.index()).or_insert_with(|| s.clone())
    }

    fn transition(&mut self, i: Tdx, t: &T) -> &mut T {
        self.1.entry(i.index()).or_insert_with(|| t.clone())
    }

    pub fn clear(&mut self) {
        self.0.clear();
        self.1.clear();
    }
}

pub struct JanetContext<'a> {
    pub client: &'a JanetClient,
    // State/transition local values.
    pub locals: Locals<Janet, Janet>,
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
/// TODO: make this a table and read the id from it?
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
            guard: resolve_symbol(symbol, client),
            local,
        }
    }
}

impl<'a> Context<Janet, Event> for JanetContext<'a> {
    type State = State;
    type Transition = Transition;

    fn dispatch(&mut self, event: &Event) {
        info!("dispatch: event: {:?}", event);
    }

    fn transition(&mut self, source: &State, target: &State) {
        info!("transition: source: {:?} target: {:?}", source, target);
    }

    fn enter(&mut self, inner: &mut Janet, event: Option<&Event>, state: &Self::State, index: Idx) {
        if let TaggedJanet::Function(mut f) = state.enter.unwrap() {
            match f.call([
                *self.locals.state(index, &state.local),
                *inner,
                event.map(|e| e.value).unwrap_or(Janet::nil()),
            ]) {
                Ok(_) => (),
                Err(e) => error!("error in enter: {e:?}"),
            }
        }
    }

    fn exit(&mut self, inner: &mut Janet, event: Option<&Event>, state: &Self::State, index: Idx) {
        if let TaggedJanet::Function(mut f) = state.exit.unwrap() {
            match f.call([
                *self.locals.state(index, &state.local),
                *inner,
                event.map(|e| e.value).unwrap_or(Janet::nil()),
            ]) {
                Ok(_) => (),
                Err(e) => error!("error in exit: {e:?}"),
            }
        }
    }

    // Do we even need this? Just put it in the guard. TODO
    fn filter(&mut self, event: &Event, transition: &Transition, _index: Tdx) -> bool {
        event.id == transition.id
    }

    fn guard(
        &mut self,
        inner: &mut Janet,
        event: &Event,
        transition: &Self::Transition,
        index: Tdx,
    ) -> bool {
        if let TaggedJanet::Function(mut f) = transition.guard.unwrap() {
            match f.call([
                *self.locals.transition(index, &transition.local),
                *inner,
                event.value,
            ]) {
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
    }
}

pub fn get_symbols(path: impl AsRef<Path>) -> Result<Option<edit::SymbolMap>, Error> {
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

        Ok(edit::SymbolMap::from_janet(res)
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
