#![allow(unused_imports)]

use anyhow::Result;
use lazy_static::lazy_static;
use petgraph::{
    graph::{IndexType, NodeIndex},
    stable_graph::StableDiGraph,
};
use ron::ser::{to_string_pretty, PrettyConfig};
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use std::any::Any;
use std::boxed::Box;
use std::collections::btree_map::{BTreeMap, Entry};
use typetag::{
    inventory::{collect, submit},
    DeserializeFn, Registry,
};
//use serde_traitobject;

// TODO: use statecharts as as states, "includes"
// the statechart needs to implement some interface that makes it behave as a state

// use a stable graph for editing, maybe switch to DiGraph at runtime
// has an external map of ids to indexes, GraphMap doesn't serialize
#[derive(Serialize, Deserialize)]
pub struct Statechart<Scc, E, Ix: IndexType>
where
    dyn StateContext<Scc, E>: Registered,
    dyn TransitionContext<Scc, E>: Registered,
{
    pub id: String,
    pub initial: Initial<Ix>,
    //ids: HashMap, do we need to externally reference by id?
    pub graph: StableDiGraph<State<Scc, E, Ix>, Transition<Scc, E>, Ix>,
    context: Scc,
    // use lenses for state context data?
    //pub edit_data: EditData,
    pub active_state: Option<NodeIndex<Ix>>,
}

#[derive(Serialize, Deserialize)]
pub enum Initial<Ix: IndexType> {
    None,
    Initial(NodeIndex<Ix>),
    History(NodeIndex<Ix>),
}

// generic over graph index?
#[derive(Serialize, Deserialize)]
pub struct State<Scc, E, Ix: IndexType>
where
    dyn StateContext<Scc, E>: Registered,
{
    // force these to be unique among siblings?
    pub id: String,
    pub initial: Initial<Ix>,
    // do we need a list of children?
    //pub states: Vec<NodeIndex<Ix>>,
    //#[serde(deserialize_with = "Statechart::deserialize_state_context")]
    pub context: Box<dyn StateContext<Scc, E>>,
    pub parent: Option<NodeIndex<Ix>>,
    //pub edit_data: EditData,
}

#[typetag::serialize]
pub trait StateContext<Scc, E> {
    fn entry(&mut self, _ctx: Scc, _event: E) -> Result<()> {
        Ok(())
    }

    fn exit(&mut self, _ctx: Scc, _event: E) -> Result<()> {
        Ok(())
    }
}

pub struct Registration<T: ?Sized> {
    name: &'static str,
    deserializer: DeserializeFn<T>,
}

trait Registered {
    //type Object: ?Sized;

    //fn registry() -> &'static Registry<Self::Object>;
    fn registry<'a>() -> &'static Registry<Self>;
    fn register(name: &'static str, deserializer: DeserializeFn<Self>) -> Registration<Self>;
}

//impl<T, Scc, E> Registered<T> for dyn StateContext<Scc, E> {}

pub fn build_registry<T, U>(iter: U) -> Registry<T>
where
    T: ?Sized,
    U: IntoIterator<Item = Registration<T>>,
{
    let mut map = BTreeMap::new();
    let mut names = Vec::new();
    for registered in iter {
        match map.entry(registered.name) {
            Entry::Vacant(entry) => {
                entry.insert(Some(registered.deserializer));
            }
            Entry::Occupied(mut entry) => {
                entry.insert(None);
            }
        }
        names.push(registered.name);
    }
    names.sort_unstable();
    typetag::Registry { map, names }
}

// impl<'de, Scc, E> Deserialize<'de> for Box<dyn StateContext<Scc, E>> {
//     fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
//     where
//         D: Deserializer<'de>,
//     {
//         panic!("not registered");
//     }
// }

impl<'de, T> Deserialize<'de> for Box<T>
where
    T: ?Sized + Registered,
{
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        // let registry = T::registry();
        let registry = build_registry::<T, _>(vec![]);
        typetag::externally::deserialize(deserializer, "FIX", &registry)
    }
}

impl<'de, Scc, E> Deserialize<'de> for Box<dyn StateContext<Scc, E>>
where
    // Scc: 'a,
    // E: 'a,
    dyn StateContext<Scc, E>: Registered,
    //Self: Registered,
{
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let registry = <dyn StateContext<Scc, E>>::registry();

        typetag::externally::deserialize(deserializer, "FIX", registry)
    }
}

#[derive(Serialize, Deserialize)]
pub struct DefaultStateContext;

#[typetag::serialize]
impl<Scc, E> StateContext<Scc, E> for DefaultStateContext {}

#[typetag::serialize]
pub trait TransitionContext<Scc, E> {
    fn action(&mut self, _ctx: Scc, _event: E) -> Result<()> {
        Ok(())
    }

    fn guard(&mut self, _ctx: Scc, _event: E) -> bool {
        true
    }
}

impl<'a, 'de, Scc, E> Deserialize<'de> for Box<dyn TransitionContext<Scc, E> + 'a>
where
    // Scc: 'static,
    // E: 'static,
    dyn TransitionContext<Scc, E>: Registered,
{
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        typetag::externally::deserialize(
            deserializer,
            "FIX",
            <dyn TransitionContext<Scc, E>>::registry(),
        )
    }
}

#[derive(Serialize, Deserialize)]
pub struct DefaultTransitionContext;

#[typetag::serialize]
impl<Scc, E> TransitionContext<Scc, E> for DefaultTransitionContext {}

#[derive(Serialize, Deserialize)]
pub struct Transition<Scc, E>
where
    dyn TransitionContext<Scc, E>: Registered,
{
    pub event: E,
    //#[serde(with = "serde_traitobject")]
    pub context: Box<dyn TransitionContext<Scc, E>>,
}

// trait Statechart {
//     type Context;
//     type Event;
// }

//impl<'de, E: Serialize + Deserialize<'de>, Ix: IndexType + Serialize + Deserialize<'de>>
impl<Scc: Serialize, E: Serialize, Ix: IndexType + Serialize> Statechart<Scc, E, Ix>
where
    dyn StateContext<Scc, E>: Registered,
    dyn TransitionContext<Scc, E>: Registered,
{
    pub fn new(id: &str, context: Scc) -> Self {
        Self {
            id: id.to_string(),
            initial: Initial::None,
            graph: StableDiGraph::default(), //<State<Ix>, Transition<E>, Ix>::new(),
            context,
            active_state: None,
        }
    }

    pub fn export(&self) -> Result<String> {
        Ok(to_string_pretty(&self, PrettyConfig::default())?)
    }

    pub fn get(&self, i: NodeIndex<Ix>) -> &State<Scc, E, Ix> {
        &self.graph[i]
    }

    pub fn get_mut(&mut self, i: NodeIndex<Ix>) -> &mut State<Scc, E, Ix> {
        &mut self.graph[i]
    }

    pub fn add_state(&mut self, s: State<Scc, E, Ix>) -> Result<NodeIndex<Ix>> {
        Ok(self.graph.add_node(s))
    }

    pub fn set_initial(&mut self, initial: Initial<Ix>) -> Result<()> {
        self.initial = initial;
        Ok(())
    }

    pub fn add_transition(
        &mut self,
        a: NodeIndex<Ix>,
        b: NodeIndex<Ix>,
        event: E,
        ctx: Box<dyn TransitionContext<Scc, E>>,
    ) -> Result<()> {
        self.graph.add_edge(a, b, Transition::new(event, ctx));
        Ok(())
    }

    // note: xstate takes the state to transition from
    pub fn transition(&mut self, _event: E) -> Result<()> {
        Ok(())
    }
}

impl<Scc, E, Ix: IndexType> State<Scc, E, Ix>
where
    dyn StateContext<Scc, E>: Registered,
{
    pub fn new(
        id: &str,
        context: Box<dyn StateContext<Scc, E>>,
        parent: Option<NodeIndex<Ix>>,
    ) -> Self {
        Self {
            id: id.to_string(),
            initial: Initial::None,
            context,
            parent,
        }
    }

    pub fn set_initial(&mut self, initial: Initial<Ix>) -> Result<()> {
        self.initial = initial;
        Ok(())
    }
}

impl<Scc, E> Transition<Scc, E>
where
    dyn TransitionContext<Scc, E>: Registered,
{
    pub fn new(event: E, context: Box<dyn TransitionContext<Scc, E>>) -> Self {
        Self { event, context }
    }
}

#[cfg(test)]
mod tests {
    //use petgraph::graphmap::DiGraphMap;
    use petgraph::graph::UnGraph;
    //use serde_json;
    use ron::ser::{to_string_pretty, PrettyConfig};

    #[test]
    fn it_works() {
        let g = UnGraph::<i32, ()>::from_edges(&[(1, 2), (2, 3), (3, 4), (1, 4)]);

        //let mut g = DiGraphMap::new();
        //g.add_edge("x", "y", -1);

        let pretty = PrettyConfig::default();
        let s = to_string_pretty(&g, pretty).unwrap();

        //let s = serde_json::to_string(&g).unwrap();

        println!("{}", s);
    }
}
