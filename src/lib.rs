#![allow(unused_imports)]

use anyhow::Result;
//use enum_dispatch::enum_dispatch;
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
//use std::mem::Discriminant;
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
pub struct Statechart<Sc, Tc, Scc, E, Ix: IndexType>
// where
//     Sc: StateContext<Scc, E>,
//     Tc: TransitionContext<Scc, E>,
{
    pub id: String,
    pub initial: Initial<Ix>,
    //ids: HashMap, do we need to externally reference by id?
    pub graph: StableDiGraph<State<Sc, Ix>, Transition<Tc, E>, Ix>,
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

#[derive(Serialize, Deserialize)]
pub struct State<Sc, Ix: IndexType> {
    // force these to be unique among siblings?
    pub id: String,
    pub initial: Initial<Ix>,
    // do we need a list of children?
    //pub states: Vec<NodeIndex<Ix>>,
    //#[serde(deserialize_with = "Statechart::deserialize_state_context")]
    pub context: Sc,
    pub parent: Option<NodeIndex<Ix>>,
    //pub edit_data: EditData,
}

//#[typetag::serialize]
//#[enum_dispatch]
pub trait StateContext<Scc, E> {
    fn entry(&mut self, _ctx: &mut Scc, _event: &E) -> Result<()> {
        Ok(())
    }

    fn exit(&mut self, _ctx: &mut Scc, _event: &E) -> Result<()> {
        Ok(())
    }
}

#[derive(Serialize, Deserialize)]
pub struct DefaultStateContext;

impl<Scc, E> StateContext<Scc, E> for DefaultStateContext {}

//#[enum_dispatch]
pub trait TransitionContext<Scc, E> {
    fn action(&mut self, _ctx: &mut Scc, _event: &E) -> Result<()> {
        Ok(())
    }

    fn guard(&mut self, _ctx: &mut Scc, _event: &E) -> bool {
        true
    }
}

#[derive(Serialize, Deserialize)]
pub struct DefaultTransitionContext;

impl<Scc, E> TransitionContext<Scc, E> for DefaultTransitionContext {}

#[derive(Serialize, Deserialize)]
pub struct Transition<Tc, E> {
    // serde doesn't do discriminants
    //pub event: Discriminant<E>,
    pub event: E,
    //#[serde(with = "serde_traitobject")]
    pub context: Tc,
}

// use this to remove Scc, E?
// trait StatechartContext {
//     type Event;
// }

//impl<'de, E: Serialize + Deserialize<'de>, Ix: IndexType + Serialize + Deserialize<'de>>
impl<'de, Sc, Tc, Scc, E, Ix: IndexType + Serialize> Statechart<Sc, Tc, Scc, E, Ix>
where
    Sc: StateContext<Scc, E> + Serialize + Deserialize<'de>,
    Tc: TransitionContext<Scc, E> + Serialize + Deserialize<'de>,
    Scc: Serialize + Deserialize<'de>,
    E: Serialize + Deserialize<'de>,
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

    pub fn get(&self, i: NodeIndex<Ix>) -> &State<Sc, Ix> {
        &self.graph[i]
    }

    pub fn get_mut(&mut self, i: NodeIndex<Ix>) -> &mut State<Sc, Ix> {
        &mut self.graph[i]
    }

    pub fn add_state(&mut self, s: State<Sc, Ix>) -> Result<NodeIndex<Ix>> {
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
        ctx: Tc,
    ) -> Result<()> {
        self.graph.add_edge(a, b, Transition::new(event, ctx));
        Ok(())
    }

    // note: xstate takes the state to transition from
    pub fn transition(&mut self, _event: E) -> Result<()> {
        Ok(())
    }
}

impl<Sc, Ix: IndexType> State<Sc, Ix> {
    pub fn new(id: &str, context: Sc, parent: Option<NodeIndex<Ix>>) -> Self {
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

impl<Tc, E> Transition<Tc, E> {
    pub fn new(event: E, context: Tc) -> Self {
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
