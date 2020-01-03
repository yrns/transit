use anyhow::Result;
use petgraph::{
    graph::{IndexType, NodeIndex},
    stable_graph::StableDiGraph,
};
use ron::ser::{to_string_pretty, PrettyConfig};
use serde::{Deserialize, Serialize};
use std::boxed::Box;
use typetag;
//use serde_traitobject;

// TODO: use statecharts as as states, "includes"
// the statechart needs to implement some interface that makes it behave as a state

// use a stable graph for editing, maybe switch to DiGraph at runtime
// has an external map of ids to indexes, GraphMap doesn't serialize
#[derive(Serialize, Deserialize)]
pub struct Statechart<E, Ix: IndexType> {
    pub id: String,
    pub initial: Initial<Ix>,
    //ids: HashMap, do we need to externally reference by id?
    pub graph: StableDiGraph<State<Ix>, Transition<E>, Ix>,
    //context: C, // does the statechart need context for global mutable state?
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
pub struct State<Ix: IndexType> {
    // force these to be unique among siblings?
    pub id: String,
    pub initial: Initial<Ix>,
    // do we need a list of children?
    //pub states: Vec<NodeIndex<Ix>>,
    //#[serde(with = "serde_traitobject")]
    pub context: Box<dyn StateContext>,
    pub parent: Option<NodeIndex<Ix>>,
    //pub edit_data: EditData,
}

#[typetag::serde(tag = "type")]
pub trait StateContext {
    fn entry(&mut self) -> Result<()> {
        Ok(())
    }

    fn exit(&mut self) -> Result<()> {
        Ok(())
    }
}

#[derive(Serialize, Deserialize)]
pub struct DefaultStateContext;

#[typetag::serde]
impl StateContext for DefaultStateContext {}

#[typetag::serde(tag = "type")]
pub trait TransitionContext {
    fn action(&mut self) -> Result<()> {
        Ok(())
    }

    fn guard(&self) -> bool {
        true
    }
}

#[derive(Serialize, Deserialize)]
pub struct DefaultTransitionContext;

#[typetag::serde]
impl TransitionContext for DefaultTransitionContext {}

#[derive(Serialize, Deserialize)]
pub struct Transition<E> {
    pub event: E,
    //#[serde(with = "serde_traitobject")]
    pub context: Box<dyn TransitionContext>,
}

// trait Statechart {
//     type Context;
//     type Event;
// }

impl<'de, E: Serialize + Deserialize<'de>, Ix: IndexType + Serialize + Deserialize<'de>>
    Statechart<E, Ix>
{
    pub fn new(id: &str) -> Self {
        Self {
            id: id.to_string(),
            initial: Initial::None,
            graph: StableDiGraph::default(), //<State<Ix>, Transition<E>, Ix>::new(),
            active_state: None,
        }
    }

    pub fn export(&self) -> Result<String> {
        Ok(to_string_pretty(&self, PrettyConfig::default())?)
    }

    pub fn get(&self, i: NodeIndex<Ix>) -> &State<Ix> {
        &self.graph[i]
    }

    pub fn get_mut(&mut self, i: NodeIndex<Ix>) -> &mut State<Ix> {
        &mut self.graph[i]
    }

    pub fn add_state(&mut self, s: State<Ix>) -> Result<NodeIndex<Ix>> {
        Ok(self.graph.add_node(s))
    }

    pub fn set_initial(&mut self, initial: Initial<Ix>) -> Result<()> {
        self.initial = initial;
        Ok(())
    }

    pub fn add_transition<C: 'static + TransitionContext>(
        &mut self,
        a: NodeIndex<Ix>,
        b: NodeIndex<Ix>,
        event: E,
        ctx: C,
    ) -> Result<()> {
        self.graph
            .add_edge(a, b, Transition::new(event, Box::new(ctx)));
        Ok(())
    }

    // note: xstate takes the state to transition from
    pub fn transition(&mut self, _event: E) -> Result<()> {
        Ok(())
    }
}

impl<Ix: IndexType> State<Ix> {
    // FIX: static?
    pub fn new<C: 'static + StateContext>(id: &str, ctx: C, parent: Option<NodeIndex<Ix>>) -> Self {
        Self {
            id: id.to_string(),
            initial: Initial::None,
            context: Box::new(ctx),
            parent: parent,
        }
    }

    pub fn set_initial(&mut self, initial: Initial<Ix>) -> Result<()> {
        self.initial = initial;
        Ok(())
    }
}

impl<E> Transition<E> {
    pub fn new(event: E, context: Box<dyn TransitionContext>) -> Self {
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
