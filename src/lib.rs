#![allow(unused_imports)]

use anyhow::{anyhow, Result};
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
use std::fmt::Debug;
use std::iter::Iterator;
use std::mem::discriminant;
use typetag::{
    inventory::{collect, submit},
    DeserializeFn, Registry,
};
//use serde_traitobject;

// TODO: use statecharts as as states, "includes"
// the statechart needs to implement some interface that makes it behave as a state

// use a stable graph for editing, maybe switch to DiGraph at runtime
// has an external map of ids to indexes, GraphMap doesn't serialize
#[derive(Serialize, Deserialize, Debug)]
pub struct Statechart<Sc, Tc, Scc: Clone, E, Ix: IndexType>
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

#[derive(Serialize, Deserialize, Debug)]
pub enum Initial<Ix: IndexType> {
    None,
    Initial(NodeIndex<Ix>),
    HistoryShallow(NodeIndex<Ix>),
    HistoryDeep(NodeIndex<Ix>),
}

#[derive(Serialize, Deserialize, Debug)]
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

#[derive(Serialize, Deserialize, Debug)]
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

#[derive(Serialize, Deserialize, Debug)]
pub struct DefaultTransitionContext;

impl<Scc, E> TransitionContext<Scc, E> for DefaultTransitionContext {}

#[derive(Serialize, Deserialize, Debug)]
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
impl<'de, Sc, Tc, Scc: Clone, E, Ix: IndexType + Serialize> Statechart<Sc, Tc, Scc, E, Ix>
where
    Sc: StateContext<Scc, E> + Serialize + Deserialize<'de> + Debug,
    Tc: TransitionContext<Scc, E> + Serialize + Deserialize<'de> + Debug,
    Scc: Serialize + Deserialize<'de> + Debug,
    E: Serialize + Deserialize<'de> + Debug,
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

    // the only reason we'd want history at the root of a statechart
    // is if it's used inside another statechart - TODO:
    pub fn set_initial(&mut self, idx: NodeIndex<Ix>) -> Result<()> {
        self.initial = Initial::Initial(idx);
        Ok(())
    }

    pub fn get_initial(&self, idx: NodeIndex<Ix>) -> Option<NodeIndex<Ix>> {
        match self.graph[idx].initial {
            Initial::None => Some(idx),
            Initial::Initial(idx) => self.get_initial(idx),
            Initial::HistoryShallow(idx) => self.get_initial(idx),
            Initial::HistoryDeep(idx) => self.get_initial(idx),
        }
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

    pub fn reset(&mut self) -> Result<()> {
        match self.initial {
            Initial::None => Err(anyhow!("no initial state set")),
            Initial::Initial(idx) => {
                // recursively find the active state
                self.active_state = Some(self.get_initial(idx).unwrap_or(idx));
                Ok(())
            }
            Initial::HistoryShallow(_) | Initial::HistoryDeep(_) => {
                // does this make sense at all?
                Err(anyhow!("invalid initial value (history)"))
            }
        }
    }

    // set initial states and return a channel?
    pub fn run(&mut self) {
        self.reset().unwrap();
    }

    // returns an iterator from idx -> root
    pub fn path_iter<'a>(&'a self, idx: NodeIndex<Ix>) -> PathIter<'a, Sc, Tc, Scc, E, Ix> {
        PathIter::new(&self, idx)
    }

    // path from root -> idx as a vec
    pub fn path(&self, idx: NodeIndex<Ix>) -> Vec<NodeIndex<Ix>> {
        let mut path = self.path_iter(idx).collect::<Vec<_>>();
        path.reverse();
        path
    }

    pub fn path_str(&self, idx: NodeIndex<Ix>) -> String {
        self.path(idx)
            .iter()
            .map(|i| self.graph[*i].id.clone())
            .collect::<Vec<String>>()
            .join("::")
    }

    pub fn parent(&self, idx: NodeIndex<Ix>) -> Option<NodeIndex<Ix>> {
        self.graph[idx].parent
    }

    pub fn common_ancestor(&self, s1: NodeIndex<Ix>, s2: NodeIndex<Ix>) -> Option<NodeIndex<Ix>> {
        self.path(s1)
            .iter()
            .zip(self.path(s2).iter())
            .find(|(a, b)| a != b)
            // both states should have the same parent
            .and_then(|(i, _)| self.parent(*i))
    }

    pub fn active_state(&self) -> &str {
        if let Some(idx) = self.active_state {
            &self.graph[idx].id
        } else {
            "none"
        }
    }

    // TODO: self-transitions?
    pub fn transition(&mut self, event: E) -> Result<()> {
        let d = discriminant(&event);
        let mut scc = self.context.clone();

        // find a matching transition from the active state
        if let Some(idx) = self.active_state {
            let mut next: Option<NodeIndex<Ix>> = None;
            let mut edges = self.graph.neighbors(idx).detach();

            while let Some((edge, node)) = edges.next(&self.graph) {
                let t = &mut self.graph[edge];
                if discriminant(&t.event) == d {
                    if t.context.guard(&mut scc, &event) {
                        t.context.action(&mut scc, &event)?;
                        next = Some(node);
                        break;
                    }
                }
            }

            // traverse states up to a common ancestor (calling exit
            // on each) and back down to the next state (calling
            // entry) - if there is an error here we may be in a weird
            // state: FIX
            if let Some(next) = next {
                let a = self.common_ancestor(idx, next);

                let p1 = self.path_iter(idx).collect::<Vec<_>>();
                let mut last = idx;
                let res: Result<Vec<()>> = p1
                    .iter()
                    .take_while(|i| a.map_or(true, |a| **i != a))
                    .map(|i| {
                        // track history
                        let s = &mut self.graph[*i];
                        s.context.exit(&mut scc, &event).and_then(|_| {
                            match s.initial {
                                Initial::HistoryShallow(ref mut h) => *h = last,
                                Initial::HistoryDeep(ref mut h) => *h = idx,
                                _ => (),
                            };
                            // you can't have a history with no child
                            // states, and we're not checking that
                            // here - FIX?
                            last = *i;
                            Ok(())
                        })
                    })
                    .collect();

                let res: Result<Vec<()>> = res.and_then(|_| {
                    self.path(next)
                        .iter()
                        .take_while(|i| a.map_or(true, |a| **i != a))
                        .map(|i| self.graph[*i].context.entry(&mut scc, &event))
                        .collect()
                });

                res?;

                // copy back mutated state context
                self.context = scc;

                // set active state
                self.active_state = Some(next);

                Ok(())
            } else {
                Err(anyhow!("no valid transitions"))
            }
        // set active state, recursively work through initial states
        } else {
            Err(anyhow!("no active state"))
        }
    }
}

pub struct PathIter<'a, Sc, Tc, Scc: Clone, E, Ix: IndexType> {
    statechart: &'a Statechart<Sc, Tc, Scc, E, Ix>,
    idx: Option<NodeIndex<Ix>>,
}

impl<'a, Sc, Tc, Scc: Clone, E, Ix: IndexType> PathIter<'a, Sc, Tc, Scc, E, Ix> {
    pub fn new(statechart: &'a Statechart<Sc, Tc, Scc, E, Ix>, idx: NodeIndex<Ix>) -> Self {
        Self {
            statechart,
            idx: Some(idx),
        }
    }
}

impl<'a, Sc, Tc, Scc: Clone, E, Ix: IndexType> Iterator for PathIter<'a, Sc, Tc, Scc, E, Ix> {
    type Item = NodeIndex<Ix>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(idx) = self.idx {
            self.idx = self.statechart.graph[idx].parent;
            Some(idx)
        } else {
            None
        }
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
    use super::*;

    #[test]
    fn it_works() {
        println!("w/ macros");
    }
}
