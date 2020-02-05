#![allow(unused_imports)]

use anyhow::{anyhow, Context as _, Result};
use petgraph::{
    graph::{IndexType, NodeIndex},
    stable_graph::StableDiGraph,
};
use ron::ser::{to_string_pretty, PrettyConfig};
use serde::{Deserialize, Serialize};
use std::any::Any;
use std::boxed::Box;
use std::collections::btree_map::{BTreeMap, Entry};
use std::collections::HashMap;
use std::fmt::Debug;
use std::iter::Iterator;
use std::mem::{discriminant, Discriminant};

// TODO: use statecharts as as states, "includes"
// the statechart needs to implement some interface that makes it behave as a state

pub type Idx = NodeIndex<u32>;

pub type ActionFn<C, E> = fn(&mut C, &E) -> Result<()>;

// Bindings?
pub trait Context: Sized + Clone {
    type Event: Debug;

    fn event(event: &str) -> Discriminant<Self::Event>;
    fn action(action: &str) -> &ActionFn<Self, Self::Event>;

    //fn entry(&mut self, event: &E) -> Result<()>
    //fn exit(&mut self, event: &E) -> Result<()>
    //fn guard(&mut self, event: &E) -> bool // Result?
    //fn action(&mut self, event: &E) -> Result<()>

    //fn bindings for guard/action
}

// separate state, actions (code), and events from the structure
pub struct Statechart<T> {
    pub context: T,
    pub graph: Graph,
    pub active: Option<Idx>,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Graph {
    pub id: String,
    pub initial: Initial,
    //ids: HashMap, do we need to externally reference by id?
    pub graph: StableDiGraph<State, Transition, u32>,
    //pub edit_data: EditData,
}

#[derive(Serialize, Deserialize, Debug)]
pub enum Initial {
    None,
    Initial(Idx),
    HistoryShallow(Idx),
    HistoryDeep(Idx),
}

#[derive(Serialize, Deserialize, Debug)]
pub struct State {
    // TODO: force these to be unique among siblings?
    pub id: String,
    pub initial: Initial,
    // do we need a list of children?
    //pub states: Vec<Idx>,
    pub parent: Option<Idx>,
    //pub edit_data: EditData,
    pub entry: Option<String>,
    pub exit: Option<String>,
}

// newtype event key?
// enum for both strings and bound version?
// can't serialize bound version
// wait on optimizing until macro gen?
#[derive(Serialize, Deserialize, Debug)]
pub struct Transition {
    event: String,
    guard: Option<String>,
    action: Option<String>,
}

impl Graph {
    pub fn new(id: &str) -> Self {
        Self {
            id: id.to_string(),
            initial: Initial::None,
            graph: StableDiGraph::default(),
        }
    }

    pub fn export(&self) -> Result<String> {
        Ok(to_string_pretty(&self, PrettyConfig::default())?)
    }

    pub fn get(&self, i: Idx) -> &State {
        &self.graph[i]
    }

    pub fn get_mut(&mut self, i: Idx) -> &mut State {
        &mut self.graph[i]
    }

    pub fn add_state(&mut self, s: State) -> Result<Idx> {
        Ok(self.graph.add_node(s))
    }

    // the only reason we'd want history at the root of a statechart
    // is if it's used inside another statechart - TODO:
    pub fn set_initial(&mut self, i: Idx) -> Result<()> {
        self.initial = Initial::Initial(i);
        Ok(())
    }

    pub fn get_initial(&self, i: Idx) -> Option<Idx> {
        match self.graph[i].initial {
            Initial::None => Some(i),
            Initial::Initial(i) => self.get_initial(i),
            Initial::HistoryShallow(i) => self.get_initial(i),
            Initial::HistoryDeep(i) => self.get_initial(i),
        }
    }

    pub fn add_transition(&mut self, a: Idx, b: Idx, t: Transition) -> Result<()> {
        self.graph.add_edge(a, b, t);
        Ok(())
    }

    // returns an iterator from idx -> root
    pub fn path_iter<'a>(&'a self, i: Idx) -> PathIter<'a> {
        PathIter::new(&self, i)
    }

    // path from root -> idx as a vec
    pub fn path(&self, i: Idx) -> Vec<Idx> {
        let mut path = self.path_iter(i).collect::<Vec<_>>();
        path.reverse();
        path
    }

    pub fn path_str(&self, i: Idx) -> String {
        self.path(i)
            .iter()
            .map(|i| self.graph[*i].id.clone())
            .collect::<Vec<String>>()
            .join("::")
    }

    pub fn parent(&self, i: Idx) -> Option<Idx> {
        self.graph[i].parent
    }

    pub fn common_ancestor(&self, s1: Idx, s2: Idx) -> Option<Idx> {
        self.path(s1)
            .iter()
            .zip(self.path(s2).iter())
            .find(|(a, b)| a != b)
            // both states should have the same parent
            .and_then(|(i, _)| self.parent(*i))
    }
}

impl<C: Context> Statechart<C> {
    pub fn new(graph: Graph, context: C) -> Result<Self> {
        // TODO: verify bindings
        Ok(Self {
            graph,
            context,
            active: None,
        })
    }

    // return option? id()?
    pub fn active(&self) -> &str {
        if let Some(idx) = self.active {
            &self.graph.graph[idx].id
        } else {
            "none"
        }
    }

    pub fn reset(&mut self) -> Result<()> {
        match self.graph.initial {
            Initial::None => Err(anyhow!("no initial state set")),
            Initial::Initial(i) => {
                // recursively find the active state
                self.active = Some(self.graph.get_initial(i).unwrap_or(i));
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

    // TODO: self-transitions?
    pub fn transition(&mut self, event: C::Event) -> Result<()> {
        let d = discriminant(&event);
        let mut scc = self.context.clone();

        // find a matching transition from the active state
        if let Some(idx) = self.active {
            let mut next: Option<Idx> = None;
            let mut edges = self.graph.graph.neighbors(idx).detach();

            while let Some((edge, node)) = edges.next(&self.graph.graph) {
                let t = &mut self.graph.graph[edge];
                if C::event(&t.event) == d {
                    // an error in the guard just means it didn't
                    // pass, we probably want to log something though
                    let guard_pass = t
                        .guard
                        .as_ref()
                        .map_or(true, |g| C::action(&g)(&mut scc, &event).is_ok());
                    if guard_pass {
                        if let Some(ref action) = t.action {
                            C::action(action)(&mut scc, &event).with_context(|| {
                                format!(
                                    "action failed on {:?} with event: {:?}",
                                    // better id for transitions?
                                    edge,
                                    event
                                )
                            })?;
                        }
                        next = Some(node);
                        break;
                    }
                }
            }

            // traverse states up to a common ancestor (calling exit
            // on each) and back down to the next state (calling
            // entry)
            if let Some(next) = next {
                let a = self.graph.common_ancestor(idx, next);

                // list of history updates, only applied after the
                // transition succeeds
                let mut h: Vec<(Idx, Idx)> = Vec::new();

                let p1 = self.graph.path_iter(idx).collect::<Vec<_>>();
                let mut last = idx;
                let res: Result<Vec<()>> = p1
                    .iter()
                    .take_while(|i| a.map_or(true, |a| **i != a))
                    .map(|i| {
                        let s = &mut self.graph.graph[*i];
                        if let Some(ref exit) = s.exit {
                            C::action(exit)(&mut scc, &event).with_context(|| {
                                format!(
                                    "exit failed on {:?} with event: {:?}",
                                    i,
                                    //self.graph.path_str(*i),
                                    event
                                )
                            })?
                        }
                        // track history when exiting a state
                        match s.initial {
                            Initial::HistoryShallow(_) => h.push((*i, last)),
                            Initial::HistoryDeep(_) => h.push((*i, idx)),
                            _ => (),
                        };
                        // you can't have a history with no child
                        // states, and we're not checking that
                        // here - FIX?
                        last = *i;

                        Ok(())
                    })
                    .collect();

                let res: Result<Vec<()>> = res.and_then(|_| {
                    self.graph
                        .path(next)
                        .iter()
                        .take_while(|i| a.map_or(true, |a| **i != a))
                        .map(|i| {
                            let s = &mut self.graph.graph[*i];
                            if let Some(ref entry) = s.entry {
                                C::action(entry)(&mut scc, &event).with_context(|| {
                                    format!(
                                        "entry failed on {:?} with event: {:?}",
                                        i,
                                        //self.graph.path_str(*i),
                                        event
                                    )
                                })?
                            }
                            Ok(())
                        })
                        .collect()
                });

                res.with_context(|| {
                    format!(
                        "transition from {} to {} failed",
                        self.graph.path_str(idx),
                        self.graph.path_str(next)
                    )
                })?;

                // copy back mutated state context
                self.context = scc;

                // apply history
                for (idx, prev) in h {
                    let s = &mut self.graph.graph[idx];
                    match s.initial {
                        Initial::HistoryShallow(ref mut i) | Initial::HistoryDeep(ref mut i) => {
                            *i = prev
                        }
                        _ => (),
                    }
                }

                // set active state
                self.active = Some(next);

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

pub struct PathIter<'a> {
    graph: &'a Graph,
    idx: Option<Idx>,
}

impl<'a> PathIter<'a> {
    pub fn new(graph: &'a Graph, idx: Idx) -> Self {
        Self {
            graph,
            idx: Some(idx),
        }
    }
}

impl<'a> Iterator for PathIter<'a> {
    type Item = Idx;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(idx) = self.idx {
            self.idx = self.graph.graph[idx].parent;
            Some(idx)
        } else {
            None
        }
    }
}

impl State {
    // entry/exit defaults to path?
    // if we use path, moving the state requires renaming functions
    // if we only use id then all ids must be globally unique
    // TODO:
    pub fn new(id: &str, parent: Option<Idx>, entry: Option<&str>, exit: Option<&str>) -> Self {
        Self {
            id: id.to_string(),
            initial: Initial::None,
            parent,
            entry: entry.map(String::from),
            exit: exit.map(String::from),
        }
    }

    pub fn set_initial(&mut self, initial: Initial) -> Result<()> {
        self.initial = initial;
        Ok(())
    }
}

impl Transition {
    pub fn new(event: &str, guard: Option<&str>, action: Option<&str>) -> Self {
        Self {
            event: event.to_string(),
            guard: guard.map(String::from),
            action: action.map(String::from),
        }
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
