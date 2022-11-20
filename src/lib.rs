//#![allow(unused_imports)]

//use anyhow::{anyhow, Context as _, Result};
use petgraph::{
    graph::{EdgeIndex, NodeIndex},
    stable_graph::StableDiGraph,
};
// use ron::de::from_reader;
// use ron::ser::{to_string_pretty, PrettyConfig};
// use serde::{Deserialize, Serialize};
// use std::fs::File;
// use std::io::prelude::*;
use std::iter::Iterator;
//use std::path::{Path, PathBuf};
use std::sync::Arc;

// TODO: use statecharts as as states, "includes"
// the statechart needs to implement some interface that makes it behave as a state

pub type Idx = NodeIndex<u32>;
pub type Tdx = EdgeIndex<u32>;

/// Both states and transitions require [Clone] since they are stored in
/// the graph inside [Arc]s which are used for clone-on-write and
/// undo/redo operations.
pub trait Context
where
    Self: Sized + Clone,
{
    type Event: std::fmt::Debug;
    /// The root state uses the default.
    type State: State<Self> + Clone + Default;
    type Transition: Transition<Self> + Clone;
    //type Index;

    fn dispatch(&mut self, _event: &Self::Event) {}
    fn transition(&mut self, _source: &Self::State, _target: &Self::State) {}
}

// separate state, actions (code), and events from the structure
// This is runtime (not serialized)?
pub struct Statechart<C: Context> {
    pub context: C,
    pub graph: Graph<C>,
    pub active: Idx,
}

//#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Graph<C: Context> {
    // There are Arcs for undo/redo?
    pub graph: StableDiGraph<Arc<StateState<C>>, Arc<TransitionData<C::Transition>>, u32>,
    pub root: Idx,
}

#[derive(/* Serialize, Deserialize,*/ Clone, Debug, Default, PartialEq, Eq)]
pub enum Initial {
    #[default]
    None,
    Initial(Idx),
    HistoryShallow(Idx),
    HistoryDeep(Idx),
}

impl Initial {
    pub fn idx(&self) -> Option<Idx> {
        match *self {
            Initial::Initial(i) | Initial::HistoryShallow(i) | Initial::HistoryDeep(i) => Some(i),
            Initial::None => None,
        }
    }

    pub fn step(self) -> Self {
        match self {
            // can't step from none
            Initial::None => self,
            Initial::Initial(i) => Initial::HistoryShallow(i),
            Initial::HistoryShallow(i) => Initial::HistoryDeep(i),
            // can't step to none? how do we unset?
            Initial::HistoryDeep(i) => Initial::Initial(i),
        }
    }
}

pub trait State<C: Context> {
    // Pass event? Pass context?
    fn enter(&mut self, ctx: &mut C, event: Option<&C::Event>);
    fn exit(&mut self, ctx: &mut C, event: Option<&C::Event>);
}

//#[derive(Serialize, Deserialize, Debug, Clone)]
#[derive(Clone)]
pub struct StateState<C>
where
    C: Context,
{
    pub initial: Initial,
    pub parent: Option<Idx>,
    pub state: C::State,
}

// We can't derive this since Context isn't Default?
impl<C: Context> Default for StateState<C> {
    fn default() -> Self {
        Self {
            initial: Initial::default(),
            parent: None,
            state: C::State::default(),
        }
    }
}

/// An internal transition is only valid for self-transitions. The
/// guard will be called, but enter/exit will not; and the active
/// state will not change. This is relevant for self-transitions on
/// parent states, which would otherwise transition from child ->
/// parent, if external.
#[derive(Clone)]
pub struct TransitionData<T> {
    transition: T,
    internal: bool,
}

impl<T> From<T> for TransitionData<T> {
    fn from(transition: T) -> Self {
        Self {
            transition,
            internal: false,
        }
    }
}

pub trait Internal
where
    Self: Sized,
{
    fn internal(self) -> TransitionData<Self>;
}

impl<T> Internal for T {
    fn internal(self) -> TransitionData<T> {
        TransitionData {
            transition: self,
            internal: true,
        }
    }
}

pub trait Transition<C: Context> {
    fn guard(&mut self, ctx: &mut C, event: &C::Event) -> bool;
    //fn action(&mut self); ???
}

impl<C: Context> Graph<C> {
    pub fn new() -> Self {
        let mut graph = StableDiGraph::default();
        let root = graph.add_node(Arc::new(StateState::default()));
        Self { graph, root }
    }

    // pub fn export(&self) -> Result<String> {
    //     Ok(to_string_pretty(&self, PrettyConfig::default())?)
    // }

    // pub fn export_to_file(&self, path: &Path) -> Result<()> {
    //     let mut file = File::create(path)?;
    //     file.write_all(self.export()?.as_bytes())?;
    //     Ok(())
    // }

    // pub fn import_from_file(path: &Path) -> Result<Self> {
    //     Ok(from_reader(File::open(path)?)?)
    // }

    pub fn add_state(&mut self, state: C::State, parent: Option<Idx>) -> Idx {
        let a = self
            .graph
            .add_node(Arc::new(StateState::new(state, parent.or(Some(self.root)))));
        // the first added node becomes initial for the root
        // TODO: validation?
        // if p.is_none() && self.initial == Initial::None {
        //     self.initial = Initial::Initial(a);
        // }
        a
    }

    pub fn remove_state(&mut self, i: Idx) -> Option<Arc<StateState<C>>> {
        // TODO: clean up transitions? move transitions to parent?
        // clean up history
        self.graph.remove_node(i)
    }

    /// Get a state reference directly.
    pub fn state(&self, i: Idx) -> Option<&C::State> {
        self.graph.node_weight(i).map(|s| &s.state)
    }

    /// Get a mutable state reference directly.   
    pub fn state_mut(&mut self, i: Idx) -> Option<&mut C::State> {
        // Should there be a version of this that takes a closure and
        // compares with the old version? `update_state`? This clones
        // every time even if nothing is changed.
        self.graph.node_weight_mut(i).map(|s| {
            let s = Arc::make_mut(s);
            &mut s.state
        })
    }

    /// Get a transition reference.
    pub fn transition(&self, i: Tdx) -> Option<&C::Transition> {
        self.graph.edge_weight(i).map(|t| &t.transition)
    }

    /// Get a mutable transition reference.
    pub fn transition_mut(&mut self, i: Tdx) -> Option<&mut C::Transition> {
        self.graph.edge_weight_mut(i).map(|t| {
            let t = Arc::make_mut(t);
            &mut t.transition
        })
    }

    pub fn contains_state(&self, i: Idx) -> bool {
        self.graph.contains_node(i)
    }

    pub fn contains_transition(&self, i: Tdx) -> bool {
        self.graph.edge_weight(i).is_some()
    }

    pub fn children(&self, p: Option<Idx>) -> impl Iterator<Item = Idx> + '_ {
        self.graph
            .node_indices()
            .filter(move |i| self.graph[*i].parent == p)
    }

    pub fn siblings(&self, i: Idx) -> impl Iterator<Item = Idx> + '_ {
        self.children(self.graph[i].parent).filter(move |s| *s != i)
    }

    pub fn transitions(&self) -> impl Iterator<Item = Tdx> + '_ {
        self.graph.edge_indices()
    }

    pub fn endpoints(&self, i: Tdx) -> Option<(Idx, Idx)> {
        self.graph.edge_endpoints(i)
    }

    pub fn is_self_transition(&self, i: Tdx) -> bool {
        if let Some((a, b)) = self.endpoints(i) {
            a == b
        } else {
            false
        }
    }

    // pub fn set_id(&mut self, i: Option<Idx>, id: &str) {
    //     if let Some(i) = i {
    //         Arc::make_mut(&mut self.graph[i]).id = id.to_string();
    //     } else {
    //         // set graph id
    //         self.id = id.to_string();
    //     }
    // }

    // This belongs in editor.
    // pub fn is_unique_id(&self, i: Idx, id: &str) -> bool {
    //     self.siblings(i)
    //         .map(|i| &self.graph[i].id)
    //         .all(|id2| id2 != id)
    // }

    // move state, check id for uniqueness among new siblings?
    // TODO: do something with all cases of make_mut except indexing?
    pub fn set_parent(&mut self, i: Idx, p: Option<Idx>) {
        Arc::make_mut(&mut self.graph[i]).parent = p;
    }

    // the only reason we'd want history at the root of a statechart
    // is if it's used inside another statechart - TODO:
    pub fn set_initial(&mut self, i: Idx) {
        assert!(self.graph.contains_node(i));
        Arc::make_mut(&mut self.graph[self.root]).initial = Initial::Initial(i);
    }

    // return true if b is a child of a
    pub fn is_child(&self, a: Idx, b: Idx) -> bool {
        self.path_iter(b).find(|i| *i == a).is_some()
    }

    pub fn get_initial(&self, i: Idx) -> Idx {
        match self.graph[i].initial {
            // no initial state, return i
            Initial::None => i,
            Initial::Initial(i) => self.get_initial(i),
            Initial::HistoryShallow(i) => self.get_initial(i),
            Initial::HistoryDeep(i) => self.get_initial(i),
        }
    }

    // Never used.
    // pub fn initial_idx(&self, a: Option<Idx>) -> Option<Idx> {
    //     if let Some(a) = a {
    //         self[a].initial.idx()
    //     } else {
    //         self.initial.idx()
    //     }
    // }

    pub fn add_transition(&mut self, a: Idx, b: Idx, t: impl Into<TransitionData<C::Transition>>) {
        let t = t.into();
        assert!(!t.internal || a == b);
        self.graph.add_edge(a, b, Arc::new(t));
        //Ok(())
    }

    pub fn remove_transition(&mut self, i: Tdx) -> Option<Arc<TransitionData<C::Transition>>> {
        self.graph.remove_edge(i)
    }

    // returns an iterator from idx -> root
    pub fn path_iter<'a>(&'a self, i: Idx) -> PathIter<'a, C> {
        PathIter::new(&self, i)
    }

    // path from root -> idx as a vec
    pub fn path(&self, i: Idx) -> Vec<Idx> {
        let mut path = self.path_iter(i).collect::<Vec<_>>();
        path.reverse();
        path
    }

    // This was just used for debugging.
    // pub fn path_str(&self, i: Idx) -> String {
    //     self.path(i)
    //         .iter()
    //         .map(|i| self.graph[*i].id.clone())
    //         .collect::<Vec<String>>()
    //         .join("::")
    // }

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

    pub fn validate() -> Result<(), String> {
        // initial is set for graph/states and valid/exists/is a child of
        // no cycles in hierarchy
        // all states ids are unique among siblings
        // edit data, rects are not negative/zero
        // root is set, exists, and no other non-parent states
        // no orphan states?
        // active exists/valid
        Ok(())
    }
}

impl<C: Context> Statechart<C> {
    pub fn new(graph: Graph<C>, context: C) -> Self {
        // TODO: verify graph? bindings? when do we apply initial?
        let active = graph.root;
        Self {
            graph,
            context,
            active,
        }
    }

    // TODO: how do we reset history state?
    pub fn reset(&mut self) {
        let root = &self.graph.graph[self.graph.root];
        match root.initial {
            Initial::None => panic!("no initial state set"),
            Initial::Initial(i) => {
                // recursively find the active state
                let initial = self.graph.get_initial(i);
                let mut ctx = self.context.clone();
                self.transition_to(&mut ctx, initial, None);
                self.context = ctx;
            }
            Initial::HistoryShallow(_) | Initial::HistoryDeep(_) => {
                // does this make sense at all?
                panic!("invalid initial value (history)");
            }
        }
    }

    // set initial states and return a channel?
    pub fn run(&mut self) {
        self.reset()
    }

    // Find a transition out of the active node based on the
    // event. Start with the active state and work through the parent
    // states.
    fn select(&mut self, ctx: &mut C, event: &C::Event) -> Option<(Idx, bool)> {
        // Collect the path so we can take use mutable refs below.
        let path = self.graph.path_iter(self.active).collect::<Vec<_>>();

        // Each potential guard can mutate the transition state.
        let g = &mut self.graph.graph;
        for i in path {
            let mut edges = g.neighbors(i).detach();
            while let Some((edge, next)) = edges.next(&g) {
                let t = Arc::make_mut(&mut g[edge]);
                if t.transition.guard(ctx, event) {
                    return Some((next, t.internal));
                }
            }
        }

        // No transitions found for event.
        None
    }

    pub fn transition(&mut self, event: C::Event) -> bool {
        self.context.dispatch(&event);
        // Clone so we can borrow self again.
        let mut ctx = self.context.clone();
        let next = self.select(&mut ctx, &event);
        let res = if let Some((next, internal)) = next {
            dbg!(internal);
            // With an internal transition we don't actually change
            // states, so we don't do anything else except copy the
            // context back. Should we verify the active state is
            // not a compound state?
            let active = self.active;
            if !internal {
                self.transition_to(&mut ctx, next, Some(&event));
                self.context.transition(
                    &self.graph.graph[active].state,
                    &self.graph.graph[next].state,
                );
            }
            true
        } else {
            false
        };
        self.context = ctx;
        res
    }

    // should we be passing new and old state to each action? use Cow?
    // this mutates history when exiting states
    pub fn transition_to(&mut self, ctx: &mut C, next: Idx, event: Option<&C::Event>) {
        // list of history updates, only applied after the transition
        // succeeds
        let mut h: Vec<(Idx, Idx)> = Vec::new();

        // We will traverse states up to a common ancestor (calling
        // [State::exit] on each) and back down to the next state
        // (calling [State::enter] on each). When does this return
        // None given there is always a root? FIX.
        let a = self.graph.common_ancestor(self.active, next);

        let not_a = |i: &Idx| a.map_or(true, |a| *i != a);

        // Path from the active state to the common ancestor. Don't
        // include the common ancestor since we are not entering or
        // exiting it. Collect so we can take mutable refs to states
        // below.
        let p1 = self
            .graph
            .path_iter(self.active)
            .take_while(not_a)
            .collect::<Vec<_>>();

        let initial = self.graph.get_initial(next);
        assert!(self.graph.is_child(next, initial));

        // Path from the common ancestor to the next node (including
        // the initial state).
        let mut p2 = self
            .graph
            .path_iter(initial)
            .take_while(not_a)
            .collect::<Vec<_>>();
        p2.reverse();

        let g = &mut self.graph.graph;

        let mut last = self.active;
        for i in p1 {
            let s = Arc::make_mut(&mut g[i]);
            s.state.exit(ctx, event);
            // track history when exiting a state
            match s.initial {
                Initial::HistoryShallow(_) => h.push((i, last)),
                Initial::HistoryDeep(_) => h.push((i, self.active)),
                _ => (),
            };
            // you can't have a history with no child
            // states, and we're not checking that
            // here - FIX?
            last = i;
            //Ok(())
        }
        //.collect::<Result<_>>()?;

        // let a = vec!["a", "b", "c"];
        // assert_eq!(a.into_iter().skip_while(|l| *l != "b").skip(1).next(), Some("c"));

        for i in p2 {
            let s = Arc::make_mut(&mut g[i]);
            s.state.enter(ctx, event);
            //Ok(())
        }
        //.collect::<Result<_>>()?;

        // apply history
        for (idx, prev) in h {
            let s = Arc::make_mut(&mut self.graph.graph[idx]);
            match s.initial {
                Initial::HistoryShallow(ref mut i) | Initial::HistoryDeep(ref mut i) => *i = prev,
                _ => (),
            }
        }

        // set active state
        self.active = next;

        //Ok(())
    }
}

pub struct PathIter<'a, C: Context> {
    graph: &'a Graph<C>,
    idx: Option<Idx>,
}

impl<'a, C: Context> PathIter<'a, C> {
    pub fn new(graph: &'a Graph<C>, idx: Idx) -> Self {
        Self {
            graph,
            idx: Some(idx),
        }
    }
}

impl<'a, C: Context> Iterator for PathIter<'a, C> {
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

impl<C> StateState<C>
where
    C: Context,
{
    // entry/exit defaults to path?
    // if we use path, moving the state requires renaming functions
    // if we only use id then all ids must be globally unique
    // TODO:
    // we can't set initial yet since children don't exist FIX?
    pub fn new(state: C::State, parent: Option<Idx>) -> Self {
        Self {
            state,
            parent,
            initial: Initial::None,
        }
    }

    pub fn with_parent(mut self, p: Option<Idx>) -> Self {
        self.parent = p;
        self
    }

    pub fn set_initial(&mut self, initial: Initial) {
        self.initial = initial;
    }

    // TODO move to initial
    // TODO: validate initial is child
    pub fn set_initial_idx(&mut self, i: Idx) {
        self.initial = match self.initial {
            Initial::None => Initial::Initial(i),
            Initial::Initial(_) => Initial::Initial(i),
            Initial::HistoryDeep(_) => Initial::HistoryDeep(i),
            Initial::HistoryShallow(_) => Initial::HistoryShallow(i),
        };
    }
}

#[cfg(test)]
mod tests {
    //use super::*;

    // make this work with any graph TODO:
    //struct TestContext;

    // #[test]
    // fn set_id() {
    //     let mut g = Graph::new("test");
    //     assert!(g.add_state("child1").is_ok());
    //     assert!(g.add_state("child1").is_err());
    // }
}
