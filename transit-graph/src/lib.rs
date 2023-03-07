//#![allow(unused_imports)]

pub mod edit;

pub use edit::*;
pub use petgraph::Direction;
use petgraph::{
    graph::{EdgeIndex, NodeIndex},
    stable_graph::{EdgeReference, StableDiGraph},
    visit::{EdgeRef, IntoEdgeReferences, IntoNodeReferences},
};
use std::{collections::HashSet, iter::Iterator};
use thiserror::Error;

// TODO: use statecharts as as states, "includes"
// the statechart needs to implement some interface that makes it behave as a state

pub type Idx = NodeIndex<u32>;
pub type Tdx = EdgeIndex<u32>;

/// Both states and transitions require [Clone] for undo/redo operations.
pub trait Context
where
    Self: Sized,
{
    type Event;
    /// The root state uses the default.
    type State: State<Self> + Clone + Default;
    type Transition: Transition<Self> + Clone;
    //type Index;

    fn dispatch(&mut self, _event: &Self::Event) {}
    fn transition(&mut self, _source: &Self::State, _target: &Self::State) {}
}

pub type ContextGraph<C> = Graph<<C as Context>::State, <C as Context>::Transition>;

// TODO: Serialize? Context needs to be separate from the graph so
// multiple contexts can share one graph. Same for state and
// transition contexts which are mutable currently.
pub struct Statechart<'a, C: Context> {
    pub context: C,
    pub graph: &'a ContextGraph<C>,
    pub active: Idx,
    pub locals: Locals<C::State, C::Transition>,
    pub history: History,
}

pub type History = nohash_hasher::IntMap<usize, Idx>;

pub struct Locals<S, T>(
    nohash_hasher::IntMap<usize, S>,
    nohash_hasher::IntMap<usize, T>,
);

impl<S, T> Default for Locals<S, T> {
    fn default() -> Self {
        Self(Default::default(), Default::default())
    }
}

impl<S, T> Locals<S, T> {
    pub fn clear(&mut self) {
        self.0.clear();
        self.1.clear();
    }
}

#[cfg_attr(feature = "serde", derive(serde::Deserialize, serde::Serialize))]
#[derive(Clone)]
pub struct Graph<S, T> {
    pub graph: StableDiGraph<Node<S>, Edge<T>, u32>,
    pub root: Idx,
}

#[cfg_attr(feature = "serde", derive(serde::Deserialize, serde::Serialize))]
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Initial {
    Initial,
    HistoryShallow,
    HistoryDeep,
}

impl Initial {
    pub fn step(self) -> Self {
        match self {
            Initial::Initial => Initial::HistoryShallow,
            Initial::HistoryShallow => Initial::HistoryDeep,
            Initial::HistoryDeep => Initial::Initial,
        }
    }
}

pub trait State<C: Context> {
    fn enter(&mut self, ctx: &mut C, event: Option<&C::Event>);
    fn exit(&mut self, ctx: &mut C, event: Option<&C::Event>);
}

#[cfg_attr(feature = "serde", derive(serde::Deserialize, serde::Serialize))]
#[derive(Debug, Default, Clone)]
pub struct Node<T> {
    pub parent: Option<Idx>,
    pub state: T,
}

/// An internal transition is only valid for self-transitions. The
/// guard will be called, but enter/exit will not; and the active
/// state will not change. This is relevant for self-transitions on
/// parent states, which would otherwise transition from child ->
/// parent, if external.
#[cfg_attr(feature = "serde", derive(serde::Deserialize, serde::Serialize))]
#[derive(Clone, Debug)]
pub enum Edge<T> {
    Transition(T),
    Internal(T),
    Initial(Initial),
}

impl<T> Edge<T> {
    pub fn set_internal(self, internal: bool) -> Self {
        match self {
            Edge::Transition(t) if internal => Edge::Internal(t),
            Edge::Internal(t) if !internal => Edge::Transition(t),
            _ => panic!("not a transition"),
        }
    }

    pub fn set_transition(&mut self, transition: T) {
        match self {
            Edge::Transition(t) | Edge::Internal(t) => *t = transition,
            _ => panic!("not a transition"),
        }
    }

    pub fn is_transition(&self) -> bool {
        matches!(self, Edge::Transition(_) | Edge::Internal(_))
    }

    pub fn transition(&self) -> Option<&T> {
        match self {
            Edge::Transition(t) | Edge::Internal(t) => Some(t),
            _ => None,
        }
    }

    pub fn transition_mut(&mut self) -> Option<&mut T> {
        match self {
            Edge::Transition(t) | Edge::Internal(t) => Some(t),
            _ => None,
        }
    }

    // Should this panic if !is_transition? Validation uses this as a filter.
    pub fn is_internal(&self) -> bool {
        matches!(self, Edge::Internal(_))
    }
}

/// A reference to a transition, index, and edge information.
pub type TransitionRef<'a, T> = (Tdx, Idx, Idx, &'a T, bool);

// Unused?
pub fn edge_transition_filter<C, E>(
    edge: EdgeReference<Edge<C::Transition>, u32>,
) -> Option<TransitionRef<'_, C::Transition>>
where
    C: Context,
{
    match edge.weight() {
        Edge::Transition(t) => Some((edge.id(), edge.source(), edge.target(), t, false)),
        Edge::Internal(t) => Some((edge.id(), edge.source(), edge.target(), t, true)),
        _ => None,
    }
}

impl<T> From<T> for Edge<T> {
    fn from(transition: T) -> Self {
        Self::Transition(transition)
    }
}

pub trait Transition<C: Context> {
    fn guard(&mut self, ctx: &mut C, event: &C::Event) -> bool;
    //fn action(&mut self); ???
}

impl<S, T> Graph<S, T> {
    pub fn new() -> Self
    where
        S: Default,
    {
        let mut graph = StableDiGraph::default();
        let root = graph.add_node(Node::default());
        Self { graph, root }
    }

    pub fn root(&self) -> &S {
        &self.graph[self.root].state
    }

    /// Returns a state reference for index.
    pub fn state(&self, i: Idx) -> Option<&S> {
        self.graph.node_weight(i).map(|s| &s.state)
    }

    /// Returns an iterator over all states.
    pub fn states(&self) -> impl Iterator<Item = (Idx, &S)> {
        self.graph.node_references().map(|(i, n)| (i, &n.state))
    }

    pub fn states_mut(&mut self) -> impl Iterator<Item = &mut S> {
        self.graph.node_weights_mut().map(|n| &mut n.state)
    }

    /// Returns a transition reference for index.
    pub fn transition(&self, i: Tdx) -> Option<&T> {
        self.graph.edge_weight(i).and_then(Edge::transition)
    }

    pub fn contains_state(&self, i: Idx) -> bool {
        self.graph.contains_node(i)
    }

    pub fn contains_transition(&self, i: Tdx) -> bool {
        self.transition(i).is_some()
    }

    // We could eliminate searching every node by using a relational edge instead of an index?
    pub fn children(&self, p: Idx) -> impl Iterator<Item = Idx> + '_ {
        self.graph
            .node_indices()
            .filter(move |i| self.graph[*i].parent == Some(p))
    }

    // Once we filter we can't reverse?
    pub fn children_rev(&self, p: Idx) -> impl Iterator<Item = (Idx, &Node<S>)> {
        self.graph
            .node_references()
            .rev()
            .filter(move |(_i, s)| s.parent == Some(p))
    }

    // If i is the root there should be no siblings.
    pub fn siblings(&self, i: Idx) -> impl Iterator<Item = Idx> + '_ {
        let p = self.graph[i].parent;
        self.graph
            .node_indices()
            .filter(move |j| *j != i && self.graph[*j].parent == p)
    }

    pub fn state_transitions(
        &self,
        i: Idx,
        direction: Direction,
    ) -> impl Iterator<Item = TransitionRef<T>> {
        self.graph
            .edges_directed(i, direction)
            .filter_map(|e| match e.weight() {
                Edge::Transition(t) => Some((e.id(), e.source(), e.target(), t, false)),
                Edge::Internal(t) => Some((e.id(), e.source(), e.target(), t, true)),
                _ => None,
            })
    }

    pub fn transitions_out(&self, i: Idx) -> impl Iterator<Item = (Tdx, Idx, Idx, &T, bool)> {
        self.state_transitions(i, Direction::Outgoing)
    }

    pub fn transitions_in(&self, i: Idx) -> impl Iterator<Item = (Tdx, Idx, Idx, &T, bool)> {
        self.state_transitions(i, Direction::Incoming)
    }

    pub fn transition_indices(&self) -> impl Iterator<Item = Tdx> + '_ {
        self.graph.edge_indices()
    }

    pub fn transitions(&self) -> impl Iterator<Item = TransitionRef<T>> {
        self.graph
            .edge_references()
            .filter_map(|e| match e.weight() {
                Edge::Transition(t) => Some((e.id(), e.source(), e.target(), t, false)),
                Edge::Internal(t) => Some((e.id(), e.source(), e.target(), t, true)),
                _ => None,
            })
    }

    pub fn transitions_mut(&mut self) -> impl Iterator<Item = &mut T> {
        self.graph
            .edge_weights_mut()
            .filter_map(Edge::transition_mut)
    }

    pub fn endpoints(&self, i: Tdx) -> Option<(Idx, Idx)> {
        self.graph.edge_endpoints(i)
    }

    pub fn is_self_transition(&self, i: Tdx) -> bool {
        self.endpoints(i).map(|(a, b)| a == b).unwrap_or_default()
    }

    // Return true if b is a child of a.
    pub fn is_child(&self, a: Idx, b: Idx) -> bool {
        a != b && self.in_path(a, b)
    }

    // Return true if a is in the path of b.
    pub fn in_path(&self, a: Idx, b: Idx) -> bool {
        self.path_iter(b).any(|i| i == a)
    }

    fn initial_edge(&self, i: Idx) -> Option<(Initial, EdgeReference<'_, Edge<T>>)> {
        self.graph
            .edges_directed(i, Direction::Outgoing)
            .find_map(|e| match e.weight() {
                Edge::Initial(initial) => Some((*initial, e)),
                _ => None,
            })
    }

    pub fn initial(&self, i: Idx) -> Option<(Initial, Idx)> {
        self.initial_edge(i)
            .map(|(initial, edge)| (initial, edge.target()))
    }

    // returns an iterator from idx -> root
    pub fn path_iter(&self, idx: Idx) -> PathIter<'_, S, T> {
        PathIter {
            graph: self,
            next: Some(idx),
        }
    }

    fn path_walk(&self, i: Idx) -> PathWalker {
        PathWalker(Some(i))
    }

    // path from root -> idx as a vec, track depth for iter len?
    // is this even used?
    pub fn path(&self, i: Idx) -> Vec<Idx> {
        let mut path = self.path_iter(i).collect::<Vec<_>>();
        path.reverse();
        path
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

    /// Check for problems with the graph. None of these conditions
    /// should be possible to achieve with the public API.
    pub fn validate(&self) -> Result<(), String> {
        // Root.
        let root = self.graph.node_weight(self.root);
        match root {
            Some(root) if root.parent.is_some() => {
                return Err(format!("root parent is {:?}", root.parent))
            }
            None => return Err(format!("missing root state: {:?}", self.root)),
            _ => (),
        }

        for (i, s) in self.graph.node_references() {
            // Parent is set for non-root states.
            if i != self.root && s.parent.is_none() {
                return Err(format!("non-root node has no parent! ({:?})", i));
            }

            // Initial.
            if let Some((_, j)) = self.initial(i) {
                if !self.graph.contains_node(i) {
                    return Err(format!("initial state ({:?}) missing for: {:?}", j, i));
                }
                // Initial state is a child.
                if !self.is_child(i, j) {
                    return Err(format!(
                        "initial state ({:?}) is not a child of: {:?}",
                        j, i
                    ));
                }
            }

            // Check for cycles.
            let mut path_set = HashSet::new();
            self.path_iter(i)
                .all(move |p| path_set.insert(p))
                .then_some(())
                .ok_or_else(|| format!("cycle in path for {:?}", i))?;
        }

        // Check all internal transitions are self-transitions.
        for edge in self.graph.edge_references() {
            if edge.weight().is_internal() && edge.source() != edge.target() {
                return Err(format!(
                    "internal transition is not a self-transition {:?}",
                    edge.id()
                ));
            }
        }

        // Active exists/valid? Needs to be in Statechart.
        Ok(())
    }
}

/// This creates a root state which requires S: [Default].
impl<S, T> Default for Graph<S, T>
where
    S: Default,
{
    fn default() -> Self {
        Graph::new()
    }
}

#[cfg(feature = "serde")]
#[derive(Error, Debug)]
pub enum ExportError {
    #[error("{0}")]
    Io(#[from] std::io::Error),
    #[error("{0}")]
    Ron(#[from] ron::Error),
}

#[cfg(feature = "serde")]
impl<S, T> Graph<S, T>
where
    S: serde::Serialize,
    T: serde::Serialize,
{
    pub fn export(&self) -> Result<String, ExportError> {
        Ok(ron::ser::to_string_pretty(&self, Default::default())?)
    }

    pub fn export_to_file(&self, path: impl AsRef<std::path::Path>) -> Result<(), ExportError> {
        use std::io::Write;

        let mut file = std::fs::File::create(path.as_ref())?;
        file.write_all(self.export()?.as_bytes())?;
        Ok(())
    }
}

#[cfg(feature = "serde")]
#[derive(Error, Debug)]
pub enum ImportError {
    #[error("{0}")]
    Io(#[from] std::io::Error),
    #[error("{0}")]
    Ron(#[from] ron::error::SpannedError),
}

#[cfg(feature = "serde")]
impl<S, T> Graph<S, T>
where
    S: for<'de> serde::Deserialize<'de>,
    T: for<'de> serde::Deserialize<'de>,
{
    pub fn import_from_file(path: &std::path::Path) -> Result<Self, ImportError> {
        Ok(ron::de::from_reader(std::fs::File::open(path)?)?)
    }
}

impl<'a, C: Context> Statechart<'a, C> {
    pub fn new(graph: &'a ContextGraph<C>, context: C) -> Self {
        assert!(graph.initial(graph.root).is_some(), "initial for root");

        let active = graph.root;
        let mut s = Self {
            graph,
            context,
            active,
            locals: Default::default(),
            history: Default::default(),
        };

        // Transition to the initial state.
        s.transition_to(s.initial(active), None);
        s
    }

    // TODO: does history make sense for the root initial?
    pub fn reset(&mut self, context: C) {
        self.context = context;
        self.locals.clear();
        self.history.clear();
        self.active = self.graph.root;
        self.transition_to(self.initial(self.active), None)
    }

    /// Find a transition out of the active node based on the event. Start with the active state and
    /// work through the parent states.
    fn select(&mut self, event: &C::Event) -> Option<(Idx, bool)> {
        let mut path = self.graph.path_walk(self.active);

        // Each potential guard can mutate the transition state and context.
        while let Some(i) = path.next(&self.graph) {
            // TODO since we are only mutating the context/locals we don't need the walker anymore?
            let mut edges = self.graph.graph.neighbors(i).detach();
            while let Some((edge, next)) = edges.next(&self.graph.graph) {
                let index = edge.index();
                let edge = &self.graph.graph[edge];
                if let Some(t) = edge.transition() {
                    // Get a mutable ref to the local transition (after copying it from the graph).
                    if self
                        .locals
                        .1
                        .entry(index)
                        .or_insert_with(|| t.clone())
                        .guard(&mut self.context, event)
                    {
                        return Some((next, edge.is_internal()));
                    }
                }
            }
        }

        // No transitions found for event.
        None
    }

    pub fn transition(&mut self, event: C::Event) -> bool {
        self.context.dispatch(&event);
        let res = if let Some((next, internal)) = self.select(&event) {
            // With an internal transition the guard is called but we don't actually change
            // states. Should we verify the active state is not a compound state?
            let active = self.active;
            if !internal {
                self.transition_to(next, Some(&event));
                self.context.transition(
                    &self.graph.graph[active].state,
                    &self.graph.graph[next].state,
                );
            }
            true
        } else {
            false
        };
        res
    }

    /// Recursively find an initial state using local history.
    pub fn initial(&self, i: Idx) -> Idx {
        match self.graph.initial(i) {
            // No initial state, return i.
            None => i,
            // We don't check the initial type.
            Some((_initial, j)) => self.initial(*self.history.get(&j.index()).unwrap_or(&j)),
        }
    }

    /// Traverse states up to a common ancestor (calling [State::exit] on each) and back down to the
    /// next state (calling [State::enter] on each).
    pub fn transition_to(&mut self, next: Idx, event: Option<&C::Event>) {
        // When does this return None given there is always a root?
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

        // Recursively find initial using local history.
        let initial = self.initial(next);

        assert!(self.graph.in_path(next, initial));

        // Path from the common ancestor to the next node (including the initial state).
        let mut p2 = self
            .graph
            .path_iter(initial)
            .take_while(not_a)
            .collect::<Vec<_>>();
        p2.reverse();

        let mut last = self.active;
        for i in p1 {
            self.locals
                .0
                .entry(i.index())
                .or_insert_with(|| self.graph.graph[i].state.clone())
                .exit(&mut self.context, event);

            // Update history when exiting a state.
            if let Some((initial, i)) = self.graph.initial(i) {
                _ = match initial {
                    Initial::HistoryShallow => self.history.insert(i.index(), last),
                    Initial::HistoryDeep => self.history.insert(i.index(), self.active),
                    _ => None,
                };
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
            self.locals
                .0
                .entry(i.index())
                .or_insert_with(|| self.graph.graph[i].state.clone())
                .enter(&mut self.context, event);
            //Ok(())
        }
        //.collect::<Result<_>>()?;

        // set active state
        self.active = next;

        //Ok(())
    }
}

pub struct PathIter<'a, S, T> {
    graph: &'a Graph<S, T>,
    next: Option<Idx>,
}

impl<'a, S, T> Iterator for PathIter<'a, S, T> {
    type Item = Idx;

    fn next(&mut self) -> Option<Self::Item> {
        self.next.map(|n| {
            self.next = self.graph.parent(n);
            n
        })
    }
}

struct PathWalker(Option<Idx>);

impl PathWalker {
    fn next<S, T>(&mut self, graph: &Graph<S, T>) -> Option<Idx> {
        self.0.map(|n| {
            self.0 = graph.parent(n);
            n
        })
    }
}

impl<T> Node<T> {
    pub fn new(state: T, parent: Option<Idx>) -> Self {
        Self { state, parent }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // impl State<TestContext> for String {
    //     fn enter(&mut self, _ctx: &mut TestContext, _event: Option<&()>) {}
    //     fn exit(&mut self, _ctx: &mut TestContext, _event: Option<&()>) {}
    // }

    // impl Transition<TestContext> for () {
    //     fn guard(&mut self, _ctx: &mut TestContext, _event: &()) -> bool {
    //         todo!()
    //     }
    // }

    // #[derive(Clone, Default)]
    // pub(crate) struct TestContext;
    // impl Context for TestContext {
    //     type Event = ();
    //     type State = String;
    //     type Transition = ();
    // }

    pub(crate) fn test_graph() -> Graph<String, ()> {
        Graph::new()
    }

    #[test]
    fn is_child() {
        let mut g = test_graph();
        let a = g.add_state("a".to_owned(), None);
        let b = g.add_state("b".to_owned(), Some(a));

        assert!(g.is_child(a, b));
        assert!(!g.is_child(a, a));
        assert!(!g.is_child(b, a));
    }

    #[test]
    fn validate() {
        let mut g = test_graph();
        let a = g.add_state("a".to_owned(), None);
        let b = g.add_state("b".to_owned(), Some(a));
        g.graph[a].parent = Some(b);
        assert!(g.validate().is_err());
    }
}
