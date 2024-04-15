use std::{collections::HashSet, iter::Iterator};

pub use petgraph::Direction;
use petgraph::{
    graph::{EdgeIndex, NodeIndex},
    stable_graph::{EdgeReference, StableDiGraph},
    visit::{EdgeRef, IntoEdgeReferences, IntoNodeReferences},
};
use thiserror::Error;

use crate::Context;

pub type Idx = NodeIndex<u32>;
pub type Tdx = EdgeIndex<u32>;

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

#[cfg_attr(feature = "serde", derive(serde::Deserialize, serde::Serialize))]
#[derive(Debug, Default, Clone)]
pub struct Node<T> {
    // pub(crate)?
    pub parent: Option<Idx>,
    pub state: T,
}

impl<T> Node<T> {
    pub fn new(state: T, parent: Option<Idx>) -> Self {
        Self { state, parent }
    }
}

/// An internal transition is only valid for self-transitions. The
/// guard will be called, but enter/exit will not; and the active
/// state will not change. This is relevant for self-transitions on
/// parent states, which would otherwise transition from child ->
/// parent, if external.
#[cfg_attr(feature = "serde", derive(serde::Deserialize, serde::Serialize))]
#[derive(Clone, Debug)]
pub enum Edge<T> {
    /// Stores T and the common ancestor for endpoints. For internal transitions and intial edges,
    /// the common ancestor is always the source.
    // TODO: make this not an option by removing/fixing Edge::set_internal? and others
    // TODO: versioning? save ca?
    Transition(T, #[serde(skip)] Option<Idx>),
    Internal(T),
    Initial(Initial),
}

impl<T> Edge<T> {
    // Not sure why this is move.
    pub fn set_internal(self, internal: bool) -> Self {
        match self {
            // TODO this should not be pub without checking the endpoints
            Edge::Transition(t, _) if internal => Edge::Internal(t),
            Edge::Internal(t) if !internal => Edge::Transition(t, None),
            _ => panic!("not a transition"),
        }
    }

    pub fn set_transition(&mut self, transition: T) {
        match self {
            Edge::Transition(t, _) | Edge::Internal(t) => *t = transition,
            _ => panic!("not a transition"),
        }
    }

    pub fn is_transition(&self) -> bool {
        matches!(self, Edge::Transition(..) | Edge::Internal(_))
    }

    pub fn transition(&self) -> Option<&T> {
        match self {
            Edge::Transition(t, _) | Edge::Internal(t) => Some(t),
            _ => None,
        }
    }

    pub fn external(&self) -> Option<&T> {
        match self {
            Edge::Transition(t, _) => Some(t),
            _ => None,
        }
    }

    pub fn transition_mut(&mut self) -> Option<&mut T> {
        match self {
            Edge::Transition(t, _) | Edge::Internal(t) => Some(t),
            _ => None,
        }
    }

    // Should this panic if !is_transition? Validation uses this as a filter.
    pub fn is_internal(&self) -> bool {
        matches!(self, Edge::Internal(_))
    }

    pub fn is_external(&self) -> bool {
        matches!(self, Edge::Transition(..))
    }

    pub fn map<'a, F, V>(&'a self, i: Tdx, mut f: F) -> Edge<V>
    where
        F: FnMut(Tdx, &'a T) -> V,
    {
        match self {
            Edge::Transition(t, a) => Edge::Transition(f(i, t), a.clone()),
            Edge::Internal(t) => Edge::Internal(f(i, t)),
            Edge::Initial(initial) => Edge::Initial(*initial),
        }
    }
}

/// A reference to a transition, index, and edge information.
pub type TransitionRef<'a, T> = (Tdx, Idx, Idx, &'a T, bool);

// Indices, and mutable references to edge information.
//pub type TransitionMut<'a, T> = (Tdx, Idx, Idx, &'a mut T, &'a mut bool);

#[allow(unused)]
fn edge_transition_filter<C, E>(
    edge: EdgeReference<Edge<C::Transition>, u32>,
) -> Option<TransitionRef<'_, C::Transition>>
where
    C: Context,
{
    match edge.weight() {
        Edge::Transition(t, _) => Some((edge.id(), edge.source(), edge.target(), t, false)),
        Edge::Internal(t) => Some((edge.id(), edge.source(), edge.target(), t, true)),
        _ => None,
    }
}

impl<T> From<T> for Edge<T> {
    fn from(transition: T) -> Self {
        Self::Transition(transition, None)
    }
}

#[cfg_attr(feature = "serde", derive(serde::Deserialize, serde::Serialize))]
#[derive(Clone)]
pub struct Graph<S, T> {
    // TODO: pub(crate)
    pub graph: StableDiGraph<Node<S>, Edge<T>, u32>,
    pub root: Idx,
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

    /// This clones the graph structure, while mapping states and transitions to new
    /// types. Internally uses [StableDiGraph::map].
    pub fn map<'a, F, G, U, V>(&'a self, mut f: F, mut g: G) -> Graph<U, V>
    where
        F: FnMut(Idx, &'a S) -> U,
        G: FnMut(Tdx, &'a T) -> V,
    {
        Graph {
            graph: self.graph.map(
                |i, node| Node {
                    state: f(i, &node.state),
                    parent: node.parent,
                },
                |i, edge| edge.map(i, &mut g),
            ),
            root: self.root,
        }
    }

    pub fn root(&self) -> &S {
        &self.graph[self.root].state
    }

    /// Returns a state reference for index.
    pub fn state(&self, i: Idx) -> Option<&S> {
        self.graph.node_weight(i).map(|s| &s.state)
    }

    /// Returns a mutable state reference for index.
    pub fn state_mut(&mut self, i: Idx) -> Option<&mut S> {
        self.graph.node_weight_mut(i).map(|s| &mut s.state)
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

    /// Returns an external transition reference for index.
    pub fn external(&self, i: Tdx) -> Option<&T> {
        self.graph.edge_weight(i).and_then(Edge::external)
    }

    /// Returns a mutable transition reference for index.
    pub fn transition_mut(&mut self, i: Tdx) -> Option<&mut T> {
        self.graph.edge_weight_mut(i).and_then(Edge::transition_mut)
    }

    pub fn is_internal(&self, i: Tdx) -> bool {
        self.graph.edge_weight(i).is_some_and(Edge::is_internal)
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
                Edge::Transition(t, _) => Some((e.id(), e.source(), e.target(), t, false)),
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

    // FIX this ain't right
    pub fn transition_indices(&self) -> impl Iterator<Item = Tdx> + '_ {
        self.graph.edge_indices()
    }

    pub fn transitions(&self) -> impl Iterator<Item = TransitionRef<T>> {
        self.graph
            .edge_references()
            .filter_map(|e| match e.weight() {
                Edge::Transition(t, _) => Some((e.id(), e.source(), e.target(), t, false)),
                Edge::Internal(t) => Some((e.id(), e.source(), e.target(), t, true)),
                _ => None,
            })
    }

    // TODO: fix naming?
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

    /// Return true if b is a child of a.
    pub fn is_child(&self, a: Idx, b: Idx) -> bool {
        a != b && self.in_path(a, b)
    }

    /// Return true if a is in the path of b.
    pub fn in_path(&self, a: Idx, b: Idx) -> bool {
        self.path_iter(b).any(|i| i == a)
    }

    // TODO better name? provide this via other access methods?
    pub fn transition_common_ancestor(&self, tdx: Tdx) -> Option<Idx> {
        match self.graph.edge_weight(tdx)? {
            Edge::Transition(_, ca) => *ca,
            Edge::Internal(_) | Edge::Initial(_) => None,
        }
    }

    /// Returns true if the source and target of `transition` are in the path of `state`.
    // TODO: cache this? or at least use common ancestor stored in the edge
    pub fn enclosed(&self, state: Idx, transition: Tdx) -> bool {
        //self.transition(transition).is_some() &&
        self.endpoints(transition)
            .map(|(a, b)| self.in_path(state, a) && self.in_path(state, b))
            .unwrap_or(false)
    }

    /// Returns all edges that are enclosed by `state`.
    pub fn enclosed_edges(&self, state: Idx) -> impl Iterator<Item = Tdx> + '_ {
        self.graph
            .edge_indices()
            .filter(move |i| self.enclosed(state, *i))
    }

    pub(crate) fn initial_edge(&self, i: Idx) -> Option<(Initial, EdgeReference<'_, Edge<T>>)> {
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

    pub(crate) fn path_walk(&self, i: Idx) -> PathWalker {
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

    // TODO: do this with iterators: ancestors?
    pub fn common_ancestor(&self, s1: Idx, s2: Idx) -> Option<Idx> {
        // self-transition, we can just return the parent
        if s1 == s2 {
            return self.parent(s1);
        }

        let mut p1 = self.path(s1).into_iter();
        let mut p2 = self.path(s2).into_iter();

        let mut last = None;

        loop {
            match (p1.next(), p2.next()) {
                // We should always at least have the root.
                (None, None) => unreachable!(),
                (None, Some(_a)) | (Some(_a), None) => {
                    // one is a child of the other, return parent of last
                    return self.parent(last.unwrap());
                }
                (Some(a), Some(b)) => {
                    if a != b {
                        // return the last common ancestor
                        return last;
                    } else {
                        last = Some(a);
                    }
                }
            }
        }
    }

    /// Check for problems with the graph. None of these conditions
    /// should be possible to achieve with the public API.
    pub fn validate(&mut self) -> Result<(), String> {
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

        self.init_ca();

        // Active exists/valid? Needs to be in Statechart.
        Ok(())
    }

    // initialize common ancestors for each transition TODO: remove me?
    pub(crate) fn init_ca(&mut self) {
        let edges = self
            .graph
            .edge_references()
            .filter_map(|e| match e.weight() {
                Edge::Transition(_, _) => Some((
                    e.id(),
                    self.common_ancestor(e.source(), e.target()).unwrap(),
                )),
                Edge::Internal(_) | Edge::Initial(_) => None,
            })
            .collect::<Vec<_>>();

        for (i, common_ancestor) in edges {
            match &mut self.graph[i] {
                Edge::Transition(_, ca) => *ca = Some(common_ancestor),
                _ => (),
            }
        }
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

pub(crate) struct PathWalker(Option<Idx>);

impl PathWalker {
    pub(crate) fn next<S, T>(&mut self, graph: &Graph<S, T>) -> Option<Idx> {
        self.0.map(|n| {
            self.0 = graph.parent(n);
            n
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_graph() -> Graph<&'static str, ()> {
        Graph::new()
    }

    #[test]
    fn is_child() {
        let mut g = test_graph();
        let a = g.add_state("a", None);
        let b = g.add_state("b", Some(a));

        assert!(g.is_child(a, b));
        assert!(!g.is_child(a, a));
        assert!(!g.is_child(b, a));
    }

    #[test]
    fn validate() {
        let mut g = test_graph();
        let a = g.add_state("a", None);
        let b = g.add_state("b", Some(a));
        g.graph[a].parent = Some(b);
        assert!(g.validate().is_err());
    }

    #[test]
    fn common_ancestor() {
        let mut g = Graph::new();
        let a = g.add_state("a", None);
        let b = g.add_state("b", None);
        let c = g.add_state("c", Some(b));
        let d = g.add_state("d", Some(c));
        let _self_to_a = g.add_transition(a, a, ());
        assert_eq!(
            g.common_ancestor(a, b).unwrap(),
            g.root,
            "common ancestor of a and b is root"
        );
        assert_eq!(
            g.common_ancestor(c, d).unwrap(),
            b,
            "common ancestor of c and d is b"
        );
    }
}
