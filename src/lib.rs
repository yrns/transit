//#![allow(unused_imports)]

use crate::undo::*;
pub use petgraph::Direction;
use petgraph::{
    graph::{EdgeIndex, NodeIndex},
    stable_graph::StableDiGraph,
    visit::{EdgeRef, IntoEdgeReferences, IntoNodeReferences},
};
use serde::{Deserialize, Serialize};
use std::{collections::HashSet, iter::Iterator};
use thiserror::Error;

mod undo;

// TODO: use statecharts as as states, "includes"
// the statechart needs to implement some interface that makes it behave as a state

pub type Idx = NodeIndex<u32>;
pub type Tdx = EdgeIndex<u32>;

/// Both states and transitions require [Clone] for undo/redo operations.
pub trait Context
where
    Self: Sized + Clone + Default,
{
    type Event: std::fmt::Debug;
    /// The root state uses the default.
    type State: State<Self> + Clone + Default;
    type Transition: Transition<Self> + Clone;
    //type Index;

    fn dispatch(&mut self, _event: &Self::Event) {}
    fn transition(&mut self, _source: &Self::State, _target: &Self::State) {}
}

// TODO: Serialize? Context needs to be separate from the graph so
// multiple contexts can share one graph. Same for state and
// transition contexts which are mutable currently.
pub struct Statechart<C: Context> {
    pub context: C,
    pub graph: Graph<C>,
    pub active: Idx,
}

#[derive(Serialize, Deserialize, Clone)]
pub struct Graph<C: Context> {
    #[serde(bound(
        serialize = "C::State: Serialize, C::Transition: Serialize",
        deserialize = "C::State: Deserialize<'de>, C::Transition: Deserialize<'de>"
    ))]
    pub(crate) graph: StableDiGraph<Node<C>, Edge<C::Transition>, u32>,
    pub root: Idx,
    // Make this an option?
    #[serde(skip_serializing, skip_deserializing)]
    undo: Undo<C>,
}

#[derive(Serialize, Deserialize, Copy, Clone, Debug, Default, PartialEq, Eq)]
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

    pub fn set_idx(self, i: Idx) -> Self {
        match self {
            Initial::None => Initial::Initial(i),
            Initial::Initial(_) => Initial::Initial(i),
            Initial::HistoryDeep(_) => Initial::HistoryDeep(i),
            Initial::HistoryShallow(_) => Initial::HistoryShallow(i),
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

impl From<Idx> for Initial {
    fn from(i: Idx) -> Self {
        Initial::Initial(i)
    }
}

pub trait State<C: Context> {
    fn enter(&mut self, ctx: &mut C, event: Option<&C::Event>);
    fn exit(&mut self, ctx: &mut C, event: Option<&C::Event>);
}

#[derive(Serialize, Deserialize, Debug, Default, Clone)]
pub struct Node<C>
where
    C: Context,
{
    pub initial: Initial,
    pub parent: Option<Idx>,
    pub state: C::State,
}

/// An internal transition is only valid for self-transitions. The
/// guard will be called, but enter/exit will not; and the active
/// state will not change. This is relevant for self-transitions on
/// parent states, which would otherwise transition from child ->
/// parent, if external.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Edge<T> {
    transition: T,
    internal: bool,
}

impl<T> From<T> for Edge<T> {
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
    fn internal(self) -> Edge<Self>;
}

impl<T> Internal for T {
    fn internal(self) -> Edge<T> {
        Edge {
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
        let root = graph.add_node(Node::default());
        Self {
            graph,
            root,
            undo: Undo::default(),
        }
    }

    pub fn root(&self) -> &C::State {
        &self.graph[self.root].state
    }

    pub fn add_state(&mut self, state: C::State, parent: Option<Idx>) -> Idx {
        let s = Node::new(state, parent.or(Some(self.root)));
        let i = self.graph.add_node(s.clone());
        self.add_undo(Op::AddState(i, s));
        i
    }

    // TODO: undo transaction, error?
    pub fn remove_state(&mut self, i: Idx, keep_transitions: bool, keep_children: bool) {
        // Cannot remove the root.
        assert!(i != self.root);

        // Kept things go to the parent state (if not root).
        let parent = self.graph.node_weight(i).and_then(|s| s.parent).unwrap();
        //.filter(|i| *i != self.root);

        // Clean up child states.
        for child in self.children(i).collect::<Vec<_>>() {
            if keep_children {
                self.set_parent(child, parent);
            } else {
                self.remove_state(child, keep_transitions, false)
            }
        }

        // Clean up initial/history.
        let mut path = self.path_walk(i);
        while let Some(p) = path.next(self) {
            if self.graph[p].initial.idx() == Some(i) {
                self.set_initial_idx(p, Some(parent));
            }
        }

        // petgraph by default removes all edges to and from this
        // node, which we want to avoid since we want the undo
        // history.

        // `neighbors` is directed/outgoing... FIX: Self-transitions will show up twice here, but
        // the comments here indicate that shouldn't happen:
        // ~/.local/share/cargo/registry/src/github.com-1ecc6299db9ec823/petgraph-0.6.2/src/graph_impl/mod.rs:2023
        let mut edges = self.graph.neighbors_undirected(i).detach();

        // Can't transition to/from root.
        if keep_transitions && parent != self.root {
            // TODO make sure this handles self-transitions correctly
            while let Some(edge) = edges.next_edge(&mut self.graph) {
                // We are looking up endpoints again when moving...
                match self.graph.edge_endpoints(edge) {
                    Some((a, b)) if a == b => self.move_transition(edge, parent, parent),
                    Some((a, b)) if a == i => self.move_transition(edge, parent, b),
                    Some((a, b)) if b == i => self.move_transition(edge, a, parent),
                    _ => (), // error
                }
            }
        } else {
            while let Some(edge) = edges.next_edge(&mut self.graph) {
                self.remove_transition(edge);
            }
        }

        if let Some(s) = self.graph.remove_node(i) {
            self.add_undo(Op::RemoveState(i, s))
        }
    }

    /// Returns a state reference for index.
    pub fn state(&self, i: Idx) -> Option<&C::State> {
        self.graph.node_weight(i).map(|s| &s.state)
    }

    /// Returns a transition reference for index.
    pub fn transition(&self, i: Tdx) -> Option<&C::Transition> {
        self.graph.edge_weight(i).map(|t| &t.transition)
    }

    /// Update state.
    pub fn update_state(&mut self, i: Idx, state: C::State) {
        let s1 = self.graph[i].clone();
        self.graph[i].state = state;
        let s2 = self.graph[i].clone();
        self.add_undo(Op::UpdateState(i, s1, s2));
    }

    pub fn contains_state(&self, i: Idx) -> bool {
        self.graph.contains_node(i)
    }

    pub fn contains_transition(&self, i: Tdx) -> bool {
        self.graph.edge_weight(i).is_some()
    }

    // We could eliminate searching every node by using a relational edge instead of an index?
    pub fn children(&self, p: Idx) -> impl Iterator<Item = Idx> + '_ {
        self.graph
            .node_indices()
            .filter(move |i| self.graph[*i].parent == Some(p))
    }

    // Once we filter we can't reverse?
    pub fn children_rev(&self, p: Idx) -> impl Iterator<Item = (Idx, &Node<C>)> {
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
    ) -> impl Iterator<Item = (Tdx, Idx, Idx, &<C as Context>::Transition, bool)> {
        self.graph.edges_directed(i, direction).map(|e| {
            let t = e.weight();
            (e.id(), e.source(), e.target(), &t.transition, t.internal)
        })
    }

    pub fn transitions_out(
        &self,
        i: Idx,
    ) -> impl Iterator<Item = (Tdx, Idx, Idx, &<C as Context>::Transition, bool)> {
        self.state_transitions(i, Direction::Outgoing)
    }

    pub fn transitions_in(
        &self,
        i: Idx,
    ) -> impl Iterator<Item = (Tdx, Idx, Idx, &<C as Context>::Transition, bool)> {
        self.state_transitions(i, Direction::Incoming)
    }

    pub fn transition_indices(&self) -> impl Iterator<Item = Tdx> + '_ {
        self.graph.edge_indices()
    }

    pub fn transitions(
        &self,
    ) -> impl Iterator<Item = (Tdx, Idx, Idx, &<C as Context>::Transition, bool)> {
        self.graph.edge_references().map(|e| {
            let t = e.weight();
            (e.id(), e.source(), e.target(), &t.transition, t.internal)
        })
    }

    pub fn endpoints(&self, i: Tdx) -> Option<(Idx, Idx)> {
        self.graph.edge_endpoints(i)
    }

    pub fn is_self_transition(&self, i: Tdx) -> bool {
        self.endpoints(i).map(|(a, b)| a == b).unwrap_or_default()
    }

    pub fn set_parent(&mut self, i: Idx, parent: Idx) {
        // Cannot change the parent of the root.
        assert!(i != self.root);

        // Make sure the new parent isn't a child.
        assert!(!self.in_path(i, parent));

        // TODO: Op::UpdateParent?
        let s = &mut self.graph[i];
        // Save previous parent.
        let p0 = s.parent;
        let s0 = s.clone();
        s.parent = Some(parent);
        let s = s.clone();
        self.add_undo(Op::UpdateState(i, s0, s));

        // Validate initial from prior path.
        if let Some(p) = p0 {
            let mut path = self.path_walk(p);
            while let Some(a) = path.next(self) {
                if let Some(i) = self.initial(a).idx() {
                    if !self.is_child(a, i) {
                        self.set_initial(a, Initial::None)
                    }
                }
            }
        }
    }

    /// Set initial for the root state (the default state for the
    /// graph). `set_graph_initial`?
    pub fn set_root_initial(&mut self, initial: impl Into<Initial>) {
        self.set_initial(self.root, initial);
    }

    pub fn set_initial(&mut self, i: Idx, initial: impl Into<Initial>) {
        let initial = initial.into();
        assert!(self.graph.contains_node(i));
        // Make sure the initial state (if any) is a child.
        if let Some(initial_idx) = initial.idx() {
            assert!(self.is_child(i, initial_idx));
        }

        // TODO: Op::UpdateInitial?
        let s1 = self.graph[i].clone();
        self.graph[i].initial = initial;
        let s2 = self.graph[i].clone();
        self.add_undo(Op::UpdateState(i, s1, s2));
    }

    // Sets the initial index, preserving the initial variant. If
    // `initial` is None, sets it to Initial::None.
    pub fn set_initial_idx(&mut self, i: Idx, initial: Option<Idx>) {
        self.set_initial(
            i,
            initial
                .map(|i0| self.graph[i].initial.clone().set_idx(i0))
                .unwrap_or_default(),
        )
    }

    // Return true if b is a child of a.
    pub fn is_child(&self, a: Idx, b: Idx) -> bool {
        a != b && self.in_path(a, b)
    }

    // Return true if a is in the path of b.
    pub fn in_path(&self, a: Idx, b: Idx) -> bool {
        self.path_iter(b).any(|i| i == a)
    }

    pub fn initial(&self, i: Idx) -> Initial {
        self.graph[i].initial
    }

    // initial_recur?
    pub fn get_initial(&self, i: Idx) -> Idx {
        match self.graph[i].initial {
            // no initial state, return i
            Initial::None => i,
            Initial::Initial(i) => self.get_initial(i),
            Initial::HistoryShallow(i) => self.get_initial(i),
            Initial::HistoryDeep(i) => self.get_initial(i),
        }
    }

    pub fn set_internal(&mut self, i: Tdx, internal: bool) {
        let i0 = self.graph[i].internal;
        if internal != i0 {
            self.add_undo(Op::SetInternal(i, internal));
            self.graph[i].internal = internal;
        }
    }

    // TODO: check root? validation?
    pub fn add_transition(&mut self, a: Idx, b: Idx, t: impl Into<Edge<C::Transition>>) -> Tdx {
        let t = t.into();
        assert!(!t.internal || a == b);
        // Can't transition to or from root.
        assert!(a != self.root && b != self.root);
        let i = self.graph.add_edge(a, b, t.clone());
        self.add_undo(Op::AddTransition(i, (a, b, t)));
        i
    }

    pub fn remove_transition(&mut self, i: Tdx) -> Option<Edge<C::Transition>> {
        self.graph
            .edge_endpoints(i)
            .zip(self.graph.remove_edge(i))
            .map(|((a, b), t)| {
                self.add_undo(Op::RemoveTransition(i, (a, b, t.clone())));
                t
            })
    }

    pub fn update_transition(&mut self, i: Tdx, t: C::Transition) {
        if let Some((a, b)) = self.graph.edge_endpoints(i) {
            let t0 = self.graph[i].clone();
            self.graph[i].transition = t;
            let t1 = self.graph[i].clone();
            self.add_undo(Op::UpdateTransition(i, (a, b, t0), (a, b, t1)));
        }
    }

    // There is no API for updating an existing edge so we add/remove
    // (see https://github.com/petgraph/petgraph/pull/103).
    pub(crate) fn move_transition_internal(&mut self, i: Tdx, a: Idx, b: Idx) {
        if let Some(t) = self.graph.remove_edge(i) {
            let i2 = self.graph.add_edge(a, b, t);
            assert_eq!(i, i2);
        }
    }

    pub fn move_transition(&mut self, i: Tdx, a: Idx, b: Idx) {
        // Can't transition to or from the root.
        assert!(a != self.root);
        assert!(b != self.root);

        if let Some((a0, b0)) = self.graph.edge_endpoints(i) {
            // Op::UpdateEndpoints?
            let t = self.graph[i].clone();
            self.add_undo(Op::UpdateTransition(i, (a0, b0, t.clone()), (a, b, t)));
            self.move_transition_internal(i, a, b)
        }
    }

    // returns an iterator from idx -> root
    pub fn path_iter(&self, idx: Idx) -> PathIter<'_, C> {
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
            if let Some(initial) = s.initial.idx() {
                if !self.graph.contains_node(i) {
                    return Err(format!(
                        "initial state ({:?}) missing for: {:?}",
                        initial, i
                    ));
                }
                // Initial state is a child.
                if !self.is_child(i, initial) {
                    return Err(format!(
                        "initial state ({:?}) is not a child of: {:?}",
                        initial, i
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
            if edge.source() == edge.target() && edge.weight().internal {
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

impl<C: Context> Default for Graph<C> {
    fn default() -> Self {
        Graph::new()
    }
}

#[derive(Error, Debug)]
pub enum ExportError {
    #[error("{0}")]
    Io(#[from] std::io::Error),
    #[error("{0}")]
    Ron(#[from] ron::Error),
}

impl<C> Graph<C>
where
    C: Context,
    C::State: Serialize,
    C::Transition: Serialize,
{
    pub fn export(&self) -> Result<String, ExportError> {
        Ok(ron::ser::to_string_pretty(&self, Default::default())?)
    }

    pub fn export_to_file(&self, path: &std::path::Path) -> Result<(), ExportError> {
        use std::io::Write;

        let mut file = std::fs::File::create(path)?;
        file.write_all(self.export()?.as_bytes())?;
        Ok(())
    }
}

#[derive(Error, Debug)]
pub enum ImportError {
    #[error("{0}")]
    Io(#[from] std::io::Error),
    #[error("{0}")]
    Ron(#[from] ron::error::SpannedError),
}

impl<C> Graph<C>
where
    C: Context,
    C::State: for<'de> Deserialize<'de>,
    C::Transition: for<'de> Deserialize<'de>,
{
    pub fn import_from_file(path: &std::path::Path) -> Result<Self, ImportError> {
        Ok(ron::de::from_reader(std::fs::File::open(path)?)?)
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
        let mut path = self.graph.path_walk(self.active);

        // Each potential guard can mutate the transition state.
        let g = &mut self.graph;
        while let Some(i) = path.next(g) {
            let mut edges = g.graph.neighbors(i).detach();
            while let Some((edge, next)) = edges.next(&mut g.graph) {
                let t = &mut g.graph[edge];
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
            let s = &mut g[i];
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
            let s = &mut g[i];
            s.state.enter(ctx, event);
            //Ok(())
        }
        //.collect::<Result<_>>()?;

        // apply history
        for (idx, prev) in h {
            let s = &mut self.graph.graph[idx];
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
    next: Option<Idx>,
}

impl<'a, C: Context> Iterator for PathIter<'a, C> {
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
    fn next<C: Context>(&mut self, graph: &Graph<C>) -> Option<Idx> {
        self.0.map(|n| {
            self.0 = graph.parent(n);
            n
        })
    }
}

impl<C> Node<C>
where
    C: Context,
{
    // we can't set initial yet since children don't exist FIX?
    pub fn new(state: C::State, parent: Option<Idx>) -> Self {
        Self {
            state,
            parent,
            initial: Initial::None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    impl State<TestContext> for String {
        fn enter(&mut self, _ctx: &mut TestContext, _event: Option<&()>) {}
        fn exit(&mut self, _ctx: &mut TestContext, _event: Option<&()>) {}
    }

    impl Transition<TestContext> for () {
        fn guard(&mut self, _ctx: &mut TestContext, _event: &()) -> bool {
            todo!()
        }
    }

    #[derive(Clone, Default)]
    pub(crate) struct TestContext;
    impl Context for TestContext {
        type Event = ();
        type State = String;
        type Transition = ();
    }

    pub(crate) fn test_graph() -> Graph<TestContext> {
        Graph::new()
    }

    #[test]
    fn is_child() {
        let mut g = test_graph();
        let a = g.add_state("a".into(), None);
        let b = g.add_state("b".into(), Some(a));

        assert!(g.is_child(a, b));
        assert!(!g.is_child(a, a));
        assert!(!g.is_child(b, a));
    }

    #[test]
    fn validate() {
        let mut g = test_graph();
        let a = g.add_state("a".into(), None);
        let b = g.add_state("b".into(), Some(a));
        g.graph[a].parent = Some(b);
        assert!(g.validate().is_err());
    }
}
