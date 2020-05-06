#![allow(unused_imports)]

use anyhow::{anyhow, Context as _, Result};
#[cfg(feature = "editor")]
use druid::Data;
use petgraph::{
    graph::{EdgeIndex, IndexType, NodeIndex},
    stable_graph::StableDiGraph,
};
use ron::de::from_reader;
use ron::ser::{to_string_pretty, PrettyConfig};
use serde::{Deserialize, Serialize};
use std::any::Any;
use std::boxed::Box;
use std::collections::btree_map::{BTreeMap, Entry};
use std::collections::HashMap;
use std::fmt::Debug;
use std::fs::File;
use std::io::prelude::*;
use std::iter::Iterator;
use std::mem::{discriminant, Discriminant};
//use std::ops::Deref;
use std::ops::{Index, IndexMut};
use std::path::{Path, PathBuf};
use std::sync::Arc;

// TODO: use statecharts as as states, "includes"
// the statechart needs to implement some interface that makes it behave as a state

pub type Idx = NodeIndex<u32>;
pub type TransIdx = EdgeIndex<u32>;

pub type ActionFn<C, E> = fn(&mut C, Option<&E>) -> Result<()>;

pub trait Context: Sized + Clone {
    type Event: Debug;

    fn event(event: &str) -> Discriminant<Self::Event>;
    fn action(action: &str) -> &ActionFn<Self, Self::Event>;
}

// separate state, actions (code), and events from the structure
pub struct Statechart<T> {
    pub context: T,
    pub graph: Graph,
    pub active: Option<Idx>,
}

#[cfg(feature = "editor")]
type Point = (f64, f64);

#[cfg(feature = "editor")]
type Size = (f64, f64);

#[cfg(feature = "editor")]
type Rect = (Point, Size);

#[cfg(feature = "editor")]
#[derive(Serialize, Deserialize, Debug, Clone, Default)]
#[serde(transparent)]
pub struct PathData(pub Option<PathBuf>);

#[cfg(feature = "editor")]
impl PathData {
    pub fn new(p: Option<PathBuf>) -> Self {
        Self(p)
    }
}

// submit PR for Path/PathBuf?
#[cfg(feature = "editor")]
impl Data for PathData {
    fn same(&self, other: &Self) -> bool {
        match (self.0.as_ref(), other.0.as_ref()) {
            (Some(a), Some(b)) => a == b,
            (None, None) => true,
            _ => false,
        }
    }
}

#[cfg(feature = "editor")]
#[derive(Serialize, Deserialize, Debug, Clone, Data)]
#[serde(default)]
pub struct GraphEditData {
    #[serde(default)]
    pub initial: Point,
    // Location of the source file associated with this graph. This
    // needs to be here so its saved. But maybe that precludes reusing
    // a graph with multiple contexts?
    #[serde(default)]
    pub src: PathData,
}

#[cfg(feature = "editor")]
impl GraphEditData {
    pub fn new() -> Self {
        Self::default()
    }
}

impl Default for GraphEditData {
    fn default() -> Self {
        Self {
            initial: (20., 20.),
            src: PathData(None),
        }
    }
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Graph {
    pub id: String,
    pub initial: Initial,
    //ids: HashMap, do we need to externally reference by id?
    graph: StableDiGraph<Arc<State>, Arc<Transition>, u32>,
    #[cfg(feature = "editor")]
    #[serde(default)]
    pub edit_data: GraphEditData,
}

#[cfg(feature = "editor")]
impl Data for Graph {
    fn same(&self, other: &Self) -> bool {
        if self.id == other.id
            && self.initial == other.initial
            && self.graph.node_count() == other.graph.node_count()
            && self.graph.edge_count() == other.graph.edge_count()
        {
            // compare every state, there is no iterator for this
            for i in self.graph.node_indices() {
                if !self.graph[i].same(&other.graph[i]) {
                    return false;
                }
            }

            // compare every transition
            for i in self.graph.edge_indices() {
                if !self.graph[i].same(&other.graph[i]) {
                    return false;
                }
            }
            return true;
        }
        return false;
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub enum Initial {
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

#[cfg(feature = "editor")]
impl Data for Initial {
    fn same(&self, other: &Self) -> bool {
        self == other
    }
}

// rename StateEditData?
#[cfg(feature = "editor")]
#[derive(Serialize, Deserialize, Debug, Clone, Data)]
#[serde(default)]
pub struct EditData {
    // relative to parent
    #[serde(default)]
    pub rect: Rect,
    #[serde(default)]
    pub initial: Point,
}

#[cfg(feature = "editor")]
impl EditData {
    pub fn new() -> Self {
        Self::default()
    }
}

#[cfg(feature = "editor")]
impl Default for EditData {
    fn default() -> Self {
        Self {
            rect: ((40., 60.), (120., 40.)),
            // set this only after initial is set, so we can match the
            // position to the state which is the initial?
            initial: (20., 40.),
        }
    }
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct State {
    pub id: String,
    pub initial: Initial,
    pub parent: Option<Idx>,
    pub entry: Option<String>,
    pub exit: Option<String>,
    #[cfg(feature = "editor")]
    #[serde(default)]
    pub edit_data: EditData,
}

#[cfg(feature = "editor")]
impl Data for State {
    fn same(&self, other: &Self) -> bool {
        self.id == other.id
            && self.initial == other.initial
            && self.parent == other.parent
            && self.entry == other.entry
            && self.exit == other.exit
            && self.edit_data.same(&other.edit_data)
    }
}

// the default won't be useful, we need to apply a better one in
// layout/validation TODO:
#[cfg(feature = "editor")]
#[derive(Serialize, Deserialize, Debug, Clone, Data, Default)]
pub struct TransitionEditData(pub Point);

// newtype event key?
// enum for both strings and bound version?
// can't serialize bound version
// wait on optimizing until macro gen?
#[derive(Serialize, Deserialize, Debug, Clone)]
#[cfg_attr(feature = "editor", derive(Data))]
pub struct Transition {
    pub event: String,
    // this only has meaning for self-transitions
    pub internal: bool,
    pub guard: Option<String>,
    pub action: Option<String>,
    #[cfg(feature = "editor")]
    #[serde(default)]
    pub edit_data: TransitionEditData,
}

impl Default for Transition {
    fn default() -> Self {
        Self {
            event: "event".to_string(),
            internal: false,
            guard: None,
            action: None,
            #[cfg(feature = "editor")]
            edit_data: TransitionEditData((0., 0.)),
        }
    }
}

// implementing index traits allows us to circumvent being unable to
// impl Data on StableGraph
impl Index<Idx> for Graph {
    type Output = Arc<State>;

    fn index(&self, i: Idx) -> &Self::Output {
        &self.graph[i]
    }
}

impl IndexMut<Idx> for Graph {
    fn index_mut(&mut self, i: Idx) -> &mut Self::Output {
        &mut self.graph[i]
    }
}

impl Index<TransIdx> for Graph {
    type Output = Transition;

    fn index(&self, i: TransIdx) -> &Self::Output {
        &self.graph[i]
    }
}

impl IndexMut<TransIdx> for Graph {
    fn index_mut(&mut self, i: TransIdx) -> &mut Self::Output {
        Arc::make_mut(&mut self.graph[i])
    }
}

impl Graph {
    pub fn new(id: &str) -> Self {
        Self {
            id: id.to_string(),
            initial: Initial::None,
            graph: StableDiGraph::default(),
            #[cfg(feature = "editor")]
            edit_data: GraphEditData::new(),
        }
    }

    pub fn export(&self) -> Result<String> {
        Ok(to_string_pretty(&self, PrettyConfig::default())?)
    }

    pub fn export_to_file(&self, path: &Path) -> Result<()> {
        let mut file = File::create(path)?;
        file.write_all(self.export()?.as_bytes())?;
        Ok(())
    }

    pub fn import_from_file(path: &Path) -> Result<Self> {
        Ok(from_reader(File::open(path)?)?)
    }

    // impl Index/Mut instead? what about transitions
    pub fn get(&self, i: Idx) -> &State {
        &self.graph[i]
    }

    pub fn get_mut(&mut self, i: Idx) -> &mut State {
        Arc::make_mut(&mut self.graph[i])
    }

    pub fn add_state<T: Into<State>>(&mut self, s: T) -> Idx {
        let s = s.into();
        let p = s.parent;
        let a = self.graph.add_node(Arc::new(s));
        // the first added node becomes initial for the root
        // TODO: validation?
        if p.is_none() && self.initial == Initial::None {
            self.initial = Initial::Initial(a);
        }
        a
    }

    pub fn remove_state(&mut self, i: Idx) -> Option<Arc<State>> {
        // TODO: clean up transitions? move transitions to parent?
        self.graph.remove_node(i)
    }

    pub fn contains_state(&self, i: Idx) -> bool {
        self.graph.contains_node(i)
    }

    pub fn contains_transition(&self, i: TransIdx) -> bool {
        self.graph.edge_weight(i).is_some()
    }

    // iter mut? over State verses Idx?
    pub fn children(&self, p: Option<Idx>) -> impl Iterator<Item = Idx> + '_ {
        self.graph
            .node_indices()
            .filter(move |i| self.graph[*i].parent == p)
    }

    pub fn transitions(&self) -> impl Iterator<Item = TransIdx> + '_ {
        self.graph.edge_indices()
    }

    pub fn endpoints(&self, i: TransIdx) -> Option<(Idx, Idx)> {
        self.graph.edge_endpoints(i)
    }

    pub fn is_self_transition(&self, i: TransIdx) -> bool {
        if let Some((a, b)) = self.endpoints(i) {
            a == b
        } else {
            false
        }
    }

    pub fn set_id(&mut self, i: Option<Idx>, id: &str) {
        if let Some(i) = i {
            Arc::make_mut(&mut self.graph[i]).id = id.to_string();
        } else {
            // set graph id
            self.id = id.to_string();
        }
    }

    pub fn is_unique_id(&self, p: Option<Idx>, id: &str) -> bool {
        self.children(p)
            .map(|i| &self.graph[i].id)
            .all(|id2| id2 != id)
    }

    // move state, check id for uniqueness among new siblings?
    pub fn set_parent(&mut self, i: Idx, p: Option<Idx>) {
        Arc::make_mut(&mut self.graph[i]).parent = p;
    }

    // the only reason we'd want history at the root of a statechart
    // is if it's used inside another statechart - TODO:
    pub fn set_initial(&mut self, i: Idx) {
        assert!(self.graph.contains_node(i));
        self.initial = Initial::Initial(i);
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

    pub fn initial_idx(&self, a: Option<Idx>) -> Option<Idx> {
        if let Some(a) = a {
            self[a].initial.idx()
        } else {
            self.initial.idx()
        }
    }

    pub fn add_transition(&mut self, a: Idx, b: Idx, t: Transition) -> Result<()> {
        self.graph.add_edge(a, b, Arc::new(t));
        Ok(())
    }

    pub fn remove_transition(&mut self, i: TransIdx) -> Option<Arc<Transition>> {
        self.graph.remove_edge(i)
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

    /// Find the absolute position of a state in the graph.
    #[cfg(feature = "editor")]
    pub fn abs_pos(&self, a: Idx) -> Point {
        self.path_iter(a)
            .map(|i| self[i].edit_data.rect.0)
            .fold((0., 0.), add)
    }

    /// Find the absolute rect of a state in the graph.
    #[cfg(feature = "editor")]
    pub fn abs_rect(&self, a: Idx) -> Rect {
        (self.abs_pos(a), self[a].edit_data.rect.1)
    }

    /// Find the position of one state relative to another using
    /// EditData. Used to draw transitions and such between any two
    /// states in the graph.
    #[cfg(feature = "editor")]
    pub fn rel_pos(&self, a: Option<Idx>, b: Idx) -> Point {
        let b = self.abs_pos(b);
        a.map(|a| sub(b, self.abs_pos(a))).unwrap_or(b)
    }
}

fn add(a: Point, b: Point) -> Point {
    (a.0 + b.0, a.1 + b.1)
}

fn sub(a: Point, b: Point) -> Point {
    (a.0 - b.0, a.1 - b.1)
}

impl<C: Context> Statechart<C> {
    pub fn new(graph: Graph, context: C) -> Result<Self> {
        // TODO: verify graph? bindings?
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

    // TODO: how do we reset history state?
    pub fn reset(&mut self) -> Result<()> {
        match self.graph.initial {
            Initial::None => Err(anyhow!("no initial state set")),
            Initial::Initial(i) => {
                // recursively find the active state
                let initial = self.graph.get_initial(i);
                let mut ctx = self.context.clone();
                self.transition_to(&mut ctx, initial, None)?;
                self.context = ctx;
                Ok(())
            }
            Initial::HistoryShallow(_) | Initial::HistoryDeep(_) => {
                // does this make sense at all?
                Err(anyhow!("invalid initial value (history)"))
            }
        }
    }

    // set initial states and return a channel?
    pub fn run(&mut self) -> Result<()> {
        self.reset()
    }

    // select a transition based on the active state and an event,
    // returns next node and whether or not the transition is internal
    fn select(&self, ctx: &mut C, event: &C::Event) -> Result<(Idx, bool)> {
        let d = discriminant(event);

        if let Some(active) = self.active {
            // we start with this index and work through its parents
            let mut p = active;

            loop {
                let mut edges = self.graph.graph.neighbors(p).detach();

                while let Some((edge, node)) = edges.next(&self.graph.graph) {
                    // need a better display for transitions
                    // println!(
                    //     "{} -> {:?} -> {}",
                    //     self.graph.path_str(p),
                    //     event,
                    //     self.graph.path_str(node)
                    // );

                    // guard event mismatch error should be upgraded
                    // or removed entirely - it's boilerplate for
                    // something that shouldn't ever happen TODO:

                    let t = &self.graph.graph[edge];
                    if C::event(&t.event) == d {
                        // FIX: "guard false" is not an error but
                        // everything else is, and we're ignoring it
                        let guard_pass = t
                            .guard
                            .as_ref()
                            .map_or(true, |g| C::action(&g)(ctx, Some(event)).is_ok());

                        if guard_pass {
                            if let Some(ref action) = t.action {
                                C::action(action)(ctx, Some(event)).with_context(|| {
                                    format!(
                                        "action failed on {:?} with event: {:?}",
                                        // better id for transitions?
                                        edge,
                                        event
                                    )
                                })?;
                            }

                            // need a better place for this check
                            if t.internal && p != node {
                                return Err(anyhow!(
                                    "internal transition is not a self-transition: {:?} -> {:?}",
                                    p,
                                    node,
                                ));
                            }

                            return Ok((node, t.internal));
                        }
                    }
                }

                // check parents' transitions
                if let Some(i) = self.graph.graph[p].parent {
                    p = i;
                } else {
                    return Err(anyhow!("no valid transitions"));
                }
            }
        } else {
            Err(anyhow!("no active state"))
        }
    }

    pub fn transition(&mut self, event: C::Event) -> Result<()> {
        let mut ctx = self.context.clone();
        let (next, internal) = self.select(&mut ctx, &event)?;

        // With an internal transition we don't actually change
        // states, so we don't do anything else except copy the
        // context back. Should we verify the active state is
        // not a compound state?
        if !internal {
            self.transition_to(&mut ctx, next, Some(event))?;
        }
        self.context = ctx;
        Ok(())
    }

    // should we be passing new and old state to each action? use Cow?
    // this mutates history when exiting states
    pub fn transition_to(&mut self, ctx: &mut C, next: Idx, event: Option<C::Event>) -> Result<()> {
        // list of history updates, only applied after the transition
        // succeeds
        let mut h: Vec<(Idx, Idx)> = Vec::new();

        // common ancestor state
        let mut a = None;

        // this can change based on the initial state for next
        let mut next = next;

        if let Some(active) = self.active {
            // traverse states up to a common ancestor (calling
            // exit on each) and back down to the next state
            // (calling entry)
            a = self.graph.common_ancestor(active, next);

            // path from active -> a (minus a)
            let p1 = self
                .graph
                .path_iter(active)
                .take_while(|i| a.map_or(true, |a| *i != a));

            let mut last = active;
            p1.map(|i| {
                let s = &self.graph.graph[i];
                if let Some(ref exit) = s.exit {
                    C::action(exit)(ctx, event.as_ref()).with_context(|| {
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
                    Initial::HistoryShallow(_) => h.push((i, last)),
                    Initial::HistoryDeep(_) => h.push((i, active)),
                    _ => (),
                };
                // you can't have a history with no child
                // states, and we're not checking that
                // here - FIX?
                last = i;

                Ok(())
            })
            .collect::<Result<_>>()?;

            // let a = vec!["a", "b", "c"];
            // assert_eq!(a.into_iter().skip_while(|l| *l != "b").skip(1).next(), Some("c"));

            // recurse initial states, if the active state is None
            // we've already done this, so we can skip it
            let initial = self.graph.get_initial(next);
            assert!(self.graph.is_child(next, initial));
            next = initial;
        }

        // path from a -> next (minus a)
        let p2 = self
            .graph
            .path_iter(next)
            .take_while(|i| a.map_or(true, |a| *i != a))
            // collect so we can reverse
            .collect::<Vec<_>>()
            .into_iter()
            .rev();

        p2.map(|i| {
            let s = &self.graph.graph[i];
            if let Some(ref entry) = s.entry {
                C::action(entry)(ctx, event.as_ref()).with_context(|| {
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
        .collect::<Result<_>>()?;

        // apply history
        for (idx, prev) in h {
            let s = Arc::make_mut(&mut self.graph.graph[idx]);
            match s.initial {
                Initial::HistoryShallow(ref mut i) | Initial::HistoryDeep(ref mut i) => *i = prev,
                _ => (),
            }
        }

        // set active state
        self.active = Some(next);

        Ok(())
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
    pub fn new<S: Into<String>>(
        id: S,
        parent: Option<Idx>,
        entry: Option<&str>,
        exit: Option<&str>,
    ) -> Self {
        Self {
            id: id.into(),
            initial: Initial::None,
            parent,
            entry: entry.map(String::from),
            exit: exit.map(String::from),
            #[cfg(feature = "editor")]
            edit_data: EditData::new(),
        }
    }

    pub fn set_initial(&mut self, initial: Initial) {
        self.initial = initial;
    }

    // TODO: validate initial is child
    pub fn set_initial_idx(&mut self, i: Idx) {
        self.initial = match self.initial {
            Initial::None => Initial::Initial(i),
            Initial::Initial(_) => Initial::Initial(i),
            Initial::HistoryDeep(_) => Initial::HistoryDeep(i),
            Initial::HistoryShallow(_) => Initial::HistoryShallow(i),
        };
    }

    // use set_id in Graph to change this
    pub fn id(&self) -> &str {
        &self.id
    }
}

impl From<&str> for State {
    fn from(id: &str) -> Self {
        State::new(id, None, None, None)
    }
}

impl Transition {
    pub fn new(event: &str, guard: Option<&str>, action: Option<&str>) -> Self {
        Self {
            event: event.to_string(),
            guard: guard.map(String::from),
            action: action.map(String::from),
            ..Default::default()
        }
    }

    #[cfg(feature = "editor")]
    pub fn at(mut self, p: Point) -> Self {
        self.edit_data = TransitionEditData(p);
        self
    }

    // TODO: only self-transitions can be internal but we can't check that here
    pub fn set_internal(mut self, internal: bool) -> Self {
        self.internal = internal;
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // make this work with any graph TODO:
    //struct TestContext;

    // #[test]
    // fn set_id() {
    //     let mut g = Graph::new("test");
    //     assert!(g.add_state("child1").is_ok());
    //     assert!(g.add_state("child1").is_err());
    // }
}
