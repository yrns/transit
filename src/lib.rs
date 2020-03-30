#![allow(unused_imports)]

use anyhow::{anyhow, Context as _, Result};
#[cfg(feature = "editor")]
use druid::Data;
use kurbo::{Point, Rect, Size};
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
use std::path::Path;
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

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Graph {
    pub id: String,
    pub initial: Initial,
    //ids: HashMap, do we need to externally reference by id?
    graph: StableDiGraph<Arc<State>, Arc<Transition>, u32>,
    //pub edit_data: EditData,
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

#[cfg(feature = "editor")]
impl Data for Initial {
    fn same(&self, other: &Self) -> bool {
        self == other
    }
}

// rename StateEditData?
#[cfg(feature = "editor")]
#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct EditData {
    // relative to parent
    #[serde(with = "serde_rect")]
    pub rect: Rect,
}

#[cfg(feature = "editor")]
impl Data for EditData {
    fn same(&self, other: &Self) -> bool {
        self.rect.x0.same(&other.rect.x0)
            && self.rect.y0.same(&other.rect.y0)
            && self.rect.x1.same(&other.rect.x1)
            && self.rect.y1.same(&other.rect.y1)
    }
}

#[cfg(feature = "editor")]
impl EditData {
    pub fn new() -> Self {
        Self {
            rect: Rect::from_origin_size(Point::new(20., 20.), Size::new(120., 40.)),
        }
    }

    pub fn rect(&self) -> Rect {
        self.rect
    }

    pub fn set_rect(&mut self, rect: Rect) {
        self.rect = rect;
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

// newtype event key?
// enum for both strings and bound version?
// can't serialize bound version
// wait on optimizing until macro gen?
#[derive(Serialize, Deserialize, Debug, Clone)]
#[cfg_attr(feature = "editor", derive(Data))]
pub struct Transition {
    event: String,
    // this only has meaning for self-transitions
    internal: bool,
    guard: Option<String>,
    action: Option<String>,
    // edit data for curve parameters, etc.
    //edit_data: TransitionEditData?
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
    type Output = Arc<Transition>;

    fn index(&self, i: TransIdx) -> &Self::Output {
        &self.graph[i]
    }
}

impl IndexMut<TransIdx> for Graph {
    fn index_mut(&mut self, i: TransIdx) -> &mut Self::Output {
        &mut self.graph[i]
    }
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
        self.graph.add_node(Arc::new(s))
    }

    pub fn remove_state(&mut self, _i: Idx) -> State {
        // TODO: clean up transitions? move transitions to parent?
        todo!()
    }

    // iter mut? over State verses Idx?
    pub fn children(&self, p: Option<Idx>) -> impl Iterator<Item = Idx> + '_ {
        self.graph
            .node_indices()
            .filter(move |i| self.graph[*i].parent == p)
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
    pub fn set_initial(&mut self, i: Idx) -> Result<()> {
        assert!(self.graph.contains_node(i));
        self.initial = Initial::Initial(i);
        Ok(())
    }

    // return true if a is a child of b
    pub fn is_child(&self, a: Idx, b: Idx) -> bool {
        self.path_iter(a).find(|i| *i == b).is_some()
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

    pub fn add_transition(&mut self, a: Idx, b: Idx, t: Transition) -> Result<()> {
        self.graph.add_edge(a, b, Arc::new(t));
        Ok(())
    }

    pub fn remove_transition(&mut self, _i: EdgeIndex<u32>) -> Transition {
        todo!()
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
            assert!(self.graph.is_child(initial, next));
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

    pub fn set_initial(&mut self, initial: Initial) -> Result<()> {
        self.initial = initial;
        Ok(())
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
            internal: false,
            guard: guard.map(String::from),
            action: action.map(String::from),
        }
    }

    // TODO: only self-transitions can be internal but we can't check that here
    pub fn set_internal(mut self, internal: bool) -> Self {
        self.internal = internal;
        self
    }
}

#[cfg(feature = "editor")]
mod serde_rect {
    use kurbo::Rect;
    use serde::de::{self, Deserializer, SeqAccess, Visitor};
    use serde::ser::{SerializeSeq, Serializer};
    use std::fmt;

    pub fn serialize<S>(rect: &Rect, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut seq = serializer.serialize_seq(Some(4))?;
        seq.serialize_element(&rect.x0)?;
        seq.serialize_element(&rect.y0)?;
        seq.serialize_element(&rect.x1)?;
        seq.serialize_element(&rect.y1)?;
        seq.end()
    }

    struct RectVisitor;

    impl<'de> Visitor<'de> for RectVisitor {
        type Value = Rect;

        fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
            write!(formatter, "a sequence of 4 f64")
        }

        fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
        where
            A: SeqAccess<'de>,
        {
            let mut rect = Rect::ZERO;
            rect.x0 = seq.next_element()?.ok_or(de::Error::custom("missing x0"))?;
            rect.y0 = seq.next_element()?.ok_or(de::Error::custom("missing y0"))?;
            rect.x1 = seq.next_element()?.ok_or(de::Error::custom("missing x1"))?;
            rect.y1 = seq.next_element()?.ok_or(de::Error::custom("missing y1"))?;
            Ok(rect)
        }
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<Rect, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_seq(RectVisitor)
    }

    #[cfg(test)]
    mod tests {
        use ron::{de::from_str, ser::to_string};

        #[cfg(feature = "editor")]
        #[test]
        fn serde_rect() {
            use crate::EditData;
            use kurbo::Rect;
            let rect = Rect::new(1., 2., 3., 4.);
            let data = EditData { rect };
            let a = to_string(&data).unwrap();
            // Rect doesn't have Eq or PartialEq
            assert!(from_str::<EditData>(&a).is_ok());
        }
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
