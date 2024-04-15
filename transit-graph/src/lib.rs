//#![allow(unused_imports)]

pub mod edit;
mod graph;

pub use edit::*;
pub use graph::*;

// TODO: use statecharts as as states, "includes"
// the statechart needs to implement some interface that makes it behave as a state

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

    /// Called for every event received.
    fn dispatch(&mut self, _event: &Self::Event) {}

    /// Called when the active state changes.
    fn transition(&mut self, _source: &Self::State, _target: &Self::State) {}
}

pub type ContextGraph<C> = Graph<<C as Context>::State, <C as Context>::Transition>;

/// A running statechart. Contains a shared reference to the graph structure (so many running
/// statecharts can share a single graph); and local state, transition, and history data.
// TODO: Serialization with a shared graph? We'd need some kind of thread-local/static registry,
// which is maybe best done elsewhere.
//#[cfg_attr(feature = "serde", derive(serde::Deserialize, serde::Serialize))]
pub struct Statechart<'a, C: Context> {
    pub context: C,
    pub graph: &'a ContextGraph<C>,
    pub active: Idx,
    pub locals: Locals<C::State, C::Transition>,
    pub history: History,
}

pub type History = nohash_hasher::IntMap<usize, Idx>;

#[cfg_attr(feature = "serde", derive(serde::Deserialize, serde::Serialize))]
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

pub trait State<C: Context> {
    fn enter(&mut self, ctx: &mut C, event: Option<&C::Event>);
    fn exit(&mut self, ctx: &mut C, event: Option<&C::Event>);
}

pub trait Transition<C: Context> {
    fn guard(&mut self, ctx: &mut C, event: &C::Event) -> bool;
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
        s.transition_to(s.initial(active), None, None);
        s
    }

    // TODO: does history make sense for the root initial?
    pub fn reset(&mut self, context: C) {
        self.context = context;
        self.locals.clear();
        self.history.clear();
        self.active = self.graph.root;
        self.transition_to(self.initial(self.active), None, None)
    }

    /// Find a transition out of the active node based on the event. Start with the active state and
    /// work through the parent states.
    fn select(&mut self, event: &C::Event) -> Option<(Idx, bool, Option<Idx>)> {
        let mut path = self.graph.path_walk(self.active);

        // Each potential guard can mutate the transition state and context.
        while let Some(i) = path.next(self.graph) {
            // TODO since we are only mutating the context/locals we don't need the walker anymore?
            let mut edges = self.graph.graph.neighbors(i).detach();
            while let Some((i, next)) = edges.next(&self.graph.graph) {
                let edge = &self.graph.graph[i];

                let (t, ca) = match edge {
                    Edge::Transition(t, ca) => (t, *ca),
                    Edge::Internal(t) => (t, None),
                    Edge::Initial(_) => continue,
                };

                // Get a mutable ref to the local transition (after copying it from the graph).
                if self
                    .locals
                    .1
                    .entry(i.index())
                    .or_insert_with(|| t.clone())
                    .guard(&mut self.context, event)
                {
                    return Some((next, edge.is_internal(), ca));
                }
            }
        }

        // No transitions found for event.
        None
    }

    // event could be a ref?
    pub fn transition(&mut self, event: C::Event) -> bool {
        self.context.dispatch(&event);

        if let Some((next, internal, ca)) = self.select(&event) {
            // With an internal transition the guard is called but we don't actually change
            // states. Should we verify the active state is not a compound state?
            let active = self.active;
            if !internal {
                self.transition_to(next, Some(&event), ca);
                self.context.transition(
                    &self.graph.graph[active].state,
                    &self.graph.graph[next].state,
                );
            }
            true
        } else {
            false
        }
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
    pub fn transition_to(
        &mut self,
        next: Idx,
        event: Option<&C::Event>,
        common_ancestor: Option<Idx>,
    ) {
        // When does this return None given there is always a root? TODO: we already know the common
        // ancestor for initial edges
        //let a = self.graph.common_ancestor(self.active, next);
        let a = common_ancestor.unwrap_or(self.active);

        let not_a = |i: &Idx| i != &a;

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
