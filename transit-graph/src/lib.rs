//#![allow(unused_imports)]

mod graph;

pub use graph::*;
//pub use source::Source;

pub use nohash_hasher::IntMap;

// TODO: use statecharts as as states, "includes"
// the statechart needs to implement some interface that makes it behave as a state

/// Both states and transitions require [Clone] for undo/redo operations.
// Actions?
pub trait Context<T, E>
where
    Self: Sized,
{
    //type Event;
    //type GraphRef: Clone;
    type State;
    type Transition;
    //type Context; // ??? Self?

    //type Error; ???

    /// Called for every event received.
    // on_event?
    fn dispatch(&mut self, _event: &E) {}

    /// Called when the active state changes.
    // on_active?
    fn transition(&mut self, _source: &Self::State, _target: &Self::State) {}

    //fn graph(&mut self) -> &Graph<Self::State, Self::Transition>;

    fn enter(&mut self, inner: &mut T, event: Option<&E>, state: &Self::State, index: Idx);

    fn exit(&mut self, inner: &mut T, event: Option<&E>, state: &Self::State, index: Idx);

    /// Pre-filters events before [Self::guard] is checked. This is useful if you want to avoid
    /// unnecessary calls to the guard or want a global filter based on, say, the event variant (if
    /// your event type is an enum).
    fn filter(&mut self, _event: &E, _transition: &Self::Transition, _index: Tdx) -> bool {
        true
    }

    fn guard(
        &mut self,
        inner: &mut T,
        event: &E,
        // The graph is immutable.
        transition: &Self::Transition,
        index: Tdx,
    ) -> bool;

    // TODO merge graph/edit
    //fn resolve(&mut self, edit: &Edit<Self>) -> Graph<Self::State, Self::Transition>;
}

//pub type ContextGraph<C> = Graph<<C as Context>::State, <C as Context>::Transition>;

/// Current history for each state index.
pub type History = IntMap<usize, Idx>;

/// A running statechart.
#[derive(Clone, Debug)]
#[cfg_attr(feature = "serde", derive(serde::Deserialize, serde::Serialize))]
pub struct Statechart<T> {
    pub inner: T,
    pub active: Idx,
    pub history: History,
    //_ctx: PhantomData<(C, E)>,
}

impl<T> Statechart<T> {
    pub fn inner(&self) -> &T {
        &self.inner
    }

    pub fn inner_mut(&mut self) -> &mut T {
        &mut self.inner
    }
}

// pub struct Process<T, C, G, E> {
//     ctx: C,
//     graph: G,
//     _phantom: PhantomData<(T, E)>,
// }

// pub trait State<C: Context> {
//     fn enter(&mut self, ctx: &mut C, event: Option<&C::Event>);
//     fn exit(&mut self, ctx: &mut C, event: Option<&C::Event>);
// }

// pub trait Transition<C: Context> {
//     fn guard(&mut self, ctx: &mut C, event: &C::Event) -> bool;
// }

impl<T> Statechart<T> {
    pub fn new<C: Context<T, E>, E>(
        inner: T,
        ctx: &mut C,
        graph: &Graph<C::State, C::Transition>,
    ) -> Statechart<T> {
        // TODO: is this really true? do we even need to call enters on init?
        let initial = graph
            .initial(graph.root)
            .map(|i| i.1)
            .expect("initial for root");

        let mut sc = Statechart {
            inner,
            active: graph.root,
            history: Default::default(),
            //_ctx: PhantomData,
        };

        // Transition to the initial state. This already recursively finds initial...
        sc.transition_to(initial, ctx, graph, None, None);
        sc
    }

    // TODO: does history make sense for the root initial?
    // FIX: remove me?
    pub fn reset<C: Context<T, E>, E>(
        &mut self,
        inner: T,
        ctx: &mut C,
        graph: &Graph<C::State, C::Transition>,
    ) {
        *self = Statechart::new(inner, ctx, graph);
    }

    /// Find a transition out of the active node based on the event. Start with the active state and
    /// work through the parent states.
    fn select<C: Context<T, E>, E>(
        &mut self,
        ctx: &mut C,
        graph: &Graph<C::State, C::Transition>,
        event: &E,
    ) -> Option<(Idx, bool, Option<Idx>)> {
        let mut path = graph.path_walk(self.active);

        // Each potential guard can mutate the transition state and context.
        while let Some(i) = path.next(graph) {
            // TODO since we are only mutating the context/locals we don't need the walker anymore?
            // TODO use transitions_out
            let mut edges = graph.graph.neighbors(i).detach();
            while let Some((i, next)) = edges.next(&graph.graph) {
                let edge = &graph.graph[i];

                let (t, ca) = match edge {
                    Edge::Transition(t, ca) => (t, *ca),
                    Edge::Internal(t) => (t, None),
                    Edge::Initial(_) => continue,
                };

                if ctx.filter(event, t, i) && ctx.guard(&mut self.inner, event, t, i) {
                    return Some((next, edge.is_internal(), ca));
                }
            }
        }

        // No transitions found for event.
        None
    }

    // TODO event could be a ref? use Borrow?
    pub fn transition<C: Context<T, E>, E>(
        &mut self,
        ctx: &mut C,
        graph: &Graph<C::State, C::Transition>,
        event: E,
    ) -> bool {
        ctx.dispatch(&event);

        // TODO select should return an edge?
        if let Some((next, internal, ca)) = self.select(ctx, graph, &event) {
            // With an internal transition the guard is called but we don't actually change
            // states. Should we verify the active state is not a compound state?
            let active = self.active;
            if !internal {
                self.transition_to(next, ctx, graph, Some(&event), ca);
                ctx.transition(&graph.graph[active].state, &graph.graph[next].state);
            }
            true
        } else {
            false
        }
    }

    /// Recursively find an initial state using local history.
    fn initial<S, Tr>(&self, graph: &Graph<S, Tr>, i: Idx) -> Idx {
        match graph.initial(i) {
            // No initial state, return i.
            None => i,
            // We don't check the initial type.
            Some((_initial, j)) => self.initial(graph, *self.history.get(&j.index()).unwrap_or(&j)),
        }
    }

    /// Traverse states up to a common ancestor (calling [State::exit] on each) and back down to the
    /// next state (calling [State::enter] on each).
    fn transition_to<C: Context<T, E>, E>(
        &mut self,
        next: Idx,
        ctx: &mut C,
        graph: &Graph<C::State, C::Transition>,
        event: Option<&E>,
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
        let p1 = graph
            .path_iter(self.active)
            .take_while(not_a)
            .collect::<Vec<_>>();

        // Recursively find initial using local history.
        let initial = self.initial(graph, next);

        assert!(graph.in_path(next, initial));

        // Path from the common ancestor to the next node (including the initial state).
        let mut p2 = graph
            .path_iter(initial)
            .take_while(not_a)
            .collect::<Vec<_>>();
        p2.reverse();

        let mut last = self.active;
        for i in p1 {
            ctx.exit(&mut self.inner, event, &graph.graph[i].state, i);

            // Update history when exiting a state.
            if let Some((initial, i)) = graph.initial(i) {
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
            ctx.enter(&mut self.inner, event, &graph.graph[i].state, i);
            //Ok(())
        }
        //.collect::<Result<_>>()?;

        // set active state
        self.active = next;

        //Ok(())
    }
}
