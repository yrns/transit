//! Provides a Bevy OneShot context.

// TODO: also support Bevy schedule states?
// see: https://docs.rs/bevy/latest/bevy/ecs/schedule/struct.State.html
// and https://docs.rs/bevy/latest/bevy/ecs/schedule/fn.apply_state_transition.html

mod asset;
mod source;

use std::{collections::HashMap, marker::PhantomData};

use bevy_asset::{AssetEvent, AssetId, Assets, Handle};
use bevy_ecs::{
    prelude::*,
    system::{SystemId, SystemState},
};
#[allow(unused)]
// TODO:
use bevy_eventlistener::event_listener::EntityEvent;
use bevy_reflect::Enum;
use edit_egui as edit;
#[allow(unused)]
use tracing::{error, info, warn};
use transit_graph::{Context, Graph, Idx, Tdx};

use asset::{EditGraph, EditGraphLoader};
pub use source::*;

#[derive(Clone, Debug, Default)]
pub struct State<E> {
    pub id: String,
    pub enter: Option<SystemId<(Entity, Option<E>)>>,
    pub exit: Option<SystemId<(Entity, Option<E>)>>,
}

#[derive(Clone, Debug, Default)]
pub struct Transition<E> {
    /// Matches the event kind.
    pub id: String,
    pub guard: Option<SystemId<(Entity, E), bool>>,
}

/// This calls one-shot systems stored in the graph, which requires a mutable [World] reference. The
/// entity stored is the "current" entity and passed to called systems along with the current event.
pub struct OneShot<'w>(&'w mut World, Entity);

/// Map of systems via string.
#[derive(Resource)]
pub struct Actions<E> {
    states: HashMap<&'static str, SystemId<(Entity, Option<E>)>>,
    transitions: HashMap<&'static str, SystemId<(Entity, E), bool>>,
}

impl<E> Default for Actions<E> {
    fn default() -> Self {
        Self {
            states: Default::default(),
            transitions: Default::default(),
        }
    }
}

#[derive(Resource)]
pub struct GraphCache<E>(HashMap<AssetId<EditGraph>, Graph<State<E>, Transition<E>>>);

impl<E> Default for GraphCache<E> {
    fn default() -> Self {
        Self(Default::default())
    }
}

// We can't process edit graphs into the resolved graph type which requires runtime information.
//#[derive(Asset, TypePath)] // Reflect
//#[reflect(type_path = false)]
//pub struct Graph<E: Event>(transit_graph::Graph<State<E>, Transition<E>>);

/// This component contains the statechart and graph handle. There is no inner state since we can't
/// pass a mutable world-owned reference into a one-shot system.
#[derive(Component, Clone, Debug)]
// FIX: this needs phantom E
pub struct Statechart(transit_graph::Statechart<()>, Handle<EditGraph>);

// impl Statechart {
//     pub fn new<'w>(ctx: OneShot<'w>, graph: Handle<EditGraph>) -> Self {
//         //let sc = transit_graph::Statechart::new((), ctx, graph);
//     }
// }

impl<'w, E> Context<(), E> for OneShot<'w>
where
    E: Event + Enum + Clone,
{
    type State = State<E>;
    type Transition = Transition<E>;

    fn enter(&mut self, _inner: &mut (), event: Option<&E>, state: &Self::State, _index: Idx) {
        if let Some(id) = &state.enter {
            if let Err(e) = self.0.run_system_with_input(*id, (self.1, event.cloned())) {
                error!(?e);
            }
        }
    }

    fn exit(&mut self, _inner: &mut (), event: Option<&E>, state: &Self::State, _index: Idx) {
        if let Some(id) = &state.enter {
            if let Err(e) = self.0.run_system_with_input(*id, (self.1, event.cloned())) {
                error!(?e);
            }
        }
    }

    fn guard(
        &mut self,
        _inner: &mut (),
        event: &E,
        transition: &Self::Transition,
        _index: Tdx,
    ) -> bool {
        // Check variant kind first.
        transition.id == event.variant_name()
            // Then call the guard system.
            && transition
                .guard
                .and_then(|id| {
                    self.0
                        .run_system_with_input(id, (self.1, event.clone()))
                        .map_err(|e| error!(?e))
                        .ok()
                })
                // No guard? Return true.
                .unwrap_or(true)
    }
}

fn resolve<E>(graph: &edit::EditGraph, actions: &Actions<E>) -> Graph<State<E>, Transition<E>> {
    graph.map(
        |_i,
         edit::State {
             id, enter, exit, ..
         }| State {
            id: id.clone(),
            // TODO: warn
            enter: enter
                .as_ref()
                .and_then(|e| actions.states.get(e.as_str()))
                .cloned(),
            exit: exit
                .as_ref()
                .and_then(|e| actions.states.get(e.as_str()))
                .cloned(),
        },
        |_i, edit::Transition { id, guard, .. }| Transition {
            id: id.clone(),
            guard: guard
                .as_ref()
                .and_then(|e| actions.transitions.get(e.as_str()))
                .cloned(),
        },
    )
}

fn resolve_graph<E: Event>(
    mut graph_cache: ResMut<GraphCache<E>>,
    edit_graphs: Res<Assets<EditGraph>>,
    actions: Res<Actions<E>>,
    mut asset_events: EventReader<AssetEvent<EditGraph>>,
) {
    for event in asset_events.read() {
        dbg!(event);
        match event {
            AssetEvent::Added { id } | AssetEvent::Modified { id } => {
                if let Some(g) = edit_graphs.get(*id) {
                    _ = graph_cache.0.insert(*id, resolve(&g.0.graph, &*actions))
                }
            }
            AssetEvent::Removed { id } | AssetEvent::Unused { id } => _ = graph_cache.0.remove(id),
            _ => (),
        }
    }
}

pub fn handle_event<E: Event + Enum + Clone>(
    world: &mut World,
    state: &mut SystemState<(Query<(Entity, &Statechart)>, EventReader<E>)>,
) {
    let (entities, mut events) = state.get(world);
    let entities = entities
        .iter()
        // Clone the graph handle.
        .map(|(e, sc)| (e, sc.1.clone()))
        .collect::<Vec<_>>();
    let events = events.read().cloned().collect::<Vec<_>>();

    if entities.is_empty() || events.is_empty() {
        return;
    }

    world.resource_scope(|world, graphs: Mut<GraphCache<E>>| {
        for (entity, handle) in entities.iter() {
            let Some(graph) = graphs.0.get(&handle.id()) else {
                continue;
            };

            // Clone the statechart since we can't hold a mutable ref.
            let mut sc = world.entity(*entity).get::<Statechart>().unwrap().clone();

            let mut ctx = OneShot(world, *entity);
            if events
                .iter()
                .filter(|&event| sc.0.transition(&mut ctx, graph, event.clone()))
                .next()
                .is_some()
            {
                // Copy the statechart back. Since we don't have any inner state it's okay to
                // selectively do this.
                *(world.entity_mut(*entity).get_mut::<Statechart>().unwrap()) = sc;
            }
        }
    });
}

// pub fn handle_entity_event<E: EntityEvent>(
//     _world: &mut World,
//     mut _query: Query<(Entity, &mut Statechart<E>)>,
//     _events: EventReader<E>,
// ) {
//     todo!()
// }

pub struct Plugin<E> {
    phantom: PhantomData<E>,
}

impl<E> Default for Plugin<E> {
    fn default() -> Self {
        Self {
            phantom: PhantomData,
        }
    }
}

impl<E> bevy_app::Plugin for Plugin<E>
where
    E: Event + Enum + Clone,
{
    fn build(&self, app: &mut bevy_app::App) {
        use bevy_app::Update;
        use bevy_asset::AssetApp;

        app.init_asset::<EditGraph>()
            .init_asset_loader::<EditGraphLoader>()
            .insert_resource(GraphCache::<E>::default())
            .add_systems(Update, (resolve_graph::<E>, handle_event::<E>).chain());
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use bevy_app::prelude::App;
    use bevy_asset::{AssetServer, LoadState};
    use bevy_reflect::Reflect;

    #[derive(Component, Default)]
    struct Counter(usize);

    #[derive(Event, Clone, Debug, Reflect)]
    enum E {
        A,
        B,
        C,
    }

    fn enter_red(In((entity, event)): In<(Entity, Option<E>)>, mut counter: Query<&mut Counter>) {
        println!("enter_red: {event:?}");
        counter.get_mut(entity).unwrap().0 += 1;
    }

    fn enter_blue(In((entity, event)): In<(Entity, Option<E>)>, mut counter: Query<&mut Counter>) {
        println!("enter_blue: {event:?}");
        counter.get_mut(entity).unwrap().0 += 1;
    }

    fn guard_a(In((entity, event)): In<(Entity, E)>, mut counter: Query<&mut Counter>) -> bool {
        println!("guard_a: {event:?}");
        counter.get_mut(entity).unwrap().0 += 1;
        true
    }

    #[test]
    fn it_works() {
        let mut app = App::new();
        let mut actions = Actions::default();
        app.add_event::<E>();

        let id = app.world.register_system(enter_red);
        actions.states.insert("enter_red", id);
        let id = app.world.register_system(enter_blue);
        actions.states.insert("enter_blue", id);

        let id = app.world.register_system(guard_a);
        actions.transitions.insert("guard_a", id);

        app.insert_resource(actions);

        app.add_plugins((
            // asset server needs this
            bevy_core::TaskPoolPlugin::default(),
            bevy_asset::AssetPlugin {
                file_path: "tests".to_owned(),
                ..Default::default()
            },
            Plugin::<E>::default(),
        ));

        let asset_server = app.world.get_resource::<AssetServer>().unwrap();

        let handle = asset_server.load("test_graph.ron");

        while let LoadState::Loading = app
            .world
            .get_resource::<AssetServer>()
            .unwrap()
            .load_state(&handle)
        {
            app.update();
        }

        // We need one additional update to resolve the graph.
        app.update();

        // We need a context, which means we need the Entity before the statechart can exist.
        let id = app.world.spawn(Counter(0)).id();

        // We need a reference to the graph while mutating world. We need a mutable world ref every
        // time we spawn a statechart...
        app.world
            .resource_scope(|world, graph_cache: Mut<GraphCache<E>>| {
                let graph = graph_cache.0.get(&handle.id()).unwrap();
                let statechart =
                    transit_graph::Statechart::new((), &mut OneShot(world, id), &graph);

                world.entity_mut(id).insert(Statechart(statechart, handle));
            });

        //dbg!(app.world.entity(id).get::<Statechart>().unwrap().0.active);

        // we start in red
        assert_eq!(app.world.entity(id).get::<Counter>().unwrap().0, 1);

        // enter blue
        app.world.send_event(E::C); // this should do nothing
        app.world.send_event(E::A);
        app.update();
        assert_eq!(app.world.entity(id).get::<Counter>().unwrap().0, 3);

        // enter red again
        app.world.send_event(E::B);
        app.update();
        assert_eq!(app.world.entity(id).get::<Counter>().unwrap().0, 4);
    }
}
