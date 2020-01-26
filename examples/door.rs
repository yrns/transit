///! this is a door
use anyhow::Result;
//use enum_dispatch::enum_dispatch;
use serde::{Deserialize, Serialize};
//use std::mem::discriminant;
use transit::{
    DefaultStateContext, DefaultTransitionContext, Initial, State, StateContext, Statechart,
    TransitionContext,
};

#[derive(Serialize, Deserialize)]
struct Door {
    hit_points: f32,
}

#[derive(Serialize, Deserialize, Default)]
struct Attack {
    damage: f32,
}

#[derive(Serialize, Deserialize)]
enum DoorEvent {
    Lock,
    Unlock,
    Open,
    Close,
    Bash(Attack),
}

// impl Statechart for Door {
//     type Event = DoorEvent;
// }

#[derive(Serialize, Deserialize)]
struct BashContext;

impl<Scc, E> TransitionContext<Scc, E> for BashContext {}

#[derive(Serialize, Deserialize, Default)]
struct LockedContext {
    attempts: usize,
}

impl<Scc, E> StateContext<Scc, E> for LockedContext {}

// TODO: devise a macro to generate these enums and the impls for contexts
//#[enum_dispatch(StateContext)]
#[derive(Serialize, Deserialize)]
enum DoorStateContext {
    DefaultStateContext(DefaultStateContext),
    LockedContext(LockedContext),
}

//#[enum_dispatch(StateContext)]
#[derive(Serialize, Deserialize)]
enum DoorTransitionContext {
    DefaultTransitionContext(DefaultTransitionContext),
    BashContext(BashContext),
}

// also make the macro do these
impl Default for DoorStateContext {
    fn default() -> Self {
        Self::DefaultStateContext(DefaultStateContext {})
    }
}

impl Default for DoorTransitionContext {
    fn default() -> Self {
        Self::DefaultTransitionContext(DefaultTransitionContext {})
    }
}

impl StateContext<Door, DoorEvent> for DoorStateContext {
    fn entry(&mut self, scc: &mut Door, event: &DoorEvent) -> Result<()> {
        match self {
            DoorStateContext::DefaultStateContext(ref mut ctx) => ctx.entry(scc, event),
            DoorStateContext::LockedContext(ref mut ctx) => ctx.entry(scc, event),
        }
    }

    fn exit(&mut self, scc: &mut Door, event: &DoorEvent) -> Result<()> {
        match self {
            DoorStateContext::DefaultStateContext(ref mut ctx) => ctx.entry(scc, event),
            DoorStateContext::LockedContext(ref mut ctx) => ctx.entry(scc, event),
        }
    }
}

impl TransitionContext<Door, DoorEvent> for DoorTransitionContext {
    fn action(&mut self, scc: &mut Door, event: &DoorEvent) -> Result<()> {
        match self {
            DoorTransitionContext::DefaultTransitionContext(ref mut ctx) => ctx.action(scc, event),
            DoorTransitionContext::BashContext(ref mut ctx) => ctx.action(scc, event),
        }
    }

    fn guard(&mut self, scc: &mut Door, event: &DoorEvent) -> bool {
        match self {
            DoorTransitionContext::DefaultTransitionContext(ref mut ctx) => ctx.guard(scc, event),
            DoorTransitionContext::BashContext(ref mut ctx) => ctx.guard(scc, event),
        }
    }
}

fn main() -> Result<()> {
    let door = mk_door()?;

    println!("{}", door.export()?);

    Ok(())
}

fn mk_door() -> Result<Statechart<DoorStateContext, DoorTransitionContext, Door, DoorEvent, u32>> {
    let mut door = Statechart::new("door", Door { hit_points: 100. });
    let intact = door.add_state(State::new("intact", DoorStateContext::default(), None))?;
    let locked = door.add_state(State::new(
        "locked",
        DoorStateContext::LockedContext(LockedContext::default()),
        Some(intact),
    ))?;
    let closed = door.add_state(State::new(
        "closed",
        DoorStateContext::default(),
        Some(intact),
    ))?;
    let open = door.add_state(State::new(
        "open",
        DoorStateContext::default(),
        Some(intact),
    ))?;
    let destroyed = door.add_state(State::new("destroyed", DoorStateContext::default(), None))?;

    // make it default to the first added state?
    door.set_initial(Initial::Initial(intact))?;
    door.get_mut(intact).set_initial(Initial::Initial(locked))?;

    door.add_transition(
        intact,
        destroyed,
        DoorEvent::Bash(Attack::default()),
        DoorTransitionContext::BashContext(BashContext {}),
    )?;

    door.add_transition(
        locked,
        closed,
        DoorEvent::Unlock,
        DoorTransitionContext::default(),
    )?;

    door.add_transition(
        closed,
        open,
        DoorEvent::Open,
        DoorTransitionContext::default(),
    )?;

    door.add_transition(
        open,
        closed,
        DoorEvent::Close,
        DoorTransitionContext::default(),
    )?;

    door.add_transition(
        closed,
        locked,
        DoorEvent::Lock,
        DoorTransitionContext::default(),
    )?;

    Ok(door)
}
