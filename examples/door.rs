///! this is a door
use anyhow::Result;
use serde::{Deserialize, Serialize};
use transit::{
    DefaultStateContext, DefaultTransitionContext, Initial, State, StateContext, Statechart,
    TransitionContext,
};

#[derive(Serialize, Deserialize)]
struct Door {
    hit_points: f32,
}

#[derive(Serialize, Deserialize)]
enum DoorEvent {
    Lock,
    Unlock,
    Open,
    Close,
    Bash,
}

// impl Statechart for Door {
//     type Event = DoorEvent;
// }

#[derive(Serialize, Deserialize)]
struct BashContext;

#[typetag::serialize]
impl TransitionContext<Door, DoorEvent> for BashContext {}

fn main() -> Result<()> {
    let door = mk_door()?;

    println!("{}", door.export()?);

    Ok(())
}

fn mk_door() -> Result<Statechart<Door, DoorEvent, u32>> {
    let mut door = Statechart::new("door", Door { hit_points: 100. });
    let intact = door.add_state(State::new("intact", DefaultStateContext {}, None))?;
    let locked = door.add_state(State::new("locked", DefaultStateContext {}, Some(intact)))?;
    let closed = door.add_state(State::new("closed", DefaultStateContext {}, Some(intact)))?;
    let open = door.add_state(State::new("open", DefaultStateContext {}, Some(intact)))?;
    let destroyed = door.add_state(State::new("destroyed", DefaultStateContext {}, None))?;

    // make it default to the first added state?
    door.set_initial(Initial::Initial(intact))?;
    door.get_mut(intact).set_initial(Initial::Initial(locked))?;

    door.add_transition(intact, destroyed, DoorEvent::Bash, BashContext {})?;

    door.add_transition(
        locked,
        closed,
        DoorEvent::Unlock,
        DefaultTransitionContext {},
    )?;

    door.add_transition(closed, open, DoorEvent::Open, DefaultTransitionContext {})?;

    door.add_transition(open, closed, DoorEvent::Close, DefaultTransitionContext {})?;

    door.add_transition(closed, locked, DoorEvent::Lock, DefaultTransitionContext {})?;

    Ok(door)
}
