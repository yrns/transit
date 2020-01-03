use anyhow::Result;
use serde::{Deserialize, Serialize};

///! this is a door
use transit::{DefaultStateContext, DefaultTransitionContext, Initial, State, Statechart};

#[derive(Serialize, Deserialize)]
enum DoorEvent {
    Lock,
    Unlock,
    Open,
    Close,
    Destroy,
}

fn main() -> Result<()> {
    let door = mk_door()?;

    println!("{}", door.export()?);

    Ok(())
}

fn mk_door() -> Result<Statechart<DoorEvent, u32>> {
    let mut door = Statechart::new("door");
    let intact = door.add_state(State::new("intact", DefaultStateContext {}, None))?;
    let locked = door.add_state(State::new("locked", DefaultStateContext {}, Some(intact)))?;
    let closed = door.add_state(State::new("closed", DefaultStateContext {}, Some(intact)))?;
    let open = door.add_state(State::new("open", DefaultStateContext {}, Some(intact)))?;
    let destroyed = door.add_state(State::new("destroyed", DefaultStateContext {}, None))?;

    // make it default to the first added state?
    door.set_initial(Initial::Initial(intact))?;
    door.get_mut(intact).set_initial(Initial::Initial(locked))?;

    door.add_transition(
        intact,
        destroyed,
        DoorEvent::Destroy,
        DefaultTransitionContext {},
    )?;

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
