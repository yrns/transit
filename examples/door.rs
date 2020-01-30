///! this is a door
use anyhow::Result;
//use enum_dispatch::enum_dispatch;
use serde::{Deserialize, Serialize};
//use std::mem::discriminant;
use rustyline::error::ReadlineError;
use transit::{
    DefaultStateContext, DefaultTransitionContext, Initial, State, StateContext, Statechart,
    TransitionContext,
};

#[derive(Serialize, Deserialize, Clone, Debug)]
struct Door {
    hit_points: f32,
    key: String,
}

#[derive(Serialize, Deserialize, Default, Debug)]
struct Attack {
    damage: f32,
}

#[derive(Serialize, Deserialize, Debug)]
enum DoorEvent {
    Lock(Option<String>),
    Unlock(Option<String>),
    Open,
    Close,
    Bash(Attack),
}

// impl Statechart for Door {
//     type Event = DoorEvent;
// }

#[derive(Serialize, Deserialize, Default, Debug)]
struct IntactContext;

impl StateContext<Door, DoorEvent> for IntactContext {
    fn entry(&mut self, _scc: &mut Door, _event: &DoorEvent) -> Result<()> {
        println!("You see a sturdy door.");
        Ok(())
    }
}

#[derive(Serialize, Deserialize, Debug)]
struct LockedContext;

impl StateContext<Door, DoorEvent> for LockedContext {}

#[derive(Serialize, Deserialize, Default, Debug)]
struct ClosedContext;

impl StateContext<Door, DoorEvent> for ClosedContext {
    fn entry(&mut self, _scc: &mut Door, _event: &DoorEvent) -> Result<()> {
        println!("The door is now closed.");
        Ok(())
    }
}

#[derive(Serialize, Deserialize, Default, Debug)]
struct OpenedContext;

impl StateContext<Door, DoorEvent> for OpenedContext {
    fn entry(&mut self, _scc: &mut Door, _event: &DoorEvent) -> Result<()> {
        println!("The door is now open.");
        Ok(())
    }
}

#[derive(Serialize, Deserialize, Default, Debug)]
struct DestroyedContext;

impl StateContext<Door, DoorEvent> for DestroyedContext {
    fn entry(&mut self, _scc: &mut Door, _event: &DoorEvent) -> Result<()> {
        println!("The door shatters into many pieces.");
        Ok(())
    }
}

// TODO: devise a macro to generate these enums and the impls for contexts
//#[enum_dispatch(StateContext)]
#[derive(Serialize, Deserialize, Debug)]
enum DoorStateContext {
    DefaultStateContext(DefaultStateContext),
    IntactContext(IntactContext),
    LockedContext(LockedContext),
    ClosedContext(ClosedContext),
    OpenedContext(OpenedContext),
    DestroyedContext(DestroyedContext),
}

#[derive(Serialize, Deserialize, Debug)]
struct BashContext;

impl TransitionContext<Door, DoorEvent> for BashContext {
    fn guard(&mut self, door: &mut Door, event: &DoorEvent) -> bool {
        if let DoorEvent::Bash(attack) = event {
            door.hit_points -= attack.damage;
            if door.hit_points <= 0. {
                // destroyed
                true
            } else {
                false
            }
        } else {
            false
        }
    }
}

#[derive(Serialize, Deserialize, Debug)]
struct LockContext;

impl TransitionContext<Door, DoorEvent> for LockContext {
    fn guard(&mut self, _door: &mut Door, event: &DoorEvent) -> bool {
        if let DoorEvent::Unlock(Some(key)) = event {
            if key == "the right key" {
                println!("You lock the door.");
                true
            } else {
                println!("That isn't the right key.");
                false
            }
        } else {
            false
        }
    }
}

#[derive(Serialize, Deserialize, Default, Debug)]
struct UnlockContext {
    attempts: usize,
}

impl TransitionContext<Door, DoorEvent> for UnlockContext {
    fn guard(&mut self, _door: &mut Door, event: &DoorEvent) -> bool {
        if let DoorEvent::Unlock(Some(key)) = event {
            if key == "the right key" {
                println!("You unlock the door.");
                true
            } else {
                println!("That isn't the right key. The lock wears slightly.");
                self.attempts += 1;
                false
            }
        } else {
            false
        }
    }
}

//#[enum_dispatch(StateContext)]
#[derive(Serialize, Deserialize, Debug)]
enum DoorTransitionContext {
    DefaultTransitionContext(DefaultTransitionContext),
    BashContext(BashContext),
    LockContext(LockContext),
    UnlockContext(UnlockContext),
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
            DoorStateContext::IntactContext(ref mut ctx) => ctx.entry(scc, event),
            DoorStateContext::LockedContext(ref mut ctx) => ctx.entry(scc, event),
            DoorStateContext::ClosedContext(ref mut ctx) => ctx.entry(scc, event),
            DoorStateContext::OpenedContext(ref mut ctx) => ctx.entry(scc, event),
            DoorStateContext::DestroyedContext(ref mut ctx) => ctx.entry(scc, event),
        }
    }

    fn exit(&mut self, scc: &mut Door, event: &DoorEvent) -> Result<()> {
        match self {
            DoorStateContext::DefaultStateContext(ref mut ctx) => ctx.exit(scc, event),
            DoorStateContext::IntactContext(ref mut ctx) => ctx.exit(scc, event),
            DoorStateContext::LockedContext(ref mut ctx) => ctx.exit(scc, event),
            DoorStateContext::ClosedContext(ref mut ctx) => ctx.exit(scc, event),
            DoorStateContext::OpenedContext(ref mut ctx) => ctx.exit(scc, event),
            DoorStateContext::DestroyedContext(ref mut ctx) => ctx.exit(scc, event),
        }
    }
}

impl TransitionContext<Door, DoorEvent> for DoorTransitionContext {
    fn action(&mut self, scc: &mut Door, event: &DoorEvent) -> Result<()> {
        match self {
            DoorTransitionContext::DefaultTransitionContext(ref mut ctx) => ctx.action(scc, event),
            DoorTransitionContext::BashContext(ref mut ctx) => ctx.action(scc, event),
            DoorTransitionContext::LockContext(ref mut ctx) => ctx.action(scc, event),
            DoorTransitionContext::UnlockContext(ref mut ctx) => ctx.action(scc, event),
        }
    }

    fn guard(&mut self, scc: &mut Door, event: &DoorEvent) -> bool {
        match self {
            DoorTransitionContext::DefaultTransitionContext(ref mut ctx) => ctx.guard(scc, event),
            DoorTransitionContext::BashContext(ref mut ctx) => ctx.guard(scc, event),
            DoorTransitionContext::LockContext(ref mut ctx) => ctx.guard(scc, event),
            DoorTransitionContext::UnlockContext(ref mut ctx) => ctx.guard(scc, event),
        }
    }
}

fn main() -> Result<()> {
    let mut door = mk_door()?;

    door.run();

    println!("You are in front of a large wooden door.");
    println!("What would you like to do? (o)pen (c)lose (l)ock (u)nlock (b)ash or maybe (s)tart over or (q)uit");

    let mut rl = rustyline::Editor::<()>::new();

    loop {
        println!("the door is: {}", door.active_state());
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => match line.trim_start().chars().next() {
                Some(c) => match c {
                    'o' => match door.transition(DoorEvent::Open) {
                        Err(_) => println!("That didn't work."),
                        _ => (),
                    },
                    'c' => match door.transition(DoorEvent::Close) {
                        Err(_) => println!("That didn't work."),
                        _ => (),
                    },
                    'l' => {
                        match door.transition(DoorEvent::Lock(Some("the right key".to_string()))) {
                            Err(_) => println!("That didn't work."),
                            _ => (),
                        }
                    }
                    'u' => match door
                        .transition(DoorEvent::Unlock(Some("the right key".to_string())))
                    {
                        Err(_) => println!("That didn't work."),
                        _ => (),
                    },
                    'b' => match door.transition(DoorEvent::Bash(Attack { damage: 40. })) {
                        Err(_) => println!("That didn't work."),
                        _ => (),
                    },
                    'q' => break,
                    _ => println!("Try again."),
                },
                _ => println!("Excuse me?"),
            },
            Err(ReadlineError::Interrupted) => {
                break;
            }
            Err(ReadlineError::Eof) => {
                break;
            }
            Err(e) => println!("Error: {:?}", e),
        }
    }

    Ok(())
}

fn mk_door() -> Result<Statechart<DoorStateContext, DoorTransitionContext, Door, DoorEvent, u32>> {
    let mut door = Statechart::new(
        "door",
        Door {
            hit_points: 100.,
            key: "the right key".to_string(),
        },
    );
    let intact = door.add_state(State::new(
        "intact",
        DoorStateContext::IntactContext(IntactContext {}),
        None,
    ))?;
    let locked = door.add_state(State::new(
        "locked",
        DoorStateContext::LockedContext(LockedContext {}),
        Some(intact),
    ))?;
    let closed = door.add_state(State::new(
        "closed",
        DoorStateContext::ClosedContext(ClosedContext {}),
        Some(intact),
    ))?;
    let open = door.add_state(State::new(
        "open",
        DoorStateContext::OpenedContext(OpenedContext {}),
        Some(intact),
    ))?;
    let destroyed = door.add_state(State::new(
        "destroyed",
        DoorStateContext::DestroyedContext(DestroyedContext {}),
        None,
    ))?;

    // make it default to the first added state?
    door.set_initial(intact)?;
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
        DoorEvent::Unlock(None),
        DoorTransitionContext::UnlockContext(UnlockContext::default()),
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
        DoorEvent::Lock(None),
        DoorTransitionContext::LockContext(LockContext {}),
    )?;

    // println!(
    //     "{:?}",
    //     door.common_ancestor(locked, destroyed)
    //         .map(|i| &door.graph[i].id)
    // );

    Ok(door)
}
