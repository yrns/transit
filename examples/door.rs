///! this is a door
use anyhow::{anyhow, Result};
use once_cell::sync::Lazy;
use rustyline::error::ReadlineError;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::mem::{discriminant, Discriminant};
use transit::{ActionFn, Context, Graph, Initial, State, Statechart, Transition};

#[derive(Serialize, Deserialize, Clone, Debug)]
struct HitPoints {
    current: f32,
    max: f32,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
struct Door {
    hit_points: HitPoints,
    key: String,
    attempts: u32,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
struct Attack {
    damage: f32,
}

#[derive(Debug)]
enum DoorEvent {
    Lock(Option<String>),
    Unlock(Option<String>),
    Open,
    Close,
    Bash(Attack),
}

// macro?
static EVENT_MAP: Lazy<HashMap<&'static str, Discriminant<DoorEvent>>> = Lazy::new(|| {
    let mut m = HashMap::new();
    m.insert("lock", discriminant(&DoorEvent::Lock(None)));
    m.insert("unlock", discriminant(&DoorEvent::Unlock(None)));
    m.insert("open", discriminant(&DoorEvent::Open));
    m.insert("close", discriminant(&DoorEvent::Close));
    m.insert(
        "bash",
        discriminant(&DoorEvent::Bash(Attack { damage: 0. })),
    );
    m
});

static ACTION_MAP: Lazy<HashMap<&'static str, ActionFn<Door, DoorEvent>>> = Lazy::new(|| {
    let mut m = HashMap::<&'static str, ActionFn<Door, DoorEvent>>::new();
    m.insert("intact_entry", |c, e| Door::intact_entry(c, e));
    m.insert("closed_entry", |c, e| Door::closed_entry(c, e));
    m.insert("open_entry", |c, e| Door::open_entry(c, e));
    m.insert("destroyed_entry", |c, e| Door::destroyed_entry(c, e));
    m.insert("bash_guard_self", |c, e| Door::bash_guard_self(c, e));
    m.insert("bash_guard", |c, e| Door::bash_guard(c, e));
    m.insert("lock_guard", |c, e| Door::lock_guard(c, e));
    m.insert("unlock_guard", |c, e| Door::unlock_guard(c, e));
    m
});

impl Context for Door {
    type Event = DoorEvent;

    // fn events() -> &'static HashMap<&'static str, Discriminant<Self::Event>> {
    //     &EVENT_MAP
    // }

    // fn actions() -> &'static HashMap<&'static str, ActionFn<Self, Self::Event>> {
    //     &ACTION_MAP
    // }

    fn event(event: &str) -> Discriminant<DoorEvent> {
        // TODO: unwrap
        *EVENT_MAP.get(event).expect("missing event")
    }

    fn action(action: &str) -> &ActionFn<Self, Self::Event> {
        // TODO: unwrap
        ACTION_MAP.get(action).expect("missing action")
    }
}

impl Door {
    fn intact_entry(&mut self, _event: Option<&DoorEvent>) -> Result<()> {
        println!("You are in front of a large wooden door.");
        Ok(())
    }

    fn closed_entry(&mut self, _event: Option<&DoorEvent>) -> Result<()> {
        // it only makes sense to print this if we're coming from open
        //println!("The door is now closed.");
        Ok(())
    }

    fn open_entry(&mut self, _event: Option<&DoorEvent>) -> Result<()> {
        println!("The door is now open.");
        Ok(())
    }

    fn destroyed_entry(&mut self, _event: Option<&DoorEvent>) -> Result<()> {
        println!("The door shatters into many pieces.");
        Ok(())
    }

    // so we need an id for transitions? bash1_bash_guard in case there are multiple?
    fn bash_guard(&mut self, event: Option<&DoorEvent>) -> Result<()> {
        if let Some(DoorEvent::Bash(attack)) = event {
            let new_hp = self.hit_points.current - attack.damage;

            if new_hp <= 0. {
                // destroyed
                self.hit_points.current = 0.;
                Ok(())
            } else {
                Err(anyhow!("guard false"))
            }
        } else {
            Err(anyhow!("guard event mismatch"))
        }
    }

    // If the hp would be reduced to zero, let the other bash_guard
    // transition work. In general we don't want to mutate state in
    // guards that don't pass since if no guard passes no state will
    // be mutated at all, and that might be confusing.
    fn bash_guard_self(&mut self, event: Option<&DoorEvent>) -> Result<()> {
        if let Some(DoorEvent::Bash(attack)) = event {
            let was_full = self.hit_points.current == self.hit_points.max;
            let new_hp = self.hit_points.current - attack.damage;

            if new_hp > 0. {
                self.hit_points.current = new_hp;
                if was_full {
                    println!("The door appears to be slightly damaged.");
                } else {
                    println!("The door appears to be more damaged.");
                }
                Ok(())
            } else {
                Err(anyhow!("guard false"))
            }
        } else {
            Err(anyhow!("guard event mismatch"))
        }
    }

    fn lock_guard(&mut self, event: Option<&DoorEvent>) -> Result<()> {
        if let Some(DoorEvent::Lock(Some(key))) = event {
            if key == "the right key" {
                println!("You lock the door.");
                Ok(())
            } else {
                println!("That isn't the right key.");
                Err(anyhow!("guard false"))
            }
        } else {
            Err(anyhow!("guard event mismatch"))
        }
    }

    fn unlock_guard(&mut self, event: Option<&DoorEvent>) -> Result<()> {
        if let Some(DoorEvent::Unlock(Some(key))) = event {
            if key == "the right key" {
                println!("You unlock the door.");
                Ok(())
            } else {
                println!("That isn't the right key. The lock wears slightly.");
                self.attempts += 1;
                Err(anyhow!("guard false"))
            }
        } else {
            Err(anyhow!("guard event mismatch"))
        }
    }
}

fn main() -> Result<()> {
    let mut door = mk_door()?;

    door.run()?;

    // TODO: start over
    println!("What would you like to do? (o)pen (c)lose (l)ock (u)nlock (b)ash, or maybe (s)tart over or (q)uit");

    let mut rl = rustyline::Editor::<()>::new();

    loop {
        //println!("the door is: {}", door.active());
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => match line.trim_start().chars().next() {
                Some(c) => {
                    let event = match c {
                        'o' => Some(DoorEvent::Open),
                        'c' => Some(DoorEvent::Close),
                        'l' => Some(DoorEvent::Lock(Some("the right key".to_string()))),
                        'u' => Some(DoorEvent::Unlock(Some("the right key".to_string()))),
                        'b' => Some(DoorEvent::Bash(Attack { damage: 40. })),
                        'q' => break,
                        _ => {
                            println!("Try again.");
                            None
                        }
                    };
                    if let Some(event) = event {
                        let res = door.transition(event);
                        if let Err(_e) = res {
                            //dbg!(_e);
                            println!("That didn't work. The door is {}.", door.active());
                        }
                    }
                }
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

fn mk_door() -> Result<Statechart<Door>> {
    let door = Door {
        hit_points: HitPoints {
            current: 100.,
            max: 100.,
        },
        key: "the right key".to_string(),
        attempts: 0,
    };

    let mut g = Graph::new("door");

    let intact = g.add_state(State::new("intact", None, Some("intact_entry"), None));
    let locked = g.add_state(State::new("locked", Some(intact), None, None));
    let closed = g.add_state(State::new(
        "closed",
        Some(intact),
        Some("closed_entry"),
        None,
    ));
    let open = g.add_state(State::new("open", Some(intact), Some("open_entry"), None));
    let destroyed = g.add_state(State::new("destroyed", None, Some("destroyed_entry"), None));

    // make it default to the first added state?
    g.set_initial(intact);
    g.get_mut(intact).set_initial(Initial::Initial(locked));

    g.add_transition(
        intact,
        destroyed,
        Transition::new("bash", Some("bash_guard"), None),
    )?;

    g.add_transition(
        intact,
        intact,
        // don't trigger entry/exit actions on this self-transition
        Transition::new("bash", Some("bash_guard_self"), None).set_internal(true),
    )?;

    g.add_transition(
        locked,
        closed,
        Transition::new("unlock", Some("unlock_guard"), None),
    )?;

    g.add_transition(closed, open, Transition::new("open", None, None))?;

    g.add_transition(open, closed, Transition::new("close", None, None))?;

    g.add_transition(
        closed,
        locked,
        Transition::new("lock", Some("lock_guard"), None),
    )?;

    Statechart::new(g, door)
}
