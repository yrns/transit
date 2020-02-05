///! this is a door
use anyhow::{anyhow, Result};
//use enum_dispatch::enum_dispatch;
use once_cell::sync::Lazy;
use rustyline::error::ReadlineError;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::mem::{discriminant, Discriminant};
use transit::{ActionFn, Context, Graph, Initial, State, Statechart, Transition};

#[derive(Serialize, Deserialize, Clone, Debug)]
struct Door {
    hit_points: f32,
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
    m.insert("opened_entry", |c, e| Door::opened_entry(c, e));
    m.insert("destroyed_entry", |c, e| Door::destroyed_entry(c, e));
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
        *EVENT_MAP.get(event).unwrap()
    }

    fn action(action: &str) -> &ActionFn<Self, Self::Event> {
        // TODO: unwrap
        ACTION_MAP.get(action).unwrap()
    }
}

impl Door {
    fn intact_entry(&mut self, _event: &DoorEvent) -> Result<()> {
        println!("You see a sturdy door.");
        Ok(())
    }

    fn closed_entry(&mut self, _event: &DoorEvent) -> Result<()> {
        println!("The door is now closed.");
        Ok(())
    }

    fn opened_entry(&mut self, _event: &DoorEvent) -> Result<()> {
        println!("The door is now open.");
        Ok(())
    }

    fn destroyed_entry(&mut self, _event: &DoorEvent) -> Result<()> {
        println!("The door shatters into many pieces.");
        Ok(())
    }

    // so we need an id for transitions? bash1_bash_guard in case there are multiple?
    fn bash_guard(&mut self, event: &DoorEvent) -> Result<()> {
        if let DoorEvent::Bash(attack) = event {
            self.hit_points -= attack.damage;
            if self.hit_points <= 0. {
                // destroyed
                Ok(())
            } else {
                Err(anyhow!("guard false"))
            }
        } else {
            Err(anyhow!("guard event mismatch"))
        }
    }

    fn lock_guard(&mut self, event: &DoorEvent) -> Result<()> {
        if let DoorEvent::Unlock(Some(key)) = event {
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

    fn unlock_guard(&mut self, event: &DoorEvent) -> Result<()> {
        if let DoorEvent::Unlock(Some(key)) = event {
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

    door.run();

    println!("You are in front of a large wooden door.");

    // TODO: start over
    println!("What would you like to do? (o)pen (c)lose (l)ock (u)nlock (b)ash or maybe (s)tart over or (q)uit");

    let mut rl = rustyline::Editor::<()>::new();

    loop {
        println!("the door is: {}", door.active());
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

fn mk_door() -> Result<Statechart<Door>> {
    let door = Door {
        hit_points: 100.,
        key: "the right key".to_string(),
        attempts: 0,
    };

    let mut g = Graph::new("door");

    let intact = g.add_state(State::new("intact", None, Some("intact_entry"), None))?;
    let locked = g.add_state(State::new("locked", Some(intact), None, None))?;
    let closed = g.add_state(State::new(
        "closed",
        Some(intact),
        Some("closed_entry"),
        None,
    ))?;
    let open = g.add_state(State::new("open", Some(intact), Some("open_entry"), None))?;
    let destroyed = g.add_state(State::new("destroyed", None, Some("destroyed_entry"), None))?;

    // make it default to the first added state?
    g.set_initial(intact)?;
    g.get_mut(intact).set_initial(Initial::Initial(locked))?;

    g.add_transition(
        intact,
        destroyed,
        Transition::new("bash", Some("bash_guard"), None),
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
