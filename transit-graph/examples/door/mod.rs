//! A door as statechart.

use serde::{Deserialize, Serialize};
use transit_graph::*;

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct HitPoints {
    pub current: f32,
    pub max: f32,
}

impl Default for HitPoints {
    fn default() -> Self {
        Self {
            current: 100.,
            max: 100.,
        }
    }
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Door {
    pub hit_points: HitPoints,
    pub key: String,
    pub attempts: u32,
}

impl Default for Door {
    fn default() -> Self {
        Door {
            hit_points: Default::default(),
            key: "the right key".to_string(),
            attempts: 0,
        }
    }
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct Attack {
    pub damage: f32,
}

#[derive(Debug)]
pub enum DoorEvent {
    Lock(Option<String>),
    Unlock(Option<String>),
    Open,
    Close,
    Bash(Attack),
}

pub struct DoorContext;

impl Context<Door, DoorEvent> for DoorContext {
    type State = DoorState;
    type Transition = DoorGuard;

    fn enter(
        &mut self,
        _inner: &mut Door,
        _event: Option<&DoorEvent>,
        state: &DoorState,
        _index: Idx,
    ) {
        state.enter()
    }

    fn exit(
        &mut self,
        _inner: &mut Door,
        _event: Option<&DoorEvent>,
        state: &DoorState,
        _index: Idx,
    ) {
        state.exit();
    }

    fn guard(
        &mut self,
        inner: &mut Door,
        event: &DoorEvent,
        transition: &DoorGuard,
        _index: Tdx,
    ) -> bool {
        transition.guard(inner, event)
    }
}

// The default is only used for the root state.
#[derive(Serialize, Deserialize, Clone, Debug, Default)]
pub enum DoorState {
    #[default]
    None,
    Intact,
    Locked,
    Closed,
    Open,
    Destroyed,
}

impl std::fmt::Display for DoorState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            DoorState::None => "???",
            DoorState::Intact => "intact",
            DoorState::Locked => "locked",
            DoorState::Closed => "closed",
            DoorState::Open => "open",
            DoorState::Destroyed => "destroyed",
        })
    }
}

// Techically this isn't needed since we're matching on the enum.
trait Transition {
    fn guard(&self, _inner: &mut Door, _event: &DoorEvent) -> bool {
        true
    }
}

impl DoorState {
    fn enter(&self) {
        match self {
            DoorState::Intact => println!("You are in front of a large wooden door."),
            // It only makes sense to print this if we're coming from open.
            // DoorState::Closed => println!("The door is now closed."),
            DoorState::Open => println!("The door is now open."),
            DoorState::Destroyed => println!("The door shatters into many pieces."),
            _ => (),
        }
    }

    fn exit(&self) {}
}

#[derive(Serialize, Deserialize, Clone)]
pub struct BashGuard;

impl Transition for BashGuard {
    fn guard(&self, inner: &mut Door, event: &DoorEvent) -> bool {
        match event {
            DoorEvent::Bash(attack) => {
                // Check if this damage would destroy us.
                let new_hp = inner.hit_points.current - attack.damage;
                if new_hp <= 0. {
                    inner.hit_points.current = 0.;
                    true
                } else {
                    false
                }
            }
            _ => false,
        }
    }
}

#[derive(Serialize, Deserialize, Clone)]
pub struct BashGuardSelf;

// If the hp would be reduced to zero, let the other bash_guard
// transition work. In general we don't want to mutate state in
// guards that don't pass since if no guard passes no state will
// be mutated at all, and that might be confusing.
impl Transition for BashGuardSelf {
    fn guard(&self, inner: &mut Door, event: &DoorEvent) -> bool {
        match event {
            DoorEvent::Bash(attack) => {
                let was_full = inner.hit_points.current == inner.hit_points.max;
                let new_hp = inner.hit_points.current - attack.damage;

                if new_hp > 0. {
                    // The damage would leave us intact. Update our
                    // hit points and return true.
                    inner.hit_points.current = new_hp;
                    if was_full {
                        println!("The door appears to be slightly damaged.");
                    } else {
                        println!("The door appears to be more damaged.");
                    }
                    true
                } else {
                    // What is the difference with a self-transition? We
                    // stop checking after true.
                    false
                }
            }
            _ => false,
        }
    }
}

#[derive(Serialize, Deserialize, Clone)]
pub struct LockGuard {
    key: String,
}

impl Transition for LockGuard {
    fn guard(&self, _inner: &mut Door, event: &DoorEvent) -> bool {
        match event {
            DoorEvent::Lock(Some(key)) if key == &self.key => {
                println!("You lock the door.");
                true
            }
            DoorEvent::Lock(Some(_)) => {
                println!("That isn't the right key.");
                false
            }
            _ => false,
        }
    }
}

#[derive(Serialize, Deserialize, Clone)]
pub struct UnlockGuard {
    key: String,
}

impl Transition for UnlockGuard {
    fn guard(&self, inner: &mut Door, event: &DoorEvent) -> bool {
        match event {
            DoorEvent::Unlock(Some(key)) if key == &self.key => {
                println!("You unlock the door.");
                true
            }
            DoorEvent::Unlock(Some(_)) => {
                println!("That isn't the right key. The lock wears slightly.");
                inner.attempts += 1;
                false
            }
            _ => false,
        }
    }
}

#[derive(Serialize, Deserialize, Clone)]
pub struct OpenGuard;

impl Transition for OpenGuard {
    fn guard(&self, _inner: &mut Door, event: &DoorEvent) -> bool {
        match event {
            DoorEvent::Open => true,
            _ => false,
        }
    }
}

#[derive(Serialize, Deserialize, Clone)]
pub struct CloseGuard;

impl Transition for CloseGuard {
    fn guard(&self, _inner: &mut Door, event: &DoorEvent) -> bool {
        match event {
            DoorEvent::Close => {
                println!("The door is now closed.");
                true
            }
            _ => false,
        }
    }
}

// We can't easily clone Box<dyn Transition> so just dispatch on an
// enum.
#[derive(Serialize, Deserialize, Clone)]
pub enum DoorGuard {
    Bash(BashGuard),
    BashSelf(BashGuardSelf),
    Lock(LockGuard),
    Unlock(UnlockGuard),
    Open(OpenGuard),
    Close(CloseGuard),
}

impl Transition for DoorGuard {
    fn guard(&self, inner: &mut Door, event: &DoorEvent) -> bool {
        match self {
            DoorGuard::Bash(g) => g.guard(inner, event),
            DoorGuard::BashSelf(g) => g.guard(inner, event),
            DoorGuard::Lock(g) => g.guard(inner, event),
            DoorGuard::Unlock(g) => g.guard(inner, event),
            DoorGuard::Open(g) => g.guard(inner, event),
            DoorGuard::Close(g) => g.guard(inner, event),
        }
    }
}

pub fn make_graph() -> Graph<DoorState, DoorGuard> {
    let mut g = Graph::new();

    let intact = g.add_state(DoorState::Intact, None);
    let locked = g.add_state(DoorState::Locked, Some(intact));
    let closed = g.add_state(DoorState::Closed, Some(intact));
    let open = g.add_state(DoorState::Open, Some(intact));
    let destroyed = g.add_state(DoorState::Destroyed, None);

    // Set the root node initial to "locked".
    let _op = g.set_root_initial((Initial::Initial, locked));

    let _t = g.add_transition(intact, destroyed, DoorGuard::Bash(BashGuard {}));

    let bash = g.add_transition(intact, intact, DoorGuard::BashSelf(BashGuardSelf {}));

    // Internal transition -- the guard mutates the context but does not transition.
    let _op = g.set_internal(bash, true);

    let _op = g.add_transition(
        locked,
        closed,
        DoorGuard::Unlock(UnlockGuard {
            key: "silver key".to_owned(),
        }),
    );

    let _op = g.add_transition(closed, open, DoorGuard::Open(OpenGuard {}));

    let _op = g.add_transition(open, closed, DoorGuard::Close(CloseGuard {}));

    let _op = g.add_transition(
        closed,
        locked,
        DoorGuard::Lock(LockGuard {
            key: "silver key".to_owned(),
        }),
    );

    g
}

#[test]
fn export() {
    let g = make_graph();
    g.export_to_file("examples/door.ron").unwrap();
}
