//! Edit graph structures.

// We only use emath here since we can build the library without the editor, in which case we only
// need emath for serialization.
use emath::*;

use crate::*;

use super::{Symbol, SymbolId};

/// Edit graph.
// TODO: revert back to type and make a trait?
#[cfg_attr(feature = "serde", derive(serde::Deserialize, serde::Serialize))]
#[cfg_attr(feature = "serde", serde(transparent))]
#[derive(Default, Clone)]
pub struct EditGraph(Graph<State, Transition>);

/// Initial (destination) port and control points. Similiar to transition.
pub type InitialData = (usize, Vec2, Vec2);

pub type MaxPorts = (Option<usize>, Option<usize>);

#[cfg_attr(feature = "serde", derive(serde::Deserialize, serde::Serialize))]
#[derive(Clone, Debug)]
pub struct State {
    pub id: String,
    pub enter: Option<String>,
    pub exit: Option<String>,
    /// Rect relative to parent.
    pub rect: Rect,
    pub initial: Option<InitialData>,
    // TODO
    #[allow(unused)]
    pub collapsed: bool,
    // TODO
    #[allow(unused)]
    pub pan: Vec2,
    // TODO
    #[allow(unused)]
    pub zoom: f32,
}

// This is only used for the root state?
impl Default for State {
    fn default() -> Self {
        State {
            id: "untitled".into(),
            enter: None,
            exit: None,
            rect: Rect::from_min_size(pos2(0.0, 0.0), Vec2::INFINITY),
            initial: None,
            collapsed: false,
            pan: Vec2::ZERO,
            zoom: 1.0,
        }
    }
}

impl From<&str> for State {
    fn from(id: &str) -> Self {
        Self {
            id: id.to_owned(),
            ..Default::default()
        }
    }
}

// Why default?
#[cfg_attr(feature = "serde", derive(serde::Deserialize, serde::Serialize))]
#[derive(Clone, Debug, Default, PartialEq)]
pub struct Transition {
    #[serde(default)]
    pub id: String,
    pub guard: Option<String>,
    /// Center widget left top position in root space.
    #[serde(default)]
    pub pos: Pos2,
    /// Control point offset from the state of origin (source).
    pub c1: Vec2, // tuples?
    /// Control point offset from the destination state (target).
    pub c2: Vec2,
    /// Source port index.
    pub port1: usize,
    /// Destination port index.
    pub port2: usize,
}

impl std::ops::Deref for EditGraph {
    type Target = Graph<State, Transition>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::DerefMut for EditGraph {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl EditGraph {
    /// Find the union of all child rects (non-recursive).
    pub fn children_rect(&self, i: Idx) -> Rect {
        let rect = |i| self.state(i).expect("state exists").rect;
        self.children(i)
            .map(rect)
            .reduce(Rect::union)
            .unwrap_or(Rect::ZERO)
    }

    pub fn path_string(&self, i: Idx) -> String {
        self.path(i)
            .into_iter()
            // Always skip the root id? We already know it's a door.
            .skip(1)
            .filter_map(|i| self.state(i).map(|s| &s.id))
            .cloned()
            .collect::<Vec<_>>()
            .join("-")
    }

    pub fn generate_symbol(&self, id: SymbolId) -> String {
        match id {
            SymbolId::Enter(i) => self.path_string(i) + "-enter",
            SymbolId::Exit(i) => self.path_string(i) + "-exit",
            SymbolId::Guard(t) => {
                self.transition(t)
                    .zip(self.endpoints(t))
                    .map(|(t, (a, _b))| self.path_string(a) + "-" + &t.id)
                    .unwrap_or_else(|| "???".into())
                    + "-guard"
            }
        }
    }

    /// Returns a list of ports in use for the specified state and direction.
    pub fn ports(&self, idx: Idx, direction: Direction) -> Vec<usize> {
        let ports = self.state_transitions(idx, direction);

        let mut ports: Vec<usize> = match direction {
            // Include the incoming port for self-transitions. Is there not a way to do this w/
            // flat_map instead of fold?
            Direction::Outgoing => ports.fold(Vec::new(), |mut acc, (_, _, target, t, _)| {
                if target == idx {
                    acc.extend([t.port1, t.port2])
                } else {
                    acc.push(t.port1)
                }
                acc
            }),

            Direction::Incoming => {
                // Filter out self-transitions, and include initial incoming connections. Maybe we
                // should store initial as an edge in the graph?
                ports
                    .filter_map(|(_, source, _, t, _)| (source != idx).then_some(t.port2))
                    .chain(
                        self.path_iter(idx)
                            .filter(|i| self.initial(*i).map(|(_, j)| j) == Some(idx))
                            .filter_map(|i| self.state(i).and_then(|s| s.initial).map(|i| i.0)),
                    )
                    .collect()
            }
        };

        ports.sort();
        ports
    }

    pub fn max_port(&self, idx: Idx, direction: Direction) -> Option<usize> {
        self.ports(idx, direction).into_iter().last()
    }

    pub fn max_ports(&self, i: Idx) -> MaxPorts {
        (
            self.max_port(i, Direction::Incoming),
            self.max_port(i, Direction::Outgoing),
        )
    }

    /// Find a free port starting from the specified port. This is used to find a second free port
    /// when adding a new self-transition.
    pub fn free_port_from(&self, idx: Idx, direction: Direction, from: usize) -> usize {
        let ports = self.ports(idx, direction);

        let mut free = from;
        for port in ports {
            if free < port {
                return free;
            } else {
                free = port + 1
            }
        }
        free
    }

    pub fn free_port2(&self, idx: Idx, direction: Direction) -> (usize, usize) {
        let p1 = self.free_port_from(idx, direction, 0);
        let p2 = self.free_port_from(idx, direction, p1 + 1);
        (p1, p2)
    }

    pub fn free_port(&self, idx: Idx, direction: Direction) -> usize {
        self.free_port_from(idx, direction, 0)
    }
}

// For self-transitions: target takes up an outgoing port.
pub fn target_dir(a: Idx, b: Idx) -> Direction {
    if a == b {
        Direction::Outgoing
    } else {
        Direction::Incoming
    }
}

// This is also the fixed inset.
const PORT_SPACING: f32 = 10.0;

// Scale based on number of ports and available vertical space. When the state is fully collapsed we
// don't want to show the endpoints?
pub fn port_pos(rect: &(Rect, MaxPorts), port: usize, direction: Direction) -> Pos2 {
    let (rect, max_ports) = rect;
    let (origin, max_port) = match direction {
        Direction::Incoming => (rect.min, max_ports.0),
        Direction::Outgoing => (rect.right_top(), max_ports.1),
    };

    // The port can be higher than the max...
    let max_port = max_port.unwrap_or_default().max(port);

    // Take available space (minus inset on top/bottom) and divide by number of ports in use. This
    // can be negative - this is where we need the original unclipped rect? TODO: show connections
    // to states that are clipped or otherwise hidden - connect to parent w/ hidden indicator?
    let spacing =
        ((rect.height() - PORT_SPACING * 2.0).max(0.0) / (max_port + 1) as f32).min(PORT_SPACING);
    origin + vec2(0.0, PORT_SPACING + spacing * port as f32)
}

impl State {
    pub const DEFAULT_SIZE: Vec2 = vec2(256.0, 64.0);

    pub fn with_id(mut self, id: impl Into<String>) -> Self {
        self.id = id.into();
        self
    }

    pub fn with_initial(mut self, initial: Option<(usize, Vec2, Vec2)>) -> Self {
        self.initial = initial;
        self
    }

    pub fn with_enter(mut self, enter: Symbol) -> Self {
        self.enter = enter;
        self
    }
    pub fn with_exit(mut self, exit: Symbol) -> Self {
        self.exit = exit;
        self
    }
}

impl Transition {
    pub fn with_id(mut self, id: impl Into<String>) -> Self {
        self.id = id.into();
        self
    }

    pub fn with_port1(mut self, port1: usize) -> Self {
        self.port1 = port1;
        self
    }

    pub fn with_port2(mut self, port2: usize) -> Self {
        self.port2 = port2;
        self
    }

    pub fn with_guard(mut self, guard: Symbol) -> Self {
        self.guard = guard;
        self
    }

    /// Offset position.
    pub fn translate(mut self, offset: Vec2) -> Self {
        self.pos += offset;
        self
    }
}

pub fn midpoint(a: impl Into<Pos2>, b: impl Into<Pos2>) -> Pos2 {
    (a.into() + b.into().to_vec2()) * 0.5
}

#[allow(unused)]
fn quad_beziers_from_points(points: &[Pos2]) -> impl Iterator<Item = [Pos2; 3]> + '_ {
    points.windows(3).enumerate().map(|(i, p)| {
        let [mut p0, p1, mut p2] = *p else {
            panic!("window is always 3");
        };
        if i != 0 {
            p0 = midpoint(p0, p1);
        }
        if i + 2 < points.len() - 1 {
            p2 = midpoint(p1, p2);
        }
        [p0, p1, p2]
    })
}
