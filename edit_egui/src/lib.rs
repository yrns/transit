mod app;
mod editabel;
mod editor;
mod search;
mod source;
mod undo;
mod widget;

use std::borrow::Cow;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};

pub use app::*;
use editabel::Editabel;
pub use editor::*;
// TODO: only depend on egui here (need to get rid of eframe-dynamic)
use eframe::egui;
use egui::{
    epaint::{text::LayoutJob, CubicBezierShape, Vertex},
    *,
};
use search::{SearchBox, Submit};
pub use source::*;
use tracing::{error, info, warn};
use transit_graph::{Direction, Graph, Idx, Initial, Op, Tdx};
use undo::*;

#[cfg_attr(feature = "serde", derive(serde::Deserialize, serde::Serialize))]
#[derive(Debug, Default, Clone)]
pub enum Selection {
    #[default]
    None,
    State(Idx),
    Transition(Tdx),
}

/// Statechart graph editor.
#[cfg_attr(feature = "serde", derive(serde::Deserialize, serde::Serialize))]
//#[derive(Default)]
pub struct Edit<S> {
    /// Source file.
    pub source: Option<S>,
    /// Graph structure.
    pub graph: Graph<State, Transition>,
    /// Current selection.
    #[serde(default)]
    pub selection: Selection,
    /// Undo history.
    #[serde(default)]
    pub undo: Undo,
    /// Versioning.
    #[serde(default)]
    pub version: usize,
}

impl<S> Default for Edit<S> {
    fn default() -> Self {
        Self {
            source: None,
            graph: Graph::default(),
            selection: Selection::None,
            undo: Undo::default(),
            version: 0,
        }
    }
}

// Initial (destination) port and control points. Similiar to transition.
pub type InitialData = (usize, Vec2, Vec2);

#[derive(Copy, Clone, Debug)]
pub enum SymbolId {
    Enter(Idx),
    Exit(Idx),
    Guard(Tdx),
}

pub type Symbol = Option<String>;

#[cfg_attr(feature = "serde", derive(serde::Deserialize, serde::Serialize))]
#[derive(Clone, Debug)]
pub struct State {
    pub id: String,
    pub enter: Option<String>,
    pub exit: Option<String>,
    /// Rect relative to parent.
    rect: Rect,
    initial: Option<InitialData>,
    #[allow(unused)]
    collapsed: bool,
    #[allow(unused)]
    pan: Vec2,
    #[allow(unused)]
    zoom: f32,
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
    pos: Pos2,
    /// Control point offset from the state of origin (source).
    c1: Vec2, // tuples?
    /// Control point offset from the destination state (target).
    c2: Vec2,
    /// Source port index.
    port1: usize,
    /// Destination port index.
    port2: usize,
}

#[derive(Debug)]
pub enum Command {
    AddState(Idx, Pos2),
    RemoveState(Idx, bool),
    UpdateState(Idx, State),
    /// Source, target, position (relative to parent).
    MoveState(Idx, Idx, Pos2),
    ResizeState(Idx, Vec2),
    AddTransition(Idx, Idx, Transition),
    RemoveTransition(Tdx),
    UpdateTransition(Tdx, Transition),
    MoveTransition(Tdx, Option<Idx>, Option<Idx>),
    SetInitial(Idx, (Initial, Idx), InitialData),
    UnsetInitial(Idx),
    StepInitial(Idx),
    SetEnter,
    SetExit,
    SetGuard,
    SetInternal(Tdx, bool),
    UpdateSelection(Selection),
    SelectSourcePath(PathBuf),
    GotoSymbol(String, PathBuf, (usize, usize)),
    UpdateSymbol(SymbolId, Option<String>),
    InsertSymbol(String, PathBuf, String),
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum ControlPoint {
    C1,
    C2,
}

// Drag target index and depth (for states) or incoming port (for transitions).
pub type DragTarget = Option<(Idx, usize)>;

#[derive(Clone, Debug, Default)]
pub enum Drag {
    #[default]
    None,
    State {
        idx: Idx,
        /// Offset from the pointer to rect.min.
        offset: Vec2,
        //target: DragTarget,
        // Original depth.
        depth: usize,
        /// We can't access the pointer `press_origin` after the drag is stopped, so we store it here.
        press_origin: Pos2,
    },
    Resize(Idx, Vec2),
    Initial(Idx, DragTarget, (Vec2, Vec2)),
    InitialControl((Idx, ControlPoint), Vec2),
    AddTransition(Idx, Option<Idx>),
    // For the next two variants the first field is the opposite endpoint!
    TransitionSource(Idx, Option<Idx>, Tdx),
    TransitionTarget(Idx, Option<Idx>, Tdx),
    TransitionControl((Tdx, ControlPoint), Vec2),
    TransitionId(Tdx, Vec2),
}

impl Drag {
    pub fn sense(&self) -> Sense {
        match self {
            Drag::None => Sense::click_and_drag(),
            _ => Sense::hover(),
        }
    }

    pub fn in_drag(&self) -> bool {
        !matches!(self, Drag::None)
    }

    // pub fn dragging(&self, idx: Idx) -> bool {
    //     matches!(self, Drag::State(i, ..) if *i == idx)
    // }

    pub fn resizing(&self, idx: Idx) -> bool {
        matches!(self, Drag::Resize(i, ..) if *i == idx)
    }

    pub fn is_target(&self, idx: Idx) -> bool {
        matches!(self,
            //Drag::State { target: Some((t, ..)), .. }
            | Drag::Initial(_, Some((t, ..)), ..)
            | Drag::AddTransition(_, Some(t), ..)
            | Drag::TransitionSource(_, Some(t), ..)
            | Drag::TransitionTarget(_, Some(t), ..)
                if *t == idx)
    }

    /// Is the drag greater than some minimum?
    // FIX: remove me
    pub fn min_drag(&self, ui: &Ui) -> bool {
        match self {
            Drag::None => false, // ?
            // Compare original press to the current pointer. This only works if the drag is in
            // progress, not after the pointer is released.
            Drag::AddTransition(..)
            | Drag::Initial(..)
            | Drag::State { .. }
            | Drag::TransitionSource(..)
            | Drag::TransitionTarget(..) => {
                let p = ui.input(|i| i.pointer.press_origin().zip(i.pointer.interact_pos()));
                p.map(|(p0, p1)| (p1 - p0).abs().max_elem() >= 1.0)
                    .unwrap_or_default()
            }
            Drag::Resize(_, d)
            | Drag::InitialControl(_, d, ..)
            | Drag::TransitionControl(_, d, ..)
            | Drag::TransitionId(_, d) => d.abs().max_elem() >= 1.0,
        }
    }
}

// Deriving the variant and id from init failed...
// macro_rules! drag_id {
//     ($variant:ident($($field:tt)*,)) => {
//         $variant
//     };
// }

impl From<Idx> for Drag {
    fn from(idx: Idx) -> Self {
        Drag::Resize(idx, Vec2::ZERO)
    }
}

impl From<Tdx> for Drag {
    fn from(tdx: Tdx) -> Self {
        Drag::TransitionId(tdx, Vec2::ZERO)
    }
}

impl From<(Idx, ControlPoint)> for Drag {
    fn from(cp: (Idx, ControlPoint)) -> Self {
        Drag::InitialControl(cp, Vec2::ZERO)
    }
}

impl From<(Tdx, ControlPoint)> for Drag {
    fn from(cp: (Tdx, ControlPoint)) -> Self {
        Drag::TransitionControl(cp, Vec2::ZERO)
    }
}

macro_rules! drag_delta {
    // First rule is the default update.
    (($response:ident, $ui:ident, $drag:ident, $drag_ty:ident, $drag_id:expr), $end:expr) => {
        drag_delta!(
            ($response, $ui, $drag, $drag_ty, $drag_id),
            |delta: &mut Vec2| *delta += $response.drag_delta(),
            $end
        )
    };
    (($response:ident, $ui:ident, $drag:ident, $drag_ty:ident, $drag_id:expr), $update:expr, $end:expr) => {
        match $drag {
            Drag::None if $response.drag_started() => *$drag = $drag_id.into(),
            Drag::$drag_ty(_id, delta, ..) if *_id == $drag_id => {
                if $response.dragged() {
                    $update(delta)
                } else if $response.drag_stopped() {
                    //if $drag.min_drag($ui)
                    if delta.abs().max_elem() >= 1.0 {
                        $end(delta)
                    }
                    *$drag = Drag::None;
                }
            }
            _ => (), // if started/dragged/released error?
        }
    };
}

pub type MaxPorts = (Option<usize>, Option<usize>);

// Store rect in root-space, and incoming/outgoing max ports. Rename?
#[derive(Default)]
pub struct Rects(nohash_hasher::IntMap<usize, (Rect, MaxPorts)>);

impl Rects {
    fn from_graph(graph: &Graph<State, Transition>) -> Self {
        let mut rects = Self::default();
        fn insert(i: Idx, graph: &Graph<State, Transition>, parent_rect: &Rect, rects: &mut Rects) {
            // Translate to world/root space.
            let rect = graph.graph[i]
                .state
                .rect
                .translate(parent_rect.min.to_vec2());
            rects.insert_rect(i, rect, || Default::default());
            for child in graph.children(i) {
                insert(child, graph, &rect, rects);
            }
        }
        insert(graph.root, graph, &Rect::ZERO, &mut rects);
        rects
    }

    pub fn get(&self, idx: Idx) -> Option<(Rect, MaxPorts)> {
        self.0.get(&idx.index()).copied()
    }

    pub fn get_rect(&self, idx: Idx) -> Option<Rect> {
        self.get(idx).map(|a| a.0)
    }

    pub fn insert_rect<F: FnOnce() -> MaxPorts>(&mut self, idx: Idx, rect: Rect, f: F) {
        self.0
            .entry(idx.index())
            .and_modify(|a| a.0 = rect)
            .or_insert_with(|| (rect, f()));
    }

    // TODO: unused?
    pub fn insert_max_port(&mut self, idx: Idx, direction: Direction, max_port: Option<usize>) {
        assert!(self
            .0
            .get_mut(&idx.index())
            .map(|(_, (max_in, max_out))| {
                match direction {
                    Direction::Incoming => *max_in = max_port,
                    Direction::Outgoing => *max_out = max_port,
                }
            })
            .is_some());
    }
}

/// Mutable data, passed to each call of show_state.
#[derive(Default)]
pub struct EditData {
    rects: Rects,
    drag: Drag,
    // TEMP
    drag_transition: Option<Transition>,
    commands: Vec<Command>,
    search: SearchBox<Idx>,
    symbols: SearchBox<String>,
}

impl EditData {
    pub fn new() -> Self {
        Self {
            search: SearchBox {
                match_required: true,
                ..Default::default()
            },
            ..Default::default()
        }
    }
}

// Rename drag variants to New*? This somewhat mirrors Edge...
pub enum Connection<'a> {
    // Source, control points.
    Initial(Idx, (Vec2, Vec2)),
    DragInitial(Idx, (Vec2, Vec2)),
    Transition(Tdx, &'a Transition, bool, Vec2),
    // TODO: this no longer represents the final curve...
    DragTransition(Vec2, Vec2), // control points
}

// These two functions are copied from egui::resize which isn't public...
fn paint_resize_corner(ui: &mut Ui, response: &Response) {
    let stroke = ui.style().interact(response).fg_stroke;
    paint_resize_corner_with_style(ui, &response.rect, stroke, Align2::RIGHT_BOTTOM);
}

fn paint_resize_corner_with_style(ui: &mut Ui, rect: &Rect, stroke: Stroke, corner: Align2) {
    let painter = ui.painter();
    let cp = painter.round_pos_to_pixels(corner.pos_in_rect(rect));
    let mut w = 2.0;

    while w <= rect.width() && w <= rect.height() {
        painter.line_segment(
            [
                pos2(cp.x - w * corner.x().to_sign(), cp.y),
                pos2(cp.x, cp.y - w * corner.y().to_sign()),
            ],
            stroke,
        );
        w += 4.0;
    }
}

/// Default control point offset from source.
const CP_OFFSET: Vec2 = vec2(64.0, 0.0);

#[inline]
pub fn approx_cp(start: Pos2, end: Pos2) -> (Vec2, Vec2) {
    (start.to_vec2() + CP_OFFSET, end.to_vec2() - CP_OFFSET)
}

/// This is for state initial connections. Initial states are always down and right since the start
/// is near the upper left.
#[inline]
pub fn approx_cp_down(start: Pos2, end: Pos2) -> (Vec2, Vec2) {
    let d = end - start;
    // Try to keep the control point inside the source/parent rect.
    (CP_OFFSET.yx(), vec2(-CP_OFFSET.x.min(d.x), CP_OFFSET.y))
}

#[inline]
pub fn approx_cp_self(_start: Pos2, _end: Pos2) -> (Vec2, Vec2) {
    (vec2(128.0, -96.0), vec2(128.0, 96.0))
}

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("{0}")]
    Io(#[from] std::io::Error),
    #[cfg(feature = "serde")]
    #[error("{0}")]
    Import(#[from] ron::error::SpannedError),
    #[error("{0}")]
    Export(#[from] ron::Error),
    #[error("{0}")]
    Other(String),
}

impl<S> Edit<S>
where
    S: Source,
{
    /// Load from path.
    #[cfg(feature = "serde")]
    pub fn load(path: impl AsRef<Path>) -> Result<Self, Error>
    where
        S: serde::de::DeserializeOwned,
    {
        let mut edit: Self = ron::de::from_reader(std::fs::File::open(path.as_ref())?)?;

        // Validate graph.
        edit.graph.validate().map_err(Error::Other)?;

        // Validate selection.
        if match edit.selection {
            Selection::State(idx) => edit.graph.state(idx).is_none(),
            Selection::Transition(tdx) => edit.graph.transition(tdx).is_none(),
            _ => false,
        } {
            edit.selection = Selection::None
        }

        if edit.version == 0 {
            // reset transition positions and control points
            let rects = Rects::from_graph(&edit.graph);

            // transitions_mut doesn't work so we have to collect
            let pts = edit
                .graph
                .transitions()
                .map(|(tdx, source, target, _t, _int)| {
                    let [a, b] = [source, target].map(|i| rects.get_rect(i).unwrap().center());
                    (tdx, midpoint(a, b))
                })
                .collect::<Vec<_>>();

            for (tdx, pt) in pts {
                let t = edit.graph.transition_mut(tdx).unwrap();
                t.pos = pt;
                t.c1 = CP_OFFSET;
                t.c2 = -CP_OFFSET;
            }

            edit.version = 1;
        }

        // Start watching source path.
        if let Some(source) = &mut edit.source {
            match S::from_path(source.path()) {
                Ok(s) => *source = s,
                Err(e) => error!("error in source: {:?}", e),
            }
        }

        Ok(edit)
    }

    /// Serialize self to path.
    #[cfg(feature = "serde")]
    pub fn save(&self, path: impl AsRef<Path>) -> Result<(), Error>
    where
        S: serde::Serialize,
    {
        ron::ser::to_writer_pretty(
            std::fs::File::create(path.as_ref())?,
            self,
            ron::ser::PrettyConfig::default(),
        )
        .map_err(Error::from)
    }
}

impl<S> Edit<S>
where
    S: Source,
{
    /// Id of the root state.
    pub fn id(&self) -> &str {
        &self.graph.root().id
    }

    pub fn process_commands(&mut self, commands: impl IntoIterator<Item = Command>) {
        for c in commands {
            let op = match c {
                Command::AddState(parent, p) => self
                    .graph
                    .add_state(
                        State {
                            id: "untitled".into(),
                            rect: Rect::from_min_size(p, State::DEFAULT_SIZE),
                            ..Default::default()
                        },
                        Some(parent),
                    )
                    .into(),
                Command::RemoveState(idx, recur) => {
                    self.graph.remove_state(idx, !recur, !recur).into()
                }
                Command::MoveState(idx, parent, offset) => {
                    let mut ops = Vec::new();

                    if self.graph.parent(idx) != Some(parent) {
                        ops.extend(self.graph.set_parent(idx, parent));
                    }

                    let mut state = self.graph.state(idx).unwrap().clone();
                    state.rect = Rect::from_min_size(offset, state.rect.size());
                    ops.push(self.graph.update_state(idx, state));

                    ops.into()
                }
                Command::ResizeState(idx, delta) => {
                    let mut state = self.graph.state(idx).unwrap().clone();
                    state.rect = Rect::from_min_size(state.rect.min, state.rect.size() + delta);
                    self.graph.update_state(idx, state)
                }
                Command::UpdateState(idx, state) => self.graph.update_state(idx, state),
                Command::AddTransition(a, b, t) => self.graph.add_transition(a, b, t).into(),
                Command::UpdateTransition(tdx, t) => self.graph.update_transition(tdx, t),
                Command::MoveTransition(tdx, source, target) => {
                    let (a, b) = self.graph.endpoints(tdx).expect("endpoints");
                    self.graph
                        .move_transition(tdx, source.unwrap_or(a), target.unwrap_or(b))
                        .into()
                }
                Command::RemoveTransition(tdx) => self.graph.remove_transition(tdx),
                Command::UpdateSelection(selection) => {
                    // TODO undo?
                    self.selection = selection;
                    Op::Noop
                }
                Command::SetInitial(i, initial, data) => {
                    // TODO? we still have to update the state unless we stick InitialData into the edge
                    let state = self
                        .graph
                        .state(i)
                        .unwrap()
                        .clone()
                        .with_initial(Some(data));
                    let mut ops = vec![self.graph.update_state(i, state)];
                    ops.extend(self.graph.set_initial(i, Some(initial)));
                    ops.into()
                }
                Command::UnsetInitial(i) => {
                    let state = self.graph.state(i).unwrap().clone().with_initial(None);
                    let mut ops = vec![self.graph.update_state(i, state)];
                    ops.extend(self.graph.set_initial(i, None));
                    ops.into()
                }
                Command::StepInitial(i) => self
                    .graph
                    .set_initial(
                        i,
                        self.graph
                            .initial(i)
                            .map(|(initial, i)| (initial.step(), i)),
                    )
                    .into(),
                Command::SetInternal(i, internal) => self.graph.set_internal(i, internal),
                Command::SelectSourcePath(p) => {
                    // TODO undo?
                    match S::from_path(&p) {
                        Ok(s) => {
                            self.source = Some(s);
                        }
                        Err(e) => error!("error: {:?}", e),
                    }
                    Op::Noop
                }
                Command::UpdateSymbol(symbol, s) => match symbol {
                    SymbolId::Enter(i) => {
                        let state = self.graph.state(i).map(|state| state.clone().with_enter(s));
                        state
                            .map(|state| self.graph.update_state(i, state))
                            .unwrap_or_default()
                    }
                    SymbolId::Exit(i) => {
                        let state = self.graph.state(i).map(|state| state.clone().with_exit(s));
                        state
                            .map(|state| self.graph.update_state(i, state))
                            .unwrap_or_default()
                    }
                    SymbolId::Guard(i) => {
                        let t = self.graph.transition(i).map(|t| t.clone().with_guard(s));
                        t.map(|t| self.graph.update_transition(i, t))
                            .unwrap_or_default()
                    }
                },
                _ => {
                    error!("unhandled command: {:?}", c);
                    Op::Noop
                }
            };

            match op {
                Op::Noop => (),
                _ => self.add_undo(op),
            }
        }
    }

    pub fn show(&self, mut edit_data: &mut EditData, home_dir: Option<&Path>, ui: &mut Ui) {
        // Toggle debug.
        let focus = ui.memory(|m| m.focused());
        if focus.is_none() && ui.input(|i| i.key_pressed(Key::D)) {
            let d = !ui.ctx().debug_on_hover();
            ui.ctx().set_debug_on_hover(d);
            ui.style_mut().debug.show_widget_hits = true;
            ui.style_mut().debug.show_interactive_widgets = true;
            info!("debug_on_hover: {}", d);
        }

        // Search states, transitions, code?
        if let Submit::Result(idx) = edit_data.search.show(
            focus.is_none() && ui.input(|i| i.key_pressed(Key::S)),
            // Can't select the root state.
            self.graph.states().filter(|(i, _s)| *i != self.graph.root),
            ui.id(),
            ui,
        ) {
            edit_data
                .commands
                .push(Command::UpdateSelection(Selection::State(idx)));

            // Clear search on submit.
            edit_data.search.query.clear();
        }

        // Can we remove use of the rect in screen space?
        // let rect = ui.max_rect();
        // ui.allocate_rect(rect, Sense::hover());

        // Show root and recursively show children.

        //let frame = Frame::default().inner_margin(4.0);
        //let (_r, dropped_payload) = ui.dnd_drop_zone::<usize, ()>(frame, |ui| {
        // FIX: remove this
        let _drop_target = self.show_state(
            self.graph.root,
            ui.min_rect().min.to_vec2(),
            0,
            &mut edit_data,
            home_dir,
            ui,
        );
        // });
        // if let Some(dp) = dropped_payload {
        //     dbg!(dp);
        // }

        // Show the dragged state last, unclipped, if any.
        // TODO: initial still draws under this... draw with show_transitions?
        if let Some(drag) = DragAndDrop::payload::<Drag>(ui.ctx()) {
            match drag.as_ref() {
                Drag::State { idx, depth, .. } => {
                    let (parent_rect, ..) = self
                        .graph
                        .parent(*idx)
                        .and_then(|p| edit_data.rects.get(p))
                        .expect("dragged state parent rect exists");

                    _ = self.show_state(
                        *idx,
                        parent_rect.min.to_vec2(),
                        *depth,
                        &mut edit_data,
                        home_dir,
                        ui,
                    );
                }
                _ => (),
            }
        }

        edit_data.drag_transition = self.show_transitions(&mut edit_data, ui);

        // Invalidate max ports if anything has changed. TODO: optimize this -> why even clear? do
        // we reuse ids? remove deletions only?
        if !edit_data.commands.is_empty() {
            // TODO just clear ports?
            //edit_data.rects.0.clear();
        }
    }

    fn resolve_drag(
        &self,
        edit_data: &mut EditData,
        // TEMP
        drag_transition: Option<Transition>,
        ui: &mut Ui,
    ) {
        let Some(p) = ui.ctx().pointer_interact_pos() else {
            return;
        };

        let Some(target) = self.drag_target(edit_data, self.graph.root, ui.dragged_idx(), p) else {
            return;
        };

        let commands = &mut edit_data.commands;

        // Resolve drag. ui.input().pointer.primary_released() doesn't work?
        if ui.input(|i| i.pointer.any_released()) {
            // Only clear drags that target states. The rest are resolved later.
            match std::mem::take(&mut edit_data.drag) {
                //_ if !drag.min_drag(ui) => (),
                drag @ Drag::State { .. } => {
                    error!("drag nowhere: {drag:?}");
                    // if let Some(p) = ui.ctx().pointer_interact_pos() {
                    //     // Place the state relative to the parent with the offset.
                    //     let p = p - parent_root_offset - offset;
                    //     commands.push(Command::MoveState(src, target, p))
                    // }
                }
                Drag::AddTransition(a, Some(b)) => match drag_transition {
                    Some(t) => commands.push(Command::AddTransition(a, b, t)),
                    None => {
                        warn!("no drag_transition!");
                    }
                },
                Drag::TransitionSource(b, Some(a), tdx)
                | Drag::TransitionTarget(a, Some(b), tdx) => {
                    if let Some((a0, b0)) = self.graph.endpoints(tdx) {
                        // Check if changed. We were doing this inside drag_transition to avoid
                        // looking up endpoints twice...
                        if a != a0 || b != b0 {
                            match drag_transition {
                                Some(t) => {
                                    commands.push(Command::UpdateTransition(tdx, t));

                                    commands.push(Command::MoveTransition(
                                        tdx,
                                        (a != a0).then_some(a),
                                        (b != b0).then_some(b),
                                    ))
                                }
                                None => {
                                    warn!("no drag_transition!");
                                }
                            }
                        }
                    } else {
                        warn!("no endpoints!");
                    }
                }
                Drag::Initial(i, Some((target, port)), (c1, c2)) => {
                    let initial = self
                        .graph
                        .initial(i)
                        .map(|(initial, _)| (initial, target))
                        .unwrap_or_else(|| (Initial::Initial, target));
                    commands.push(Command::SetInitial(i, initial, (port, c1, c2)))
                }
                // put it back for now TEMP
                drag @ _ => edit_data.drag = drag,
            }

            if let Some(drag) = DragAndDrop::payload::<Drag>(ui.ctx()) {
                match drag.as_ref() {
                    // A state can't be dragged to itself, hence the index check. TODO: we already
                    // checked this?
                    Drag::State {
                        idx: i,
                        offset,
                        press_origin,
                        ..
                    } if *i != target => {
                        if let Some(p) = ui.ctx().pointer_interact_pos() {
                            // Since the transition id is free-floating and no longer relative to
                            // the source and target, we need to move all the enclosed
                            // transitions. TODO: handle inside MoveState? or make a transaction
                            let drag_offset = *press_origin - p;
                            for (i, e) in self
                                .graph
                                .enclosed_edges(*i)
                                .filter_map(|i| self.graph.transition(i).map(|e| (i, e)))
                            {
                                edit_data.commands.push(Command::UpdateTransition(
                                    i,
                                    e.clone().translate(-drag_offset),
                                ));
                            }

                            // Get the target rect in screen-space.
                            if let Some(target_rect) = edit_data.rects.get_rect(target) {
                                edit_data.commands.push(Command::MoveState(
                                    *i,
                                    target,
                                    // Find new position relative to target (parent) including
                                    // pointer offset.
                                    p - target_rect.min.to_vec2() - *offset,
                                ));
                            }

                            DragAndDrop::clear_payload(ui.ctx());
                        }
                    }
                    _ => (),
                }
            }
        }
    }

    // Use the previous frame's rect data (clipped, in screen-space) to find the front-most
    // descendant (hovered). Discluding the dragged state.
    fn drag_target(
        &self,
        edit_data: &EditData,
        idx: Idx,
        dragged_idx: Option<Idx>,
        p: Pos2,
    ) -> Option<Idx> {
        if let Some(r) = edit_data.rects.get_rect(idx) {
            if r.contains(p) {
                return self
                    .graph
                    .children_rev(idx)
                    .filter_map(|(child, _state)| (Some(child) != dragged_idx).then_some(child))
                    .find_map(|child| self.drag_target(edit_data, child, dragged_idx, p))
                    .or(Some(idx));
            }
        }
        None
    }

    /// Create a new transition from a to b, derived from an original transition t0, along with
    /// start and end positions.
    pub fn drag_transition(
        &self,
        a: Idx,
        b: Idx,
        rects: &Rects,
        t0: Tdx,
    ) -> (Transition, (Pos2, Pos2)) {
        let is_self = a == b;

        // We don't use the index so just pass this in?
        let (t0, (a0, b0)) = self
            .graph
            .transition(t0)
            .zip(self.graph.endpoints(t0))
            .unwrap();

        let mut t = t0.clone();

        // New ports. Self-transitions take up two outgoing ports - which is a weird special case,
        // but it's visually distinctive and looks better than looping completely around the state.
        let ports = if a != a0 {
            let port1 = self.free_port(a, Direction::Outgoing);
            if is_self {
                // Reassign b to outgoing too.
                (
                    port1,
                    self.free_port_from(a, Direction::Outgoing, port1 + 1),
                )
            } else {
                (port1, t0.port2)
            }
        } else if b != b0 {
            // We are only ever changing a or b, never both?
            (t0.port1, self.free_port(b, target_dir(a, b)))
        } else {
            (t0.port1, t0.port2)
        };

        t.port1 = ports.0;
        t.port2 = ports.1;

        let (start, end) = rects
            .get(a)
            .map(|r| port_out(r, ports.0))
            .zip(rects.get(b).map(|r| port_pos(r, ports.1, target_dir(a, b))))
            .unwrap();

        // Reset control points?
        if is_self != (a0 == b0) {
            let cp = if is_self {
                approx_cp_self(start, end)
            } else {
                approx_cp(start, end)
            };
            t.c1 = cp.0;
            t.c2 = cp.1;
        };

        //(t != *t0).then_some((t, (start, end)))
        (t, (start, end))
    }

    pub fn new_transition(&self, a: Idx, b: Idx, rects: &Rects) -> (Transition, (Pos2, Pos2)) {
        // TEMP label
        let id = format!(
            "{} > {}",
            self.graph.state(a).map(|s| s.id.as_str()).unwrap_or("?"),
            self.graph.state(b).map(|s| s.id.as_str()).unwrap_or("?"),
        );

        let is_self = a == b;
        let ports = if is_self {
            self.free_port2(a, Direction::Outgoing)
        } else {
            (
                self.free_port(a, Direction::Outgoing),
                self.free_port(b, Direction::Incoming),
            )
        };

        let (start, end) = rects
            .get(a)
            .map(|r| port_out(r, ports.0))
            .zip(rects.get(b).map(|r| port_pos(r, ports.1, target_dir(a, b))))
            .unwrap();

        // Reset control points?
        let cp = if is_self {
            approx_cp_self(start, end)
        } else {
            approx_cp(start, end)
        };

        (
            Transition {
                id,
                guard: None,
                pos: start + ((cp.0 + cp.1) * 0.5),
                c1: cp.0,
                c2: cp.1,
                port1: ports.0,
                port2: ports.1,
            },
            (start, end),
        )
    }

    /// Shows all transitions.
    pub fn show_transitions(&self, edit_data: &mut EditData, ui: &mut Ui) -> Option<Transition> {
        let mut drag_transition = None;

        // Offset enclosed transitions if dragging a state.
        let dragged_state = ui.dragged_idx_offset();

        for (tdx, source, target, t, internal) in self.graph.transitions() {
            match edit_data.drag {
                Drag::TransitionSource(b, a, _tdx) if _tdx == tdx => match a {
                    Some(a) => {
                        let (t, (start, end)) = self.drag_transition(a, b, &edit_data.rects, tdx);
                        self.show_connection(
                            start,
                            end,
                            Connection::Transition(tdx, &t, internal, Vec2::ZERO),
                            edit_data,
                            ui,
                        );
                        drag_transition = Some(t);
                    }
                    None => {
                        if let Some(start) = ui
                            .ctx()
                            .pointer_interact_pos()
                            .or_else(|| edit_data.rects.get(source).map(|r| port_out(r, t.port1)))
                        {
                            if let Some(end) = edit_data
                                .rects
                                .get(target)
                                .map(|r| port_pos(r, t.port2, target_dir(source, target)))
                            {
                                self.show_connection(
                                    start,
                                    end,
                                    Connection::Transition(tdx, t, internal, Vec2::ZERO),
                                    edit_data,
                                    ui,
                                );
                            }
                        }
                    }
                },
                Drag::TransitionTarget(a, b, _tdx) if _tdx == tdx => match b {
                    Some(b) => {
                        let (t, (start, end)) = self.drag_transition(a, b, &edit_data.rects, tdx);
                        self.show_connection(
                            start,
                            end,
                            Connection::Transition(tdx, &t, internal, Vec2::ZERO),
                            edit_data,
                            ui,
                        );
                        drag_transition = Some(t);
                    }
                    None => {
                        if let Some(start) =
                            edit_data.rects.get(source).map(|r| port_out(r, t.port1))
                        {
                            if let Some(end) = ui.ctx().pointer_interact_pos().or_else(|| {
                                edit_data
                                    .rects
                                    .get(target)
                                    .map(|r| port_pos(r, t.port2, target_dir(source, target)))
                            }) {
                                self.show_connection(
                                    start,
                                    end,
                                    Connection::Transition(tdx, t, internal, Vec2::ZERO),
                                    edit_data,
                                    ui,
                                );
                            }
                        }
                    }
                },
                _ => {
                    if let Some(start) = edit_data.rects.get(source).map(|r| port_out(r, t.port1)) {
                        if let Some(end) = edit_data
                            .rects
                            .get(target)
                            .map(|r| port_pos(r, t.port2, target_dir(source, target)))
                        {
                            self.show_connection(
                                start,
                                end,
                                Connection::Transition(
                                    tdx,
                                    t,
                                    internal,
                                    dragged_state
                                        .filter(|(i, _)| self.graph.enclosed(*i, tdx))
                                        .map(|(_, offset)| offset)
                                        .unwrap_or_default(),
                                ),
                                edit_data,
                                ui,
                            );
                        }
                    }
                }
            };
        }

        // New transition in progress.
        if let Drag::AddTransition(source, target) = edit_data.drag {
            match target {
                Some(target) => {
                    let (t, (start, end)) = self.new_transition(source, target, &edit_data.rects);
                    self.show_connection(
                        start,
                        end,
                        Connection::DragTransition(t.c1, t.c2),
                        edit_data,
                        ui,
                    );
                    drag_transition = Some(t);
                }
                None => {
                    // Put the port in AddTransition?
                    let port1 = self.free_port(source, Direction::Outgoing);
                    if let Some(start) = edit_data.rects.get(source).map(|r| port_out(r, port1)) {
                        if let Some(end) = ui.ctx().pointer_interact_pos() {
                            let cp = approx_cp(start, end);
                            self.show_connection(
                                start,
                                end,
                                Connection::DragTransition(cp.0, cp.1),
                                edit_data,
                                ui,
                            );
                        }
                    }
                }
            }
        }

        drag_transition
    }

    pub fn show_resize(&self, id: Id, rect: Rect, ui: &mut Ui) -> Response {
        let resize_size = Vec2::splat(ui.visuals().resize_corner_size);
        let resize_rect = Rect::from_min_size(rect.max - resize_size, resize_size);
        let resize_response = ui.interact(resize_rect, id.with("resize"), Sense::drag());
        paint_resize_corner(ui, &resize_response);
        resize_response
    }

    /// Returns the drop target, which is ourself or descendant that contains the pointer.
    pub fn show_state(
        &self,
        idx: Idx,
        // Parent rect min. FIX: this is just ui.min_rect().min?
        offset: Vec2,
        depth: usize,
        edit_data: &mut EditData,
        home_dir: Option<&Path>,
        ui: &mut Ui,
    ) {
        //let offset = ui.min_rect().min.to_vec2();
        let state = self.graph.state(idx).unwrap();
        let root = idx == self.graph.root;
        let interact_pos = ui.ctx().pointer_interact_pos();

        let mut rect = state.rect.translate(offset);

        if let Some(d) = DragAndDrop::payload::<Drag>(ui.ctx()) {
            match d.as_ref() {
                Drag::State {
                    idx: i,
                    press_origin,
                    ..
                } if *i == idx => {
                    // Missing from x11 or the default cursor theme?
                    //ui.ctx().set_cursor_icon(CursorIcon::Grabbing);

                    if let Some(p) = interact_pos {
                        rect = rect.translate(-(*press_origin - p))
                    } else {
                        warn!("no interact_pos with drag");
                    }
                }
                _ => (),
            }
        }

        match edit_data.drag {
            // Use all available space for the root. TODO: move this out?
            _ if root => rect = state.rect.intersect(ui.max_rect()),

            Drag::Resize(i, delta) if i == idx => {
                // Include the offset and the resize delta.
                rect = Rect::from_min_size(state.rect.min + offset, state.rect.size() + delta);
            }
            _ => (),
        };

        // Write rect. Transitions use these to find ports, so clip them to the parent. We don't
        // clip the rect outright because we don't want anything wrapping or otherwise changing,
        // just clipping.
        edit_data
            .rects
            .insert_rect(idx, rect.intersect(ui.clip_rect()), || {
                (
                    self.max_port(idx, Direction::Incoming),
                    self.max_port(idx, Direction::Outgoing),
                )
            });

        // This means offscreen states won't get updated, but that shouldn't matter?
        if !ui.is_rect_visible(rect) {
            return;
        }

        // FIX use drop target
        let is_target = edit_data.drag.is_target(idx);

        let parent_clip_rect = ui.clip_rect();

        let child_states_and_header = |ui: &mut Ui| {
            // Do we need to shrink based on the frame stoke still? TODO
            ui.set_clip_rect(ui.max_rect().intersect(parent_clip_rect));

            // Set drag target first and let child states override. TODO remove me
            self.set_drag_target(idx, depth, &mut edit_data.drag, ui);

            // We can't use dragged_id because we don't know the child's id yet.
            let dragged_idx = ui.dragged_idx();

            // Show child states.
            for child in self
                .graph
                .children(idx)
                // The dragged child will be shown last from the root, so we skip it here.
                .filter(|child| Some(*child) != dragged_idx)
            {
                self.show_state(
                    child,
                    rect.min.to_vec2(),
                    depth + 1,
                    edit_data,
                    home_dir,
                    ui,
                )
            }

            // Show the header and resize after child states so they interact first. So does this
            // mean the allocated rects for child states don't take up space? Something is taking up
            // space in the parent ui and throwing the available space off. This only ever worked
            // because nothing was taking up space.
            let header_inset = Vec2::splat(4.0);
            //let header_rect = ui.available_rect_before_wrap().shrink2(header_inset);
            let header_rect = rect.shrink2(header_inset);
            let header_response = ui
                .allocate_ui_at_rect(header_rect, |ui| {
                    self.show_header(idx, root, state, edit_data, home_dir, ui);
                })
                .response;
            let header_rect = header_response.rect.expand2(header_inset);

            // Resize. Cannot resize the root state. Only show the resize if the pointer is in the rect,
            // or we are currently resizing it (since the pointer can be outside while dragging - it's a
            // frame behind?). TODO: this should be hovered, not contained
            if !root && (edit_data.drag.resizing(idx) || ui.rect_contains_pointer(rect)) {
                let response = self.show_resize(ui.id(), rect, ui);

                let drag = &mut edit_data.drag;

                drag_delta!(
                    (response, ui, drag, Resize, idx),
                    |delta: &mut Vec2| {
                        // Find the minimum delta such that we don't resize smaller than the header size
                        // (including inset). Limit max?
                        let size = state.rect.size();
                        *delta = (header_rect.size() - size).max(*delta + response.drag_delta())
                    },
                    |delta: &Vec2| edit_data.commands.push(Command::ResizeState(idx, *delta))
                );
            }

            // This fills the space so we can interact with the background.
            _ = ui.allocate_space(ui.available_size_before_wrap());

            // Can't drag the root state.
            ui.interact_bg(if !root {
                Sense::click_and_drag()
            } else {
                Sense::click()
            })
        };

        // TODO: state_ui?
        let state_response = {
            // There is no "allocate_ui_at_rect_with_id"... We don't even need the
            // `allocate_ui_at_rect` since we're positioning the contents manually and it calls
            // `child_ui_with_id_source` itself.
            let mut ui = ui.child_ui_with_id_source(rect, *ui.layout(), idx);

            widget::state::state_frame(
                root,
                depth,
                matches!(self.selection, Selection::State(i) if i == idx),
                is_target,
            )
            .show(&mut ui, child_states_and_header)
        };

        // ui.ctx()
        //     .debug_painter()
        //     .debug_rect(ui.min_rect(), Color32::DEBUG_COLOR, "min_rect");

        // Can't select root.
        if state_response.clicked() {
            edit_data.commands.push(Command::UpdateSelection(if root {
                Selection::None
            } else {
                Selection::State(idx)
            }))
        }

        // this never happens now
        if ui.input(|i| i.pointer.any_released()) && state_response.dragged() {
            warn!("nope");
        }

        // New drags. We should not get a drag here if a drag is started in a child state, but we
        // check `in_drag` anyway.
        if state_response.drag_started_by(PointerButton::Primary) && !edit_data.drag.in_drag() {
            if let Some(p) = interact_pos {
                if ui.input(|i| i.modifiers.shift) {
                    // New transition, drag to target.
                    //let port1 = self.free_port(idx, transit::Direction::Outgoing);
                    edit_data.drag =
                        // free_port always returns a port locator?
                        Drag::AddTransition(idx, None);
                } else {
                    // Save the pointer offset from the state origin. Zero is never correct for the
                    // parent offest, so maybe it should be an Option like target?

                    // response.dnd_set_drag_payload does not use drag_started_by nor checks
                    // modifiers
                    DragAndDrop::set_payload(
                        ui.ctx(),
                        Drag::State {
                            idx,
                            offset: p - rect.min,
                            depth,
                            press_origin: p,
                        },
                    );
                    //edit_data.drag = Drag::State(idx, p - rect.min, None, Vec2::ZERO);
                }
            }
            // ResponseExt? we can't check the response since it may be dragged with
        }

        // The payload is automatically cleared at the end of the frame if the
        // pointer is released. We want to continue drawing this frame as if the
        // state is still being dragged. Otherwise we will get flickering
        // transitions, drawn without the offset (until they are updated next
        // frame).

        // We don't update anything each frame with the drag delta since we want
        // to undo the whole drag. Perhaps we should revisit this by only
        // recording history by diffing the state at drag start and stop.

        // Context menu on right click.
        if !edit_data.drag.in_drag() {
            // debug ports:
            // let state_response = state_response.on_hover_text_at_pointer(format!(
            //     "incoming ports: {:?} outgoing ports: {:?}",
            //     self.ports(idx, Direction::Incoming),
            //     self.ports(idx, Direction::Outgoing)
            // ));

            _ = state_response.context_menu(|ui| {
                ui.label(format!("{} ({})", state.id, idx.index()));
                if ui.button("Add state").clicked() {
                    // Position is the original click, relative to parent.
                    let p = ui.min_rect().min - rect.min.to_vec2();
                    edit_data.commands.push(Command::AddState(idx, p));
                    ui.close_menu();
                }
                // Can't remove the root state.
                if idx != self.graph.root {
                    if ui.button("Remove state").clicked() {
                        edit_data.commands.push(Command::RemoveState(idx, false));
                        ui.close_menu();
                    }
                    if ui.button("Remove state (recursive)").clicked() {
                        edit_data.commands.push(Command::RemoveState(idx, true));
                        ui.close_menu();
                    }
                }
            });
        }
    }

    // Collapsable? How do we redirect incoming transitions to child states?
    pub fn show_header(
        &self,
        idx: Idx,
        root: bool,
        state: &State,
        edit_data: &mut EditData,
        home_dir: Option<&Path>,
        ui: &mut Ui,
    ) -> InnerResponse<()> {
        ui.horizontal(|ui| {
            // Initial first, with less spacing between the initial control and the history indicator.
            ui.horizontal(|ui| {
                ui.spacing_mut().item_spacing.x = 2.0;

                let initial_size = Vec2::splat(6.0);
                let response = ui.allocate_response(initial_size, Sense::click_and_drag());
                let initial = self.graph.initial(idx);

                let rect = Rect::from_center_size(response.rect.center(), initial_size);
                let start = rect.center();

                // Left click cycles initial type. Right click clears. Drag sets new initial.
                if response.clicked() {
                    edit_data.commands.push(Command::StepInitial(idx))
                } else if response.clicked_by(PointerButton::Secondary) {
                    edit_data.commands.push(Command::UnsetInitial(idx))
                }

                let show_initial = match edit_data.drag {
                    Drag::None if response.drag_started() => {
                        edit_data.drag = Drag::Initial(idx, None, Default::default());
                        true
                    }
                    Drag::Initial(_idx, target, ref mut cp) if _idx == idx => {
                        // Reuse existing port if dragging to existing target?
                        if let Some(end) = target
                            .and_then(|(t, p)| edit_data.rects.get(t).zip(Some(p)))
                            .map(|(r, p)| port_in(r, p))
                            .or_else(|| ui.ctx().pointer_interact_pos())
                        {
                            // bias the initial down
                            *cp = approx_cp_down(start, end);
                            self.show_connection(
                                start,
                                end,
                                Connection::DragInitial(idx, *cp),
                                edit_data,
                                ui,
                            );
                        }
                        true
                    }
                    _ => {
                        if let Some((i, (port, c1, c2))) =
                            initial.map(|(_, i)| i).zip(state.initial)
                        {
                            if let Some(end) = edit_data.rects.get(i).map(|r| port_in(r, port)) {
                                self.show_connection(
                                    start,
                                    end,
                                    Connection::Initial(idx, (c1, c2)),
                                    edit_data,
                                    ui,
                                );
                            }
                            true
                        } else {
                            false
                        }
                    }
                };

                let color = ui.style().interact(&response).fg_stroke.color;
                let color = ecolor::tint_color_towards(color, Color32::YELLOW);
                ui.painter().circle(
                    rect.center(),
                    rect.size().max_elem() * 0.5,
                    if show_initial {
                        color
                    } else {
                        Color32::TRANSPARENT
                    },
                    Stroke::new(2.0, color),
                );

                // Initial type.
                if let Some((initial, _)) = initial {
                    match initial {
                        Initial::HistoryShallow => {
                            ui.colored_label(color, "h");
                        }
                        Initial::HistoryDeep => {
                            ui.colored_label(color, "h*");
                        }
                        _ => (),
                    }
                }
            }); // end initial

            // Show id.
            let InnerResponse { inner, response } = Editabel::new().show(&state.id, ui);

            // Clicking the label selects the state.
            if response.clicked() {
                edit_data.commands.push(Command::UpdateSelection(if root {
                    Selection::None
                } else {
                    Selection::State(idx)
                }))
            }

            // Update id.
            if let Some(id) = inner {
                edit_data
                    .commands
                    .push(Command::UpdateState(idx, state.clone().with_id(id)))
            }

            self.show_symbol(SymbolId::Enter(idx), &state.enter, edit_data, ui);
            self.show_symbol(SymbolId::Exit(idx), &state.exit, edit_data, ui);

            // Show source file in the root state. Maybe this should be left click to goto, right
            // click to set, like symbols.
            if root {
                let response = ui.small_button("source");
                if response.clicked() {
                    let dialog = native_dialog::FileDialog::new();
                    let dialog = match self.source.as_ref() {
                        Some(source) => {
                            let dialog = match source.path().parent() {
                                Some(p) => dialog.set_location(p),
                                None => dialog,
                            };
                            dialog.add_filter(source.description(), source.extensions())
                        }
                        None => dialog,
                    };

                    if let Ok(Some(p)) = dialog.show_open_single_file() {
                        edit_data.commands.push(Command::SelectSourcePath(p));
                    }
                }
                if let Some(path) = &self.source.as_ref().map(|s| s.path()) {
                    // I like tildes.
                    #[cfg(target_os = "linux")]
                    let path = if let Some(path) = home_dir.and_then(|d| path.strip_prefix(d).ok())
                    {
                        PathBuf::from("~/").join(path)
                    } else {
                        path.to_path_buf()
                    };
                    response.on_hover_text_at_pointer(path.display().to_string());
                }
            }
        })
    }

    // TODO: display "left click to goto symbol (if set) or insert based on path id" and "right
    // click to search for a symbol" somewhere
    // TODO: validation for symbols if things are renamed in the source
    pub fn show_symbol(
        &self,
        id: SymbolId,
        symbol: &Symbol,
        edit_data: &mut EditData,
        ui: &mut Ui,
    ) -> Response {
        let gensym = symbol
            .as_ref()
            .map(Cow::from)
            .unwrap_or_else(|| self.generate_symbol(id).into());

        let location = self
            .source
            .as_ref()
            .and_then(|source| source.symbol(&gensym));

        // Hovering displays symbol name and source location. This API is insane just to bold one word...
        let hover_text = {
            let style = Style::default();
            let mut job = LayoutJob::default();
            match location {
                Some((path, line, col)) => {
                    RichText::new(gensym.as_str())
                        // TODO this is not bold
                        .color(style.visuals.strong_text_color())
                        .append_to(&mut job, &style, FontSelection::Default, Align::Center);

                    RichText::new(format!(
                        "{} ({}:{}:{})",
                        // If the symbol is not set but the location for the generated symbol exists show "?".
                        if symbol.is_some() { "" } else { "?" },
                        // Do we even need the path since it's always the same?
                        path.file_name().and_then(|f| f.to_str()).unwrap_or("-"),
                        line,
                        col
                    ))
                    .append_to(
                        &mut job,
                        &style,
                        FontSelection::Default,
                        Align::Center,
                    );
                }
                None => RichText::new(format!("{}?", gensym)).append_to(
                    &mut job,
                    &style,
                    FontSelection::Default,
                    Align::Center,
                ),
            }

            job
        };

        let button = Button::new(match id {
            SymbolId::Enter(_) => "enter",
            SymbolId::Exit(_) => "exit",
            SymbolId::Guard(_) => "guard",
        })
        .small();

        // If the symbol is set and the location exists, highlight the button.
        let button = if symbol.is_some() && location.is_some() {
            let color = ecolor::tint_color_towards(
                ui.style().visuals.widgets.inactive.fg_stroke.color,
                Color32::YELLOW,
            );
            button.stroke((1.0, color))
        } else if symbol.is_some() {
            // Show a warning if the symbol is set but doesn't exist yet.
            button.stroke((1.0, ui.style().visuals.warn_fg_color)) //Color32::TEMPORARY_COLOR
        } else {
            button
        };

        // Disabled when no source.
        let response = ui
            .add_enabled(self.source.is_some(), button)
            .on_hover_text(hover_text)
            .on_disabled_hover_text("source file is unset");

        if response.clicked() {
            // enabled -> clicked -> source exists
            let source = self.source.as_ref().expect("source");
            let symbol = source.normalize_symbol(&gensym);

            match location {
                // We could use references here if source existed outside self. If `commands`
                // references self we can't get a mutable ref later to process them.
                Some((path, line, col)) => edit_data.commands.push(Command::GotoSymbol(
                    symbol,
                    path.to_path_buf(),
                    (*line, *col),
                )),
                // A symbol is set but doesn't exist in the source so insert it.
                None => {
                    let template = source.insert_template().replace("{}", &symbol);
                    edit_data.commands.push(Command::InsertSymbol(
                        symbol,
                        source.path().to_path_buf(),
                        template,
                    ));
                }
            }
        }

        // Show search and/or focus on right click.
        let set_focus = if response.clicked_by(PointerButton::Secondary) {
            // Move to response right top.
            let symbols = &mut edit_data.symbols;
            symbols.position = Some(response.rect.right_top() + vec2(4.0, 0.0));

            // Set query to the current symbol (if any).
            symbols.query = gensym.to_string();
            symbols.results = None;
            true
        } else {
            false
        };

        // Show search box.
        if let Some(symbols) = self
            .source
            .as_ref()
            // We just want the keys.
            .map(|source| source.symbols().map(|(s, _)| s))
        {
            match edit_data.symbols.show(set_focus, symbols, response.id, ui) {
                Submit::Query(s) | Submit::Result(s) => {
                    // Clear action/guard if the submit is empty.
                    edit_data
                        .commands
                        .push(Command::UpdateSymbol(id, (!s.is_empty()).then_some(s)));
                }
                _ => (),
            }
        }

        response
    }

    pub fn path_string(&self, i: Idx) -> String {
        self.graph
            .path(i)
            .into_iter()
            // Always skip the root id? We already know it's a door.
            .skip(1)
            .filter_map(|i| self.graph.state(i).map(|s| &s.id))
            .cloned()
            .collect::<Vec<_>>()
            .join("-")
    }

    pub fn generate_symbol(&self, id: SymbolId) -> String {
        match id {
            SymbolId::Enter(i) => self.path_string(i) + "-enter",
            SymbolId::Exit(i) => self.path_string(i) + "-exit",
            SymbolId::Guard(t) => {
                self.graph
                    .transition(t)
                    .zip(self.graph.endpoints(t))
                    .map(|(t, (a, _b))| self.path_string(a) + "-" + &t.id)
                    .unwrap_or_else(|| "???".into())
                    + "-guard"
            }
        }
    }

    /// Each state sets the drag target for its children. The topmost child that contains the
    /// pointer becomes the target. This is similiar to egui's builtin handling of overlapping
    /// interactions. We can't set the target inside each child recursively because they aren't
    /// aware of siblings or their ordering. In order to reduce the amount of work done here that is
    /// thrown away we could recurse from the root once before drawing anything.
    pub fn set_drag_target(&self, idx: Idx, depth: usize, drag: &mut Drag, ui: &Ui) {
        // FIX: return drag target?

        // let Some(p) = ui.ctx().pointer_interact_pos() else {
        //     return;
        // };

        // These are the same...
        //dbg!(&rect, ui.max_rect());
        let rect = ui.max_rect();
        let parent_offset = rect.min.to_vec2();

        // For transition endpoints only.
        let dir = match drag {
            Drag::TransitionSource(..) => Direction::Outgoing,
            //Drag::TransitionTarget(..) => transit::Direction::Incoming,
            _ => Direction::Incoming,
        };

        match drag {
            // We can't drag into ourselves.
            // Drag::State(i, ..) if *i == idx => (),
            // Drag::State(dragged_idx, _drag_offset, target, offset) => {
            //     // Root clears the target by setting itself initially (any state can be dragged to
            //     // root).
            //     if depth == 0 {
            //         *target = Some((idx, depth));
            //         *offset = parent_offset;
            //     }

            //     if let Some((t, new_offset)) = self.graph.children_rev(idx).find_map(|(i, s)| {
            //         let child_rect = s.state.rect.translate(parent_offset).intersect(rect);

            //         // The drag target cannot be in the path of the dragged state (cycles). This
            //         // never happens in practice since the dragged states are moving with the
            //         // pointer.
            //         (!self.graph.in_path(*dragged_idx, i) && child_rect.contains(p)).then(|| {
            //             // ui.ctx().debug_painter().debug_rect(child_rect, Color32::GREEN, format!("{:?}", p - child_rect.min));
            //             (i, child_rect.min.to_vec2())
            //         })
            //     }) {
            //         *target = Some((t, depth + 1));
            //         *offset = new_offset;
            //     }
            // }
            Drag::TransitionSource(_, target, ..)
            | Drag::TransitionTarget(_, target, ..)
            | Drag::AddTransition(_, target, ..) => {
                if depth == 0 {
                    // No transition can target the root.
                    *target = None;
                }

                if let Some((i, _)) = self
                    .graph
                    .children_rev(idx)
                    // TODO Ui::rect_contains_pointer takes into account layer and clipping, make
                    // sure we keep this
                    .find(|(_i, s)| ui.rect_contains_pointer(s.state.rect.translate(parent_offset)))
                {
                    *target = Some(i)
                }
            }
            Drag::Initial(initial_idx, target, _) => {
                // Initial must be a child state.
                if depth == 0 || *initial_idx == idx {
                    *target = None;
                }
                // Make a flag in show_state to avoid checking this for every state?
                if self.graph.in_path(*initial_idx, idx) {
                    if let Some((i, _)) = self.graph.children_rev(idx).find(|(_i, s)| {
                        // TODO see above
                        ui.rect_contains_pointer(s.state.rect.translate(parent_offset))
                    }) {
                        // Find a free port.
                        *target = Some((i, self.free_port(i, dir)))
                    }
                }
            }
            _ => (),
        };
    }

    /// Returns a list of ports in use for the specified state and direction.
    pub fn ports(&self, idx: Idx, direction: Direction) -> Vec<usize> {
        let ports = self.graph.state_transitions(idx, direction);

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
                        self.graph
                            .path_iter(idx)
                            .filter(|i| self.graph.initial(*i).map(|(_, j)| j) == Some(idx))
                            .filter_map(|i| {
                                self.graph.state(i).and_then(|s| s.initial).map(|i| i.0)
                            }),
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

    // TODO clicking the curve for selection? https://github.com/emilk/egui/discussions/1959
    pub fn show_connection(
        &self,
        start: Pos2,
        end: Pos2,
        conn: Connection,
        edit_data: &mut EditData,
        ui: &mut Ui,
    ) {
        let selected = match conn {
            Connection::Transition(tdx, ..) => {
                matches!(self.selection, Selection::Transition(i) if i == tdx)
            }
            Connection::Initial(idx, ..) => {
                matches!(self.selection, Selection::State(i) if i == idx)
            }
            _ => false,
        };

        let mut ui = ui.child_ui_with_id_source(
            ui.max_rect(),
            *ui.layout(),
            match conn {
                Connection::Initial(src, ..) => Id::new(src).with("initial"),
                Connection::DragInitial(..) => ui.id().with("drag_initial"),
                Connection::Transition(tdx, ..) => Id::new(tdx),
                Connection::DragTransition(..) => ui.id().with("drag_transition"),
            },
        );

        let control_size = if selected { 8.0 } else { 6.0 };
        let control_size_sq = Vec2::splat(control_size);

        let color = if selected {
            ui.style().visuals.selection.stroke.color
        } else {
            ui.style().visuals.widgets.active.fg_stroke.color
        };

        // Make drag/selected more visible?
        // TODO highlight incoming and outgoing transitions on hover/selected?
        // TODO it would be nice if we could make this a gradient so as to make the direction more clear
        let stroke = Stroke::new(1.2, color);

        let cubic = |points| {
            CubicBezierShape::from_points_stroke(points, false, Color32::TRANSPARENT, stroke)
        };

        match conn {
            Connection::Transition(tdx, t, internal, drag_offset) => {
                // Include drag in control points and position.
                let (c1, t_pos, c2) = match edit_data.drag {
                    Drag::TransitionId(i, delta) if i == tdx => (t.c1, t.pos + delta, t.c2),
                    Drag::TransitionControl((i, cp), delta) if i == tdx => match cp {
                        ControlPoint::C1 => (t.c1 + delta, t.pos, t.c2),
                        ControlPoint::C2 => (t.c1, t.pos, t.c2 + delta),
                    },
                    _ => (t.c1, t.pos - drag_offset, t.c2),
                };

                // Control points are relative to the start and end, respectively.
                let c1 = start + c1;
                let c2 = end + c2;

                // Pass these in?
                let endpoints = self.graph.endpoints(tdx).unwrap();

                // Show source control. Maybe only if selected?
                let source_rect = Rect::from_center_size(start, control_size_sq);
                if ui.is_rect_visible(source_rect) {
                    let response = ui.allocate_rect(source_rect, Sense::drag());

                    match edit_data.drag {
                        Drag::None if response.drag_started() => {
                            edit_data.drag = Drag::TransitionSource(endpoints.1, None, tdx)
                        }

                        _ => (),
                    }

                    let color = ui.style().interact(&response).fg_stroke.color;
                    ui.painter().circle_filled(start, control_size * 0.5, color);
                }

                // Show target control.
                let target_rect = Rect::from_center_size(end, control_size_sq);
                if ui.is_rect_visible(target_rect) {
                    let response = ui.allocate_rect(target_rect, Sense::drag());

                    match edit_data.drag {
                        Drag::None if response.drag_started() => {
                            edit_data.drag = Drag::TransitionTarget(endpoints.0, None, tdx)
                        }
                        _ => (),
                    }

                    let color = ui.style().interact(&response).fg_stroke.color;

                    let mut mesh = arrow(control_size, color);
                    mesh.translate(end.to_vec2());
                    ui.painter().add(mesh);
                }

                // Show control points if selected.
                if selected {
                    for (cp, p) in [(ControlPoint::C1, c1), (ControlPoint::C2, c2)] {
                        let response = self.show_control_point(
                            Rect::from_center_size(p, control_size_sq),
                            &mut ui,
                        );

                        let drag = &mut edit_data.drag;
                        drag_delta!(
                            (response, ui, drag, TransitionControl, (tdx, cp)),
                            |delta: &mut Vec2| {
                                let mut t = t.clone();
                                match cp {
                                    ControlPoint::C1 => t.c1 += *delta,
                                    ControlPoint::C2 => t.c2 += *delta,
                                }
                                edit_data.commands.push(Command::UpdateTransition(tdx, t))
                            }
                        );
                    }
                }

                // This doesn't work because the label doesn't always end up with the midpoint.
                // quad_beziers_from_points(&[start, c1, t_pos, c2, end]).for_each(|points| {
                //     let bezier = QuadraticBezierShape::from_points_stroke(
                //         points,
                //         false,
                //         Color32::TRANSPARENT,
                //         stroke,
                //     );
                //     ui.painter().add(bezier);
                // });

                // `pos` is left/top (rect min), relative to root, so we have to include the ui min rect
                let h = ui.style().spacing.interact_size.y;
                let rect = Rect::from_min_size(
                    ui.min_rect().min + t_pos.to_vec2(), // - vec2(0.0, h / 2.0),
                    // What width?
                    Vec2::new(128.0, h),
                );

                // Show id, guard, internal...
                let response = ui.allocate_ui_at_rect(rect, |ui| {
                    let _response = ui
                        .horizontal(|ui| {
                            // ui.label("🗝");

                            // HACK: If the id is empty we need something to click on to change it...
                            let tid = if t.id.is_empty() { "untitled" } else { &t.id };
                            let InnerResponse { inner, response } =
                                Editabel::sense(Sense::click_and_drag()).show(tid, ui);
                            if let Some(id) = inner {
                                edit_data
                                    .commands
                                    .push(Command::UpdateTransition(tdx, t.clone().with_id(id)))
                            }

                            self.show_symbol(SymbolId::Guard(tdx), &t.guard, edit_data, ui);

                            // Self-transition, internal checkbox.
                            if endpoints.0 == endpoints.1 {
                                let mut internal = internal;
                                if ui.checkbox(&mut internal, "int.").clicked() {
                                    edit_data.commands.push(Command::SetInternal(tdx, internal));
                                }
                            }

                            // Context menu on right click.
                            if !edit_data.drag.in_drag() {
                                // debug ports:
                                // let response = response.on_hover_text_at_pointer(format!(
                                //     "port1: {} port2: {}",
                                //     t.port1, t.port2
                                // ));
                                _ = response.context_menu(|ui| {
                                    if ui.button("Remove transition").clicked() {
                                        edit_data.commands.push(Command::RemoveTransition(tdx));
                                        ui.close_menu();
                                    }
                                })
                            }

                            if response.double_clicked() {
                                dbg!("double");
                            } else {
                                if response.clicked() {
                                    edit_data
                                        .commands
                                        .push(Command::UpdateSelection(Selection::Transition(tdx)))
                                }

                                let drag = &mut edit_data.drag;
                                drag_delta!(
                                    (response, ui, drag, TransitionId, tdx),
                                    |delta: &Vec2| {
                                        let mut t = t.clone();
                                        t.pos = t.pos + *delta;
                                        edit_data.commands.push(Command::UpdateTransition(tdx, t))
                                    }
                                );
                            }
                        })
                        .response;
                });

                // Draw curve(s) last because we need the min response rect.
                let rect_offset = vec2(4.0, 0.0);
                let left = rect.left_center() - rect_offset;
                let right = response.response.rect.right_center() + rect_offset;

                // Flipping don't work.
                if start.x < end.x {
                    ui.painter().add(cubic([start, c1, left - CP_OFFSET, left]));
                    ui.painter().add(cubic([right, right + CP_OFFSET, c2, end]));
                } else {
                    // If the transition is moving right to left swap left and right.
                    ui.painter()
                        .add(cubic([start, c1, right + CP_OFFSET, right]));
                    ui.painter().add(cubic([left, left - CP_OFFSET, c2, end]));
                }
            }
            Connection::Initial(i, (c1, c2)) => {
                // Include drag.
                let (c1, c2) = match edit_data.drag {
                    Drag::InitialControl((idx, cp), delta) if idx == i => match cp {
                        ControlPoint::C1 => (c1 + delta, c2),
                        ControlPoint::C2 => (c1, c2 + delta),
                    },
                    _ => (c1, c2),
                };

                let c1 = start + c1;
                let c2 = end + c2;

                ui.painter().add(cubic([start, c1, c2, end]));

                let mut mesh = arrow(control_size, color);
                mesh.translate(end.to_vec2());
                ui.painter().add(mesh);

                // Show control points if selected.
                if selected {
                    for (cp, p) in [(ControlPoint::C1, c1), (ControlPoint::C2, c2)] {
                        let response = self.show_control_point(
                            Rect::from_center_size(p, control_size_sq),
                            &mut ui,
                        );

                        let drag = &mut edit_data.drag;
                        drag_delta!(
                            (response, ui, drag, InitialControl, (i, cp)),
                            |delta: &Vec2| {
                                if let Some(state) = self.graph.state(i) {
                                    let mut state = state.clone();
                                    if let Some(initial) = &mut state.initial {
                                        match cp {
                                            ControlPoint::C1 => initial.1 += *delta,
                                            ControlPoint::C2 => initial.2 += *delta,
                                        }
                                        edit_data.commands.push(Command::UpdateState(i, state))
                                    }
                                }
                            }
                        );
                    }
                }

                // Show target control.
                let target_rect = Rect::from_center_size(end, control_size_sq);
                if ui.is_rect_visible(target_rect) {
                    let response = ui.allocate_rect(target_rect, Sense::drag());

                    match edit_data.drag {
                        Drag::None if response.drag_started() => {
                            edit_data.drag = Drag::Initial(i, None, Default::default());
                        }
                        _ => (),
                    }

                    let color = ui.style().interact(&response).fg_stroke.color;

                    let mut mesh = arrow(control_size, color);
                    mesh.translate(end.to_vec2());
                    ui.painter().add(mesh);
                }
            }
            Connection::DragTransition(c1, c2) | Connection::DragInitial(_, (c1, c2)) => {
                ui.painter().add(cubic([start, start + c1, end + c2, end]));
            }
        }
    }

    pub fn show_control_point(&self, rect: Rect, ui: &mut Ui) -> Response {
        let response = ui.allocate_rect(rect, Sense::drag());

        // Ignore selection since its always selected?
        let wv = ui.style().interact(
            &response,
            // match self.selection {
            //     Selection::Transition(_tdx) if tdx == _tdx => true,
            //     _ => false,
            // },
        );

        ui.painter().circle_filled(
            rect.center(),
            rect.size().min_elem() * 0.5,
            wv.fg_stroke.color,
        );

        response
    }
}

pub fn arrow(size: f32, color: Color32) -> Mesh {
    let mut mesh = Mesh::default();

    mesh.add_triangle(0, 1, 2);

    let rect = Rect::from_center_size(pos2(0.0, 0.0), Vec2::splat(size));

    mesh.vertices.push(Vertex {
        pos: rect.left_top(),
        color,
        ..Default::default()
    });
    mesh.vertices.push(Vertex {
        pos: rect.right_center(),
        color,
        ..Default::default()
    });
    mesh.vertices.push(Vertex {
        pos: rect.left_bottom(),
        color,
        ..Default::default()
    });
    mesh
}

// For self-transitions: target takes up an outgoing port.
#[inline]
fn target_dir(a: Idx, b: Idx) -> Direction {
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
pub fn port_pos(rect: (Rect, MaxPorts), port: usize, direction: Direction) -> Pos2 {
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

pub fn port_in(rect: (Rect, MaxPorts), port: usize) -> Pos2 {
    port_pos(rect, port, Direction::Incoming)
}

pub fn port_out(rect: (Rect, MaxPorts), port: usize) -> Pos2 {
    port_pos(rect, port, Direction::Outgoing)
}

impl State {
    const DEFAULT_SIZE: Vec2 = vec2(256.0, 64.0);

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

fn midpoint(a: Pos2, b: Pos2) -> Pos2 {
    (a + b.to_vec2()) * 0.5
}

pub fn quad_beziers_from_points(points: &[Pos2]) -> impl Iterator<Item = [Pos2; 3]> + '_ {
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

trait UiExt {
    fn dragged_idx(&self) -> Option<Idx>;
    fn dragged_idx_offset(&self) -> Option<(Idx, Vec2)>;
    fn drag_offset(&self) -> Option<Vec2>;
}

impl UiExt for Ui {
    /// Returns dragged state's index.
    fn dragged_idx(&self) -> Option<Idx> {
        DragAndDrop::payload::<Drag>(self.ctx()).and_then(|d| match d.as_ref() {
            Drag::State { idx, .. } => Some(*idx),
            _ => None,
        })
    }

    /// Returns dragged state's (index, drag offset).
    fn dragged_idx_offset(&self) -> Option<(Idx, Vec2)> {
        let ctx = self.ctx();
        let p = ctx.pointer_interact_pos();
        DragAndDrop::payload::<Drag>(ctx)
            .zip(p)
            .and_then(|(d, p)| match d.as_ref() {
                Drag::State {
                    idx, press_origin, ..
                } => Some((*idx, *press_origin - p)),
                _ => None,
            })
    }

    /// This is pretty useless because the press origin goes away once the pointer is released, so
    /// we can't use it to get the offset on release. We have to store it somewhere ourselves.
    fn drag_offset(&self) -> Option<Vec2> {
        self.ctx().input(|i| {
            i.pointer
                .press_origin()
                .zip(i.pointer.latest_pos())
                .map(|(a, b)| a - b)
        })
    }
}
