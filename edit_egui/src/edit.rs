mod app;
mod command;
mod drag;
mod editabel;
mod editor;
mod search;
mod watch;
mod widget;

use std::borrow::Cow;
use std::path::{Path, PathBuf};

pub use app::*;
use command::*;
use drag::*;
use editabel::Editabel;
pub use editor::*;
// TODO: only depend on egui here (need to get rid of eframe-dynamic)
use crate::*;
use eframe::egui;
use egui::{
    epaint::{text::LayoutJob, CubicBezierShape, Vertex},
    *,
};
use search::{SearchBox, Submit};
use tracing::{error, info, warn};
use transit_graph::{Direction, Graph, Idx, Initial, Tdx};
pub use watch::*;

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
                    // egui handles dragging vs clicking now, but we still check the delta
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

// Store rect in root-space, and incoming/outgoing max ports. Rename?
#[derive(Default)]
pub struct Rects(nohash_hasher::IntMap<usize, (Rect, MaxPorts)>);

impl Rects {
    fn from_graph(graph: &Graph<State, Transition>) -> Self {
        let mut rects = Self::default();
        fn insert(i: Idx, graph: &Graph<State, Transition>, parent_rect: &Rect, rects: &mut Rects) {
            // Translate to root-space.
            let rect = graph.graph[i]
                .state
                .rect
                .translate(parent_rect.min.to_vec2());
            rects.insert_rect(i, rect, |_| Default::default());
            for child in graph.children(i) {
                insert(child, graph, &rect, rects);
            }
        }
        insert(graph.root, graph, &Rect::ZERO, &mut rects);
        rects
    }

    pub fn get(&self, idx: Idx) -> Option<&(Rect, MaxPorts)> {
        self.0.get(&idx.index())
    }

    fn port_pos(&self, i: Idx, port: usize, direction: Direction) -> Option<Pos2> {
        self.get(i).map(|r| port_pos(r, port, direction))
    }

    fn port_in(&self, i: Idx, port: usize) -> Option<Pos2> {
        self.port_pos(i, port, Direction::Incoming)
    }

    fn port_out(&self, i: Idx, port: usize) -> Option<Pos2> {
        self.port_pos(i, port, Direction::Outgoing)
    }

    pub fn get_rect(&self, idx: Idx) -> Option<Rect> {
        self.get(idx).map(|a| a.0)
    }

    fn insert_rect<F: FnOnce(Idx) -> MaxPorts>(&mut self, idx: Idx, rect: Rect, f: F) {
        self.0
            .entry(idx.index())
            .and_modify(|a| a.0 = rect)
            .or_insert_with(|| (rect, f(idx)));
    }

    /// Recursively finds a screen-space rect and ports, inserting intermediate results as it goes.
    /// This mimics what show_state does, but for off-screen or clipped states.
    fn get_cached_rect(&mut self, root: Idx, graph: &EditGraph, i: Idx) -> (Rect, MaxPorts) {
        // we can't recurse with the entry api
        if let Some(cached) = self.get(i) {
            cached.clone()
        } else {
            // the root is always cached from show_states
            assert_ne!(i, root, "no cached screen-space rect for root");

            let parent = self.get_cached_rect(root, graph, graph.parent(i).expect("parent exists"));

            // find state rect in screen-space, translate by the parent min
            let rect = graph
                .state(i)
                .expect("state exists")
                .rect
                // TODO state frame margins
                .translate(parent.0.min.to_vec2());
            let max_ports = graph.max_ports(i);
            self.0.insert(i.index(), (rect, max_ports));
            (rect, max_ports)
        }
    }

    // TODO: unused?
    #[allow(unused)]
    fn insert_max_port(&mut self, idx: Idx, direction: Direction, max_port: Option<usize>) {
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
pub struct EditData {
    root_rect: Rect,
    rects: Rects,
    /// Current drag state.
    drag: Drag,
    // TEMP
    drag_transition: Option<Transition>,
    commands: Vec<Command>,
    search: SearchBox<Idx>,
    symbols: SearchBox<String>,
}

impl Default for EditData {
    fn default() -> Self {
        Self {
            root_rect: Rect::ZERO, // ?
            rects: Default::default(),
            drag: Default::default(),
            drag_transition: None,
            commands: Default::default(),
            search: Default::default(),
            symbols: Default::default(),
        }
    }
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

pub fn approx_cp(_start: Pos2, _end: Pos2) -> (Vec2, Vec2) {
    // (start.to_vec2() + CP_OFFSET, end.to_vec2() - CP_OFFSET)
    (CP_OFFSET, -CP_OFFSET)
}

/// This is for state initial connections. Initial states are always down and right since the start
/// is near the upper left.
pub fn approx_cp_down(start: Pos2, end: Pos2) -> (Vec2, Vec2) {
    let d = end - start;
    // Try to keep the control point inside the source/parent rect.
    (CP_OFFSET.yx(), vec2(-CP_OFFSET.x.min(d.x), CP_OFFSET.y))
}

pub fn approx_cp_self(_start: Pos2, _end: Pos2) -> (Vec2, Vec2) {
    (vec2(128.0, -96.0), vec2(128.0, 96.0))
}

impl<S> Edit<S>
where
    S: Source,
{
    /// Load from path and validate.
    #[cfg(feature = "serde")]
    pub fn load_and_validate(path: impl AsRef<Path>) -> Result<Self, Error>
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

    /// Returns the narrowed state, or root if None.
    fn root(&self) -> Idx {
        self.narrow.unwrap_or(self.graph.root)
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
            self.graph.states().filter(|(i, _s)| *i != self.root()),
            ui.id(),
            ui,
        ) {
            edit_data
                .commands
                .push(Command::UpdateSelection(Selection::State(idx)));

            // Clear search on submit.
            edit_data.search.query.clear();
        }

        let offset = ui.min_rect().min.to_vec2();

        // The root rect auto-sizes to include its children. This is the scrollable area.
        // TODO: this should include transitions too
        let root_rect = self.graph.children_rect(self.root()).translate(offset);

        // Add a small amount of buffer space.
        let root_rect = Rect::from_min_max(root_rect.min, root_rect.max + Vec2::splat(64.0));

        // Clamp to zero since egui doesn't work with negative scroll offsets. TODO: force states
        // into the root_rect when dragging?
        let root_rect = root_rect.intersect(Rect::from_min_max(ui.min_rect().min, root_rect.max));

        // Expand to the available space (this might not be needed with auto-shrink off).
        let root_rect = root_rect.union(ui.max_rect());

        edit_data.root_rect = root_rect;

        // Show root and recursively show children.
        self.show_state(self.root(), offset, 0, &mut edit_data, home_dir, ui);

        // Show the dragged state last, unclipped, if any.
        // TODO: initial still draws under this... draw with show_transitions?
        match &edit_data.drag {
            &Drag::State { source, depth, .. } => {
                let parent_rect = self
                    .graph
                    .parent(source)
                    .and_then(|p| edit_data.rects.get_rect(p))
                    .expect("dragged state parent rect exists");

                _ = self.show_state(
                    source,
                    parent_rect.min.to_vec2(),
                    depth, // TODO match target rather than source?
                    &mut edit_data,
                    home_dir,
                    ui,
                );
            }
            _ => (),
        }

        edit_data.drag_transition = self.show_drag_transition(&mut edit_data, ui);

        // Invalidate rects if anything has changed. TODO: optimize this
        if !edit_data.commands.is_empty() {
            edit_data.rects.0.clear();
        }
    }

    /// Create a new transition from a to b, derived from an original transition t0, along with
    /// start and end positions.
    pub fn drag_transition(
        &self,
        a: Idx,
        b: Idx,
        rects: &Rects,
        t0: &Transition,
        (a0, b0): (Idx, Idx),
    ) -> (Transition, (Pos2, Pos2)) {
        let is_self = a == b;

        let mut t = t0.clone();

        // New ports. Self-transitions take up two outgoing ports - which is a weird special case,
        // but it's visually distinctive and looks better than looping completely around the state.
        let ports = if a != a0 {
            let port1 = self.graph.free_port(a, Direction::Outgoing);
            if is_self {
                // Reassign b to outgoing too.
                (
                    port1,
                    self.graph.free_port_from(a, Direction::Outgoing, port1 + 1),
                )
            } else {
                (port1, t0.port2)
            }
        } else if b != b0 {
            // We are only ever changing a or b, never both?
            (t0.port1, self.graph.free_port(b, target_dir(a, b)))
        } else {
            (t0.port1, t0.port2)
        };

        t.port1 = ports.0;
        t.port2 = ports.1;

        let (start, end) = rects
            .port_out(a, ports.0)
            .zip(rects.port_pos(b, ports.1, target_dir(a, b)))
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
            self.graph.free_port2(a, Direction::Outgoing)
        } else {
            (
                self.graph.free_port(a, Direction::Outgoing),
                self.graph.free_port(b, Direction::Incoming),
            )
        };

        let (start, end) = rects
            .port_out(a, ports.0)
            .zip(rects.port_pos(b, ports.1, target_dir(a, b)))
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
                // This is in screen-space which is wrong, we adjust it when the drag is resolved.
                pos: midpoint(start, end),
                c1: cp.0,
                c2: cp.1,
                port1: ports.0,
                port2: ports.1,
            },
            (start, end),
        )
    }

    /// Shows the current drag transition.
    pub fn show_drag_transition(
        &self,
        edit_data: &mut EditData,
        ui: &mut Ui,
    ) -> Option<Transition> {
        match edit_data.drag {
            // these two are the same with start/end reversed?
            Drag::TransitionSource(b, a, tdx) => {
                let (t, (source, target), internal) = self.transition(tdx)?;

                match a {
                    Some(a) => {
                        let (t, (start, end)) =
                            // FIX: how is this different from new_transition?
                            self.drag_transition(a, b, &edit_data.rects, t, (source, target));
                        self.show_connection(
                            start,
                            end,
                            Connection::Transition(tdx, &t, internal, Vec2::ZERO),
                            edit_data,
                            ui,
                        );
                        return Some(t);
                    }
                    None => {
                        if let Some(start) = ui
                            .ctx()
                            .pointer_interact_pos()
                            .or_else(|| edit_data.rects.port_out(source, t.port1))
                        {
                            if let Some(end) = edit_data.rects.port_pos(
                                target,
                                t.port2,
                                target_dir(source, target),
                            ) {
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
                }
            }
            Drag::TransitionTarget(a, b, tdx) => {
                let (t, (source, target), internal) = self.transition(tdx)?;

                match b {
                    Some(b) => {
                        let (t, (start, end)) =
                            self.drag_transition(a, b, &edit_data.rects, t, (source, target));
                        self.show_connection(
                            start,
                            end,
                            Connection::Transition(tdx, &t, internal, Vec2::ZERO),
                            edit_data,
                            ui,
                        );
                        return Some(t);
                    }
                    None => {
                        if let Some(start) = edit_data.rects.port_out(source, t.port1) {
                            if let Some(end) = ui.ctx().pointer_interact_pos().or_else(|| {
                                edit_data.rects.port_pos(
                                    target,
                                    t.port2,
                                    target_dir(source, target),
                                )
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
                }
            }
            _ => (),
        };

        // FIX: merge these?

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
                    return Some(t);
                }
                None => {
                    // Put the port in AddTransition?
                    let port1 = self.graph.free_port(source, Direction::Outgoing);
                    if let Some(start) = edit_data.rects.port_out(source, port1) {
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

        None
    }

    // Get transition ref, endpoints, and internal flag. TODO this should be elsewhere?
    fn transition(&self, i: Tdx) -> Option<(&Transition, (Idx, Idx), bool)> {
        Some((
            self.graph.transition(i)?,
            self.graph.endpoints(i)?,
            self.graph.is_internal(i),
        ))
    }

    pub fn show_transition(&self, edit_data: &mut EditData, i: Tdx, ui: &mut Ui) {
        let Some((t, (source, target), is_internal)) = self.transition(i) else {
            return;
        };

        // Offset enclosed transitions if dragging a state. Pass this in?
        let dragged_state = match &edit_data.drag {
            Drag::State {
                source,
                press_origin,
                ..
            } => ui
                .ctx()
                .pointer_interact_pos()
                .map(|p| (*source, *press_origin - p)),
            _ => None,
        };

        // If the state/rect is off-screen and a child of the drag state we need to add the drag
        // offset ourselves. FIX: this does not work
        let offset = |idx, rect| {
            dragged_state
                .and_then(|(drag_idx, offset)| {
                    (!ui.is_rect_visible(rect) && self.graph.in_path(drag_idx, idx))
                        .then_some(offset)
                })
                .unwrap_or_default()
        };

        // Find (and cache) transition endpoints even if the source and/or target
        // weren't shown this frame.
        let rect = edit_data
            .rects
            .get_cached_rect(self.root(), &self.graph, source);
        let start = port_pos(&rect, t.port1, Direction::Outgoing) - offset(source, rect.0);

        let rect = edit_data
            .rects
            .get_cached_rect(self.root(), &self.graph, target);
        let end = port_pos(&rect, t.port2, target_dir(source, target)) - offset(target, rect.0);

        self.show_connection(
            start,
            end,
            Connection::Transition(
                i,
                t,
                is_internal,
                dragged_state
                    .filter(|(idx, _)| self.graph.enclosed(*idx, i))
                    .map(|(_, offset)| offset)
                    .unwrap_or_default(),
            ),
            edit_data,
            ui,
        );
    }

    pub fn show_resize(&self, ui: &mut Ui) -> Response {
        let resize_size = Vec2::splat(ui.visuals().resize_corner_size);
        let resize_rect = Rect::from_min_size(ui.max_rect().max - resize_size, resize_size);
        let resize_response = ui.interact(resize_rect, ui.id().with("resize"), Sense::drag());
        if ui.is_rect_visible(resize_rect) {
            paint_resize_corner(ui, &resize_response);
        }
        resize_response
    }

    /// Show a state and its descendants recursively.
    pub fn show_state(
        &self,
        idx: Idx,
        // Parent rect min in screen-space. This is not the same as ui.min_rect().min for the root
        // state which is constrained by the scroll area viewport.
        offset: Vec2,
        depth: usize,
        edit_data: &mut EditData,
        home_dir: Option<&Path>,
        ui: &mut Ui, // parent_ui
    ) {
        let state = self.graph.state(idx).unwrap();
        let root = idx == self.root();
        let interact_pos = ui.ctx().pointer_interact_pos();

        let rect = if root {
            edit_data.root_rect
        } else {
            let rect = state.rect.translate(offset);

            match edit_data.drag {
                Drag::State {
                    source,
                    press_origin,
                    ..
                } if source == idx => {
                    // Missing from x11 or the default cursor theme?
                    //ui.ctx().set_cursor_icon(CursorIcon::Grabbing);

                    if let Some(p) = interact_pos {
                        rect.translate(-(press_origin - p))
                    } else {
                        warn!("no interact_pos with drag");
                        rect
                    }
                }

                Drag::Resize(i, delta) if i == idx => {
                    Rect::from_min_max(rect.min, rect.max + delta)
                }
                _ => rect,
            }
        };

        // This means offscreen states won't get updated, but that shouldn't matter?
        if !ui.is_rect_visible(rect) {
            return;
        }

        let clipped_rect = rect.intersect(ui.clip_rect());

        // Write rect. Transitions use these to find ports, so clip them to the parent. We don't
        // clip the rect outright because we don't want anything wrapping or otherwise changing,
        // just clipping.
        edit_data.rects.insert_rect(
            idx,
            // We want edit_data.root_rect to be a shortcut for (and identical to) the rect stored.
            if root { rect } else { clipped_rect },
            |idx| self.graph.max_ports(idx),
        );

        let is_target = edit_data.drag.is_target(idx);
        let is_dragging = edit_data.drag.is_dragging(idx);

        let child_states_and_header = |ui: &mut Ui| {
            // We are inside the state frame, which is already clipped with the margin, but we still
            // need to intersect with the parent. (Except for the root.)
            if !root {
                ui.set_clip_rect(ui.clip_rect().intersect(clipped_rect));
            }

            // We can't use dragged_id because we don't know the child's id yet.
            let dragged_idx = edit_data.drag.dragged_idx();

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

            // replace this with a frame?
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
            if !root
                && (edit_data.drag.resizing(idx)
                    // Unless we're resizing this state, don't show when dragging.
                    || (ui.rect_contains_pointer(rect) && !edit_data.drag.in_drag()))
            {
                let response = self.show_resize(ui);

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

            // Show all transitions we are the common ancestor for. Filter out the dragged
            // transition since it's drawn later.
            // TODO cache this list
            let dragged_tdx = edit_data.drag.dragged_tdx();
            for t in self.graph.transition_indices().filter(|&t| {
                self.graph.transition_common_ancestor(t) == Some(idx) && dragged_tdx != Some(t)
            }) {
                //warn!(t = t.index(), ca = state.id);
                self.show_transition(edit_data, t, ui)
            }

            // This fills the rest of the frame so we can interact with the background.
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
            let mut child_ui = ui.child_ui_with_id_source(rect, *ui.layout(), idx);
            let InnerResponse { inner, response: _ } = widget::state::state_frame(
                root,
                depth,
                matches!(self.selection, Selection::State(i) if i == idx),
                is_target,
                is_dragging,
            )
            .show(&mut child_ui, child_states_and_header);

            if root {
                _ = ui.allocate_rect(child_ui.min_rect(), Sense::hover());
            }

            inner
        };

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
                edit_data.drag = if ui.input(|i| i.modifiers.shift) {
                    // New transition, drag to target.
                    //let port1 = self.free_port(idx, transit::Direction::Outgoing);

                    // free_port always returns a port locator?
                    Drag::AddTransition(idx, None)
                } else {
                    // Save the pointer offset from the state origin. Zero is never correct for the
                    // parent offest, so maybe it should be an Option like target?
                    Drag::State {
                        source: idx,
                        target: None,
                        offset: p - rect.min,
                        depth,
                        press_origin: p,
                    }
                }
            }
            // ResponseExt? we can't check the response since it may be dragged with
        }

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
                if idx != self.root() {
                    if ui.button("Remove state").clicked() {
                        edit_data.commands.push(Command::RemoveState(idx, false));
                        ui.close_menu();
                    }
                    if ui.button("Remove state (recursive)").clicked() {
                        edit_data.commands.push(Command::RemoveState(idx, true));
                        ui.close_menu();
                    }
                }

                // No reason to narrow to root.
                if self.narrow != Some(idx) && idx != self.graph.root {
                    if ui.button("Narrow here").clicked() {
                        edit_data.commands.push(Command::SetNarrow(Some(idx)));
                        ui.close_menu();
                    }
                }

                if self.narrow.is_some() {
                    if ui.button("Widen").clicked() {
                        edit_data.commands.push(Command::SetNarrow(None));
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
                            .and_then(|(t, p)| edit_data.rects.port_in(t, p))
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
                            if let Some(end) = edit_data.rects.port_in(i, port) {
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

            // Show state id.
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
            .unwrap_or_else(|| self.graph.generate_symbol(id).into());

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
        let source_rect = Rect::from_center_size(start, control_size_sq);
        let target_rect = Rect::from_center_size(end, control_size_sq);

        let color = if selected {
            ui.style().visuals.selection.stroke.color
        } else {
            ui.style().visuals.widgets.active.fg_stroke.color
        };

        // Make drag/selected more visible?
        // TODO highlight incoming and outgoing transitions on hover/selected?
        // TODO it would be nice if we could make this a gradient so as to make the direction more clear
        let stroke = Stroke::new(1.2, color);

        let cubic = |points, ui: &mut Ui| {
            let curve =
                CubicBezierShape::from_points_stroke(points, false, Color32::TRANSPARENT, stroke);
            if ui.is_rect_visible(curve.visual_bounding_rect()) {
                ui.painter().add(curve);
            }
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

                // Control points are relative to the start and end, respectively. This puts them in
                // screen-space.
                let c1 = start + c1;
                let c2 = end + c2;

                // Pass these in?
                let endpoints = self.graph.endpoints(tdx).unwrap();

                // Show source control. Maybe only if selected?
                if self
                    .show_endpoint(true, source_rect, &mut ui)
                    .drag_started()
                {
                    assert!(!edit_data.drag.in_drag());
                    edit_data.drag = Drag::TransitionSource(endpoints.1, None, tdx)
                }

                // Show target control.
                if self
                    .show_endpoint(false, target_rect, &mut ui)
                    .drag_started()
                {
                    assert!(!edit_data.drag.in_drag());
                    edit_data.drag = Drag::TransitionTarget(endpoints.0, None, tdx);
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

                let h = ui.style().spacing.interact_size.y;
                let rect = Rect::from_min_size(
                    // `t_pos` is left/top (rect min), relative to root.
                    edit_data.root_rect.min + t_pos.to_vec2(),
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
                                    ui.label(format!("{} ({})", t.id, tdx.index()));
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

                // If the transition is moving right to left swap left and right.
                for pts in if start.x < end.x {
                    [
                        [start, c1, left - CP_OFFSET, left],
                        [right, right + CP_OFFSET, c2, end],
                    ]
                } else {
                    [
                        [start, c1, right + CP_OFFSET, right],
                        [left, left - CP_OFFSET, c2, end],
                    ]
                } {
                    cubic(pts, &mut ui);
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

                cubic([start, c1, c2, end], &mut ui);

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
                if self
                    .show_endpoint(false, target_rect, &mut ui)
                    .drag_started()
                {
                    assert!(!edit_data.drag.in_drag());
                    edit_data.drag = Drag::Initial(i, None, Default::default());
                }
            }
            Connection::DragTransition(c1, c2) | Connection::DragInitial(_, (c1, c2)) => {
                // Initial does not need this and self-transitions look weird. TODO?
                //self.show_endpoint(true, source_rect, &mut ui);
                self.show_endpoint(false, target_rect, &mut ui);
                cubic([start, start + c1, end + c2, end], &mut ui);
            }
        }
    }

    pub fn show_endpoint(&self, is_source: bool, rect: Rect, ui: &mut Ui) -> Response {
        let response = ui.allocate_rect(rect, Sense::drag());

        if ui.is_rect_visible(rect) {
            let color = ui.style().interact(&response).fg_stroke.color;

            if is_source {
                ui.painter()
                    .circle_filled(rect.center(), rect.width() * 0.5, color);
            } else {
                // TODO this should be flipped for a self-transition
                let mesh = arrow(rect, color);
                //mesh.translate(end.to_vec2());
                ui.painter().add(mesh);
            }
        }
        response
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

pub fn arrow(rect: Rect, color: Color32) -> Mesh {
    let mut mesh = Mesh::default();

    mesh.add_triangle(0, 1, 2);

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

trait UiExt {
    fn drag_offset(&self) -> Option<Vec2>;
}

impl UiExt for Ui {
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
