use std::sync::{Arc, Mutex};

// TODO: make a separte crate for the bin and only depend on egui here
use editabel::Editabel;
use eframe::egui::epaint::{CubicBezierShape, Vertex};
use eframe::egui::*;
use eframe::epaint::RectShape;
use transit::{ExportError, ImportError};

mod editabel;

#[cfg_attr(feature = "serde", derive(serde::Deserialize, serde::Serialize))]
#[derive(Debug, Default, Clone)]
pub enum Selection {
    #[default]
    None,
    State(transit::Idx),
    Transition(transit::Tdx),
}

#[cfg_attr(feature = "serde", derive(serde::Deserialize, serde::Serialize))]
#[derive(Default)]
pub struct Statechart<C: transit::Context> {
    pub path: Option<std::path::PathBuf>,
    //statechart: transit::Statechart<C>,
    #[serde(skip)]
    pub graph: transit::Graph<C>,
    #[serde(default)]
    pub selection: Selection,
}

// Initial (destination) port and control points. Similiar to transition.
pub type Initial = (usize, Vec2, Vec2);

#[cfg_attr(feature = "serde", derive(serde::Deserialize, serde::Serialize))]
#[derive(Clone, Debug)]
pub struct State {
    id: String,
    // These needs to be pulled from a contained struct.
    //enter: String,
    //exit: String,
    /// Rect relative to parent.
    rect: Rect,
    initial: Option<Initial>,
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
            rect: Rect::from_min_size(pos2(0.0, 0.0), Vec2::INFINITY),
            initial: None,
            collapsed: false,
            pan: Vec2::ZERO,
            zoom: 1.0,
        }
    }
}

#[cfg_attr(feature = "serde", derive(serde::Deserialize, serde::Serialize))]
#[derive(Clone, Debug, Default)]
pub struct Transition {
    // event? guard name?
    #[serde(default)]
    id: String,
    c1: Vec2,
    c2: Vec2,
    // Source port index.
    port1: usize,
    /// Destination port index.
    port2: usize,
}

#[cfg_attr(feature = "serde", derive(serde::Deserialize, serde::Serialize))]
#[derive(Clone, Default)]
pub struct EditContext {}

impl transit::Context for EditContext {
    type Event = ();
    type State = State;
    type Transition = Transition;
}

impl transit::State<EditContext> for State {
    fn enter(
        &mut self,
        _ctx: &mut EditContext,
        _event: Option<&<EditContext as transit::Context>::Event>,
    ) {
        todo!()
    }

    fn exit(
        &mut self,
        _ctx: &mut EditContext,
        _event: Option<&<EditContext as transit::Context>::Event>,
    ) {
        todo!()
    }
}

impl transit::Transition<EditContext> for Transition {
    fn guard(
        &mut self,
        _ctx: &mut EditContext,
        _event: &<EditContext as transit::Context>::Event,
    ) -> bool {
        todo!()
    }
}

#[derive(Debug)]
pub enum Command {
    AddState(transit::Idx, Pos2),
    RemoveState(transit::Idx, bool),
    UpdateState(transit::Idx, State),
    /// Source, target, position (relative to parent), offset of the
    /// parent (relative to root).
    MoveState(transit::Idx, transit::Idx, Pos2),
    ResizeState(transit::Idx, Vec2),
    AddTransition(transit::Idx, transit::Idx, Transition),
    RemoveTransition,
    UpdateTransition(transit::Tdx, Transition),
    MoveTransition(transit::Tdx, Option<transit::Idx>, Option<transit::Idx>),
    SetInitial(transit::Idx, transit::Idx, Initial),
    UnsetInitial(transit::Idx),
    StepInitial(transit::Idx),
    SetEnter,
    SetExit,
    SetGuard,
    UpdateSelection(Selection),
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum ControlPoint {
    C1,
    C2,
}

// Drag target index and depth. Or incoming port?
pub type DragTarget = Option<(transit::Idx, usize)>;

#[derive(Clone, Debug, Default)]
pub enum Drag {
    #[default]
    None,
    State(transit::Idx, Vec2, DragTarget, Vec2),
    Resize(transit::Idx, Vec2),
    Initial(transit::Idx, DragTarget, (Vec2, Vec2)),
    // Remove transition here and use port1 only. Port2 in drag target.
    AddTransition(transit::Idx, Transition, DragTarget),
    TransitionSource(transit::Tdx, usize, DragTarget, transit::Idx),
    TransitionTarget(transit::Tdx, usize, DragTarget, transit::Idx),
    TransitionControl((transit::Tdx, ControlPoint), Vec2),
    TransitionId(transit::Tdx, Vec2),
}

impl Drag {
    pub fn sense(&self) -> Sense {
        match self {
            Drag::None => Sense::click_and_drag(),
            _ => Sense::hover(),
        }
    }

    pub fn in_drag(&self) -> bool {
        match self {
            Drag::None => false,
            _ => true,
        }
    }

    pub fn dragging(&self, idx: transit::Idx) -> bool {
        match self {
            Drag::State(i, ..) if *i == idx => true,
            _ => false,
        }
    }

    pub fn resizing(&self, idx: transit::Idx) -> bool {
        match self {
            Drag::Resize(i, ..) if *i == idx => true,
            _ => false,
        }
    }

    pub fn is_target(&self, idx: transit::Idx) -> bool {
        let target = match self {
            Drag::State(_, _, t, _)
            | Drag::Initial(_, t, ..)
            | Drag::AddTransition(_, _, t)
            | Drag::TransitionSource(_, _, t, _)
            | Drag::TransitionTarget(_, _, t, _) => t,
            _ => &None,
        };
        target.map(|(t, _)| t == idx).unwrap_or_default()
    }

    /// Is the drag greater than some minimum?
    pub fn min_drag(&self, ui: &Ui) -> bool {
        match self {
            Drag::None => false, // ?
            // Compare original press to the current pointer. This only works if the drag is in
            // progress, not after the pointer is released.
            Drag::AddTransition(..)
            | Drag::Initial(..)
            | Drag::State(..)
            | Drag::TransitionSource(..)
            | Drag::TransitionTarget(..) => {
                let p = &ui.input().pointer;
                p.press_origin()
                    .zip(p.interact_pos())
                    .map(|(p0, p1)| (p1 - p0).abs().max_elem() >= 1.0)
                    .unwrap_or_default()
            }
            Drag::Resize(_, d) | Drag::TransitionControl(_, d, ..) | Drag::TransitionId(_, d) => {
                d.abs().max_elem() >= 1.0
            }
        }
    }
}

// Deriving the variant and id from init failed...
// macro_rules! drag_id {
//     ($variant:ident($($field:tt)*,)) => {
//         $variant
//     };
// }

macro_rules! drag_delta {
    (($response:ident, $ui:ident, $drag:ident, $drag_ty:ident, $drag_id:expr), $init:expr, $update:expr, $end:expr) => {
        match $drag {
            Drag::None if $response.drag_started() => *$drag = $init,
            Drag::$drag_ty(_id, delta, ..) if *_id == $drag_id => {
                if $response.dragged() {
                    $update(delta)
                } else if $response.drag_released() {
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

pub type Rects = nohash_hasher::IntMap<usize, Rect>;

/// Mutable data, passed to each call of show_state.
#[derive(Default)]
pub struct EditData {
    rects: Rects,
    drag: Drag,
    commands: Vec<Command>,
}

pub enum Connection<'a, 'b> {
    // Source, control points.
    Initial(transit::Idx, (Vec2, Vec2)),
    DragInitial,
    Transition(transit::Tdx, &'a Transition, bool, &'b mut Drag),
    DragTransition(&'a Transition),
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

#[inline]
pub fn approx_cp(start: Pos2, end: Pos2) -> (Vec2, Vec2) {
    let d = (end - start) * 0.5;
    (vec2(d.x, -d.y), vec2(-d.x, d.y))
}

impl Statechart<EditContext> {
    /// Id of the root state.
    pub fn id(&self) -> &str {
        &self.graph.root().id
    }

    pub fn load(&mut self) -> Result<(), ImportError> {
        if let Some(path) = &self.path {
            self.graph = transit::Graph::import_from_file(&path)?;
        }

        // Validate selection.
        if match self.selection {
            Selection::State(idx) => self.graph.state(idx).is_none(),
            Selection::Transition(tdx) => self.graph.transition(tdx).is_none(),
            _ => false,
        } {
            self.selection = Selection::None
        }

        Ok(())
    }

    pub fn save(&mut self) -> Result<(), ExportError> {
        if let Some(path) = &self.path {
            self.graph.export_to_file(&path)?;
        }
        Ok(())
    }

    pub fn process_commands(&mut self, commands: Vec<Command>) {
        for c in commands {
            //dbg!(&c);
            match c {
                Command::AddState(parent, p) => {
                    self.graph.add_state(
                        State {
                            id: "untitled".into(),
                            rect: Rect::from_min_size(p, State::DEFAULT_SIZE),
                            ..Default::default()
                        },
                        Some(parent),
                    );
                }
                Command::RemoveState(idx, rec) => self.graph.remove_state(idx, !rec, !rec),
                Command::MoveState(idx, parent, offset) => {
                    if let Some(state) = self.graph.state(idx) {
                        let mut state = state.clone();
                        state.rect = Rect::from_min_size(offset, state.rect.size());

                        // TODO: merge undos
                        if self.graph.parent(idx) != Some(parent) {
                            self.graph.set_parent(idx, Some(parent));
                        }
                        self.graph.update_state(idx, state);
                    }
                }
                Command::ResizeState(idx, delta) => {
                    if let Some(state) = self.graph.state(idx) {
                        let mut state = state.clone();
                        state.rect = Rect::from_min_size(state.rect.min, state.rect.size() + delta);
                        self.graph.update_state(idx, state);
                    }
                }
                Command::UpdateState(idx, state) => self.graph.update_state(idx, state),
                Command::AddTransition(a, b, t) => {
                    self.graph.add_transition(a, b, t);
                }
                Command::UpdateTransition(tdx, t) => {
                    self.graph.update_transition(tdx, t);
                }
                Command::MoveTransition(tdx, source, target) => {
                    if let Some(endpoints) = self.graph.endpoints(tdx) {
                        self.graph.move_transition(
                            tdx,
                            source.unwrap_or(endpoints.0),
                            target.unwrap_or(endpoints.1),
                        );
                    }
                }
                Command::UpdateSelection(selection) => {
                    // TODO undo?
                    self.selection = selection;
                }
                Command::SetInitial(idx, target, initial) => {
                    if let Some(state) = self.graph.state(idx) {
                        // Merge undo?
                        self.graph
                            .update_state(idx, state.clone().with_initial(Some(initial)));
                        self.graph
                            .set_initial(idx, self.graph.initial(idx).set_idx(target));
                    }
                }
                Command::UnsetInitial(idx) => {
                    if let Some(state) = self.graph.state(idx) {
                        // Merge undo?
                        self.graph
                            .update_state(idx, state.clone().with_initial(None));
                        self.graph.set_initial(idx, transit::Initial::None);
                    }
                }
                Command::StepInitial(idx) => {
                    let initial = self.graph.initial(idx);
                    match initial {
                        transit::Initial::None => (), // error?
                        _ => self.graph.set_initial(idx, initial.step()),
                    }
                }
                _ => println!("unhandled command: {:?}", c),
            }
        }
    }

    pub fn show(&self, ui: &mut Ui) -> Vec<Command> {
        let rect = ui.max_rect();
        ui.allocate_rect(rect, Sense::hover());

        // Toggle debug.
        if ui.input().key_pressed(Key::D) {
            ui.ctx().set_debug_on_hover(!ui.ctx().debug_on_hover());
            ui.style_mut().debug.show_blocking_widget = true;
            ui.style_mut().debug.show_interactive_widgets = true;
        }

        // Keep edit data in temp storage.
        let edit_data = ui.data().get_temp(ui.id());
        let edit_data = edit_data.unwrap_or_else(|| {
            let data = Arc::new(Mutex::new(EditData::default()));
            ui.data().insert_temp(ui.id(), data.clone());
            data
        });
        let mut edit_data = edit_data.lock().unwrap();

        // Show root and recursively show children.
        self.show_state(self.graph.root, rect.min.to_vec2(), 0, &mut edit_data, ui);

        self.show_transitions(&mut edit_data, ui);

        let mut commands = std::mem::take(&mut edit_data.commands);

        // Resolve drag. ui.input().pointer.primary_released() doesn't work?
        if ui.input().pointer.any_released() {
            // Take drag to clear it.
            match std::mem::take(&mut edit_data.drag) {
                //_ if !drag.min_drag(ui) => (),
                Drag::State(src, offset, Some((target, _)), parent_root_offset) => {
                    if let Some(p) = ui.input().pointer.interact_pos() {
                        // Place the state relative to the parent with the offset.
                        let p = p - parent_root_offset - offset;
                        commands.push(Command::MoveState(src, target, p))
                    }
                }
                Drag::AddTransition(src, t, Some((target, _))) => {
                    // TEMP label
                    let id = format!(
                        "{} > {}",
                        self.graph.state(src).map(|s| s.id.as_str()).unwrap_or("?"),
                        self.graph
                            .state(target)
                            .map(|s| s.id.as_str())
                            .unwrap_or("?"),
                    );

                    edit_data
                        .commands
                        .push(Command::AddTransition(src, target, t.with_id(id)))
                }
                Drag::TransitionSource(tdx, port1, Some((new_src, _)), _) => {
                    if let Some((t, (src, _))) =
                        // Should there be an accessor for both?
                        self.graph.transition(tdx).zip(self.graph.endpoints(tdx))
                    {
                        // Check if the source has changed.
                        if src != new_src {
                            // Update port. TODO: merge undos
                            if t.port1 != port1 {
                                commands.push(Command::UpdateTransition(
                                    tdx,
                                    t.clone().with_port1(port1),
                                ));
                            }
                            commands.push(Command::MoveTransition(tdx, Some(new_src), None))
                        }
                    } // else, error?
                }
                Drag::TransitionTarget(tdx, port2, Some((new_target, _)), _) => {
                    if let Some((t, (_, target))) =
                        self.graph.transition(tdx).zip(self.graph.endpoints(tdx))
                    {
                        // Check if the target has changed.
                        if target != new_target {
                            // Update port. TODO: merge undos
                            if t.port2 != port2 {
                                commands.push(Command::UpdateTransition(
                                    tdx,
                                    t.clone().with_port2(port2),
                                ));
                            }
                            commands.push(Command::MoveTransition(tdx, None, Some(new_target)))
                        }
                    } // else, error?
                }
                Drag::Initial(i, Some((target, port)), (c1, c2)) => {
                    commands.push(Command::SetInitial(i, target, (port, c1, c2)))
                }
                _ => (),
            }
        }

        commands
    }

    pub fn show_transitions(&self, edit_data: &mut EditData, ui: &mut Ui) {
        let EditData {
            rects,
            drag,
            commands,
        } = &mut *edit_data;

        // Show all transitions.
        for (tdx, source, target, t, internal) in self.graph.transitions() {
            if let (Some(source_rect), Some(target_rect)) =
                (rects.get(&source.index()), rects.get(&target.index()))
            {
                let start = port_out(*source_rect, t.port1);
                let end = port_in(*target_rect, t.port2);
                self.show_connection(
                    rects,
                    start,
                    end,
                    Connection::Transition(tdx, t, internal, drag),
                    ui,
                    commands,
                );
            }
        }

        // New transition in progress.
        match drag {
            Drag::AddTransition(source, ref mut t, target) => {
                if let (Some(start), Some(end)) = (
                    rects.get(&source.index()).map(|r| port_out(*r, t.port1)),
                    target
                        .and_then(|target| rects.get(&target.0.index()))
                        .map(|r| port_in(*r, t.port2))
                        .or(ui.ctx().pointer_interact_pos()),
                ) {
                    // Make up control points based on the start and end.
                    let (c1, c2) = approx_cp(start, end);
                    t.c1 = c1;
                    t.c2 = c2;

                    self.show_connection(
                        rects,
                        start,
                        end,
                        Connection::DragTransition(&t),
                        ui,
                        commands,
                    );
                }
            }
            _ => (),
        }
    }

    pub fn show_resize(&self, id: Id, rect: Rect, ui: &mut Ui) -> Response {
        let resize_size = Vec2::splat(ui.visuals().resize_corner_size);
        let resize_rect = Rect::from_min_size(rect.max - resize_size, resize_size);
        let resize_response = ui.interact(resize_rect, id.with("resize"), Sense::drag());
        paint_resize_corner(ui, &resize_response);
        resize_response
    }

    pub fn show_state(
        &self,
        idx: transit::Idx,
        // Parent rect min.
        offset: Vec2,
        depth: usize,
        edit_data: &mut EditData,
        ui: &mut Ui,
    ) {
        let id = Id::new(idx);
        let state = self.graph.state(idx).unwrap();
        let root = idx == self.graph.root;
        let interact_pos = ui.input().pointer.interact_pos();

        // let drag = &mut edit_data.drag;
        // let commands = &mut edit_data.commands;

        let rect = match edit_data.drag {
            // Use all available space for the root.
            _ if root => state.rect.intersect(ui.max_rect()),
            Drag::State(i, drag_offset, ..) if i == idx => {
                let p = interact_pos.unwrap_or_default();
                Rect::from_min_size(p - drag_offset, state.rect.size())
            }
            Drag::Resize(i, delta) if i == idx => {
                // Include the offset and the resize delta.
                Rect::from_min_size(state.rect.min + offset, state.rect.size() + delta)
            }
            // Just the offset.
            _ => state.rect.translate(offset),
        };

        // Write rect.
        edit_data.rects.insert(idx.index(), rect);

        // Reserve background shape.
        let bg = ui.painter().add(Shape::Noop);

        // Can't drag the root state.
        let sense = if !root {
            Sense::click_and_drag()
        } else {
            Sense::click()
        };

        // Background interaction, dragging states and context menu.
        let mut state_response = ui.interact(rect, id, sense);

        // The inner_ui only serves to convey the clip_rect to children.
        let mut inner_ui = ui.child_ui_with_id_source(rect, *ui.layout(), idx);

        // Inset the clip_rect so things don't draw over the
        // stroke. The default theme stroke(s) is generally 1.0?
        let clip_rect = rect.shrink(if root { 0.0 } else { 1.0 });

        // Intersect our clip rect with the parent's.
        inner_ui.set_clip_rect(clip_rect.intersect(ui.clip_rect()));

        // Set drag target first and let child states override.
        self.set_drag_target(idx, rect, depth, &mut edit_data.drag, &inner_ui);

        // Show child states.
        for child in self.graph.children(Some(idx)) {
            // If the child state is being dragged, unset the clip rect so it can be dragged out. We
            // check min_drag before drawing in another layer because moving layers upsets the click
            // checking (selection). If you drag back to the original position the layer will revert
            // which is a bug. This is a hack until egui supports only starting the drag due to time
            // or motion.
            if edit_data.drag.dragging(child) && edit_data.drag.min_drag(&inner_ui) {
                // Draw in a layer so it draws on top.
                let layer_id = LayerId::new(Order::Tooltip, id);
                // HACK: with_layer_id allocates a zero-sized rect plus item_spacing here, which
                // will shift the header down while dragging. We can't set the layer_id directly. We
                // could also fix this by drawing the header at a specific rect.
                inner_ui.spacing_mut().item_spacing = Vec2::ZERO;
                inner_ui.with_layer_id(layer_id, |mut ui| {
                    let root_rect = *edit_data.rects.get(&self.graph.root.index()).unwrap();
                    ui.set_clip_rect(root_rect);
                    self.show_state(child, rect.min.to_vec2(), depth + 1, edit_data, &mut ui);
                    ui.set_clip_rect(clip_rect);
                });
                inner_ui.reset_style(); // HACK
            } else {
                self.show_state(
                    child,
                    rect.min.to_vec2(),
                    depth + 1,
                    edit_data,
                    &mut inner_ui,
                );
            }
        }

        // Show the header and resize after child states so they interact first.
        let header_inset = Vec2::splat(4.0);
        let header_rect = inner_ui.available_rect_before_wrap().shrink2(header_inset);
        let header_response = inner_ui
            .allocate_ui_at_rect(header_rect, |ui| {
                self.show_header(idx, root, state, edit_data, ui);
            })
            .response;
        let header_rect = header_response.rect.expand2(header_inset);

        // ui.ctx()
        //     .debug_painter()
        //     .debug_rect(min_rect, Color32::DEBUG_COLOR, "show_header");

        // Resize. Cannot resize the root state. Only show the resize if the pointer is in the rect,
        // or we are currently resizing it (since the pointer can be outside while dragging - it's a
        // frame behind?).
        if !root && (edit_data.drag.resizing(idx) || ui.rect_contains_pointer(rect)) {
            let response = self.show_resize(id, rect, &mut inner_ui);

            let drag = &mut edit_data.drag;

            drag_delta!(
                (response, ui, drag, Resize, idx),
                Drag::Resize(idx, response.drag_delta()),
                |delta: &mut Vec2| {
                    // Find the minimum delta such that we don't resize smaller than the header size
                    // (including inset). Limit max?
                    let size = state.rect.size();
                    *delta = (header_rect.size() - size).max(*delta + response.drag_delta())
                },
                |delta: &mut Vec2| edit_data.commands.push(Command::ResizeState(idx, *delta))
            );
        }

        // Can't select root.
        if state_response.clicked_by(PointerButton::Primary) {
            edit_data.commands.push(Command::UpdateSelection(if root {
                Selection::None
            } else {
                Selection::State(idx)
            }))
        }

        // New drags. There is no `drag_started_by`. We should not get a drag here if a drag is
        // started in a child state, but we check `in_drag` anyway since `dragged` is continuous.
        let dragged = state_response.dragged_by(PointerButton::Primary);
        if dragged && !edit_data.drag.in_drag() {
            if let Some(p) = interact_pos {
                if ui.input().modifiers.shift {
                    // New transition, drag to target.
                    let port1 = self.free_port(idx, transit::Direction::Outgoing);
                    edit_data.drag = Drag::AddTransition(
                        idx,
                        // Use a builder instead of default? No guarantees on port2 being set or zero.
                        Transition {
                            port1,
                            ..Default::default()
                        },
                        None,
                    );
                } else {
                    // Save the pointer offset from the state origin. Zero is never correct for the
                    // parent offest, so maybe it should be an Option like target?
                    edit_data.drag = Drag::State(idx, p - rect.min, None, Vec2::ZERO);
                }
            }
        }

        // Context menu on right click.
        if !edit_data.drag.in_drag() {
            state_response = state_response.context_menu(|ui| {
                if ui.button("Add state").clicked() {
                    // Position is the original click, relative to parent.
                    let p = ui.min_rect().min - rect.min.to_vec2();
                    edit_data.commands.push(Command::AddState(idx, p));
                    ui.close_menu();
                }
                // Can't remove the root state.
                if idx != self.graph.root {
                    if ui.button("Remove state").clicked() {
                        edit_data.commands.push(Command::RemoveState(idx, true));
                        ui.close_menu();
                    }
                    if ui.button("Remove state (recursive)").clicked() {
                        edit_data.commands.push(Command::RemoveState(idx, false));
                        ui.close_menu();
                    }
                }
            });
        }

        // TODO: Make a background for the root so we can show drag_valid.
        if !root {
            let style = ui.style();
            let selected = match self.selection {
                Selection::State(i) if i == idx => true,
                _ => false,
            };

            // There is some weirdness with the response and the layer change while dragging, so
            // just force it to active if not selected:
            let mut widget_visuals = if !selected && edit_data.drag.dragging(idx) {
                style.visuals.widgets.active
            } else {
                style.interact_selectable(&state_response, selected)
            };

            // Alternate background colors based on depth. The default dark theme is very dark here,
            // closer to the window background.
            if depth % 2 == 1 && widget_visuals == style.visuals.widgets.inactive {
                widget_visuals.bg_fill = style.visuals.faint_bg_color;
            }

            let stroke = if edit_data.drag.is_target(idx) {
                // The default stroke for selection is thin, so make it more prominent here to show
                // the drag target.
                let mut stroke = ui.style().visuals.selection.stroke;
                stroke.width = 4.0;
                stroke
            } else {
                // Use the fg_stroke since the default dark theme has no bg_stroke for inactive,
                // which makes all the contained states indiscernible.
                widget_visuals.fg_stroke
            };
            ui.painter().set(
                bg,
                RectShape {
                    rect,
                    rounding: widget_visuals.rounding,
                    fill: widget_visuals.bg_fill,
                    stroke,
                },
            );
        }
    }

    // Collapsable? How do we redirect incoming transitions to child states?
    pub fn show_header(
        &self,
        idx: transit::Idx,
        root: bool,
        state: &State,
        edit_data: &mut EditData,
        ui: &mut Ui,
    ) -> InnerResponse<()> {
        ui.horizontal(|mut ui| {
            let initial_size = Vec2::splat(6.0);
            let response = ui.allocate_response(initial_size, Sense::click_and_drag());

            // initial first? drag to select? or select from list?
            let initial = self.graph.initial(idx);
            //let initial_idx = initial.idx();

            let rect = Rect::from_center_size(response.rect.center(), initial_size);
            let start = rect.center();

            // Left click toggles type. Right click clears. Drag sets new initial. TODO:
            let show_initial = match edit_data.drag {
                Drag::None if response.drag_started() => {
                    edit_data.drag = Drag::Initial(idx, None, Default::default());
                    true
                }
                Drag::Initial(_idx, target, ref mut cp) if _idx == idx => {
                    // Reuse existing port if dragging to existing target?
                    if let Some(end) = target
                        .and_then(|(t, p)| edit_data.rects.get(&t.index()).zip(Some(p)))
                        .map(|(r, p)| port_in(*r, p))
                        .or_else(|| ui.ctx().pointer_interact_pos())
                    {
                        // TODO: bias down
                        *cp = approx_cp(start, end);
                        self.show_connection(
                            &edit_data.rects,
                            start,
                            end,
                            Connection::Initial(idx, *cp),
                            &mut ui,
                            &mut edit_data.commands,
                        );
                    }
                    true
                }
                _ => {
                    if let Some((i, (port, c1, c2))) = initial.idx().zip(state.initial) {
                        if let Some(end) =
                            edit_data.rects.get(&i.index()).map(|r| port_in(*r, port))
                        {
                            self.show_connection(
                                &edit_data.rects,
                                start,
                                end,
                                Connection::Initial(idx, (c1, c2)),
                                &mut ui,
                                &mut edit_data.commands,
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
            match initial {
                transit::Initial::HistoryShallow(_) => {
                    ui.label("h");
                }
                transit::Initial::HistoryDeep(_) => {
                    ui.label("h*");
                }
                _ => (),
            }

            // Show id.
            let InnerResponse { inner, response } = Editabel::new().show(&state.id, ui);

            // Clicking the label selects the state.
            if response.clicked_by(PointerButton::Primary) {
                edit_data.commands.push(Command::UpdateSelection(if root {
                    Selection::None
                } else {
                    Selection::State(idx)
                }))
            }

            // Dragging the label drags the state.

            if let Some(id) = inner {
                edit_data
                    .commands
                    .push(Command::UpdateState(idx, state.clone().with_id(id)))
            }

            // hover should show source/location by name? click to
            // select fns from source?
            if ui.small_button("enter").clicked() {
                dbg!("clicked enter");
            };
            if ui.small_button("exit").clicked() {
                dbg!("clicked exit");
            }
        })
    }

    /// Each state sets the drag target for its children. The topmost child that contains the
    /// pointer becomes the target. This is similiar to egui's builtin handling of overlapping
    /// interactions. We can't set the target inside each child recursively because they aren't
    /// aware of siblings or their ordering. In order to reduce the amount of work done here that is
    /// thrown away we could recurse from the root once before drawing anything.
    pub fn set_drag_target(
        &self,
        idx: transit::Idx,
        rect: Rect,
        depth: usize,
        drag: &mut Drag,
        ui: &Ui,
    ) {
        if !ui.rect_contains_pointer(rect) {
            return;
        }

        let Some(p) = ui.input().pointer.interact_pos() else {
            return
        };

        let parent_offset = rect.min.to_vec2();

        // For transition endpoints only.
        let dir = match drag {
            Drag::TransitionSource(..) => transit::Direction::Outgoing,
            //Drag::TransitionTarget(..) => transit::Direction::Incoming,
            _ => transit::Direction::Incoming,
        };

        match drag {
            // We can't drag into ourselves.
            Drag::State(i, ..) if *i == idx => (),
            Drag::State(dragged_idx, _drag_offset, target, offset) => {
                // Root clears the target by setting itself initially (any state can be dragged to
                // root).
                if depth == 0 {
                    *target = Some((idx, depth));
                    *offset = parent_offset;
                }

                if let Some((t, new_offset)) = self.graph.children_rev(idx).find_map(|(i, s)| {
                    let child_rect = s.state.rect.translate(parent_offset).intersect(rect);

                    // The drag target cannot be in the path of the dragged state (cycles). This
                    // never happens in practice since the dragged states are moving with the
                    // pointer.
                    (!self.graph.in_path(*dragged_idx, i) && child_rect.contains(p)).then(|| {
                        // ui.ctx().debug_painter().debug_rect(child_rect, Color32::GREEN, format!("{:?}", p - child_rect.min));
                        (i, child_rect.min.to_vec2())
                    })
                }) {
                    *target = Some((t, depth + 1));
                    *offset = new_offset;
                }
            }
            Drag::TransitionSource(_, port2, target, _)
            | Drag::TransitionTarget(_, port2, target, _)
            | Drag::AddTransition(_, Transition { port2, .. }, target) => {
                if depth == 0 {
                    // No transition can target the root.
                    *target = None;
                }

                if let Some((i, _)) = self
                    .graph
                    .children_rev(idx)
                    .find(|(_i, s)| ui.rect_contains_pointer(s.state.rect.translate(parent_offset)))
                {
                    // Find a free port.
                    *port2 = self.free_port(i, dir);
                    *target = Some((i, depth + 1))
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

    // Do self-transitions show up twice?
    pub fn free_port(&self, idx: transit::Idx, direction: transit::Direction) -> usize {
        let mut ts: Vec<_> = self
            .graph
            .state_transitions(idx, direction)
            .map(|(_, _, t, _)| t.port1)
            .collect();
        ts.sort();
        let mut free = 0;
        for port in ts {
            if free < port {
                return free;
            } else {
                free = port + 1
            }
        }
        free
    }

    // Clicking the curve for selection? https://github.com/emilk/egui/discussions/1959

    // Sample drag to "paint" the curve?

    // Dragging the label offsets both control points equally, which leads to situations where the
    // position of the label doesn't follow the pointer. Using something like
    // [[https://docs.rs/lyon_geom/1.0.4/lyon_geom/cubic_bezier/struct.CubicBezierSegment.html#method.drag]]
    // might solve this.
    pub fn show_connection(
        &self,
        rects: &Rects,
        start: Pos2,
        end: Pos2,
        conn: Connection,
        ui: &mut Ui,
        commands: &mut Vec<Command>,
    ) {
        let selected = match conn {
            Connection::Transition(tdx, _, _, _) => match self.selection {
                Selection::Transition(i) if i == tdx => true,
                _ => false,
            },
            _ => false,
        };

        let mut ui = ui.child_ui_with_id_source(
            ui.max_rect(),
            *ui.layout(),
            match conn {
                Connection::Initial(src, ..) => Id::new(src).with("initial"),
                Connection::DragInitial => ui.id().with("drag_initial"),
                Connection::Transition(tdx, ..) => Id::new(tdx),
                Connection::DragTransition(_) => ui.id().with("drag_transition"),
            },
        );

        let control_size = if selected { 8.0 } else { 6.0 };

        // draw curve, label, guard, internal toggle ----

        // Set curve points based on the transition drag state.
        let (start, c1, c2, end) = match conn {
            Connection::Transition(tdx, t, _, ref drag) => {
                let endpoints = self.graph.endpoints(tdx).unwrap();

                // Include the drag (from last frame).
                match drag {
                    Drag::TransitionId(i, delta) if *i == tdx => {
                        (start, t.c1 + *delta, t.c2 + *delta, end)
                    }
                    Drag::TransitionControl((i, cp), delta) if *i == tdx => match cp {
                        ControlPoint::C1 => (start, t.c1 + *delta, t.c2, end),
                        ControlPoint::C2 => (start, t.c1, t.c2 + *delta, end),
                    },
                    Drag::TransitionSource(_tdx, port, target, _) if *_tdx == tdx => (
                        target
                            .and_then(|(i, _)| {
                                rects.get(&i.index()).zip(
                                    // Use the original port if this is the existing target.
                                    Some(if endpoints.0 == i { t.port1 } else { *port }),
                                )
                            })
                            .map(|(r, p)| port_out(*r, p))
                            .or(ui.ctx().pointer_interact_pos())
                            .unwrap_or(start),
                        t.c1,
                        t.c2,
                        end,
                    ),
                    Drag::TransitionTarget(_tdx, port, target, _) if *_tdx == tdx => (
                        start,
                        t.c1,
                        t.c2,
                        target
                            .and_then(|(i, _)| {
                                rects.get(&i.index()).zip(
                                    // Use the original port if this is the existing target.
                                    Some(if endpoints.1 == i { t.port2 } else { *port }),
                                )
                            })
                            .map(|(r, p)| port_in(*r, p))
                            .or(ui.ctx().pointer_interact_pos())
                            .unwrap_or(start),
                    ),
                    _ => (start, t.c1, t.c2, end),
                }
            }
            Connection::DragTransition(t) => (start, t.c1, t.c2, end),
            Connection::Initial(_i, cp) => (start, cp.0, cp.1, end),
            Connection::DragInitial => todo!(),
        };

        // Control points are relative to the start and end, respectively.
        let c1 = start + c1;
        let c2 = end + c2;

        let color = if selected {
            ui.style().visuals.selection.stroke.color
        } else {
            ui.style().visuals.widgets.active.fg_stroke.color
        };

        // Make drag/selected more visible?
        let stroke = Stroke::new(2.0, color);
        let bezier = CubicBezierShape::from_points_stroke(
            [start, c1, c2, end],
            false,
            Color32::TRANSPARENT,
            stroke,
        );

        ui.painter().add(bezier);

        match conn {
            Connection::Transition(tdx, t, _internal, drag) => {
                // Show endpoints. Maybe only if selected?
                let control_size_sq = Vec2::splat(control_size);
                let source_rect = Rect::from_center_size(start, control_size_sq);
                let response = ui.allocate_rect(source_rect, Sense::drag());

                match drag {
                    Drag::None if response.drag_started() => {
                        match self.graph.endpoints(tdx) {
                            Some((i, _)) => *drag = Drag::TransitionSource(tdx, 0, None, i),
                            _ => (), // error
                        }
                    }
                    _ => (),
                }

                let color = ui.style().interact(&response).fg_stroke.color;
                ui.painter().circle_filled(start, control_size * 0.5, color);

                let target_rect = Rect::from_center_size(end, control_size_sq);
                let response = ui.allocate_rect(target_rect, Sense::drag());

                match drag {
                    Drag::None if response.drag_started() => {
                        match self.graph.endpoints(tdx) {
                            Some((_, i)) => *drag = Drag::TransitionTarget(tdx, 0, None, i),
                            _ => (), // error
                        }
                    }
                    _ => (),
                }

                let color = ui.style().interact(&response).fg_stroke.color;

                let mut mesh = arrow(control_size, color);
                mesh.translate(end.to_vec2());
                ui.painter().add(mesh);

                // Show control points if selected.
                if selected {
                    for (cp, p) in [(ControlPoint::C1, c1), (ControlPoint::C2, c2)] {
                        self.show_control_point(
                            tdx,
                            t,
                            cp,
                            Rect::from_center_size(p, control_size_sq),
                            drag,
                            &mut ui,
                            commands,
                        );
                    }
                }

                let rect = Rect::from_center_size(
                    bezier.sample(0.5),
                    // What width?
                    Vec2::new(128.0, ui.style().spacing.interact_size.y),
                );

                ui.allocate_ui_at_rect(rect, |ui| {
                    let _response = ui
                        .horizontal(|ui| {
                            let InnerResponse { inner, response } =
                                Editabel::sense(Sense::click_and_drag()).show(&t.id, ui);
                            if let Some(id) = inner {
                                commands.push(Command::UpdateTransition(tdx, t.clone().with_id(id)))
                            }
                            if response.double_clicked() {
                                dbg!("double");
                            } else {
                                if response.clicked() {
                                    commands
                                        .push(Command::UpdateSelection(Selection::Transition(tdx)))
                                }

                                drag_delta!(
                                    (response, ui, drag, TransitionId, tdx),
                                    Drag::TransitionId(tdx, Vec2::ZERO),
                                    |delta: &mut Vec2| *delta += response.drag_delta(),
                                    |delta: &mut Vec2| {
                                        let mut t = t.clone();
                                        t.c1 += *delta;
                                        t.c2 += *delta;
                                        commands.push(Command::UpdateTransition(tdx, t))
                                    }
                                );
                            }
                        })
                        .response;
                });
            }
            _ => {}
        }
    }

    pub fn show_control_point(
        &self,
        tdx: transit::Tdx,
        transition: &Transition,
        cp: ControlPoint,
        rect: Rect,
        drag: &mut Drag,
        ui: &mut Ui,
        commands: &mut Vec<Command>,
    ) {
        let response = ui.allocate_rect(rect, Sense::drag());

        // Ignore selection since its always selected.
        let wv = ui.style().interact(
            &response,
            // match self.selection {
            //     Selection::Transition(_tdx) if tdx == _tdx => true,
            //     _ => false,
            // },
        );

        drag_delta!(
            (response, ui, drag, TransitionControl, (tdx, cp)),
            Drag::TransitionControl((tdx, cp), Vec2::ZERO),
            |delta: &mut Vec2| *delta += response.drag_delta(),
            |delta: &mut Vec2| {
                let mut t = transition.clone();
                match cp {
                    ControlPoint::C1 => t.c1 += *delta,
                    ControlPoint::C2 => t.c2 += *delta,
                }
                commands.push(Command::UpdateTransition(tdx, t))
            }
        );

        ui.painter().circle_filled(
            rect.center(),
            rect.size().min_elem() * 0.5,
            wv.fg_stroke.color,
        );
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

// Scale based on number of ports and vertical size? We have to
// select the transition first to drag?
pub fn port_in(rect: Rect, port: usize) -> Pos2 {
    rect.min + vec2(0.0, 10.0 * (port + 1) as f32)
}

pub fn port_out(rect: Rect, port: usize) -> Pos2 {
    rect.right_top() + vec2(0.0, 10.0 * (port + 1) as f32)
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
}
