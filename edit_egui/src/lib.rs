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

#[cfg_attr(feature = "serde", derive(serde::Deserialize, serde::Serialize))]
#[derive(Clone, Debug)]
pub struct State {
    id: String,
    // These needs to be pulled from a contained struct.
    //enter: String,
    //exit: String,
    /// Rect relative to parent.
    rect: Rect,
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
    MoveTransition(transit::Tdx, transit::Idx, transit::Idx),
    SetInitial,
    SetEnter,
    SetExit,
    SetGuard,
    UpdateSelection(Selection),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ControlPoint {
    C1,
    C2,
}

// Drag target index and depth.
pub type DragTarget = Option<(transit::Idx, usize)>;

#[derive(Clone, Debug, Default)]
pub enum Drag {
    #[default]
    None,
    State(transit::Idx, Vec2, DragTarget, Vec2),
    Resize(transit::Idx, Vec2),
    Initial(transit::Idx, Vec2, DragTarget),
    AddTransition(transit::Idx, Transition, DragTarget),
    TransitionSource(transit::Tdx, Vec2, DragTarget),
    TransitionTarget(transit::Tdx, Vec2, DragTarget),
    TransitionControl(transit::Tdx, Vec2, ControlPoint),
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
            | Drag::Initial(_, _, t)
            | Drag::AddTransition(_, _, t)
            | Drag::TransitionSource(_, _, t)
            | Drag::TransitionTarget(_, _, t) => t,
            _ => &None,
        };
        target.map(|(t, _)| t == idx).unwrap_or_default()
    }

    /// Is the drag greater than some minimum?
    pub fn min_drag(&self, ui: &Ui) -> bool {
        match self {
            Drag::None => false, // ?
            Drag::AddTransition(..) => true,
            // Compare original press to the current pointer. This only works if the drag is in
            // progress, not after the pointer is released.
            Drag::State(..) => {
                let p = &ui.input().pointer;
                p.press_origin()
                    .zip(p.interact_pos())
                    .map(|(p0, p1)| (p1 - p0).abs().max_elem() >= 1.0)
                    .unwrap_or_default()
            }
            Drag::Resize(_, d)
            | Drag::Initial(_, d, _)
            | Drag::TransitionSource(_, d, _)
            | Drag::TransitionTarget(_, d, _)
            | Drag::TransitionControl(_, d, _)
            | Drag::TransitionId(_, d) => d.abs().max_elem() >= 1.0,
        }
    }
}

/// Mutable data, passed to each call of show_state.
#[derive(Default)]
pub struct EditData {
    rects: nohash_hasher::IntMap<usize, Rect>,
    drag: Drag,
    commands: Vec<Command>,
}

pub enum Connection<'a, 'b> {
    // Where are the control points stored?
    Initial(transit::Idx, transit::Idx),
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
                Command::UpdateSelection(selection) => {
                    // TODO undo?
                    self.selection = selection;
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
                    let d = (end - start) * 0.5;
                    t.c1 = vec2(d.x, -d.y);
                    t.c2 = vec2(-d.x, d.y);

                    self.show_connection(start, end, Connection::DragTransition(&t), ui, commands);
                }
            }
            _ => (),
        }

        // Resolve drag. ui.input().pointer.primary_released() doesn't work?
        if ui.input().pointer.any_released() {
            match drag {
                //_ if !drag.min_drag(ui) => (),
                Drag::State(src, offset, Some((target, _)), parent_root_offset) => {
                    if let Some(p) = ui.input().pointer.interact_pos() {
                        // Place the state relative to the parent with the offset.
                        let p = p - *parent_root_offset - *offset;
                        commands.push(Command::MoveState(*src, *target, p));
                    }
                }
                Drag::AddTransition(src, t, Some((target, _))) => {
                    // TEMP label
                    t.id = format!(
                        "{} > {}",
                        self.graph.state(*src).map(|s| s.id.as_str()).unwrap_or("?"),
                        self.graph
                            .state(*target)
                            .map(|s| s.id.as_str())
                            .unwrap_or("?"),
                    );

                    commands.push(Command::AddTransition(*src, *target, t.clone()));
                }
                _ => (),
            }
            edit_data.drag = Drag::None;
        }

        std::mem::take(&mut edit_data.commands)
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

        // Inset the header from the rect.
        let header_inset = Vec2::splat(4.0);
        let header_rect = Rect::from_min_max(rect.min + header_inset, rect.max);

        // TODO: children should not be able to cover the header, draw
        // this last

        let header_response = inner_ui
            .allocate_ui_at_rect(header_rect, |ui| {
                ui.horizontal(|ui| {
                    // Collapsable? How do we redirect incoming transitions to child states?

                    // initial first? drag to select? or select from list?

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
                });
            })
            .response;

        // This does nothing since you can't click between elements or the margin?
        // if header_response.clicked() {
        //     commands.push(Command::UpdateSelection(Selection::State(idx)))
        // }

        let header_rect = header_response.rect;

        // Resize. Cannot resize the root state. Only show the resize if the pointer is in the rect,
        // or we are currently resizing it (since the pointer can be outside while dragging - it's a
        // frame behind?).
        if !root && (edit_data.drag.resizing(idx) || ui.rect_contains_pointer(rect)) {
            match edit_data.drag {
                Drag::None => {
                    let response = self.show_resize(id, rect, &mut inner_ui);
                    if response.drag_started() {
                        // This needs the same treatment for minimum size?
                        edit_data.drag = Drag::Resize(idx, response.drag_delta());
                    }
                }
                Drag::Resize(idx, ref mut delta) => {
                    let response = self.show_resize(id, rect, &mut inner_ui);
                    if response.dragged() {
                        // Find the minimum delta such that we don't
                        // resize smaller than the header size
                        // (including inset).
                        let min = header_rect.expand2(header_inset).size();
                        let size = state.rect.size();
                        *delta = (min - size).max(*delta + response.drag_delta());
                    } else if response.drag_released() {
                        edit_data.commands.push(Command::ResizeState(idx, *delta));
                        edit_data.drag = Drag::None
                    }
                }
                _ => (),
            }
        }

        // Show child states.
        for child in self.graph.children(Some(idx)) {
            // If the child state is being dragged, unset the clip rect so it can be dragged out. We
            // check min_drag before drawing in another layer because moving layers upsets the click
            // checking (selection). If you drag back to the original position the layer will revert
            // which is a bug. This is a hack until egui supports only starting the drag due to time
            // or motion.
            if edit_data.drag.dragging(child) && edit_data.drag.min_drag(ui) {
                // Draw in a layer so it draws on top.
                let layer_id = LayerId::new(Order::Tooltip, id);
                inner_ui.with_layer_id(layer_id, |mut ui| {
                    let root_rect = *edit_data.rects.get(&self.graph.root.index()).unwrap();
                    ui.set_clip_rect(root_rect);
                    self.show_state(child, rect.min.to_vec2(), depth + 1, edit_data, &mut ui);
                    ui.set_clip_rect(clip_rect);
                });
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
        let Some(p) = ui.input().pointer.interact_pos() else {
            return
        };

        let parent_offset = rect.min.to_vec2();

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
            Drag::AddTransition(_src, t, target) => {
                if depth == 0 {
                    // No transition can target the root.
                    *target = None;
                }

                if let Some((i, _)) = self
                    .graph
                    .children_rev(idx)
                    .find(|(_i, s)| ui.rect_contains_pointer(s.state.rect.translate(parent_offset)))
                {
                    // Find a free incoming port.
                    t.port2 = self.free_port(i, transit::Direction::Incoming);
                    *target = Some((i, depth + 1))
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

        let color = if selected {
            ui.style().visuals.selection.stroke.color
        } else {
            ui.style().visuals.widgets.active.fg_stroke.color
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

        // draw port at start, dragging moves transition start
        ui.painter().circle_filled(start, control_size * 0.5, color);

        // need to select first to modify control points

        // draw curve, label, guard, internal toggle ----

        let stroke = Stroke::new(2.0, color);

        let (c1, c2) = match conn {
            Connection::Transition(tdx, t, _, ref drag) => {
                // Include the drag (from last frame).
                match drag {
                    Drag::TransitionId(i, delta) if *i == tdx => (t.c1 + *delta, t.c2 + *delta),
                    Drag::TransitionControl(i, delta, cp) if *i == tdx => match cp {
                        ControlPoint::C1 => (t.c1 + *delta, t.c2),
                        ControlPoint::C2 => (t.c1, t.c2 + *delta),
                    },
                    _ => (t.c1, t.c2),
                }
            }
            Connection::DragTransition(t) => (t.c1, t.c2),

            _ => {
                // Initial?
                //let dx = (end - start).x * 0.3;
                //(start + vec2(dx, 0.0), end + vec2(-dx, 0.0))
                (Vec2::ZERO, Vec2::ZERO)
            }
        };

        // Control points are relative to the start and end, respectively.
        let c1 = start + c1;
        let c2 = end + c2;

        let bezier = CubicBezierShape::from_points_stroke(
            [start, c1, c2, end],
            false,
            Color32::TRANSPARENT,
            stroke,
        );

        ui.painter().add(bezier);

        let mut mesh = arrow(control_size, color);
        mesh.translate(end.to_vec2());
        ui.painter().add(mesh);

        match conn {
            Connection::Transition(tdx, t, _internal, drag) => {
                // Show control points.
                if selected {
                    for (cp, p) in [(ControlPoint::C1, c1), (ControlPoint::C2, c2)] {
                        self.show_control_point(
                            tdx,
                            t,
                            cp,
                            Rect::from_center_size(p, Vec2::splat(control_size)),
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

                                // set_drag / validation w/ response? all these checks are redundant
                                if response.drag_started() {
                                    if !drag.in_drag() {
                                        *drag = Drag::TransitionId(tdx, Vec2::ZERO);
                                    } // else, error?
                                } else if response.dragged() {
                                    if let Drag::TransitionId(i, ref mut delta) = drag {
                                        if *i == tdx {
                                            *delta += response.drag_delta()
                                        }
                                    } // else, error?
                                } else if response.drag_released() {
                                    if drag.min_drag(ui) {
                                        match drag {
                                            Drag::TransitionId(i, delta) if *i == tdx => {
                                                let mut t = t.clone();
                                                t.c1 += *delta;
                                                t.c2 += *delta;
                                                commands.push(Command::UpdateTransition(tdx, t));
                                            }
                                            _ => (), // error?
                                        }
                                    }
                                    *drag = Drag::None
                                }
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

        if response.drag_started() {
            if !drag.in_drag() {
                *drag = Drag::TransitionControl(tdx, Vec2::ZERO, cp)
            }
        } else if response.dragged() {
            match drag {
                Drag::TransitionControl(_tdx, ref mut delta, _cp) if tdx == *_tdx && cp == *_cp => {
                    *delta += response.drag_delta();
                    // Already added the delta in show_connection to move the curve.
                    //rect = rect.translate(*delta);
                }
                _ => (), // error?
            }
        } else if response.drag_released() {
            if drag.min_drag(ui) {
                match drag {
                    Drag::TransitionControl(_tdx, delta, _cp) if tdx == *_tdx && cp == *_cp => {
                        let mut t = transition.clone();
                        match cp {
                            ControlPoint::C1 => t.c1 += *delta,
                            ControlPoint::C2 => t.c2 += *delta,
                        }
                        //rect = rect.translate(*delta);
                        commands.push(Command::UpdateTransition(tdx, t));
                    }
                    _ => (), // error?
                }
            }
            *drag = Drag::None;
        }

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
}

impl Transition {
    pub fn with_id(mut self, id: impl Into<String>) -> Self {
        self.id = id.into();
        self
    }
}
