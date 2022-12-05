// TODO: make a separte crate for the bin and only depend on egui here
use editabel::Editabel;
use eframe::egui::epaint::{CubicBezierShape, Vertex};
use eframe::egui::*;
use eframe::epaint::RectShape;
use transit::{ExportError, ImportError};

mod editabel;

#[derive(Default, Clone)]
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
    // This should be in temp?
    #[serde(skip)]
    pub selection: Selection,
}

#[cfg_attr(feature = "serde", derive(serde::Deserialize, serde::Serialize))]
#[derive(Clone, Debug)]
pub struct State {
    id: String,
    // These needs to be pulled from a contained struct.
    //enter: String,
    //exit: String,
    /// Offset of the parent (relative to root) such that a global
    /// position can be found for this state.
    parent_root_offset: Vec2,
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
            parent_root_offset: Vec2::ZERO,
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
    // event? label?
    c1: Pos2,
    c2: Pos2,
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
    MoveState(transit::Idx, transit::Idx, Pos2, Vec2),
    ResizeState(transit::Idx, Vec2),
    AddTransition(transit::Idx, transit::Idx, Transition),
    RemoveTransition,
    UpdateTransition,
    MoveTransition,
    SetInitial,
    SetEnter,
    SetExit,
    SetGuard,
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
    TransitionControl(transit::Tdx, Vec2, bool),
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
}

pub enum Connection<'t> {
    // Where are the control points stored?
    Initial(transit::Idx, transit::Idx),
    DragInitial,
    Transition(transit::Tdx, &'t Transition, bool),
    DragTransition(&'t Transition),
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

    pub fn root_rect(&self) -> Rect {
        self.graph.root().rect
    }

    pub fn load(&mut self) -> Result<(), ImportError> {
        if let Some(path) = &self.path {
            self.graph = transit::Graph::import_from_file(&path)?;
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
                Command::AddState(parent, pos) => {
                    self.graph.add_state(
                        State {
                            id: "untitled".into(),
                            rect: Rect::from_min_size(pos, State::DEFAULT_SIZE),
                            ..Default::default()
                        },
                        Some(parent),
                    );
                }
                Command::RemoveState(idx, rec) => self.graph.remove_state(idx, !rec, !rec),
                Command::MoveState(idx, parent, offset, parent_root_offset) => {
                    if let Some(state) = self.graph.state(idx) {
                        let mut state = state.clone();
                        state.rect = Rect::from_min_size(offset, state.rect.size());
                        state.parent_root_offset = parent_root_offset;

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
                _ => (),
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

        // Keep drag state in temp storage.
        let mut drag = ui.ctx().data().get_temp(ui.id()).unwrap_or_default();
        let mut commands = Vec::new();

        // Show root and recursively show children.
        self.show_state(
            self.graph.root,
            rect.min.to_vec2(),
            0,
            &mut drag,
            ui,
            &mut commands,
        );

        // Resolve drag. ui.input().pointer.primary_released() doesn't work?
        if ui.input().pointer.any_released() {
            match drag {
                Drag::State(src, offset, Some((target, _)), parent_root_offset) => {
                    if let Some(p) = ui.input().pointer.interact_pos() {
                        // Place the state relative to the parent with the offset.
                        let p = p - parent_root_offset - offset;
                        commands.push(Command::MoveState(src, target, p, parent_root_offset));
                    }
                }
                Drag::AddTransition(src, t, Some((target, _))) => {
                    commands.push(Command::AddTransition(src, target, t.clone()));
                }
                _ => (),
            }
            drag = Drag::None;
        }

        // Save drag state.
        ui.ctx().data().insert_temp(ui.id(), drag);

        commands
    }

    pub fn show_resize(&self, id: Id, rect: Rect, ui: &mut Ui) -> Response {
        let resize_size = Vec2::splat(ui.visuals().resize_corner_size);
        let resize_rect = Rect::from_min_size(rect.max - resize_size, resize_size);
        let resize_response = ui.interact(resize_rect, id.with("resize"), Sense::drag());
        paint_resize_corner(ui, &resize_response);
        resize_response
    }

    /// Returns a state rect including drag state.
    pub fn state_rect(&self, idx: transit::Idx, drag: &Drag, ui: &Ui) -> Option<Rect> {
        self.graph.state(idx).and_then(|s| match drag {
            Drag::State(i, offset, ..) if *i == idx => ui
                .input()
                .pointer
                .interact_pos()
                .map(|p| Rect::from_min_size(p - *offset, s.rect().size())),
            Drag::Resize(i, delta) if *i == idx => {
                Some(Rect::from_min_size(s.rect().min, s.rect.size() + *delta))
            }
            _ => Some(s.rect()),
        })
    }

    pub fn show_state(
        &self,
        idx: transit::Idx,
        // Parent rect min.
        offset: Vec2,
        depth: usize,
        drag: &mut Drag,
        ui: &mut Ui,
        commands: &mut Vec<Command>,
    ) {
        let id = Id::new(idx);
        let state = self.graph.state(idx).unwrap();
        let root = idx == self.graph.root;
        let interact_pos = ui.input().pointer.interact_pos();

        let rect = match drag {
            Drag::State(i, drag_offset, ..) if *i == idx => {
                let p = interact_pos.unwrap_or_default();
                Rect::from_min_size(p - *drag_offset, state.rect.size())
            }
            Drag::Resize(i, delta) if *i == idx => {
                // Include the offset and the resize delta.
                Rect::from_min_size(state.rect.min + offset, state.rect.size() + *delta)
            }
            // Just the offset.
            _ => state.rect.translate(offset),
        };

        // Use all available space for the root, don't draw background.
        let inner_rect = if root {
            rect.intersect(ui.max_rect())
        } else {
            rect
        };

        // Reserve background shape.
        let bg = ui.painter().add(Shape::Noop);

        // The inner_ui only serves to convey the clip_rect to children.
        let mut inner_ui = ui.child_ui_with_id_source(inner_rect, *ui.layout(), idx);

        // Inset the clip_rect so things don't draw over the
        // stroke. The default theme stroke(s) is generally 1.0?
        let clip_rect = rect.shrink(if root { 0.0 } else { 1.0 });

        // Intersect our clip rect with the parent's.
        inner_ui.set_clip_rect(clip_rect.intersect(ui.clip_rect()));

        self.set_drag_target(idx, offset, depth, drag, &inner_ui);

        // Can't drag the root state.
        let sense = if !root {
            Sense::click_and_drag()
        } else {
            Sense::click()
        };

        // Background interaction, dragging states and context menu.
        let mut state_response = ui.interact(inner_rect, id, sense);

        // Inset the header from the inner_rect.
        let header_inset = Vec2::splat(4.0);
        let header_rect = Rect::from_min_max(inner_rect.min + header_inset, inner_rect.max);

        // TODO: children should not be able to cover the header, draw
        // this last

        let header_response = inner_ui
            .allocate_ui_at_rect(header_rect, |ui| {
                ui.horizontal(|ui| {
                    // initial first? drag to select? or select from list?

                    if let Some(id) = Editabel::show(&state.id, ui).inner {
                        commands.push(Command::UpdateState(idx, state.clone().with_id(id)))
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
        let header_rect = header_response.rect;

        // Resize.
        if !root {
            match drag {
                Drag::None => {
                    let response = self.show_resize(id, inner_rect, &mut inner_ui);
                    if response.drag_started() {
                        // This needs the same treatment for minimum size?
                        *drag = Drag::Resize(idx, response.drag_delta());
                    }
                }
                Drag::Resize(idx, ref mut delta) => {
                    let response = self.show_resize(id, inner_rect, &mut inner_ui);
                    if response.dragged() {
                        // Find the minimum delta such that we don't
                        // resize smaller than the header size
                        // (including inset).
                        let min = header_rect.expand2(header_inset).size();
                        let size = state.rect.size();
                        *delta = (min - size).max(*delta + response.drag_delta());
                    } else if response.drag_released() {
                        commands.push(Command::ResizeState(*idx, *delta));
                        *drag = Drag::None
                    }
                }
                _ => (),
            }
        }

        for (tdx, target, t, internal) in self.graph.transitions_out(idx) {
            let start = port_out(rect, t.port1);
            let end = port_in(self.state_rect(target, drag, &inner_ui).unwrap(), t.port2);
            self.show_connection(start, end, Connection::Transition(tdx, t, internal), ui);
        }

        // New transition in progress. We could do this in root too, to draw last and not check the
        // index.
        match drag {
            Drag::AddTransition(source, t, target) if *source == idx => {
                let start = port_out(rect, t.port1);
                let end = match target {
                    Some((target, _)) => {
                        // Since we are dragging already we don't need state_rect.
                        let target = self.graph.state(*target).unwrap();
                        port_in(target.rect(), t.port2)
                    }
                    // else, root?
                    None => interact_pos.unwrap(),
                };

                // Make up control points based on the start and end.
                let d = (end - start) * 0.3;
                t.c1 = start + vec2(d.x, -d.y);
                t.c2 = end + vec2(-d.x, d.y);

                self.show_connection(start, end, Connection::DragTransition(t), ui);
            }
            _ => (),
        }

        // Show child states.
        for child in self.graph.children(Some(idx)) {
            // If the child state is being dragged, unset the clip
            // rect so it can be dragged out.
            if drag.dragging(child) {
                // Draw in a layer so it draws on top.
                let layer_id = LayerId::new(Order::Tooltip, id);
                inner_ui.with_layer_id(layer_id, |mut ui| {
                    ui.set_clip_rect(self.root_rect());
                    self.show_state(
                        child,
                        rect.min.to_vec2(),
                        depth + 1,
                        drag,
                        &mut ui,
                        commands,
                    );
                    ui.set_clip_rect(clip_rect);
                });
            } else {
                self.show_state(
                    child,
                    rect.min.to_vec2(),
                    depth + 1,
                    drag,
                    &mut inner_ui,
                    commands,
                );
            }
        }

        // New drags. There is no `drag_started_by`. We should not get a drag here if a drag is
        // started in a child state, but we check `in_drag` anyway since `dragged` is continuous.
        let dragged = state_response.dragged_by(PointerButton::Primary);
        if dragged && !drag.in_drag() {
            if let Some(p) = interact_pos {
                if ui.input().modifiers.shift {
                    // New transition, drag to target.
                    let port1 = self.free_port(idx, transit::Direction::Outgoing);
                    *drag = Drag::AddTransition(
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
                    *drag = Drag::State(idx, p - rect.min, None, Vec2::ZERO);
                }
            }
        }

        // Context menu on right click.
        if !drag.in_drag() {
            state_response = state_response.context_menu(|ui| {
                if ui.button("Add state").clicked() {
                    // Position is the original click, relative to parent.
                    let pos = ui.min_rect().min - rect.min;
                    commands.push(Command::AddState(idx, pos.to_pos2()));
                    ui.close_menu();
                }
                // Can't remove the root state.
                if idx != self.graph.root {
                    if ui.button("Remove state").clicked() {
                        commands.push(Command::RemoveState(idx, true));
                        ui.close_menu();
                    }
                    if ui.button("Remove state (recursive)").clicked() {
                        commands.push(Command::RemoveState(idx, false));
                        ui.close_menu();
                    }
                }
            });
        }

        // TODO: Make a background for the root so we can show drag_valid.
        if !root {
            // TODO: selection
            let style = ui.style();
            let mut widget_visuals = style.interact_selectable(&state_response, drag.dragging(idx));

            // Alternate background colors based on depth. The default dark theme is very dark here,
            // closer to the window background.
            if depth % 2 == 1 && widget_visuals == style.visuals.widgets.inactive {
                widget_visuals.bg_fill = style.visuals.faint_bg_color;
            }

            let stroke = if drag.is_target(idx) {
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
        parent_offset: Vec2,
        depth: usize,
        drag: &mut Drag,
        ui: &Ui,
    ) {
        match drag {
            Drag::State(dragged_idx, _drag_offset, target, offset) => {
                // Root clears the target by setting itself initially (any state can be dragged to
                // root).
                if depth == 0 {
                    *target = Some((idx, depth));
                    *offset = parent_offset;
                }

                if let Some((t, p)) = self.graph.children_rev(idx).find_map(|(i, s)| {
                    let rect = s.state.rect;
                    (ui.rect_contains_pointer(rect.translate(parent_offset))
                        // The drag target cannot be in the path of the dragged state (cycles).
                        && !self.graph.in_path(*dragged_idx, i))
                    .then_some((i, rect.min.to_vec2()))
                }) {
                    *target = Some((t, depth + 1));
                    *offset = parent_offset + p;
                }
            }
            Drag::AddTransition(_, _, target) => {
                if depth == 0 {
                    // No transition can target the root.
                    *target = None;
                }

                if let Some((t, _)) = self
                    .graph
                    .children_rev(idx)
                    .find(|(_i, s)| ui.rect_contains_pointer(s.state.rect.translate(parent_offset)))
                {
                    *target = Some((t, depth + 1))
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

    // https://github.com/emilk/egui/discussions/1959 clicking the curve

    // sample drag to "paint" the curve?
    pub fn show_connection(
        &self,
        start: Pos2,
        end: Pos2,
        conn: Connection,
        ui: &mut Ui,
    ) -> Option<Command> {
        let color = Color32::WHITE;
        // draw port at start, dragging moves transition start
        ui.painter().circle_filled(start, 4.0, color);

        // need to select first to modify control points

        // draw curve, label, guard, internal toggle

        let stroke = Stroke::new(2.0, color);

        let (c1, c2) = match conn {
            Connection::Transition(_, t, _) | Connection::DragTransition(t) => (t.c1, t.c2),
            _ => {
                // Initial?
                let dx = (end - start).x * 0.3;
                (start + vec2(dx, 0.0), end + vec2(-dx, 0.0))
            }
        };

        let bezier = CubicBezierShape::from_points_stroke(
            [start, c1, c2, end],
            false,
            Color32::TRANSPARENT,
            stroke,
        );

        // use sample to get the mid point for label

        ui.painter().add(bezier);

        ui.painter().circle_filled(c1, 2.0, Color32::LIGHT_YELLOW);
        ui.painter().circle_filled(c2, 2.0, Color32::LIGHT_YELLOW);

        let mut mesh = arrow(8.0, color);
        mesh.translate(end.to_vec2());
        ui.painter().add(mesh);

        None
    }
}

fn arrow(size: f32, color: Color32) -> Mesh {
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

    pub fn rect(&self) -> Rect {
        self.rect.translate(self.parent_root_offset)
    }
}
