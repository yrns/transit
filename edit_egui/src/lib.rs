use egui::epaint::CubicBezierShape;
use egui::*;

#[derive(Default, Clone)]
pub enum Selection {
    #[default]
    None,
    State(transit::Idx),
    Transition(transit::Tdx),
}

#[derive(Default)]
pub struct Statechart<C: transit::Context> {
    pub id: String,
    pub path: Option<std::path::PathBuf>,
    //statechart: transit::Statechart<C>,
    pub graph: transit::Graph<C>,
    pub selection: Selection,
}

#[derive(Clone)]
pub struct State {
    id: String,
    //enter: String,
    //exit: String,
    // relative to root?
    rect: egui::Rect,
    min_size: Vec2,
    #[allow(unused)]
    collapsed: bool,
    #[allow(unused)]
    pan: egui::Vec2,
    #[allow(unused)]
    zoom: f32,
}

// This is only used for the root state?
impl Default for State {
    fn default() -> Self {
        State {
            id: "untitled".into(),
            rect: egui::Rect::from_min_size(egui::pos2(0.0, 0.0), egui::Vec2::INFINITY),
            // This should really be the size of the header? Or the
            // collapsed header?
            min_size: Self::DEFAULT_SIZE,
            collapsed: false,
            pan: egui::Vec2::ZERO,
            zoom: 1.0,
        }
    }
}

#[derive(Clone)]
pub struct Transition {
    // event? label?
    c1: egui::Pos2,
    c2: egui::Pos2,
    // Source port index.
    port1: usize,
    /// Destination port index.
    port2: usize,
}

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
    AddState(transit::Idx, egui::Pos2),
    RemoveState(transit::Idx, bool),
    UpdateState,
    MoveState(transit::Idx, transit::Idx, egui::Pos2),
    ResizeState(transit::Idx, egui::Vec2),
    AddTransition,
    RemoveTransition,
    UpdateTransition,
    MoveTransition,
    SetInitial,
    SetEnter,
    SetExit,
    SetGuard,
}

#[derive(Debug)]
pub enum Drag {
    None,
    State(transit::Idx, egui::Vec2),
    Resize(transit::Idx, egui::Vec2),
    Initial(transit::Idx, egui::Vec2),
    TransitionSource(transit::Tdx, egui::Vec2),
    TransitionTarget(transit::Tdx, egui::Vec2),
    TransitionControl(transit::Tdx, egui::Vec2, bool),
}

impl Drag {
    pub fn sense(&self) -> egui::Sense {
        match self {
            Drag::None => egui::Sense::click_and_drag(),
            _ => egui::Sense::hover(),
        }
    }

    pub fn in_drag(&self) -> bool {
        match self {
            Drag::None => false,
            _ => true,
        }
    }
}

#[inline]
fn offset_rect(rect: egui::Rect, offset: egui::Vec2) -> egui::Rect {
    egui::Rect::from_min_size(rect.min + offset, rect.size())
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
    pub fn new(id: impl Into<String>) -> Self {
        let id = id.into();
        Self {
            id,
            ..Default::default()
        }
    }

    pub fn process_commands(&mut self, commands: Vec<Command>) {
        for c in commands {
            match c {
                Command::AddState(parent, pos) => {
                    self.graph.add_state(
                        State {
                            id: "untitled".into(),
                            rect: egui::Rect::from_min_size(pos, State::DEFAULT_SIZE),
                            ..Default::default()
                        },
                        Some(parent),
                    );
                }
                Command::RemoveState(idx, rec) => self.graph.remove_state(idx, !rec, !rec),
                Command::MoveState(idx, parent, offset) => {
                    if let Some(state) = self.graph.state(idx) {
                        let mut state = state.clone();
                        state.rect = egui::Rect::from_min_size(offset, state.rect.size());

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
                        state.rect = Rect::from_min_size(
                            state.rect.min,
                            state.min_size.max(state.rect.size() + delta),
                        );
                        self.graph.update_state(idx, state);
                    }
                }
                _ => (),
            }
        }
    }

    pub fn show(&self, drag: &mut Drag, ui: &mut egui::Ui) -> Vec<Command> {
        let rect = ui.max_rect();
        ui.allocate_rect(rect, egui::Sense::hover());
        let mut commands = Vec::new();

        // Show root and recursively show children.
        self.show_state(self.graph.root, rect.min.to_vec2(), drag, ui, &mut commands);

        commands
    }

    pub fn show_resize(&self, id: Id, rect: Rect, ui: &mut Ui) -> Response {
        let resize_size = Vec2::splat(ui.visuals().resize_corner_size);
        let resize_rect = egui::Rect::from_min_size(rect.max - resize_size, resize_size);
        let resize_response = ui.interact(resize_rect, id.with("resize"), egui::Sense::drag());
        paint_resize_corner(ui, &resize_response);
        resize_response
    }

    pub fn show_state(
        &self,
        idx: transit::Idx,
        // Parent rect min.
        offset: egui::Vec2,
        drag: &mut Drag,
        ui: &mut egui::Ui,
        commands: &mut Vec<Command>,
    ) {
        let id = egui::Id::new(idx);
        let state = self.graph.state(idx).unwrap();
        let root = idx == self.graph.root;

        let rect = match drag {
            Drag::State(i, drag_offset) if *i == idx => {
                let p = ui.input().pointer.interact_pos().unwrap_or_default();
                egui::Rect::from_min_size(p - *drag_offset, state.rect.size())
            }
            Drag::Resize(i, delta) if *i == idx => {
                // Include the offset and the resize delta.
                Rect::from_min_size(
                    state.rect.min + offset,
                    state.min_size.max(state.rect.size() + *delta),
                )
            }
            // Just the offset.
            _ => offset_rect(state.rect, offset),
        };

        // if ui.memory().is_being_dragged(id) {
        //     dbg!(id);
        // }

        // Use all available space for the root, don't draw
        // background.
        let stroke_width = 2.0;
        let inner_rect = if root {
            rect.intersect(ui.max_rect())
        } else {
            ui.painter().rect(
                rect,
                4.0,
                ui.visuals().widgets.active.bg_fill,
                ui.visuals().widgets.active.bg_stroke,
            );

            // Remove this and put space around header only.
            rect.shrink(4.0)
        };
        let mut child_ui = ui.child_ui_with_id_source(inner_rect, *ui.layout(), idx);
        let clip_rect = rect.shrink(stroke_width * 0.5);
        child_ui.set_clip_rect(clip_rect);

        // children should not be able to cover the header, draw this last
        if child_ui
            .horizontal(|ui| {
                ui.set_clip_rect(clip_rect);

                // initial first? drag to select? or select from list?

                // should be editable
                ui.label(&state.id);

                // hover should show source/location by name? click to
                // select fns from source?
                if ui.small_button("enter").clicked() {
                    dbg!("clicked enter");
                };
                if ui.small_button("exit").clicked() {
                    dbg!("clicked exit");
                }
            })
            .response
            .hovered()
        {
            //dbg!("header hover");
        }

        // Resize.
        if !root {
            match drag {
                Drag::None => {
                    let response = self.show_resize(id, inner_rect, ui);
                    if response.drag_started() {
                        *drag = Drag::Resize(idx, response.drag_delta());
                    }
                }
                Drag::Resize(idx, ref mut delta) => {
                    let response = self.show_resize(id, inner_rect, ui);
                    if response.dragged() {
                        *delta += response.drag_delta();
                    } else if response.drag_released() {
                        commands.push(Command::ResizeState(*idx, *delta));
                        *drag = Drag::None
                    }
                }
                _ => (),
            }
        }

        // ui.painter().arrow(
        //     rect.max - egui::Vec2::new(10.0, 10.0),
        //     egui::Vec2::new(8.0, 8.0),
        //     egui::Stroke::new(1.0, egui::Color32::BLACK),
        // );

        for (tdx, target, t, internal) in self.graph.transitions_out(idx) {
            self.show_transition(state.port_position(t.port1), tdx, target, t, internal, ui);
        }

        for child in self.graph.children(Some(idx)) {
            self.show_state(child, rect.min.to_vec2(), drag, &mut child_ui, commands);
        }

        // Background interaction, dragging states and context menu.

        // ui.input().pointer.primary_released() doesn't work?
        if drag.in_drag() {
            if ui.input().pointer.any_released() {
                match (&drag, ui.input().pointer.interact_pos()) {
                    (Drag::State(i, offset), Some(p)) => {
                        // Cannot drag into ourself.
                        if rect.contains(p) && *i != idx {
                            // Place the state relative to the parent with the offset.
                            let p = (p - rect.min - *offset).to_pos2();
                            commands.push(Command::MoveState(*i, idx, p));
                            *drag = Drag::None;
                        } else if root {
                            // If we haven't found a place to put this
                            // state and we're at the root, cancel it.
                            println!("cancel drag?");
                            *drag = Drag::None;
                        }
                    }
                    _ => (),
                }
            }
        } else {
            // Can't drag the root state.
            let sense = if !root {
                egui::Sense::click_and_drag()
            } else {
                egui::Sense::click()
            };

            let state_response = ui.interact(rect, id, sense);
            let dragged = state_response.dragged_by(egui::PointerButton::Primary);

            if dragged {
                if let Some(p) = ui.input().pointer.interact_pos() {
                    // Save the pointer offset from the state origin.
                    *drag = Drag::State(idx, p - rect.min);
                }
            } else {
                // Context menu on right click.
                state_response.context_menu(|ui| {
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
        }
    }

    // https://github.com/emilk/egui/discussions/1959 clicking the curve

    // sample drag to "paint" the curve?
    pub fn show_transition(
        &self,
        start: egui::Pos2,
        _tdx: transit::Tdx,
        target: transit::Idx,
        t: &Transition,
        _internal: bool,
        ui: &mut egui::Ui,
    ) -> Option<Command> {
        // draw port at start, dragging moves transition start
        ui.painter().circle_filled(start, 6.0, egui::Color32::BLACK);

        // need to select first to modify control points

        // draw curve, label, guard, internal toggle

        let stroke = egui::Stroke::new(2.0, egui::Color32::BLACK);

        let end = self.graph.state(target).unwrap().port_position(t.port2);

        let bezier = CubicBezierShape::from_points_stroke(
            [start, t.c1, t.c2, end],
            false,
            egui::Color32::BLACK,
            stroke,
        );

        // use sample to get the mid point for label

        ui.painter().add(bezier);

        //ui.painter().arrow(origin, vec, stroke)

        // draw port at end

        None
    }
}

impl State {
    const DEFAULT_SIZE: egui::Vec2 = egui::vec2(256.0, 64.0);

    // TODO: incoming vs outgoing
    pub fn port_position(&self, port: usize) -> egui::Pos2 {
        self.rect.min + egui::vec2(0.0, 10.0 * (port + 1) as f32)
    }
}
