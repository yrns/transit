pub struct Statechart<C: transit::Context> {
    pub id: String,
    pub path: Option<std::path::PathBuf>,
    //statechart: transit::Statechart<C>,
    pub graph: transit::Graph<C>,
}

#[derive(Clone)]
pub struct State {
    id: String,
    //enter: String,
    //exit: String,
    // relative to root?
    rect: egui::Rect,
}

// This is only used for the root state?
impl Default for State {
    fn default() -> Self {
        State {
            id: "untitled".into(),
            rect: egui::Rect::from_min_size(egui::pos2(10.0, 10.0), egui::vec2(128.0, 32.0)),
        }
    }
}

#[derive(Clone)]
pub struct Transition {
    // event? label?
    c1: egui::Pos2,
    c2: egui::Pos2,
}

#[derive(Debug)]
pub enum Response {
    AddState,
    RemoveState,
    UpdateState,
    MoveState,
    AddTransition,
    RemoveTransition,
    UpdateTransition,
    MoveTransition,
    SetInitial,
    SetEnter,
    SetExit,
    SetGuard,
}

impl<C: transit::Context> Statechart<C> {
    pub fn show(&self, ui: &mut egui::Ui) -> () {
        let rect = ui.max_rect();
        ui.allocate_rect(rect, egui::Sense::hover());
    }
}
