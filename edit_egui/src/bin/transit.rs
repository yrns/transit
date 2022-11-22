use edit_egui::*;
use eframe::egui;

fn main() {
    //tracing_subscriber::fmt::init();

    let options = eframe::NativeOptions::default();
    eframe::run_native(
        "transit",
        options,
        Box::new(|_cc| Box::new(Transit::default())),
    );
}

#[derive(Clone, Default)]
struct EditContext {}

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

struct Transit {
    statechart: Statechart<EditContext>,
}

impl Default for Transit {
    fn default() -> Self {
        Self {
            statechart: edit_egui::Statechart {
                id: "untitled".into(),
                path: None,
                graph: transit::Graph::default(),
            },
        }
    }
}

impl eframe::App for Transit {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        egui::CentralPanel::default().show(ctx, |ui| self.statechart.show(ui));
    }
}
