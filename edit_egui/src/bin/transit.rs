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

struct Transit {
    statechart: Statechart<EditContext>,
    drag: Drag,
}

impl Default for Transit {
    fn default() -> Self {
        Self {
            statechart: edit_egui::Statechart::new("untitled"),
            drag: Drag::None,
        }
    }
}

impl eframe::App for Transit {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        egui::CentralPanel::default().show(ctx, |ui| {
            let commands = self.statechart.show(&mut self.drag, ui);
            self.statechart.process_commands(commands);
        });
    }
}
