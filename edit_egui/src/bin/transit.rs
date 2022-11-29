use edit_egui::*;
use eframe::egui;

// TODO wasm https://github.com/emilk/eframe_template

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
}

impl Default for Transit {
    fn default() -> Self {
        Self {
            statechart: edit_egui::Statechart::new("untitled"),
        }
    }
}

impl eframe::App for Transit {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        egui::CentralPanel::default().show(ctx, |ui| {
            let commands = self.statechart.show(ui);
            self.statechart.process_commands(commands);
        });
    }
}
