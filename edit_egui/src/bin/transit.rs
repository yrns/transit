use eframe::egui;

// TODO wasm https://github.com/emilk/eframe_template

struct App(edit_egui::app::App);

fn main() {
    //tracing_subscriber::fmt::init();

    let options = eframe::NativeOptions::default();

    eframe::run_native(
        "transit",
        options,
        Box::new(|cc| {
            // Set dark theme w/ smaller shadows.
            let mut visuals = egui::Visuals::dark();
            visuals.popup_shadow = egui::epaint::Shadow {
                extrusion: 4.0,
                color: egui::Color32::from_black_alpha(64),
            };
            cc.egui_ctx.set_visuals(visuals);

            let mut app: edit_egui::app::App = cc
                .storage
                .and_then(|storage| eframe::get_value(storage, eframe::APP_KEY))
                .unwrap_or_default();

            app.load();

            Box::new(App(app))
        }),
    );
}

impl eframe::App for App {
    // Prompt for save?
    fn on_close_event(&mut self) -> bool {
        dbg!("on close");
        true // can close
    }

    /// Save recent path.
    fn save(&mut self, storage: &mut dyn eframe::Storage) {
        eframe::set_value(storage, eframe::APP_KEY, &self.0);
    }

    fn update(&mut self, ctx: &egui::Context, frame: &mut eframe::Frame) {
        if self.0.update(ctx) {
            frame.close();
        }
    }
}
