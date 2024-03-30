use edit_egui as edit;
use eframe::egui;
use tracing_subscriber::{
    filter::{LevelFilter, Targets},
    fmt::Subscriber,
    layer::SubscriberExt,
    util::SubscriberInitExt,
};

// TODO wasm https://github.com/emilk/eframe_template

struct App(edit::App<janet::Source>);

fn main() {
    // Trace only transit crates.
    Subscriber::builder()
        // TODO: make these work in emacs' compilation buffer
        .with_file(true)
        .with_line_number(true)
        .finish()
        .with(Targets::new().with_targets(vec![
            ("transit", LevelFilter::INFO),
            ("edit_egui", LevelFilter::INFO),
            ("janet", LevelFilter::INFO),
        ]))
        .try_init()
        .expect("tracing");

    let options = eframe::NativeOptions::default();

    eframe::run_native(
        "transit",
        options,
        Box::new(|cc| {
            // Set dark theme w/ smaller shadows.
            let mut visuals = egui::Visuals::dark();
            visuals.popup_shadow = egui::epaint::Shadow {
                offset: egui::Vec2::new(4.0, 4.0),
                blur: 8.0,
                spread: 0.0,
                color: egui::Color32::from_black_alpha(64),
            };
            cc.egui_ctx.set_visuals(visuals);

            let mut app: edit::App<janet::Source> = cc
                .storage
                .and_then(|storage| eframe::get_value(storage, eframe::APP_KEY))
                .unwrap_or_default();

            app.load();

            Box::new(App(app))
        }),
    )
    .expect("run")
}

impl eframe::App for App {
    /// Save recent path.
    fn save(&mut self, storage: &mut dyn eframe::Storage) {
        eframe::set_value(storage, eframe::APP_KEY, &self.0);
    }

    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        self.0.update(ctx);
    }
}
