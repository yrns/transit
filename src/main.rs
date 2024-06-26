mod font;

use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

use edit_egui::*;

use eframe::egui;
use tracing_subscriber::{
    filter::{LevelFilter, Targets},
    fmt::Subscriber,
    layer::SubscriberExt,
    util::SubscriberInitExt,
};

// TODO wasm https://github.com/emilk/eframe_template

#[derive(Debug, thiserror::Error)]
enum Error {
    #[error("janet: {0}")]
    Janet(#[from] janet::Error),
    #[error("rust: {0}")]
    Rust(#[from] rust::Error),
}

#[derive(serde::Serialize, serde::Deserialize)]
enum SourceType {
    Janet(janet::Source),
    Rust(rust::Source),
}

// I tried enum_delegate and others and they didn't work...
impl Source for SourceType {
    type Error = Error;

    fn path(&self) -> &Path {
        match self {
            SourceType::Janet(s) => s.path(),
            SourceType::Rust(s) => s.path(),
        }
    }

    fn normalize_symbol(&self, symbol: &str) -> String {
        match self {
            SourceType::Janet(s) => s.normalize_symbol(symbol),
            SourceType::Rust(s) => s.normalize_symbol(symbol),
        }
    }

    fn symbols(&mut self) -> Result<HashMap<String, Locator>, Self::Error> {
        match self {
            SourceType::Janet(s) => s.symbols().map_err(From::from),
            SourceType::Rust(s) => s.symbols().map_err(From::from),
        }
    }

    fn template(&self) -> &str {
        match self {
            SourceType::Janet(s) => s.template(),
            SourceType::Rust(s) => s.template(),
        }
    }

    fn description(&self) -> &str {
        match self {
            SourceType::Janet(s) => s.description(),
            SourceType::Rust(s) => s.description(),
        }
    }

    fn extensions(&self) -> &[&str] {
        match self {
            SourceType::Janet(s) => s.extensions(),
            SourceType::Rust(s) => s.extensions(),
        }
    }
}

impl SelectSource for SourceType {
    fn select(path: PathBuf) -> Option<Self> {
        if path.extension().is_some_and(|e| e.to_str() == Some("rs")) {
            path.clone().try_into().ok().map(Self::Rust)
        } else if path
            .extension()
            .is_some_and(|e| e.to_str() == Some("janet"))
        {
            Some(Self::Janet(path.clone().into()))
        } else {
            None
        }
    }
}

// impl From<PathBuf> for SourceType {
//     fn from(path: PathBuf) -> Self {
//         Self::select(path).unwrap()
//     }
// }

struct Transit(edit_egui::App<SourceType>);

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

            font::load_system_font(&cc.egui_ctx);

            let mut app: App<SourceType> = cc
                .storage
                .and_then(|storage| eframe::get_value(storage, eframe::APP_KEY))
                .unwrap_or_default();

            app.init();

            Box::new(Transit(app))
        }),
    )
    .expect("run")
}

impl eframe::App for Transit {
    /// Save recent path.
    fn save(&mut self, storage: &mut dyn eframe::Storage) {
        eframe::set_value(storage, eframe::APP_KEY, &self.0);
    }

    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        self.0.update(ctx);
    }
}
