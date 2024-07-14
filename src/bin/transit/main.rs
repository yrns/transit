mod font;

use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

use transit::{
    edit_egui::*,
    source::{Locator, Source},
};

#[cfg(feature = "janet")]
use transit::source::janet;

#[cfg(feature = "rust")]
use transit::source::rust;

use eframe::egui;
use tracing_subscriber::{
    filter::{LevelFilter, Targets},
    fmt::Subscriber,
    layer::SubscriberExt,
    util::SubscriberInitExt,
};

// TODO wasm https://github.com/emilk/eframe_template

// SourceError?
#[derive(Debug, thiserror::Error)]
enum Error {
    #[cfg(feature = "janet")]
    #[error("janet: {0}")]
    Janet(#[from] janet::Error),
    #[cfg(feature = "rust")]
    #[error("rust: {0}")]
    Rust(#[from] rust::Error),
    #[error("unknown source type")]
    Unknown,
}

// This serializes (untagged) as a bare path. We deserialize based on the extension from the path.
#[derive(serde::Serialize, serde::Deserialize)]
#[serde(untagged)]
#[serde(try_from = "PathBuf")]
enum SourceType {
    #[cfg(feature = "janet")]
    Janet(janet::Source),
    #[cfg(feature = "rust")]
    Rust(rust::Source),
}

// TODO: Revisit using trait objects (Box<dyn Source>) using this crate:
// https://crates.io/crates/serde_flexitos. It uses a manual registry and does not require the
// inventory crate. And so it works on WASM.

// I tried enum_delegate and others and they didn't work...
impl transit::source::Source for SourceType {
    type Error = Error;

    fn path(&self) -> &Path {
        match self {
            #[cfg(feature = "janet")]
            SourceType::Janet(s) => s.path(),
            #[cfg(feature = "rust")]
            SourceType::Rust(s) => s.path(),
        }
    }

    fn normalize_symbol(&self, symbol: &str) -> String {
        match self {
            #[cfg(feature = "janet")]
            SourceType::Janet(s) => s.normalize_symbol(symbol),
            #[cfg(feature = "rust")]
            SourceType::Rust(s) => s.normalize_symbol(symbol),
        }
    }

    fn symbols(&mut self) -> Result<HashMap<String, Locator>, Self::Error> {
        match self {
            #[cfg(feature = "janet")]
            SourceType::Janet(s) => s.symbols().map_err(From::from),
            #[cfg(feature = "rust")]
            SourceType::Rust(s) => s.symbols().map_err(From::from),
        }
    }

    fn template(&self) -> &str {
        match self {
            #[cfg(feature = "janet")]
            SourceType::Janet(s) => s.template(),
            #[cfg(feature = "rust")]
            SourceType::Rust(s) => s.template(),
        }
    }

    fn description(&self) -> &str {
        match self {
            #[cfg(feature = "janet")]
            SourceType::Janet(s) => s.description(),
            #[cfg(feature = "rust")]
            SourceType::Rust(s) => s.description(),
        }
    }

    fn extensions(&self) -> &[&str] {
        match self {
            #[cfg(feature = "janet")]
            SourceType::Janet(s) => s.extensions(),
            #[cfg(feature = "rust")]
            SourceType::Rust(s) => s.extensions(),
        }
    }
}

impl TryFrom<PathBuf> for SourceType {
    type Error = Error;

    fn try_from(path: PathBuf) -> Result<Self, Self::Error> {
        let ext = path
            .extension()
            .and_then(std::ffi::OsStr::to_str)
            .ok_or(Error::Unknown)?;

        #[cfg(feature = "janet")]
        if janet::Source::EXT.iter().any(|e| e == &ext) {
            return Ok(Self::Janet(path.into()));
        }

        #[cfg(feature = "rust")]
        if rust::Source::EXT.iter().any(|e| e == &ext) {
            return Ok(Self::Rust(path.into()));
        }

        Err(Error::Unknown)
    }
}

struct Transit(transit::edit_egui::App<SourceType>);

fn main() -> eframe::Result {
    // Trace only transit crates.
    Subscriber::builder()
        // TODO: make these work in emacs' compilation buffer...
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

            Ok(Box::new(Transit(app)))
        }),
    )
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
