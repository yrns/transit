use std::fs::read;

use eframe::{
    egui::{Context, FontData, FontDefinitions},
    epaint::FontFamily,
};
use font_kit::{
    family_name::FamilyName, handle::Handle, properties::Properties, source::SystemSource,
};

// https://github.com/emilk/egui/discussions/1344

pub(crate) fn load_system_font(ctx: &Context) {
    let mut fonts = FontDefinitions::default();

    let handle = SystemSource::new()
        .select_best_match(&[FamilyName::SansSerif], &Properties::new())
        .unwrap();

    let buf: Vec<u8> = match handle {
        Handle::Memory { bytes, .. } => bytes.to_vec(),
        Handle::Path { path, .. } => read(path).unwrap(),
    };

    const FONT_SYSTEM_SANS_SERIF: &'static str = "System Sans Serif";

    fonts
        .font_data
        .insert(FONT_SYSTEM_SANS_SERIF.to_owned(), FontData::from_owned(buf));

    if let Some(vec) = fonts.families.get_mut(&FontFamily::Proportional) {
        vec.push(FONT_SYSTEM_SANS_SERIF.to_owned());
    }

    if let Some(vec) = fonts.families.get_mut(&FontFamily::Monospace) {
        vec.push(FONT_SYSTEM_SANS_SERIF.to_owned());
    }

    ctx.set_fonts(fonts);
}
