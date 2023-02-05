use std::path::PathBuf;

use edit_egui::*;
use eframe::egui;

// TODO wasm https://github.com/emilk/eframe_template

fn main() {
    //tracing_subscriber::fmt::init();

    let options = eframe::NativeOptions::default();
    eframe::run_native(
        "transit",
        options,
        Box::new(|cc| Box::new(Transit::new(cc))),
    );
}

#[derive(Default, serde::Deserialize, serde::Serialize)]
#[serde(default)]
struct Transit {
    /// Most-recently opened file.
    recent: Option<PathBuf>,
    #[serde(skip)] // De/serialized from/to `recent`.
    edit: Edit,
    #[serde(skip)] // TODO configurable?
    editor: EmacsClient,
}

impl Transit {
    fn new(cc: &eframe::CreationContext<'_>) -> Self {
        if let Some(storage) = cc.storage {
            let mut transit: Transit =
                eframe::get_value(storage, eframe::APP_KEY).unwrap_or_default();

            // Load recent file.
            if let Some(path) = &transit.recent {
                match Edit::load(path) {
                    Ok(edit) => transit.edit = edit,
                    Err(e) => println!("error: {:?}", e),
                }
            }
            transit
        } else {
            Self::default()
        }
    }

    /// Select a path and save.
    fn file_save_as(&mut self) {
        if let Ok(Some(p)) = native_dialog::FileDialog::new()
            .set_filename(&self.edit.id())
            .add_filter("ron", &["ron"])
            .show_save_single_file()
        {
            println!("saving to {:?}", p);
            match self.edit.save(&p) {
                Ok(_) => self.recent = Some(p),
                Err(e) => println!("error saving: {:?}", e),
            }
        }
    }

    /// Save to recent path. Else, save as.
    fn file_save(&mut self) {
        if let Some(p) = &self.recent {
            if let Err(e) = self.edit.save(p) {
                println!("failed to save: {:?}", e);
            }
        } else {
            self.file_save_as();
        }
    }

    /// Open a new file, replacing current.
    fn file_open(&mut self) {
        match native_dialog::FileDialog::new()
            .add_filter("ron", &["ron"])
            .show_open_single_file()
        {
            Ok(Some(p)) => match Edit::load(&p) {
                Ok(edit) => {
                    self.recent = Some(p);
                    self.edit = edit;
                }
                Err(e) => println!("error loading: {:?}", e),
            },
            Err(e) => println!("error: {:?}", e),
            _ => (),
        }
    }
}

impl eframe::App for Transit {
    // Prompt for save?
    fn on_close_event(&mut self) -> bool {
        dbg!("on close");
        true // can close
    }

    /// Save recent path.
    fn save(&mut self, storage: &mut dyn eframe::Storage) {
        eframe::set_value(storage, eframe::APP_KEY, self);
    }

    fn update(&mut self, ctx: &egui::Context, frame: &mut eframe::Frame) {
        // Need unique ids for each open statechart.
        let mut clear_state = false;

        // Check for updates to the source.
        if let Some(ref mut source) = self.edit.source {
            source.update();
        }

        #[cfg(not(target_arch = "wasm32"))]
        egui::TopBottomPanel::top("top_panel").show(ctx, |ui| {
            egui::menu::bar(ui, |ui| {
                ui.menu_button("File", |ui| {
                    if ui.button("New").clicked() {
                        clear_state = true;
                        self.edit = Default::default();
                        ui.close_menu();
                    }
                    if ui.button("Open...").clicked() {
                        clear_state = true;
                        self.file_open();
                        ui.close_menu();
                    }
                    if ui.button("Save").clicked() {
                        self.file_save();
                        ui.close_menu();
                    }
                    if ui.button("Save as...").clicked() {
                        self.file_save_as();
                        ui.close_menu();
                    }
                    if ui.button("Quit").clicked() {
                        // TODO: prompt to save?
                        frame.close();
                    }
                });
            });
        });

        egui::CentralPanel::default().show(ctx, |ui| {
            if clear_state {
                ctx.data().remove::<EditData>(ui.id());
            }
            let mut commands = self.edit.show(ui);

            // Process editor commands.
            commands.retain(|command| match command {
                Command::GotoSymbol(symbol, path, loc) => {
                    self.editor.goto(symbol, path, *loc).unwrap();
                    false
                }
                _ => true,
            });

            self.edit.process_commands(commands);
        });
    }
}
