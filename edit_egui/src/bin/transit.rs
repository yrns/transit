use edit_egui::{source::*, *};
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
    statechart: Statechart<EditContext>,
}

fn load(s: &mut Statechart<EditContext>) {
    match s.load() {
        Err(e) => println!("failed to load {:?}: {}", s.path, e),
        _ => (),
    }
}

impl Transit {
    fn new(cc: &eframe::CreationContext<'_>) -> Self {
        if let Some(storage) = cc.storage {
            let mut transit: Transit =
                eframe::get_value(storage, eframe::APP_KEY).unwrap_or_default();
            // This imports the graph from the last saved path.
            load(&mut transit.statechart);
            // Start watching source path.
            transit.statechart.source = transit
                .statechart
                .source_path
                .as_ref()
                .map(|p| Source::new(p));
            transit
        } else {
            Self::default()
        }
    }

    // TODO: errors
    fn file_save_as(&mut self) {
        if let Ok(Some(p)) = native_dialog::FileDialog::new()
            .set_filename(&self.statechart.id())
            .add_filter("ron", &["ron"])
            .show_save_single_file()
        {
            println!("saving to {:?}", p);
            self.statechart.path = Some(p);
            self.save();
        }
    }

    fn save(&mut self) {
        match self.statechart.save() {
            Err(e) => println!("failed to save {:?}: {}", &self.statechart.path, e),
            _ => (),
        }
    }

    // TODO: errors
    fn file_open(&mut self) {
        if let Ok(Some(p)) = native_dialog::FileDialog::new()
            .add_filter("ron", &["ron"])
            .show_open_single_file()
        {
            self.statechart = Default::default();
            self.statechart.path = Some(p);
            load(&mut self.statechart);
        }
    }
}

impl eframe::App for Transit {
    // Prompt for save?
    fn on_close_event(&mut self) -> bool {
        dbg!("on close");
        true // can close
    }

    fn save(&mut self, storage: &mut dyn eframe::Storage) {
        eframe::set_value(storage, eframe::APP_KEY, self);
    }

    fn update(&mut self, ctx: &egui::Context, frame: &mut eframe::Frame) {
        // Need unique ids for each open statechart.
        let mut clear_state = false;

        // Check for updates to the source.
        if let Some(ref mut source) = self.statechart.source {
            source.update();
        }

        #[cfg(not(target_arch = "wasm32"))]
        egui::TopBottomPanel::top("top_panel").show(ctx, |ui| {
            egui::menu::bar(ui, |ui| {
                ui.menu_button("File", |ui| {
                    if ui.button("New").clicked() {
                        clear_state = true;
                        self.statechart = Default::default();
                        ui.close_menu();
                    }
                    if ui.button("Open...").clicked() {
                        clear_state = true;
                        self.file_open();
                        ui.close_menu();
                    }
                    if ui.button("Save").clicked() {
                        match &self.statechart.path {
                            Some(p) => {
                                println!("saving to {:?}", p);
                                self.save()
                            }
                            None => self.file_save_as(),
                        }
                        ui.close_menu();
                    }
                    if ui.button("Save as...").clicked() {
                        self.file_save_as();
                        ui.close_menu();
                    }
                    if ui.button("Quit").clicked() {
                        // TODO: promp to save?
                        frame.close();
                    }
                });
            });
        });

        egui::CentralPanel::default().show(ctx, |ui| {
            if clear_state {
                // TODO: StatechartData? You can't be in-drag when
                // using the menus, but we want to clear selection too.
                ctx.data().remove::<Drag>(ui.id());
            }
            let commands = self.statechart.show(ui);
            self.statechart.process_commands(commands);
        });
    }
}
