use edit_egui::*;
use eframe::{egui, rfd};

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

#[derive(serde::Deserialize, serde::Serialize)]
#[serde(default)]
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
            transit
        } else {
            Self::default()
        }
    }

    fn file_save_as(&mut self) {
        if let Some(p) = rfd::FileDialog::new()
            .set_file_name(&self.statechart.id)
            .add_filter("ron", &["ron"])
            .save_file()
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

    fn file_open(&mut self) {
        if let Some(p) = rfd::FileDialog::new()
            .add_filter("ron", &["ron"])
            .pick_file()
        {
            self.statechart = Default::default();
            self.statechart.path = Some(p);
            load(&mut self.statechart);
        }
    }
}

impl eframe::App for Transit {
    fn save(&mut self, storage: &mut dyn eframe::Storage) {
        eframe::set_value(storage, eframe::APP_KEY, self);
    }

    fn update(&mut self, ctx: &egui::Context, frame: &mut eframe::Frame) {
        #[cfg(not(target_arch = "wasm32"))]
        egui::TopBottomPanel::top("top_panel").show(ctx, |ui| {
            egui::menu::bar(ui, |ui| {
                ui.menu_button("File", |ui| {
                    if ui.button("New").clicked() {
                        self.statechart = Default::default();
                        ui.close_menu();
                    }
                    if ui.button("Open...").clicked() {
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
            let commands = self.statechart.show(ui);
            self.statechart.process_commands(commands);
        });
    }
}
