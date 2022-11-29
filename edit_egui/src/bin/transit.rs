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

impl Transit {
    fn new(cc: &eframe::CreationContext<'_>) -> Self {
        if let Some(storage) = cc.storage {
            let mut transit: Transit =
                eframe::get_value(storage, eframe::APP_KEY).unwrap_or_default();
            // This imports the graph from the last saved path.
            transit.statechart.load().unwrap();
            transit
        } else {
            Self::default()
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
                        // TODO
                        ui.close_menu();
                    }
                    if ui.button("Save").clicked() {
                        // TODO: file dialog + error handling
                        match &self.statechart.path {
                            Some(p) => {
                                println!("saving to {:?}", p);
                                self.statechart.save().unwrap();
                            }
                            None => {
                                let p = PathBuf::new()
                                    .with_file_name(&self.statechart.id)
                                    .with_extension("ron");
                                if p.exists() {
                                    println!("path already exists! {:?}", p);
                                } else {
                                    println!("saving to {:?}", p);
                                    self.statechart.path = Some(p);
                                    self.statechart.save().unwrap();
                                }
                            }
                        }
                        ui.close_menu();
                    }
                    if ui.button("Save as...").clicked() {
                        // TODO
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
