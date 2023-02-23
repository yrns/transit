use crate::*;
use eframe::egui;
use std::path::PathBuf;
use tracing::error;

pub const UNDO: KeyboardShortcut = KeyboardShortcut::new(Modifiers::CTRL, Key::Z);
pub const REDO: KeyboardShortcut =
    KeyboardShortcut::new(Modifiers::CTRL.plus(Modifiers::SHIFT), Key::Z);

#[derive(serde::Deserialize, serde::Serialize)]
#[serde(default)]
pub struct App<S> {
    /// Most-recently opened file.
    pub recent: Option<PathBuf>,
    #[serde(skip)] // De/serialized from/to `recent`.
    pub edit: Edit<S>,
    #[serde(skip)] // TODO configurable?
    pub editor: EmacsClient,
}

/// There is no default for Source.
impl<S> Default for App<S> {
    fn default() -> Self {
        Self {
            recent: None,
            edit: Edit::default(),
            editor: EmacsClient::default(),
        }
    }
}

impl<S> App<S>
where
    S: Source + serde::de::DeserializeOwned + serde::Serialize,
{
    pub fn load(&mut self) {
        // Load recent file.
        if let Some(path) = &self.recent {
            match Edit::load(path) {
                Ok(edit) => {
                    info!("loaded from recent: {}", path.display());
                    self.edit = edit;
                }
                Err(e) => error!("error: {:?}", e),
            }
        }
    }

    /// Select a path and save.
    pub fn file_save_as(&mut self) {
        if let Ok(Some(p)) = native_dialog::FileDialog::new()
            .set_filename(self.edit.id())
            .add_filter("ron", &["ron"])
            .show_save_single_file()
        {
            match self.edit.save(&p) {
                Ok(_) => {
                    info!("file saved: {p:?}");
                    self.recent = Some(p);
                }
                Err(e) => error!("error saving: {e:?}"),
            }
        }
    }

    /// Save to recent path. Else, save as.
    pub fn file_save(&mut self) {
        if let Some(p) = &self.recent {
            match self.edit.save(p) {
                Ok(_) => info!("file saved: {}", p.display()),
                Err(e) => error!("error saving: {e:?}"),
            }
        } else {
            self.file_save_as();
        }
    }

    /// Open a new file, replacing current.
    pub fn file_open(&mut self) {
        match native_dialog::FileDialog::new()
            .add_filter("ron", &["ron"])
            .show_open_single_file()
        {
            Ok(Some(p)) => match Edit::load(&p) {
                Ok(edit) => {
                    self.recent = Some(p);
                    self.edit = edit;
                }
                Err(e) => error!("error loading: {:?}", e),
            },
            Err(e) => error!("error: {:?}", e),
            _ => (),
        }
    }

    pub fn update(&mut self, ctx: &egui::Context) -> bool {
        // Need unique ids for each open statechart.
        let mut clear_state = false;

        // Check for updates to the source.
        if let Some(ref mut source) = self.edit.source {
            source.update().unwrap_or_else(|e| error!("{e:?}"));
        }

        let mut quit = false;

        if ctx.input_mut(|i| i.consume_shortcut(&UNDO)) {
            self.edit.undo();
        } else if ctx.input_mut(|i| i.consume_shortcut(&REDO)) {
            self.edit.redo();
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
                    if ui
                        .add_enabled(
                            self.edit.undo.can_undo(),
                            Button::new("Undo").shortcut_text(ui.ctx().format_shortcut(&UNDO)),
                        )
                        .clicked()
                    {
                        self.edit.undo();
                    }
                    if ui
                        .add_enabled(
                            self.edit.undo.can_redo(),
                            Button::new("Redo").shortcut_text(ui.ctx().format_shortcut(&REDO)),
                        )
                        .clicked()
                    {
                        self.edit.redo();
                    }
                    if ui.button("Quit").clicked() {
                        // TODO: prompt to save?
                        quit = true;
                    }
                });
            });
        });

        egui::CentralPanel::default().show(ctx, |ui| {
            if clear_state {
                ctx.data_mut(|d| d.remove::<EditData>(ui.id()));
            }
            let mut commands = self.edit.show(ui);

            // Process editor commands.
            commands.retain(|command| match command {
                Command::GotoSymbol(symbol, path, loc) => {
                    if let Err(e) = self.editor.goto(symbol, path, *loc) {
                        error!("goto failed: {e:?}");
                    }
                    false
                }
                Command::InsertSymbol(symbol, path, template) => {
                    if let Err(e) = self.editor.insert(symbol, path, template) {
                        error!("insert failed: {e:?}");
                    }
                    false
                }
                _ => true,
            });

            self.edit.process_commands(commands);
        });

        quit
    }
}
