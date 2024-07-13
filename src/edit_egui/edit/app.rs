use std::{
    path::PathBuf,
    sync::{Arc, Mutex},
};

use super::*;

pub const UNDO: KeyboardShortcut = KeyboardShortcut::new(Modifiers::CTRL, Key::Z);
pub const REDO: KeyboardShortcut =
    KeyboardShortcut::new(Modifiers::CTRL.plus(Modifiers::SHIFT), Key::Z);
pub const QUIT: KeyboardShortcut = KeyboardShortcut::new(Modifiers::CTRL, Key::Q);

//impl<T: Source + ?Sized> Source for Box<T> {}

//#[derive(Default)]
#[cfg_attr(feature = "serde", derive(serde::Deserialize, serde::Serialize))]
#[serde(default)]
pub struct App<S> {
    /// Most-recently opened file.
    pub recent: Option<PathBuf>,
    #[serde(skip)] // De/serialized from/to `recent`.
    pub edit: Edit<S>,
    /// Watches source files for changes.
    #[serde(skip)]
    pub watcher: Option<Watcher>,
    #[serde(skip)] // TODO configurable?
    pub editor: EmacsClient,
    #[serde(skip)]
    pub base_dirs: Option<directories::BaseDirs>,
}

/// There is no default for Source.
impl<S> Default for App<S> {
    fn default() -> Self {
        Self {
            recent: None,
            edit: Edit::default(),
            watcher: None,
            editor: EmacsClient::default(),
            base_dirs: directories::BaseDirs::new(),
        }
    }
}

impl<S> App<S>
where
    S: TryFrom<PathBuf> + Source + serde::de::DeserializeOwned + serde::Serialize,
{
    pub fn init(&mut self) {
        // Load recent file.
        if let Some(path) = &self.recent {
            match self.load(path.as_path()) {
                Ok(edit) => {
                    info!("loaded from recent: {}", path.display());
                    self.edit = edit;
                    self.source_changed();
                }
                Err(e) => error!("error loading recent file: {path:?}: {e:?}"),
            }
        }
    }

    pub fn load(&self, path: &Path) -> Result<Edit<S>, Error> {
        Edit::load_and_validate(path)
    }

    /// Select a path and save.
    pub fn file_save_as(&mut self) {
        if let Some(p) = rfd::FileDialog::new()
            .set_file_name(self.edit.id())
            .add_filter("ron", &["ron"])
            .pick_file()
        {
            match self.edit.save(&p) {
                Ok(_) => {
                    info!("file saved: {p:?}");
                    self.recent = Some(p);
                }
                Err(e) => error!("error saving: {e:?}"),
            }
        } else {
            info!("no file picked");
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
        match rfd::FileDialog::new()
            .add_filter("ron", &["ron"])
            .pick_file()
        {
            // TODO call load
            Some(p) => match Edit::load_and_validate(&p) {
                Ok(edit) => {
                    self.recent = Some(p);
                    self.edit = edit;
                }
                Err(e) => error!("error loading: {:?}", e),
            },
            _ => info!("no file picked"),
        }
    }

    pub fn update(&mut self, ctx: &egui::Context) {
        // Need unique ids for each open statechart.
        let mut clear_state = false;

        // Check for updates to the source.
        if self.watcher.as_mut().is_some_and(|w| w.changed()) {
            self.source_changed();
        }

        if ctx.input_mut(|i| i.consume_shortcut(&UNDO)) {
            self.edit.undo();
        } else if ctx.input_mut(|i| i.consume_shortcut(&REDO)) {
            self.edit.redo();
        } else if ctx.input_mut(|i| i.consume_shortcut(&QUIT)) {
            ctx.send_viewport_cmd(egui::ViewportCommand::Close);
        }

        // TODO: https://github.com/emilk/egui/blob/master/examples/confirm_exit/src/main.rs
        if ctx.input(|i| i.viewport().close_requested()) {
            info!("close requested");
        }

        #[cfg(not(target_arch = "wasm32"))]
        egui::TopBottomPanel::top("top_panel").show(ctx, |ui| {
            egui::menu::bar(ui, |ui| {
                // Rename "Transit"?
                ui.menu_button("File", |ui| {
                    if ui.button("New").clicked() {
                        clear_state = true;
                        self.edit = Default::default();
                        self.recent = None;
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
                    if ui
                        .add(Button::new("Quit").shortcut_text(ui.ctx().format_shortcut(&QUIT)))
                        .clicked()
                    {
                        // TODO: prompt to save?
                        ui.ctx().send_viewport_cmd(egui::ViewportCommand::Close);
                    }
                });
            });
        });

        // The root state already has a frame and we want to draw up to the edges (no margins).
        let cp = egui::CentralPanel::default().frame(Frame::none());
        cp.show(ctx, |ui| {
            if clear_state {
                ctx.data_mut(|d| d.remove::<EditData>(ui.id()));
            }

            // Keep edit data in temp storage. So it persists?
            let edit_data = ui.data_mut(|d| d.get_temp(ui.id()));
            let edit_data = edit_data.unwrap_or_else(|| {
                let data = Arc::new(Mutex::new(EditData::new()));
                ui.data_mut(|d| d.insert_temp(ui.id(), data.clone()));
                data
            });
            let mut edit_data = edit_data.lock().unwrap();

            if let Some(p) = ui.ctx().pointer_interact_pos() {
                // Set drag target if dragging.
                self.edit.set_drag_target(&mut edit_data, p);

                // Resolve any current drag and update immediately so we're drawing the most up to date
                // state. Otherwise we get one frame of flickering when things are moved.
                if ui.input(|i| i.pointer.any_released()) {
                    let drag_transition = edit_data.drag_transition.take();
                    self.edit.resolve_drag(&mut edit_data, drag_transition, p);
                    self.edit.process_commands(edit_data.commands.drain(..));
                }
            }

            egui::ScrollArea::both()
                .scroll_bar_visibility(scroll_area::ScrollBarVisibility::AlwaysVisible)
                .auto_shrink(false)
                .show(ui, |ui| {
                    self.edit.show(
                        &mut edit_data,
                        self.base_dirs.as_ref().map(|b| b.home_dir()),
                        ui,
                    );
                });

            // Process editor commands.
            edit_data.commands.retain(|command| match command {
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
                Command::SelectSourcePath(p) => {
                    self.select_source(p.clone());
                    false
                }
                _ => true,
            });

            self.edit.process_commands(edit_data.commands.drain(..));
        });
    }

    fn source_changed(&mut self) {
        if let Some(source) = &mut self.edit.source {
            self.watcher = Watcher::new(source.path())
                .map_err(|e| error!(?e, "error creating watcher"))
                .ok();

            match source.symbols() {
                Ok(s) => self.edit.symbols = s,
                Err(e) => error!(?e, "error generating symbols"),
            }
        } else {
            self.edit.symbols.clear();
        }
    }

    fn select_source(&mut self, path: PathBuf) {
        match S::try_from(path) {
            Ok(s) => {
                self.edit.source = Some(s);
                self.source_changed();
            }
            Err(_) => error!("no supported source type"),
        }
    }
}
