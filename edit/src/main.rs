#![allow(unused_imports, dead_code)]

mod widgets;

pub use crate::widgets::Root;
use anyhow::{anyhow, Result};
use druid::lens::{self, LensExt};
use druid::theme;
use druid::widget::{
    Align, Button, Container, Flex, Label, List, Padding, Scroll, SizedBox, WidgetExt,
};
use druid::{
    AppDelegate, AppLauncher, Color, Command, Data, DelegateCtx, Env, Event, FileInfo, Lens,
    LocalizedString, MenuDesc, Point, Rect, Selector, Target, UnitPoint, Widget, WidgetId,
    WidgetPod, WindowDesc, WindowId,
};
use log;
use std::collections::HashMap;
use std::fs::File;
use std::io::prelude::*;
use std::path::PathBuf;
use std::sync::Arc;
use transit::{Graph, Idx, State, Transition};

const RESET: Selector = Selector::new("transit-reset");

#[derive(Clone, Data, Lens)]
struct EditData {
    // we only have one graph editable at a time for now
    graph1: GraphData,
    // command to run when editing an action (emacsclient, etc.)
    graph1_str: String,
    #[druid(ignore)]
    edit_action: Option<String>,
    // command to run when renaming an action
    #[druid(ignore)]
    rename_action: Option<String>,
}

#[derive(Clone, Data, Lens)]
struct GraphData {
    graph: Arc<Graph>,
    // path to file on disk
    #[druid(ignore)]
    path: Option<PathBuf>,
    // the state widgets store a state index, but we can't access the
    // widgets directly - this gives us widget id -> state index
    #[druid(ignore)]
    wids: HashMap<WidgetId, Idx>,
}

impl Default for EditData {
    fn default() -> Self {
        EditData {
            graph1: GraphData::new(),
            graph1_str: "this is a test".to_string(),
            edit_action: None,
            rename_action: None,
        }
    }
}

impl EditData {
    fn reset(&mut self) {
        self.graph1 = GraphData::new();
        // TODO: Default
        self.edit_action = None;
        self.rename_action = None;
    }
}

// we need to do layout in the data here so that layout in the widget can just read the data
// we can't update the data in update
impl GraphData {
    pub fn new() -> Self {
        Self {
            graph: Arc::new(Graph::new("untitled")),
            path: None,
            // we're only doing this since we aren't storing state ids
            // in DragData, maybe make the state id a type parameter? TODO:
            wids: HashMap::new(),
        }
    }

    pub fn from_graph(graph: Graph) -> Self {
        Self {
            graph: Arc::new(graph),
            path: None,
            wids: HashMap::new(),
        }
    }

    // make a unique id
    fn unique_id(&self, id: &str, parent: Option<Idx>) -> Result<Option<String>> {
        if self.graph.is_unique_id(parent, &id) {
            Ok(None)
        } else {
            for n in 1..10 {
                let id = format!("{}-{}", id, n);
                if self.graph.is_unique_id(parent, &id) {
                    return Ok(Some(id));
                }
            }
            Err(anyhow!("failed to make unique id"))
        }
    }

    pub fn add_state(&mut self, id: &str, parent: Option<Idx>) -> Result<()> {
        let uid = self.unique_id(id, parent)?;
        let g = Arc::make_mut(&mut self.graph);
        g.add_state(State::new(
            uid.unwrap_or_else(|| id.to_string()),
            parent,
            None,
            None,
        ));
        Ok(())
    }

    pub fn move_state(&mut self, parent: Option<Idx>, i: Idx, rect: Rect) -> Result<()> {
        let rect = if let Some(p) = parent {
            fit_rect(self.graph.get(p).edit_data.rect, rect)
        } else {
            rect
        };

        // we only want a new id if it's a new parent and the current
        // id won't be unique
        let id = {
            let p0 = self.graph.get(i).parent;
            if p0 != parent {
                let id = &self.graph.get(i).id;
                self.unique_id(id, parent)?
            } else {
                None
            }
        };
        let s = Arc::make_mut(&mut self.graph).get_mut(i);
        s.parent = parent;
        if let Some(id) = id {
            s.id = id;
        }
        s.edit_data.set_rect(rect);
        Ok(())
    }
}

// fit rect b into a
fn fit_rect(a: Rect, b: Rect) -> Rect {
    b.with_origin(Point::new(
        b.x0.min(a.width() - b.width()).max(0.),
        b.y0.min(a.height() - b.height()).max(0.),
    ))
}

fn main() {
    let main_window = WindowDesc::new(ui_builder)
        .menu(make_menu(&EditData::default()))
        // TODO update with graph name and file path
        .title(LocalizedString::new("transit-window-title").with_placeholder("transit"));

    // start with a blank graph
    let data = EditData::default();

    AppLauncher::with_window(main_window)
        // black on white
        .configure_env(|env, _| {
            env.set(theme::FONT_NAME, "serif");
            env.set(theme::SELECTION_COLOR, Color::rgb8(0xA6, 0xCC, 0xFF));
            env.set(theme::WINDOW_BACKGROUND_COLOR, Color::WHITE);
            env.set(theme::LABEL_COLOR, Color::BLACK);
            env.set(theme::CURSOR_COLOR, Color::rgb8(0xFF, 0x33, 0x33));
            env.set(theme::BACKGROUND_LIGHT, Color::WHITE);
            env.set(theme::BACKGROUND_DARK, Color::rgb8(230, 230, 230));
        })
        //.debug_paint_layout()
        .use_simple_logger()
        .delegate(Delegate)
        .launch(data)
        .expect("launch failed");
}

fn ui_builder() -> impl Widget<EditData> {
    Flex::column()
        .with_child(Scroll::new(Root::new(None)), 1.0)
        .with_child(
            // show path to hovered state, key commands, etc.
            Label::new(|data: &EditData, _env: &_| format!("{}", data.graph1.graph.id))
                .fix_height(40.),
            0.0,
        )
}

struct Delegate;

impl AppDelegate<EditData> for Delegate {
    fn event(
        &mut self,
        _ctx: &mut DelegateCtx,
        _window_id: WindowId,
        event: Event,
        _data: &mut EditData,
        _env: &Env,
    ) -> Option<Event> {
        Some(event)
    }

    fn command(
        &mut self,
        ctx: &mut DelegateCtx,
        target: &Target,
        cmd: &Command,
        data: &mut EditData,
        _env: &Env,
    ) -> bool {
        match &cmd.selector {
            &druid::commands::NEW_FILE => {
                // TODO: save unsaved changes? no modals
                data.reset();
                ctx.submit_command(RESET, None);
                false
            }
            &druid::commands::OPEN_FILE => {
                // the command contains the path from the open panel
                match cmd.get_object::<FileInfo>() {
                    Ok(f) => match Graph::import_from_file(f.path()) {
                        Ok(g) => {
                            data.graph1 = GraphData::from_graph(g);
                            data.graph1.path = Some(f.path().to_path_buf());
                            ctx.submit_command(RESET, None);
                        }
                        Err(e) => log::error!("failed to export graph: {:?}", e),
                    },
                    Err(_) => {
                        ctx.submit_command(druid::commands::SHOW_OPEN_PANEL, *target);
                    }
                };
                false
            }
            &druid::commands::SAVE_FILE => {
                // the command contains the path from the "save as" panel
                let path = match cmd.get_object::<FileInfo>() {
                    Ok(f) => {
                        data.graph1.path = Some(f.path().to_path_buf());
                        f.path()
                    }
                    // else use the path stored in the edit data
                    Err(_) => {
                        if let Some(ref path) = data.graph1.path {
                            path
                        } else {
                            // else present the save panel
                            ctx.submit_command(druid::commands::SHOW_SAVE_PANEL, *target);
                            return false;
                        }
                    }
                };

                if let Err(e) = data.graph1.graph.export_to_file(path) {
                    log::error!("failed to export graph: {:?}", e);
                }

                false
            }
            // TODO:
            &druid::commands::UNDO => false,
            &druid::commands::REDO => false,
            _ => true,
        }
    }
}

#[allow(unused_assignments)]
fn make_menu<T: Data>(_data: &EditData) -> MenuDesc<T> {
    let mut base = MenuDesc::empty();
    #[cfg(target_os = "macos")]
    {
        base = druid::platform_menus::mac::menu_bar();
    }
    #[cfg(any(target_os = "windows", target_os = "linux"))]
    {
        base = base.append(druid::platform_menus::win::file::default());
    }
    base
}
