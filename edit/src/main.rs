#![allow(unused_imports, dead_code)]

mod sync;
mod widgets;

use crate::widgets::{FilePath, Root, StateIdLens};
use anyhow::{anyhow, Result};
use druid::{kurbo::*, lens, lens::*, piet::*, theme, widget::*, *};
use log;
use ron::de::from_reader;
use ron::ser::{to_string_pretty, PrettyConfig};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs::File;
use std::io::prelude::*;
use std::io::prelude::*;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use transit::{Graph, GraphEditData, Idx, PathData, TransIdx};

const RESET: Selector = Selector::new("transit.edit.reset");
const SELECT_SRC: Selector = Selector::new("transit.edit.select-src");

#[derive(Clone, Data, Lens)]
pub struct EditData {
    // we only have one graph editable at a time for now
    graph1: GraphData,
    #[druid(ignore)]
    conf: EditConf,
    #[druid(ignore)]
    // TODO: which graph, if/when applicable
    select_src: bool,
}

#[derive(Clone)]
struct History {
    graph: Arc<Graph>,
    hint: &'static str,
}

#[derive(Clone, Data, Lens)]
struct GraphData {
    graph: Arc<Graph>,
    // path to file on disk
    #[druid(ignore)]
    path: Option<PathBuf>,
    // the state widgets store a state index, but we can't access the
    // widgets directly - this gives us widget id -> state index
    // TODO: remove this? we don't need it for dragging anymore
    #[druid(ignore)]
    wids: HashMap<WidgetId, Idx>,
    #[druid(ignore)]
    history: Vec<History>,
}

impl EditData {
    fn new(conf: EditConf) -> Self {
        EditData {
            graph1: conf
                .last_open_file
                .as_ref()
                .map(|p| GraphData::from_path(p)) // -> Option<Result<GraphData>>
                .transpose() // -> Result<Option<GraphData>>
                .unwrap_or_else(|e| {
                    log::error!("error importing last open file: {}", e);
                    None
                }) // -> Option<GraphData>
                .unwrap_or_else(|| GraphData::new()),
            conf,
            select_src: false,
        }
    }

    fn reset(&mut self) {
        self.graph1 = GraphData::new();
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
            history: Vec::new(),
        }
    }

    pub fn from_path(path: &Path) -> Result<Self> {
        Ok(Self {
            graph: Arc::new(Graph::import_from_file(path)?),
            path: Some(path.to_path_buf()),
            wids: HashMap::new(),
            history: Vec::new(),
        })
    }

    // similar to lens; clone graph, pass to closure, check if
    // different - if so, append to history and update arc
    pub fn with_undo<F: Fn(&mut Graph)>(&mut self, f: F, hint: &'static str) {
        let g0 = self.graph.clone();
        let g = Arc::make_mut(&mut self.graph);
        f(g);
        if !g0.same(&self.graph) {
            // TODO: limit this?
            self.history.push(History { graph: g0, hint });
        } else {
            log::warn!("no change for {}!", hint);
        }
    }

    //pub fn with_undo_idx()

    pub fn add_state(&mut self, id: &str, parent: Option<Idx>) -> Result<()> {
        let uid = unique_id(&self.graph, id, parent)?;
        let g = Arc::make_mut(&mut self.graph);
        g.add_state(transit::State::new(
            uid.unwrap_or_else(|| id.to_string()),
            parent,
            None,
            None,
        ));
        Ok(())
    }

    pub fn move_state(&mut self, parent: Option<Idx>, i: Idx, rect: Rect) -> Result<()> {
        let rect = if let Some(p) = parent {
            // we want the new rect relative to the parent, so when
            // fitting we only use the parent size
            let a = Rect::from_origin_size(Point::ZERO, self.graph.get(p).edit_data.rect.1);
            fit_rect(a, rect)
        } else {
            rect
        };

        // we only want a new id if it's a new parent and the current
        // id won't be unique
        let id = {
            let p0 = self.graph.get(i).parent;
            if p0 != parent {
                let id = &self.graph.get(i).id;
                unique_id(&self.graph, id, parent)?
            } else {
                None
            }
        };
        let s = Arc::make_mut(&mut self.graph).get_mut(i);
        s.parent = parent;
        if let Some(id) = id {
            s.id = id;
        }
        s.edit_data.rect = (rect.origin().into(), rect.size().into());
        Ok(())
    }

    // TODO: unset initial with delete key?
    pub fn set_initial(&mut self, a: Option<Idx>, b: Idx) -> Result<()> {
        match a {
            Some(a) => {
                if self.graph.is_child(a, b) {
                    let s = Arc::make_mut(&mut self.graph).get_mut(a);
                    s.set_initial_idx(b);
                    Ok(())
                } else {
                    Err(anyhow!("invalid initial state: {:?}", b))
                }
            }
            None => {
                // set graph initial
                Arc::make_mut(&mut self.graph).set_initial(b);
                Ok(())
            }
        }
    }

    pub fn step_initial(&mut self, a: Option<Idx>) {
        match a {
            Some(idx) => {
                self.with_undo(
                    |g| {
                        let s = g.get_mut(idx);
                        s.initial = s.initial.clone().step();
                    },
                    "step initial",
                );
            }
            None => self.with_undo(|g| g.initial = g.initial.clone().step(), "step initial"),
        }
    }

    pub fn rect(&self, i: Idx) -> Rect {
        let rect = self.graph[i].edit_data.rect;
        Rect::from_origin_size(rect.0, rect.1)
    }

    // checking containment isn't really needed since anything outside
    // the state in question will either be a setting a new initial
    // state or invalid, remove this?
    pub fn move_initial(&mut self, a: Option<Idx>, p: Point) {
        let contains = match a {
            Some(a) => self.rect(a).with_origin(Point::ZERO).contains(p),
            None => true,
        };
        if contains {
            let g = Arc::make_mut(&mut self.graph);
            match a {
                Some(a) => g.get_mut(a).edit_data.initial = p.into(),
                None => g.edit_data.initial = p.into(),
            }
        } else {
            log::warn!("can't drag initial outside rect bounds");
        }
    }

    pub fn add_transition(&mut self, from: Idx, to: Idx, p: Point) {
        self.with_undo(
            |g| {
                g.add_transition(from, to, transit::Transition::default().at(p.into()))
                    .unwrap()
            },
            "add transition",
        )
    }
}

// make a unique id
fn unique_id(graph: &Graph, id: &str, parent: Option<Idx>) -> Result<Option<String>> {
    if graph.is_unique_id(parent, &id) {
        Ok(None)
    } else {
        for n in 1..10 {
            let id = format!("{}-{}", id, n);
            if graph.is_unique_id(parent, &id) {
                return Ok(Some(id));
            }
        }
        Err(anyhow!("failed to make unique id"))
    }
}

// fit rect b into a, keep a small border when fitting so the child
// state is distinct from the parent - we should really have a
// distinction for overlapping states, too; a slight shadow?
fn fit_rect(a: Rect, b: Rect) -> Rect {
    // method 1 - just move it so the origin fits, do not resize
    // b.with_origin(Point::new(
    //     b.x0.min(a.width() - b.width()).max(10.),
    //     b.y0.min(a.height() - b.height()).max(10.),
    // ))

    // method 2 - fit b into a with a small border
    a.inset(-10.).intersect(b)
}

#[derive(Serialize, Deserialize, Debug, Clone, Default)]
struct EditConf {
    last_open_file: Option<PathBuf>,
    edit_action: Option<String>,
    rename_action: Option<String>,
    //keys: Vec<Key>,
    //autosave: Option<f64>,
}

const CONF_PATH: &str = "conf.ron";

fn read_conf() -> Result<EditConf> {
    Ok(from_reader(File::open(CONF_PATH)?)?)
}

fn save_conf(conf: &EditConf) -> Result<()> {
    let mut file = File::create(CONF_PATH)?;
    file.write_all(to_string_pretty(conf, PrettyConfig::default())?.as_bytes())?;
    Ok(())
}

fn main() {
    // do this earlier to log errors initializing data
    simple_logger::init().ok();

    let data = EditData::new(read_conf().unwrap_or_default());

    let main_window = WindowDesc::new(ui_builder)
        .menu(make_menu(&data))
        // TODO update with graph name and file path
        .title(LocalizedString::new("transit-window-title").with_placeholder("transit"));

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

            // for buttons
            env.set(theme::BUTTON_DARK, Color::rgb8(0xCC, 0xCC, 0xCC));
            env.set(theme::BUTTON_LIGHT, Color::rgb8(0xEE, 0xEE, 0xEE));
            env.set(theme::BORDER_DARK, Color::rgb8(0x33, 0x33, 0x33));
            env.set(theme::BORDER_LIGHT, Color::rgb8(0xCC, 0xCC, 0xCC));
        })
        //.use_simple_logger()
        .delegate(Delegate)
        .launch(data)
        .expect("launch failed");
}

pub fn graph_lens() -> impl Lens<EditData, Arc<Graph>> {
    lens!(EditData, graph1).then(lens!(GraphData, graph))
}

pub(crate) fn graph_id_lens() -> impl Lens<EditData, String> {
    graph_lens().then(lens!(Graph, id).in_arc())
}

pub(crate) fn state_lens(i: Idx) -> impl Lens<EditData, Arc<transit::State>> {
    graph_lens().then(lens::Id.index(i).in_arc())
}

pub(crate) fn state_id_lens(i: Idx) -> impl Lens<EditData, String> {
    graph_lens().then(StateIdLens::new(i).in_arc())
}

pub fn state_initial_lens(i: Idx) -> impl Lens<EditData, transit::Initial> {
    state_lens(i).then(lens!(transit::State, initial).in_arc())
}

pub fn graph_initial_lens() -> impl Lens<EditData, transit::Initial> {
    graph_lens().then(lens!(Graph, initial).in_arc())
}

pub fn graph_edit_data_lens() -> impl Lens<EditData, transit::GraphEditData> {
    graph_lens().then(lens!(Graph, edit_data).in_arc())
}

pub fn graph_src_lens() -> impl Lens<EditData, transit::PathData> {
    graph_edit_data_lens().then(lens!(GraphEditData, src))
}

// indexing with transitions doesn't return an Arc, testing this out
pub fn transition_lens(i: TransIdx) -> impl Lens<EditData, transit::Transition> {
    graph_lens().then(lens::Id.index(i).in_arc())
}

pub fn transition_event_lens(i: TransIdx) -> impl Lens<EditData, String> {
    transition_lens(i).then(lens!(transit::Transition, event))
}

fn ui_builder() -> impl Widget<EditData> {
    Flex::column()
        .cross_axis_alignment(CrossAxisAlignment::Start)
        .with_flex_child(Scroll::new(Root::new(None)), 1.0)
        .with_child(
            // show path to hovered state, key commands, etc.
            Flex::row()
                .must_fill_main_axis(true)
                .with_child(
                    Label::new(|id: &String, _env: &_| format!("{}", id)).lens(graph_id_lens()),
                )
                .with_spacer(8.)
                .with_child(
                    FilePath::new().lens(graph_src_lens()),
                    // Button::new(|data: &EditData, _env: &_| {
                    //     match dbg!(&data.graph1.graph.edit_data.src) {
                    //         Some(src) => format!("{}", src.to_string_lossy()),
                    //         None => "select src".to_string(),
                    //     }
                    // })
                    // .on_click(|ctx, _data, _env| {
                    //     dbg!("click");
                    //     ctx.submit_command(SELECT_SRC, None);
                    //     ctx.
                    // }),
                )
                .padding(2.)
                .background(Color::rgb8(0xEF, 0xEF, 0xEF)), //.debug_paint_layout(),
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
                dbg!("OPEN_FILE");

                // the command contains the path from the open panel
                match cmd.get_object::<FileInfo>() {
                    Ok(f) => {
                        if data.select_src {
                            // update source file for graph
                            data.graph1.with_undo(
                                |g| g.edit_data.src = PathData::new(Some(f.path().to_path_buf())),
                                "select src",
                            );
                            data.select_src = false;
                            dbg!(&data.graph1.graph.edit_data.src);
                        } else {
                            // open new graph
                            match GraphData::from_path(f.path()) {
                                Ok(g) => {
                                    data.graph1 = g;
                                    ctx.submit_command(RESET, None);
                                }
                                Err(e) => log::error!("failed to import graph: {:?}", e),
                            }
                        }
                    }
                    Err(_) => {
                        dbg!("submit SHOW_OPEN_PANEL");
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
            &SELECT_SRC => {
                dbg!("SELECT_SRC recv");
                data.select_src = true;
                ctx.submit_command(druid::commands::OPEN_FILE, *target);
                false
            }
            // TODO:
            &druid::commands::UNDO => false,
            &druid::commands::REDO => false,
            _ => true,
        }
    }
}

// TODO: we want to pass mouse position too so we can create states at
// mouse location rather than dumping them on top of each other; also,
// create transitions by selecting a state and hovering another with "t"
pub(crate) fn handle_key(
    ctx: &mut EventCtx,
    event: &KeyEvent,
    data: &mut EditData,
    idx: Option<Idx>,
) {
    match event {
        // Select all states
        k_e if (HotKey::new(SysMods::Cmd, "a")).matches(k_e) => {
            // TODO:
        }
        // Backspace focuses parent?
        k_e if (HotKey::new(None, KeyCode::Backspace)).matches(k_e) => {
            // TODO:
        }
        // Delete this state
        k_e if (HotKey::new(None, KeyCode::Delete)).matches(k_e) => {
            // TODO:
        }
        k_e if HotKey::new(None, KeyCode::Escape).matches(k_e) => ctx.resign_focus(),
        // Tab and shift+tab change focus to child states
        k_e if HotKey::new(None, KeyCode::Tab).matches(k_e) => ctx.focus_next(),
        k_e if HotKey::new(RawMods::Shift, KeyCode::Tab).matches(k_e) => ctx.focus_prev(),
        k_e if HotKey::new(None, "n").matches(k_e) => {
            if let Err(err) = data.graph1.add_state("untitled", idx) {
                log::error!("error on adding state: {}", err);
            }
            ctx.set_handled();
        }
        // step initial type
        k_e if HotKey::new(None, "i").matches(k_e) => {
            if idx.is_some() {
                data.graph1.step_initial(idx);
                ctx.set_handled();
            } else {
                log::warn!("can't step initial for root (yet?)");
            }
        }
        k_e if HotKey::new(None, "q").matches(k_e) => {
            // save and quit if we have a path
            if let Some(path) = &data.graph1.path {
                data.conf.last_open_file = Some(path.clone());
                if let Err(e) = save_conf(&data.conf) {
                    log::error!("error saving configuration file: {}", e);
                }
                // copied from SAVE_FILE
                if let Err(e) = data.graph1.graph.export_to_file(&path) {
                    log::error!("failed to export graph: {:?}", e);
                }
                ctx.submit_command(druid::commands::QUIT_APP, None);
            } else {
                // FIX: have to hit q twice here
                ctx.submit_command(druid::commands::SAVE_FILE, None);
            }
        }
        _ => {
            log::info!("unhandled key: {:?}", event);
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

#[cfg(test)]
mod tests {
    use super::*;
    use druid::*;
    use std::sync::Arc;
    use transit::*;

    #[derive(Clone, Data)]
    struct A {
        b: usize,
    }

    struct L;
    impl Lens<A, f64> for L {
        fn with<V, F: FnOnce(&f64) -> V>(&self, data: &A, f: F) -> V {
            f(&(data.b as f64))
        }

        fn with_mut<V, F: FnOnce(&mut f64) -> V>(&self, data: &mut A, f: F) -> V {
            f(&mut (data.b as f64))
        }
    }

    #[test]
    fn test_lens() {
        let a = A { b: 42 };
        assert_eq!(lens!(A, b).get(&a), 42);

        assert_eq!(L {}.get(&a), 42.);
    }

    #[test]
    fn test_arc() {
        let lens = lens::Id.index(2).in_arc();
        let mut x = Arc::new(vec![0, 1, 2, 3]);
        let original = x.clone();
        assert_eq!(lens.get(&x), 2);

        lens.put(&mut x, 2);
        assert!(
            Arc::ptr_eq(&original, &x),
            "no-op writes don't cause a deep copy"
        );
        lens.put(&mut x, 42);
        assert_eq!(&*x, &[0, 1, 42, 3]);
    }

    #[test]
    fn test_arc2() {
        use druid::lens::Field;

        let tuple0 = Field::new(|a: &(usize, usize)| &a.0, |a| &mut a.0);
        let lens = lens::Id.index(2).in_arc().then(tuple0);
        let mut x = Arc::new(vec![(0, 0), (1, 0), (2, 0), (3, 0)]);
        let original = x.clone();
        assert_eq!(lens.get(&x), 2);

        lens.put(&mut x, 2);
        assert!(
            Arc::ptr_eq(&original, &x),
            "no-op writes don't cause a deep copy"
        );
        lens.put(&mut x, 42);
        assert_eq!(&x[2], &(42, 0));
    }

    #[test]
    fn test_graph_index() {
        let mut g = Graph::new("untitled");
        let id = g.add_state("child1");
        let mut g = Arc::new(g);
        let original = g.clone();
        let lens = lens::Id.index(id).in_arc().then(lens!(State, id).in_arc());
        assert_eq!(lens.get(&g), "child1");

        lens.put(&mut g, "child1".to_string());
        assert!(Arc::ptr_eq(&original, &g), "no-op, graph");
        assert!(Arc::ptr_eq(&original[id], &g[id]), "no-op, state");
        lens.put(&mut g, "child2".to_string());
        assert_eq!(lens.get(&g), "child2");
    }
}
