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
    AppLauncher, Color, Data, Lens, LocalizedString, Point, Rect, UnitPoint, Widget, WidgetId,
    WidgetPod, WindowDesc,
};
use log;
use std::collections::HashMap;
use std::sync::Arc;
use transit::{Graph, Idx, State, Transition};

#[derive(Clone, Data)]
struct EditData {
    graph: GraphData,
    // command to run when editing an action (emacsclient, etc.)
    #[druid(ignore)]
    edit_action: Option<String>,
    // command to run when renaming an action
    #[druid(ignore)]
    rename_action: Option<String>,
}

#[derive(Clone, Data)]
struct GraphData {
    graph: Arc<Graph>,
    // path to file on disk
    //path: Option<Path>
    // the state widgets store a state index, but we can't access the
    // widgets directly - this gives us widget id -> state index
    #[druid(ignore)]
    wids: HashMap<WidgetId, Idx>,
}

// we need to do layout in the data here so that layout in the widget can just read the data
// we can't update the data in update
impl GraphData {
    pub fn new() -> Self {
        Self {
            graph: Arc::new(Graph::new("untitled")),
            wids: HashMap::new(),
        }
    }

    // make an id unique
    fn unique_id(&self, id: String, parent: Option<Idx>) -> Result<String> {
        if self.graph.is_unique_id(parent, &id) {
            Ok(id)
        } else {
            for n in 1..10 {
                let id = format!("{}-{}", id, n);
                if self.graph.is_unique_id(parent, id.as_str()) {
                    return Ok(id);
                }
            }
            Err(anyhow!("failed to make unique id"))
        }
    }

    pub fn add_state(&mut self, id: &str, parent: Option<Idx>) -> Result<()> {
        let id = self.unique_id(id.into(), parent)?;
        let g = Arc::make_mut(&mut self.graph);
        g.add_state(State::new(id, parent, None, None));
        Ok(())
    }

    // there is way too much cloning happening here for an id that's
    // probably not changing FIX:
    pub fn set_parent(&mut self, i: Idx, p: Option<Idx>) -> Result<()> {
        let id = self.unique_id(self.graph.get(i).id.clone(), p)?;
        let g = Arc::make_mut(&mut self.graph);
        g.set_id(Some(i), id.as_str());
        g.set_parent(i, p);
        Ok(())
    }
}

fn main() {
    let main_window = WindowDesc::new(ui_builder);

    // start with a blank graph
    let data = EditData {
        graph: GraphData::new(),
        edit_action: None,
        rename_action: None,
    };

    AppLauncher::with_window(main_window)
        // black on white
        .configure_env(|env, _| {
            env.set(theme::FONT_NAME, "serif");
            env.set(theme::SELECTION_COLOR, Color::rgb8(0xA6, 0xCC, 0xFF));
            env.set(theme::WINDOW_BACKGROUND_COLOR, Color::WHITE);
            env.set(theme::LABEL_COLOR, Color::BLACK);
            env.set(theme::CURSOR_COLOR, Color::rgb8(0xFF, 0x33, 0x33));
            env.set(theme::BACKGROUND_LIGHT, Color::rgb8(230, 230, 230));
        })
        //.debug_paint_layout()
        .use_simple_logger()
        .launch(data)
        .expect("launch failed");
}

fn ui_builder() -> impl Widget<EditData> {
    //let text = LocalizedString::new("state-name").with_placeholder("state1".to_string());
    //let state1 = State::new("state1").padding(10.0);
    //let state2 = State::new("state2").padding(10.0);
    //let state3 = State::new("state3").padding(10.0);

    // let mut card = Card::new();
    // card.children.push((
    //     Rect::from_origin_size((20.0, 20.0), (100.0, 100.0)),
    //     WidgetPod::new(Box::new(state1)),
    // ));

    //let solid = Color::rgb8(0xfe, 0xee, 0xee);

    Flex::column()
        .with_child(Scroll::new(Root::new(None)), 1.0)
        .with_child(
            // show path to hovered state, key commands, etc.
            Label::new(|data: &EditData, _env: &_| format!("{}", data.graph.graph.id))
                .fix_height(40.),
            0.0,
        )
}
