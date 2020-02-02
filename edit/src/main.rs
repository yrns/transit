#![allow(unused_imports, dead_code)]

mod widgets;

use crate::widgets::{Card, State};
use druid::lens::{self, LensExt};
use druid::theme;
use druid::widget::{Align, Button, Flex, Label, List, Padding, Scroll, WidgetExt};
use druid::{AppLauncher, Color, Data, Lens, LocalizedString, Rect, Widget, WidgetPod, WindowDesc};
use std::sync::Arc;
use transit::Statechart;

#[derive(Clone, Data, Lens)]
struct EditData {
    //statechart: Statechart,
// path of file on disk
//path: Path
}

fn main() {
    let main_window = WindowDesc::new(ui_builder);

    // need to make this contain statechart
    // let data = AppData {
    //     left: Arc::new(vec![1, 2]),
    //     right: Arc::new(vec![1, 2, 3]),
    // };

    let data = 0_u32;

    AppLauncher::with_window(main_window)
        .configure_env(|env, _| {
            env.set(theme::SELECTION_COLOR, Color::rgb8(0xA6, 0xCC, 0xFF));
            env.set(theme::WINDOW_BACKGROUND_COLOR, Color::WHITE);
            env.set(theme::LABEL_COLOR, Color::BLACK);
            env.set(theme::CURSOR_COLOR, Color::BLACK);
            env.set(theme::BACKGROUND_LIGHT, Color::rgb8(230, 230, 230));
        })
        .use_simple_logger()
        .launch(data)
        .expect("launch failed");
}

fn ui_builder() -> impl Widget<u32> {
    //let text = LocalizedString::new("state-name").with_placeholder("state1".to_string());
    let state1 = State::new("state1").padding(10.0);

    let state2 = State::new("state2").padding(10.0);

    let mut card = Card::new();
    card.children.push((
        Rect::from_origin_size((20.0, 20.0), (100.0, 100.0)),
        WidgetPod::new(Box::new(state1)),
    ));

    card.children.push((
        Rect::from_origin_size((120.0, 80.0), (80.0, 40.0)),
        WidgetPod::new(Box::new(state2)),
    ));

    card
}
