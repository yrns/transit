use crate::widgets::{DragData, DRAG_END, DRAG_START};
use crate::{handle_key, EditData, RESET};
use druid::kurbo::RoundedRect;
use druid::kurbo::{BezPath, Circle, Shape};
use druid::piet::{FontBuilder, ImageFormat, InterpolationMode, Text, TextLayoutBuilder};
use druid::theme;
use druid::widget::*;
use druid::*;
use std::sync::Arc;
use transit::{Graph, Idx, State, Transition};

pub struct Initial {
    // T is unused
    label: Option<Align<transit::Initial>>,
}

impl Initial {
    pub fn new() -> Self {
        Self { label: None }
    }
}

impl Widget<transit::Initial> for Initial {
    // use drag to "drag" this widget to a child state to set initial
    fn event(&mut self, ctx: &mut EventCtx, event: &Event, data: &mut transit::Initial, env: &Env) {
        if let Some(label) = &mut self.label {
            label.event(ctx, event, data, env);
        }
    }

    fn lifecycle(
        &mut self,
        ctx: &mut LifeCycleCtx,
        event: &LifeCycle,
        data: &transit::Initial,
        env: &Env,
    ) {
        match event {
            LifeCycle::HotChanged(_) => {
                ctx.request_paint();
            }
            _ => {}
        }

        if let Some(label) = &mut self.label {
            label.lifecycle(ctx, event, data, env);
        }
    }

    fn update(
        &mut self,
        ctx: &mut UpdateCtx,
        _old_data: &transit::Initial,
        data: &transit::Initial,
        env: &Env,
    ) {
        let a = self.label.is_some();
        match data {
            transit::Initial::None | transit::Initial::Initial(_) => {
                self.label = None;
            }
            _ => {
                if !a {
                    self.label = Some(Align::new(
                        UnitPoint::CENTER,
                        // TODO: how do I make this bold?
                        Label::new("H")
                            .with_text_color(env.get(theme::BACKGROUND_LIGHT))
                            .with_text_size(14.),
                    ));
                }
            }
        }
        if a != self.label.is_some() {
            ctx.children_changed();
            ctx.request_layout();
        }
        ctx.request_paint();

        // no need to update label since it's a static string
    }

    fn layout(
        &mut self,
        ctx: &mut LayoutCtx,
        _bc: &BoxConstraints,
        data: &transit::Initial,
        env: &Env,
    ) -> Size {
        let size = if self.label.is_some() {
            Size::new(18., 18.)
        } else {
            Size::new(10., 10.)
        };
        if let Some(label) = &mut self.label {
            let _size = label.layout(ctx, &BoxConstraints::tight(size), data, env);
        }
        size
    }

    fn paint(&mut self, ctx: &mut PaintCtx, data: &transit::Initial, env: &Env) {
        let active = ctx.is_active();

        let width = ctx.size().width;
        let r = width * 0.5;

        let color = env.get(if active || ctx.is_hot() {
            theme::SELECTION_COLOR
        } else {
            theme::LABEL_COLOR
        });

        let circle = Circle::new(Point::new(r, r), r);

        match data {
            transit::Initial::None => ctx.stroke(circle, &color, 1.),
            transit::Initial::Initial(_) => ctx.fill(circle, &color),
            transit::Initial::HistoryShallow(_) => {
                ctx.fill(circle, &color);
            }
            transit::Initial::HistoryDeep(_) => {
                let color = Color::rgb8(0x90, 0x00, 0x90);
                ctx.fill(circle, &color);
            }
        }

        if let Some(label) = &mut self.label {
            label.paint(ctx, data, env);
        }
    }
}
