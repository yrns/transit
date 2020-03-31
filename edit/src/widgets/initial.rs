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
    sidx: Option<Idx>,
}

impl Initial {
    pub fn new(sidx: Option<Idx>) -> Self {
        Self { sidx }
    }
}

impl Widget<EditData> for Initial {
    // use drag to "drag" this widget to a child state to set initial
    fn event(&mut self, _ctx: &mut EventCtx, _event: &Event, _data: &mut EditData, _env: &Env) {}

    fn lifecycle(
        &mut self,
        ctx: &mut LifeCycleCtx,
        event: &LifeCycle,
        _data: &EditData,
        _env: &Env,
    ) {
        match event {
            LifeCycle::HotChanged(_) => {
                ctx.request_paint();
            }
            _ => {}
        }
    }

    fn update(&mut self, _ctx: &mut UpdateCtx, _old_data: &EditData, _data: &EditData, _env: &Env) {
    }

    fn layout(
        &mut self,
        _ctx: &mut LayoutCtx,
        _bc: &BoxConstraints,
        _data: &EditData,
        _env: &Env,
    ) -> Size {
        Size::new(10., 10.)
    }

    fn paint(&mut self, ctx: &mut PaintCtx, _data: &EditData, env: &Env) {
        let active = ctx.is_active();

        let width = ctx.size().width;
        let r = width * 0.5;

        let color = env.get(if active {
            theme::SELECTION_COLOR
        } else {
            theme::LABEL_COLOR
        });

        let circle = Circle::new(Point::new(r, r), r);

        if active || ctx.is_hot() {
            ctx.fill(circle, &color);
        } else {
            ctx.stroke(circle, &color, 1.);
        }
    }
}
