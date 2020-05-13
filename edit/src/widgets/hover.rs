use crate::{widgets::*, *};
use druid::{kurbo::*, lens, lens::*, piet::*, theme, widget::*, *};
use std::fmt;
use std::sync::Arc;
use transit::Idx;

pub const UPDATE_HOVER: Selector = Selector::new("transit.edit.update-hover");

pub struct Hover {
    label: WidgetPod<String, Box<dyn Widget<String>>>,
}

impl Hover {
    pub fn new() -> Self {
        Self {
            label: WidgetPod::new(Box::new(
                Label::dynamic(|s: &String, _| s.clone()).padding(6.),
            )),
        }
    }
}

impl Widget<String> for Hover {
    fn event(&mut self, ctx: &mut EventCtx, event: &Event, data: &mut String, env: &Env) {
        match event {
            Event::Command(cmd) if cmd.selector == UPDATE_HOVER => {
                *data = cmd.get_object::<String>().unwrap().clone();
                // we need layout too?
                ctx.request_paint();
                return;
            }
            // Event::MouseDown(_) => {}
            // Event::KeyDown(_) => {}
            // Event::MouseMove(_) => {}
            _ => (),
        }
        self.label.event(ctx, event, data, env);
    }

    fn lifecycle(&mut self, ctx: &mut LifeCycleCtx, event: &LifeCycle, data: &String, env: &Env) {
        match event {
            LifeCycle::HotChanged(_) => ctx.request_paint(),
            _ => (),
        }
        self.label.lifecycle(ctx, event, data, env);
    }

    fn update(&mut self, ctx: &mut UpdateCtx, _old_data: &String, data: &String, env: &Env) {
        self.label.update(ctx, data, env);
    }

    fn layout(
        &mut self,
        ctx: &mut LayoutCtx,
        bc: &BoxConstraints,
        data: &String,
        env: &Env,
    ) -> Size {
        let size = self.label.layout(ctx, bc, data, env);
        self.label.set_layout_rect(ctx, data, env, size.to_rect());
        size
    }

    fn paint(&mut self, ctx: &mut PaintCtx, data: &String, env: &Env) {
        //let is_hot = ctx.is_hot();

        let stroke_width = 1.0;
        let rect = ctx
            .size()
            .to_rect()
            .inset(-stroke_width / 2.)
            .to_rounded_rect(4.);

        let border_color = env.get(theme::BORDER_LIGHT);
        ctx.stroke(rect, &border_color, stroke_width);
        ctx.fill(rect, &env.get(theme::BACKGROUND_LIGHT));

        self.label.paint(ctx, data, env);
    }
}
