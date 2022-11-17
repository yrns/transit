use crate::*;
use druid::{kurbo::*, piet::*, theme, widget::*, *};
use std::time::Duration;
use transit::PathData;

pub struct FilePath {
    timer_id: TimerToken,
    label: Label<PathData>,
}

impl FilePath {
    pub fn new() -> Self {
        Self {
            timer_id: TimerToken::INVALID,
            label: Label::dynamic(|data: &PathData, _env| {
                data.0
                    .as_ref()
                    .map(|p| format!("src: {}", p.to_string_lossy()))
                    .unwrap_or("select src".to_string())
            }),
        }
    }
}

impl Widget<PathData> for FilePath {
    fn event(&mut self, ctx: &mut EventCtx, event: &Event, data: &mut PathData, env: &Env) {
        match event {
            Event::Timer(id) => {
                if *id == self.timer_id {
                    ctx.submit_command(SELECT_SRC);
                }
            }
            Event::MouseDown(e) => {
                if e.button == MouseButton::Left {
                    self.timer_id = ctx.request_timer(Duration::from_millis(1));
                }
            }
            _ => (),
        }

        self.label.event(ctx, event, data, env)
    }

    fn lifecycle(&mut self, ctx: &mut LifeCycleCtx, event: &LifeCycle, data: &PathData, env: &Env) {
        self.label.lifecycle(ctx, event, data, env)
    }

    fn update(&mut self, ctx: &mut UpdateCtx, old_data: &PathData, data: &PathData, env: &Env) {
        self.label.update(ctx, old_data, data, env)
    }

    fn layout(
        &mut self,
        ctx: &mut LayoutCtx,
        bc: &BoxConstraints,
        data: &PathData,
        env: &Env,
    ) -> Size {
        self.label.layout(ctx, bc, data, env)
    }

    fn paint(&mut self, ctx: &mut PaintCtx, data: &PathData, env: &Env) {
        let stroke_width = 1.0;
        let rect = ctx
            .size()
            .to_rect()
            .inset(-stroke_width / 2.)
            .to_rounded_rect(4.);
        ctx.stroke(rect, &env.get(theme::BORDER_LIGHT), stroke_width);
        self.label.paint(ctx, data, env)
    }
}
