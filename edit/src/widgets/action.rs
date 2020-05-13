use crate::*;
use druid::{kurbo::*, piet::*, theme, widget::*, *};
use transit::{Graph, Idx, TransIdx};

// ENTRY https://icons.getbootstrap.com/icons/box-arrow-in-right/
// or https://icons.getbootstrap.com/icons/box-arrow-in-down-right/
// EXIT https://icons.getbootstrap.com/icons/box-arrow-down-right/
// GUARD https://icons.getbootstrap.com/icons/lock-fill/
// ACTION https://icons.getbootstrap.com/icons/play-fill/

const ACTION: &str = "M11.596 8.697l-6.363 3.692c-.54.313-1.233-.066-1.233-.697V4.308c0-.63.692-1.01 1.233-.696l6.363 3.692a.802.802 0 0 1 0 1.393z";

pub const SET_ACTION: Selector = Selector::new("transit.edit.set-action");

#[derive(Copy, Clone)]
pub enum ActionType {
    Entry(Idx),
    Exit(Idx),
    Guard(TransIdx),
    Action(TransIdx),
}

pub struct Action {
    ty: ActionType,
}

impl Action {
    pub fn new(ty: ActionType) -> Self {
        Self { ty }
    }
}

impl Widget<Option<String>> for Action {
    fn event(&mut self, ctx: &mut EventCtx, event: &Event, data: &mut Option<String>, _env: &Env) {
        match event {
            Event::MouseDown(_mouse) => {
                ctx.set_active(true);
                ctx.request_paint();
            }
            Event::MouseUp(mouse) => {
                if mouse.button == MouseButton::Left {
                    // toggle action on (if off) and jump to action in
                    // source file
                    ctx.submit_command(Command::new(SET_ACTION, (self.ty, true)), None);
                } else if mouse.button == MouseButton::Right {
                    // toggle action
                    if data.is_some() {
                        // TODO: this can't be undone? need to build with_undo into the lens
                        *data = None;
                    } else {
                        ctx.submit_command(Command::new(SET_ACTION, (self.ty, false)), None);
                    }
                }
                ctx.set_active(false);
                ctx.request_paint();
            }
            _ => {}
        }
    }

    fn lifecycle(
        &mut self,
        ctx: &mut LifeCycleCtx,
        event: &LifeCycle,
        _data: &Option<String>,
        _env: &Env,
    ) {
        match event {
            LifeCycle::HotChanged(_) => {
                ctx.request_paint();
            }
            _ => {}
        }
    }

    fn update(
        &mut self,
        ctx: &mut UpdateCtx,
        _old_data: &Option<String>,
        _data: &Option<String>,
        _env: &Env,
    ) {
        ctx.request_paint()
    }

    fn layout(
        &mut self,
        _ctx: &mut LayoutCtx,
        _bc: &BoxConstraints,
        _data: &Option<String>,
        _env: &Env,
    ) -> Size {
        Size::new(16., 16.)
    }

    fn paint(&mut self, ctx: &mut PaintCtx, data: &Option<String>, env: &Env) {
        let color = env.get(if ctx.is_hot() {
            theme::SELECTION_COLOR
        } else {
            theme::LABEL_COLOR
        });

        // use once-cell?
        let r = BezPath::from_svg(ACTION).unwrap();
        //let r = ctx.size().to_rect().inset(-1.);
        if data.is_some() {
            ctx.fill(r, &color);
        } else {
            ctx.stroke(r, &color, 1.);
        }
    }
}
