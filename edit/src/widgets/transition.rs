use crate::{widgets::*, *};
use druid::{kurbo::*, piet::*, theme, widget::*, *};
use std::sync::Arc;
use transit::TransIdx;

pub struct Transition {
    //idx: TransIdx,
    child: Drag<transit::Transition, Flex<transit::Transition>>,
}

impl Transition {
    pub fn new(i: TransIdx) -> Self {
        Self {
            child: Drag::new(
                Flex::row().with_child(
                    TextBox::new()
                        .lens(lens!(transit::Transition, event))
                        .padding(4.),
                ),
                DragType::MoveTransition(i),
                None,
            ),
        }
    }
}

impl Widget<transit::Transition> for Transition {
    fn event(
        &mut self,
        ctx: &mut EventCtx,
        event: &Event,
        data: &mut transit::Transition,
        env: &Env,
    ) {
        self.child.event(ctx, event, data, env);
    }

    fn lifecycle(
        &mut self,
        ctx: &mut LifeCycleCtx,
        event: &LifeCycle,
        data: &transit::Transition,
        env: &Env,
    ) {
        match event {
            LifeCycle::HotChanged(_) => {
                ctx.request_paint();
            }
            _ => {}
        }

        self.child.lifecycle(ctx, event, data, env);
    }

    fn update(
        &mut self,
        ctx: &mut UpdateCtx,
        old_data: &transit::Transition,
        data: &transit::Transition,
        env: &Env,
    ) {
        self.child.update(ctx, old_data, data, env);
    }

    fn layout(
        &mut self,
        ctx: &mut LayoutCtx,
        bc: &BoxConstraints,
        data: &transit::Transition,
        env: &Env,
    ) -> Size {
        self.child.layout(ctx, bc, data, env)
    }

    fn paint(&mut self, ctx: &mut PaintCtx, data: &transit::Transition, env: &Env) {
        self.child.paint(ctx, data, env);
    }
}
