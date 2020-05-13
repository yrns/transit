use crate::{widgets::*, *};
use druid::{kurbo::*, piet::*, theme, widget::*, *};
use std::sync::Arc;
use transit::TransIdx;

pub struct Transition {
    idx: TransIdx,
    child: Drag<transit::Transition, Padding<transit::Transition>>,
}

impl Transition {
    pub fn new(i: TransIdx) -> Self {
        Self {
            idx: i,
            child: Drag::new(
                Flex::row()
                    //.main_axis_alignment(MainAxisAlignment::SpaceAround)
                    .with_spacer(6.)
                    .with_child(IdBox::new().lens(lens!(transit::Transition, event)))
                    .with_spacer(6.)
                    .with_child(
                        Action::new(ActionType::Guard(i)).lens(lens!(transit::Transition, guard)),
                    )
                    .with_spacer(6.)
                    .with_child(
                        Action::new(ActionType::Action(i)).lens(lens!(transit::Transition, action)),
                    )
                    .with_spacer(6.)
                    .padding(4.),
                // draw internal icon later, maybe just change connector color for now
                // .with_child(
                //     Checkbox::new("internal")
                //         .lens(lens!(transit::Transition, internal))
                //         .padding(4.),
                // ),
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

        // the drag is internal, unlike state, so this doesn't work FIX:?
        // if ctx.is_handled() {
        //     return;
        // }

        match event {
            Event::MouseDown(_) => {
                dbg!(self.child.has_active());
                if !self.child.has_active() {
                    if !ctx.is_focused() {
                        ctx.request_focus();
                    }
                }
            }
            Event::KeyDown(e) => {
                if ctx.is_focused() {
                    match e {
                        // focus endpoint a
                        e if HotKey::new(None, KeyCode::Backspace).matches(e) => todo!(),
                        // remove transition
                        e if HotKey::new(None, KeyCode::Delete).matches(e) => {
                            ctx.submit_command(Command::new(REMOVE_TRANSITION, self.idx), None)
                        }
                        // toggle internal (if self)
                        e if HotKey::new(None, "i").matches(e) => todo!(),
                        e if HotKey::new(None, KeyCode::Escape).matches(e) => ctx.resign_focus(),
                        e if HotKey::new(None, KeyCode::Tab).matches(e) => ctx.focus_next(),
                        e if HotKey::new(RawMods::Shift, KeyCode::Tab).matches(e) => {
                            ctx.focus_prev()
                        }

                        _ => log::info!("unhandled key: {:?}", e),
                    }
                    ctx.set_handled();
                }
            }
            Event::MouseMove(_mouse) => {
                //log::debug!("mouse in {:?}", ctx.widget_id());
                ctx.submit_command(
                    Command::new(UPDATE_HOVER, format!("transition: {:?}", ctx.widget_id())),
                    None,
                );
                ctx.set_handled();
            }
            _ => (),
        }
    }

    fn lifecycle(
        &mut self,
        ctx: &mut LifeCycleCtx,
        event: &LifeCycle,
        data: &transit::Transition,
        env: &Env,
    ) {
        match event {
            LifeCycle::WidgetAdded => ctx.register_for_focus(),
            LifeCycle::HotChanged(_) => ctx.request_paint(),
            LifeCycle::FocusChanged(_) => ctx.request_paint(),
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
        let stroke_width = 2.0;
        let rect = ctx
            .size()
            .to_rect()
            .inset(-stroke_width / 2.)
            .to_rounded_rect(2.);

        let border_color = if ctx.is_focused() {
            env.get(theme::PRIMARY_LIGHT)
        } else {
            env.get(theme::BORDER_LIGHT)
        };

        ctx.stroke(rect, &border_color, stroke_width);
        ctx.fill(rect, &env.get(theme::BACKGROUND_LIGHT));

        self.child.paint(ctx, data, env);
    }
}
