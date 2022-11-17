use crate::{widgets::*, *};
use druid::{kurbo::*, lens, lens::*, piet::*, theme, widget::*, *};

const COPY_BACK: Selector<String> = Selector::new("transit.edit.idbox.copy-back");

// This is a label that turns into a textbox for editing.
pub struct IdBox {
    either: EitherFocus<String>,
    data: String,
}

impl IdBox {
    pub fn new() -> Self {
        Self {
            either: EitherFocus::new(TextBox::new(), Label::dynamic(|s: &String, _| s.clone())),
            data: String::new(),
        }
    }
}

impl Widget<String> for IdBox {
    fn event(&mut self, ctx: &mut EventCtx, event: &Event, data: &mut String, env: &Env) {
        match event {
            Event::Command(cmd) if cmd.is(COPY_BACK) => {
                *data = cmd.get_unchecked(COPY_BACK).clone();
            }
            _ => (),
        }
        self.either.event(ctx, event, &mut self.data, env);
    }

    fn lifecycle(&mut self, ctx: &mut LifeCycleCtx, event: &LifeCycle, data: &String, env: &Env) {
        match event {
            LifeCycle::WidgetAdded => self.data = data.clone(),
            _ => (),
        }

        let had_focus = self.either.has_focus;

        self.either.lifecycle(ctx, event, &self.data, env);

        if had_focus != self.either.has_focus {
            if had_focus {
                // we cannot update data here, so bounce the new data back through an event
                ctx.submit_command(Command::new(COPY_BACK, self.data.clone(), ctx.widget_id()));
            } else {
                self.data = data.clone();
            }
        }
    }

    fn update(&mut self, ctx: &mut UpdateCtx, old_data: &String, data: &String, env: &Env) {
        // only update data when we don't have focus, otherwise an
        // update will clobber the edit? this means we may lose an
        // external update
        if !self.either.has_focus && data != &self.data {
            self.data = data.clone();
        }
        self.either.update(ctx, old_data, &self.data, env)
    }

    fn layout(
        &mut self,
        ctx: &mut LayoutCtx,
        bc: &BoxConstraints,
        _data: &String,
        env: &Env,
    ) -> Size {
        self.either.layout(ctx, bc, &self.data, env)
    }

    fn paint(&mut self, paint: &mut PaintCtx, _data: &String, env: &Env) {
        self.either.paint(paint, &self.data, env)
    }

    // why?
    // fn id(&self) -> Option<WidgetId> {
    //     self.either.id()
    // }
}

/// This is like Either but switches on focus, not a closure.
pub struct EitherFocus<T> {
    focus: WidgetPod<T, Box<dyn Widget<T>>>,
    nof: WidgetPod<T, Box<dyn Widget<T>>>,
    pub has_focus: bool,
}

impl<T> EitherFocus<T> {
    pub fn new(focus: impl Widget<T> + 'static, nof: impl Widget<T> + 'static) -> EitherFocus<T> {
        EitherFocus {
            focus: WidgetPod::new(focus).boxed(),
            nof: WidgetPod::new(nof).boxed(),
            has_focus: false,
        }
    }
}

impl<T: Data> Widget<T> for EitherFocus<T> {
    fn event(&mut self, ctx: &mut EventCtx, event: &Event, data: &mut T, env: &Env) {
        if self.has_focus {
            self.focus.event(ctx, event, data, env)
        } else {
            self.nof.event(ctx, event, data, env)
        }
    }

    fn lifecycle(&mut self, ctx: &mut LifeCycleCtx, event: &LifeCycle, data: &T, env: &Env) {
        match event {
            // We can no longer rely on FocusChanged since only the
            // leaf widget gets it.
            LifeCycle::Internal(InternalLifeCycle::RouteFocusChanged { old, new }) => {
                if old != new {
                    self.has_focus = *new == Some(self.focus.id());
                    ctx.request_layout();
                }
            }
            _ => {}
        }

        self.focus.lifecycle(ctx, event, data, env);
        self.nof.lifecycle(ctx, event, data, env);
    }

    fn update(&mut self, ctx: &mut UpdateCtx, _old_data: &T, data: &T, env: &Env) {
        if self.has_focus {
            self.focus.update(ctx, data, env);
        } else {
            self.nof.update(ctx, data, env);
        }
    }

    // Layout both widgets. This is only to silence a warning about
    // receiving events whilst not laid-out. Maybe fix this later.
    fn layout(&mut self, ctx: &mut LayoutCtx, bc: &BoxConstraints, data: &T, env: &Env) -> Size {
        let size1 = self.focus.layout(ctx, bc, data, env);
        self.focus.set_layout_rect(ctx, data, env, size1.to_rect());
        ctx.set_paint_insets(self.focus.paint_insets());

        let size2 = self.nof.layout(ctx, bc, data, env);
        self.nof.set_layout_rect(ctx, data, env, size2.to_rect());
        ctx.set_paint_insets(self.focus.paint_insets());

        if self.has_focus {
            size1
        } else {
            size2
        }
    }

    fn paint(&mut self, ctx: &mut PaintCtx, data: &T, env: &Env) {
        if ctx.has_focus() {
            self.focus.paint(ctx, data, env);
        } else {
            self.nof.paint(ctx, data, env);
        }
    }
}
