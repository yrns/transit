use crate::{widgets::*, *};
use druid::{kurbo::*, lens, lens::*, piet::*, theme, widget::*, *};

const COPY_BACK: Selector = Selector::new("transit.edit.idbox.copy-back");

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
            Event::Command(cmd) if cmd.selector == COPY_BACK => {
                *data = cmd.get_object::<String>().unwrap().clone();
            }
            _ => (),
        }
        self.either.event(ctx, event, &mut self.data, env);
    }

    fn lifecycle(&mut self, ctx: &mut LifeCycleCtx, event: &LifeCycle, data: &String, env: &Env) {
        match event {
            LifeCycle::WidgetAdded => self.data = data.clone(),
            LifeCycle::FocusChanged(focus) => {
                if *focus != self.either.has_focus {
                    if *focus {
                        self.data = data.clone();
                    } else {
                        // we cannot update data here, so bounce the new data back through an event
                        ctx.submit_command(
                            Command::new(COPY_BACK, self.data.clone()),
                            ctx.widget_id(),
                        );
                    }
                }
            }
            _ => (),
        }
        self.either.lifecycle(ctx, event, &self.data, env)
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
        //if let LifeCycle::WidgetAdded = event {}
        if let LifeCycle::FocusChanged(focus) = event {
            if self.has_focus != *focus {
                self.has_focus = *focus;
                log::info!("focus: {} id: {:?}", focus, ctx.widget_id());
                ctx.request_layout();
            }
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

    fn layout(&mut self, ctx: &mut LayoutCtx, bc: &BoxConstraints, data: &T, env: &Env) -> Size {
        if self.has_focus {
            let size = self.focus.layout(ctx, bc, data, env);
            self.focus.set_layout_rect(ctx, data, env, size.to_rect());
            ctx.set_paint_insets(self.focus.paint_insets());
            size
        } else {
            let size = self.nof.layout(ctx, bc, data, env);
            self.nof.set_layout_rect(ctx, data, env, size.to_rect());
            ctx.set_paint_insets(self.focus.paint_insets());
            size
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
