//! card layout

use druid::{
    kurbo::{Point, Rect, Size},
    widget::ListIter,
    BoxConstraints, Cursor, Data, Env, Event, EventCtx, LayoutCtx, LifeCycle, LifeCycleCtx,
    PaintCtx, UpdateCtx, Widget, WidgetId, WidgetPod,
};
use std::sync::Arc;

pub struct Card<T: Data> {
    pub children: Vec<(Rect, WidgetPod<T, Box<dyn Widget<T>>>)>,
    pub active_child: Option<(Rect, Point, WidgetId, bool)>,
}

impl<T: Data> Card<T> {
    //pub fn new<W: Widget<T> + 'static>() -> Self {
    pub fn new() -> Self {
        Self {
            children: Vec::new(),
            active_child: None,
        }
    }
}

impl<T: Data> Widget<T> for Card<T> {
    fn event(&mut self, ctx: &mut EventCtx, event: &Event, data: &mut T, env: &Env) {
        match event {
            Event::MouseDown(mouse) => {
                for (rect, child) in &mut self.children {
                    if rect.contains(mouse.pos) {
                        let inset = rect.inset(-20.);
                        //let resize = (rect.x1 - mouse.pos.x) < 10. && (rect.y1 - mouse.pos.y) < 10.;
                        let resize = !inset.contains(mouse.pos);

                        // save original rect, mouse position
                        self.active_child = Some((*rect, mouse.pos, child.id(), resize));

                        // no diagonal?
                        //ctx.set_cursor(&Cursor::ResizeLeftRight);
                    }
                }

                ctx.set_active(true);
                ctx.invalidate();
            }
            Event::MouseUp(_) => {
                // if ctx.is_active() {
                //     ctx.set_active(false);
                //     ctx.invalidate();
                //     if ctx.is_hot() {
                //         (self.action)(ctx, data, env);
                //     }
                // }
                self.active_child = None;
                //ctx.set_cursor(&Cursor::Arrow);
            }
            Event::MouseMoved(mouse) => {
                if let Some((rect0, mouse_pos0, id, resize)) = self.active_child {
                    for (ref mut rect, child) in &mut self.children {
                        if child.id() == id {
                            if resize {
                                let size = rect0.size().to_vec2() + (mouse.pos - mouse_pos0);
                                *rect = rect.with_size(Size::new(size.x, size.y));
                            } else {
                                let origin = rect0.origin() + (mouse.pos - mouse_pos0);
                                *rect = rect.with_origin(origin);
                            }
                            ctx.invalidate();
                            break;
                        }
                    }
                }
            }
            _ => (),
        }

        for (_rect, child) in &mut self.children {
            child.event(ctx, event, data, env);
        }
    }

    fn lifecycle(&mut self, ctx: &mut LifeCycleCtx, event: &LifeCycle, data: &T, env: &Env) {
        match event {
            LifeCycle::HotChanged(_) => {
                ctx.invalidate();
            }
            _ => (),
        }

        for (_rect, child) in &mut self.children {
            child.lifecycle(ctx, event, data, env)
        }
    }

    fn update(&mut self, ctx: &mut UpdateCtx, _old_data: &T, data: &T, env: &Env) {
        for (_rect, child) in &mut self.children {
            child.update(ctx, data, env);
        }
    }

    fn layout(
        &mut self,
        layout_ctx: &mut LayoutCtx,
        bc: &BoxConstraints,
        data: &T,
        env: &Env,
    ) -> Size {
        let mut min_rect = Rect::ZERO;
        for (mut rect, child) in &mut self.children {
            // each child has the same box constraints since in theory
            // they can all overlap? or we constrain to the rect?
            //let child_bc = bc;
            let child_bc = BoxConstraints::tight(rect.size());
            let child_size = child.layout(layout_ctx, &child_bc, data, env);
            rect = Rect::from_origin_size(rect.origin(), child_size);
            child.set_layout_rect(rect);
            min_rect = min_rect.union(rect);
        }

        bc.constrain(min_rect.size())
    }

    fn paint(&mut self, paint_ctx: &mut PaintCtx, data: &T, env: &Env) {
        for (_rect, child) in &mut self.children {
            child.paint_with_offset(paint_ctx, data, env);
        }
    }
}
