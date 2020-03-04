use crate::EditData;
use druid::{
    kurbo::{BezPath, Point, Rect, RoundedRect, Size, Vec2},
    theme,
    widget::Align,
    Affine, BoxConstraints, BoxedWidget, Command, Cursor, Data, Env, Event, EventCtx, LayoutCtx,
    LifeCycle, LifeCycleCtx, MouseButton, MouseEvent, PaintCtx, RenderContext, Selector, UnitPoint,
    UpdateCtx, Widget, WidgetId, WidgetPod,
};

pub const DRAG_START: Selector = Selector::new("transit.edit.drag-start");
pub const DRAG_END: Selector = Selector::new("transit.edit.drag-end");
// pub const RESIZE_START: Selector = Selector::new("transit.edit.resize-start");
// pub const RESIZE_END: Selector = Selector::new("transit.edit.resize-end");

#[derive(Debug, Clone)]
pub struct DragData {
    // original mouse down
    pub p0: Point,
    // offset to get into window coords
    //pub offset: Vec2,
    // drag mirror, relative to widget origin
    pub rect1: Rect,
    pub id: WidgetId,
    pub resize: bool,
}

impl DragData {
    pub fn new(mouse: &MouseEvent, size: Size, id: WidgetId, resize: bool) -> Self {
        DragData {
            p0: mouse.pos,
            //offset: Vec2::ZERO,
            rect1: Rect::from_origin_size(Point::ZERO, size),
            id,
            resize,
        }
    }
}

pub struct DragZone<W> {
    pub inner: W,
    pub drag: Option<DragData>,
}

impl<W> DragZone<W> {
    pub fn new(inner: W) -> Self {
        Self { inner, drag: None }
    }
}

// grab generates a command on mouse down (and no child handles the click)
// (or updates data?)
// only the deepest grab widget generates the command
// top level widget get the command and handles the drag with mouse, drawing a rect
// mouse up event in the

impl<W: Widget<EditData>> Widget<EditData> for DragZone<W> {
    fn event(&mut self, ctx: &mut EventCtx, event: &Event, data: &mut EditData, env: &Env) {
        // let inner handle this event first
        self.inner.event(ctx, event, data, env);

        match event {
            Event::Command(cmd) if cmd.selector == DRAG_START => {
                let drag = cmd.get_object::<DragData>().unwrap().clone();
                // we have to set the rect size here based on the edit
                // data since the resizer doesn't know the size of the
                // drag widget OR NOT
                // let sid = data.graph.wids[&drag.id];
                // let rect = data.graph.graph.get(sid).edit_data.rect;
                // drag.rect0 = drag.rect0.with_size(rect.size());
                // drag.rect1 = drag.rect0.clone();
                self.drag = Some(drag);
                ctx.request_paint();
            }
            Event::Command(cmd) if cmd.selector == DRAG_END => {
                if !ctx.is_handled() {
                    let drag = cmd.get_object::<DragData>().unwrap().clone();
                    println!("unhandled drag: {:?}", drag);
                }
                ctx.request_paint();
            }
            _ => (),
        }
    }

    fn lifecycle(&mut self, ctx: &mut LifeCycleCtx, event: &LifeCycle, data: &EditData, env: &Env) {
        self.inner.lifecycle(ctx, event, data, env);
    }

    fn update(&mut self, ctx: &mut UpdateCtx, old_data: &EditData, data: &EditData, env: &Env) {
        self.inner.update(ctx, old_data, data, env);
    }

    fn layout(
        &mut self,
        ctx: &mut LayoutCtx,
        bc: &BoxConstraints,
        data: &EditData,
        env: &Env,
    ) -> Size {
        self.inner.layout(ctx, bc, data, env)
    }

    fn paint(&mut self, ctx: &mut PaintCtx, data: &EditData, env: &Env) {
        self.inner.paint(ctx, data, env);

        if let Some(drag) = &self.drag {
            // we could do this inside the widget on drag move?
            //ctx.transform(Affine::translate(offset));
            //self.inner.paint_with_offset_always(ctx, data, env);

            let color = env.get(theme::SELECTION_COLOR);
            // set alpha?
            let rect = RoundedRect::from_rect(drag.rect1, 4.);
            ctx.stroke(rect, &color, 4.);
        }
    }
}

// we have to know the widget id to drag on creation
pub struct Drag<T, W> {
    drag_id: WidgetId,
    drag: Option<DragData>,
    inner: W,
    // make the resizer optional?
    resizer: Align<T>,
}

impl<T: Data, W: Widget<T>> Drag<T, W> {
    pub fn new(inner: W, drag_id: WidgetId) -> Self {
        Self {
            drag_id,
            drag: None,
            inner,
            resizer: Align::new(UnitPoint::BOTTOM_RIGHT, Resizer::new(drag_id)),
        }
    }
}

impl<T: Data, W: Widget<T>> Widget<T> for Drag<T, W> {
    fn event(&mut self, ctx: &mut EventCtx, event: &Event, data: &mut T, env: &Env) {
        // resize takes precedence
        self.resizer.event(ctx, event, data, env);

        // if the resizer handled this mouse down event, submit resize command
        if ctx.is_handled() {
            match event {
                Event::MouseDown(mouse) => {
                    let drag = DragData::new(mouse, ctx.size(), self.drag_id, true);
                    //ctx.submit_command(Command::new(DRAG_START, drag), None);
                    self.drag = Some(drag);
                    ctx.set_active(true);
                    ctx.request_paint();
                }
                _ => {}
            }
        } else {
            self.inner.event(ctx, event, data, env);
        }

        if !ctx.is_handled() {
            match event {
                Event::MouseDown(mouse) => {
                    let drag = DragData::new(mouse, ctx.size(), self.drag_id, false);
                    //ctx.submit_command(Command::new(DRAG_START, drag), None);
                    self.drag = Some(drag);
                    ctx.set_active(true);
                    ctx.set_handled();
                    ctx.request_paint();
                }
                _ => {}
            }
        }

        match event {
            Event::MouseUp(mouse) => {
                if let Some(mut drag) = self.drag.take() {
                    // put the drag data into window coords
                    let d = mouse.window_pos - mouse.pos;
                    //drag.p0 += d;
                    drag.rect1 = drag.rect1.with_origin(drag.rect1.origin() + d);
                    ctx.submit_command(Command::new(DRAG_END, drag), None);
                    ctx.set_active(false);
                    ctx.request_paint();
                }
            }
            Event::MouseMoved(mouse) => {
                if let Some(ref mut drag) = self.drag {
                    if drag.resize {
                        let size = ctx.size().to_vec2() + (mouse.pos - drag.p0);
                        // force minimum size, use a better value for this TODO:
                        // validation function? do this in lib
                        let size = Size::new(size.x.max(100.), size.y.max(40.));
                        drag.rect1 = drag.rect1.with_size(size);
                    } else {
                        // keep it inside the widget
                        // fit the moved state inside its parent when the drag ends?
                        let origin = mouse.pos - drag.p0.to_vec2();
                        //let origin = Point::new(origin.x.max(0.), origin.y.max(0.));
                        drag.rect1 = drag.rect1.with_origin(origin);
                    }
                    ctx.request_paint();
                }
            }
            _ => (),
        }
    }

    fn lifecycle(&mut self, ctx: &mut LifeCycleCtx, event: &LifeCycle, data: &T, env: &Env) {
        self.resizer.lifecycle(ctx, event, data, env);
        self.inner.lifecycle(ctx, event, data, env)
    }

    fn update(&mut self, ctx: &mut UpdateCtx, old_data: &T, data: &T, env: &Env) {
        self.resizer.update(ctx, old_data, data, env);
        self.inner.update(ctx, old_data, data, env);
    }

    fn layout(&mut self, ctx: &mut LayoutCtx, bc: &BoxConstraints, data: &T, env: &Env) -> Size {
        self.resizer.layout(ctx, bc, data, env);
        self.inner.layout(ctx, bc, data, env)
    }

    fn paint(&mut self, ctx: &mut PaintCtx, data: &T, env: &Env) {
        self.inner.paint(ctx, data, env);
        // paint on top
        self.resizer.paint(ctx, data, env);

        if let Some(drag) = &self.drag {
            let color = env.get(theme::SELECTION_COLOR);
            let rect = RoundedRect::from_rect(drag.rect1, 4.);
            ctx.stroke(rect, &color, 4.);
        }
    }
}

pub struct Resizer {
    drag_id: WidgetId,
}

impl Resizer {
    pub fn new(drag_id: WidgetId) -> Self {
        Self { drag_id }
    }
}

impl<T: Data> Widget<T> for Resizer {
    fn event(&mut self, ctx: &mut EventCtx, event: &Event, _data: &mut T, _env: &Env) {
        match event {
            Event::MouseDown(_mouse) => {
                // this tells the drag widget to submit the resize command
                ctx.set_handled();
                ctx.set_active(true);
                ctx.request_paint();
            }
            Event::MouseUp(_) => {
                ctx.set_active(false);
                ctx.request_paint();
            }
            _ => {}
        }
    }

    fn lifecycle(&mut self, ctx: &mut LifeCycleCtx, event: &LifeCycle, _data: &T, _env: &Env) {
        match event {
            LifeCycle::HotChanged(_) => {
                ctx.request_paint();
            }
            _ => {}
        }
    }

    fn update(&mut self, _ctx: &mut UpdateCtx, _old_data: &T, _data: &T, _env: &Env) {}

    fn layout(
        &mut self,
        _ctx: &mut LayoutCtx,
        _bc: &BoxConstraints,
        _data: &T,
        _env: &Env,
    ) -> Size {
        Size::new(12., 12.)
    }

    fn paint(&mut self, paint_ctx: &mut PaintCtx, _data: &T, env: &Env) {
        let active = paint_ctx.is_active();
        if active || paint_ctx.is_hot() {
            let Size { width, height } = paint_ctx.size();

            let color = env.get(if active {
                theme::SELECTION_COLOR
            } else {
                theme::LABEL_COLOR
            });

            let mut arrow = BezPath::new();
            arrow.move_to(Point::new(width - 1., 1.));
            arrow.line_to(Point::new(width - 1., height - 1.));
            arrow.line_to(Point::new(1., height - 1.));
            arrow.close_path();

            paint_ctx.fill(arrow, &color);
        }
    }
}
