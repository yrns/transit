use crate::EditData;
use druid::{
    kurbo::{BezPath, Point, Rect, RoundedRect, Size, Vec2},
    theme,
    widget::Align,
    Affine, BoxConstraints, Command, Cursor, Data, Env, Event, EventCtx, LayoutCtx, LifeCycle,
    LifeCycleCtx, MouseButton, MouseEvent, PaintCtx, RenderContext, Selector, Target, UnitPoint,
    UpdateCtx, Widget, WidgetId, WidgetPod,
};
use transit::{Idx, TransIdx};

pub const DRAG_START: Selector = Selector::new("transit.edit.drag-start");
pub const DRAG_END: Selector = Selector::new("transit.edit.drag-end");

// TODO:? these might be useful to paint drag targets, but we'd need
// to filter like DRAG_END to be useful
//pub const DRAG_OVER: Selector = Selector::new("transit.edit.drag-end");
//pub const DRAG_OUT: Selector = Selector::new("transit.edit.drag-end");

// a resize doesn't have a target widget, so maybe it's a different event?
// pub const RESIZE_START: Selector = Selector::new("transit.edit.resize-start");
// pub const RESIZE_END: Selector = Selector::new("transit.edit.resize-end");

// this is used register the drag handle's target widget id, rather
// than passing the id down on create
pub const REGISTER_DRAG_HANDLE: Selector = Selector::new("transit.edit.register-drag-handle");

#[derive(Debug, Copy, Clone)]
pub enum DragType {
    MoveState(Idx),
    // resize direction for edge drag?
    ResizeState(Idx),
    MoveInitial(Option<Idx>),
    CreateTransition(Idx),
    MoveTransition(TransIdx),
}

pub type DragTypeSelector = Box<dyn Fn(&MouseEvent) -> DragType>;

impl Into<DragTypeSelector> for DragType {
    fn into(self) -> DragTypeSelector {
        Box::new(move |_| self)
    }
}

#[derive(Debug, Clone)]
pub struct DragData {
    // original mouse down
    pub p0: Point,
    // offset to get into window coords
    //pub offset: Vec2,
    // drag mirror, relative to widget origin
    pub rect1: Rect,
    pub id: WidgetId,
    pub ty: DragType,
}

impl DragData {
    pub fn new(p0: Point, size: Size, id: WidgetId, ty: DragType) -> Self {
        DragData {
            p0,
            //offset: Vec2::ZERO,
            rect1: Rect::from_origin_size(Point::ZERO, size),
            id,
            ty,
        }
    }

    // this is where the mouse is inside the mirror
    pub fn anchor(&self) -> Point {
        self.rect1.origin() + self.p0.to_vec2()
    }

    pub fn offset(&self, v: Vec2) -> Self {
        let mut drag = self.clone();
        let origin = drag.rect1.origin() - v;
        drag.rect1 = drag.rect1.with_origin(origin);
        drag
    }

    // wrap into an event
    pub fn to_event(self) -> Event {
        Event::Command(Command::new(DRAG_END, self))
    }
}

pub struct Drag<T, W> {
    drag: Option<DragData>,
    ty: DragTypeSelector,
    // we use WidgetPod here only so we can tell if the inner is active
    inner: WidgetPod<T, W>,
    // we cannot use Align here because we need to access the handle's
    // drag type, Align (or any other container) obscures the inner
    handle: Option<Align<T>>,
    handle_p0: Point,
}

impl<T: Data, W: Widget<T>> Drag<T, W> {
    pub fn new<D>(inner: W, ty: D, handle: Option<DragTypeSelector>) -> Self
    where
        D: Into<DragTypeSelector>,
    {
        let handle = handle.map(|a| Align::new(UnitPoint::BOTTOM_RIGHT, Resizer::new(a)));
        Self {
            drag: None,
            ty: ty.into(),
            inner: WidgetPod::new(inner),
            handle,
            handle_p0: Point::ZERO,
        }
    }

    pub fn inner(&self) -> &W {
        self.inner.widget()
    }

    // no longer needed?
    pub fn has_active(&self) -> bool {
        self.inner.has_active()
    }
}

// TODO: add a minimum drag distance so we're not interpreting every click as a drag?
impl<T: Data, W: Widget<T>> Widget<T> for Drag<T, W> {
    fn event(&mut self, ctx: &mut EventCtx, event: &Event, data: &mut T, env: &Env) {
        let new_drag = |ctx: &mut EventCtx, p, ty| {
            ctx.set_active(true);
            ctx.request_paint();
            DragData::new(p, ctx.size(), ctx.widget_id(), ty)
        };

        // resize takes precedence; we can't access the drag type
        // directly since Align hides it, so we receive an event
        // instead
        if let Some(handle) = self.handle.as_mut() {
            handle.event(ctx, event, data, env);

            if ctx.is_handled() {
                // this is a hack to save the mouse position; we can't
                // access the layout_rect inside Align
                if let Event::MouseDown(mouse) = event {
                    self.handle_p0 = mouse.pos;
                }
                return;
            }
        }

        match event {
            Event::Command(cmd) if cmd.selector == DRAG_START => {
                let ty = cmd.get_object::<DragType>().unwrap().clone();
                self.drag = Some(new_drag(ctx, self.handle_p0, ty));
                return;
            }
            _ => {}
        }

        self.inner.event(ctx, event, data, env);

        if ctx.is_handled() || self.inner.has_active() {
            return;
        }

        match event {
            Event::MouseDown(mouse) => {
                self.drag = Some(new_drag(ctx, mouse.pos, (self.ty)(mouse)));
                ctx.set_handled();
            }
            Event::MouseUp(mouse) => {
                if let Some(mut drag) = self.drag.take() {
                    // put the drag data into window coords
                    let d = mouse.window_pos - mouse.pos;
                    drag.rect1 = drag.rect1.with_origin(drag.rect1.origin() + d);
                    let target = match drag.ty {
                        // initial can only be moved within the
                        // current state, but the widget id is wrong
                        // from here, so handle this in root.
                        //DragType::MoveInitial(_) => Target::Widget(ctx.widget_id()),
                        _ => Target::Global,
                    };
                    ctx.submit_command(Command::new(DRAG_END, drag), target);
                    ctx.set_active(false);
                    ctx.request_paint();
                }
            }
            Event::MouseMoved(mouse) => {
                if let Some(drag) = self.drag.as_mut() {
                    match drag.ty {
                        DragType::ResizeState(_) => {
                            let size = ctx.size().to_vec2() + (mouse.pos - drag.p0);
                            // force minimum size, use a better value for this TODO:
                            // validation function? do this in lib
                            let size = Size::new(size.x.max(100.), size.y.max(40.));
                            drag.rect1 = drag.rect1.with_size(size);
                        }
                        // use absolute positioning?
                        //DragType::CreateTransition(_) => {}
                        _ => {
                            // keep it inside the widget
                            // fit the moved state inside its parent when the drag ends?
                            let origin = mouse.pos - drag.p0.to_vec2();
                            //let origin = Point::new(origin.x.max(0.), origin.y.max(0.));
                            drag.rect1 = drag.rect1.with_origin(origin);
                        }
                    }
                    ctx.request_paint();
                }
            }
            _ => (),
        }
    }

    fn lifecycle(&mut self, ctx: &mut LifeCycleCtx, event: &LifeCycle, data: &T, env: &Env) {
        if let Some(handle) = self.handle.as_mut() {
            match event {
                LifeCycle::WidgetAdded => {
                    // send our widget id to the handle so it can send us events
                    let id = ctx.widget_id();
                    ctx.submit_command(
                        Command::new(REGISTER_DRAG_HANDLE, id),
                        // this sends the event to us and all descendants
                        id,
                    );
                }
                _ => (),
            }
            handle.lifecycle(ctx, event, data, env);
        }
        self.inner.lifecycle(ctx, event, data, env)
    }

    fn update(&mut self, ctx: &mut UpdateCtx, old_data: &T, data: &T, env: &Env) {
        if let Some(handle) = self.handle.as_mut() {
            handle.update(ctx, old_data, data, env);
        }
        self.inner.update(ctx, data, env);
    }

    fn layout(&mut self, ctx: &mut LayoutCtx, bc: &BoxConstraints, data: &T, env: &Env) -> Size {
        if let Some(handle) = self.handle.as_mut() {
            handle.layout(ctx, bc, data, env);
        }
        let size = self.inner.layout(ctx, bc, data, env);
        // should there be a warning on zero-size widgets? I always
        // forget this part when changing Widget -> WidgetPod
        self.inner
            .set_layout_rect(Rect::from_origin_size(Point::ORIGIN, size));
        size
    }

    fn paint(&mut self, ctx: &mut PaintCtx, data: &T, env: &Env) {
        self.inner.paint_with_offset(ctx, data, env);

        // paint on top
        if let Some(handle) = self.handle.as_mut() {
            handle.paint(ctx, data, env);
        }

        if let Some(drag) = &self.drag {
            let color = env.get(theme::SELECTION_COLOR);
            let rect = RoundedRect::from_rect(drag.rect1, 4.);
            ctx.stroke(rect, &color, 4.);
        }
    }
}

pub struct Resizer {
    ty: DragTypeSelector,
    parent: Option<WidgetId>,
}

impl Resizer {
    pub fn new(ty: DragTypeSelector) -> Self {
        Self { ty, parent: None }
    }
}

impl<T: Data> Widget<T> for Resizer {
    fn event(&mut self, ctx: &mut EventCtx, event: &Event, _data: &mut T, _env: &Env) {
        match event {
            Event::Command(cmd) if cmd.selector == REGISTER_DRAG_HANDLE => {
                self.parent = Some(cmd.get_object::<WidgetId>().unwrap().clone());
            }
            Event::MouseDown(mouse) => {
                // this tells the drag widget to start resize via handle
                if let Some(id) = self.parent {
                    ctx.submit_command(Command::new(DRAG_START, (self.ty)(mouse)), id);
                    ctx.set_handled();
                    ctx.set_active(true);
                    ctx.request_paint();
                } else {
                    log::warn!("handle parent is None");
                }
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
