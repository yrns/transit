use crate::widgets::{Drag, Root, STATE_ADDED};
use crate::EditData;
use druid::kurbo::BezPath;
use druid::kurbo::RoundedRect;
use druid::piet::{FontBuilder, ImageFormat, InterpolationMode, Text, TextLayoutBuilder};
use druid::theme;
use druid::widget::{Align, Flex, Label, LabelText, Padding, SizedBox, WidgetExt};
use druid::{
    Affine, AppLauncher, BoxConstraints, Color, Command, Data, Env, Event, EventCtx, LayoutCtx,
    LifeCycle, LifeCycleCtx, LinearGradient, LocalizedString, MouseButton, MouseEvent, PaintCtx,
    Point, Rect, RenderContext, Size, UnitPoint, UpdateCtx, Widget, WidgetId, WindowDesc,
};
use std::fmt;
use transit::{Graph, Idx, State as TransitState, Transition};

// each state contains a label and child states
pub struct State {
    id: WidgetId,
    // should be state index? sidx? si?
    pub sid: Idx,
    header: Flex<EditData>,
    // having the drag on the inner means the header will take
    // precedence over the resize - FIX:?
    inner: Drag<EditData, Root>,
    //history: History,
    //root: Root, // children
}

impl State {
    pub fn new(sid: Idx) -> Self {
        // we set our own id so we can pass it to the drag widget
        let id = WidgetId::next();

        let header = Flex::column()
            .with_child(
                Flex::row()
                    .with_child(
                        // replace this with a state label that is editable:
                        Label::new(move |data: &EditData, _env: &_| {
                            format!("{}", data.graph1.graph.graph[sid].id())
                        })
                        .text_align(UnitPoint::LEFT)
                        .padding(2.),
                        0.0,
                    )
                    // can't read the env here
                    .background(Color::WHITE)
                    .padding(4.),
                0.0,
            )
            .with_child(SizedBox::empty(), 1.0);

        let inner = Drag::new(Root::new(Some(sid)), id);

        Self {
            id,
            sid,
            header,
            inner,
        }
    }
}

impl fmt::Debug for State {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "State {{ id: {:?}, sid: {:?} }}", self.id, self.sid)
    }
}

impl Widget<EditData> for State {
    fn id(&self) -> Option<WidgetId> {
        Some(self.id)
    }

    fn event(&mut self, ctx: &mut EventCtx, event: &Event, data: &mut EditData, env: &Env) {
        self.header.event(ctx, event, data, env);
        self.inner.event(ctx, event, data, env);
    }

    fn lifecycle(&mut self, ctx: &mut LifeCycleCtx, event: &LifeCycle, data: &EditData, env: &Env) {
        match event {
            LifeCycle::WidgetAdded => {
                ctx.submit_command(Command::new(STATE_ADDED, (ctx.widget_id(), self.sid)), None);
            }
            LifeCycle::HotChanged(_) => {
                ctx.request_paint();
            }
            LifeCycle::FocusChanged(_) => {
                ctx.request_paint();
            }
            _ => (),
        }

        self.header.lifecycle(ctx, event, data, env);
        self.inner.lifecycle(ctx, event, data, env);
    }

    fn update(&mut self, ctx: &mut UpdateCtx, old_data: &EditData, data: &EditData, env: &Env) {
        self.header.update(ctx, old_data, data, env);
        self.inner.update(ctx, old_data, data, env);
    }

    fn layout(
        &mut self,
        layout_ctx: &mut LayoutCtx,
        bc: &BoxConstraints,
        data: &EditData,
        env: &Env,
    ) -> Size {
        // it's at the origin - we don't need to set the layout rect?
        let _header_size = self.header.layout(layout_ctx, bc, data, env);

        self.inner.layout(layout_ctx, bc, data, env)
    }

    fn paint(&mut self, paint_ctx: &mut PaintCtx, data: &EditData, env: &Env) {
        let is_hot = paint_ctx.is_hot();

        // neither drag nor root are in a WidgetPod, so requesting
        // focus in the root gives us focus, this seems weird but it
        // works
        let has_focus = paint_ctx.has_focus();

        let rounded_rect =
            RoundedRect::from_origin_size(Point::ORIGIN, paint_ctx.size().to_vec2(), 4.);

        let border_color = if has_focus {
            env.get(theme::PRIMARY_LIGHT)
        } else {
            env.get(theme::BORDER_LIGHT)
        };

        let border_size = if is_hot { 4.0 } else { 2.0 };

        paint_ctx.stroke(rounded_rect, &border_color, border_size);
        paint_ctx.fill(rounded_rect, &env.get(theme::BACKGROUND_LIGHT));

        self.inner.paint(paint_ctx, data, env);
        self.header.paint(paint_ctx, data, env);
    }
}
