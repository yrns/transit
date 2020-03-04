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
    pub sid: Idx,
    inner: Drag<EditData, Flex<EditData>>,
    //history: History,
    //root: Root, // children
}

impl State {
    pub fn new(sid: Idx) -> Self {
        // we set our own id so we can pass it to the drag widget
        let id = WidgetId::next();
        let inner = Drag::new(
            Flex::column()
                .with_child(
                    Flex::row().with_child(
                        // replace this with a state label that is editable:
                        Label::new(move |data: &EditData, _env: &_| {
                            format!("{}", data.graph.graph.graph[sid].id())
                        })
                        .text_align(UnitPoint::LEFT)
                        .padding(2.0),
                        1.0,
                    ),
                    // with_child action handlers here
                    0.0,
                )
                // horizontal rule that only draws when this state has children
                //.with_child(Sep::new(), 0.0);
                // empty row for child states
                .with_child(Root::new(Some(sid)), 1.0),
            id,
        );

        Self { id, sid, inner }
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
            _ => (),
        }

        self.inner.lifecycle(ctx, event, data, env);
    }

    fn update(&mut self, ctx: &mut UpdateCtx, old_data: &EditData, data: &EditData, env: &Env) {
        self.inner.update(ctx, old_data, data, env)
    }

    fn layout(
        &mut self,
        layout_ctx: &mut LayoutCtx,
        bc: &BoxConstraints,
        data: &EditData,
        env: &Env,
    ) -> Size {
        self.inner.layout(layout_ctx, bc, data, env)
    }

    fn paint(&mut self, paint_ctx: &mut PaintCtx, data: &EditData, env: &Env) {
        let is_hot = paint_ctx.is_hot();

        let rounded_rect =
            RoundedRect::from_origin_size(Point::ORIGIN, paint_ctx.size().to_vec2(), 4.);

        let border_color = env.get(theme::BORDER_LIGHT);

        let border_size = if is_hot { 5.0 } else { 4.0 };

        paint_ctx.stroke(rounded_rect, &border_color, border_size);

        paint_ctx.fill(rounded_rect, &Color::WHITE);

        self.inner.paint(paint_ctx, data, env);
    }
}
