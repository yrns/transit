use druid::kurbo::BezPath;
use druid::piet::{FontBuilder, ImageFormat, InterpolationMode, Text, TextLayoutBuilder};

use druid::kurbo::RoundedRect;
use druid::theme;
use druid::widget::{Align, Flex, Label, LabelText, Padding, SizedBox, WidgetExt};
use druid::{
    Affine, AppLauncher, BoxConstraints, Color, Data, Env, Event, EventCtx, LayoutCtx, LifeCycle,
    LifeCycleCtx, LinearGradient, LocalizedString, PaintCtx, Point, Rect, RenderContext, Size,
    UnitPoint, UpdateCtx, Widget, WindowDesc,
};

// each state contains a label and child states
pub struct State<T: Data> {
    flex: Flex<T>,
    //label: Label<T>,
}

impl<T: Data + 'static> State<T> {
    pub fn new(text: impl Into<LabelText<T>>) -> Self {
        let flex = Flex::column()
            .with_child(
                Flex::row().with_child(
                    //SizedBox::new(
                    Label::new(text).text_align(UnitPoint::LEFT).padding(2.0),
                    //).height(20.0),
                    1.0,
                ),
                0.0,
            )
            // empty row for child states
            .with_child(Flex::row(), 1.0);

        Self { flex }
    }
}

impl<T: Data + 'static> Widget<T> for State<T> {
    fn event(&mut self, ctx: &mut EventCtx, event: &Event, _data: &mut T, _env: &Env) {
        match event {
            Event::MouseDown(_) => {
                //ctx.set_active(true);
                //ctx.invalidate();
            }
            Event::MouseUp(_) => {
                if ctx.is_active() {
                    ctx.set_active(false);
                    ctx.invalidate();
                    if ctx.is_hot() {
                        //(self.action)(ctx, data, env);
                    }
                }
            }
            _ => (),
        }
    }

    fn lifecycle(&mut self, ctx: &mut LifeCycleCtx, event: &LifeCycle, _data: &T, _env: &Env) {
        match event {
            LifeCycle::HotChanged(_) => {
                ctx.invalidate();
            }
            _ => (),
        }
    }

    fn update(&mut self, ctx: &mut UpdateCtx, old_data: &T, data: &T, env: &Env) {
        self.flex.update(ctx, old_data, data, env)
    }

    fn layout(
        &mut self,
        layout_ctx: &mut LayoutCtx,
        bc: &BoxConstraints,
        data: &T,
        env: &Env,
    ) -> Size {
        //bc.constrain(Size::new(200.0, 60.0))
        self.flex.layout(layout_ctx, bc, data, env)
        //bc.max()
    }

    fn paint(&mut self, paint_ctx: &mut PaintCtx, data: &T, env: &Env) {
        // paint_ctx.clear(Color::WHITE);

        //let is_active = paint_ctx.is_active();
        let is_hot = paint_ctx.is_hot();

        let rounded_rect =
            RoundedRect::from_origin_size(Point::ORIGIN, paint_ctx.size().to_vec2(), 4.);

        let border_color = env.get(theme::BORDER);

        let border_size = if is_hot { 5.0 } else { 4.0 };

        paint_ctx.stroke(rounded_rect, &border_color, border_size);

        paint_ctx.fill(rounded_rect, &Color::WHITE);

        self.flex.paint(paint_ctx, data, env);
    }
}
