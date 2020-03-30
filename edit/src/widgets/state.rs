use crate::widgets::{Drag, Resizer, Root, STATE_ADDED};
use crate::{handle_key, state_id_lens, EditData, GraphData};
use druid::kurbo::BezPath;
use druid::kurbo::RoundedRect;
use druid::piet::{FontBuilder, ImageFormat, InterpolationMode, Text, TextLayoutBuilder};
use druid::theme;
use druid::widget::{
    Align, CrossAxisAlignment, Flex, Label, LabelText, Padding, SizedBox, TextBox, WidgetExt,
};
use druid::{
    lens,
    lens::{Field, InArc, Lens, Then},
    *,
};
use std::fmt;
use std::sync::Arc;
use transit::{Graph, Idx, State as TransitState, Transition};

// each state contains a label and child states
pub struct State {
    id: WidgetId,
    // should be state index? sidx? si?
    pub sid: Idx,
    layers: Drag<EditData>,
    //header: WidgetPod<EditData, Flex<EditData>>,
    //header: Flex<EditData>,
    // having the drag on the inner means the header will take
    // precedence over the resize - FIX:?
    //inner: Drag<Root>,
    //history: History,
    //root: Root, // children
}

impl State {
    pub fn new(sid: Idx) -> Self {
        // we set our own id so we can pass it to the drag widget
        let id = WidgetId::next();

        let layers = Drag::new(
            Layers::new()
                .add_layer(
                    // align top left in the layer
                    Flex::row()
                        .cross_axis_alignment(CrossAxisAlignment::Start)
                        .with_child(TextBox::new().lens(state_id_lens(sid)))
                        .padding(4.),
                    None,
                )
                .add_layer(Root::new(Some(sid)), None),
            id,
        );

        Self {
            id,
            sid,
            layers,
            // header,
            // inner,
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
        self.layers.event(ctx, event, data, env);

        if ctx.is_handled() {
            return;
        }

        match event {
            Event::MouseDown(_) => {
                // the id textbox will set itself active on mouse
                // down, which is how we detect this event was handled
                // already
                if !self.layers.has_active() {
                    if !ctx.is_focused() {
                        ctx.request_focus();
                    }
                    ctx.set_handled();
                }
            }
            Event::KeyDown(e) => {
                if ctx.is_focused() {
                    handle_key(ctx, e, data, Some(self.sid));
                    ctx.set_handled();
                }
            }
            _ => (),
        }
    }

    fn lifecycle(&mut self, ctx: &mut LifeCycleCtx, event: &LifeCycle, data: &EditData, env: &Env) {
        match event {
            LifeCycle::WidgetAdded => {
                ctx.submit_command(Command::new(STATE_ADDED, (ctx.widget_id(), self.sid)), None);
                ctx.register_for_focus();
            }
            LifeCycle::HotChanged(_) => {
                ctx.request_paint();
            }
            LifeCycle::FocusChanged(focus) => {
                log::info!("focus changed for state {:?}: {}", ctx.widget_id(), focus);
                ctx.request_paint();
            }
            _ => (),
        }

        self.layers.lifecycle(ctx, event, data, env);
    }

    fn update(&mut self, ctx: &mut UpdateCtx, old_data: &EditData, data: &EditData, env: &Env) {
        self.layers.update(ctx, old_data, data, env);
    }

    fn layout(
        &mut self,
        ctx: &mut LayoutCtx,
        bc: &BoxConstraints,
        data: &EditData,
        env: &Env,
    ) -> Size {
        self.layers.layout(ctx, bc, data, env)
    }

    fn paint(&mut self, ctx: &mut PaintCtx, data: &EditData, env: &Env) {
        let is_hot = ctx.is_hot();

        let rounded_rect = RoundedRect::from_origin_size(Point::ORIGIN, ctx.size().to_vec2(), 4.);

        let border_color = if ctx.is_focused() {
            env.get(theme::PRIMARY_LIGHT)
        } else {
            env.get(theme::BORDER_LIGHT)
        };

        let border_size = if is_hot { 4.0 } else { 2.0 };

        ctx.stroke(rounded_rect, &border_color, border_size);
        ctx.fill(rounded_rect, &env.get(theme::BACKGROUND_LIGHT));

        self.layers.paint(ctx, data, env);
    }
}

struct Layers<T> {
    children: Vec<WidgetPod<T, Box<dyn Widget<T>>>>,
}

impl<T: Data> Layers<T> {
    pub fn new() -> Self {
        Self {
            children: Vec::new(),
        }
    }

    // currently unused
    pub fn has_active(&self) -> bool {
        self.children.iter().any(|a| a.has_active())
    }

    pub fn add_layer(mut self, layer: impl Widget<T> + 'static, align: Option<UnitPoint>) -> Self {
        let layer = match align {
            Some(align) => WidgetPod::new(Align::new(align, layer)).boxed(),
            None => WidgetPod::new(layer).boxed(),
        };
        self.children.push(layer);
        self
    }
}

impl<T: Data> Widget<T> for Layers<T> {
    fn event(&mut self, ctx: &mut EventCtx, event: &Event, data: &mut T, env: &Env) {
        for layer in &mut self.children {
            layer.event(ctx, event, data, env);

            // if the event is handled already or the layer is now
            // active, stop recursing
            if ctx.is_handled() || layer.has_active() {
                break;
            }
        }
    }

    fn lifecycle(&mut self, ctx: &mut LifeCycleCtx, event: &LifeCycle, data: &T, env: &Env) {
        for layer in &mut self.children {
            layer.lifecycle(ctx, event, data, env);
        }
    }

    fn update(&mut self, ctx: &mut UpdateCtx, _old_data: &T, data: &T, env: &Env) {
        for layer in &mut self.children {
            layer.update(ctx, data, env);
        }
    }

    fn layout(&mut self, ctx: &mut LayoutCtx, bc: &BoxConstraints, data: &T, env: &Env) -> Size {
        for layer in &mut self.children {
            let layer_size = layer.layout(ctx, bc, data, env);
            layer.set_layout_rect(Rect::from_origin_size(Point::ORIGIN, layer_size));
        }

        bc.max()
    }

    fn paint(&mut self, ctx: &mut PaintCtx, data: &T, env: &Env) {
        // draw lower priority layers first
        for layer in &mut self.children.iter_mut().rev() {
            layer.paint(ctx, data, env);
        }
    }
}
