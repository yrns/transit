use crate::{widgets::*, *};
use druid::{kurbo::*, lens, lens::*, piet::*, theme, widget::*, *};
use std::fmt;
use std::sync::Arc;
use transit::Idx;

#[derive(Debug, Copy, Clone)]
pub struct StateIdLens {
    idx: Idx,
}

impl StateIdLens {
    pub fn new(idx: Idx) -> Self {
        Self { idx }
    }
}

// we require the graph to force uniqueness among siblings for the id
impl Lens<Graph, String> for StateIdLens {
    fn with<V, F: FnOnce(&String) -> V>(&self, g: &Graph, f: F) -> V {
        f(&g[self.idx].id)
    }

    fn with_mut<V, F: FnOnce(&mut String) -> V>(&self, g: &mut Graph, f: F) -> V {
        let mut temp = g[self.idx].id.clone();
        let v = f(&mut temp);
        let p = g[self.idx].parent;
        match unique_id(g, &temp, p) {
            Ok(uid) => Arc::make_mut(&mut g[self.idx]).id = uid.unwrap_or(temp),
            Err(e) => {
                log::error!("error in state id lens: {}", e);
            }
        }
        v
    }
}

// each state contains a label and child states
pub struct State {
    id: WidgetId,
    // should be state index? sidx? si?
    pub sid: Idx,
    layers: Layers<EditData>,
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

        let layers = Layers::new()
            .add_layer(
                // align top left in the layer
                Flex::row()
                    .cross_axis_alignment(CrossAxisAlignment::Start)
                    .with_child(TextBox::new().lens(state_id_lens(sid)))
                    .padding(4.),
                None,
            )
            .add_layer(Root::new(Some(sid)), None);

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
                        // don't set handled, we still want to drag
                        ctx.request_focus();
                    }
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
