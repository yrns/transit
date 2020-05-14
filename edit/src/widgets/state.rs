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

// Removed uniqueness for now. Revert to combinators since we don't
// need the graph?
impl Lens<Graph, String> for StateIdLens {
    fn with<V, F: FnOnce(&String) -> V>(&self, g: &Graph, f: F) -> V {
        f(&g[self.idx].id)
    }

    fn with_mut<V, F: FnOnce(&mut String) -> V>(&self, g: &mut Graph, f: F) -> V {
        let id = &g[self.idx].id;
        let mut temp = id.clone();
        let v = f(&mut temp);
        if temp != *id {
            Arc::make_mut(&mut g[self.idx]).id = temp;
        }
        // let p = g[self.idx].parent;
        // match unique_id(g, &temp, p) {
        //     Ok(uid) => Arc::make_mut(&mut g[self.idx]).id = uid.unwrap_or(temp),
        //     Err(e) => {
        //         log::error!("error in state id lens: {}", e);
        //     }
        // }
        v
    }
}

// each state contains a label and child states
pub struct State {
    id: WidgetId,
    // should be state index? sidx? si?
    sid: Idx,
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
                    .with_child(IdBox::new().lens(state_id_lens(sid)))
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
        // key overrides for the textbox
        // if let Event::KeyDown(e) = event {
        //     if HotKey::new(None, KeyCode::Return).matches(e) {
        //         ctx.focus_next();
        //         ctx.set_handled();
        //         return;
        //     }
        // }

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
            Event::MouseMove(_mouse) => {
                ctx.submit_command(
                    Command::new(UPDATE_HOVER, format!("state: {:?}", ctx.widget_id())),
                    None,
                );
            }
            Event::Command(cmd) if cmd.selector == FOCUS_STATE => {
                let i = cmd.get_object::<Idx>().unwrap();
                // Have to check the index otherwise all descendant
                // states will request focus too.
                if *i == self.sid {
                    ctx.request_focus();
                }
            }
            _ => (),
        }
    }

    fn lifecycle(&mut self, ctx: &mut LifeCycleCtx, event: &LifeCycle, data: &EditData, env: &Env) {
        match event {
            LifeCycle::WidgetAdded => {
                ctx.register_for_focus();
                // We can't request focus here so we have to submit an
                // event to ourselves.
                if data.graph1.retain_focus == Some(self.sid) {
                    ctx.submit_command(Command::new(FOCUS_STATE, self.sid), ctx.widget_id());
                }
            }
            LifeCycle::HotChanged(_) | LifeCycle::FocusChanged(_) => ctx.request_paint(),
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

        let stroke_width = if is_hot { 4.0 } else { 2.0 };
        let rect = ctx
            .size()
            .to_rect()
            .inset(-stroke_width / 2.)
            .to_rounded_rect(4.);

        let border_color = if ctx.is_focused() {
            env.get(theme::PRIMARY_LIGHT)
        } else {
            env.get(theme::BORDER_LIGHT)
        };

        ctx.stroke(rect, &border_color, stroke_width);
        ctx.fill(rect, &env.get(theme::BACKGROUND_LIGHT));

        self.layers.paint(ctx, data, env);
    }
}

pub struct Layers<T> {
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

impl<T> fmt::Debug for Layers<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Layers {{ children: {:?} }}",
            self.children
                .iter()
                .map(|w| format!("{:?}", w.id()))
                .collect::<Vec<String>>()
                .join(" ")
        )
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
        // Size doesn't have union.
        let mut max = Rect::ZERO;
        for layer in &mut self.children {
            let layer_size = layer.layout(ctx, bc, data, env);
            layer.set_layout_rect(ctx, data, env, layer_size.to_rect());
            max = max.union(layer_size.to_rect());
        }
        max.size()
    }

    fn paint(&mut self, ctx: &mut PaintCtx, data: &T, env: &Env) {
        // draw lower priority layers first
        for layer in &mut self.children.iter_mut().rev() {
            layer.paint(ctx, data, env);
        }
    }
}
