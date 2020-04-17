use crate::widgets::{Drag, DragData, DragType, Initial, State, DRAG_END, DRAG_START};
use crate::{graph_initial_lens, handle_key, state_initial_lens, EditData, RESET};
use druid::kurbo::RoundedRect;
use druid::kurbo::{BezPath, Line, Shape, Vec2};
use druid::piet::{FontBuilder, ImageFormat, InterpolationMode, Text, TextLayoutBuilder};
use druid::theme;
use druid::widget::{Align, Flex, Label, LabelText, Padding, SizedBox, WidgetExt};
use druid::{
    Affine, AppLauncher, BoxConstraints, Color, Command, Data, Env, Event, EventCtx, HotKey,
    KeyCode, LayoutCtx, LifeCycle, LifeCycleCtx, LinearGradient, LocalizedString, PaintCtx, Point,
    RawMods, Rect, RenderContext, Selector, Size, SysMods, UnitPoint, UpdateCtx, Widget, WidgetId,
    WidgetPod, WindowDesc,
};
use std::cmp::Ordering;
use std::f64::consts::PI;
use std::sync::Arc;
use transit::{Graph, Idx, State as TransitState, Transition};

pub const STATE_ADDED: Selector = Selector::new("transit.edit.state-added");
// TODO:
pub const STATE_REMOVED: Selector = Selector::new("transit.edit.state-removed");

pub struct Root {
    sid: Option<Idx>,
    // make this an option? we don't need it at all unless there are
    // child states TODO:
    initial: WidgetPod<EditData, Box<dyn Widget<EditData>>>,
    states: Vec<WidgetPod<EditData, Drag<EditData, State>>>,
}

impl Root {
    pub fn new(sid: Option<Idx>) -> Self {
        let initial: Box<dyn Widget<EditData>> = match sid {
            Some(i) => Box::new(Initial::new().lens(state_initial_lens(i))),
            None => Box::new(Initial::new().lens(graph_initial_lens())),
        };

        Self {
            sid,
            initial: WidgetPod::new(Drag::new(initial, DragType::MoveInitial(sid), None)).boxed(),
            states: Vec::new(),
        }
    }

    pub fn is_root(&self) -> bool {
        self.sid.is_none()
    }
}

fn to_rect(rect: ((f64, f64), (f64, f64))) -> Rect {
    Rect::from_origin_size(rect.0, rect.1)
}

impl Widget<EditData> for Root {
    fn event(&mut self, ctx: &mut EventCtx, event: &Event, data: &mut EditData, env: &Env) {
        let mut has_active = false;

        // recurse first, with special cases:
        match event {
            Event::Command(cmd) if cmd.selector == RESET => {
                //data.reset();
                self.states.truncate(0);
                ctx.children_changed();
                // this does nothing FIX:?
                //ctx.request_paint();
                return;
            }
            Event::Command(cmd) if cmd.selector == STATE_ADDED => {
                let (id, sid) = cmd.get_object::<(WidgetId, Idx)>().unwrap().clone();
                log::info!("state_added {:?} {:?}", id, sid);
                data.graph1.wids.insert(id, sid);
                return;
            }
            // transform drag end event for children, like WidgetPod
            // does for mouse events
            Event::Command(cmd) if cmd.selector == DRAG_END => {
                let drag = cmd.get_object::<DragData>().unwrap();

                // reverse so states on top handle the event first
                for state in self.states.iter_mut().rev() {
                    // can't move a state inside itself
                    if state.id() != drag.id {
                        let rect = state.layout_rect();
                        if rect.winding(drag.anchor()) != 0 {
                            let event = drag.offset(rect.origin().to_vec2()).to_event();
                            state.event(ctx, &event, data, env);
                        }
                    }
                }
            }
            _ => {
                if self.states.len() > 0 {
                    self.initial.event(ctx, event, data, env);
                    has_active |= self.initial.has_active();
                }

                for state in &mut self.states.iter_mut().rev() {
                    state.event(ctx, event, data, env);
                    has_active |= state.has_active();
                }
            }
        }

        if ctx.is_handled() {
            return;
        }

        match event {
            Event::WindowConnected => {
                // the root widget initially has focus, this may need
                // to change if we have multiple graphs open
                if self.is_root() {
                    ctx.request_focus();
                }
            }
            Event::Command(cmd) if cmd.selector == DRAG_END => {
                // we are inside the deepest root widget that will
                // accept this drag
                let drag = cmd.get_object::<DragData>().unwrap();
                match drag.ty {
                    DragType::MoveState(idx) => {
                        if let Err(err) = data.graph1.move_state(self.sid, idx, drag.rect1) {
                            log::error!("error on drag: {}", err);
                        }
                    }
                    DragType::ResizeState(idx) => {
                        // just set rect for resizes, use same parent
                        let p = data.graph1.graph[idx].parent;
                        if let Err(err) = data.graph1.move_state(p, idx, drag.rect1) {
                            log::error!("error on drag: {}", err);
                        }
                    }
                    DragType::MoveInitial(idx) => {
                        // if the drag id is our initial widget id, we are moving the widget
                        if self.initial.id() == drag.id {
                            data.graph1.move_initial(idx, drag.rect1.origin());
                        } else {
                            // otherwise we are setting the initial index
                            match self.sid {
                                Some(b) => {
                                    if let Err(err) = data.graph1.set_initial(idx, b) {
                                        log::error!("error setting initial: {:?}", err);
                                    }
                                }
                                None => log::error!("can't set initial to root"),
                            }
                        }
                    }
                    _ => log::error!("invalid drag type: {:?}", drag),
                }
                ctx.set_handled();
                ctx.request_layout();
            }

            Event::MouseDown(_mouse) => {
                // we have no visual indicator for the root having focus FIX?
                if self.is_root() && !has_active && !ctx.is_focused() {
                    ctx.request_focus();
                }
            }
            Event::KeyDown(e) => {
                // state handles other keys
                if self.is_root() && ctx.is_focused() {
                    handle_key(ctx, e, data, self.sid);
                }
            }
            _ => (),
        }
    }

    fn lifecycle(&mut self, ctx: &mut LifeCycleCtx, event: &LifeCycle, data: &EditData, env: &Env) {
        match event {
            // since we aren't a WidgetPod (except the root) we have
            // to accept the route event
            LifeCycle::WidgetAdded => {
                //| LifeCycle::RouteWidgetAdded => {
                if self.is_root() {
                    ctx.register_for_focus();
                }

                // the list widget syncs data and children here too,
                // but isn't update called initially? TODO:
            }
            LifeCycle::HotChanged(_) => {
                ctx.request_paint();
            }
            // LifeCycle::FocusChanged(focus) => {
            //     log::info!("focus for {:?}: {}", ctx.widget_id(), focus);
            //     if *focus && !self.is_root() {
            //         log::warn!("non-root root has focus!");
            //     }
            // }
            // LifeCycle::RouteFocusChanged { old, new } => {
            //     // only the root root gets focus
            //     log::info!(
            //         "route focus me: {:?} old: {:?}, new: {:?}",
            //         ctx.widget_id(),
            //         old,
            //         new
            //     );
            //     self.is_focused = &Some(ctx.widget_id()) == new;
            // }
            _ => (),
        }

        if self.states.len() > 0 {
            self.initial.lifecycle(ctx, event, data, env);
        }

        for state in &mut self.states {
            state.lifecycle(ctx, event, data, env);
        }
    }

    // TODO: figure out z, put selected state on top
    fn update(&mut self, ctx: &mut UpdateCtx, _old_data: &EditData, data: &EditData, env: &Env) {
        if self.states.len() > 0 {
            self.initial.update(ctx, data, env);
        }

        // sync Vec<State> with this state's children
        // if the state stored the child list we might be able to lens over just the state
        let mut changed = false;
        let mut n = 0;
        let children = data.graph1.graph.children(self.sid).enumerate();

        let new_state = |sid| {
            WidgetPod::new(Drag::new(
                State::new(sid),
                DragType::MoveState(sid),
                Some(DragType::ResizeState(sid)),
            ))
        };

        for (index, sid) in children {
            if index + 1 > self.states.len() {
                // new state
                let state = new_state(sid);
                self.states.push(state);
                changed = true;
            } else {
                // this is ugly, accessing child struct data inside
                // layers of widgets
                match sid.cmp(&self.states[index].widget().inner().sid) {
                    Ordering::Equal => {
                        // put the update recursion here so we are not
                        // calling update on new or removed states
                        &self.states[index].update(ctx, data, env);
                    }
                    Ordering::Greater => {
                        // state got re/moved
                        self.states.remove(index);
                    }
                    Ordering::Less => {
                        // stated added
                        self.states.insert(index, new_state(sid));
                        changed = true;
                    }
                }
            }
            n = index + 1;
        }

        // TODO: we want to actually move widgets with deep
        // hierarchies rather than recreate them?

        // remove excess states (if the last state was removed)
        self.states.truncate(n);

        if changed {
            ctx.children_changed();
            // why? seems like the above should repaint, the list widget does not do this
            ctx.request_paint();
        }
    }

    // we're not doing any layout really, since all the states are
    // user placed and sized, and they can overlap - we're not doing
    // any adjustments, only assigning the default rect for new states
    fn layout(
        &mut self,
        ctx: &mut LayoutCtx,
        bc: &BoxConstraints,
        data: &EditData,
        env: &Env,
    ) -> Size {
        let g = &data.graph1.graph;

        if self.states.len() > 0 {
            let initial_size = self.initial.layout(ctx, bc, data, env);
            let p = match self.sid {
                Some(idx) => g[idx].edit_data.initial,
                None => g.edit_data.initial,
            };
            self.initial
                .set_layout_rect(Rect::from_origin_size(p, initial_size));
        }

        //let mut min_rect = Rect::ZERO;

        // we can't modify the rects here, so do all the layout elsewhere?
        // all this does is copy the rect from the edit data
        //for (_i, state) in self.states.iter_mut().enumerate() {
        for state in self.states.iter_mut() {
            // if this is a new state widget its rect will be zero?
            // let mut rect = state.layout_rect();
            // if rect.width() == 0.0 {
            //     // place under the last added state instead?
            //     let p = Point::new(20. * (i + 1) as f64, 20. * (i + 1) as f64);
            //     rect = Rect::from_origin_size(p, default_size);
            //     state.set_layout_rect(rect);
            // }
            // tuple -> Rect
            let rect = to_rect(g[state.widget().inner().sid].edit_data.rect);
            state.set_layout_rect(rect);
            let child_bc = BoxConstraints::tight(rect.size());
            // we were using this to make the state smaller, but we're
            // not anymore
            let _child_size = state.layout(ctx, &child_bc, data, env);
            //min_rect = min_rect.union(rect);
        }

        // is this even used? no we always return the max
        //bc.constrain(min_rect.size())

        // TODO: make root area dynamically sized to fit all states
        if self.is_root() {
            // for the root we want a big area to play in, but not infinite
            let max = 2.0_f64.powi(12);
            Size::new(max, max)
        } else {
            // we want the root maxed so we can drag to it
            bc.max()
        }
    }

    // TODO: we need to fit states inside the root on resize
    fn paint(&mut self, ctx: &mut PaintCtx, data: &EditData, env: &Env) {
        // don't clip if we are the root widget (since our size is
        // infinite) or if we are dragging (which draws outside the
        // paint region)
        let clip = !(self.is_root() || self.states.iter().any(|s| s.has_active()));

        // we are ensuring that the origin of each child state is
        // inside our paint region, but not the extents, so we need to
        // set a clip region
        if clip {
            if let Err(e) = ctx.save() {
                log::error!("saving render context failed: {:?}", e);
                return;
            }
            let clip_rect = Rect::from_origin_size(Point::ORIGIN, ctx.size());
            ctx.clip(clip_rect);
        }

        for state in &mut self.states {
            state.paint_with_offset(ctx, data, env)
        }

        // don't paint if we have no children
        if self.states.len() > 0 {
            // we have to draw the initial arrow here since only we know
            // the position of the widget; draw after states so we can
            // connect to children of children
            let color = env.get(&theme::LABEL_COLOR);
            let g = &data.graph1.graph;
            if let Some(initial) = g.initial_idx(self.sid) {
                // TODO: make this a quadratic curve
                let a = self.initial.layout_rect().center();
                let b = g.rel_pos(self.sid, initial);
                ctx.stroke(Line::new(a, b), &color, 1.5);
                let b: Vec2 = b.into();
                let affine = Affine::translate(b);
                let up = Vec2::new(0., -1.);
                let mut th = (b - a.to_vec2()).normalize().dot(up).acos();
                if b.x < a.x {
                    th = -th;
                }
                let affine = affine * Affine::rotate(th);
                let mut arrow = arrow(8.0, 12.0);
                arrow.apply_affine(affine);
                ctx.fill(arrow, &color);
            }
            self.initial.paint_with_offset(ctx, data, env);
        }

        if clip {
            if let Err(e) = ctx.restore() {
                log::error!("restoring render context failed: {:?}", e);
            }
        }
    }
}

pub fn arrow(width: f64, height: f64) -> BezPath {
    let mut a = BezPath::new();
    a.move_to(Point::new(0., 0.));
    a.line_to(Point::new(width / 2., height));
    a.line_to(Point::new(-width / 2., height));
    a.close_path();
    a
}
