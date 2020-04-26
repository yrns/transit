use crate::{sync::SyncMap, widgets::*, *};
use druid::{kurbo::*, piet::*, theme, widget::*, *};
use flo_curves::line::{line_clip_to_bounds, Coord2, Line as _};
use std::cmp::Ordering;
use std::f64::consts::PI;
use std::sync::Arc;
use transit::{Graph, Idx};

pub const STATE_ADDED: Selector = Selector::new("transit.edit.state-added");
// TODO:
pub const STATE_REMOVED: Selector = Selector::new("transit.edit.state-removed");
pub const ROOT_FOCUS: Selector = Selector::new("transit.edit.root-focus");

type Child = WidgetPod<EditData, Box<dyn Widget<EditData>>>;

pub struct Root {
    sid: Option<Idx>,
    // make this an option? we don't need it at all unless there are
    // child states TODO:
    initial: WidgetPod<EditData, Box<dyn Widget<EditData>>>,
    states: SyncMap<Idx, Child>,
    transitions: SyncMap<TransIdx, Child>,
}

impl Root {
    pub fn new(sid: Option<Idx>) -> Self {
        let initial: Box<dyn Widget<EditData>> = match sid {
            Some(i) => Box::new(Initial::new().lens(state_initial_lens(i))),
            None => Box::new(Initial::new().lens(graph_initial_lens())),
        };

        let new_state = |sid| {
            let drag: DragTypeSelector = Box::new(move |mouse: &MouseEvent| match mouse.button {
                MouseButton::Right => DragType::CreateTransition(sid),
                _ => DragType::MoveState(sid),
            });

            WidgetPod::new(Drag::new(
                State::new(sid),
                drag,
                Some(DragType::ResizeState(sid).into()),
            ))
            .boxed()
        };

        let new_transition =
            |i| WidgetPod::new(Transition::new(i).lens(transition_lens(i))).boxed();

        Self {
            sid,
            initial: WidgetPod::new(Drag::new(initial, DragType::MoveInitial(sid), None)).boxed(),
            states: SyncMap::new(new_state),
            transitions: SyncMap::new(new_transition),
        }
    }

    pub fn is_root(&self) -> bool {
        self.sid.is_none()
    }

    pub fn children(&self) -> impl DoubleEndedIterator<Item = &Child> {
        self.states.values().chain(self.transitions.values())
    }

    pub fn children_mut(&mut self) -> impl DoubleEndedIterator<Item = &mut Child> {
        self.states
            .values_mut()
            .chain(self.transitions.values_mut())
    }

    /// Sync child widgets with data. Returns true if there are
    /// changes.
    pub fn sync(&mut self, data: &EditData) -> bool {
        // provide children via lens?
        let children = data.graph1.graph.children(self.sid);
        self.states.sync(children)
            | (self.is_root() && self.transitions.sync(data.graph1.graph.transitions()))
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
                self.states.clear();
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
                for state in self.states.values_mut().rev() {
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
            // the unique path for a transition includes the index
            // since there's no other unique identifier - maybe just
            // do the same for states rather than have unique
            // (editable) ids?
            Event::Command(cmd) if cmd.selector == SET_ACTION => {
                let (a, go) = cmd.get_object::<(ActionType, bool)>().unwrap();
                // TODO: somehow make this configurable, just use the path for now:
                match a {
                    ActionType::Guard(i) => {
                        let g = &data.graph1.graph;
                        if let Some((s, _)) = g.endpoints(*i) {
                            let path =
                                format!("{}_{}_{}_guard", g.path_str(s), g[*i].event, i.index());
                            data.graph1
                                .with_undo(|g| g[*i].guard = Some(path.clone()), "set guard");
                            if *go {
                                // TODO:
                                log::info!("GO EMACS!!!");
                            }
                        }
                    }
                    _ => (),
                }
                return;
            }
            _ => {
                if self.states.len() > 0 {
                    self.initial.event(ctx, event, data, env);
                    has_active |= self.initial.has_active();
                }

                has_active = self.children_mut().rev().fold(has_active, |a, w| {
                    w.event(ctx, event, data, env);
                    a | w.has_active()
                });
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
            Event::Command(cmd) if cmd.selector == ROOT_FOCUS => {
                ctx.request_focus();
                return;
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
                    DragType::CreateTransition(from) => {
                        if let Some(to) = self.sid {
                            // place the transition widget at the midpoint between the drag start and end
                            let offset = drag.p0.to_vec2();
                            let p0 = Point::from(data.graph1.graph.abs_pos(from)) + offset;
                            let p1 = Point::from(data.graph1.graph.abs_pos(to))
                                + drag.rect1.origin().to_vec2()
                                + offset;
                            let m = p0.midpoint(p1);
                            data.graph1.add_transition(from, to, m);
                        } else {
                            log::warn!("can't transition to root")
                        }
                    }
                    DragType::MoveTransition(idx) => {
                        data.graph1.with_undo(
                            |g| g[idx].edit_data.0 = drag.rect1.origin().into(),
                            "move transition",
                        );
                    }
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

                if self.sync(data) {
                    ctx.children_changed();
                }
            }
            LifeCycle::HotChanged(_) => {
                ctx.request_paint();
            }
            LifeCycle::FocusChanged(focus) => {
                if !focus && self.is_root() {
                    // make sure the root widget always has focus -
                    // this will break with multiple graphs/windows
                    // FIX:?
                    ctx.submit_command(ROOT_FOCUS, None);
                }
            }
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

        self.children_mut()
            .for_each(|w| w.lifecycle(ctx, event, data, env));
    }

    // TODO: in order to raise states and work with undo we need to
    // store the z order in the data, raise a state when it's focused
    // or moved, and resort the widgets by z in update
    fn update(&mut self, ctx: &mut UpdateCtx, _old_data: &EditData, data: &EditData, env: &Env) {
        // this will update widgets that are about to be removed FIX:?
        self.states
            .values_mut()
            .chain(self.transitions.values_mut())
            .for_each(|w| w.update(ctx, data, env));

        // TODO: we want to actually move widgets with deep
        // hierarchies rather than recreate them when moved?
        if self.sync(data) {
            ctx.children_changed();
            // why? seems like the above should repaint, the list widget does not do this
            ctx.request_paint();
        }

        if self.states.len() > 0 {
            self.initial.update(ctx, data, env);
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
        for (idx, state) in self.states.iter_mut() {
            // if this is a new state widget its rect will be zero?
            // let mut rect = state.layout_rect();
            // if rect.width() == 0.0 {
            //     // place under the last added state instead?
            //     let p = Point::new(20. * (i + 1) as f64, 20. * (i + 1) as f64);
            //     rect = Rect::from_origin_size(p, default_size);
            //     state.set_layout_rect(rect);
            // }
            // tuple -> Rect - can't index by ref?
            let rect = to_rect(g[*idx].edit_data.rect);
            state.set_layout_rect(rect);
            let child_bc = BoxConstraints::tight(rect.size());
            // we were using this to make the state smaller, but we're
            // not anymore
            let _child_size = state.layout(ctx, &child_bc, data, env);
            //min_rect = min_rect.union(rect);
        }

        self.transitions.iter_mut().for_each(|(i, t)| {
            let size = t.layout(ctx, &bc, data, env);
            let p: Point = g[*i].edit_data.0.into();
            t.set_layout_rect(Rect::from_origin_size(p, size));
        });

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
        let clip = !(self.is_root() || self.states.values().any(|s| s.has_active()));

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

        self.states
            .values_mut()
            .for_each(|w| w.paint_with_offset(ctx, data, env));

        // drag transition lines here, the widget only draws the label
        let g = &data.graph1.graph;
        self.transitions.iter().for_each(|(i, w)| {
            if let Some((from, to)) = g.endpoints(*i) {
                let r0 = to_rect(g.abs_rect(from));
                let r1 = to_rect(g.abs_rect(to));
                let p0 = r0.center();
                let p1 = r1.center();

                // take a line from center to center, then clip that
                // line with both rects to find terminals
                let line = (Coord2(p0.x, p0.y), Coord2(p1.x, p1.y));
                let t0 = line_clip_to_bounds(&line, &(Coord2(r0.x0, r0.y0), Coord2(r0.x1, r0.y1)))
                    .unwrap()
                    .1;
                let t1 = line_clip_to_bounds(&line, &(Coord2(r1.x0, r1.y0), Coord2(r1.x1, r1.y1)))
                    .unwrap()
                    .0;

                let color = env.get(&theme::LABEL_COLOR);
                let center = w.layout_rect().center();
                ctx.stroke(
                    fit_quadbez(
                        Vec2::new(t0.0, t0.1),
                        center.to_vec2(),
                        Vec2::new(t1.0, t1.1),
                    ),
                    &color,
                    1.0,
                );
            }
        });

        // draw transition widgets over the connection curves
        self.transitions
            .values_mut()
            .for_each(|w| w.paint_with_offset(ctx, data, env));

        // don't paint initial if we have no children
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
                ctx.stroke(Line::new(a, b), &color, 1.0);
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

// Given 3 points, find a quad bezier that passes through the middle point at time 0.5.
pub fn fit_quadbez(p0: Vec2, p1: Vec2, p2: Vec2) -> BezPath {
    let cp = (p1 - 0.25 * (p0 + p2)) / 0.5;
    // there's no easy way to QuadBez -> BezPath?
    let mut a = BezPath::new();
    a.move_to(p0.to_point());
    a.quad_to(cp.to_point(), p2.to_point());
    a
}
