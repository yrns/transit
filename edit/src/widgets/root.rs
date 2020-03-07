use crate::widgets::{DragData, State, DRAG_END, DRAG_START};
use crate::EditData;
use druid::kurbo::RoundedRect;
use druid::kurbo::{BezPath, Shape};
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
use std::sync::Arc;
use transit::{Graph, Idx, State as TransitState, Transition};

pub const STATE_ADDED: Selector = Selector::new("transit.edit.state-added");
// TODO:
pub const STATE_REMOVED: Selector = Selector::new("transit.edit.state-removed");

pub struct Root {
    sid: Option<Idx>,
    states: Vec<WidgetPod<EditData, State>>,
}

impl Root {
    pub fn new(sid: Option<Idx>) -> Self {
        Self {
            sid,
            states: Vec::new(),
        }
    }

    pub fn is_root(&self) -> bool {
        self.sid.is_none()
    }
}

impl Widget<EditData> for Root {
    fn event(&mut self, ctx: &mut EventCtx, event: &Event, data: &mut EditData, env: &Env) {
        // recurse first, with special cases:
        match event {
            Event::Command(cmd) if cmd.selector == STATE_ADDED => {
                let (id, sid) = cmd.get_object::<(WidgetId, Idx)>().unwrap().clone();
                data.graph1.wids.insert(id, sid);
                return;
            }
            // transform drag end event for children, like WidgetPod
            // does for mouse events
            Event::Command(cmd) if cmd.selector == DRAG_END => {
                let drag = cmd.get_object::<DragData>().unwrap();

                // reverse so states on top handle the event first
                for state in &mut self.states.iter_mut().rev() {
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
                for state in &mut self.states.iter_mut().rev() {
                    state.event(ctx, event, data, env);
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
                let drag = cmd.get_object::<DragData>().unwrap();
                let sid = data.graph1.wids[&drag.id];
                if let Err(err) = data.graph1.move_state(self.sid, sid, drag.rect1) {
                    log::error!("error on drag: {}", err);
                }
                ctx.set_handled();
                ctx.request_layout();
            }

            Event::MouseDown(_mouse) => {
                // we only get this in the root widget because drag intercepts all other mouse downs
                log::info!("request_focus for {:?}", ctx.widget_id());
                ctx.request_focus();
            }
            Event::KeyDown(key_event) => {
                if ctx.has_focus() {
                    match key_event {
                        // Select all states
                        k_e if (HotKey::new(SysMods::Cmd, "a")).matches(k_e) => {
                            // TODO:
                        }
                        // Backspace focuses parent?
                        k_e if (HotKey::new(None, KeyCode::Backspace)).matches(k_e) => {
                            // TODO:
                        }
                        // Delete this state
                        k_e if (HotKey::new(None, KeyCode::Delete)).matches(k_e) => {
                            // TODO:
                        }
                        // Tab and shift+tab change focus to child states
                        k_e if HotKey::new(None, KeyCode::Tab).matches(k_e) => ctx.focus_next(),
                        k_e if HotKey::new(RawMods::Shift, KeyCode::Tab).matches(k_e) => {
                            ctx.focus_prev()
                        }
                        k_e if HotKey::new(None, "n").matches(k_e) => {
                            if let Err(err) = data.graph1.add_state("untitled", self.sid) {
                                log::error!("error on adding state: {}", err);
                            }
                            ctx.set_handled();
                        }

                        _ => {
                            //dbg!("unhandled key in root: {:?}", key_event);
                        }
                    }
                }
            }
            _ => (),
        }
    }

    fn lifecycle(&mut self, ctx: &mut LifeCycleCtx, event: &LifeCycle, data: &EditData, env: &Env) {
        match event {
            LifeCycle::WidgetAdded => {
                ctx.register_for_focus();

                // the list widget syncs data and children here too,
                // but isn't update called initially? TODO:
            }
            LifeCycle::HotChanged(_) => {
                ctx.request_paint();
            }
            LifeCycle::FocusChanged(focus) => {
                log::info!("focus for {:?}: {}", ctx.widget_id(), focus);
            }
            LifeCycle::RouteFocusChanged { old, new } => {
                log::info!("old: {:?}, new: {:?}", old, new);
            }
            _ => (),
        }

        for state in &mut self.states {
            state.lifecycle(ctx, event, data, env);
        }
    }

    // TODO: figure out z, put selected state on top
    fn update(&mut self, ctx: &mut UpdateCtx, _old_data: &EditData, data: &EditData, env: &Env) {
        // sync Vec<State> with this state's children
        // if the state stored the child list we might be able to lens over just the state
        let mut changed = false;
        let mut n = 0;
        let children = data.graph1.graph.children(self.sid).enumerate();
        for (index, sid) in children {
            if index + 1 > self.states.len() {
                // new state
                let state = WidgetPod::new(State::new(sid));
                self.states.push(state);
                changed = true;
            } else {
                match sid.cmp(&self.states[index].widget().sid) {
                    Ordering::Equal => (),
                    Ordering::Greater => {
                        // state got re/moved
                        self.states.remove(index);
                    }
                    Ordering::Less => {
                        // stated added
                        self.states.insert(index, WidgetPod::new(State::new(sid)));
                    }
                }
            }
            n = index + 1;
        }

        // remove excess states (if the last state was removed)
        self.states.truncate(n);

        if changed {
            ctx.children_changed();
            // why? seems like the above should repaint, the list widget does not do this
            ctx.request_paint();
        }

        // FIX: put this in the loop above? how do we tell if an update is needed?
        for state in &mut self.states {
            state.update(ctx, data, env);
        }
    }

    // we're not doing any layout really, since all the states are
    // user placed and sized, and they can overlap - we're not doing
    // any adjustments, only assigning the default rect for new states
    fn layout(
        &mut self,
        layout_ctx: &mut LayoutCtx,
        bc: &BoxConstraints,
        data: &EditData,
        env: &Env,
    ) -> Size {
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
            let rect = data.graph1.graph.graph[state.widget().sid].edit_data.rect;
            state.set_layout_rect(rect);
            let child_bc = BoxConstraints::tight(rect.size());
            // we were using this to make the state smaller, but we're
            // not anymore
            let _child_size = state.layout(layout_ctx, &child_bc, data, env);
            //min_rect = min_rect.union(rect);
        }

        // is this even used? no we always return the max
        //bc.constrain(min_rect.size())

        // we always want the root maxed so we can drag to it
        bc.max()
    }

    // TODO: don't paint outside the root and/or crop
    // TODO: we need to fit states inside the root on resize
    fn paint(&mut self, ctx: &mut PaintCtx, data: &EditData, env: &Env) {
        // TODO: we have to paint the focus here since has_focus is
        // false for ancestors (in paint) - add a flag in state that
        // tracks that the root has focus, or directly access it
        // outside the flex
        let has_focus = ctx.has_focus();
        let is_hot = ctx.is_hot();

        // we want to draw something on hover to show drag target TODO:

        // the size is infinite for the root since it's in a scroll widget
        if !self.is_root() && (has_focus || is_hot) {
            let rect = Rect::from_origin_size(Point::ORIGIN, ctx.size()).inset(-1.);
            let rounded_rect = RoundedRect::from_rect(rect, 4.);

            let color = if has_focus {
                env.get(theme::PRIMARY_LIGHT)
            } else {
                env.get(theme::BORDER_LIGHT)
            };
            let size = 2.0;

            ctx.stroke(rounded_rect, &color, size);
        }

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

        if clip {
            if let Err(e) = ctx.restore() {
                log::error!("restoring render context failed: {:?}", e);
            }
        }
    }
}
