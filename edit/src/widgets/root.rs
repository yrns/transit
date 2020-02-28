use crate::widgets::{DragData, State, DRAG_END, DRAG_START};
use crate::{EditData, GrabData};
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
}

impl Widget<EditData> for Root {
    fn event(&mut self, ctx: &mut EventCtx, event: &Event, data: &mut EditData, env: &Env) {
        match event {
            Event::Command(cmd) if cmd.selector == STATE_ADDED => {
                let (id, sid) = cmd.get_object::<(WidgetId, Idx)>().unwrap().clone();
                data.graph.wids.insert(id, sid);
            }
            // transform drag end event for children, like WidgetPod does
            // for mouse events
            Event::Command(cmd) if cmd.selector == DRAG_END => {
                let drag = cmd.get_object::<DragData>().unwrap().clone();

                // reverse so states on top handle the event first
                for state in &mut self.states.iter_mut().rev() {
                    let mouse_pos = drag.rect1.origin() + drag.p0.to_vec2();
                    let rect = state.layout_rect();
                    if rect.winding(mouse_pos) != 0 {
                        let mut drag = drag.clone();
                        let origin = drag.rect1.origin() - rect.origin().to_vec2();
                        drag.rect1 = drag.rect1.with_origin(origin);
                        let event = Event::Command(Command::new(DRAG_END, drag));
                        state.event(ctx, &event, data, env);
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
                if self.sid.is_none() {
                    ctx.request_focus();
                }
            }
            Event::Command(cmd) if cmd.selector == DRAG_END => {
                let drag = cmd.get_object::<DragData>().unwrap().clone();
                let sid = data.graph.wids[&drag.id];
                let graph = Arc::make_mut(&mut data.graph.graph);
                let state = graph.get_mut(sid);
                state.edit_data.set_rect(drag.rect1);
                ctx.set_handled();
                ctx.request_layout();
            }

            Event::MouseDown(mouse) => {
                // start drag, start at the end of the list since
                // those states draw last
                for state in self.states.iter().rev() {
                    let rect = state.layout_rect();
                    if rect.contains(mouse.pos) {
                        // let drag = DragData {
                        //     p0: mouse.pos,
                        //     rect0: rect,
                        //     rect1: rect,
                        //     id: state.id(),
                        //     resize: false,
                        // };
                        // // always target the drag space? pass its id around?
                        // ctx.submit_command(Command::new(DRAG_START, drag), None);
                        // ctx.set_handled();
                        break;
                    }
                }

                ctx.request_focus();
            }
            Event::MouseUp(_) => {
                // if ctx.is_active() {
                //     ctx.set_active(false);
                //     ctx.request_paint();
                //     if ctx.is_hot() {
                //         //(self.action)(ctx, data, env);
                //     }
                // }
            }
            Event::MouseMoved(_mouse) => {
                // if we are the parent of the widget, mousebutton
            }
            Event::KeyDown(key_event) => {
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
                        //ctx.submit_command(NEW_STATE, ctx.widget_id());
                        // TODO: unique or use short guid?
                        let g = Arc::make_mut(&mut data.graph.graph);
                        if g.add_state("untitled").is_err() {
                            for n in 1..10 {
                                let id = format!("untitled-{}", n);
                                if g.add_state(id.as_str()).is_ok() {
                                    break;
                                }
                            }
                        }
                    }

                    _ => {
                        dbg!("unhandled key in root: {:?}", key_event);
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
            LifeCycle::FocusChanged(_) => {}
            _ => (),
        }

        for state in &mut self.states {
            state.lifecycle(ctx, event, data, env);
        }
    }

    fn update(&mut self, ctx: &mut UpdateCtx, _old_data: &EditData, data: &EditData, env: &Env) {
        // sync Vec<State> with this state's children
        // if the state stored the child list we might be able to lens over just the state
        let mut changed = false;
        let mut max = 0;
        let children = data.graph.graph.children(self.sid).enumerate();
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
            max = index;
        }

        let len = self.states.len();
        if len > 0 && max < len - 1 {
            // the last state got re/moved
            self.states.truncate(max + 1);
        }

        if changed {
            ctx.children_changed();
            // why? seems like the above should repaint, the list widget does not do this
            ctx.request_paint();
        }

        //dbg!(old_data, data);
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
            let rect = data.graph.graph.graph[state.widget().sid].edit_data.rect;
            state.set_layout_rect(rect);
            let child_bc = BoxConstraints::tight(rect.size());
            // we were using this to make the state smaller, but we're
            // not anymore
            let _child_size = state.layout(layout_ctx, &child_bc, data, env);
            //min_rect = min_rect.union(rect);
        }

        // is this even used? no we always return the max
        //bc.constrain(min_rect.size())

        // return zero if we are not the root state and have no states
        if self.sid.is_some() && self.states.is_empty() {
            Size::ZERO
        } else {
            bc.max()
        }
    }

    fn paint(&mut self, paint_ctx: &mut PaintCtx, data: &EditData, env: &Env) {
        // we want to draw something on hover to show drag target TODO:
        for state in &mut self.states {
            // if let Some(GrabData { p0, p1, rect, id }) = data.graph.grab {
            //     if state.id() == id {}
            // }
            state.paint_with_offset(paint_ctx, data, env)
        }
    }
}
