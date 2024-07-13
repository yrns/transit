use crate::edit_egui::*;
use crate::graph::Direction;
use crate::IntSet;

use super::*;

/// Current state, transition, or initial drag.
#[derive(Clone, Debug, Default)]
pub enum Drag {
    #[default]
    None,
    State {
        source: Idx,
        /// Offset from the pointer to rect.min.
        offset: Vec2,
        target: Option<Idx>,
        // Original depth.
        depth: usize,
        /// We can't access the pointer `press_origin` after the drag is stopped, so we store it here.
        press_origin: Pos2,
        /// Set of descendants.
        desc: IntSet<usize>,
    },
    Resize(Idx, Vec2),
    // Only Initial has a port?
    Initial(Idx, Option<(Idx, usize)>, (Vec2, Vec2)),
    InitialControl((Idx, ControlPoint), Vec2),
    AddTransition(Idx, Option<Idx>),
    /// The first field is the opposite endpoint!
    TransitionSource(Idx, Option<Idx>, Tdx),
    /// The first field is the opposite endpoint!
    TransitionTarget(Idx, Option<Idx>, Tdx),
    TransitionControl((Tdx, ControlPoint), Vec2),
    TransitionId(Tdx, Vec2),
}

impl Drag {
    // pub(crate) fn sense(&self) -> Sense {
    //     match self {
    //         Drag::None => Sense::click_and_drag(),
    //         _ => Sense::hover(),
    //     }
    // }

    pub(crate) fn in_drag(&self) -> bool {
        !matches!(self, Drag::None)
    }

    /// Returns the index of the dragged state (if any).
    pub(crate) fn dragged_idx(&self) -> Option<Idx> {
        match self {
            Drag::State { source, .. } => Some(*source),
            _ => None,
        }
    }

    pub(crate) fn is_dragging(&self, idx: Idx) -> bool {
        self.dragged_idx() == Some(idx)
    }

    /// Get the index of the currently dragged transition.
    // Should we change the name? This only refers to transitions where we're dragging the
    // endpoints.
    pub(crate) fn dragged_tdx(&self) -> Option<Tdx> {
        match self {
            Drag::TransitionSource(_, _, tdx) | Drag::TransitionTarget(_, _, tdx) => Some(*tdx),
            // | Drag::TransitionId(tdx, _)
            _ => None,
        }
    }

    pub(crate) fn resizing(&self, idx: Idx) -> bool {
        matches!(self, Drag::Resize(i, ..) if *i == idx)
    }

    pub(crate) fn target(&self) -> Option<Idx> {
        match self {
            Drag::State {
                target: Some(t), ..
            }
            | Drag::Initial(_, Some((t, ..)), ..)
            | Drag::AddTransition(_, Some(t), ..)
            | Drag::TransitionSource(_, Some(t), ..)
            | Drag::TransitionTarget(_, Some(t), ..) => Some(*t),
            _ => None,
        }
    }

    pub(crate) fn is_target(&self, idx: Idx) -> bool {
        self.target() == Some(idx)
    }
}

// Deriving the variant and id from init failed...
// macro_rules! drag_id {
//     ($variant:ident($($field:tt)*,)) => {
//         $variant
//     };
// }

impl From<Idx> for Drag {
    fn from(idx: Idx) -> Self {
        Drag::Resize(idx, Vec2::ZERO)
    }
}

impl From<Tdx> for Drag {
    fn from(tdx: Tdx) -> Self {
        Drag::TransitionId(tdx, Vec2::ZERO)
    }
}

impl From<(Idx, ControlPoint)> for Drag {
    fn from(cp: (Idx, ControlPoint)) -> Self {
        Drag::InitialControl(cp, Vec2::ZERO)
    }
}

impl From<(Tdx, ControlPoint)> for Drag {
    fn from(cp: (Tdx, ControlPoint)) -> Self {
        Drag::TransitionControl(cp, Vec2::ZERO)
    }
}

impl<S> Edit<S>
where
    S: Source,
{
    pub(crate) fn resolve_drag(
        &self,
        edit_data: &mut EditData,
        // TEMP
        drag_transition: Option<Transition>,
        p: Pos2,
    ) {
        let commands = &mut edit_data.commands;

        match std::mem::take(&mut edit_data.drag) {
            Drag::AddTransition(a, Some(b)) => match drag_transition {
                Some(mut t) => {
                    // The position is in screen-space, adjust to root-space.
                    t.pos -= edit_data.root_rect.min.to_vec2();
                    commands.push(Command::AddTransition(a, b, t));
                }
                None => {
                    warn!("no drag_transition!");
                }
            },
            Drag::TransitionSource(b, Some(a), tdx) | Drag::TransitionTarget(a, Some(b), tdx) => {
                if let Some((a0, b0)) = self.graph.endpoints(tdx) {
                    // Check if changed. We were doing this inside drag_transition to avoid
                    // looking up endpoints twice...
                    if a != a0 || b != b0 {
                        match drag_transition {
                            Some(t) => {
                                commands.push(Command::UpdateTransition(tdx, t));

                                commands.push(Command::MoveTransition(
                                    tdx,
                                    (a != a0).then_some(a),
                                    (b != b0).then_some(b),
                                ))
                            }
                            None => {
                                warn!("no drag_transition!");
                            }
                        }
                    }
                } else {
                    warn!("no endpoints!");
                }
            }
            Drag::Initial(i, Some((target, port)), (c1, c2)) => {
                let initial = self
                    .graph
                    .initial(i)
                    .map(|(initial, _)| (initial, target))
                    .unwrap_or_else(|| (Initial::Initial, target));
                commands.push(Command::SetInitial(i, initial, (port, c1, c2)))
            }
            Drag::Resize(idx, delta) => edit_data.commands.push(Command::ResizeState(idx, delta)),
            Drag::InitialControl((idx, cp), delta) => {
                if let Some(mut state) = self.graph.state(idx).cloned() {
                    if let Some(initial) = &mut state.initial {
                        match cp {
                            ControlPoint::C1 => initial.1 += delta,
                            ControlPoint::C2 => initial.2 += delta,
                        }
                        edit_data.commands.push(Command::UpdateState(idx, state))
                    }
                }
            }
            Drag::TransitionControl((tdx, cp), delta) => {
                if let Some(mut t) = self.graph.transition(tdx).cloned() {
                    match cp {
                        ControlPoint::C1 => t.c1 += delta,
                        ControlPoint::C2 => t.c2 += delta,
                    }
                    edit_data.commands.push(Command::UpdateTransition(tdx, t))
                }
            }
            Drag::TransitionId(tdx, delta) => {
                if let Some(mut t) = self.graph.transition(tdx).cloned() {
                    t.pos += delta;
                    edit_data.commands.push(Command::UpdateTransition(tdx, t))
                }
            }
            // A state can't be dragged to itself, hence the index check. TODO: we already
            // checked this?
            Drag::State {
                source,
                target: Some(target),
                offset,
                press_origin,
                ..
            } if source != target => {
                // Since the transition id is free-floating and no longer relative to
                // the source and target, we need to move all the enclosed
                // transitions. TODO: handle inside MoveState? or make a transaction
                let drag_offset = press_origin - p;
                for (i, e) in self
                    .graph
                    .enclosed_edges(source)
                    .filter_map(|i| self.graph.transition(i).map(|e| (i, e)))
                {
                    edit_data.commands.push(Command::UpdateTransition(
                        i,
                        e.clone().translate(-drag_offset),
                    ));
                }

                // Get the target rect in screen-space.
                if let Some(target_rect) = edit_data.rects.get_rect(target) {
                    edit_data.commands.push(Command::MoveState(
                        source,
                        target,
                        // Find new position relative to target (parent) including
                        // pointer offset.
                        p - target_rect.min.to_vec2() - offset,
                    ));
                }
            }

            // No target is not invalid.
            //drag @ _ => error!("invalid drag resolution: {:?}", drag),
            _ => (),
        }
    }

    // Use the previous frame's rect data (clipped, in screen-space) to rescursively find the
    // front-most descendant (hovered). Discluding the dragged state.
    fn drag_target(
        &self,
        rects: &Rects,
        parent: Idx,
        dragged: Option<Idx>,
        p: Pos2,
    ) -> Option<Idx> {
        let target = rects
            .get_rect(parent)
            .filter(|r| r.contains(p))
            .map(|_| parent);

        target
            .and(
                self.graph
                    .children_rev(parent)
                    // If we filter the child we can't target its children, but we don't care about
                    // this for dragged states.
                    .filter_map(|(child, _state)| (Some(child) != dragged).then_some(child))
                    .find_map(|child| self.drag_target(rects, child, dragged, p)),
            )
            .or(target)
    }

    /// Sets drag target for current drag.
    pub(crate) fn set_drag_target(&self, edit_data: &mut EditData, p: Pos2) {
        // For transition endpoints only. What happened to this?
        // let dir = match &edit_data.drag {
        //     Drag::TransitionSource(..) => Direction::Outgoing,
        //     //Drag::TransitionTarget(..) => transit::Direction::Incoming,
        //     _ => Direction::Incoming,
        // };

        match &mut edit_data.drag {
            Drag::State { source, target, .. } => {
                // We can't drag into ourselves.
                *target = self.drag_target(&edit_data.rects, self.root(), Some(*source), p)
            }
            Drag::TransitionSource(_, target, ..)
            | Drag::TransitionTarget(_, target, ..)
            | Drag::AddTransition(_, target, ..) => {
                *target = self
                    .drag_target(&edit_data.rects, self.root(), None, p)
                    // No transition can target the root.
                    .filter(|t| *t != self.root())
            }
            Drag::Initial(initial_idx, target, _) => {
                let new_target = self
                    .drag_target(&edit_data.rects, self.root(), None, p)
                    // Initial must be a child state.
                    .filter(|t| self.graph.is_child(*initial_idx, *t));
                if target.map(|(t, _)| t) != new_target {
                    // Find free port.
                    *target = new_target.map(|t| (t, self.graph.free_port(t, Direction::Incoming)));
                }
            }
            _ => (),
        };
    }
}
