use crate::{Edit, State, Transition};
use transit::{is_internal, Edge, EditGraph, Idx, Initial, Node, Tdx};

// (a -> b, edge)
pub type TOp<T> = (Idx, Idx, Edge<T>);

#[derive(Clone)]
pub enum Op<S, T> {
    AddState(Idx, Node<S>),
    UpdateState(Idx, Node<S>, Node<S>),
    RemoveState(Idx, Node<S>),
    AddTransition(Tdx, TOp<T>),
    UpdateTransition(Tdx, TOp<T>, TOp<T>),
    RemoveTransition(Tdx, TOp<T>),
    SetInternal(Tdx, bool),
    // For grouping state removals with transitions?
    //Transaction(Vec<Op<C>>),
}

// TODO: error handling
impl Edit {
    pub fn apply_undo(&mut self, op: &Op<State, Transition>) {
        let g = &mut self.graph.graph;
        match op {
            Op::AddState(i, _s) => {
                let _s = g.remove_node(*i).expect("add state op does not exist!");
            }
            Op::UpdateState(i, s1, _s2) => {
                g[*i] = s1.clone();
            }
            // The index may be different which may be a problem? Do
            // we need ghost states to save indices from the history?
            Op::RemoveState(i, s) => {
                let i2 = g.add_node(s.clone());
                if *i != i2 {
                    dbg!(i, i2);
                }
            }
            Op::AddTransition(i, _t) => {
                let _t = g
                    .remove_edge(*i)
                    .expect("add transition op does not exist!");
            }
            Op::UpdateTransition(i, (a, b, t1), _) => {
                g[*i] = t1.clone();
                let _ = self.graph.move_transition(*i, *a, *b);
            }
            Op::RemoveTransition(i, (a, b, t)) => {
                let i2 = g.add_edge(*a, *b, t.clone());
                if *i != i2 {
                    dbg!(i, i2);
                }
            }
            Op::SetInternal(i, internal) => {
                // We can't move the transition w/o removing/adding the edge.
                let edge = &mut g[*i];
                *edge = edge.clone().set_internal(*internal);
            }
        }
    }

    pub fn apply_redo(&mut self, op: &Op<State, Transition>) {
        self.apply_undo(&op.clone().rev())
    }
}

impl<S, T> Op<S, T> {
    pub fn rev(self) -> Self {
        match self {
            Op::AddState(i, s) => Op::RemoveState(i, s),
            Op::UpdateState(i, s1, s2) => Op::UpdateState(i, s2, s1),
            Op::RemoveState(i, s) => Op::AddState(i, s),
            Op::AddTransition(i, t) => Op::RemoveTransition(i, t),
            Op::UpdateTransition(i, a, b) => Op::UpdateTransition(i, b, a),
            Op::RemoveTransition(i, t) => Op::AddTransition(i, t),
            Op::SetInternal(i, internal) => Op::SetInternal(i, !internal),
        }
    }
}

impl Edit {
    pub fn undo(&mut self) -> bool {
        if let Some(op) = self.undos.pop() {
            self.apply_undo(&op);
            self.redos.push(op);
            true
        } else {
            false
        }
    }

    pub fn redo(&mut self) -> bool {
        self.redos
            .pop()
            .map(|op| {
                self.apply_redo(&op);
                self.undos.push(op);
            })
            .is_some()
    }

    /// If inserting an undo when there are existing redos in the
    /// history, we want to preserve them by moving them into the undo
    /// stack, followed by corresponding undos to revert them. Then we
    /// add the new undo at the end.
    pub fn add_undo(&mut self, undo: Op<State, Transition>) {
        let redos = std::mem::take(&mut self.redos);
        //let redos = self.undo.redos.drain(..).collect::<Vec<_>>();

        self.undos.extend(redos.iter().cloned().rev());
        self.undos.extend(redos.into_iter().map(|op| op.rev()));
        self.undos.push(undo)
    }
}

impl EditGraph<State, Transition> for Edit {
    fn add_state(&mut self, state: State, parent: Option<Idx>) -> Idx {
        let i = self.graph.add_state(state, parent);
        let s = self.graph.graph[i].clone();
        self.add_undo(Op::AddState(i, s));
        i
    }

    // FIX: undo transaction
    fn remove_state(
        &mut self,
        i: Idx,
        keep_transitions: bool,
        keep_children: bool,
    ) -> Option<Node<State>> {
        // FIX: this potentially does all sorts of mutations besides removing the state
        let s = self.graph.remove_state(i, keep_transitions, keep_children);
        if let Some(ref s) = s {
            self.add_undo(Op::RemoveState(i, s.clone()))
        }
        s
    }

    fn update_state(&mut self, i: Idx, state: State) {
        let s1 = self.graph.graph[i].clone();
        self.graph.graph[i].state = state;
        let s2 = self.graph.graph[i].clone();
        self.add_undo(Op::UpdateState(i, s1, s2));
    }

    fn set_parent(&mut self, i: Idx, parent: Idx) {
        // TODO: Op::UpdateParent?
        let s0 = self.graph.graph[i].clone();
        self.graph.set_parent(i, parent);
        let s = self.graph.graph[i].clone();
        self.add_undo(Op::UpdateState(i, s0, s));
    }

    fn set_root_initial(&mut self, initial: Initial) {
        self.graph.set_root_initial(initial);
    }

    fn set_initial(&mut self, i: Idx, initial: Initial) {
        // TODO: Op::UpdateInitial?
        let s1 = self.graph.graph[i].clone();
        self.graph.set_initial(i, initial);
        let s2 = self.graph.graph[i].clone();
        self.add_undo(Op::UpdateState(i, s1, s2));
    }

    fn set_initial_idx(&mut self, i: Idx, initial: Option<Idx>) {
        self.graph.set_initial_idx(i, initial);
    }

    fn set_internal(&mut self, i: Tdx, internal: bool) {
        let i0 = is_internal(&self.graph.graph[i]);
        if internal != i0 {
            self.add_undo(Op::SetInternal(i, internal));
            self.graph.set_internal(i, internal);
        }
    }

    fn add_transition(&mut self, a: Idx, b: Idx, t: impl Into<Edge<Transition>>) -> Tdx {
        let i = self.graph.add_transition(a, b, t);
        let t = self.graph.graph[i].clone();
        self.add_undo(Op::AddTransition(i, (a, b, t)));
        i
    }

    fn remove_transition(&mut self, i: Tdx) -> Option<Edge<Transition>> {
        self.graph
            .graph
            .edge_endpoints(i)
            .zip(self.graph.remove_transition(i))
            .map(|((a, b), t)| {
                self.add_undo(Op::RemoveTransition(i, (a, b, t.clone())));
                t
            })
    }

    fn update_transition(&mut self, i: Tdx, t: Transition) {
        if let Some((a, b)) = self.graph.graph.edge_endpoints(i) {
            let t0 = self.graph.graph[i].clone();
            self.graph.update_transition(i, t);
            let t1 = self.graph.graph[i].clone();
            self.add_undo(Op::UpdateTransition(i, (a, b, t0), (a, b, t1)));
        }
    }

    fn move_transition(&mut self, i: Tdx, a: Idx, b: Idx) {
        // Can't transition to or from the root.
        assert!(a != self.graph.root);
        assert!(b != self.graph.root);

        if let Some((a0, b0)) = self.graph.graph.edge_endpoints(i) {
            // Op::UpdateEndpoints?
            let t = self.graph.graph[i].clone();
            self.add_undo(Op::UpdateTransition(i, (a0, b0, t.clone()), (a, b, t)));
            self.graph.move_transition(i, a, b)
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::Edit;
    use transit::EditGraph;

    #[test]
    fn undo_redo() {
        let mut g = Edit::default();

        let a = g.add_state("a".into(), None);
        assert_eq!(g.graph.state(a).unwrap().id, "a");
        g.remove_state(a, false, false);
        assert!(g.undo()); // undo remove
        assert_eq!(g.graph.state(a).unwrap().id, "a");
        assert!(g.undo()); // undo add
        assert!(!g.undo()); // no more undos
        assert_eq!(g.graph.state(a).is_none(), true);
        assert!(g.redo()); // redo add
        assert_eq!(g.graph.state(a).unwrap().id, "a");
        assert!(g.redo()); // redo remove
        assert_eq!(g.graph.state(a).is_none(), true);
        assert!(!g.redo()); // no more redos
    }

    #[test]
    fn rewrite_redos() {
        let mut g = Edit::default();

        let a = g.add_state("a".into(), None);
        g.update_state(a, "a1".into());
        g.update_state(a, "a2".into());
        g.undo();
        g.undo();
        assert_eq!(g.graph.state(a).unwrap().id, "a");
        g.update_state(a, "a3".into());
        g.undo(); // undo a3
        assert_eq!(g.graph.state(a).unwrap().id, "a");
        g.undo();
        assert_eq!(g.graph.state(a).unwrap().id, "a1");
        g.undo();
        assert_eq!(g.graph.state(a).unwrap().id, "a2");
        g.undo();
        assert_eq!(g.graph.state(a).unwrap().id, "a1");
        g.undo();
        assert_eq!(g.graph.state(a).unwrap().id, "a");
        g.undo(); // undo add
        assert_eq!(g.graph.state(a).is_none(), true);
    }

    #[test]
    fn ghosts() {
        let mut g = Edit::default();

        let a = g.add_state("a".into(), None);
        let _b = g.add_state("b".into(), None);
        g.remove_state(a, false, false);
        let c = g.add_state("c".into(), None);
        assert_eq!(a, c);
        g.undo(); // undo add c
        g.undo(); // undo remove a
        assert_eq!(g.graph.state(a).unwrap().id, "a");
        assert_eq!(a, c);
    }
}
