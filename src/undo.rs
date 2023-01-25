use crate::*;

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
impl<C: Context> Graph<C> {
    pub fn apply_undo(&mut self, op: &Op<C::State, C::Transition>) {
        let g = &mut self.graph;
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
                let _ = self.move_transition_internal(*i, *a, *b);
            }
            Op::RemoveTransition(i, (a, b, t)) => {
                let i2 = g.add_edge(*a, *b, t.clone());
                if *i != i2 {
                    dbg!(i, i2);
                }
            }
            Op::SetInternal(i, internal) => {
                g[*i].internal = !internal;
            }
        }
    }

    pub fn apply_redo(&mut self, op: &Op<C::State, C::Transition>) {
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

#[derive(Clone)]
pub struct Undo<S, T> {
    undos: Vec<Op<S, T>>,
    redos: Vec<Op<S, T>>,
    //in_undo: bool,
}

// Only need this for skipping serialization.
impl<S, T> Default for Undo<S, T> {
    fn default() -> Self {
        Self {
            undos: Vec::new(),
            redos: Vec::new(),
        }
    }
}

impl<C: Context> Graph<C> {
    pub fn undo(&mut self) -> bool {
        if let Some(op) = self.undo.undos.pop() {
            self.apply_undo(&op);
            self.undo.redos.push(op);
            true
        } else {
            false
        }
    }

    pub fn redo(&mut self) -> bool {
        self.undo
            .redos
            .pop()
            .map(|op| {
                self.apply_redo(&op);
                self.undo.undos.push(op);
            })
            .is_some()
    }

    /// If inserting an undo when there are existing redos in the
    /// history, we want to preserve them by moving them into the undo
    /// stack, followed by corresponding undos to revert them. Then we
    /// add the new undo at the end.
    pub fn add_undo(&mut self, undo: Op<C::State, C::Transition>) {
        let redos = std::mem::take(&mut self.undo.redos);
        //let redos = self.undo.redos.drain(..).collect::<Vec<_>>();

        self.undo.undos.extend(redos.iter().cloned().rev());
        self.undo.undos.extend(redos.into_iter().map(|op| op.rev()));
        self.undo.undos.push(undo)
    }
}

#[cfg(test)]
mod tests {
    //use super::*;
    use crate::tests::*;

    #[test]
    fn undo_redo() {
        let mut g = test_graph();
        let a = g.add_state("a".into(), None);
        assert_eq!(g.state(a).unwrap(), "a");
        g.remove_state(a, false, false);
        assert!(g.undo()); // undo remove
        assert_eq!(g.state(a).unwrap(), "a");
        assert!(g.undo()); // undo add
        assert!(!g.undo()); // no more undos
        assert_eq!(g.state(a), None);
        assert!(g.redo()); // redo add
        assert_eq!(g.state(a).unwrap(), "a");
        assert!(g.redo()); // redo remove
        assert_eq!(g.state(a), None);
        assert!(!g.redo()); // no more redos
    }

    #[test]
    fn rewrite_redos() {
        let mut g = test_graph();
        let a = g.add_state("a".into(), None);
        g.update_state(a, "a1".into());
        g.update_state(a, "a2".into());
        g.undo();
        g.undo();
        assert_eq!(g.state(a).unwrap(), "a");
        g.update_state(a, "a3".into());
        g.undo(); // undo a3
        assert_eq!(g.state(a).unwrap(), "a");
        g.undo();
        assert_eq!(g.state(a).unwrap(), "a1");
        g.undo();
        assert_eq!(g.state(a).unwrap(), "a2");
        g.undo();
        assert_eq!(g.state(a).unwrap(), "a1");
        g.undo();
        assert_eq!(g.state(a).unwrap(), "a");
        g.undo(); // undo add
        assert_eq!(g.state(a), None);
    }

    #[test]
    fn ghosts() {
        let mut g = test_graph();
        let a = g.add_state("a".into(), None);
        let _b = g.add_state("b".into(), None);
        g.remove_state(a, false, false);
        let c = g.add_state("c".into(), None);
        assert_eq!(a, c);
        g.undo(); // undo add c
        g.undo(); // undo remove a
        assert_eq!(g.state(a).unwrap(), "a");
        assert_eq!(a, c);
    }
}
