use crate::*;

// (a -> b, edge)
pub type TOp<T> = (Idx, Idx, TransitionData<T>);

#[derive(Clone)]
pub enum Op<C: Context> {
    AddState(Idx, StateState<C>),
    UpdateState(Idx, StateState<C>, StateState<C>),
    RemoveState(Idx, StateState<C>),
    AddTransition(Tdx, TOp<C::Transition>),
    UpdateTransition(Tdx, TOp<C::Transition>, TOp<C::Transition>),
    RemoveTransition(Tdx, TOp<C::Transition>),
    // For grouping state removals with transitions?
    //Transaction(Vec<Op<C>>),
}

impl<C: Context> Op<C> {
    // TODO: error handling
    pub fn undo(&self, g: &mut Graph<C>) {
        match self {
            Op::AddState(i, _s) => {
                let _s = g
                    .graph
                    .remove_node(*i)
                    .expect("add state op does not exist!");
            }
            Op::UpdateState(i, s1, _s2) => {
                g.graph[*i] = s1.clone();
            }
            // The index may be different which may be a problem? Do
            // we need ghost states to save indices from the history?
            Op::RemoveState(i, s) => {
                let i2 = g.graph.add_node(s.clone());
                if *i != i2 {
                    dbg!(i, i2);
                }
            }
            Op::AddTransition(i, _t) => {
                let _t = g
                    .graph
                    .remove_edge(*i)
                    .expect("add transition op does not exist!");
            }
            // TODO: handle internal?
            Op::UpdateTransition(i, (a, b, t1), _) => {
                g.graph[*i] = t1.clone();
                let _ = g.move_transition_internal(*i, *a, *b);
            }
            Op::RemoveTransition(i, (a, b, t)) => {
                let i2 = g.graph.add_edge(*a, *b, t.clone());
                if *i != i2 {
                    dbg!(i, i2);
                }
            }
        }
    }

    pub fn redo(&self, g: &mut Graph<C>) {
        self.clone().rev().undo(g)
    }

    pub fn rev(self) -> Op<C> {
        match self {
            Op::AddState(i, s) => Op::RemoveState(i, s),
            Op::UpdateState(i, s1, s2) => Op::UpdateState(i, s2, s1),
            Op::RemoveState(i, s) => Op::AddState(i, s),
            Op::AddTransition(i, t) => Op::RemoveTransition(i, t),
            Op::UpdateTransition(i, a, b) => Op::UpdateTransition(i, b, a),
            Op::RemoveTransition(i, t) => Op::AddTransition(i, t),
        }
    }
}

#[derive(Clone, Default)]
pub struct Undo<C: Context> {
    undos: Vec<Op<C>>,
    redos: Vec<Op<C>>,
    //in_undo: bool,
}

impl<C: Context> Graph<C> {
    pub fn undo(&mut self) -> bool {
        if let Some(op) = self.undo.undos.pop() {
            op.undo(self);
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
                op.redo(self);
                self.undo.undos.push(op);
            })
            .is_some()
    }

    /// If inserting an undo when there are existing redos in the
    /// history, we want to preserve them by moving them into the undo
    /// stack, followed by corresponding undos to revert them. Then we
    /// add the new undo at the end.
    pub fn add_undo(&mut self, undo: Op<C>) {
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