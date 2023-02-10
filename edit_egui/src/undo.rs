use crate::{Edit, State, Transition};
use transit::{Idx, Op};

#[cfg_attr(feature = "serde", derive(serde::Deserialize, serde::Serialize))]
#[derive(Default)]
pub struct Undo {
    undos: Vec<Op<State, Transition>>,
    redos: Vec<Op<State, Transition>>,
}

impl Undo {
    pub fn can_undo(&self) -> bool {
        !self.undos.is_empty()
    }

    pub fn can_redo(&self) -> bool {
        !self.redos.is_empty()
    }
}

impl Edit {
    pub fn undo(&mut self) -> bool {
        if let Some(op) = self.undo.undos.pop() {
            self.undo.redos.push(op.undo(&mut self.graph));
            true
        } else {
            false
        }
    }

    pub fn redo(&mut self) -> bool {
        self.undo
            .redos
            .pop()
            .map(|op| self.undo.undos.push(op.undo(&mut self.graph)))
            .is_some()
    }

    // Returns a string representing the history of the specified node.
    pub fn history(&self, i: Idx) -> String {
        let mut s = String::new();
        for op in &self.undo.undos {
            match op {
                Op::InsertNode(_i) if *_i == i => s.push('+'),
                Op::UpdateNode(_i, n) if *_i == i => s.push_str(&n.state.id),
                Op::RemoveNode(_i, n) if *_i == i => {
                    s.push('-');
                    s.push_str(&n.state.id)
                }
                _ => (),
            }
            s.push(' ');
        }

        s.push_str("->");
        s.push_str(&self.graph.graph[i].state.id);
        s.push(' ');

        // undo0 undo1 ->current redo1 redo0, etc.
        for op in self.undo.redos.iter().rev() {
            match op {
                Op::InsertNode(_i) if *_i == i => s.push('+'),
                Op::UpdateNode(_i, n) if *_i == i => s.push_str(&n.state.id),
                Op::RemoveNode(_i, n) if *_i == i => {
                    s.push('-');
                    s.push_str(&n.state.id)
                }
                _ => (),
            }
            s.push(' ');
        }

        s
    }

    /// Add an undo op to the stack, while retaining redo history.
    pub fn add_undo(&mut self, op: impl Into<Op<State, Transition>>) {
        let op = op.into();

        let redos = std::mem::take(&mut self.undo.redos);

        // If inserting an undo when there are existing redos in the history, we want to preserve
        // them by moving them into the undo stack, followed by corresponding undos to revert
        // them. Then we add the new undo at the end.
        if redos.len() > 1 {
            let g = &mut self.graph;

            // The undo ops don't store current state, and the current operation has already updated
            // the graph, so we need to undo it first then redo it after.
            let current = op.clone().undo(g);

            // Apply redos.
            let undos: Vec<_> = redos.into_iter().rev().map(|op| op.undo(g)).collect();
            self.undo.undos.extend(undos.iter().cloned());

            // Undo redos (in reverse).
            self.undo
                .undos
                .extend(undos.into_iter().rev().map(|op| op.undo(g)));

            // Reset the current value.
            current.undo(g);
        }

        // Push the new undo.
        self.undo.undos.push(op); //.clone());

        // if let Some(i) = &op.idx() {
        //     println!("add_undo: {}", self.history(*i));
        // }
    }
}

#[cfg(test)]
mod tests {
    use crate::Edit;

    #[test]
    fn undo_redo() {
        let mut g = Edit::default();

        let a = g.graph.add_state("a".into(), None);
        g.add_undo(a);
        assert_eq!(g.graph.state(a).unwrap().id, "a");
        let op = g.graph.remove_state(a, false, false);
        g.add_undo(op);
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

        let a = g.graph.add_state("a".into(), None);
        g.add_undo(a);
        let op = g.graph.update_state(a, "a1".into()); // a ->a1
        g.add_undo(op);
        let op = g.graph.update_state(a, "a2".into()); // a a1 ->a2
        g.add_undo(op);
        println!("{}", g.history(a));
        g.undo(); // a ->a1 a2
        println!("{}", g.history(a));
        g.undo(); // ->a a1 a2
        assert_eq!(g.graph.state(a).unwrap().id, "a");
        println!("{}", g.history(a));
        let op = g.graph.update_state(a, "a3".into()); // a a1 a2 a1 a ->a3
        g.add_undo(op);
        println!("{}", g.history(a));
        g.undo(); // a a1 a2 a1 ->a a3
        assert_eq!(g.graph.state(a).unwrap().id, "a");
        g.undo();
        assert_eq!(g.graph.state(a).unwrap().id, "a1");
        g.undo();
        assert_eq!(g.graph.state(a).unwrap().id, "a2");
        g.undo();
        assert_eq!(g.graph.state(a).unwrap().id, "a1");
        g.undo();
        assert_eq!(g.graph.state(a).unwrap().id, "a");
        assert_eq!(g.undo(), true); // undo add
        assert_eq!(g.graph.state(a).is_none(), true);
    }

    #[test]
    fn ghosts() {
        let mut g = Edit::default();

        let a = g.graph.add_state("a".into(), None);
        g.add_undo(a);
        let b = g.graph.add_state("b".into(), None);
        g.add_undo(b);
        let op = g.graph.remove_state(a, false, false);
        g.add_undo(op);
        let c = g.graph.add_state("c".into(), None);
        g.add_undo(c);
        assert_eq!(a, c);
        g.undo(); // undo add c
        g.undo(); // undo remove a
        assert_eq!(g.graph.state(a).unwrap().id, "a");
        assert_eq!(a, c);
    }
}
