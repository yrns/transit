use crate::*;

pub trait EditGraph<S, T> {
    fn add_state(&mut self, state: S, parent: Option<Idx>) -> Idx;
    fn remove_state(
        &mut self,
        i: Idx,
        keep_transitions: bool,
        keep_children: bool,
    ) -> Option<Node<S>>;
    fn update_state(&mut self, i: Idx, state: S);
    fn set_parent(&mut self, i: Idx, parent: Idx);
    fn set_root_initial(&mut self, initial: Initial);
    fn set_initial(&mut self, i: Idx, initial: Initial);
    fn set_initial_idx(&mut self, i: Idx, initial: Option<Idx>);
    fn set_internal(&mut self, i: Tdx, internal: bool);
    fn add_transition(&mut self, a: Idx, b: Idx, t: impl Into<Edge<T>>) -> Tdx;
    fn remove_transition(&mut self, i: Tdx) -> Option<Edge<T>>;
    fn update_transition(&mut self, i: Tdx, t: T);
    fn move_transition(&mut self, i: Tdx, a: Idx, b: Idx);
}

impl<S, T> EditGraph<S, T> for Graph<S, T>
where
    S: Clone,
    T: Clone,
{
    fn add_state(&mut self, state: S, parent: Option<Idx>) -> Idx {
        let s = Node::new(state, parent.or(Some(self.root)));
        let i = self.graph.add_node(s.clone());
        i
    }

    fn remove_state(
        &mut self,
        i: Idx,
        keep_transitions: bool,
        keep_children: bool,
    ) -> Option<Node<S>> {
        // Cannot remove the root.
        assert!(i != self.root);

        // Kept things go to the parent state (if not root).
        let parent = self.graph.node_weight(i).and_then(|s| s.parent).unwrap();
        //.filter(|i| *i != self.root);

        // Clean up child states.
        for child in self.children(i).collect::<Vec<_>>() {
            if keep_children {
                self.set_parent(child, parent);
            } else {
                self.remove_state(child, keep_transitions, false);
            }
        }

        // Clean up initial/history.
        let mut path = self.path_walk(i);
        while let Some(p) = path.next(self) {
            if self.graph[p].initial.idx() == Some(i) {
                self.set_initial_idx(p, Some(parent));
            }
        }

        // petgraph by default removes all edges to and from this
        // node, which we want to avoid since we want the undo
        // history.

        // `neighbors` is directed/outgoing... FIX: Self-transitions will show up twice here, but
        // the comments here indicate that shouldn't happen:
        // ~/.local/share/cargo/registry/src/github.com-1ecc6299db9ec823/petgraph-0.6.2/src/graph_impl/mod.rs:2023
        let mut edges = self.graph.neighbors_undirected(i).detach();

        // Can't transition to/from root.
        if keep_transitions && parent != self.root {
            // TODO make sure this handles self-transitions correctly
            while let Some(edge) = edges.next_edge(&mut self.graph) {
                // We are looking up endpoints again when moving...
                match self.graph.edge_endpoints(edge) {
                    Some((a, b)) if a == b => self.move_transition(edge, parent, parent),
                    Some((a, b)) if a == i => self.move_transition(edge, parent, b),
                    Some((a, b)) if b == i => self.move_transition(edge, a, parent),
                    _ => (), // error
                }
            }
        } else {
            while let Some(edge) = edges.next_edge(&mut self.graph) {
                self.remove_transition(edge);
            }
        }

        self.graph.remove_node(i)
    }

    /// Update state.
    fn update_state(&mut self, i: Idx, state: S) {
        self.graph[i].state = state;
    }

    fn set_parent(&mut self, i: Idx, parent: Idx) {
        // Cannot change the parent of the root.
        assert!(i != self.root);

        // Make sure the new parent isn't a child.
        assert!(!self.in_path(i, parent));

        let s = &mut self.graph[i];
        let p0 = s.parent;
        s.parent = Some(parent);

        // Validate initial from prior path.
        if let Some(p) = p0 {
            let mut path = self.path_walk(p);
            while let Some(a) = path.next(self) {
                if let Some(i) = self.initial(a).idx() {
                    if !self.is_child(a, i) {
                        self.set_initial(a, Initial::None)
                    }
                }
            }
        }
    }

    /// Set initial for the root state (the default state for the
    /// graph). `set_graph_initial`?
    fn set_root_initial(&mut self, initial: Initial) {
        self.set_initial(self.root, initial);
    }

    fn set_initial(&mut self, i: Idx, initial: Initial) {
        assert!(self.graph.contains_node(i));

        // Make sure the initial state (if any) is a child.
        if let Some(initial_idx) = initial.idx() {
            assert!(self.is_child(i, initial_idx));
        }

        self.graph[i].initial = initial;
    }

    // Sets the initial index, preserving the initial variant. If
    // `initial` is None, sets it to Initial::None.
    fn set_initial_idx(&mut self, i: Idx, initial: Option<Idx>) {
        self.set_initial(
            i,
            initial
                .map(|i0| self.graph[i].initial.clone().set_idx(i0))
                .unwrap_or_default(),
        )
    }

    fn set_internal(&mut self, i: Tdx, internal: bool) {
        let edge = &mut self.graph[i];
        *edge = edge.clone().set_internal(internal);
    }

    // TODO: check root? validation?
    fn add_transition(&mut self, a: Idx, b: Idx, t: impl Into<Edge<T>>) -> Tdx {
        let t = t.into();
        assert!(!is_internal(&t) || a == b);
        // Can't transition to or from root.
        assert!(a != self.root && b != self.root);
        let i = self.graph.add_edge(a, b, t.clone());

        i
    }

    fn remove_transition(&mut self, i: Tdx) -> Option<Edge<T>> {
        self.graph.remove_edge(i)
    }

    fn update_transition(&mut self, i: Tdx, t: T) {
        self.graph[i].set_transition(t);
    }

    // There is no API for updating an existing edge so we add/remove
    // (see https://github.com/petgraph/petgraph/pull/103).
    fn move_transition(&mut self, i: Tdx, a: Idx, b: Idx) {
        if let Some(t) = self.graph.remove_edge(i) {
            let i2 = self.graph.add_edge(a, b, t);
            assert_eq!(i, i2);
        }
    }
}
