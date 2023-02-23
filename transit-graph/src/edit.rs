use crate::*;

// Source, target, edge.
pub type EOp<T> = (Idx, Idx, Edge<T>);

#[cfg_attr(feature = "serde", derive(serde::Deserialize, serde::Serialize))]
#[derive(Clone, Debug, Default)]
pub enum Op<S, T> {
    #[default]
    Noop,
    InsertNode(Idx),
    UpdateNode(Idx, Node<S>),
    RemoveNode(Idx, Node<S>),
    InsertEdge(Tdx),
    UpdateEdge(Tdx, Edge<T>),
    RemoveEdge(Tdx, EOp<T>),
    Transaction(Vec<Self>),
}

impl<S, T> From<Idx> for Op<S, T> {
    fn from(i: Idx) -> Self {
        Op::InsertNode(i)
    }
}

impl<S, T> From<Tdx> for Op<S, T> {
    fn from(i: Tdx) -> Self {
        Op::InsertEdge(i)
    }
}

impl<S, T> Op<S, T> {
    /// Find the node index for this op.
    pub fn idx(&self) -> Option<Idx> {
        match self {
            Op::InsertNode(i) | Op::UpdateNode(i, _) | Op::RemoveNode(i, _) => Some(*i),
            // FIX: this is not generally true, add the source to the transaction variant?
            Op::Transaction(ops) => ops.iter().find_map(|op| op.idx()),
            _ => None,
        }
    }

    /// Find the edge index for this op.
    pub fn tdx(&self) -> Option<Tdx> {
        match self {
            Op::InsertEdge(i) | Op::UpdateEdge(i, _) | Op::RemoveEdge(i, _) => Some(*i),
            // FIX: see above
            Op::Transaction(ops) => ops.iter().find_map(|op| op.tdx()),
            _ => None,
        }
    }
}

// TODO: remove must_use at some point
impl<S, T> Graph<S, T>
where
    S: Clone,
    T: Clone,
{
    #[must_use = "op"]
    pub fn add_state(&mut self, state: S, parent: Option<Idx>) -> Idx {
        self.graph
            .add_node(Node::new(state, parent.or(Some(self.root))))
    }

    #[must_use = "op"]
    pub fn remove_state(
        &mut self,
        i: Idx,
        keep_transitions: bool,
        keep_children: bool,
    ) -> Vec<Op<S, T>> {
        // Cannot remove the root.
        assert!(i != self.root);

        let mut ops = Vec::new();

        // Kept things go to the parent state (if not root).
        let parent = self.graph.node_weight(i).and_then(|s| s.parent).unwrap();
        //.filter(|i| *i != self.root);

        // Clean up child states.
        for child in self.children(i).collect::<Vec<_>>() {
            ops.extend(if keep_children {
                self.set_parent(child, parent)
            } else {
                self.remove_state(child, keep_transitions, false)
            })
        }

        // Clean up initial/history.
        let mut path = self.path_walk(i);
        while let Some(p) = path.next(self) {
            if let Some((initial, _)) = self.initial(p) {
                ops.extend(self.set_initial(p, Some((initial, parent))))
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
            while let Some(edge) = edges.next_edge(&self.graph) {
                // We are looking up endpoints again when moving...
                ops.extend(match self.graph.edge_endpoints(edge) {
                    Some((a, b)) if a == b => self.move_transition(edge, parent, parent),
                    Some((a, b)) if a == i => self.move_transition(edge, parent, b),
                    Some((a, b)) if b == i => self.move_transition(edge, a, parent),
                    _ => panic!("no endpoints"),
                })
            }
        } else {
            while let Some(edge) = edges.next_edge(&self.graph) {
                ops.push(self.remove_transition(edge))
            }
        }

        ops.push(Op::RemoveNode(i, self.graph.remove_node(i).unwrap()));

        ops
    }

    // For [Op::undo].
    #[must_use = "op"]
    pub fn update_node(&mut self, i: Idx, node: Node<S>) -> Op<S, T> {
        let n = &mut self.graph[i];
        let op = Op::UpdateNode(i, n.clone());
        *n = node;
        op
    }

    /// Update state.
    #[must_use = "op"]
    pub fn update_state(&mut self, i: Idx, state: S) -> Op<S, T> {
        let mut n = self.graph[i].clone();
        n.state = state;
        self.update_node(i, n)
    }

    #[must_use = "op"]
    pub fn set_parent(&mut self, i: Idx, parent: Idx) -> Vec<Op<S, T>> {
        // Cannot change the parent of the root.
        assert!(i != self.root);

        // Make sure the new parent isn't a child.
        assert!(!self.in_path(i, parent));

        let mut ops = Vec::new();

        let n = &mut self.graph[i];
        let p0 = n.parent;
        ops.push(Op::UpdateNode(i, n.clone()));
        n.parent = Some(parent);

        // Validate initial from prior path.
        if let Some(p) = p0 {
            let mut path = self.path_walk(p);
            while let Some(a) = path.next(self) {
                if let Some((_, i)) = self.initial(a) {
                    if !self.is_child(a, i) {
                        ops.extend(self.set_initial(a, None));
                    }
                }
            }
        }

        ops
    }

    /// Set initial for the root state (the default state for the
    /// graph). `set_graph_initial`?
    #[must_use = "op"]
    pub fn set_root_initial(&mut self, initial: (Initial, Idx)) -> Vec<Op<S, T>> {
        self.set_initial(self.root, Some(initial))
    }

    #[must_use = "op"]
    pub fn set_initial(&mut self, i: Idx, initial: Option<(Initial, Idx)>) -> Vec<Op<S, T>> {
        assert!(self.graph.contains_node(i));

        // This can return 0-2 ops (remove, update, remove/add, add, or nothing).
        let mut ops = vec![];

        match self.initial_edge(i) {
            Some((_, edge)) => {
                match initial {
                    None => ops.push(self.remove_edge(edge.id())),
                    Some((initial, j)) => {
                        // Make sure the initial state (if any) is a child.
                        assert!(self.is_child(i, j));
                        let initial = Edge::Initial(initial);
                        if edge.target() == j {
                            // Same target, just update.
                            ops.push(self.update_edge(edge.id(), initial))
                        } else {
                            ops.push(self.remove_edge(edge.id()));
                            ops.push(self.graph.add_edge(i, j, initial).into())
                        }
                    }
                }
            }
            None => {
                if let Some((initial, j)) = initial {
                    assert!(self.is_child(i, j));
                    ops.push(self.graph.add_edge(i, j, Edge::Initial(initial)).into())
                }
            }
        }

        ops
    }

    #[must_use = "op"]
    pub fn set_internal(&mut self, i: Tdx, internal: bool) -> Op<S, T> {
        let e = &mut self.graph[i];
        let op = Op::UpdateEdge(i, e.clone());
        *e = e.clone().set_internal(internal);
        op
    }

    // TODO: check root? validation?
    #[must_use = "op"]
    pub fn add_transition(&mut self, a: Idx, b: Idx, t: impl Into<Edge<T>>) -> Tdx {
        let t = t.into();

        // Only self-transitions can be internal.
        assert!(!is_internal(&t) || a == b);

        // Can't transition to or from root.
        assert!(a != self.root && b != self.root);

        self.graph.add_edge(a, b, t)
    }

    #[must_use = "op"]
    pub fn remove_edge(&mut self, i: Tdx) -> Op<S, T> {
        Op::RemoveEdge(
            i,
            self.graph
                .edge_endpoints(i)
                .zip(self.graph.remove_edge(i))
                .map(|((a, b), t)| (a, b, t))
                .unwrap(),
        )
    }

    #[must_use = "op"]
    pub fn remove_transition(&mut self, i: Tdx) -> Op<S, T> {
        // TODO: assert on type
        self.remove_edge(i)
    }

    #[must_use = "op"]
    pub fn update_edge(&mut self, i: Tdx, edge: Edge<T>) -> Op<S, T> {
        let e = &mut self.graph[i];
        let op = Op::UpdateEdge(i, e.clone());
        *e = edge;
        op
    }

    #[must_use = "op"]
    pub fn update_transition(&mut self, i: Tdx, t: T) -> Op<S, T> {
        let e = &mut self.graph[i];
        let op = Op::UpdateEdge(i, e.clone());
        e.set_transition(t);
        op
    }

    // There is no API for updating an existing edge so we remove/insert
    // (see https://github.com/petgraph/petgraph/pull/103).
    #[must_use = "op"]
    pub fn move_transition(&mut self, i: Tdx, a: Idx, b: Idx) -> Vec<Op<S, T>> {
        // Can't transition to or from the root.
        assert!(a != self.root);
        assert!(b != self.root);

        let remove = self.remove_transition(i);
        let Op::RemoveEdge(_, (_, _, ref t)) = remove else {
            panic!("not a remove");
        };
        let insert = self.add_transition(a, b, t.clone());

        // Make sure we get the same index back.
        assert_eq!(i, insert);

        vec![remove, insert.into()]
    }
}

impl<S, T> From<Vec<Op<S, T>>> for Op<S, T> {
    fn from(mut ops: Vec<Op<S, T>>) -> Self {
        if ops.len() == 1 {
            ops.pop().unwrap() // remove?
        } else {
            Op::Transaction(ops)
        }
    }
}

impl<S, T> Op<S, T>
where
    S: Clone,
    T: Clone,
{
    pub fn undo(self, g: &mut Graph<S, T>) -> Self {
        match self {
            Op::Noop => Op::Noop,
            Op::InsertNode(i) => {
                // If the history is ordered this state should not have any relations.
                assert!(g.children(i).next().is_none());
                assert!(g.transitions_in(i).next().is_none());
                assert!(g.transitions_out(i).next().is_none());
                g.remove_state(i, false, false).into()
            }
            Op::UpdateNode(i, n) => g.update_node(i, n),
            Op::RemoveNode(i, n) => {
                let insert = g.graph.add_node(n);
                assert_eq!(insert, i);
                i.into()
            }
            Op::InsertEdge(i) => g.remove_transition(i),
            Op::UpdateEdge(i, e) => g.update_edge(i, e),
            Op::RemoveEdge(i, (a, b, e)) => {
                let insert = g.graph.add_edge(a, b, e);
                assert_eq!(insert, i);
                i.into()
            }
            // Is there a way to move and replace without recollection or cloning?
            Op::Transaction(ops) => ops
                .into_iter()
                // Undo from the end first.
                .rev()
                .map(|op| op.undo(g))
                .collect::<Vec<_>>()
                .into(),
        }
    }

    // This was used by `add_undo` to reverse redos w/o applying them to the graph, but it is no
    // longer. This reads from the graph rather than writes.
    pub fn rev(self, g: &mut Graph<S, T>) -> Self {
        match self {
            Op::Noop => Op::Noop,
            Op::InsertNode(i) => Op::RemoveNode(i, g.graph[i].clone()),
            Op::UpdateNode(i, _n) => Op::UpdateNode(i, g.graph[i].clone()),
            Op::RemoveNode(i, _n) => Op::InsertNode(i),
            Op::InsertEdge(i) => g
                .endpoints(i)
                .map(|(a, b)| Op::RemoveEdge(i, (a, b, g.graph[i].clone())))
                .expect("endpoints"),
            Op::UpdateEdge(i, _e) => Op::UpdateEdge(i, g.graph[i].clone()),
            Op::RemoveEdge(i, _e) => Op::InsertEdge(i),
            Op::Transaction(ops) => {
                Op::Transaction(ops.into_iter().rev().map(|op| op.rev(g)).collect())
            }
        }
    }
}
