use crate::*;

#[derive(Debug)]
pub(crate) enum Command {
    AddState(Idx, Pos2),
    RemoveState(Idx, bool),
    UpdateState(Idx, State),
    /// Source, target, position (relative to parent).
    MoveState(Idx, Idx, Pos2),
    ResizeState(Idx, Vec2),
    AddTransition(Idx, Idx, Transition),
    RemoveTransition(Tdx),
    UpdateTransition(Tdx, Transition),
    MoveTransition(Tdx, Option<Idx>, Option<Idx>),
    SetInitial(Idx, (Initial, Idx), InitialData),
    UnsetInitial(Idx),
    StepInitial(Idx),
    // SetEnter, ???
    // SetExit,
    // SetGuard,
    SetInternal(Tdx, bool),
    UpdateSelection(Selection),
    SelectSourcePath(PathBuf),
    GotoSymbol(String, PathBuf, (usize, usize)),
    UpdateSymbol(SymbolId, Option<String>),
    InsertSymbol(String, PathBuf, String),
    SetNarrow(Option<Idx>),
}

// TODO: impl Into<Command> for Drag?

impl<S> Edit<S>
where
    S: Source,
{
    pub(crate) fn process_commands(&mut self, commands: impl IntoIterator<Item = Command>) {
        for c in commands {
            let op = match c {
                Command::AddState(parent, p) => self
                    .graph
                    .add_state(
                        State {
                            id: "untitled".into(),
                            rect: Rect::from_min_size(p, State::DEFAULT_SIZE),
                            ..Default::default()
                        },
                        Some(parent),
                    )
                    .into(),
                Command::RemoveState(idx, recur) => {
                    self.graph.remove_state(idx, !recur, !recur).into()
                }
                Command::MoveState(idx, parent, offset) => {
                    let mut ops = Vec::new();

                    if self.graph.parent(idx) != Some(parent) {
                        ops.extend(self.graph.set_parent(idx, parent));
                    }

                    let mut state = self.graph.state(idx).unwrap().clone();
                    state.rect = Rect::from_min_size(offset, state.rect.size());
                    ops.push(self.graph.update_state(idx, state));

                    ops.into()
                }
                Command::ResizeState(idx, delta) => {
                    let mut state = self.graph.state(idx).unwrap().clone();
                    state.rect = Rect::from_min_size(state.rect.min, state.rect.size() + delta);
                    self.graph.update_state(idx, state)
                }
                Command::UpdateState(idx, state) => self.graph.update_state(idx, state),
                Command::AddTransition(a, b, t) => self.graph.add_transition(a, b, t).into(),
                Command::UpdateTransition(tdx, t) => self.graph.update_transition(tdx, t),
                Command::MoveTransition(tdx, source, target) => {
                    let (a, b) = self.graph.endpoints(tdx).expect("endpoints");
                    self.graph
                        .move_transition(tdx, source.unwrap_or(a), target.unwrap_or(b))
                        .into()
                }
                Command::RemoveTransition(tdx) => self.graph.remove_transition(tdx),
                Command::UpdateSelection(selection) => {
                    // TODO undo?
                    self.selection = selection;
                    Op::Noop
                }
                Command::SetInitial(i, initial, data) => {
                    // TODO? we still have to update the state unless we stick InitialData into the edge
                    let state = self
                        .graph
                        .state(i)
                        .unwrap()
                        .clone()
                        .with_initial(Some(data));
                    let mut ops = vec![self.graph.update_state(i, state)];
                    ops.extend(self.graph.set_initial(i, Some(initial)));
                    ops.into()
                }
                Command::UnsetInitial(i) => {
                    let state = self.graph.state(i).unwrap().clone().with_initial(None);
                    let mut ops = vec![self.graph.update_state(i, state)];
                    ops.extend(self.graph.set_initial(i, None));
                    ops.into()
                }
                Command::StepInitial(i) => self
                    .graph
                    .set_initial(
                        i,
                        self.graph
                            .initial(i)
                            .map(|(initial, i)| (initial.step(), i)),
                    )
                    .into(),
                Command::SetInternal(i, internal) => self.graph.set_internal(i, internal),
                Command::SelectSourcePath(p) => {
                    // TODO undo?
                    match S::from_path(&p) {
                        Ok(s) => {
                            self.source = Some(s);
                        }
                        Err(e) => error!("error: {:?}", e),
                    }
                    Op::Noop
                }
                Command::UpdateSymbol(symbol, s) => match symbol {
                    SymbolId::Enter(i) => {
                        let state = self.graph.state(i).map(|state| state.clone().with_enter(s));
                        state
                            .map(|state| self.graph.update_state(i, state))
                            .unwrap_or_default()
                    }
                    SymbolId::Exit(i) => {
                        let state = self.graph.state(i).map(|state| state.clone().with_exit(s));
                        state
                            .map(|state| self.graph.update_state(i, state))
                            .unwrap_or_default()
                    }
                    SymbolId::Guard(i) => {
                        let t = self.graph.transition(i).map(|t| t.clone().with_guard(s));
                        t.map(|t| self.graph.update_transition(i, t))
                            .unwrap_or_default()
                    }
                },
                Command::SetNarrow(n) => {
                    self.narrow = n;
                    Op::Noop
                }
                // Some commands (symbols) are handled by the app, so they never show up here.
                _ => {
                    error!(?c, "unhandled command:");
                    Op::Noop
                }
            };

            match op {
                Op::Noop => (),
                _ => self.add_undo(op),
            }
        }
    }
}
