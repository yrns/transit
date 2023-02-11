use eframe::egui::*;

pub struct Editabel {
    sense: Sense,
}

#[derive(Clone)]
pub enum EditabelState {
    Closed(f32),
    Open(String, f32, bool),
}

impl Default for EditabelState {
    fn default() -> Self {
        Self::Closed(0.0)
    }
}

/// This shows as a Label, but changes to a TextEdit when
/// double-clicked. When enter is pressed it returns the new text and
/// reverts to a Label. This takes up the minimal space when not
/// editing.
impl Editabel {
    pub fn new() -> Self {
        Self {
            sense: Sense::click(),
        }
    }

    pub fn sense(sense: Sense) -> Self {
        Self { sense }
    }

    pub fn show(&self, text: &str, ui: &mut Ui) -> InnerResponse<Option<String>> {
        let state: EditabelState = ui.data_mut(|d| d.get_temp(ui.id()).unwrap_or_default());
        match state {
            EditabelState::Closed(width) => {
                let response = ui.add(Label::new(text).sense(self.sense));

                let t = ui.ctx().animate_bool(ui.id(), false);
                ui.add_space(t * width);

                if response.double_clicked() {
                    // Next frame select all?
                    ui.data_mut(|d| {
                        d.insert_temp(
                            ui.id(),
                            EditabelState::Open(text.to_owned(), response.rect.width(), true),
                        )
                    });
                }
                InnerResponse::new(None, response)
            }
            EditabelState::Open(mut text, width, request_focus) => {
                let t = ui.ctx().animate_bool(ui.id(), true);
                let extra = t * 24.0;

                let output = TextEdit::singleline(&mut text)
                    .desired_width(width + extra)
                    .cursor_at_end(true)
                    .show(ui);

                let response = output.response;

                if request_focus {
                    ui.memory_mut(|m| m.request_focus(response.id));
                }

                // Return new text on enter (else revert).
                if response.lost_focus() {
                    ui.data_mut(|d| d.insert_temp(ui.id(), EditabelState::Closed(width)));
                    InnerResponse::new(
                        ui.input(|i| i.key_pressed(Key::Enter)).then_some(text),
                        response,
                    )
                } else {
                    // Write new text if changed, or unsetting request_focus.
                    if response.changed() || request_focus {
                        ui.data_mut(|d| {
                            d.insert_temp(ui.id(), EditabelState::Open(text, width, false))
                        });
                    }
                    InnerResponse::new(None, response)
                }
            }
        }
    }
}
