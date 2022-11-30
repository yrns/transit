use eframe::egui::*;

pub struct Editabel;

#[derive(Clone)]
pub enum EditabelState {
    Closed,
    Open(String, f32),
}

impl Editabel {
    pub fn show(text: &str, ui: &mut Ui) -> InnerResponse<Option<String>> {
        let state: Option<EditabelState> = ui.data().get_temp(ui.id());
        match state {
            None | Some(EditabelState::Closed) => {
                let response = ui.add(Label::new(text).sense(Sense::click()));
                if response.double_clicked() {
                    ui.data().insert_temp(
                        ui.id(),
                        EditabelState::Open(text.to_owned(), response.rect.width()),
                    );
                }
                InnerResponse::new(None, response)
            }
            Some(EditabelState::Open(mut text, width)) => {
                let output = TextEdit::singleline(&mut text)
                    .desired_width(width + 24.0)
                    .cursor_at_end(true)
                    .show(ui);

                if output.response.lost_focus() && ui.input().key_pressed(Key::Enter) {
                    ui.data().insert_temp(ui.id(), EditabelState::Closed);
                    InnerResponse::new(Some(text), output.response)
                } else {
                    if output.response.changed() {
                        // make it bigger?
                        ui.data()
                            .insert_temp(ui.id(), EditabelState::Open(text, width));
                    }
                    InnerResponse::new(None, output.response)
                }
            }
        }
    }
}
