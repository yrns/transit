use crate::*;
//use eframe::egui::*;
use transit::Idx;

#[derive(Default)]
pub struct SearchBox {
    pub visible: bool,
    pub query: String,
    pub results: Vec<(Idx, String)>,
}

impl SearchBox {
    pub fn show<'a, S>(&mut self, set_focus: bool, states: S, ui: &mut Ui) -> Option<Idx>
    where
        S: Iterator<Item = (Idx, &'a State)>,
    {
        let mut selected = None;

        Frame::popup(ui.style()).show(ui, |ui| {
            ui.vertical(|ui| {
                let response = ui.text_edit_singleline(&mut self.query);
                if set_focus {
                    response.request_focus();
                }

                if response.changed {
                    self.results.clear();

                    for (i, state) in states {
                        let matches: Vec<_> = state.id.matches(self.query.as_str()).collect();
                        if matches.len() > 0 {
                            self.results
                                .push((i, format!("{} ({})", &state.id, i.index())));
                        }
                    }
                }

                let _submit = if response.lost_focus() {
                    if ui.input().key_down(Key::Escape) {
                        self.visible = false;
                        return;
                    }
                    ui.input().key_down(Key::Enter)
                } else {
                    false
                };

                Frame::default().show(ui, |ui| {
                    ScrollArea::vertical().show(ui, |ui| {
                        for (i, result) in self.results.iter() {
                            if ui.selectable_label(false, result).clicked() {
                                selected = Some(*i);
                                return;
                            }
                        }
                    });
                });
            });
        });

        selected
    }
}
