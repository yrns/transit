use crate::*;
use eframe::egui::text::*;
use transit::Idx;

#[derive(Default)]
pub struct SearchBox<T> {
    pub visible: bool,
    pub query: String,
    pub results: Vec<(T, LayoutJob)>,
}

pub trait Matches<T> {
    fn matches(
        &self,
        query: &str,
        text_format: TextFormat,
        match_color: Color32,
    ) -> Option<(T, LayoutJob)>;
}

impl<T> SearchBox<T>
where
    T: Clone,
{
    pub fn show<'a, U, I>(&mut self, set_focus: bool, iter: I, ui: &mut Ui) -> Option<T>
    where
        U: Matches<T>,
        I: Iterator<Item = U>,
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

                    for i in iter {
                        if let Some(result) = i.matches(
                            self.query.as_str(),
                            // This should match the current style:
                            TextFormat::default(),
                            ui.style().visuals.code_bg_color,
                        ) {
                            self.results.push(result);
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
                            // clone?
                            if ui.selectable_label(false, result.clone()).clicked() {
                                selected = Some(i.clone());
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

/// Highlight the substring matches.
fn match_layout(
    search: &str,
    query: &str,
    text_format: TextFormat,
    match_color: Color32,
) -> Option<LayoutJob> {
    let mut indices = search.match_indices(query).peekable();
    if indices.peek() != None {
        let mut result = LayoutJob::default();
        let mut last_end = 0;
        let mut match_format = text_format.clone();
        match_format.background = match_color;

        for (start, part) in indices {
            result.append(&search[last_end..start], 0.0, text_format.clone());
            result.append(part, 0.0, match_format.clone());
            last_end = start + part.len();
        }
        result.append(&search[last_end..], 0.0, text_format.clone());
        Some(result)
    } else {
        None
    }
}

impl<'a> Matches<Idx> for (Idx, &'a State) {
    fn matches(
        &self,
        query: &str,
        text_format: TextFormat,
        match_color: Color32,
    ) -> Option<(Idx, LayoutJob)> {
        match_layout(&self.1.id, query, text_format, match_color).map(|a| (self.0, a))
    }
}
