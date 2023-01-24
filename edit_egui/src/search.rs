use crate::*;
use eframe::egui::text::*;
use transit::Idx;

#[derive(Default)]
pub struct SearchBox<T> {
    pub parent_id: Option<Id>,
    pub query: String,
    pub position: Option<Pos2>,
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
    T: Clone + std::fmt::Debug,
{
    pub fn show<'a, U, I>(&mut self, set_focus: bool, iter: I, ui: &mut Ui) -> Option<T>
    where
        U: Matches<T>,
        I: Iterator<Item = U>,
    {
        if set_focus {
            self.parent_id = Some(ui.id());
        } else if self.parent_id != Some(ui.id()) {
            return None;
        }

        let mut selected = None;

        // Split search box and completions?
        let area = Area::new(ui.id().with("search")).order(Order::Foreground);

        let area = if let Some(p) = self.position {
            area.current_pos(p)
        } else {
            area
        };

        area.show(ui.ctx(), |ui| {
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
                            self.parent_id = None;
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
                                    self.parent_id = None;
                                    return;
                                }
                            }
                        });
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

impl<'a> Matches<String> for String {
    fn matches(
        &self,
        query: &str,
        text_format: TextFormat,
        match_color: Color32,
    ) -> Option<(String, LayoutJob)> {
        match_layout(self, query, text_format, match_color).map(|a| (self.clone(), a))
    }
}
