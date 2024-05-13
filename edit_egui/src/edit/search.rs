use super::*;
use eframe::egui::text::*;

#[derive(Debug, Default)]
pub struct SearchBox<T> {
    // Do we need this?
    pub parent_id: Option<Id>,
    pub query: String,
    pub position: Option<Pos2>,
    pub results: Vec<(T, LayoutJob)>,
    /// If true [show] will only ever return an exact match, or the only matching result.
    pub match_required: bool,
}

/// If [SearchBox::match_required] is false, [SearchBox::show] can return the query string when
/// enter is pressed.
#[derive(Debug)]
pub enum Submit<T> {
    None,
    Result(T),
    Query(String),
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
    pub fn clear(&mut self) {
        self.query.clear();
        self.parent_id = None;
        self.results.clear();
    }

    pub fn show<U, I>(&mut self, set_focus: bool, iter: I, parent_id: Id, ui: &mut Ui) -> Submit<T>
    where
        U: Matches<T>,
        I: Iterator<Item = U>,
    {
        if set_focus {
            self.parent_id = Some(parent_id);
        } else if self.parent_id != Some(parent_id) {
            return Submit::None;
        }

        let mut submit = Submit::None;

        // Split search box and results?
        let area = Area::new(parent_id.with("search")).order(Order::Foreground);

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

                    // Update results initially, or if the query has changed.
                    if set_focus || response.changed {
                        self.results = iter
                            .filter_map(|mat| {
                                mat.matches(
                                    self.query.as_str(),
                                    // This should match the current style:
                                    TextFormat::default(),
                                    ui.style().visuals.code_bg_color,
                                )
                            })
                            .collect();
                    }

                    let mut result_has_focus = false;

                    // Show results.
                    if self.results.is_empty() {
                        // "Press return to insert."?
                        ui.label("No matches.");
                    } else {
                        Frame::none()
                            .inner_margin(ui.style().spacing.menu_margin)
                            .show(ui, |ui| {
                                ScrollArea::vertical().show(ui, |ui| {
                                    for (result, layout_job) in &self.results {
                                        // clone layout?
                                        let label_response =
                                            ui.selectable_label(false, layout_job.clone());
                                        if label_response.clicked() {
                                            submit = Submit::Result(result.clone());
                                            return;
                                        } else {
                                            result_has_focus |= label_response.has_focus();
                                        }
                                    }
                                });
                            });
                    }

                    // If a result was clicked we can return now.
                    if matches!(submit, Submit::Result(_)) {
                        return;
                    }

                    // Submit on enter (if it matches).
                    if response.lost_focus() && ui.input(|i| i.key_down(Key::Enter)) {
                        if self.match_required {
                            // If there is only one result just submit it.
                            if self.results.len() == 1 {
                                submit = Submit::Result(self.results[0].0.clone());
                            } else {
                                // No match, just hide? Error?
                                self.clear();
                            }
                        } else {
                            // Submit the query.
                            submit = Submit::Query(self.query.clone());
                        }
                        return;
                    }

                    if !set_focus && response.clicked_elsewhere() {
                        self.clear();
                        return;
                    }

                    // FIX: this clears on mouse press which means nothing is submitted
                    // Nothing has focus, clear. Escape, tab, etc.
                    // if response.lost_focus() && !result_has_focus {
                    //     self.clear();
                    // }
                });
            });
        });

        // Clear on submit.
        if !matches!(submit, Submit::None) {
            self.clear();
        }
        submit
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
    if indices.peek().is_some() {
        let mut result = LayoutJob::default();
        let mut last_end = 0;
        let mut match_format = text_format.clone();
        match_format.background = match_color;

        for (start, part) in indices {
            result.append(&search[last_end..start], 0.0, text_format.clone());
            result.append(part, 0.0, match_format.clone());
            last_end = start + part.len();
        }
        result.append(&search[last_end..], 0.0, text_format);
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

impl Matches<String> for &String {
    fn matches(
        &self,
        query: &str,
        text_format: TextFormat,
        match_color: Color32,
    ) -> Option<(String, LayoutJob)> {
        match_layout(self, query, text_format, match_color).map(|a| (self.to_string(), a))
    }
}
