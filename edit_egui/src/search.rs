use crate::*;
use eframe::egui::text::*;
use transit_graph::Idx;

#[derive(Default)]
pub struct SearchBox<T> {
    pub parent_id: Option<Id>,
    pub query: String,
    pub position: Option<Pos2>,
    pub results: Option<Vec<(T, LayoutJob)>>,
    /// If true [show] will only ever return an exact match, or the only matching result.
    pub match_required: bool,
}

/// If [SearchBox::match_required] is false, [SearchBox::show] can return the query string when
/// enter is pressed.
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
        self.results = None;
    }

    pub fn show<'a, U, I>(&mut self, set_focus: bool, iter: I, ui: &mut Ui) -> Submit<T>
    where
        U: Matches<T>,
        I: Iterator<Item = U>,
    {
        if set_focus {
            self.parent_id = Some(ui.id());
        } else if self.parent_id != Some(ui.id()) {
            return Submit::None;
        }

        let mut submit = Submit::None;

        // Split search box and results?
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

                    // Update results if this is our first time or the query has changed.
                    if self.results.is_none() || response.changed {
                        let results = self.results.get_or_insert(Vec::new());
                        results.clear();

                        for i in iter {
                            if let Some(result) = i.matches(
                                self.query.as_str(),
                                // This should match the current style:
                                TextFormat::default(),
                                ui.style().visuals.code_bg_color,
                            ) {
                                results.push(result);
                            }
                        }
                    }

                    // Show results.
                    let result_has_focus = self
                        .results
                        .as_ref()
                        .map(|results| {
                            Frame::default()
                                .show(ui, |ui| {
                                    ScrollArea::vertical()
                                        .show(ui, |ui| {
                                            results.iter().fold(
                                                false,
                                                |focus, (result, layout_job)| {
                                                    // clone layout?
                                                    let label_response = ui.selectable_label(
                                                        false,
                                                        layout_job.clone(),
                                                    );
                                                    if label_response.clicked() {
                                                        submit = Submit::Result(result.clone());
                                                        self.parent_id = None;
                                                    }

                                                    focus || label_response.has_focus()
                                                },
                                            )
                                        })
                                        .inner
                                })
                                .inner
                        })
                        .unwrap_or_default();

                    // If a result was clicked we can return now.
                    if let Submit::Result(_) = submit {
                        return;
                    }

                    if response.lost_focus() {
                        // Submit on enter (if it matches).
                        if ui.input(|i| i.key_down(Key::Enter)) {
                            if self.match_required {
                                // If there is only one result just submit it.
                                if let Some(results) = &self.results {
                                    if results.len() == 1 {
                                        submit = Submit::Result(results[0].0.clone());
                                    }
                                } else {
                                    // No match, just hide? Error?
                                }
                                self.clear();
                            } else {
                                // Submit the query.
                                submit = Submit::Query(self.query.clone());
                                self.clear();
                            }
                        } else {
                            // Nothing has focus, clear. Escape, tab, etc.
                            if !result_has_focus {
                                self.clear();
                            }
                        }
                    };
                });
            });
        });

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
