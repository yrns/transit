use eframe::egui;
use egui::*;

pub struct Frame {
    is_root: bool,
    depth: usize,
    selected: bool,
    is_target: bool,
    is_dragging: bool,
}

pub fn state_frame(
    is_root: bool,
    depth: usize,
    selected: bool,
    is_target: bool,
    is_dragging: bool,
) -> Frame {
    Frame {
        is_root,
        depth,
        selected,
        is_target,
        is_dragging,
    }
}

impl Frame {
    pub fn show(
        &self,
        ui: &mut Ui,
        add_contents: impl FnOnce(&mut Ui) -> Response,
    ) -> InnerResponse<Response> {
        // Always? Check root:
        let frame = if self.is_root {
            egui::Frame::none() //.fill(ui.style().visuals.window_fill()) // too flat
        } else {
            // TODO: we are clipping the stroke, make the margin depend on the stroke?
            egui::Frame::window(&ui.style()).inner_margin(Margin::same(2.0))
        };

        // Can't change the margins after this. Frame is Copy and gets copied here.
        let mut prep = frame.begin(ui);
        let frame = &mut prep.frame;
        let response = add_contents(&mut prep.content_ui);

        // if !self.is_root {
        //     ui.ctx().debug_painter().debug_rect(
        //         prep.content_ui.min_rect(),
        //         Color32::DEBUG_COLOR,
        //         "frame clip",
        //     );
        // }

        let style = ui.style();

        // The default window frame looks good, so let's stick closer to that.
        //let widget_visuals = style.interact_selectable(&response, self.selected);

        let odd = self.depth % 2 == 1;

        if !self.is_root {
            // Alternate background colors based on depth.
            frame.fill = if odd {
                style.visuals.window_fill() //.gamma_multiply(0.65).to_opaque()
            } else {
                // this is the same color as window_stroke with default dark theme
                //style.visuals.widgets.inactive.bg_fill

                // FIX: there is no guarantee this will work with any theme other than default dark
                style.visuals.window_fill().gamma_multiply(0.05).to_opaque()
            };
        }

        frame.stroke = if self.is_target {
            // The default stroke for selection is thin, so make it more prominent here to show
            // the drag target. The fill color is brighter.
            Stroke::new(2.5, style.visuals.selection.stroke.color)
        } else if self.selected {
            // TODO: Make selected only modify color and the rest width?
            Stroke::new(2.0, style.visuals.selection.bg_fill)
        } else if self.is_dragging
                || response.is_pointer_button_down_on()
                || response.has_focus() // not used?
                || response.clicked()
        {
            style.visuals.widgets.active.bg_stroke
        } else if self.is_root {
            Stroke::NONE
        } else if response.hovered() || response.highlighted() {
            //Stroke::new(1.25, style.visuals.window_stroke().color)
            style.visuals.widgets.hovered.bg_stroke
        }
        // Note the default dark theme has no bg_stroke for inactive, which makes all the
        // contained states indiscernible. Non-interactive never happens.
        // } else if !odd {
        //     style.visuals.widgets.inactive.bg_stroke
        else {
            style.visuals.window_stroke()
        };

        // ui.ctx()
        //     .debug_painter()
        //     .debug_rect(ui.min_rect(), Color32::DEBUG_COLOR, "frame");

        InnerResponse::new(response, prep.end(ui))
    }
}
