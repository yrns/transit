use crate::*;

pub struct Frame {
    is_root: bool,
    depth: usize,
    selected: bool,
    is_target: bool,
}

pub fn state_frame(is_root: bool, depth: usize, selected: bool, is_target: bool) -> Frame {
    Frame {
        is_root,
        depth,
        selected,
        is_target,
    }
}

impl Frame {
    // We always return the inner response and drop the frame's.
    pub fn show(&self, ui: &mut Ui, add_contents: impl FnOnce(&mut Ui) -> Response) -> Response {
        // Always? Check root:
        let frame = if self.is_root {
            egui::Frame::none() //.fill(ui.style().visuals.window_fill()) // too flat
        } else {
            egui::Frame::window(&ui.style()).inner_margin(Margin::ZERO)
        };

        // Can't change the margins after this. Frame is Copy and gets copied here.
        let mut prep = frame.begin(ui);
        let frame = &mut prep.frame;
        let response = add_contents(&mut prep.content_ui);

        let style = ui.style();

        // TODO: Make a background for the root so we can show drag_valid.

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

        // TODO: once selected, there is no other state

        if self.is_target {
            // The default stroke for selection is thin, so make it more prominent here to show
            // the drag target. The fill color is brighter.
            frame.stroke = Stroke::new(3.0, style.visuals.selection.stroke.color);
        } else if !self.is_root {
            // Use the fg_stroke (or window_stroke()) since the default dark theme has no bg_stroke
            // for inactive, which makes all the contained states indiscernible. Non-interactive
            // never happens.
            frame.stroke = if self.selected {
                Stroke::new(2.0, style.visuals.selection.bg_fill)
            } else if
            //response.dragged()
            // dragged does not work since the widget id changes after one frame... check idx FIX
            response.is_pointer_button_down_on()
                || response.has_focus()
                || response.clicked()
            {
                style.visuals.widgets.active.bg_stroke
            } else if response.hovered() || response.highlighted() {
                //Stroke::new(1.25, style.visuals.window_stroke().color)
                style.visuals.widgets.hovered.bg_stroke
            // } else if !odd {
            //     style.visuals.widgets.inactive.bg_stroke
            } else {
                style.visuals.window_stroke()
            };
        }

        _ = prep.end(ui);
        response
    }
}
