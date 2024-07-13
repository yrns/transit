///! Text-based example of a door as a statechart.
mod door;

use inquire::{error::InquireError, Select};
use transit::Statechart;

use door::*;

fn main() -> Result<(), InquireError> {
    let graph = make_graph();
    let mut ctx = DoorContext;
    let mut door = Statechart::new(Door::default(), &mut ctx, &graph);

    let options = vec![
        "Open",
        "Close",
        "Lock",
        "Unlock",
        "Bash",
        // This is to test history. The door will be restored to its previous state from the
        // destroyed state.
        "Cast a restoration spell",
        "Start over",
        "Quit",
    ];

    let select = Select::new("What would you like to do?", options);

    let select_key = || {
        Select::new("Which key?", vec!["brass key", "silver key"])
            .prompt()
            .ok()
            .map(|key| key.to_owned())
    };

    loop {
        let event = match select.clone().prompt()? {
            "Open" => DoorEvent::Open,
            "Close" => DoorEvent::Close,
            "Lock" => DoorEvent::Lock(select_key()),
            "Unlock" => DoorEvent::Unlock(select_key()),
            "Bash" => DoorEvent::Bash(Attack { damage: 40. }),
            "Cast a restoration spell" => DoorEvent::Restore,
            "Start over" => {
                door.reset(Door::default(), &mut ctx, &graph);
                continue;
            }
            // "Quit" => break,
            _ => break,
        };

        // Generic failure response:
        if !door.transition(&mut ctx, &graph, event) {
            println!(
                //"That didn't work."
                "The door is {}.",
                graph.state(door.active).unwrap()
            );
        }
    }

    Ok(())
}
