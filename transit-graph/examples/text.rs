///! this is a door
mod door;

use door::*;
use rustyline::error::ReadlineError;
use rustyline::{DefaultEditor, Result};
use transit_graph::Statechart;

fn main() -> Result<()> {
    let graph = make_graph();
    let mut ctx = DoorContext;
    let mut door = Statechart::new(Door::default(), &mut ctx, &graph);

    // TODO: start over make reset work and fix history reset
    println!("What would you like to do? (o)pen (c)lose (l)ock (u)nlock (b)ash, or maybe (s)tart over or (q)uit");

    let mut rl = DefaultEditor::new()?;

    loop {
        //println!("the door is: {}", door.active());
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => match line.trim_start().chars().next() {
                Some(c) => {
                    let event = match c {
                        'o' => Some(DoorEvent::Open),
                        'c' => Some(DoorEvent::Close),
                        'l' => Some(DoorEvent::Lock(Some("the right key".to_string()))),
                        'u' => Some(DoorEvent::Unlock(Some("the right key".to_string()))),
                        'b' => Some(DoorEvent::Bash(Attack { damage: 40. })),
                        's' => {
                            door.reset(Door::default(), &mut ctx, &graph);
                            continue;
                        }
                        'q' => break,
                        _ => {
                            println!("Try again.");
                            None
                        }
                    };
                    if let Some(event) = event {
                        let res = door.transition(&mut ctx, &graph, event);
                        if !res {
                            //dbg!(_e);
                            println!(
                                "That didn't work. The door is {}.",
                                graph.state(door.active).unwrap()
                            );
                        }
                    }
                }
                _ => println!("Excuse me?"),
            },
            Err(ReadlineError::Interrupted) => {
                break;
            }
            Err(ReadlineError::Eof) => {
                break;
            }
            Err(e) => println!("Error: {:?}", e),
        }
    }

    Ok(())
}
