mod door;

use door::*;
///! this is a door
use rustyline::error::ReadlineError;
use transit_graph::Statechart;

fn main() {
    let door = Door {
        hit_points: HitPoints {
            current: 100.,
            max: 100.,
        },
        key: "the right key".to_string(),
        attempts: 0,
    };

    let graph = make_graph();

    let mut door = Statechart::new(&graph, door);

    // this does nothing
    door.run();

    // TODO: start over make reset work and fix history reset
    println!("What would you like to do? (o)pen (c)lose (l)ock (u)nlock (b)ash, or maybe (s)tart over or (q)uit");

    let mut rl = rustyline::Editor::<()>::new();

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
                        'q' => break,
                        _ => {
                            println!("Try again.");
                            None
                        }
                    };
                    if let Some(event) = event {
                        let res = door.transition(event);
                        if !res {
                            //dbg!(_e);
                            println!(
                                "That didn't work. The door is {}.",
                                door.graph.state(door.active).unwrap()
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
}
