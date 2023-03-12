use std::fs::read_to_string;

use edit::Source as _;
use edit_egui as edit;
use janet::*;
use janetrs::{client::JanetClient, *};
use transit_graph::{Idx, Statechart};

#[test]
fn door() {
    // Load the edit graph.
    let edit = edit::Edit::<Source>::load("tests/door.ron").unwrap();

    // Initialize and Load the janet source.
    let client = JanetClient::init_with_default_env().unwrap();
    let source = edit.source.as_ref().unwrap();
    client.run(read_to_string(&source.path).unwrap()).unwrap();

    // Map the edit graph to Graph<JanetContext>.
    let graph = source.resolve(&edit, &client);

    let door = table! {
        ":hit-points" => table! {
            ":current" => 100.,
            ":max" => 100.,
        },
        ":key" => "the right key",
        ":attempts" => 0,
    };

    let mut door = Statechart::new(
        &graph,
        JanetContext {
            context: door.into(),
        },
    );

    // Read the id from the edit graph.
    let id = |i: Idx| &edit.graph.state(i).unwrap().id;

    assert_eq!(id(door.active), "locked");
    assert_eq!(door.transition(Event::id("open")), false); // fails since it's locked
    assert_eq!(
        door.transition(Event::id("unlock").with(Janet::from("the right key"))),
        true
    ); // unlock the door
    assert_eq!(id(door.active), "closed"); // it's now closed (unlocked)
    assert_eq!(door.transition(Event::id("open")), true); // open the door
    assert_eq!(id(door.active), "open"); // it's now opened

    assert_eq!(
        door.transition(Event::id("bash").with(Janet::from(50.0))),
        false
    ); // bash
    assert_eq!(id(door.active), "open"); // still intact (and open)
    assert_eq!(
        door.transition(Event::id("bash").with(Janet::from(50.0))),
        true
    ); // bash again
    assert_eq!(id(door.active), "destroyed"); // destroyed
}

// #[test]
// fn export() {
//     let client = JanetClient::init_with_default_env().unwrap();

//     let _ = client
//         .run(read_to_string("tests/door.janet").unwrap())
//         .unwrap();

//     let (g, _) = make_door(&client);
//     g.export_to_file("tests/export.ron").unwrap();
// }
