use std::{collections::HashMap, fs::read_to_string};

use edit::Source as _;
use edit_egui as edit;
use janet::*;
use janetrs::{client::JanetClient, *};
use transit_graph::{Graph, Idx, Initial, Statechart};

fn make_door(client: &JanetClient) -> (Graph<State, Transition>, HashMap<Idx, String>) {
    let mut g = Graph::new();

    // Track state indices and names for testing. An alternative would be to put the state name in
    // state local data.
    let mut states = HashMap::new();

    let intact = g.add_state(
        State {
            enter: resolve("intact-enter", &client),
            ..Default::default()
        },
        None,
    );
    states.insert(intact, "intact".into());

    let locked = g.add_state(
        State {
            enter: resolve("locked-enter", &client),
            ..Default::default()
        },
        Some(intact),
    );
    states.insert(locked, "locked".into());

    let closed = g.add_state(
        State {
            enter: resolve("closed-enter", &client),
            ..Default::default()
        },
        Some(intact),
    );
    states.insert(closed, "closed".into());

    let open = g.add_state(
        State {
            enter: resolve("open-enter", &client),
            ..Default::default()
        },
        Some(intact),
    );
    states.insert(open, "open".into());

    let destroyed = g.add_state(
        State {
            enter: resolve("destroyed-enter", &client),
            ..Default::default()
        },
        None,
    );
    states.insert(destroyed, "destroyed".into());

    let _op = g.set_root_initial((Initial::Initial, locked));

    let _t = g.add_transition(
        intact,
        destroyed,
        Transition::new("bash", "bash-guard", &client, Janet::nil()),
    );

    // add_internal?
    // let bash = g.add_transition(
    //     intact,
    //     intact,
    //     Transition::new("bash", "bash-internal", &client, Janet::nil()),
    // );
    // g.set_internal(bash, true);

    let _t = g.add_transition(
        locked,
        closed,
        Transition::new("unlock", "unlock-guard", &client, "the right key".into()),
    );

    // just match open
    let _t = g.add_transition(
        closed,
        open,
        Transition::new("open", "open-guard", &client, Janet::nil()),
    );

    // just match close
    let _t = g.add_transition(
        open,
        closed,
        Transition::new("close", "close-guard", &client, Janet::nil()),
    );

    let _t = g.add_transition(
        closed,
        locked,
        Transition::new("lock", "lock-guard", &client, "the right key".into()),
    );

    (g, states)
}

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

#[test]
fn export() {
    let client = JanetClient::init_with_default_env().unwrap();

    let _ = client
        .run(read_to_string("tests/door.janet").unwrap())
        .unwrap();

    let (g, _) = make_door(&client);
    g.export_to_file("tests/export.ron").unwrap();
}
