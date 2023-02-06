use std::{collections::HashMap, fs::read_to_string};

use janet::*;
use janetrs::{client::JanetClient, *};
use transit::{EditGraph, Graph, Idx, Statechart};

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

    g.set_root_initial(locked.into());

    g.add_transition(
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

    g.add_transition(
        locked,
        closed,
        Transition::new("unlock", "unlock-guard", &client, "the right key".into()),
    );

    // just match open
    g.add_transition(
        closed,
        open,
        Transition::new("open", "open-guard", &client, Janet::nil()),
    );

    // just match close
    g.add_transition(
        open,
        closed,
        Transition::new("close", "close-guard", &client, Janet::nil()),
    );

    g.add_transition(
        closed,
        locked,
        Transition::new("lock", "lock-guard", &client, "the right key".into()),
    );

    (g, states)
}

#[test]
fn door() {
    let client = JanetClient::init_with_default_env().unwrap();

    client
        .run(read_to_string("tests/door.janet").unwrap())
        .unwrap();

    let (graph, states) = make_door(&client);

    let door = table! {
        ":hit-points" => table! {
            ":current" => 100.,
            ":max" => 100.,
        },
        ":key" => "the right key",
        ":attempts" => 0,
    };

    let mut door = Statechart::new(
        graph,
        JanetContext {
            context: door.into(),
        },
    );

    // TODO: remove
    door.run();

    assert_eq!(states.get(&door.active).unwrap(), "locked");
    assert_eq!(door.transition(Event::id("open")), false); // fails since it's locked
    assert_eq!(
        door.transition(Event::id("unlock").with(Janet::from("the right key"))),
        true
    ); // unlock the door
    assert_eq!(states.get(&door.active).unwrap(), "closed"); // it's now closed (unlocked)
    assert_eq!(door.transition(Event::id("open")), true); // open the door
    assert_eq!(states.get(&door.active).unwrap(), "open"); // it's now opened

    assert_eq!(
        door.transition(Event::id("bash").with(Janet::from(50.0))),
        false
    ); // bash
    assert_eq!(states.get(&door.active).unwrap(), "open"); // still intact (and open)
    assert_eq!(
        door.transition(Event::id("bash").with(Janet::from(50.0))),
        true
    ); // bash again
    assert_eq!(states.get(&door.active).unwrap(), "destroyed"); // destroyed
}
