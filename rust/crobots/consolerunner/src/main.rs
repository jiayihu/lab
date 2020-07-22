use botengine::{Combatant, Gameloop};
use std::sync::{mpsc::channel, Arc};
use std::thread;

fn main() {
    let game_state = Arc::new(botengine::GameState::new());

    let buffer1 = Combatant::buffer_from_file("./bots/dumbotrs.wasm");
    let bot1 = buffer1.unwrap();

    let buffer2 = Combatant::buffer_from_file("./bots/rook.wasm");
    let bot2 = buffer2.unwrap();

    let buffer3 = Combatant::buffer_from_file("./bots/rabbit.wasm");
    let bot3 = buffer3.unwrap();

    let my_gs = game_state.clone();
    let debug_gs = game_state.clone();

    let (sender, receiver) = channel();
    thread::spawn(move || loop {
        match receiver.recv() {
            Ok(ge) => println!("{:?}", ge),
            Err(_) => {}
        }
    });

    Combatant::start("bot-1", bot1, game_state.clone());
    Combatant::start("rook", bot2, game_state.clone());
    Combatant::start("rabbit", bot3, game_state.clone());

    let handle = thread::spawn(move || {
        let mut game_loop = Gameloop::new(my_gs, 100_000, 3, Some(sender));
        let game_result = game_loop.start();

        println!(
            "Game loop terminated: {:?}\nState: {:?}",
            game_result, debug_gs
        );
    });

    handle.join().unwrap();
}
