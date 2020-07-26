use botengine::{Combatant, Gameloop};
use std::sync::{mpsc::channel, Arc};
use std::thread;

fn main() {
    let engine = Arc::new(Combatant::get_engine());
    let game_state = Arc::new(botengine::GameState::new());

    let bot1 = Combatant::new(&engine, "./bots/dumbotrs.wasm");
    let bot2 = Combatant::new(&engine, "./bots/rook.wasm");
    let bot3 = Combatant::new(&engine, "./bots/rabbit.wasm");

    let my_gs = game_state.clone();
    let debug_gs = game_state.clone();

    let (sender, receiver) = channel();
    thread::spawn(move || loop {
        match receiver.recv() {
            Ok(ge) => println!("{:?}", ge),
            Err(_) => {}
        }
    });

    Combatant::start("bot-1", engine.clone(), bot1.module, game_state.clone());
    Combatant::start("rook", engine.clone(), bot2.module, game_state.clone());
    Combatant::start("rabbit", engine.clone(), bot3.module, game_state.clone());

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
