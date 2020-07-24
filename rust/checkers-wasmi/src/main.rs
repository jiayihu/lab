mod checkersgame;
mod imports;
mod runtime;

use checkersgame::CheckersGame;
use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    let mut game = CheckersGame::new("../wasm/checkers.wasm");
    game.init()?;

    let board_display = game.get_board_contents()?;
    println!("game board at start:\n{}\n", board_display);

    println!(
        "At game start, current turn is : {:?}",
        game.get_turn_owner()?
    );
    game.move_piece(&(0, 2), &(1, 3))?;
    println!(
        "After first move, current turn is : {:?}",
        game.get_turn_owner()?
    );

    let board_display = game.get_board_contents()?;
    println!("game board after 1 move:\n{}\n", board_display);

    Ok(())
}
