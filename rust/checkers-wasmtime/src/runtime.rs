pub struct Runtime {}

impl Runtime {
    pub fn handle_piece_moved(from: (i32, i32), to: (i32, i32)) {
        println!(
            "A piece was moved from ({}, {}) to ({}, {})",
            from.0, from.1, to.0, to.1
        );
    }

    pub fn handle_piece_crowned(loc: (i32, i32)) {
        println!("A piece was crowned at ({}, {})", loc.0, loc.1);
    }
}
