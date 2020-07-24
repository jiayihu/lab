use super::runtime::Runtime;
use std::error::Error;
use wasmtime::{Func, Instance, Memory, Module, Store};

pub struct CheckersGame {
    instance: Instance,
}

#[derive(Debug)]
pub enum PieceColor {
    White,
    Black,
}

type Result<T> = ::std::result::Result<T, Box<dyn Error>>;
type Coordinate = (i32, i32);

fn load_instance(module_file: &str) -> Result<Instance> {
    let store = Store::default();
    let module = Module::from_file(store.engine(), module_file)?;

    let piece_moved_func = Func::wrap(&store, |from_x: i32, from_y: i32, to_x: i32, to_y: i32| {
        Runtime::handle_piece_moved((from_x, from_y), (to_x, to_y));
    });
    let piece_crowned_func = Func::wrap(&store, |loc_x: i32, loc_y: i32| {
        Runtime::handle_piece_crowned((loc_x, loc_y));
    });
    let imports = [piece_moved_func.into(), piece_crowned_func.into()];

    let instance = Instance::new(&store, &module, &imports)?;

    Ok(instance)
}

impl CheckersGame {
    pub fn new(module_file: &str) -> CheckersGame {
        let instance = load_instance(module_file).unwrap();

        CheckersGame { instance }
    }

    pub fn init(&self) -> Result<()> {
        let init = self
            .instance
            .get_func("initBoard")
            .expect("Failed to find the exported init function")
            .get0::<()>()?;

        init()?;

        Ok(())
    }

    pub fn move_piece(&self, from: &Coordinate, to: &Coordinate) -> Result<bool> {
        let movefn = self
            .instance
            .get_func("move")
            .expect("Failed to find the exported move function")
            .get4::<i32, i32, i32, i32, i32>()?;

        let res = movefn(from.0, from.1, to.0, to.1)?;

        if res == 0 {
            Ok(false)
        } else {
            Ok(true)
        }
    }

    pub fn get_turn_owner(&self) -> Result<PieceColor> {
        let get_turn_over_fn = self
            .instance
            .get_func("getTurnOwner")
            .expect("Failed to find the exported getTurnOwner function")
            .get0::<i32>()?;

        let res = get_turn_over_fn()?;

        if res == 0 {
            Ok(PieceColor::White)
        } else {
            Ok(PieceColor::Black)
        }
    }

    pub fn get_board_contents(&self) -> Result<String> {
        let memory = self
            .instance
            .get_memory("memory")
            .expect("Failed to find memory export");
        let header = r#"
    0   1   2   3   4   5   6   7
  .---.---.---.---.---.---.---.---."#;
        let footer = "  `---^---^---^---^---^---^---^---^";

        let middle_string = gen_board(&memory);

        Ok(format!("{}\n{}{}\n", header, middle_string, footer))
    }
}

fn gen_board(memory: &Memory) -> String {
    let mut vals = Vec::<String>::new();

    for y in 0..8 {
        vals.push(format!("{} ", y));
        for x in 0..8 {
            unsafe {
                let offset = calc_offset(x, y);
                let bytevec: &[u8] = &memory.data_unchecked()[offset..offset + 0x4];
                let value = to_u32(bytevec);

                vals.push(format!("|{}", value_label(value)));
            }
        }
        vals.push("|\n".into())
    }

    vals.join("")
}

fn value_label(v: u32) -> String {
    match v {
        0 => "   ",
        1 => " B ",
        2 => " W ",
        5 => " B*",
        6 => " W*",
        _ => "???",
    }
    .into()
}

fn to_u32(bytes: &[u8]) -> u32 {
    bytes.iter().rev().fold(0, |acc, &b| acc * 2 + b as u32)
}

fn calc_offset(x: usize, y: usize) -> usize {
    (x + y * 8) * 4
}
