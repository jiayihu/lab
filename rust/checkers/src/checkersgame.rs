use super::imports::RuntimeModuleImportResolver;
use super::runtime::Runtime;
use std::error::Error;
use std::fs::File;
use std::io::prelude::*;
use wasmi::{
    ExternVal, ImportsBuilder, MemoryRef, ModuleImportResolver, ModuleInstance, ModuleRef,
    RuntimeValue,
};

pub struct CheckersGame {
    runtime: Runtime,
    module_instance: ModuleRef,
}

#[derive(Debug)]
pub enum PieceColor {
    White,
    Black,
}

type Result<T> = ::std::result::Result<T, Box<dyn Error>>;
type Coordinate = (i32, i32);

fn load_instance(
    import_resolver: &impl ModuleImportResolver,
    module_file: &str,
) -> Result<ModuleRef> {
    let mut buffer = Vec::new();
    {
        let mut f = File::open(module_file)?;
        f.read_to_end(&mut buffer)?;
    }
    let module = wasmi::Module::from_buffer(buffer)?;
    let mut builder = ImportsBuilder::new();
    builder.push_resolver("events", import_resolver);

    let instance = ModuleInstance::new(&module, &builder)
        .expect("Failed to instantiate WASM module")
        .assert_no_start();

    Ok(instance)
}

impl CheckersGame {
    pub fn new(module_file: &str) -> CheckersGame {
        let resolver = RuntimeModuleImportResolver::new();

        let instance = load_instance(&resolver, module_file).unwrap();
        let runtime = Runtime::new();

        CheckersGame {
            module_instance: instance,
            runtime,
        }
    }

    pub fn init(&mut self) -> Result<()> {
        self.module_instance
            .invoke_export("initBoard", &[], &mut self.runtime)?;

        Ok(())
    }

    pub fn move_piece(&mut self, from: &Coordinate, to: &Coordinate) -> Result<bool> {
        let res = self.module_instance.invoke_export(
            "move",
            &[
                RuntimeValue::from(from.0),
                RuntimeValue::from(from.1),
                RuntimeValue::from(to.0),
                RuntimeValue::from(to.1),
            ],
            &mut self.runtime,
        )?;

        match res {
            Some(RuntimeValue::I32(v)) => Ok(v != 0),
            _ => {
                println!("Did not get an appropriate response from move.");
                Ok(false)
            }
        }
    }

    pub fn get_turn_owner(&mut self) -> Result<PieceColor> {
        let res = self
            .module_instance
            .invoke_export("getTurnOwner", &[], &mut self.runtime)?;

        match res {
            Some(RuntimeValue::I32(v)) => {
                if v == 1 {
                    Ok(PieceColor::Black)
                } else {
                    Ok(PieceColor::White)
                }
            }
            _ => Err(From::from("Bad invocation")),
        }
    }

    pub fn get_board_contents(&mut self) -> Result<String> {
        let export = self.module_instance.export_by_name("memory");
        let header = r#"
    0   1   2   3   4   5   6   7
  .---.---.---.---.---.---.---.---."#;
        let footer = "  `---^---^---^---^---^---^---^---^";

        let middle_string = match export {
            Some(ExternVal::Memory(mr)) => gen_board(&mr),
            _ => " -- no board data found -- ".to_string(),
        };

        Ok(format!("{}\n{}{}\n", header, middle_string, footer))
    }
}

fn gen_board(memory: &MemoryRef) -> String {
    let mut vals = Vec::<String>::new();

    for y in 0..8 {
        vals.push(format!("{} ", y));
        for x in 0..8 {
            let offset = calc_offset(x, y);
            let bytevec: Vec<u8> = memory.get(offset, 4).unwrap();
            let value = to_u32(&bytevec[..]);

            vals.push(format!("|{}", value_label(value)));
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

fn calc_offset(x: usize, y: usize) -> u32 {
    ((x + y * 8) * 4) as u32
}
