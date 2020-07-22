mod events;
mod game;
mod runtime;

use std::fmt;
use std::sync::Arc;
use std::thread;
use std::thread::JoinHandle;
use wasmi::{HostError, ImportsBuilder, Module, ModuleInstance, ModuleRef};

pub use game::{GameState, Gameloop};
pub use runtime::{Runtime, BOTINIT_NAME};

#[derive(Debug)]
pub enum Kind {
    InterpreterError(wasmi::Error),
    IoError(std::io::Error),
    ExportResolve(String),
    MiscFailure(String),
}

#[derive(Debug)]
pub struct Error {
    kind: Kind,
}

impl HostError for Error {}

// Implement standard error trait for the botengine error
impl std::error::Error for Error {
    fn description(&self) -> &str {
        "A botengine error occurred"
    }

    fn cause(&self) -> Option<&dyn std::error::Error> {
        None
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.kind {
            Kind::InterpreterError(ref wasm_error) => fmt::Display::fmt(wasm_error, f),
            Kind::MiscFailure(ref e) => fmt::Display::fmt(e, f),
            Kind::IoError(ref e) => fmt::Display::fmt(e, f),
            Kind::ExportResolve(ref e) => fmt::Display::fmt(e, f),
        }
    }
}

// Creates a botengine error from an I/O Error
impl From<std::io::Error> for Error {
    fn from(source: std::io::Error) -> Error {
        Error {
            kind: Kind::IoError(source),
        }
    }
}

impl From<wasmi::Error> for Error {
    fn from(source: wasmi::Error) -> Error {
        Error {
            kind: Kind::InterpreterError(source),
        }
    }
}

type Result<T> = std::result::Result<T, Error>;

pub struct Combatant {}

impl Combatant {
    pub fn buffer_from_file(path: &str) -> Result<Vec<u8>> {
        use std::fs::File;
        use std::io::prelude::*;

        let mut file = File::open(path)?;
        let mut wasm_buf = Vec::new();
        let _bytes_read = file.read_to_end(&mut wasm_buf)?;

        Ok(wasm_buf)
    }

    pub fn start(name: &str, buffer: Vec<u8>, game_state: Arc<GameState>) -> JoinHandle<()> {
        let n = name.to_string();

        thread::spawn(move || {
            let module = Module::from_buffer(&buffer).unwrap();
            let mut runtime = runtime::Runtime::init(game_state, n.clone());
            let moduleref = Self::get_module_instance_from_module(&module).unwrap();
            let res = moduleref.invoke_export(BOTINIT_NAME, &[][..], &mut runtime);
            println!("Bot init loop exited for player {} - {:?}", n, res);
        })
    }

    fn get_module_instance_from_module(module: &Module) -> Result<ModuleRef> {
        let mut imports = ImportsBuilder::new();
        imports.push_resolver("env", &runtime::RuntimeModuleImportResolver);
        let instance = ModuleInstance::new(module, &imports)
            .expect("Failed to instantiate module")
            .assert_no_start();

        Ok(instance)
    }
}
