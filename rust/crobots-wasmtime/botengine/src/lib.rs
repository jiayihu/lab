mod events;
mod game;
mod runtime;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::Arc;
use std::thread;
use std::thread::JoinHandle;
use wasmtime::{Engine, Func, Instance, Module, Store};

pub use game::{GameState, Gameloop};
pub use runtime::{Runtime, BOTINIT_NAME};

pub struct Combatant {
    pub module: Module,
}

impl Combatant {
    pub fn get_engine() -> Engine {
        Engine::default()
    }

    pub fn new(engine: &Engine, path: &str) -> Combatant {
        let module = Module::from_file(engine, path).unwrap();

        Combatant { module }
    }

    pub fn start(
        name: &str,
        engine: Arc<Engine>,
        module: Module,
        game_state: Arc<GameState>,
    ) -> JoinHandle<()> {
        let n = name.to_string();

        thread::spawn(move || {
            let store = Store::new(&engine);
            let runtime = Rc::new(RefCell::new(runtime::Runtime::init(game_state, n.clone())));

            let mut imports_by_name = Self::build_imports(&store, runtime);
            let mut imports = Vec::with_capacity(module.imports().len());

            for imp in module.imports() {
                imports.push(
                    imports_by_name
                        .remove(imp.name())
                        .expect(&format!("Unknown import {}", imp.name()))
                        .into(),
                );
            }

            let instance = Instance::new(&store, &module, &imports[..]).unwrap();

            let init = instance
                .get_func(BOTINIT_NAME)
                .unwrap()
                .get0::<i32>()
                .unwrap();

            println!("Init");
            init().unwrap();
        })
    }

    fn build_imports(store: &Store, runtime: Rc<RefCell<Runtime>>) -> HashMap<String, Func> {
        let r1 = Rc::clone(&runtime);
        let r2 = Rc::clone(&runtime);
        let r3 = Rc::clone(&runtime);
        let r4 = Rc::clone(&runtime);
        let r5 = Rc::clone(&runtime);
        let r6 = Rc::clone(&runtime);
        let r7 = Rc::clone(&runtime);
        let r8 = Rc::clone(&runtime);
        let r9 = Rc::clone(&runtime);
        let r10 = Rc::clone(&runtime);
        let r11 = Rc::clone(&runtime);
        let r12 = Rc::clone(&runtime);
        let r13 = Rc::clone(&runtime);
        let r14 = Rc::clone(&runtime);

        let mut map = HashMap::new();
        map.insert(
            "scan".to_string(),
            Func::wrap(&store, move |a: i32, b: i32| r1.borrow_mut().scan(a, b)),
        );
        map.insert(
            "cannon".to_string(),
            Func::wrap(&store, move |a: i32, b: i32| r2.borrow_mut().cannon(a, b)),
        );
        map.insert(
            "drive".to_string(),
            Func::wrap(&store, move |a: i32, b: i32| r3.borrow_mut().drive(a, b)),
        );
        map.insert(
            "plot_course".to_string(),
            Func::wrap(&store, move |a: i32, b: i32| {
                r4.borrow_mut().plot_course(a, b)
            }),
        );
        map.insert(
            "damage".to_string(),
            Func::wrap(&store, move || r5.borrow_mut().damage()),
        );
        map.insert(
            "speed".to_string(),
            Func::wrap(&store, move || r6.borrow_mut().speed()),
        );
        map.insert(
            "loc_x".to_string(),
            Func::wrap(&store, move || r7.borrow_mut().loc_x()),
        );
        map.insert(
            "loc_y".to_string(),
            Func::wrap(&store, move || r8.borrow_mut().loc_y()),
        );
        map.insert(
            "rand".to_string(),
            Func::wrap(&store, move |a: i32| r9.borrow_mut().rand(a)),
        );
        map.insert(
            "sqrt".to_string(),
            Func::wrap(&store, move |a: i32| r10.borrow_mut().sqrt(a)),
        );
        map.insert(
            "sin".to_string(),
            Func::wrap(&store, move |a: i32| r11.borrow_mut().sin(a)),
        );
        map.insert(
            "cos".to_string(),
            Func::wrap(&store, move |a: i32| r12.borrow_mut().cos(a)),
        );
        map.insert(
            "tan".to_string(),
            Func::wrap(&store, move |a: i32| r13.borrow_mut().tan(a)),
        );
        map.insert(
            "atan".to_string(),
            Func::wrap(&store, move |a: i32| r14.borrow_mut().atan(a)),
        );

        map
    }
}
