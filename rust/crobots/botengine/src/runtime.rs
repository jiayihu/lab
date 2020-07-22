use super::{Error, Kind};
use crate::game::{readlock, scanner::ScannerSystem, writelock, GameState};
use nalgebra::Point2;
use std::sync::Arc;
use wasmi::{
    Error as InterpreterError, Externals, FuncInstance, FuncRef, ModuleImportResolver, RuntimeArgs,
    RuntimeValue, Signature, Trap, ValueType,
};

const SCAN_NAME: &'static str = "scan";
const SCAN_INDEX: usize = 0;
const CANNON_NAME: &'static str = "cannon";
const CANNON_INDEX: usize = 1;
const DRIVE_NAME: &'static str = "drive";
const DRIVE_INDEX: usize = 2;
const DAMAGE_NAME: &'static str = "damage";
const DAMAGE_INDEX: usize = 3;
const SPEED_NAME: &'static str = "speed";
const SPEED_INDEX: usize = 4;
const LOCX_NAME: &'static str = "loc_x";
const LOCX_INDEX: usize = 5;
const LOCY_NAME: &'static str = "loc_y";
const LOCY_INDEX: usize = 6;
const RAND_NAME: &'static str = "rand";
const RAND_INDEX: usize = 7;
const SQRT_NAME: &'static str = "wsqrt";
const SQRT_INDEX: usize = 8;
const SIN_NAME: &'static str = "wsin";
const SIN_INDEX: usize = 9;
const COS_NAME: &'static str = "wcos";
const COS_INDEX: usize = 10;
const TAN_NAME: &'static str = "wtan";
const TAN_INDEX: usize = 11;
const ATAN_NAME: &'static str = "watan";
const ATAN_INDEX: usize = 12;
const PLOT_COURSE_NAME: &'static str = "plot_course";
const PLOT_COURSE_INDEX: usize = 13;
pub const BOTINIT_NAME: &'static str = "botinit";

fn gen_funcref(name: &str) -> Option<FuncRef> {
    match name {
        SCAN_NAME => Some(FuncInstance::alloc_host(
            Signature::new(&[ValueType::I32, ValueType::I32][..], Some(ValueType::I32)),
            SCAN_INDEX,
        )),
        CANNON_NAME => Some(FuncInstance::alloc_host(
            Signature::new(&[ValueType::I32, ValueType::I32][..], Some(ValueType::I32)),
            CANNON_INDEX,
        )),
        DRIVE_NAME => Some(FuncInstance::alloc_host(
            Signature::new(&[ValueType::I32, ValueType::I32][..], Some(ValueType::I32)),
            DRIVE_INDEX,
        )),
        DAMAGE_NAME => Some(FuncInstance::alloc_host(
            Signature::new(&[][..], Some(ValueType::I32)),
            DAMAGE_INDEX,
        )),
        SPEED_NAME => Some(FuncInstance::alloc_host(
            Signature::new(&[][..], Some(ValueType::I32)),
            SPEED_INDEX,
        )),
        LOCX_NAME => Some(FuncInstance::alloc_host(
            Signature::new(&[][..], Some(ValueType::I32)),
            LOCX_INDEX,
        )),
        LOCY_NAME => Some(FuncInstance::alloc_host(
            Signature::new(&[][..], Some(ValueType::I32)),
            LOCY_INDEX,
        )),
        RAND_NAME => Some(FuncInstance::alloc_host(
            Signature::new(&[ValueType::I32][..], Some(ValueType::I32)),
            RAND_INDEX,
        )),
        SQRT_NAME => Some(FuncInstance::alloc_host(
            Signature::new(&[ValueType::I32][..], Some(ValueType::I32)),
            SQRT_INDEX,
        )),
        SIN_NAME => Some(FuncInstance::alloc_host(
            Signature::new(&[ValueType::I32][..], Some(ValueType::I32)),
            SIN_INDEX,
        )),
        COS_NAME => Some(FuncInstance::alloc_host(
            Signature::new(&[ValueType::I32][..], Some(ValueType::I32)),
            COS_INDEX,
        )),
        TAN_NAME => Some(FuncInstance::alloc_host(
            Signature::new(&[ValueType::I32][..], Some(ValueType::I32)),
            TAN_INDEX,
        )),
        ATAN_NAME => Some(FuncInstance::alloc_host(
            Signature::new(&[ValueType::I32][..], Some(ValueType::I32)),
            ATAN_INDEX,
        )),
        PLOT_COURSE_NAME => Some(FuncInstance::alloc_host(
            Signature::new(&[ValueType::I32, ValueType::I32][..], Some(ValueType::I32)),
            PLOT_COURSE_INDEX,
        )),
        _ => None,
    }
}

pub struct RuntimeModuleImportResolver;

impl<'a> ModuleImportResolver for RuntimeModuleImportResolver {
    fn resolve_func(
        &self,
        field_name: &str,
        _signature: &Signature,
    ) -> Result<FuncRef, InterpreterError> {
        println!("Resolving {}", field_name);
        let func_ref = gen_funcref(field_name);

        match func_ref {
            Some(fr) => Ok(fr),
            None => Err(InterpreterError::Function(field_name.to_string())),
        }
    }
}

pub struct Runtime {
    pub game_state: Arc<GameState>,
    pub module_name: String,
    dead: bool,
}

impl Externals for Runtime {
    fn invoke_index(
        &mut self,
        index: usize,
        args: RuntimeArgs,
    ) -> Result<Option<RuntimeValue>, Trap> {
        match index {
            SCAN_INDEX => self.scan(args.nth(0), args.nth(1)),
            CANNON_INDEX => self.cannon(args.nth(0), args.nth(1)),
            DRIVE_INDEX => self.drive(args.nth(0), args.nth(1)),
            DAMAGE_INDEX => self.damage(),
            SPEED_INDEX => self.speed(),
            LOCX_INDEX => self.loc_x(),
            LOCY_INDEX => self.loc_y(),
            RAND_INDEX => self.rand(args.nth(0)),
            SQRT_INDEX => self.sqrt(args.nth(0)),
            SIN_INDEX => self.sin(args.nth(0)),
            COS_INDEX => self.cos(args.nth(0)),
            TAN_INDEX => self.tan(args.nth(0)),
            ATAN_INDEX => self.atan(args.nth(0)),
            PLOT_COURSE_INDEX => self.plot_course(args.nth(0), args.nth(1)),

            _ => Err(Trap::from(Error {
                kind: Kind::MiscFailure("Invalid export index".to_string()),
            })),
        }
    }
}

type WasmRuntimeResult = Result<Option<RuntimeValue>, Trap>;

impl Runtime {
    pub fn init(game_state: Arc<GameState>, module_name: String) -> Runtime {
        game_state.combatant_entered(&module_name);

        Runtime {
            game_state,
            module_name,
            dead: false,
        }
    }

    fn is_dead(&mut self) -> bool {
        if !self.dead {
            let dcs = self.game_state.damage_components.read().unwrap();
            let dc = dcs.get(&self.module_name);

            match dc {
                Some(d) => {
                    if let crate::game::damage::DamageStatus::Dead = d.status {
                        self.dead = true;
                    }
                }
                None => {}
            }
        }

        self.dead
    }

    fn scan(&mut self, angle: i32, resolution: i32) -> WasmRuntimeResult {
        if self.is_dead() {
            return Ok(Some(RuntimeValue::from(-1)));
        }

        let angle = ScannerSystem::to_real_heading(angle);
        let resolution = (resolution as f32)
            .max(0.0)
            .min(super::game::scanner::RES_LIMIT);

        let degree = angle as f32;

        writelock(&self.game_state.scanner_components)
            .entry(self.module_name.to_string())
            .and_modify(|sc| sc.angle = degree as i32);

        let scan_result: i32 =
            ScannerSystem::scan(&self.game_state, &self.module_name, degree, resolution);

        Ok(Some(RuntimeValue::from(ScannerSystem::to_user_heading(
            scan_result as f32,
        ))))
    }

    fn cannon(&mut self, angle: i32, range: i32) -> WasmRuntimeResult {
        if self.is_dead() {
            return Ok(Some(RuntimeValue::from(0)));
        }

        let angle = ScannerSystem::to_real_heading(angle);
        let mut launch_result = 0;
        let mc = &self.game_state.motion_components.read().unwrap()[&self.module_name];

        writelock(&self.game_state.projectile_components)
            .entry(self.module_name.to_string())
            .and_modify(|pc| launch_result = pc.launch(&mc.position, angle, range as u32));

        Ok(Some(RuntimeValue::from(launch_result)))
    }

    fn drive(&mut self, angle: i32, speed: i32) -> WasmRuntimeResult {
        if self.is_dead() {
            return Ok(Some(RuntimeValue::from(0)));
        }

        let angle = ScannerSystem::to_real_heading(angle);
        let speed = speed.min(super::game::motion::MAX_ENGINE);

        self.game_state
            .motion_components
            .write()
            .unwrap()
            .entry(self.module_name.to_string())
            .and_modify(|mc| {
                mc.origin = mc.position.clone();
                mc.distance_along_heading = 0;
                mc.heading = angle;
                mc.desidered_speed = speed;
            });

        Ok(Some(RuntimeValue::from(1_i32)))
    }

    fn damage(&mut self) -> WasmRuntimeResult {
        if self.is_dead() {
            return Ok(Some(RuntimeValue::from(100)));
        }

        Ok(
            match readlock(&self.game_state.damage_components).get(&self.module_name) {
                Some(dc) => Some(RuntimeValue::from(dc.damage)),
                None => None,
            },
        )
    }

    fn plot_course(&mut self, tx: i32, ty: i32) -> WasmRuntimeResult {
        if self.is_dead() {
            return Ok(Some(RuntimeValue::from(-1)));
        }

        Ok(
            match readlock(&self.game_state.motion_components).get(&self.module_name) {
                Some(mc) => {
                    let h = ScannerSystem::heading_to_target(
                        &mc.position,
                        &Point2::new(tx as f32, ty as f32),
                    );
                    Some(RuntimeValue::from(ScannerSystem::to_user_heading(h)))
                }
                None => None,
            },
        )
    }

    fn speed(&mut self) -> WasmRuntimeResult {
        if self.is_dead() {
            return Ok(Some(RuntimeValue::from(0)));
        }

        Ok(
            match readlock(&self.game_state.motion_components).get(&self.module_name) {
                Some(mc) => Some(RuntimeValue::from(mc.speed)),
                None => None,
            },
        )
    }

    fn loc_x(&mut self) -> WasmRuntimeResult {
        Ok(
            match readlock(&self.game_state.motion_components).get(&self.module_name) {
                Some(mc) => Some(RuntimeValue::from(mc.position.x as i32)),
                None => None,
            },
        )
    }

    fn loc_y(&mut self) -> WasmRuntimeResult {
        Ok(
            match readlock(&self.game_state.motion_components).get(&self.module_name) {
                Some(mc) => Some(RuntimeValue::from(mc.position.y as i32)),
                None => None,
            },
        )
    }

    fn rand(&mut self, limit: i32) -> WasmRuntimeResult {
        use rand::Rng;
        let mut rng = rand::thread_rng();
        let n: i32 = rng.gen_range(0, limit);

        Ok(Some(RuntimeValue::from(n)))
    }

    fn sqrt(&mut self, number: i32) -> WasmRuntimeResult {
        let val = (number as f32).sqrt();
        Ok(Some(RuntimeValue::from(val as i32)))
    }

    fn sin(&mut self, degree: i32) -> WasmRuntimeResult {
        Ok(Some(RuntimeValue::from(ScannerSystem::to_user_heading(
            (degree as f32).to_radians().sin(),
        ))))
    }

    fn cos(&mut self, degree: i32) -> WasmRuntimeResult {
        Ok(Some(RuntimeValue::from(ScannerSystem::to_user_heading(
            (degree as f32).to_radians().cos(),
        ))))
    }

    fn tan(&mut self, degree: i32) -> WasmRuntimeResult {
        Ok(Some(RuntimeValue::from(ScannerSystem::to_user_heading(
            (degree as f32).to_radians().tan(),
        ))))
    }

    fn atan(&mut self, degree: i32) -> WasmRuntimeResult {
        Ok(Some(RuntimeValue::from(ScannerSystem::to_user_heading(
            (degree as f32).to_radians().atan(),
        ))))
    }
}
