use crate::game::{readlock, scanner::ScannerSystem, writelock, GameState};
use nalgebra::Point2;
use std::sync::Arc;

pub const BOTINIT_NAME: &'static str = "botinit";

pub struct Runtime {
    pub game_state: Arc<GameState>,
    pub module_name: String,
    dead: bool,
}

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

    pub fn scan(&mut self, angle: i32, resolution: i32) -> i32 {
        if self.is_dead() {
            return -1;
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

        ScannerSystem::to_user_heading(scan_result as f32)
    }

    pub fn cannon(&mut self, angle: i32, range: i32) -> i32 {
        if self.is_dead() {
            return 0;
        }

        let angle = ScannerSystem::to_real_heading(angle);
        let mut launch_result = 0;

        writelock(&self.game_state.projectile_components)
            .entry(self.module_name.to_string())
            .and_modify(|pc| {
                let mc = &self.game_state.motion_components.read().unwrap()[&self.module_name];
                launch_result = pc.launch(&mc.position, angle, range as u32);
            });

        launch_result
    }

    pub fn drive(&mut self, angle: i32, speed: i32) -> i32 {
        if self.is_dead() {
            return 0;
        }

        let angle = ScannerSystem::to_real_heading(angle);
        let speed = speed.min(super::game::motion::MAX_ENGINE);

        writelock(&self.game_state.motion_components)
            .entry(self.module_name.to_string())
            .and_modify(|mc| {
                mc.origin = mc.position.clone();
                mc.distance_along_heading = 0;
                mc.heading = angle;
                mc.desidered_speed = speed;
            });

        1
    }

    pub fn damage(&mut self) -> u32 {
        if self.is_dead() {
            return 100;
        }

        match readlock(&self.game_state.damage_components).get(&self.module_name) {
            Some(dc) => dc.damage,
            None => 100,
        }
    }

    pub fn plot_course(&mut self, tx: i32, ty: i32) -> i32 {
        if self.is_dead() {
            return -1;
        }

        match readlock(&self.game_state.motion_components).get(&self.module_name) {
            Some(mc) => {
                let h = ScannerSystem::heading_to_target(
                    &mc.position,
                    &Point2::new(tx as f32, ty as f32),
                );
                ScannerSystem::to_user_heading(h)
            }
            None => -1,
        }
    }

    pub fn speed(&mut self) -> i32 {
        if self.is_dead() {
            return 0;
        }

        match readlock(&self.game_state.motion_components).get(&self.module_name) {
            Some(mc) => mc.speed,
            None => 0,
        }
    }

    pub fn loc_x(&mut self) -> i32 {
        match readlock(&self.game_state.motion_components).get(&self.module_name) {
            Some(mc) => mc.position.x as i32,
            None => 0,
        }
    }

    pub fn loc_y(&mut self) -> i32 {
        match readlock(&self.game_state.motion_components).get(&self.module_name) {
            Some(mc) => mc.position.y as i32,
            None => 0,
        }
    }

    pub fn rand(&mut self, limit: i32) -> i32 {
        use rand::Rng;
        let mut rng = rand::thread_rng();
        let n: i32 = rng.gen_range(0, limit);

        n
    }

    pub fn sqrt(&mut self, number: i32) -> i32 {
        let val = (number as f32).sqrt();
        val as i32
    }

    pub fn sin(&mut self, degree: i32) -> i32 {
        ScannerSystem::to_user_heading((degree as f32).to_radians().sin())
    }

    pub fn cos(&mut self, degree: i32) -> i32 {
        ScannerSystem::to_user_heading((degree as f32).to_radians().cos())
    }

    pub fn tan(&mut self, degree: i32) -> i32 {
        ScannerSystem::to_user_heading((degree as f32).to_radians().tan())
    }

    pub fn atan(&mut self, degree: i32) -> i32 {
        ScannerSystem::to_user_heading((degree as f32).to_radians().atan())
    }
}
