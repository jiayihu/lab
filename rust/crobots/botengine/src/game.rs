pub mod damage;
pub mod motion;
pub mod projectile;
pub mod scanner;

use crate::events::{log_event, GameEvent};
use damage::*;
use motion::*;
use projectile::*;
use scanner::*;
use std::collections::HashMap;
use std::sync::{mpsc::Sender, Arc, RwLock};
use std::sync::{RwLockReadGuard, RwLockWriteGuard};

const MAX_X: f32 = 1000.0;
const MAX_Y: f32 = 1000.0;

pub type ReadWriteLocked<T> = Arc<RwLock<T>>;
pub type ComponentHash<T> = ReadWriteLocked<HashMap<String, T>>;

pub trait System {
    fn apply(self: &Self, cycle: u32, game_state: &Arc<GameState>);
}

#[derive(Debug)]
pub struct GameState {
    pub players: ReadWriteLocked<Vec<String>>,
    pub motion_components: ComponentHash<MotionComponent>,
    pub damage_components: ComponentHash<DamageComponent>,
    pub scanner_components: ComponentHash<ScannerComponent>,
    pub projectile_components: ComponentHash<ProjectileComponent>,
}

impl GameState {
    pub fn new() -> GameState {
        GameState {
            players: Arc::new(RwLock::new(Vec::new())),
            motion_components: Arc::new(RwLock::new(HashMap::new())),
            damage_components: Arc::new(RwLock::new(HashMap::new())),
            scanner_components: Arc::new(RwLock::new(HashMap::new())),
            projectile_components: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    pub fn combatant_entered(&self, module_name: &str) {
        self.players.write().unwrap().push(module_name.to_string());
        writelock(&self.motion_components)
            .entry(module_name.to_string())
            .or_insert(MotionComponent::new());
        writelock(&self.damage_components)
            .entry(module_name.to_string())
            .or_insert(DamageComponent::new());
        writelock(&self.scanner_components)
            .entry(module_name.to_string())
            .or_insert(ScannerComponent::new());
        writelock(&self.projectile_components)
            .entry(module_name.to_string())
            .or_insert(ProjectileComponent::new());
    }
}

#[derive(Debug)]
pub enum LoopTerminationReason {
    CycleCountExceeded,
}

pub struct Gameloop {
    game_state: Arc<GameState>,
    systems: Vec<Box<dyn System>>,
    cycle: u32,
    max_cycles: u32,
    #[allow(dead_code)]
    num_combatants: usize,
    logger: Option<Sender<GameEvent>>,
}

impl Gameloop {
    pub fn new(
        game_state: Arc<GameState>,
        max_cycles: u32,
        num_combatants: usize,
        logger: Option<Sender<GameEvent>>,
    ) -> Gameloop {
        Gameloop {
            game_state,
            systems: vec![
                Box::new(ScannerSystem::new(logger.clone())),
                Box::new(MotionSystem::new(logger.clone())),
                Box::new(ProjectileSystem::new(logger.clone())),
                Box::new(DamageSystem::new(logger.clone())),
            ],
            cycle: 0,
            max_cycles,
            num_combatants,
            logger,
        }
    }

    pub fn start(&mut self) -> LoopTerminationReason {
        loop {
            self.systems
                .iter()
                .for_each(|s| s.apply(self.cycle, &self.game_state));
            self.cycle = self.cycle + 1;

            if self.cycle % 1000 == 0 {
                log_event(&self.logger, GameEvent::GameLooping(self.cycle));
            }

            if self.cycle >= self.max_cycles {
                return LoopTerminationReason::CycleCountExceeded;
            }
        }
    }
}

pub fn readlock<'a, T>(component: &'a ComponentHash<T>) -> RwLockReadGuard<'a, HashMap<String, T>> {
    component.read().unwrap()
}

pub fn writelock<'a, T>(
    component: &'a ComponentHash<T>,
) -> RwLockWriteGuard<'a, HashMap<String, T>> {
    component.write().unwrap()
}
