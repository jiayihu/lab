use super::*;
use crate::events::{log_event, GameEvent};
use crate::game::{readlock, writelock};

const DAMAGE_COLLISION: u32 = 2;
pub const DAMAGE_MAX: u32 = 100;

#[derive(Debug)]
pub enum DamageStatus {
    Alive,
    Dead,
}

#[derive(Debug)]
pub enum DamageKind {
    Collision(CollisionType),
    Projectile,
}

#[derive(Debug)]
pub struct DamageComponent {
    pub damage: u32,
    pub status: DamageStatus,
}

impl DamageComponent {
    pub fn new() -> DamageComponent {
        DamageComponent {
            damage: 0,
            status: DamageStatus::Alive,
        }
    }

    pub fn dead(&self) -> bool {
        match self.status {
            DamageStatus::Dead => true,
            _ => false,
        }
    }

    fn add_damage(&mut self, amount: u32) {
        self.damage += amount; // Death will be checked at end of the tick
    }
}

pub struct DamageSystem {
    logger: Option<Sender<GameEvent>>,
}

impl DamageSystem {
    pub fn new(logger: Option<Sender<GameEvent>>) -> DamageSystem {
        DamageSystem { logger }
    }

    pub fn advance(
        &self,
        player: &str,
        game_state: &Arc<GameState>,
        dc: &mut DamageComponent,
        cycle: u32,
    ) {
        self.apply_collision_damage(player, game_state, dc, cycle);
        self.apply_projectile_damage(player, game_state, dc, cycle);
        self.check_death(player, dc, cycle);
    }

    fn check_death(&self, player: &str, dc: &mut DamageComponent, cycle: u32) {
        if dc.damage >= DAMAGE_MAX && !dc.dead() {
            dc.damage = DAMAGE_MAX;
            dc.status = DamageStatus::Dead;
            log_event(
                &self.logger,
                GameEvent::Death {
                    cycle,
                    victim: player.to_string(),
                },
            );
        }
    }

    fn apply_collision_damage(
        &self,
        player: &str,
        game_state: &Arc<GameState>,
        dc: &mut DamageComponent,
        cycle: u32,
    ) {
        match readlock(&game_state.motion_components).get(player) {
            Some(mc) => match mc.collision {
                Some(CollisionType::Player(ref p)) => {
                    dc.add_damage(DAMAGE_COLLISION);

                    self.log_damage(
                        cycle,
                        DAMAGE_COLLISION,
                        DamageKind::Collision(CollisionType::Player(p.to_string())),
                        player,
                    )
                }
                Some(CollisionType::Wall(ref p)) => {
                    dc.add_damage(DAMAGE_COLLISION);
                    self.log_damage(
                        cycle,
                        DAMAGE_COLLISION,
                        DamageKind::Collision(CollisionType::Wall(p.clone())),
                        player,
                    );
                }
                None => {}
            },
            None => {}
        }
    }

    fn apply_projectile_damage(
        &self,
        player: &str,
        game_state: &Arc<GameState>,
        dc: &mut DamageComponent,
        cycle: u32,
    ) {
        let pcs = game_state.projectile_components.read().unwrap();
        let pc_opt = pcs.get(player);
        match pc_opt {
            Some(pc) => {
                for x in 0..1 {
                    if pc.projectiles[x].active_hits.contains_key(player) {
                        let dmg: u32 = pc.projectiles[x].active_hits[player];
                        println!("Doing explosion damage {} to player {}", dmg, player);
                        dc.add_damage(dmg);
                        self.log_damage(cycle, dmg, DamageKind::Projectile, player);
                    }
                }
            }
            None => {}
        }
    }

    fn log_damage(&self, cycle: u32, amount: u32, kind: DamageKind, victim: &str) {
        log_event(
            &self.logger,
            GameEvent::Damage {
                cycle,
                amount,
                kind,
                victim: victim.to_string(),
            },
        )
    }
}

impl System for DamageSystem {
    fn apply(&self, cycle: u32, game_state: &Arc<GameState>) {
        game_state.players.read().unwrap().iter().for_each(|p| {
            writelock(&game_state.damage_components)
                .entry(p.to_string())
                .and_modify(|dc| self.advance(p, game_state, dc, cycle));
        });
    }
}
