use super::*;
use crate::events::{log_event, GameEvent};
use motion::*;
use nalgebra::Point2;

const DIRECT_RANGE: u32 = 5;
const NEAR_RANGE: u32 = 20;
const FAR_RANGE: u32 = 40;

const DIRECT_HIT: u32 = 10;
const NEAR_HIT: u32 = 5;
const FAR_HIT: u32 = 3;

const EXP_DAMAGE: [[u32; 2]; 3] = [
    [DIRECT_RANGE, DIRECT_HIT],
    [NEAR_RANGE, NEAR_HIT],
    [FAR_RANGE, FAR_HIT],
];

const PROJECTILE_SPEED: u32 = 50;
const PROJECTILE_MAX_RANGE: u32 = 200;
const RELOAD_CYCLES: u32 = 15;
const EXPLODE_CYCLES: u32 = 5;

#[derive(Debug, PartialEq)]
pub enum ProjectileStatus {
    Available,
    Flying,
    Exploding,
    ReadyToLaunch {
        origin: Point2<f32>,
        angle: i32,
        range: u32,
    },
}

#[derive(Debug)]
pub struct Projectile {
    pub status: ProjectileStatus,
    pub start_pos: Point2<f32>,
    pub position: Point2<f32>,
    pub heading: i32,
    pub cycle_count: u32,
    pub current_distance: u32,
    pub range: u32,
    pub active_hits: HashMap<String, u32>,
}

impl Projectile {
    pub fn new() -> Projectile {
        Projectile {
            position: Point2::new(0.0_f32, 0.0),
            start_pos: Point2::new(0.0_f32, 0.0),
            heading: 0,
            cycle_count: 0,
            current_distance: 0,
            range: 0,
            status: ProjectileStatus::Available,
            active_hits: HashMap::new(),
        }
    }

    pub fn launch(&mut self, from_position: &Point2<f32>, angle: i32, range: u32) {
        self.status = ProjectileStatus::ReadyToLaunch {
            origin: from_position.clone(),
            angle,
            range,
        };
    }

    pub fn reset(&mut self) {
        self.position = Point2::new(0.0_f32, 0.0);
        self.start_pos = Point2::new(0.0_f32, 0.0);
        self.heading = 0;
        self.cycle_count = 0;
        self.heading = 0;
        self.range = 0;
        self.current_distance = 0;
        self.status = ProjectileStatus::Available;
        self.active_hits.clear();
    }

    fn add_hit(&mut self, player: String, amount: u32) {
        self.active_hits.entry(player).or_insert(amount);
    }

    fn clear_hits(&mut self) {
        self.active_hits.clear();
    }
}

#[derive(Debug)]
pub struct ProjectileComponent {
    pub projectiles: [Projectile; 2],
}

impl ProjectileComponent {
    pub fn new() -> ProjectileComponent {
        ProjectileComponent {
            projectiles: [Projectile::new(), Projectile::new()],
        }
    }

    pub fn launch(&mut self, origin: &Point2<f32>, angle: i32, range: u32) -> i32 {
        let range = range.min(PROJECTILE_MAX_RANGE);
        for idx in 0..1 {
            if self.projectiles[idx].status == ProjectileStatus::Available
                && self.projectiles[idx].cycle_count == 0
            {
                self.projectiles[idx].launch(origin, angle, range);
                return 1;
            }
        }
        return 0;
    }
}

pub struct ProjectileSystem {
    logger: Option<Sender<GameEvent>>,
}

impl ProjectileSystem {
    pub fn new(logger: Option<Sender<GameEvent>>) -> ProjectileSystem {
        ProjectileSystem { logger }
    }

    fn advance(&self, projectile: &mut Projectile, gs: &Arc<GameState>, cycle: u32, player: &str) {
        self.launch_projectile(projectile, cycle, player);
        self.move_projectile(projectile, cycle);
        self.check_wall_collision(projectile, cycle);
        self.inflict_splash_damage(projectile, gs);
        self.decay_projectile(projectile);
    }

    fn launch_projectile(&self, projectile: &mut Projectile, cycle: u32, player: &str) {
        if let ProjectileStatus::ReadyToLaunch {
            origin,
            angle,
            range,
        } = projectile.status
        {
            projectile.position = origin.clone();
            projectile.start_pos = origin.clone();
            projectile.heading = angle;
            projectile.range = range;
            projectile.cycle_count = 0;
            projectile.current_distance = 0;
            projectile.status = ProjectileStatus::Flying;
            projectile.active_hits.clear();

            log_event(
                &self.logger,
                GameEvent::Launch {
                    cycle,
                    player: player.to_string(),
                    from: origin.clone(),
                    heading: angle,
                    range,
                },
            );
        }
    }

    fn move_projectile(&self, projectile: &mut Projectile, cycle: u32) {
        if projectile.status == ProjectileStatus::Flying {
            projectile.current_distance += PROJECTILE_SPEED;
            projectile.current_distance = projectile.current_distance.min(projectile.range);

            projectile.position = MotionSystem::point_along_heading(
                &projectile.start_pos,
                projectile.heading as f32,
                projectile.current_distance as f32,
            );

            if projectile.current_distance == projectile.range {
                self.set_exploding(projectile, cycle);
            }
        }
    }

    fn check_wall_collision(&self, projectile: &mut Projectile, cycle: u32) {
        if projectile.status == ProjectileStatus::Flying {
            // Check for wall collision
            if projectile.position.x < 0.0 {
                self.set_exploding(projectile, cycle);
                projectile.position.x = 1.0;
            }
            if projectile.position.x >= MAX_X {
                self.set_exploding(projectile, cycle);
                projectile.position.x = MAX_X - 1.0;
            }
            if projectile.position.y < 0.0 {
                self.set_exploding(projectile, cycle);
                projectile.position.y = 1.0;
            }
            if projectile.position.y >= MAX_Y {
                self.set_exploding(projectile, cycle);
                projectile.position.y = MAX_Y - 1.0;
            }
        }
    }

    fn set_exploding(&self, projectile: &mut Projectile, cycle: u32) {
        if projectile.status == ProjectileStatus::Exploding {
            return;
        }

        projectile.status = ProjectileStatus::Exploding;
        projectile.cycle_count = EXPLODE_CYCLES;

        log_event(
            &self.logger,
            GameEvent::Explode {
                cycle,
                position: projectile.position.clone(),
            },
        );
    }

    // Exploding missiles last for (cycle_count) ticks, inflicting damage each tick they are active.
    // Damage is placed in a "queue" for damage syste to actuallly inflict
    fn inflict_splash_damage(&self, projectile: &mut Projectile, gs: &Arc<GameState>) {
        if projectile.status == ProjectileStatus::Exploding {
            projectile.clear_hits();

            for (p, mc) in gs.motion_components.read().unwrap().iter() {
                let distance = ScannerSystem::range_to_target(&projectile.position, &mc.position);

                for idx in 0..2 {
                    if distance < (EXP_DAMAGE[idx][0] as f32) {
                        projectile.add_hit(p.to_string(), EXP_DAMAGE[idx][1]);
                    }
                }
            }
        }
    }

    fn decay_projectile(&self, projectile: &mut Projectile) {
        projectile.cycle_count = match projectile.cycle_count.checked_sub(1) {
            Some(n) => n,
            None => 0,
        };

        // Stop exploding
        if projectile.cycle_count == 0 && projectile.status == ProjectileStatus::Exploding {
            projectile.reset();
            projectile.cycle_count = RELOAD_CYCLES;
        }
    }
}

impl System for ProjectileSystem {
    fn apply(&self, cycle: u32, game_state: &Arc<GameState>) {
        game_state.players.read().unwrap().iter().for_each(|p| {
            writelock(&game_state.projectile_components)
                .entry(p.to_string())
                .and_modify(|pc| {
                    self.advance(&mut pc.projectiles[0], game_state, cycle, p);
                    self.advance(&mut pc.projectiles[1], game_state, cycle, p);
                });
        })
    }
}
