use super::*;
use nalgebra::{Point2, Rotation2, Vector2};

pub const RES_LIMIT: f32 = 10.0;
const SCAN_MAX_RANGE: f32 = 700.0;

#[derive(Debug)]
pub struct ScannerComponent {
    pub angle: i32,
}

impl ScannerComponent {
    pub fn new() -> ScannerComponent {
        ScannerComponent { angle: 0 }
    }
}

pub struct ScannerSystem {
    #[allow(dead_code)]
    logger: Option<Sender<GameEvent>>,
}

impl ScannerSystem {
    pub fn new(logger: Option<Sender<GameEvent>>) -> ScannerSystem {
        ScannerSystem { logger }
    }

    pub fn scan(game_state: &Arc<GameState>, player: &str, degree: f32, resolution: f32) -> i32 {
        let resolution = resolution.min(RES_LIMIT);

        let dcs = game_state.damage_components.read().unwrap();
        let mcs = game_state.motion_components.read().unwrap(); // Acquire mcs lock only after dmc lock
        let source = mcs.get(player).unwrap();
        let living_players: Vec<_> = game_state
            .players
            .read()
            .unwrap()
            .iter()
            .filter_map(|p| match dcs.get(p) {
                Some(dc) => {
                    if dc.dead() {
                        None
                    } else {
                        Some(p.to_string())
                    }
                }
                None => None,
            })
            .collect();

        let mut targets: Vec<_> = living_players
            .iter()
            .filter(|t| *t != player)
            .filter_map(|t| {
                let target = mcs.get(t).unwrap();
                let heading = Self::heading_to_target(&source.position, &target.position);
                let spread = (heading - degree).abs();

                // Is the heading to that target within indicated scan range?
                if spread <= resolution {
                    let r = Self::range_to_target(&source.position, &target.position);
                    if r <= SCAN_MAX_RANGE {
                        Some(r as i32)
                    } else {
                        None
                    }
                } else {
                    None
                }
            })
            .collect();

        targets.sort();
        targets.reverse(); // Now we have the shortest distance to target in scan range
        match targets.first() {
            Some(n) => *n,
            None => 0,
        }
    }

    pub fn heading_to_target(source: &Point2<f32>, target: &Point2<f32>) -> f32 {
        let heading = Rotation2::rotation_between(&Vector2::x(), &(target - source));
        heading.angle().to_degrees()
    }

    pub fn range_to_target(source: &Point2<f32>, target: &Point2<f32>) -> f32 {
        nalgebra::distance(source, target)
    }

    pub fn to_user_heading(real_heading: f32) -> i32 {
        (real_heading + 360.0) as i32 % 360
    }

    pub fn to_real_heading(user_heading: i32) -> i32 {
        if user_heading < 180 {
            user_heading
        } else {
            user_heading - 360
        }
    }
}

impl System for ScannerSystem {
    fn apply(&self, _cycle: u32, _game_state: &Arc<GameState>) {
        // Noop
    }
}
