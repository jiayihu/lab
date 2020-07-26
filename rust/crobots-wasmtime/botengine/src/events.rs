use crate::game::damage::DamageKind;
use crate::game::motion::CollisionType;
use nalgebra::Point2;
use std::sync::mpsc::Sender;

#[derive(Debug)]
pub enum GameEvent {
    GameStarted,
    Collision {
        cycle: u32,
        kind: CollisionType,
    },
    Damage {
        cycle: u32,
        amount: u32,
        kind: DamageKind,
        victim: String,
    },
    Death {
        cycle: u32,
        victim: String,
    },
    Launch {
        cycle: u32,
        player: String,
        from: Point2<f32>,
        heading: i32,
        range: u32,
    },
    Explode {
        cycle: u32,
        position: Point2<f32>,
    },
    GameLooping(u32),
    GameTerminated(i32),
}

pub fn log_event(logger: &Option<Sender<GameEvent>>, ge: GameEvent) {
    match logger {
        Some(l) => match l.send(ge) {
            Ok(()) => {}
            Err(e) => {
                println!("Sent to missing logger: {}", e);
            }
        },
        None => {}
    }
}
