use warsdk::*;

const BOUND_X_MIN: i32 = 80;
const BOUND_X_MAX: i32 = 920;

struct State {
    course: i32,
}

#[no_mangle]
pub extern "C" fn botinit() -> i32 {
    go(500, 500);

    let mut state = State { course: 0 };

    loop {
        look(ANGLE_EAST, &mut state);
        look(ANGLE_NORTH, &mut state);
        look(ANGLE_WEST, &mut state);
        look(ANGLE_SOUTH, &mut state);

        if loc_x() > BOUND_X_MAX {
            reverse(&mut state);
        }

        if loc_x() < BOUND_X_MIN {
            reverse(&mut state);
        }

        // Bumped into something
        if speed() == 0 {
            reverse(&mut state);
        }
    }
}

fn look(angle: i32, state: &mut State) {
    let mut range = scan(angle, 2);

    // Fire at targets in range until we have no targets in range
    while range > 0 && range < PROJECTILE_MAX_RANGE as i32 {
        if speed() > 0 {
            drive(state.course, 0);
        }

        // Don't want to blow ourselves up!
        if range > BLAST_RADIUS {
            cannon(angle, range);
        }

        range = scan(angle, 2);
    }
}

fn reverse(state: &mut State) {
    if state.course == ANGLE_EAST {
        state.course = ANGLE_WEST;
    } else {
        state.course = ANGLE_EAST;
    }
}
