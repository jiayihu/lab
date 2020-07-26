mod ffi;

pub fn scan(angle: i32, resolution: i32) -> i32 {
    unsafe { ffi::scan(angle, resolution) }
}
pub fn cannon(angle: i32, range: i32) -> i32 {
    unsafe { ffi::cannon(angle, range) }
}
pub fn drive(angle: i32, speed: i32) -> i32 {
    unsafe { ffi::drive(angle, speed) }
}
pub fn damage() -> i32 {
    unsafe { ffi::damage() }
}
pub fn speed() -> i32 {
    unsafe { ffi::speed() }
}
pub fn loc_x() -> i32 {
    unsafe { ffi::loc_x() }
}
pub fn loc_y() -> i32 {
    unsafe { ffi::loc_y() }
}
pub fn rand(limit: i32) -> i32 {
    unsafe { ffi::rand(limit) }
}
pub fn wsqrt(number: i32) -> i32 {
    unsafe { ffi::wsqrt(number) }
}
pub fn wsin(degree: i32) -> i32 {
    unsafe { ffi::wsin(degree) }
}
pub fn wcos(degree: i32) -> i32 {
    unsafe { ffi::wcos(degree) }
}
pub fn wtan(degree: i32) -> i32 {
    unsafe { ffi::wtan(degree) }
}
pub fn watan(degree: i32) -> i32 {
    unsafe { ffi::watan(degree) }
}
pub fn plot_course(tx: i32, ty: i32) -> i32 {
    unsafe { ffi::plot_course(tx, ty) }
}

pub fn go(target_x: i32, target_y: i32) {
    let course = plot_course(target_x, target_y);
    drive(course, 20);

    while (target_x - loc_x()).abs() > 40 && (target_y - loc_y()).abs() > 40 && speed() > 0 {
        // Wait till we get to the target
    }

    drive(course, 0); // Turn off engine

    while speed() > 0 {
        // Steady on until we stop
    }
}

pub const ANGLE_EAST: i32 = 0;
pub const ANGLE_NORTH: i32 = 90;
pub const ANGLE_WEST: i32 = 180;
pub const ANGLE_SOUTH: i32 = 270;

pub const MAX_X: u32 = 1000;
pub const MAX_Y: u32 = 1000;

pub const DAMAGE_COLLISION: u32 = 2;
pub const DAMAGE_DIRECTHIT: u32 = 10;
pub const DAMAGE_NEARHIT: u32 = 5;
pub const DAMAGE_FARHIT: u32 = 3;

pub const BLAST_RADIUS: i32 = 40;
pub const PROJECTILE_MAX_RANGE: u32 = 200;
