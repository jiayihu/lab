use warsdk::*;

#[no_mangle]
pub extern "C" fn botinit() -> i32 {
    drive(90, 10);

    loop {
        damage();
        speed();
    }
}
