use warsdk::*;

// Rabbit runs around the field randomly
// use it as a target

#[no_mangle]
pub extern "C" fn botinit() -> i32 {
    loop {
        go(60 + rand(900), 60 + rand(900)); /* go somewhere in the field */
    }
}
