#![feature(core_intrinsics)]
#![no_std]
#![no_main]

mod vga_buffer;

extern crate rlibc;

use core::fmt::Write;
use core::panic::PanicInfo;
use vga_buffer::ERR_WRITER;
use x86_64::instructions::hlt;

#[panic_handler]
#[no_mangle]
fn panic(info: &PanicInfo) -> ! {
    write!(ERR_WRITER.lock(), "{}", info).unwrap();

    loop {
        hlt();
    }
}

#[no_mangle]
pub extern "C" fn _start() -> ! {
    println!("Windows is shit");

    loop {
        hlt();
    }
}
