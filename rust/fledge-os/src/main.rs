#![feature(core_intrinsics)]
#![no_std]
#![no_main]

extern crate rlibc;

use core::fmt;
use core::fmt::Write;
use core::panic::PanicInfo;
use x86_64::instructions::hlt;

#[allow(unused)]
#[derive(Copy, Clone)]
#[repr(u8)]
enum Color {
    Black = 0x0,
    Blue = 0x1,
    Green = 0x2,
    Cyan = 0x3,
    Red = 0x4,
    Magenta = 0x5,
    Brown = 0x6,
    Gray = 0x7,
    DarkGray = 0x8,
    BrightBlue = 0x9,
    BrightGreen = 0xA,
    BrightCyan = 0xB,
    BrightRed = 0xC,
    BrightMagenta = 0xD,
    Yellow = 0xE,
    White = 0xF,
}

struct Cursor {
    position: isize,
    foreground: Color,
    background: Color,
}

impl Cursor {
    fn color(&self) -> u8 {
        let fg = self.foreground as u8;
        let bg = (self.background as u8) << 4;

        fg | bg
    }

    fn print(&mut self, text: &[u8]) {
        let color = self.color();

        let framebuffer = 0xb8000 as *mut u8;

        for &character in text {
            unsafe {
                framebuffer.offset(self.position).write_volatile(character);
                framebuffer.offset(self.position + 1).write_volatile(color);
            }

            self.position += 2;
        }
    }
}

impl fmt::Write for Cursor {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.print(s.as_bytes());
        Ok(())
    }
}

#[panic_handler]
#[no_mangle]
fn panic(info: &PanicInfo) -> ! {
    let mut cursor = Cursor {
        position: 0,
        foreground: Color::White,
        background: Color::Red,
    };
    for _ in 0..(80 * 25) {
        cursor.print(b" ");
    }
    cursor.position = 0;
    write!(cursor, "{}", info).unwrap();

    loop {
        hlt();
    }
}

#[no_mangle]
pub extern "C" fn _start() -> ! {
    let text = b"Hello world";
    let mut cursor = Cursor {
        position: 0,
        foreground: Color::Black,
        background: Color::White,
    };
    cursor.print(text);

    loop {
        hlt();
    }
}
