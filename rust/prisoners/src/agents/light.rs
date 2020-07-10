use super::types::Receiver;
use rand;

pub struct Light {
    on: Receiver,
    off: Receiver,
}

impl Light {
    pub fn new(on: Receiver, off: Receiver) -> Light {
        Light { on, off }
    }

    pub fn run(&self) {
        if rand::random() {
            loop {
                self.on.recv().unwrap();
                self.off.recv().unwrap();
            }
        } else {
            loop {
                self.off.recv().unwrap();
                self.on.recv().unwrap();
            }
        }
    }
}
