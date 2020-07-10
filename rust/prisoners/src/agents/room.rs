use super::types::Receiver;

pub struct Room {
    enter: Receiver,
    leave: Receiver,
}

impl Room {
    pub fn new(enter: Receiver, leave: Receiver) -> Room {
        Room { enter, leave }
    }

    pub fn run(&self) {
        loop {
            self.enter.recv().expect("Room enter closed");
            self.leave.recv().expect("Room leave closed");
        }
    }
}
