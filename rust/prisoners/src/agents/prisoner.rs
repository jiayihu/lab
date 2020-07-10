use super::types::Sender;
use crossbeam::crossbeam_channel::select;

pub struct Prisoner {
    id: u32,
    enter: Sender,
    leave: Sender,
    off: Sender,
    on: Sender,
    once: Sender,
}

impl Prisoner {
    pub fn new(
        id: u32,
        enter: Sender,
        leave: Sender,
        off: Sender,
        on: Sender,
        once: Sender,
    ) -> Prisoner {
        Prisoner {
            id,
            enter,
            leave,
            off,
            on,
            once,
        }
    }

    pub fn run(&self) {
        self.enter();

        self.once.send(()).unwrap();
        self.in_room(0);
    }

    fn in_room(&self, count_switched: u32) {
        select! {
            send(self.off, ()) -> res => {
                res.expect("No OFF channel");

                let count_switched = count_switched + 1;
                println!("Prisoner {} turned OFF the light {} times", self.id, count_switched);

                self.leave();

                if count_switched == 1 {
                    self.enter();
                    self.in_room(count_switched);
                }
            },
            send(self.on, ()) -> res => {
                res.expect("No ON channel");
                self.off.send(()).unwrap();

                self.leave();
                self.enter();
                self.in_room(count_switched);
            },
        }
    }

    fn enter(&self) {
        self.enter.send(()).expect("No enter channel");
        println!("Prisoner {} entered the room", self.id);
    }

    fn leave(&self) {
        println!("Prisoner {} is leaving the room", self.id);
        self.leave.send(()).expect("No leave channel");
    }
}
