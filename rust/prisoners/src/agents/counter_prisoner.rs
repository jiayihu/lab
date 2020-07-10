use super::types::Sender;
use crossbeam::crossbeam_channel::select;

pub struct CounterPrisoner {
    num_prisoners: u32,
    enter: Sender,
    leave: Sender,
    off: Sender,
    on: Sender,
    once: Sender,
    chance: Sender,
}

impl CounterPrisoner {
    pub fn new(
        num_prisoners: u32,
        enter: Sender,
        leave: Sender,
        off: Sender,
        on: Sender,
        once: Sender,
        chance: Sender,
    ) -> CounterPrisoner {
        CounterPrisoner {
            num_prisoners,
            enter,
            leave,
            off,
            on,
            once,
            chance,
        }
    }

    pub fn run(&self) {
        self.enter();

        self.once.send(()).unwrap();
        self.in_room(0);
    }

    fn in_room(&self, count_switched: u32) {
        select! {
            send(self.on, ()) -> res => {
                res.unwrap();

                let count_switched = count_switched + 1;
                println!("CounterPrisoner turned ON the light {} times", count_switched);

                self.leave();

                if count_switched < 2 * (self.num_prisoners - 1) {
                    self.enter();
                    self.in_room(count_switched);
                } else {
                    println!("CounterPrisoner is taking the CHANCE");
                    self.chance.send(()).unwrap();
                }
            },
            send(self.off, ()) -> res => {
                res.unwrap();
                self.on.send(()).unwrap();

                self.leave();
                self.enter();
                self.in_room(count_switched);
            },
        }
    }

    fn enter(&self) {
        self.enter.send(()).unwrap();
        println!("CounterPrisoner entered the room");
    }

    fn leave(&self) {
        println!("CounterPrisoner is leaving the room");
        self.leave.send(()).unwrap();
    }
}
