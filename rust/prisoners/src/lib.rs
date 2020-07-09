use crossbeam::crossbeam_channel::{self, select};
use rand;

pub type Sender = crossbeam_channel::Sender<()>;
pub type Receiver = crossbeam_channel::Receiver<()>;
pub type Channel = (Sender, Receiver);

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
                res.unwrap();

                let count_switched = count_switched + 1;
                println!("Prisoner {} turned OFF the light {} times", self.id, count_switched);

                self.leave();

                if count_switched == 1 {
                    self.enter();
                    self.in_room(count_switched);
                }
            },
            send(self.on, ()) -> res => {
                res.unwrap();
                self.off.send(()).unwrap();

                self.leave();
                self.enter();
                self.in_room(count_switched);
            },
        }
    }

    fn enter(&self) {
        self.enter.send(()).unwrap();
        println!("Prisoner {} entered the room", self.id);
    }

    fn leave(&self) {
        println!("Prisoner {} is leaving the room", self.id);
        self.leave.send(()).unwrap();
    }
}

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
                    println!("CounterPrisoner is taking the chance");
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

pub struct Warden {
    num_prisoners: u32,
    once: Receiver,
    chance: Receiver,
}

impl Warden {
    pub fn new(num_prisoners: u32, once: Receiver, chance: Receiver) -> Warden {
        Warden {
            num_prisoners,
            once,
            chance,
        }
    }

    pub fn run(&self) {
        let mut trapped = false;

        for i in 1..=self.num_prisoners {
            select! {
                recv(self.once) -> msg => {
                    msg.unwrap();
                    println!("{} prisoners have entered the room", i);
                },
                recv(self.chance) -> msg => {
                    msg.unwrap();
                    trapped = true;
                    break;
                }
            }
        }

        if trapped {
            println!("LOCK all prisoners!");
        } else {
            self.chance.recv().unwrap();
            println!("FREE all prisoners!");
        }
    }
}
