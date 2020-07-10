use super::types::Receiver;
use crossbeam::crossbeam_channel::select;

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
