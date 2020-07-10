use crossbeam::crossbeam_channel::{self};

pub type Sender = crossbeam_channel::Sender<()>;
pub type Receiver = crossbeam_channel::Receiver<()>;
pub type Channel = (Sender, Receiver);
