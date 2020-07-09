# Prisoners Game

This is a Rust solution to the following game, with a formal verification in CCS (Calculus of Communicating Systems) and HML (Hennessy-Miner Logic). The cuncurrent implementation is guaranteed to terminate and lead to victory for the prisoners.

The CCS specification and properties are saved as .caal in [game.cwb](game.cwb) and can be loaded using [CAAL](http://caal.cs.aau.dk/).

## The game

"50 prisoners kept in separate cells got a chance to be released: From time to time one of them will be carried in a special room (in no particular order, possibly multiple times consecutively, but with a fair schedule to avoid infinite wait) and then back to the cell.

The room is completely empty except for a switch that can turn the light either on or off (the light is not visible from outside and cannot be broken). At any time, if any of them says that all the prisoners have already entered the room at least once and this is true, then all prisoners will be released (but if it is false, then the chance ends and they will never be released). Before the challenge starts, the prisoners have the possibility to discuss together some "protocol" to follow.

Can you find a winning strategy for the prisoners?

Note that the initial state of the light in the room is not known."
