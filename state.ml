(* [effect] represents the type of special effect a card has. *)
type effect = Add | Skip | Draw | Reverse | None

(* [color] represents the color of a card *)
type color = Red | Green | Blue | Yellow | Black

type card =
  {value: int; color: color; effect: effect; id: int}

type player = Player

type direction = Clockwise | Counter

type state = {
  (* size of list 3, where index is the value corresponding to the players
  eg. 0 is the user *)
  players : player list;
  (* change to queue *)
  draw_pile : card list;
  played_pile : card list;
  current_player: player;
  direction: direction;
  turn: int
}

let init_state = failwith("unimplemented")
let turn s = failwith("unimplemented")
let next_turn s = failwith("unimplemented")
let top_card s = failwith("unimplemented")
let update_state cmd p = failwith("unimplemented")
let get_winner s = failwith("unimplemented")
