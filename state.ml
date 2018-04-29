open List
open Stack

(* [effect] represents the type of special effect a card has. *)
type effect = Add | Skip | Draw | Reverse | None

(* [color] represents the color of a card *)
type color = Red | Green | Blue | Yellow | Black

type card = {value: int; color: color; effect: effect; id: int}

(* change later *)
type player = AI | Human

type direction = Clockwise | Counter

type state = {
  (* size of list 3, where index is the value corresponding to the players
  eg. 0 is the user *)
  players : player list;
  draw_pile : card Queue.t;
  played_pile : card Stack.t;
  current_player: player;
  direction: direction;
  turn: int
}

let user_hand s = let p = hd s.players in p.hand
let ai1_hand s = let p = nth s.players 1 in p.hand
let ai2_hand s = let p = nth s.players 2 in p.hand
let ai3_hand s = let p = tl s.players in p.hand

let init_state = {
      players = [Human; AI; AI; AI];
      draw_pile = Queue.create ();
      played_pile = Stack.create ();
      current_player = Human;
      direction = Clockwise;
      turn = 0;
}
let turn s = s.turn

let next_turn s =
  if s.direction = Clockwise then
    if s.turn != 3 then
      s.turn + 1
    else 0
  else if s.turn != 0 then
    s.turn - 1
  else 3

let top_card s = top s.played_pile

let rec win_help (lst: player list) = match lst with
  | [] -> -1
  | h :: t -> if (length h.hand = 0) then h.id
    else win_help t

let get_winner s = win_help s.players

let update_state cmd p = failwith("unimplemented")
