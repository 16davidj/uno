open List
open Stack
open Player

(* [effect] represents the type of special effect a card has. *)
type effect = Plus | Skip | Reverse | None

(* [color] represents the color of a card *)
type color = Red | Green | Blue | Yellow | Black

type card = Player.card

type player = Player.player

(* change later *)

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
let ai3_hand s = let p = nth s.players 3 in p.hand


let user = {id = 0; name = "human"; hand = []; intelligence = Human;}
let dumbai1 = {id = 1; name = "ai1"; hand = []; intelligence = AI;}
let dumbai2 = {id = 1; name = "ai2"; hand = []; intelligence = AI;}
let dumbai3 = {id = 1; name = "ai3"; hand = []; intelligence = AI;}

let init_state = {
      players = [user; dumbai1; dumbai2; dumbai3];
      draw_pile = Queue.create ();
      played_pile = Stack.create ();
      current_player = user;
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
  | h :: t -> if (List.length h.hand = 0) then h.id
    else win_help t

let get_winner s = win_help s.players

let update_state cmd p = failwith("unimplemented")
