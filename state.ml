open List
open Stack
open Player

(* [effect] represents the type of special effect a card has. *)
type effect = Plus | Skip | Reverse | NoEffect | Wild | Wild4

(* [color] represents the color of a card *)
type color = Red | Green | Blue | Yellow | Black | NoColor

type card = Player.card

type player = Player.player

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

let all_cards = [(0, {value = 0; color = Red; effect = NoEffect; id = 40});
                 (1, {value = 1; color = Red; effect = NoEffect; id = 41});
                 (2, {value = 2; color = Red; effect = NoEffect; id = 42});
                 (3, {value = 3; color = Red; effect = NoEffect; id = 43});
                 (4, {value = 4; color = Red; effect = NoEffect; id = 44});
                 (5, {value = 5; color = Red; effect = NoEffect; id = 45});
                 (6, {value = 6; color = Red; effect = NoEffect; id = 46});
                 (7, {value = 7; color = Red; effect = NoEffect; id = 47});
                 (8, {value = 8; color = Red; effect = NoEffect; id = 48});
                 (9, {value = 9; color = Red; effect = NoEffect; id = 49});
                 (10, {value = -1; color = Red; effect = Plus; id = 54});
                 (11, {value = -1; color = Red; effect = Skip; id = 64});
                 (12, {value = -1; color = Red; effect = Reverse; id = 74});
                 (*yellow *)
                (13, {value = 0; color = Yellow; effect = NoEffect; id = 10});
                (14, {value = 1; color = Yellow; effect = NoEffect; id = 11});
                (15, {value = 2; color = Yellow; effect = NoEffect; id = 12});
                (16, {value = 3; color = Yellow; effect = NoEffect; id = 13});
                (17, {value = 4; color = Yellow; effect = NoEffect; id = 14});
                (18, {value = 5; color = Yellow; effect = NoEffect; id = 15});
                (19, {value = 6; color = Yellow; effect = NoEffect; id = 16});
                (20, {value = 7; color = Yellow; effect = NoEffect; id = 17});
                (21, {value = 8; color = Yellow; effect = NoEffect; id = 18});
                (22, {value = 9; color = Yellow; effect = NoEffect; id = 19});
                (23, {value = -1; color = Yellow; effect = Plus; id = 51});
                (24, {value = -1; color = Yellow; effect = Skip; id = 61});
                (25, {value = -1; color = Yellow; effect = Reverse; id = 71});
                 (* green *)
                (26, {value = 0; color = Green; effect = NoEffect; id = 20});
                (27, {value = 1; color = Green; effect = NoEffect; id = 21});
                (28, {value = 2; color = Green; effect = NoEffect; id = 22});
                (29, {value = 3; color = Green; effect = NoEffect; id = 23});
                (30, {value = 4; color = Green; effect = NoEffect; id = 24});
                (31, {value = 5; color = Green; effect = NoEffect; id = 25});
                (32, {value = 6; color = Green; effect = NoEffect; id = 26});
                (33, {value = 7; color = Green; effect = NoEffect; id = 27});
                (34, {value = 8; color = Green; effect = NoEffect; id = 28});
                (35, {value = 9; color = Green; effect = NoEffect; id = 29});
                (36, {value = -1; color = Green; effect = Plus; id = 52});
                (37, {value = -1; color = Green; effect = Skip; id = 62});
                (38, {value = -1; color = Green; effect = Reverse; id = 72});
                 (* blue *)
                (39, {value = 0; color = Blue; effect = NoEffect; id = 30});
                (40, {value = 1; color = Blue; effect = NoEffect; id = 31});
                (41, {value = 2; color = Blue; effect = NoEffect; id = 32});
                (42, {value = 3; color = Blue; effect = NoEffect; id = 33});
                (43, {value = 4; color = Blue; effect = NoEffect; id = 34});
                (44, {value = 5; color = Blue; effect = NoEffect; id = 35});
                (45, {value = 6; color = Blue; effect = NoEffect; id = 36});
                (46, {value = 7; color = Blue; effect = NoEffect; id = 37});
                (47, {value = 8; color = Blue; effect = NoEffect; id = 38});
                (48, {value = 9; color = Blue; effect = NoEffect; id = 39});
                (49, {value = -1; color = Blue; effect = Plus; id = 53});
                (50, {value = -1; color = Blue; effect = Skip; id = 63});
                (51, {value = -1; color = Blue; effect = Reverse; id = 73});
                 (*the four wilds*)
               (52, {value = -1; color = Black; effect = Wild; id = 80});
               (53, {value = -1; color = Black; effect = Wild; id = 80});
               (54, {value = -1; color = Black; effect = Wild; id = 80});
               (55, {value = -1; color = Black; effect = Wild; id = 80});
               (*the four wilds +4*)
               (56, {value = -1; color = Black; effect = Wild; id = 90});
               (57, {value = -1; color = Black; effect = Wild; id = 90});
               (58, {value = -1; color = Black; effect = Wild; id = 90});
               (59, {value = -1; color = Black; effect = Wild; id = 90});

              (60, {value = 1; color = Red; effect = NoEffect; id = 41});
              (61, {value = 2; color = Red; effect = NoEffect; id = 42});
              (62, {value = 3; color = Red; effect = NoEffect; id = 43});
              (63, {value = 4; color = Red; effect = NoEffect; id = 44});
              (64, {value = 5; color = Red; effect = NoEffect; id = 45});
              (65, {value = 6; color = Red; effect = NoEffect; id = 46});
              (66, {value = 7; color = Red; effect = NoEffect; id = 47});
              (67, {value = 8; color = Red; effect = NoEffect; id = 48});
              (68, {value = 9; color = Red; effect = NoEffect; id = 49});
              (69, {value = -1; color = Red; effect = Plus; id = 54});
              (70, {value = -1; color = Red; effect = Skip; id = 64});
              (71, {value = -1; color = Red; effect = Reverse; id = 74});

             (72, {value = 1; color = Yellow; effect = NoEffect; id = 11});
             (73, {value = 2; color = Yellow; effect = NoEffect; id = 12});
             (74, {value = 3; color = Yellow; effect = NoEffect; id = 13});
             (75, {value = 4; color = Yellow; effect = NoEffect; id = 14});
             (76, {value = 5; color = Yellow; effect = NoEffect; id = 15});
             (77, {value = 6; color = Yellow; effect = NoEffect; id = 16});
             (78, {value = 7; color = Yellow; effect = NoEffect; id = 17});
             (79, {value = 8; color = Yellow; effect = NoEffect; id = 18});
             (80, {value = 9; color = Yellow; effect = NoEffect; id = 19});
             (81, {value = -1; color = Yellow; effect = Plus; id = 51});
             (82, {value = -1; color = Yellow; effect = Skip; id = 61});
             (83, {value = -1; color = Yellow; effect = Reverse; id = 71});

             (84, {value = 1; color = Green; effect = NoEffect; id = 21});
             (85, {value = 2; color = Green; effect = NoEffect; id = 22});
             (86, {value = 3; color = Green; effect = NoEffect; id = 23});
             (87, {value = 4; color = Green; effect = NoEffect; id = 24});
             (88, {value = 5; color = Green; effect = NoEffect; id = 25});
             (89, {value = 6; color = Green; effect = NoEffect; id = 26});
             (90, {value = 7; color = Green; effect = NoEffect; id = 27});
             (91, {value = 8; color = Green; effect = NoEffect; id = 28});
             (92, {value = 9; color = Green; effect = NoEffect; id = 29});
             (93, {value = -1; color = Green; effect = Plus; id = 52});
             (94, {value = -1; color = Green; effect = Skip; id = 62});
             (95, {value = -1; color = Green; effect = Reverse; id = 72});

             (96, {value = 1; color = Blue; effect = NoEffect; id = 31});
             (97, {value = 2; color = Blue; effect = NoEffect; id = 32});
             (98, {value = 3; color = Blue; effect = NoEffect; id = 33});
             (99, {value = 4; color = Blue; effect = NoEffect; id = 34});
             (100, {value = 5; color = Blue; effect = NoEffect; id = 35});
             (101, {value = 6; color = Blue; effect = NoEffect; id = 36});
             (102, {value = 7; color = Blue; effect = NoEffect; id = 37});
             (103, {value = 8; color = Blue; effect = NoEffect; id = 38});
             (104, {value = 9; color = Blue; effect = NoEffect; id = 39});
             (105, {value = -1; color = Blue; effect = Plus; id = 53});
             (106, {value = -1; color = Blue; effect = Skip; id = 63});
             (107, {value = -1; color = Blue; effect = Reverse; id = 73}); ]

let user_hand s = let p = hd s.players in p.hand
let ai1_hand s = let p = nth s.players 1 in p.hand
let ai2_hand s = let p = nth s.players 2 in p.hand
let ai3_hand s = let p = nth s.players 3 in p.hand

let rec draw7 card_lst acc =
if List.length acc != 7 then
  let idx = Random.int 108 in
    if List.mem_assoc idx card_lst then
      let card_draw = List.assoc idx card_lst in
        draw7 (List.remove_assoc idx card_lst) (card_draw :: acc)
    else draw7 card_lst acc
else (card_lst, acc)

let (drawn1, hand1) = draw7 all_cards []
let (drawn2, hand2) = draw7 drawn1 []
let (drawn3, hand3) = draw7 drawn2 []
let (drawn4, hand4) = draw7 drawn3 []

let rec lst_to_q lst q = match lst with
  | [] -> q
  | (k, v):: t -> Queue.add v q; lst_to_q t q

let user = {id = 0; name = "human"; hand = hand1; intelligence = Human;}
let dumbai1 = {id = 1; name = "ai1"; hand = hand2; intelligence = AI;}
let dumbai2 = {id = 2; name = "ai2"; hand = hand3; intelligence = AI;}
let dumbai3 = {id = 3; name = "ai3"; hand = hand4; intelligence = AI;}

let empty = Queue.create ()

let init_state = {
      players = [user; dumbai1; dumbai2; dumbai3];
      draw_pile = lst_to_q drawn4 empty;
      played_pile = Stack.create ();
      current_player = user;
      direction = Clockwise;
      turn = 0;
    }

let turn s = s.turn

let draw_pile s = s.draw_pile

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
