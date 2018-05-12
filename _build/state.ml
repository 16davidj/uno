open List
open Stack
open Queue
open Player
open Command

type direction = Clockwise | Counter

type state = {
  (* size of list 3, where index is the value corresponding to the players
  eg. 0 is the user *)
  players : Player.player list;
  draw_pile : Player.card Queue.t;
  played_pile : Player.card Stack.t;
  current_color: Player.color;
  current_player: Player.player;
  direction: direction;
  turn: int
}

let all_cards = [{value = 0; color = Red; effect = NoEffect; id = 40};
                 {value = 1; color = Red; effect = NoEffect; id = 41};
                 {value = 2; color = Red; effect = NoEffect; id = 42};
                 {value = 3; color = Red; effect = NoEffect; id = 43};
                 {value = 4; color = Red; effect = NoEffect; id = 44};
                 {value = 5; color = Red; effect = NoEffect; id = 45};
                 {value = 6; color = Red; effect = NoEffect; id = 46};
                 {value = 7; color = Red; effect = NoEffect; id = 47};
                 {value = 8; color = Red; effect = NoEffect; id = 48};
                 {value = 9; color = Red; effect = NoEffect; id = 49};
                 {value = -1; color = Red; effect = Plus; id = 54};
                 {value = -1; color = Red; effect = Skip; id = 64};
                 {value = -1; color = Red; effect = Reverse; id = 74};
                 (*yellow *)
                {value = 0; color = Yellow; effect = NoEffect; id = 10};
                {value = 1; color = Yellow; effect = NoEffect; id = 11};
                {value = 2; color = Yellow; effect = NoEffect; id = 12};
                {value = 3; color = Yellow; effect = NoEffect; id = 13};
                {value = 4; color = Yellow; effect = NoEffect; id = 14};
                {value = 5; color = Yellow; effect = NoEffect; id = 15};
                {value = 6; color = Yellow; effect = NoEffect; id = 16};
                {value = 7; color = Yellow; effect = NoEffect; id = 17};
                {value = 8; color = Yellow; effect = NoEffect; id = 18};
                {value = 9; color = Yellow; effect = NoEffect; id = 19};
                {value = -1; color = Yellow; effect = Plus; id = 51};
                {value = -1; color = Yellow; effect = Skip; id = 61};
                {value = -1; color = Yellow; effect = Reverse; id = 71};
                 (* green *)
                {value = 0; color = Green; effect = NoEffect; id = 20};
                {value = 1; color = Green; effect = NoEffect; id = 21};
                {value = 2; color = Green; effect = NoEffect; id = 22};
                {value = 3; color = Green; effect = NoEffect; id = 23};
                {value = 4; color = Green; effect = NoEffect; id = 24};
                {value = 5; color = Green; effect = NoEffect; id = 25};
                {value = 6; color = Green; effect = NoEffect; id = 26};
                {value = 7; color = Green; effect = NoEffect; id = 27};
                {value = 8; color = Green; effect = NoEffect; id = 28};
                {value = 9; color = Green; effect = NoEffect; id = 29};
                {value = -1; color = Green; effect = Plus; id = 52};
                {value = -1; color = Green; effect = Skip; id = 62};
                {value = -1; color = Green; effect = Reverse; id = 72};
                 (* blue *)
                {value = 0; color = Blue; effect = NoEffect; id = 30};
                {value = 1; color = Blue; effect = NoEffect; id = 31};
                {value = 2; color = Blue; effect = NoEffect; id = 32};
                {value = 3; color = Blue; effect = NoEffect; id = 33};
                {value = 4; color = Blue; effect = NoEffect; id = 34};
                {value = 5; color = Blue; effect = NoEffect; id = 35};
                {value = 6; color = Blue; effect = NoEffect; id = 36};
                {value = 7; color = Blue; effect = NoEffect; id = 37};
                {value = 8; color = Blue; effect = NoEffect; id = 38};
                {value = 9; color = Blue; effect = NoEffect; id = 39};
                {value = -1; color = Blue; effect = Plus; id = 53};
                {value = -1; color = Blue; effect = Skip; id = 63};
                {value = -1; color = Blue; effect = Reverse; id = 73};
                 (*the four wilds*)
               {value = -1; color = Black; effect = Wild; id = 80};
               {value = -1; color = Black; effect = Wild; id = 80};
               {value = -1; color = Black; effect = Wild; id = 80};
               {value = -1; color = Black; effect = Wild; id = 80};
               (*the four wilds +4*)
               {value = -1; color = Black; effect = Wild4; id = 90};
               {value = -1; color = Black; effect = Wild4; id = 90};
               {value = -1; color = Black; effect = Wild4; id = 90};
               {value = -1; color = Black; effect = Wild4; id = 90};

              {value = 1; color = Red; effect = NoEffect; id = 41};
              {value = 2; color = Red; effect = NoEffect; id = 42};
              {value = 3; color = Red; effect = NoEffect; id = 43};
              {value = 4; color = Red; effect = NoEffect; id = 44};
              {value = 5; color = Red; effect = NoEffect; id = 45};
              {value = 6; color = Red; effect = NoEffect; id = 46};
              {value = 7; color = Red; effect = NoEffect; id = 47};
              {value = 8; color = Red; effect = NoEffect; id = 48};
              {value = 9; color = Red; effect = NoEffect; id = 49};
              {value = -1; color = Red; effect = Plus; id = 54};
              {value = -1; color = Red; effect = Skip; id = 64};
              {value = -1; color = Red; effect = Reverse; id = 74};

             {value = 1; color = Yellow; effect = NoEffect; id = 11};
             {value = 2; color = Yellow; effect = NoEffect; id = 12};
             {value = 3; color = Yellow; effect = NoEffect; id = 13};
             {value = 4; color = Yellow; effect = NoEffect; id = 14};
             {value = 5; color = Yellow; effect = NoEffect; id = 15};
             {value = 6; color = Yellow; effect = NoEffect; id = 16};
             {value = 7; color = Yellow; effect = NoEffect; id = 17};
             {value = 8; color = Yellow; effect = NoEffect; id = 18};
             {value = 9; color = Yellow; effect = NoEffect; id = 19};
             {value = -1; color = Yellow; effect = Plus; id = 51};
             {value = -1; color = Yellow; effect = Skip; id = 61};
             {value = -1; color = Yellow; effect = Reverse; id = 71};

             {value = 1; color = Green; effect = NoEffect; id = 21};
             {value = 2; color = Green; effect = NoEffect; id = 22};
             {value = 3; color = Green; effect = NoEffect; id = 23};
             {value = 4; color = Green; effect = NoEffect; id = 24};
             {value = 5; color = Green; effect = NoEffect; id = 25};
             {value = 6; color = Green; effect = NoEffect; id = 26};
             {value = 7; color = Green; effect = NoEffect; id = 27};
             {value = 8; color = Green; effect = NoEffect; id = 28};
             {value = 9; color = Green; effect = NoEffect; id = 29};
             {value = -1; color = Green; effect = Plus; id = 52};
             {value = -1; color = Green; effect = Skip; id = 62};
             {value = -1; color = Green; effect = Reverse; id = 72};

             {value = 1; color = Blue; effect = NoEffect; id = 31};
             {value = 2; color = Blue; effect = NoEffect; id = 32};
             {value = 3; color = Blue; effect = NoEffect; id = 33};
             {value = 4; color = Blue; effect = NoEffect; id = 34};
             {value = 5; color = Blue; effect = NoEffect; id = 35};
             {value = 6; color = Blue; effect = NoEffect; id = 36};
             {value = 7; color = Blue; effect = NoEffect; id = 37};
             {value = 8; color = Blue; effect = NoEffect; id = 38};
             {value = 9; color = Blue; effect = NoEffect; id = 39};
             {value = -1; color = Blue; effect = Plus; id = 53};
             {value = -1; color = Blue; effect = Skip; id = 63};
             {value = -1; color = Blue; effect = Reverse; id = 73}; ]

let user_hand s = let p = hd s.players in p.hand
let ai1_hand s = let p = nth s.players 1 in p.hand
let ai2_hand s = let p = nth s.players 2 in p.hand
let ai3_hand s = let p = nth s.players 3 in p.hand

let rec draw7 card_lst acc =
  if List.length acc != 7 then
    begin match card_lst with
      | [] -> failwith("nocards")
      | h :: t -> draw7 t (h :: acc)
    end
  else (card_lst, acc)

(* shuffling idea and concept from: http://www.codecodex.com/wiki/Shuffle_an_array#OCaml_:_Array.sort *)
let shuffle = Random.self_init(); List.sort (fun _ _ -> (Random.int 3) - 1)

let (drawn1, hand1) = draw7 (shuffle (shuffle (shuffle (shuffle all_cards)))) []
let (drawn2, hand2) = draw7 drawn1 []
let (drawn3, hand3) = draw7 drawn2 []
let (drawn4, hand4) = draw7 drawn3 []

let rec lst_to_q lst q = match lst with
  | [] -> q
  | h :: t -> Queue.add h q; lst_to_q t q

let user = {id = 0; name = "You"; hand = hand1; intelligence = Human;}
let dumbai1 = {id = 1; name = "Player 2"; hand = hand2; intelligence = AI;}
let dumbai2 = {id = 2; name = "Player 3"; hand = hand3; intelligence = AI;}
let dumbai3 = {id = 3; name = "Frank"; hand = hand4; intelligence = AI;}

let top_card s = Stack.top s.played_pile

let empty = Queue.create ()

let pile = Stack.create ()

let init_draw = lst_to_q drawn4 empty

let rec get_init_card () =
  let c = Queue.pop init_draw in
  if c.effect = Wild then (get_init_card (Queue.push c init_draw))
  else c

let init_card = get_init_card ()

let init_pile () = Stack.push (init_card) pile

let init_color = init_card.color

let init_state = {
  players = [user; dumbai1; dumbai2; dumbai3];
  draw_pile = init_draw;
  played_pile = pile;
  current_color = init_color;
  current_player = user;
  direction = Clockwise;
  turn = 0;
}

let players s = s.players
let draw_pile s = s.draw_pile
let played_pile s = s.played_pile
let current_color s = s.current_color
let current_player s = s.current_player
let direction s = s.direction
let turn s = s.turn

let is_counter s = match s.direction with
  | Counter -> true
  | _ -> false

let next_turn s =
  if s.direction = Clockwise then
    if s.turn != 3 then
      s.turn + 1
    else 0
  else if s.turn != 0 then
    s.turn - 1
  else 3

let next_next_turn s =
  if s.turn < 2 then
    s.turn + 2
  else s.turn - 2

let prev_turn s =
  if s.direction = Counter then
    if s.turn != 3 then
      s.turn + 1
    else 0
  else if s.turn != 0 then
    s.turn - 1
  else 3

let rec win_help (lst: Player.player list) = match lst with
  | [] -> -1
  | h :: t -> if (List.length h.hand = 0) then h.id
    else win_help t

let get_winner s = win_help s.players


let rec remove_card_from_hand (hand: Player.card list) (card: Player.card) =
match hand with
| h::t ->
  if h.id = card.id then t
  else h::(remove_card_from_hand t card)
| [] -> []

let rec remove_card_from_player players player_id card =
match players with
| p::others ->
  if p.id = player_id then let hand' = remove_card_from_hand p.hand card in
  let p' = {id = p.id; name = p.name; hand = hand';
    intelligence = p.intelligence} in p'::others
  else p::(remove_card_from_player others player_id card)
| [] -> []

let rec add_card_to_player players id card =
match players with
| p::others ->
  if p.id = id then let p' = {id = p.id; name = p.name; hand = card::(p.hand);
       intelligence = p.intelligence} in p'::others
  else p::(add_card_to_player others id card)
| [] -> []

let rec add_cards_to_player players id cards =
match cards with
| [] -> players
| h::t -> add_cards_to_player (add_card_to_player players id h) id t

let rec card_in_hand (hand: Player.card list) (card: Player.card) =
match hand with
| [] -> false
| c1::others -> if c1.id = card.id then true
    else card_in_hand others card

let rec player_has_card (players: Player.player list) id card =
match players with
| [] -> false
| p1::others -> if p1.id = id then card_in_hand (p1.hand) card
    else player_has_card others id card

let reverse dir =
match dir with
| Clockwise -> Counter
| Counter -> Clockwise

let update_state_play_card card s =
Stack.push card s.played_pile;
let curr_player = s.current_player in
match card.effect with
| NoEffect ->
  { s with
    players = remove_card_from_player s.players curr_player.id card;
    current_color = card.color;
    current_player = nth s.players (next_turn s);
    turn = next_turn s;
  }
| Skip ->
  { s with
    players = remove_card_from_player s.players curr_player.id card;
    current_color = card.color;
    current_player = nth s.players (next_next_turn s);
    turn = next_next_turn s;
  }
| Plus ->
  let c1 = take s.draw_pile in
  let c2 = take s.draw_pile in
  let plus_cards = c1::c2::[] in
  let next_player = nth s.players (next_turn s) in
  let players' = remove_card_from_player s.players curr_player.id card in
  { s with
    players = add_cards_to_player players' next_player.id plus_cards;
    current_color = card.color;
    current_player = next_player;
    turn = next_turn s;
  }
| Reverse ->
  { s with
    players = remove_card_from_player s.players curr_player.id card;
    current_color = card.color;
    current_player = nth s.players (prev_turn s);
    direction = reverse s.direction;
    turn = prev_turn s;
  }
| Wild ->
  { s with
    players = remove_card_from_player s.players curr_player.id card;
    current_color = Black;
  }
| Wild4 ->
  let c1 = pop s.draw_pile in
  let c2 = pop s.draw_pile in
  let c3 = pop s.draw_pile in
  let c4 = pop s.draw_pile in
  let plus_cards = c1::c2::c3::c4::[] in
  let next_player = nth s.players (next_turn s) in
  let players' = remove_card_from_player s.players curr_player.id card in
  { s with
    players = add_cards_to_player players' next_player.id plus_cards;
    current_color = Black;
  }

let check_playability color c1 c2 =
  if c2.color = color then true
  else if c2.color = Black then true
  else if c2.effect = NoEffect && c2.value = c1.value then true
  else c2.effect = c1.effect

let update_state_color color s =
{ s with
  current_color = color;
  current_player = nth s.players (next_turn s);
  turn = next_turn s;
}

let add_2_to_prev_player s =
let c1 = pop s.draw_pile in
let c2 = pop s.draw_pile in
let plus_cards = c1::c2::[] in
let prev_player = nth s.players (prev_turn s) in
{ s with
  players = add_cards_to_player s.players prev_player.id plus_cards;
}

let draw_card s =
let c1 = pop s.draw_pile in
let plus_cards = c1::[] in
let curr_player = s.current_player in
{ s with
  players = add_cards_to_player s.players curr_player.id plus_cards;
  current_player = nth s.players (next_turn s);
  turn = next_turn s;
}

let check_uno s = let curr_player = s.current_player in
List.length curr_player.hand = 1

let update_state cmd s =
  let curr_color = s.current_color in
  let top_card = Stack.top s.played_pile in
  let curr_player = s.current_player in
  if s.current_color <> Black then
    match cmd with
    | Play card ->
      if (check_playability curr_color top_card card)
      && (player_has_card s.players curr_player.id card) then
        let new_state = update_state_play_card card s in
        if check_uno new_state then add_2_to_prev_player new_state
        else new_state
      else s
    | Draw -> draw_card s
    | Uno card ->
      if check_uno s then update_state_play_card card s
      else s
    | _ -> s
  else
    match cmd with
    | Choose color ->
      if s.current_color = Black && color <> Black
      then update_state_color color s
      else s
    | _ -> s
