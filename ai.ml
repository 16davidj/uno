open Player
open State
open Command

type effect = Skip | Plus | Reverse | NoEffect | Wild | Wild4

(* [color] represents the color of a card *)
type color = Red | Green | Blue | Yellow | Black | NoColor

let rec does_wild4_exist hand =
  match hand with
  | [] -> None
  | h::t -> if h.effect = Wild4 || h.effect = Wild then Some h else does_wild4_exist t

let rec find_possible_card color num eff hand =
  match hand with
  | [] -> None
  | h::t -> begin
      if h.color = color then Some h else
      if h.value = num then Some h else
      if h.effect = eff && h.effect <> NoEffect then Some h else
        find_possible_card color num eff t
    end

(* this no longer works
let dumbai_choose_card top_card hand =
  let exists_card = find_possible_card (top_card.color) (top_card.value) (top_card.effect) hand in
  match exists_card with
  | None -> begin
      match (does_wild4_exist hand) with
      | None -> Draw (* {value = -1; color = NoColor; effect = NoEffect; id = -1} *)
      | Some x -> Play x; Choose Red (* {value = -1; color = Red; effect = x.effect; id = x.id} *)
    end
                                          | Some h -> if List.length hand = 2 then Uno h else Play h
                                        *)

(* returns a list of all the possible cards you could play
 * given a top_card. Includes cards of the same color, value, effect, or
 * Wild cards. (implemented in a way that reverses original order)*)
let rec get_possible_list hand top_card lst =
  (* match hand with
  | [] -> lst
  | h::t -> if h.color = Black then get_possible_list t top_card (h::lst) else
    if (h.value <> -1) then
      if h.value = top_card.value || h.color = top_card.color then
        get_possible_list t top_card (h::lst) else
        get_possible_list t top_card lst else
    if h.color = top_card.color || (h.effect = top_card.effect && h.effect <> NoEffect) then
      get_possible_list t top_card (h::lst) else
      get_possible_list t top_card lst *)
  match hand with
  | [] -> lst
  | h::t -> if h.color = Black then get_possible_list t top_card (h::lst) else
    if h.color = top_card.color then get_possible_list t top_card (h::lst) else
    if h.effect = NoEffect && h.value = top_card.value then get_possible_list t top_card (h::lst) else
    if h.effect = top_card.effect && h.effect <> NoEffect then get_possible_list t top_card (h::lst) else
      get_possible_list t top_card lst

(* counts the number of colors you have in your hand and stores each value in
 * a tuple in the order Y,G,B,R,Bl. *)
let rec color_count hand tup =
  match hand with
  | [] -> tup
  | h::t -> begin
      match tup with
      | (y,g,b,r,bl) -> if h.color = Yellow then color_count t (y+1,g,b,r,bl) else
        if h.color = Green then color_count t (y,g+1,b,r,bl) else
        if h.color = Blue then color_count t (y,g,b+1,r,bl) else
        if h.color = Red then color_count t (y,g,b,r+1,bl) else
          color_count t (y,g,b,r,bl+1)
    end

(* finds out which color you have the most of, and returns an integer representing
 * that particular color. *)
let most_color tup =
  match tup with
  | (y,g,b,r,_) -> if (max (max (max y g) b) r) = y then Yellow else
    if (max (max (max y g) b) r) = g then Green else
    if (max (max (max y g) b) r) = b then Blue else
    if (max (max (max y g) b) r) = r then Red else Black

(* gets a list of cards already played, extracting each card from the stack.
 * Returns the elements to the list in the order opposite that of the stack. *)
let rec board_stats play_pile lst =
  let pp = Stack.copy play_pile in
  match Stack.is_empty pp with
  | true -> lst
  | _ -> let t = (Stack.pop pp) in board_stats pp (t::lst)

(* extracting values in a 5 tuple. *)
let fir tup =
  match tup with
  | (t,_,_,_,_) -> t
let sen tup =
  match tup with
  | (_,t,_,_,_) -> t
let trd tup =
  match tup with
  | (_,_,t,_,_) -> t
let frt tup =
  match tup with
  | (_,_,_,f,_) -> f
let fif tup =
  match tup with
  | (_,_,_,_,f) -> f

let rec find_max lst num c =
  match lst with
  | [] -> c
  | (n,ca)::t -> if n > num then find_max t n ca else find_max t num c

let call_color lst =
  match most_color (color_count lst (0,0,0,0,0)) with
  | Black -> Player.Red
  | Yellow -> Player.Yellow
  | Green -> Player.Green
  | Blue -> Player.Blue
  | _ -> Player.Red

let helper_best lst  =
  if List.length lst = 0 then Draw else
    let best_card = find_max lst 0 ({value = -1; color = NoColor; effect = NoEffect; id = -1}) in begin
    (* let best_color = call_color (snd (List.split lst)) in begin *)
      match List.length lst with
      | 2 -> begin
          match best_card.effect with
          | Wild -> Uno best_card; Choose (call_color (snd (List.split lst)))
          | Wild4 -> Uno best_card; Choose (call_color (snd (List.split lst)))
          | _ -> Uno best_card
      end
      | _ -> begin
          match best_card.effect with
          | Wild -> Play best_card; Choose (call_color (snd (List.split lst)))
          | Wild4 -> Play best_card; Choose (call_color (snd (List.split lst)))
          | _ -> Play best_card
        end
    end


    (* if List.length lst = 2 then if (best_card.effect = Wild || best_card.effect = Wild4) then Uno best_card; Choose best_color; else Uno best_card
    else if (best_card.effect = Wild || best_card.effect = Wild4) then Play best_card; Choose best_color; else Play best_card *)

    (* if List.length lst = 2 then Uno (find_max lst (fst h) (snd h)) else
      Play (find_max lst (fst h) (snd h)) (* take UNO into acct, change color *) *)


let rec find_best_card hand c top_card num_h s lst =
  match hand with
  | [] -> helper_best lst
  | h::t ->
    if (next_turn s = 0) then
      if (h.effect = Skip || h.effect = Plus || h.effect = Reverse) then find_best_card t c top_card num_h s ((50,h)::lst)
      else if (h.effect = Wild4) then if (num_h <= 3) then find_best_card t c top_card num_h s ((100,h)::lst) else find_best_card t c top_card num_h s ((50,h)::lst)
      else if (h.effect = Wild) then if (List.length hand <= 3) then find_best_card t c top_card num_h s ((75,h)::lst) else find_best_card t c top_card num_h s ((20,h)::lst)
      (* else NoEffect *)
      else if (h.value = 0) then if (h.color = c) then find_best_card t c top_card num_h s ((40,h)::lst) else find_best_card t c top_card num_h s ((30,h)::lst)
      else if (h.color = c) then find_best_card t c top_card num_h s ((25,h)::lst) else find_best_card t c top_card num_h s ((20,h)::lst)
    else if (next_next_turn s = 0) then
      if (h.effect = Skip || h.effect = Plus || h.effect = Wild4) then find_best_card t c top_card num_h s ((10,h)::lst)
      else if (h.effect = Reverse) then find_best_card t c top_card num_h s ((20,h)::lst)
      else if (h.effect = Wild) then if (List.length hand <= 3) then find_best_card t c top_card num_h s ((50,h)::lst) else find_best_card t c top_card num_h s ((20,h)::lst)
      (* else NoEffect *)
      else if (h.value = 0) then if (h.color = c) then find_best_card t c top_card num_h s ((60,h)::lst) else find_best_card t c top_card num_h s ((55,h)::lst) else find_best_card t c top_card num_h s ((52,h)::lst)
    else
      if (h.effect = Reverse || h.effect = Wild4) then find_best_card t c top_card num_h s ((5,h)::lst)
      else if (h.effect = Skip || h.effect = Plus) then find_best_card t c top_card num_h s ((15,h)::lst)
      else if (h.effect = Wild) then find_best_card t c top_card num_h s ((25,h)::lst)
      else if (h.value = 0) then if (h.color = c) then find_best_card t c top_card num_h s ((70,h)::lst) else find_best_card t c top_card num_h s ((60,h)::lst)
      else find_best_card t c top_card num_h s ((50,h)::lst)

  (* match hand with
  | [] -> play_best lst
  | h::t -> if (State.next_turn st = 0 && h.effect <> NoEffect) then
      if (h.effect = Wild4 || h.effect = Plus || h.effect = Skip) then find_best_card t c top_card num_h st ((150,h)::lst) else
      find_best_card t c top_card num_h st ((100,h)::lst) else
    if (h.value = 0) then find_best_card t c top_card num_h st ((30,h)::lst) else
    if ((h.color = top_card.color) && h.value <> -1) then
      let play_tup = color_count (board_stats (played_pile st) []) (0,0,0,0,0) in begin
        match h.color with
        | Yellow -> let add_n = (fir play_tup) in
          if h.color = c then
             find_best_card t c top_card num_h st (((h.value + add_n + 10),h)::lst)
          else find_best_card t c top_card num_h st (((h.value + add_n),h)::lst)
        | Green -> let add_n = sen play_tup in
          if h.color = c then
            find_best_card t c top_card num_h st (((h.value + add_n + 10),h)::lst)
          else find_best_card t c top_card num_h st (((h.value + add_n),h)::lst)
        | Blue -> let add_n = trd play_tup in
          if h.color = c then
            find_best_card t c top_card num_h st (((h.value + add_n + 10),h)::lst)
          else find_best_card t c top_card num_h st (((h.value + add_n),h)::lst)
        | Red -> let add_n = frt play_tup in
          if h.color = c then
             find_best_card t c top_card num_h st (((h.value + add_n + 10),h)::lst)
          else find_best_card t c top_card num_h st (((h.value + add_n),h)::lst)
        | Black -> if List.length hand > 4 then find_best_card t c top_card num_h st ((10,h)::lst) else
            find_best_card t c top_card num_h st ((75,h)::lst)
        | _ -> failwith "Impossible"
      end
        (* for effect cards *)
    else
      let next_player_cards = List.length ((List.nth (players st) (next_turn st)).hand) in
      if next_player_cards = 1 then
        find_best_card t c top_card num_h st ((50,h)::lst) else
      if next_player_cards < 4 then
        find_best_card t c top_card num_h st ((40,h)::lst) else
        find_best_card t c top_card num_h st ((25,h)::lst) *)

let smartai_choose_card s =
  let curr = current_player s in let hand = curr.hand in
  let get_human = List.find (fun x -> x.id = 0) (players s) in
  let num_h_cards = List.length (get_human.hand) in
  let possible_plays = get_possible_list hand (top_card s) [] in
  let curr_hand_stats = color_count possible_plays (0,0,0,0,0) in
  begin
    match most_color (curr_hand_stats) with
    | Yellow -> find_best_card possible_plays Yellow (top_card s) num_h_cards s []
    | Green -> find_best_card possible_plays Green (top_card s) num_h_cards s []
    | Blue -> find_best_card possible_plays Blue (top_card s) num_h_cards s []
    | Red -> find_best_card possible_plays Red (top_card s) num_h_cards s []
    | _ -> find_best_card possible_plays Black (top_card s) num_h_cards s []
  end
