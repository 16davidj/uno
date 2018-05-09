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

let dumbai_choose_card top_card hand =
  let exists_card = find_possible_card (top_card.color) (top_card.value) (top_card.effect) hand in
  match exists_card with
  | None -> begin
      match (does_wild4_exist hand) with
      | None -> Draw (* {value = -1; color = NoColor; effect = NoEffect; id = -1} *)
      | Some x -> Play x; Choose Red (* {value = -1; color = Red; effect = x.effect; id = x.id} *)
    end
  | Some h -> Play h

(* returns a list of all the possible cards you could play
 * given a top_card. *)
let rec get_possible_list hand top_card lst =
  match hand with
  | [] -> lst
  | h::t -> if h.color = Black then get_possible_list t top_card (h::lst) else
    if top_card.value = -1 then
      if (top_card.color = h.color ||
          (top_card.effect = h.effect && top_card.effect <> NoEffect)) then
        get_possible_list t top_card (h::lst) else
        get_possible_list t top_card lst else
    if h.color = Black || (h.value = top_card.value && h.value <> -1) then
      get_possible_list t top_card (h::lst) else
      get_possible_list t top_card lst

(* counts the number of colors you have in your hand and stores each value in
 * a tuple. *)
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
  | (y,g,b,r,_) -> if (max (max (max y g) b) r) = y then 1 else
    if (max (max (max y g) b) r) = g then 2 else
    if (max (max (max y g) b) r) = b then 3 else
    if (max (max (max y g) b) r) = r then 4 else 5

(* let find_best_card hand color top_card num_h =


   let smartai_choose_card top_card hand state =
    let get_human = List.find (fun x -> x.id = 0) state.players in
    let num_h_cards = List.length get_human.hand in
    let possible_plays = get_possible_list hand top_card [] in
    let curr_hand_stats = color_count possible_plays (0,0,0,0,0) in
    begin
      match most_color (curr_hand_stats) with
      | 1 -> find_best_card possible_plays Yellow top_card num_h_cards
      | 2 -> find_best_card possible_plays Green top_card num_h_cards
      | 3 -> find_best_card possible_plays Blue top_card num_h_cards
      | 4 -> find_best_card possible_plays Red top_card num_h_cards
      | _ -> find_best_card possible_plays Black top_card num_h_cards
    end *)
