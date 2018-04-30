type effect = Add | Skip | Draw | Reverse | No | Wild | Wild4

(* [color] represents the color of a card *)
type color = Red | Green | Blue | Yellow | Black | No

type intelligence = AI | Human

(* [card] represents a card in the UNO game*)
type card = {value: int; color: color; effect: effect; id: int}

(* [Player] is a module representing a player in the game *)
module type Player = sig
  val id : int
  (* [name] is the name of the player *)
  val name : string
  val intelligence : intelligence

  (* [hand] is a list of cards representing a player's hand *)
  val hand : card list
  (* [choose_card] returns the card that will be played by the
   * player given a top_card and a hand. This function will vary
   * depending on the type of player: AI or human *)
  val choose_card : card -> card list -> card
  (* [cards_left] returns the number of cards left in the
   * player's hand *)
  val cards_left : card list -> int
end

let rec does_wild4_exist hand =
  match hand with
  | [] -> None
  | h::t -> if h.effect = Wild4 then Some h else does_wild4_exist t

let rec find_possible_card color num eff hand =
  match hand with
  | [] -> None
  | h::t -> begin
      if h.color = color then Some h else
      if h.value = num then Some h else
      if h.effect = eff then Some h else
        find_possible_card color num eff t
    end

let dumbai_choose_card top_card hand =
  let exists_card = find_possible_card (top_card.color) (top_card.value) (top_card.effect) hand in
  match exists_card with
  | None -> begin
      match (does_wild4_exist hand) with
      | None -> {value = -1; color = No; effect = No; id = -1}
      | Some x -> x
    end
  | Some h -> h

module DumbAI = struct
  let id = 1
  let name = "ai"
  let hand = []
  let intelligence = AI
  let choose_card top hand = dumbai_choose_card
  let cards_left lst = List.length lst
end
