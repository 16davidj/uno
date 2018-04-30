type effect = Add | Skip | Draw | Reverse | No | Wild | Wild4
(* wtf is add? *)

(* [color] represents the color of a card *)
type color = Red | Green | Blue | Yellow | Black | No

(* [card] represents a card in the UNO game*)
type card =
  {value: int; color: color; effect: effect; id: int}

(* This module helps differentiate between AI and Human*)
module type Intelligence = sig
  val choose_card : card -> card list -> card
  type t
end

(* [Player] is a module representing a player in the game *)
module type Player = sig
  module Intel : Intelligence
  type player_type = Intel.t
  val id : int
  (* [name] is the name of the player *)
  val name : string
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

module type PlayerMaker = functor(T: Intelligence) ->
  Player with module Intel = T

module DumbAI = functor (T : Intelligence) -> struct
  type t = T.t

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

  let choose_card top_card hand =
    let exists_card = find_possible_card (top_card.color) (top_card.value) (top_card.effect) hand in
    match exists_card with
    | None -> begin
        match (does_wild4_exist hand) with
        | None -> {value = -1; color = No; effect = No; id = -1}
        | Some x -> x
    end
    | Some h -> h
end
