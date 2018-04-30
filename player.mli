(* [effect] represents the type of special effect a card has. *)
type effect = Add | Skip | Draw | Reverse | None | Wild | Wild4
                (* wtf is add? *)

(* [color] represents the color of a card *)
type color = Red | Green | Blue | Yellow | Black | None

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

module DumbAI = PlayerMaker

(* module AI : Player

module Human : Player *)
