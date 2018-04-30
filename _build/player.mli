
(* [effect] represents the type of special effect a card has. *)
type effect

(* [color] represents the color of a card *)
type color

(* [card] represents a card in the UNO game*)
type card

type intelligence

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
