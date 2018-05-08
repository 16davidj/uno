open Queue

(* [state] is an abstract type representing the state of the game. *)
type state

(* [card] is an abstract type representing a card in the UNO game*)
type card

(* [user] is an abstract type representing a player in the game *)
type player

(* [user_hand] is an object of type card list that represents
 * the human player's hand *)
val user_hand : state -> card list

(* [ai1_hand] is an object of type card list that represents
 * AI_1's hand*)
val ai1_hand : state -> card list

(* [ai2_hand] is an object of type card list that represents
 * AI_2's hand*)
val ai2_hand : state -> card list

(* [ai3_hand] is an object of type card list that represents
 * AI_3's hand*)
val ai3_hand : state -> card list

(* [init_state] is an object of type state that represents
 * the starting state of a game of UNO *)
val init_state : state

(* [turn] returns an int corresponding with which player's turn
 * it is in the current state. 0 will be human's turn  *)
val turn : state -> int

(* [next_turn] returns an int corresponding with which player's turn
 * it is for the next turn. 0 will represent the human's turn  *)
val next_turn : state -> int

(* [draw_ouke] returns the current draw_pile  *)
val draw_pile : state -> card Queue.t

(* [top_card] returns an object of type card that represents
 * the card played in the previous turn on top of the stack *)
val top_card : state -> card

(* [update_state] returns an object of type state that represents
 * the new state of the game after player (user) chooses a card
 * from hand (card list) *)
val update_state : Command.command -> state -> state

(* [get_winner] returns an int corresponding to a player if
 * there is a winner in the current state, returns -1 otherwise *)
val get_winner : state -> int
