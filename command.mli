(* [command] represents a command input by a player. *)
type command = int

(* [parse str] is the command that represents player input [str].
 * The command is of type int because the int signifies which
 * card the user would like to place.
 * requires: [str] is a card in the player's hand. *)
val parse : string -> command
