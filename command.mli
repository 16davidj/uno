(* [command] represents a command input by a player. *)
type command =
  | Play of int             (* plays a card in your hand with the int "id" you specify.
                             * "id" calculated from the string as follows:
                               * Y:       10 + face/action value
                               * G:       20 + face/action value
                               * B:       30 + face/action value
                               * R:       40 + face/action value
                               * +2:      50 + color value/10
                               * Skip:    60 + color value/10
                               * Reverse: 70 + color value/10
                               * W:       80
                               * W4:      90 *)
  | Draw                    (* draws a card from the draw pile and place it in your hand *)
  | Info 										(* prints information about the state of the game *)
  | Hand 										(* prints information about your hand *)
  | Challenge               (* valid command only when a WILD +4 is played *)
  | UNO                     (* NOT TOO SURE how this will be implemented since the user
                             * would have to type in 2 commands *)
  | NA                      (* For invalid commands *)
  | Quit                    (* Quit the game *)

(* [parse str] is the command that represents player input [str].
 * The command is of type int because the int signifies which
 * card the user would like to place.
 * requires: [str] is a card in the player's hand. *)
val parse : string -> command
