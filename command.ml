
(* An UNO module responsible for the text-based commands and
 * implementation for parsing of the commands. *)

type command =
  | Play of int
  | Draw
  | Info
  | Hand
  | Challenge
  | UNO
  | NA
  | Quit

(* User input when entering their command must account for
 * whatever they type in. This normalizes whatever they type
 * in into a readable string *)
let normalize str =
  String.trim (String.lowercase_ascii str)

(* Returns the "command" part of the raw user input, or
 * equivalently, the characters before the first white
 * space ' '. *)
let get_command str =
  let n_str = normalize str in
  if String.contains n_str ' ' then
    let find_space = String.index n_str ' ' in
    String.sub n_str 0 find_space
  else n_str

(* Returns the arguments for a particular command. It may be a
 * string, or may be empty if there is no particular
 * argument for that command. Returns args in a string format. *)
let get_args str =
  let n_str = normalize str in
  let command_len = String.length (get_command n_str) in
  String.trim (String.sub n_str command_len (String.length n_str - command_len))

(* Parses user-input string into a command and/or its arguments. *)
let parse str =
  match (get_command str) with
  | "play" -> Play (get_args str)
  | "draw" -> Draw
  | "info" -> Info
  | "hand" -> Hand
  | "challenge" -> Challenge
  | "uno" -> UNO
  | "quit" -> Quit
  | _ -> NA

(* let rec repl s =
   if get_winner s <> 0 then print_endline "Game is over" else
   let user_input = read_line() in
   try
    match parse user_input with
    | Play i -> let new_state = State.update_state (parse user_input) s in
                if new_state = s then print_endline "Invalid card value"; repl s
                else repl new_state
    | Draw ->
    | Info ->
    | Hand ->
    | Challenge ->
    | Uno ->
    | Quit -> exit 0
    | NA ->
   with
    | _ -> print_endline "Not a valid input. Try again"; repl s  *)
