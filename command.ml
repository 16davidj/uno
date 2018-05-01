 (* An UNO module responsible for the text-based commands and
 * implementation for parsing of the commands. *)
open Player

type command =
  | Play of card
  | Draw of int
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

(* Helper function for parse_args, which shortens our code by determining the
 * effect of the int id of the card ONLY when d is between 50 and 80. *)

let det_effect num : Player.effect =
  if num >= 50 && num < 80 then begin
    match num / 10 with
    | 5 -> Plus
    | 6 -> Skip
    | 7 -> Reverse
    | _ -> NoEffect
  end
  else NoEffect

let convert_color str =
  match str with
  | "yellow" -> 10
  | "green" -> 20
  | "blue" -> 30
  | "red" -> 40
  | "black" -> 0
  | _ -> -1

let convert_to_id arg =
  if String.contains arg ' ' then
    let get_color = get_command arg in let get_num_or_action = get_args arg in
    try
      let num = int_of_string get_num_or_action in
      if num >= 0 && num < 10 then num + (convert_color get_color) else -1
    with
    | _ -> begin
        match get_num_or_action with
        | "draw2" -> 50 + (convert_color get_color)/10
        | "skip" -> 60 + (convert_color get_color)/10
        | "reverse" -> 70 + (convert_color get_color)/10
        | "wild" -> 80
        | "wild4" -> 90
        | _ -> -1
      end
  else -1

(* Converts the int id of the card into the actual card record itself. *)
let rec parse_args arg =
  try
    let id_1 = int_of_string arg in
    begin
      match id_1 with
      | d when d >= 10 && d < 20 -> {value = d mod 10; color = Yellow; effect = NoEffect; id = d}
      | d when d >= 20 && d < 30 -> {value = d mod 10; color = Green; effect = NoEffect; id = d}
      | d when d >= 30 && d < 40 -> {value = d mod 10; color = Blue; effect = NoEffect; id = d}
      | d when d >= 40 && d < 50 -> {value = d mod 10; color = Red; effect = NoEffect; id = d}
      | d when d >= 50 && d < 80 -> begin
        let eff = det_effect d in
        match d mod 10 with
          | 1 -> {value = -1; color = Yellow; effect = eff; id = d}
          | 2 -> {value = -1; color = Green; effect = eff; id = d}
          | 3 -> {value = -1; color = Blue; effect = eff; id = d}
          | 4 -> {value = -1; color = Red; effect = eff; id = d}
          | _ -> {value = -1; color = NoColor; effect = eff; id = -1}
      end
      | d when d = 80 -> {value = -1; color = Black; effect = Wild; id = d}
      | d when d = 90 -> {value = -1; color = Black; effect = Wild4; id = d}
      | _ -> {value = -1; color = NoColor; effect = NoEffect; id = -1}
  end
  with
  | _ -> parse_args (string_of_int (convert_to_id arg))

(* Parses user-input string into a command and/or its arguments. *)
let parse str =
  match (get_command str) with
  | "play" -> Play (parse_args (get_args str))
  | "draw" -> Draw (int_of_string (get_args str))
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
