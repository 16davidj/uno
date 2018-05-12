open State
open Command
open Player
open Graphics
open Gui

let update_user_hand updated_s = fill_rect 535 0 745 183;
  (draw_human_hand (cardlst_to_png updated_s) 535 75)

let update_ai1_hand updated_s = fill_rect 1025 315 720 405;
  draw_ai1_hand (ai1_hand updated_s) 1025 315

let update_ai2_hand updated_s = fill_rect 525 589 335 108;
  draw_ai2_hand (ai2_hand updated_s) 535 589

let update_ai3_hand updated_s = fill_rect 225 290 108 430;
  draw_ai3_hand (ai3_hand updated_s) 225 290

let update_arrow updated_s = draw_circle (updated_s)

let update_turn updated_s = set_color green;
  let p = current_player updated_s in match p.id with
  | 0 -> moveto 450 185; draw_string "Player 1";
         set_color black;
    moveto 1050 185; draw_string "Player 2";
    moveto 450 700; draw_string "Player 3";
    moveto 250 160; draw_string "Frank";
  | 1 -> moveto 1050 185; draw_string "Player 2";
    set_color black;
    moveto 450 185; draw_string "Player 1";
    moveto 450 700; draw_string "Player 3";
    moveto 250 160; draw_string "Frank";
  | 2 -> moveto 450 700; draw_string "Player 3";
    set_color black;
    moveto 450 185; draw_string "Player 1";
    moveto 1050 185; draw_string "Player 2";
    moveto 250 160; draw_string "Frank";
  | 3 -> moveto 250 160; draw_string "Frank";
    set_color black;
    moveto 450 185; draw_string "Player 1";
    moveto 1050 185; draw_string "Player 2";
    moveto 450 700; draw_string "Player 3";
  | _ -> failwith("player does not exist")

let update_stack updated_s =
  draw_image (Png.load_as_rgb24 (card_to_str (top_card updated_s)) []) 565 300

let update_hand old_s updated_s =
  set_color 0xb30000;
  if user_hand old_s != user_hand updated_s then
    update_user_hand updated_s
  else if ai1_hand old_s != ai1_hand updated_s then
    update_ai1_hand updated_s
  else if ai2_hand old_s != ai2_hand updated_s then
    update_ai2_hand updated_s
  else if ai3_hand old_s != ai3_hand updated_s then
    update_ai3_hand updated_s

  let update_gui cmd old_s updated_s = match cmd with
  | Play c ->
    update_hand old_s updated_s;
    update_arrow updated_s;
    update_stack updated_s;
    update_turn updated_s
  | Draw -> update_hand old_s updated_s; update_arrow updated_s;
    update_turn updated_s;
  | _ -> ()

let rec one_row hand x y acc = begin match hand with
  | [] -> List.rev acc
  | h :: [] -> List.rev ((((x, y), ((x+69), (y+108))), h) :: acc)
  | h :: t -> one_row t (x+30) y ((((x, y), ((x+30), (y+108))), h) :: acc)
end

let rec mult_row row_lst x y =
  begin match row_lst with
  | [] -> []
  | h :: [] -> one_row h 535 y []
  | h :: t -> begin match h with
      | [] -> begin match t with
          | [] -> []
          | h1 :: t1 -> mult_row (h1 :: t1) 535 (y-40)
        end
      | h1 :: t1 -> (((x, y+68), ((x+30), (y+40))), h1) :: (mult_row (t1 :: t) (x+30) y)
    end
end

let rec get_first_n lst n acc = if n = 0 then ((List.rev acc), lst) else
  begin match lst with
    | [] -> (List.rev acc), lst
    | h :: t -> get_first_n t (n-1) (h :: acc)
  end

let rec split_lst lst acc n =
  if n = 0 then List.rev acc else
    begin match lst with
      | [] -> List.rev acc
      | h :: t -> let (fstn, rest) = (get_first_n (h :: t) 19 []) in
        split_lst rest (fstn :: acc) (n-1)
    end

let convert_hand_to_pos human_hand =
  let lst = split_lst human_hand [] (((List.length human_hand)/19) + 1) in
  mult_row lst 535 75

let in_position status position =
  let coordinates = fst position in
  status.mouse_x >= fst (fst coordinates) && status.mouse_x <= fst (snd coordinates)
     && status.mouse_y >= snd (fst coordinates) && status.mouse_y <= snd (snd coordinates)

let convert_statustocmd status positions =
  try let (_, card) = (List.find (in_position status) positions)
  in Play card with
| _ -> if (status.mouse_x >= 850 && status.mouse_x <= 950 && status.mouse_y >= 291
           && status.mouse_y <= 447) then Draw else NA

let parse_click curr_hand =
  let positions = (convert_hand_to_pos curr_hand) in
  convert_statustocmd (wait_next_event [Button_down]) positions

let rec repl_loop s =
  let curr = current_player s in
  if curr.id != 0 then (Unix.sleep 1);
  let cmd =
    if curr.id = 0 then parse_click curr.hand else Ai.smartai_choose_card s in
  let updated_s = update_state cmd s in
  begin match cmd with
    | Play c ->
      if updated_s != s then update_gui (Play c) s updated_s;
      let top = State.top_card updated_s in
      print_endline(string_of_int top.value);
      repl_loop updated_s;
      if updated_s = s then print_endline("Play a valid card");
      repl_loop s;
    | Draw -> update_gui (Draw) s updated_s;
      repl_loop updated_s;
    | Choose col -> update_gui (Choose col) s updated_s;
      repl_loop updated_s;
    | Info -> print_endline("info goes here");
    | Uno c -> failwith("unimplemented");
    | Quit -> exit 0;
    | _ -> print_endline("\n**Console**: not a valid command");
      repl_loop s;
  end

  let main () = open_graph " 1280x720";
    set_window_title "Uno";
    set_color 0xb30000;
    fill_rect 0 0 1280 720;

    (*drawing the draw pile *)
    draw_image (Png.load_as_rgb24 "assets/draw.png" []) 850 300;
    draw_image (Png.load_as_rgb24 "assets/draw.png" []) 847 297;
    draw_image (Png.load_as_rgb24 "assets/draw.png" []) 844 294;
    draw_image (Png.load_as_rgb24 "assets/draw.png" []) 841 291;

    set_color black;
    moveto 450 185;
    draw_string "Player 1";
    draw_image (Png.load_as_rgb24 "assets/player1.png" []) 425 75;

    moveto 1050 185;
    draw_string "Player 2";
    draw_image (Png.load_as_rgb24 "assets/player2.png" []) 1025 205;

    moveto 450 700;
    draw_string "Player 3";
    draw_image (Png.load_as_rgb24 "assets/player3.png" []) 425 590;

    moveto 250 160;
    draw_string "Frank";
    draw_image (Png.load_as_rgb24 "assets/frank.png" []) 225 175;

    Random.self_init();
    init_pile ();
    draw_state init_state;
    update_turn init_state;
    repl_loop init_state

    let () = main ()
