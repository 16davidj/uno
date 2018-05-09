open State
open Command
open Player
open Graphics
open Gui

let rec loop () = loop ()

let info = "UNO info goes here"

let update_user_hand updated_s = fill_rect 535 75 745 108;
  (draw_human_hand (cardlst_to_png updated_s) 535)

let update_ai1_hand updated_s = fill_rect 1025 315 108 405;
  draw_ai1_hand (ai1_hand updated_s) 315

let update_ai2_hand updated_s = fill_rect 525 589 535 108;
  draw_ai2_hand (ai2_hand updated_s) 535

let update_ai3_hand updated_s = fill_rect 225 290 108 430;
  draw_ai3_hand (ai3_hand updated_s) 290

let update_arrow updated_s = draw_circle (updated_s)

let update_curr updated_s =
  set_color 0xb30000;
  fill_rect 10 10 200 10;
  moveto 10 10;
  set_color black;
  let curr = (current_player updated_s) in draw_string ("Current player: " ^ curr.name)

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
    update_curr updated_s;
    update_stack updated_s;
  | Draw -> update_hand old_s updated_s; update_arrow updated_s; update_curr updated_s;
  | _ -> ()


let rec repl_loop input s = let updated_s = update_state (parse input) s in
  begin match (parse input) with
  | Play c -> update_gui (Play c) s updated_s;
              repl_loop (read_line ()) updated_s;
  | Draw -> update_gui (Draw) s updated_s;
            repl_loop (read_line ()) updated_s;
  | Choose col -> update_gui (Choose col) s updated_s;
                  repl_loop (read_line ()) updated_s;
  | Info -> print_endline info;
  | Uno c -> failwith("unimplemented");
  | Quit -> exit 0;
  | _ -> print_endline("\n**Console**: not a valid command");
    repl_loop (read_line ()) s;
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
    repl_loop (read_line ()) init_state

    let () = main ()
