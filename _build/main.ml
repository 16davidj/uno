open State
open Command
open Player
open Graphics
open Gui

let rec loop () = loop ()

let info = "UNO info goes here"

let update_gui cmd old_s updated_s = () (*begin match cmd with
  | Play c -> if is_counter s then draw_counter () else draw_clock ();
    Png.load_as_rgb24 (card_to_str c) [] 640 360;
                                          end *)

let rec repl_loop input s = let updated_s = update_state (parse input) s in
  begin match (parse input) with
  | Play c -> update_gui (Play c) s updated_s; repl_loop (read_line ()) updated_s;
  | Draw -> update_gui (Draw) s updated_s; repl_loop (read_line ()) updated_s;
  | Choose col -> update_gui (Choose col) s updated_s; repl_loop (read_line ()) updated_s;
  | Info -> print_endline info;
  | Uno c -> failwith("unimplemented")
  | Quit -> exit 0
  | _ -> print_endline("\n**Console**: not a valid command"); repl_loop (read_line ()) s;
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

    set_color black; moveto 450 185; draw_string "Player 1";
    draw_image (Png.load_as_rgb24 "assets/player1.png" []) 425 75;

    moveto 1050 185; draw_string "Player 2";
    draw_image (Png.load_as_rgb24 "assets/player2.png" []) 1025 205;

    moveto 450 700; draw_string "Player 3";
    draw_image (Png.load_as_rgb24 "assets/player3.png" []) 425 590;

    moveto 250 160; draw_string "Frank";
    draw_image (Png.load_as_rgb24 "assets/frank.png" []) 225 175;

    draw_state init_state;
    loop ()

    let () = main ()
