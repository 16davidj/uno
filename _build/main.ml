open State
open Command
open Player

let info = "UNO info goes here"

(*
let rec repl_loop input state =
  match parse input with
  | Play c -> (*update_gui;*) repl_loop (read_line ()) update_state (Play c) state
  | Draw i -> (*update_gui;*) repl_loop (read_line ()) update_state (Draw i) state;
  | Choose col -> (*update_gui;*) repl_loop (read_line ()) update_state (Choose col) state;
  | Info -> print_endline info;
  | UNO of c -> failwith("unimplemented")
  | Quit -> exit 0
  | NA -> print_endline("\n**Console**: not a valid command"); repl_loop (read_line ()) state;
*)

  let main () = Graphics.open_graph " 1280x720";
    Graphics.set_window_title "Uno";
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
    draw_image (Png.load_as_rgb24 "assets/cards/sidewaybackcard.png" []) 1025 315;
    draw_image (Png.load_as_rgb24 "assets/cards/sidewaybackcard.png" []) 1025 335;
    draw_image (Png.load_as_rgb24 "assets/cards/sidewaybackcard.png" []) 1025 355;
    draw_image (Png.load_as_rgb24 "assets/cards/sidewaybackcard.png" []) 1025 375;
    draw_image (Png.load_as_rgb24 "assets/cards/sidewaybackcard.png" []) 1025 395;
    draw_image (Png.load_as_rgb24 "assets/cards/sidewaybackcard.png" []) 1025 415;
    draw_image (Png.load_as_rgb24 "assets/cards/sidewaybackcard.png" []) 1025 435;

    moveto 450 700; draw_string "Player 3";
    draw_image (Png.load_as_rgb24 "assets/player3.png" []) 425 590;
    draw_image (Png.load_as_rgb24 "assets/cards/upsidebackcard.png" []) 535 589;
    draw_image (Png.load_as_rgb24 "assets/cards/upsidebackcard.png" []) 555 589;
    draw_image (Png.load_as_rgb24 "assets/cards/upsidebackcard.png" []) 575 589;
    draw_image (Png.load_as_rgb24 "assets/cards/upsidebackcard.png" []) 595 589;
    draw_image (Png.load_as_rgb24 "assets/cards/upsidebackcard.png" []) 615 589;
    draw_image (Png.load_as_rgb24 "assets/cards/upsidebackcard.png" []) 635 589;
    draw_image (Png.load_as_rgb24 "assets/cards/upsidebackcard.png" []) 655 589;

    moveto 250 160; draw_string "Frank";
    draw_image (Png.load_as_rgb24 "assets/frank.png" []) 225 175;
    draw_image (Png.load_as_rgb24 "assets/cards/sidewaybackcard2.png" []) 225 290;
    draw_image (Png.load_as_rgb24 "assets/cards/sidewaybackcard2.png" []) 225 310;
    draw_image (Png.load_as_rgb24 "assets/cards/sidewaybackcard2.png" []) 225 330;
    draw_image (Png.load_as_rgb24 "assets/cards/sidewaybackcard2.png" []) 225 350;
    draw_image (Png.load_as_rgb24 "assets/cards/sidewaybackcard2.png" []) 225 370;
    draw_image (Png.load_as_rgb24 "assets/cards/sidewaybackcard2.png" []) 225 390;
    draw_image (Png.load_as_rgb24 "assets/cards/sidewaybackcard2.png" []) 225 410;

    draw_human_hand (init_state);

    let () = main ()
