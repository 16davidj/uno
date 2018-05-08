open State
open Graphics
open Command
open Camlimages
open Images

let rec loop ()  = loop ()

(* Note: this function was taken from the original source code of Camlimages,
   graphic_image.ml
 Source:https://bitbucket.org/camlspotter/camlimages/src/1611545463f493462aeafab65839c1112162559a?at=default*)
let array_of_image img =
  match img with
  | Images.Index8 bitmap ->
    let w = bitmap.Index8.width
    and h = bitmap.Index8.height
    and colormap = bitmap.Index8.colormap.map in
    let cmap = Array.map (fun {r = r; g = g; b = b} -> Graphics.rgb r g b) colormap in
    if bitmap.Index8.transparent <> -1 then
      cmap.(bitmap.Index8.transparent) <- transp;
    Array.init h (fun i ->
        Array.init w (fun j -> cmap.(Index8.unsafe_get bitmap j i)))
  | Index16 bitmap ->
    let w = bitmap.Index16.width
    and h = bitmap.Index16.height
    and colormap = bitmap.Index16.colormap.map in
    let cmap = Array.map (fun {r = r; g = g; b = b} -> rgb r g b) colormap in
    if bitmap.Index16.transparent <> -1 then
      cmap.(bitmap.Index16.transparent) <- transp;
    Array.init h (fun i ->
        Array.init w (fun j -> cmap.(Index16.unsafe_get bitmap j i)))
  | Rgb24 bitmap ->
    let w = bitmap.Rgb24.width
    and h = bitmap.Rgb24.height in
    Array.init h (fun i ->
        Array.init w (fun j ->
            let {r = r; g = g; b = b} = Rgb24.unsafe_get bitmap j i in
            rgb r g b))
  | Rgba32 _ | Cmyk32 _ -> failwith "RGBA and CMYK not supported"


let of_image img = Graphics.make_image (array_of_image img)

let draw_image img x y = Graphics.draw_image (of_image img) x y

let image_of grpimg =
  let rgb_of_color color =
    { r = (color lsr 16) land 0xFF;
      g = (color lsr 8) land 0xFF;
      b = color land 0xFF; } in
  let array = Graphics.dump_image grpimg in
  let height = Array.length array in
  let width = Array.length array.(0) in
  let img = Rgb24.create width height in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      Rgb24.unsafe_set img x y (rgb_of_color array.(y).(x))
    done
  done;
  img

let () = Graphics.open_graph " 1280x720";
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
  draw_image (Png.load_as_rgb24 "assets/cards/blue0.png" []) 535 75;
  draw_image (Png.load_as_rgb24 "assets/cards/yellow3.png" []) 565 75;
  draw_image (Png.load_as_rgb24 "assets/cards/wild.png" []) 595 75;
  draw_image (Png.load_as_rgb24 "assets/cards/greenSkip.png" []) 625 75;
  draw_image (Png.load_as_rgb24 "assets/cards/drawFour.png" []) 655 75;
  draw_image (Png.load_as_rgb24 "assets/cards/blue6.png" []) 685 75;
  draw_image (Png.load_as_rgb24 "assets/cards/red8.png" []) 715 75;

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

  (* direction arrows *)
  set_line_width 6;
  set_color green;
  draw_arc 640 360 100 100 (-55) 55;
  draw_segments (Array.of_list [697, 443, 725, 435]);
  draw_segments (Array.of_list [697, 443, 700, 420]);

  set_color black;
  draw_arc 550 360 100 100 125 235;
  draw_segments (Array.of_list [495, 279, 490, 305]);
  draw_segments (Array.of_list [495, 279, 471, 277]);

  loop ()
