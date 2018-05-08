open Graphics
open Command
open Camlimages
open Images
open Player
open State

let rec loop () = loop ()

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

let effect_to_str ef = match ef with
  | Plus -> "DrawTwo"
  | Skip -> "Skip"
  | Reverse -> "Reverse"
  | Wild -> "wild"
  | Wild4 -> "drawFour"
  | _ -> "noeffect"

let color_to_str col = match col with
  | Blue -> "blue"
  | Yellow -> "yellow"
  | Green -> "green"
  | Red -> "red"
  | Black -> "black"
  | _ -> failwith("nocolor")

let card_to_str card = if card.value != -1 then
    "assets/cards/" ^ (color_to_str card.color) ^ (string_of_int card.value) ^ ".png"
  else if card.color = Black then
    "assets/cards/" ^ effect_to_str (card.effect) ^ ".png"
  else "assets/cards/" ^ (color_to_str card.color) ^ (effect_to_str (card.effect)) ^ ".png"

let cardlst_to_png s =
  List.map (fun card -> Png.load_as_rgb24 (card_to_str card) []) (user_hand s)

(* x-coordinate should always start at 535 *)
let rec draw_human_hand img_lst x = begin match img_lst with
  | [] -> ();
  | h :: t -> draw_image h x 75; draw_human_hand t (x+30);
 end

let draw_counter () = set_line_width 6; draw_arc 640 360 100 100 (-55) 55;
  draw_segments (Array.of_list [697, 443, 725, 435]);
  draw_segments (Array.of_list [697, 443, 700, 420])

let draw_clock () = set_line_width 6;
  draw_arc 550 360 100 100 125 235;
  draw_segments (Array.of_list [495, 279, 490, 305]);
  draw_segments (Array.of_list [495, 279, 471, 277])

let draw_state s =
  (* direction arrows *)
  if is_counter s then draw_counter () else draw_clock ();

  draw_human_hand (cardlst_to_png s) 535;
  moveto 10 10;
  let curr = (current_player s) in
  draw_string (curr.name);
