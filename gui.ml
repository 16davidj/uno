open State
open Graphics
open Command
open Camlimages

let rec loop ()  = loop ()

let () = Graphics.open_graph " ";
  Graphics.set_window_title "Uno";
  loop ()
