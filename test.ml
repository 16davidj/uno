open OUnit2
open Player
(* open Command  *)

module type Tests = sig
  val tests : OUnit2.test list
end

module DumbTester = struct

  let card1 = {value = 4; color = Red; effect = No; id = 44}
  let card2 = {value = 5; color = Blue; effect = No; id = 35}
  let card5 = {value = 6; color = Red; effect = No; id = 46}
  let card3 = {value = 8; color = Yellow; effect = No; id = 18}
  let card4 = {value = 2; color = Green; effect = No; id = 22}
  let card6 = {value = 5; color = Blue; effect = No; id = 35}
  let ncard = {value = -1; color = No; effect = No; id = -1}

  module DumbPlayer = struct
    (* let id = 1
    let name = "Dumb"
    let hand = card1::card2::card5::card3::card4::card6::[]
    let cards_left = 6 *)

    let rec does_wild4_exist hand =
      match hand with
      | [] -> None
      | h::t -> if h.effect = Wild4 then Some h else does_wild4_exist t

    let rec find_possible_card color num eff hand =
      match hand with
      | [] -> None
      | h::t -> begin
        if h.color = color then Some h else
        if h.value = num then Some h else
        if h.effect = eff then Some h else
          find_possible_card color num eff t
      end

    let choose_card top_card hand =
      let exists_card = find_possible_card (top_card.color) (top_card.value) (top_card.effect) hand in
      match exists_card with
      | None -> begin
          match (does_wild4_exist hand) with
          | None -> {value = -1; color = No; effect = No; id = -1}
          | Some x -> x
      end
      | Some h -> h


  end

  let hand1 = card1::card2::card5::card3::card4::card6::[]
  let hand2 = card2::card5::card3::card4::card6::[]
  let hand3 = card3::[]

  let top_card0 = {value = 5; color = Red; effect = No; id = 43}
  let top_card1 = {value = 7; color = Green; effect = No; id = 27}
  let top_card2 = {value = 0; color = Red; effect = No; id = 40}
  let top_card3 = {value = 9; color = Blue; effect = No; id = 39}

  let tests = [
    "play_44" >:: (fun _ -> assert_equal (card1) (DumbPlayer.choose_card top_card0 hand1));
    "play_35" >:: (fun _ -> assert_equal (card2) (DumbPlayer.choose_card top_card0 hand2));
    "play_22" >:: (fun _ -> assert_equal (card4) (DumbPlayer.choose_card top_card1 hand1));
    "play_46" >:: (fun _ -> assert_equal (card5) (DumbPlayer.choose_card top_card2 hand2));
    "play_-1" >:: (fun _ -> assert_equal (ncard) (DumbPlayer.choose_card top_card0 hand3));
  ]
end

let tests = DumbTester.tests
