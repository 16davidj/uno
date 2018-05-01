open OUnit2
open Player
(* open Command  *)

  let card1 = {value = 4; color = Red; effect = No; id = 44}
  let card2 = {value = 5; color = Blue; effect = No; id = 35}
  let card5 = {value = 6; color = Red; effect = No; id = 46}
  let card3 = {value = 8; color = Yellow; effect = No; id = 18}
  let card4 = {value = 2; color = Green; effect = No; id = 22}
  let card6 = {value = 5; color = Blue; effect = No; id = 35}
  let ncard = {value = -1; color = No; effect = No; id = -1}

  let hand1 = card1::card2::card5::card3::card4::card6::[]
  let hand2 = card2::card5::card3::card4::card6::[]
  let hand3 = card3::[]

  let top_card0 = {value = 5; color = Red; effect = No; id = 43}
  let top_card1 = {value = 7; color = Green; effect = No; id = 27}
  let top_card2 = {value = 0; color = Red; effect = No; id = 40}
  let top_card3 = {value = 9; color = Blue; effect = No; id = 39}

  let tests = [
    "play_44" >:: (fun _ -> assert_equal (card1) (dumbai_choose_card top_card0 hand1));
    "play_35" >:: (fun _ -> assert_equal (card2) (dumbai_choose_card top_card0 hand2));
    "play_22" >:: (fun _ -> assert_equal (card4) (dumbai_choose_card top_card1 hand1));
    "play_46" >:: (fun _ -> assert_equal (card5) (dumbai_choose_card top_card2 hand2));
    "play_-1" >:: (fun _ -> assert_equal (ncard) (dumbai_choose_card top_card0 hand3));
  ]

let suite = "Uno test suite" >::: tests

let _ = run_test_tt_main suite
