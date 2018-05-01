open OUnit2
open Player
open Command

  let card1 = {value = 4; color = Red; effect = NoEffect; id = 44}
  let card2 = {value = 5; color = Blue; effect = NoEffect; id = 35}
  let card5 = {value = 6; color = Red; effect = NoEffect; id = 46}
  let card3 = {value = 8; color = Yellow; effect = NoEffect; id = 18}
  let card4 = {value = 2; color = Green; effect = NoEffect; id = 22}
  let card6 = {value = 5; color = Blue; effect = NoEffect; id = 35}
  let ncard = {value = -1; color = NoColor; effect = NoEffect; id = -1}

  let hand1 = card1::card2::card5::card3::card4::card6::[]
  let hand2 = card2::card5::card3::card4::card6::[]
  let hand3 = card3::[]

  let top_card0 = {value = 5; color = Red; effect = NoEffect; id = 43}
  let top_card1 = {value = 7; color = Green; effect = NoEffect; id = 27}
  let top_card2 = {value = 0; color = Red; effect = NoEffect; id = 40}
  let top_card3 = {value = 9; color = Blue; effect = NoEffect; id = 39}

  let tests = [
    "play_44" >:: (fun _ -> assert_equal (card1) (dumbai_choose_card top_card0 hand1));
    "play_35" >:: (fun _ -> assert_equal (card2) (dumbai_choose_card top_card0 hand2));
    "play_22" >:: (fun _ -> assert_equal (card4) (dumbai_choose_card top_card1 hand1));
    "play_46" >:: (fun _ -> assert_equal (card5) (dumbai_choose_card top_card2 hand2));
    "play_-1" >:: (fun _ -> assert_equal (ncard) (dumbai_choose_card top_card0 hand3));

    "command_1" >:: (fun _ -> assert_equal "54" (get_args "Play    54"));
    "command_2" >:: (fun _ -> assert_equal "54" (get_args "play 54"));
    "command_3" >:: (fun _ -> assert_equal "" (get_args "Info"));
    "command_4" >:: (fun _ -> assert_equal "play" (get_command "PLaY    47"));
    "command_5" >:: (fun _ -> assert_equal "1" (get_args "Draw 1"));
    "command_a" >:: (fun _ -> assert_equal "red 4" (get_args "Play    red 4"));

    "command_6" >:: (fun _ -> assert_equal (Plus) (det_effect 55));
    "command_7" >:: (fun _ -> assert_equal (Skip) (det_effect 69));
    "command_8" >:: (fun _ -> assert_equal (Reverse) (det_effect 70));
    "command_9" >:: (fun _ -> assert_equal (NoEffect) (det_effect 15));
    "command_x" >:: (fun _ -> assert_equal (NoEffect) (det_effect 80));

    "parse_1" >:: (fun _ -> assert_equal (Play card1) (parse "play 44"));
    "parse_2" >:: (fun _ -> assert_equal (Play ncard) (parse "play 235235"));
    "parse_3" >:: (fun _ -> assert_equal (Play card2) (parse "play 35"));
    "parse_4" >:: (fun _ -> assert_equal (Play card5) (parse "play 46"));
    "parse_5" >:: (fun _ -> assert_equal (Play card3) (parse "play 18"));
  ]

let suite = "Uno test suite" >::: tests

let _ = run_test_tt_main suite
