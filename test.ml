open OUnit2
open Player
open Command
open State
open Ai
open List

  let card1 = {value = 4; color = Red; effect = NoEffect; id = 44}
  let card2 = {value = 5; color = Blue; effect = NoEffect; id = 35}
  let card5 = {value = 6; color = Red; effect = NoEffect; id = 46}
  let card3 = {value = 8; color = Yellow; effect = NoEffect; id = 18}
  let card4 = {value = 2; color = Green; effect = NoEffect; id = 22}
  let card6 = {value = 5; color = Blue; effect = NoEffect; id = 35}

  let card7 = {value = -1; color = Blue; effect = Skip; id = 63}
  let card8 = {value = -1; color = Yellow; effect = Plus; id = 51}
  let card9 = {value = -1; color = Green; effect = Reverse; id = 72}
  let card10 = {value = -1; color = Yellow; effect = Skip; id = 61}
  let card11 = {value = -1; color = Black; effect = Wild; id = 80}
  let card12 = {value = -1; color = Black; effect = Wild4; id = 90}

  let ncard = {value = -1; color = NoColor; effect = NoEffect; id = -1}

  let hand1 = card1::card2::card5::card3::card4::card6::[]
  let hand2 = card2::card5::card3::card4::card6::[]
  let hand3 = card3::[]

  let hand4 = hand1 @ (card7::card8::card9::card10::card11::card12::[])

  let top_card0 = {value = 5; color = Red; effect = NoEffect; id = 43}
  let top_card1 = {value = 7; color = Green; effect = NoEffect; id = 27}
  let top_card2 = {value = 0; color = Red; effect = NoEffect; id = 40}
  let top_card3 = {value = 9; color = Blue; effect = NoEffect; id = 39}
  let top_card4 = {value = -1; color = Blue; effect = Plus; id = 53}

  let queue1 = (Queue.create ())

    let stack1 = (Stack.create ())

  (* Queue.add card1 queue1; *)

let (p1 : Player.player) = {id = 0; name="bruh"; hand = hand1; intelligence = Human}
let (p2 : Player.player) = {id = 1; name="brua"; hand = hand1; intelligence = AI}
let (p3 : Player.player) = {id = 2; name="brux"; hand = hand1; intelligence = AI}
let (p4 : Player.player) = {id = 3; name="bruc"; hand = hand1; intelligence = AI}

let (st1 : State.state) = {
  players = (p1::p2::p3::p4::[]);
  draw_pile = queue1;
  played_pile = stack1;
  current_color = Red;
  current_player = p2;
  direction = Clockwise;
  turn = 2
}

  let tests = [

    "ai1" >:: (fun _ -> assert_equal (Play card1) (smartai_choose_card st1));

    "lst1" >:: (fun _ -> assert_equal [card6;card5;card2;card1] (get_possible_list hand1 top_card0 []));
    "nlst" >:: (fun _ -> assert_equal [] (get_possible_list hand3 top_card1 []));
    "lst2" >:: (fun _ -> assert_equal [card12;card11;card8;card7;card6;card2] (get_possible_list hand4 top_card4 []));
    "lst3" >:: (fun _ -> assert_equal [card12;card11;card5;card1] (get_possible_list hand4 top_card2 []));
    "lst4" >:: (fun _ -> assert_equal [card12;card11;card10;card8;card3] (get_possible_list hand4 card8 []));

    "cc1" >:: (fun _ -> assert_equal (1,1,2,2,0) (color_count hand1 (0,0,0,0,0)));
    "cc2" >:: (fun _ -> assert_equal (1,1,2,1,0) (color_count hand2 (0,0,0,0,0)));
    "cc3" >:: (fun _ -> assert_equal (1,0,0,0,0) (color_count hand3 (0,0,0,0,0)));
    "cc4" >:: (fun _ -> assert_equal (3,2,3,2,2) (color_count hand4 (0,0,0,0,0)));

    "mc1" >:: (fun _ -> assert_equal (Blue) (most_color (color_count hand1 (0,0,0,0,0))));
    "mc2" >:: (fun _ -> assert_equal (Blue) (most_color ((color_count hand2 (0,0,0,0,0)))));
    "mc3" >:: (fun _ -> assert_equal (Yellow) (most_color ((color_count hand3 (0,0,0,0,0)))));
    "mc4" >:: (fun _ -> assert_equal (Yellow) (most_color ((color_count hand4 (0,0,0,0,0)))));

    (* "play_44" >:: (fun _ -> assert_equal (Play card1) (dumbai_choose_card top_card0 hand1));
    "play_35" >:: (fun _ -> assert_equal (Play card2) (dumbai_choose_card top_card0 hand2));
    "play_22" >:: (fun _ -> assert_equal (Play card4) (dumbai_choose_card top_card1 hand1));
    "play_46" >:: (fun _ -> assert_equal (Play card5) (dumbai_choose_card top_card2 hand2));
    "play_-1" >:: (fun _ -> assert_equal (Draw) (dumbai_choose_card top_card0 hand3)); *)

    "command_1" >:: (fun _ -> assert_equal "54" (get_args "Play    54"));
    "command_2" >:: (fun _ -> assert_equal "54" (get_args "play 54"));
    "command_3" >:: (fun _ -> assert_equal "" (get_args "Info"));
    "command_4" >:: (fun _ -> assert_equal "play" (get_command "PLaY    47"));
    "command_5" >:: (fun _ -> assert_equal "1" (get_args "Draw 1"));

    "command_a" >:: (fun _ -> assert_equal "red 4" (get_args "Play    red 4"));
    "command_b" >:: (fun _ -> assert_equal "yellow draw2" (get_args "Play    yellow draw2"));
    "command_c" >:: (fun _ -> assert_equal "green 0" (get_args "Play    GREEn 0"));
    "command_d" >:: (fun _ -> assert_equal "black wild" (get_args "Play BLACK WILD"));
    "command_e" >:: (fun _ -> assert_equal "blue reverse" (get_args "Play blue Reverse"));

    (* "command_6" >:: (fun _ -> assert_equal (Plus) (det_effect 55));
    "command_7" >:: (fun _ -> assert_equal (Skip) (det_effect 69));
    "command_8" >:: (fun _ -> assert_equal (Reverse) (det_effect 70));
    "command_9" >:: (fun _ -> assert_equal (NoEffect) (det_effect 15));
    "command_x" >:: (fun _ -> assert_equal (NoEffect) (det_effect 80)); *)

    "parse_1" >:: (fun _ -> assert_equal (Play card1) (parse "play 44"));
    "parse_2" >:: (fun _ -> assert_equal (NA) (parse "play 235235"));
    "parse_3" >:: (fun _ -> assert_equal (Play card2) (parse "play 35"));
    "parse_4" >:: (fun _ -> assert_equal (Play card5) (parse "play 46"));
    "parse_5" >:: (fun _ -> assert_equal (Play card3) (parse "play 18"));

    "parse_a" >:: (fun _ -> assert_equal (Play card1) (parse "Play red 4"));
    "parse_b" >:: (fun _ -> assert_equal (Play card3) (parse "Play yeLLoW 8"));
    "parse_c" >:: (fun _ -> assert_equal (Play card4) (parse "Play       gReen 2"));
    "parse_d" >:: (fun _ -> assert_equal (NA) (parse "Play 4 red "));
    "parse_e" >:: (fun _ -> assert_equal (NA) (parse "Play wild"));

    "user_hand" >:: (fun _ -> assert_equal 7 (List.length (user_hand init_state)));
    "ai1_hand" >:: (fun _ -> assert_equal 7 (List.length (ai1_hand init_state)));
    "ai2_hand" >:: (fun _ -> assert_equal 7 (List.length (ai2_hand init_state)));
    "ai3_hand" >:: (fun _ -> assert_equal 7 (List.length (ai3_hand init_state)));

    "next_turn" >:: (fun _ -> assert_equal 1 (next_turn (init_state)));
    "draw_pile_length" >:: (fun _ -> assert_equal 79 (Queue.length (draw_pile init_state)));
  ]

let suite = "Uno test suite" >::: tests

let _ = run_test_tt_main suite
