open OUnit2
open Board.GameBoard
open Game
open Ai
open GameAI
open Display
open TextDisplay

let bomb = {rank=0; player=false; hasBeenSeen=false}

let corner_board =
    add_mapping (0,0) (Some {rank=5; player=false; hasBeenSeen = false})
        (empty_board ())

let flag_top_row =
    add_mapping (3, 5) (Some {bomb with rank=11}) (empty_board ())

let cap_first_row =
    add_mapping (0, 3) (Some {bomb with rank=6}) (empty_board ())
let cap_first_row_none =
    add_mapping (0, 4) (None) cap_first_row
let cap_first_row_ally =
    add_mapping (0, 4) (Some {bomb with rank=6}) cap_first_row_none
let cap_first_row_enemy =
    add_mapping (0, 4) (Some {bomb with player=true}) cap_first_row_none
let cap_first_row_far =
    add_mapping (0, 5) (None) cap_first_row_none

let scout_first_row =
    add_mapping (9, 3) (Some {bomb with rank=2}) (empty_board ())
let scout_first_row_none =
    add_mapping (9, 4) (None) scout_first_row
let scout_first_row_nonetwo =
    add_mapping (9, 5) None scout_first_row_none
let scout_first_row_topp =
    add_mapping (9, 6) (Some {bomb with rank=4}) scout_first_row_nonetwo
let scout_first_row_leftp =
    add_mapping (8, 3) (Some {bomb with rank=4}) scout_first_row_topp
let scout_first_row_botp =
    add_mapping (9, 2) (Some {bomb with rank=4}) scout_first_row_leftp

let rec none_whole_board board pos=
    match pos with
    |(9,9) -> board
    |(10,y) -> none_whole_board board (0, y+1)
    |(x,y) -> let brd = add_mapping (x,y) None board
                in none_whole_board brd (x+1,y)

let map_one =
    add_mapping (0, 0) (Some bomb) (empty_board ())
let map_two =
    add_mapping (1, 0) (None) (map_one)

let map_a =
    add_mapping (1, 0) (None) (empty_board ())
let map_b =
    add_mapping (0, 0) (Some bomb) (map_a)

let tests = "ai tests" >::: [

    "sample test" >:: (fun _ -> assert_equal 1 1);

    "making board 1" >:: (fun _ -> assert_equal
        (Some {rank=5; player=false; hasBeenSeen = false})
        (search (0,0) corner_board));

    "is_valid_move_flag" >:: (fun _ -> assert_equal
        (false, "That piece can't move there!")
        (is_valid_move flag_top_row false (3, 5) (3, 6)));

    "is_valid_move_valid" >:: (fun _ -> assert_equal
        (true, "")
        (is_valid_move cap_first_row_none false (0, 3) (0, 4)));

    "is_valid_move_ally" >:: (fun _ -> assert_equal
        (false, "Don't attack your own team!")
        (is_valid_move cap_first_row_ally false (0, 3) (0, 4)));

    "is_valid_move_enemy" >:: (fun _ -> assert_equal
        (true, "")
        (is_valid_move cap_first_row_enemy false (0, 3) (0, 4)));

    "is_valid_move_off_left" >:: (fun _ -> assert_equal
        (false, "Position outside of board")
        (is_valid_move cap_first_row_none false (0, 3) (-1, 3)));

    "is_valid_move_too_far" >:: (fun _ -> assert_equal
        (false, "That piece can't move there!")
        (is_valid_move cap_first_row_far false (0, 3) (0, 5)));

    "is_valid_move_scout_right" >:: (fun _ -> assert_equal
        (false, "Position outside of board")
        (is_valid_move scout_first_row false (9, 3) (10, 3)));

    "is_valid_move_scout_one" >:: (fun _ -> assert_equal
        (true, "")
        (is_valid_move scout_first_row_botp false (9, 3) (9, 4)));

    "is_valid_move_scout_two" >:: (fun _ -> assert_equal
        (true, "")
        (is_valid_move scout_first_row_botp false (9, 3) (9, 5)));

    "random board generation" >:: (fun _ -> assert_equal ()
        (display_board (setup_board (none_whole_board (empty_board ()) (0,0)))));

    "equal_boards" >:: (fun _ -> assert_equal
        true
        (equal_board map_two map_b));

    "No_equal_boards" >:: (fun _ -> assert_equal
        false
        (equal_board map_a map_one));


    ]

let _ = run_test_tt_main tests
