open OUnit2
open Board.GameBoard

let bomb = {rank=0; player=false; hasBeenSeen=false}

let corner_board =
    BoardMap.add (0,0) (Some {rank=5; player=false; hasBeenSeen = false})
        BoardMap.empty

let flag_top_row = 
    BoardMap.add (3, 5) (Some {bomb with rank=11}) BoardMap.empty

let cap_first_row = 
    BoardMap.add (0, 3) (Some {bomb with rank=6}) BoardMap.empty
let cap_first_row_none = 
    BoardMap.add (0, 4) (None) cap_first_row
let cap_first_row_ally = 
    BoardMap.add (0, 4) (Some {bomb with rank=6}) cap_first_row_none
let cap_first_row_enemy = 
    BoardMap.add (0, 4) (Some {bomb with player=true}) cap_first_row_none
let cap_first_row_far =     
    BoardMap.add (0, 5) (None) cap_first_row_none

let scout_first_row = 
    BoardMap.add (9, 3) (Some {bomb with rank=2}) BoardMap.empty
let scout_first_row_none = 
    BoardMap.add (9, 4) (None) scout_first_row
let scout_first_row_nonetwo = 
    BoardMap.add (9, 5) None scout_first_row_none
let scout_first_row_topp = 
    BoardMap.add (9, 6) (Some {bomb with rank=4}) scout_first_row_nonetwo
let scout_first_row_leftp = 
    BoardMap.add (8, 3) (Some {bomb with rank=4}) scout_first_row_topp
let scout_first_row_botp = 
    BoardMap.add (9, 2) (Some {bomb with rank=4}) scout_first_row_leftp

let tests = "ai tests" >::: [

    "sample test" >:: (fun _ -> assert_equal 1 1);

    "making board 1" >:: (fun _ -> assert_equal
        (Some {rank=5; player=false; hasBeenSeen = false})
        (BoardMap.find (0,0) corner_board));

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




    

    ]

let _ = run_test_tt_main tests
