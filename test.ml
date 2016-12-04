open OUnit2
open Board.GameBoard
open Game
open Ai.GameAI
open Display.TextDisplay

let pos_00 = make_position 0 0 
let pos_01 = make_position 0 1
let pos_02 = make_position 0 2
let pos_51 = make_position 5 1

let flag_ai = make_piece 11 false false 
let flag_u = make_piece 11 true false
let scout_ai = make_piece 2 false true 

let spy_u = make_piece 1 true false 
let scout_u = make_piece 2 true false
let marsh_ai = make_piece 10 false false

let spy_u2 = make_piece 1 true false

let corner_board = 
    add_mapping pos_00 (Some scout_u) (empty_board ())

let spy_scout = 
    add_mapping pos_00 (Some spy_u) (empty_board ())
let spy_scout_map = 
    add_mapping pos_01 (Some scout_u) (spy_scout)

let flag_near = 
    add_mapping pos_01 (Some flag_ai) (empty_board ())
let flag_near_scout = 
    add_mapping pos_00 (Some scout_u) (flag_near)

let flag_near2 = 
    add_mapping pos_01 (Some flag_u) (empty_board ())
let flag_near_scout2 = 
    add_mapping pos_00 (Some scout_ai) (flag_near2)

let spy_map = 
    add_mapping pos_00 (Some spy_u) (empty_board ())
let marsh_near = 
    add_mapping pos_01 (Some marsh_ai) (spy_map)
let spy_w = 
    add_mapping pos_00 (None) marsh_near
let spy_win = 
    add_mapping pos_01 (Some spy_u) spy_w 


let tests = "ai tests" >::: [

    "sample test" >:: (fun _ -> assert_equal 1 1);

    "making board 1" >:: (fun _ -> assert_equal
        (Some scout_u)
        (search pos_00 corner_board)); 

     "is_valid_move_flag" >:: (fun _ -> assert_equal
        (false, "That piece can't move there!")
        (is_valid_move flag_near false pos_01 pos_00));

    "is_valid_move_valid" >:: (fun _ -> assert_equal
        (true, "")
        (is_valid_move spy_map true pos_00 pos_01));

    "is_valid_move_ally" >:: (fun _ -> assert_equal
        (false, "That piece can't move there!")
        (is_valid_move spy_scout_map true pos_00 pos_01));

    "is_valid_move_enemy" >:: (fun _ -> assert_equal
        (true, "")
        (is_valid_move marsh_near true pos_00 pos_01));

    "is_valid_move_off_left" >:: (fun _ -> assert_equal
        (false, "Position outside of board")
        (is_valid_move spy_scout_map true pos_00 (make_position 0 (-1))));

    "is_valid_move_too_far" >:: (fun _ -> assert_equal
        (false, "That piece can't move there!")
        (is_valid_move spy_map true pos_00 pos_02));

    "is_valid_move_scout_right" >:: (fun _ -> assert_equal
        (false, "Position outside of board")
        (is_valid_move spy_scout_map true pos_01 (make_position 10 1)));

    "is_valid_move_scout_one" >:: (fun _ -> assert_equal
        (true, "")
        (is_valid_move spy_scout_map true pos_01 pos_51));

    "is_valid_move_scout_two" >:: (fun _ -> assert_equal
        (true, "")
        (is_valid_move spy_scout_map true pos_01 pos_02)); 

    "game_terminate" >:: (fun _ -> assert_equal
        (Victory(true), [flag_ai], "Congrats! You won the game!")
        (make_move flag_near_scout pos_00 pos_01));

    "game_terminate2" >:: (fun _ -> assert_equal
        (Victory(false), [flag_u], "The AI won the game! Better luck next time!")
        (make_move flag_near_scout2 pos_00 pos_01));

    "equal_boards" >:: (fun _ -> assert_equal
        (true)
        (equal_board spy_map 
                    (add_mapping pos_00 (Some spy_u2) (empty_board ()))));

    "No_equal_boards" >:: (fun _ -> assert_equal
        (false)
        (equal_board spy_map spy_scout_map));

    (* Note: The following test cases were run by adding specific functions to 
     * ai.mli purely for testing purposes. Since said functions are not needed 
     * in the mli file, they have since been deleted, making the following test 
     * cases not compile. Thus, they have been commented out. *)

    (* "AI get_moveable_init 1" >:: (fun _ -> assert_equal
        ([])
        (get_moveable_init (empty_board ()) true));

    "AI get_moveable_init 2" >:: (fun _ -> assert_equal
        ([])
        (get_moveable_init corner_board false));

    "AI get_moveable_init 3" >:: (fun _ -> assert_equal
        ([])
        (get_moveable_init corner_board true));

    "AI get_moveable_init 4" >:: (fun _ -> assert_equal
        ([])
        (get_moveable_init flag_top_row true));

    "AI get_moveable_init 5" >:: (fun _ -> assert_equal
        []
        (get_moveable_init flag_top_row false));

    "AI get_moveable_init 6" >:: (fun _ -> assert_equal
        []
        (get_moveable_init cap_first_row_enemy true));  *)
    ]



let _ = run_test_tt_main tests
