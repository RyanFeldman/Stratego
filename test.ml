open OUnit2
open Board
open BoardMap
open IntTuple

let corner_board =
  BoardMap.add (0,0) (Some {rank=5; player=false; hasBeenSeen = false})
    BoardMap.empty


let tests = "ai tests" >::: [

    "sample test" >:: (fun _ -> assert_equal 1 1)

    (*"making board 1" >::(fun _ -> assert_equal
        (Some {rank=5; player=false; hasBeenSeen = false})
        (BoardMap.find (0,0) corner_board)
      );
    *)

    ]

let _ = run_test_tt_main tests
