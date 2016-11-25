open OUnit2
open Board

type position = int * int

(* piece.player = true -> AI
 * piece.player = false -> User *)
type piece = {
    rank : int;
    player : bool;
    hasBeenSeen : bool
}   

module IntTuple = struct

    type t = (int * int)

    let compare (t1:t) (t2:t) =
        match Pervasives.compare (fst t1) (fst t2) with
            | 0 -> Pervasives.compare (snd t1) (snd t2)
            | c -> c
end

module BoardMap = Map.Make(IntTuple)

let corner_board =
    BoardMap.add (0,0) (Some {rank=5; player=false; hasBeenSeen = false})
        BoardMap.empty

let ai_tests = "ai tests" >::: [

    "sample test" >:: (fun _ -> assert_equal 1 1);

    "making board 1" >:: (fun _ -> assert_equal
        (Some {rank=5; player=false; hasBeenSeen = false})
        (BoardMap.find (0,0) corner_board));
    

    ]

let _ = run_test_tt_main ai_tests
