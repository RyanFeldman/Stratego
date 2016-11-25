module IntTuple = struct

	type t = (int * int)

	let compare (t1:t) (t2:t) =
        match Pervasives.compare (fst t1) (fst t2) with
            | 0 -> Pervasives.compare (snd t1) (snd t2)
            | c -> c
end

module BoardMap = Map.Make(IntTuple)

type position = int * int

(* piece.player = true -> AI
 * piece.player = false -> User *)
type piece = {
    rank : int;
    player : bool;
    hasBeenSeen : bool
}

module type Board = sig
    type t
    val instantiate_board : unit -> t
    val is_valid_move : t -> position -> position -> (bool * string)
    val make_move : t -> position -> position -> (t * piece list)
end

module Board = struct

	type t = (piece option) BoardMap.t

	let instantiate_board () : t =
		failwith "Unimplemented"

	let is_valid_move (board:t) (pos_one:position) (pos_two:position) : bool * string =
		failwith "Unimplemented"

	let make_move (board:t) (pos_one:position) (pos_two:position) : (t * piece list) =
		failwith "Unimplemented"
end
