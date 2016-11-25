module IntTuple = struct

	type t = (int * int)

	let compare (t1:t) (t2:t) =
        match Pervasives.compare (fst t1) (fst t2) with
            | 0 -> Pervasives.compare (snd t1) (snd t2)
            | c -> c

end

module BoardMap = Map.Make(IntTuple)

module Board = struct

    type position = int * int
    type piece = {
        rank : int;
        player : bool;
        hasBeenSeen : bool
    }

	type t = (piece option) BoardMap.t

	let instantiate_board () =
		failwith "Unimplemented"

	let is_valid_move board pos_one pos_two =
		failwith "Unimplemented"

	let make_move board pos_one pos_two : (t * piece list) =
		failwith "Unimplemented"


end
