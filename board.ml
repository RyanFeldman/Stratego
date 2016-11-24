module IntTuple = struct
	
	type t = (int * int) option

	let compare (t1:t) (t2:t) = 
        match (t1, t2) with 
        | (None, None) -> 0
        | (Some x, None) -> 1
        | (None, Some x) -> -1
        | (Some x, Some y) -> 
            match Pervasives.compare (fst x) (fst y) with 
            | 0 -> Pervasives.compare (snd x) (snd y)
            | c -> c

end

module BoardMap = Map.Make(IntTuple)

module Board = struct

	type t = IntTuple.t BoardMap.t

	type position = int * int
	type piece = { 
		rank : int;
		player : bool;
		hasBeenSeen : bool
	}


	let instantiate_board () = 
		failwith "Unimplemented"

	let is_valid_move board pos_one pos_two = 
		failwith "Unimplemented"

	let make_move board pos_one pos_two = 
		failwith "Unimplemented"


end
