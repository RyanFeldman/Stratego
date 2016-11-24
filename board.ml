module CharIntTuple = struct
	
	type t = char * int 

	let compare t1 t2 = 
		match Pervasives.compare (fst t1) (fst t2) with 
		| 0 -> Pervasives.compare (snd t1) (snd t2)
		| c -> c

end

module BoardMap = Map.Make(CharIntTuple)

module Board  = struct

	type t = CharIntTuple.t BoardMap.t

	type position = char * int
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
