module Board  = struct

	type t = ()
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
