module type Tuple = sig 
    type t
    val compare : t -> t -> int
end

module IntTuple : (Tuple with type t = (int * int)) = struct

	type t = (int * int)

	let compare (t1:t) (t2:t) =
        match Pervasives.compare (fst t1) (fst t2) with
            | 0 -> Pervasives.compare (snd t1) (snd t2)
            | c -> c
end

module type Board = sig
    type t
    type position = int * int
    type piece = {
        rank : int;
        player : bool;
        hasBeenSeen: bool
    }
    module BoardMap : Map.S with type key = IntTuple.t
    val instantiate_board : unit -> t
    val is_valid_move : t -> position -> position -> (bool * string)
    val make_move : t -> position -> position -> (t * piece list)
end

module GameBoard : Board = struct

    type position = int * int

    (* piece.player = true -> AI
     * piece.player = false -> User *)
    type piece = {
        rank : int;
        player : bool;
        hasBeenSeen : bool
    }   

    module BoardMap = Map.Make(IntTuple)

	type t = (piece option) BoardMap.t

    let rec get_user_input board = failwith "Unimplemented"

    let rec instantiate_user_board board = function 
    | [] -> board 
    | h::t -> 
        let new_board = get_user_input board in 
        instantiate_user_board new_board t 

    let get_list_all_pieces () = 
        let p       = {rank=0; player=false; hasBeenSeen=false} in 
        let col     = {p with rank=8} in 
        let major   = {p with rank=7} in 
        let cap     = {p with rank=6} in 
        let lieut   = {p with rank=5} in 
        let serg    = {p with rank=4} in 
        let miner   = {p with rank=3} in 
        let scout   = {p with rank=2} in 
        let bomb_list = [p; p; p; p; p; p] in 
        let marsh_list = [{p with rank=10}] in 
        let gen_list = [{p with rank=9}] in 
        let col_list = [col; col] in 
        let maj_list = [major; major; major] in 
        let cap_list = [cap; cap; cap; cap] in 
        let lieut_list = [lieut; lieut; lieut; lieut] in 
        let serg_list = [serg; serg; serg; serg] in 
        let mine_list = [miner; miner; miner; miner; miner] in 
        let sco_list = [scout; scout; scout; scout; scout; scout; 
                        scout; scout] in 
        let spy_list = [{p with rank=1}] in 
        let flag_lst = [{p with rank=11}] in 
        bomb_list @ marsh_list @ gen_list @ col_list @ maj_list @ cap_list @
        lieut_list @ serg_list @ mine_list @ sco_list @ spy_list @ flag_lst

	let instantiate_board () : t =
		let new_board = BoardMap.empty in 
        let full_pieces = get_list_all_pieces () in 
        let user_board = instantiate_user_board new_board full_pieces in 
        user_board

	let is_valid_move (board:t) (pos_one:position) (pos_two:position) 
            : bool * string =
		failwith "Unimplemented"

	let make_move (board:t) (pos_one:position) (pos_two:position) 
            : t * piece list =
		failwith "Unimplemented"
end
