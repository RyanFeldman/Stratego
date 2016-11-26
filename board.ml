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
    type position = int * int
    type piece = {
        rank : int;
        player : bool;
        hasBeenSeen: bool
    }
    type t
    val empty_board : unit -> t
    val search : position -> t -> (piece option)
    val is_member : position -> t -> bool
    val add_mapping : position -> (piece option) -> t -> t
    val board_fold : (position -> piece option -> 'a -> 'a) -> t -> 'a -> 'a
    val board_iter : (position -> piece option -> unit) -> t -> unit
    val get_possible_moves : t -> bool -> piece -> position -> position list
    val is_valid_move : t -> bool -> position -> position -> (bool * string)
    val make_move : t -> position -> position -> (t * piece list)
end

module BoardMap = Map.Make(IntTuple)

module GameBoard : Board = struct

    type position = int * int

    (* piece.player = true -> AI
     * piece.player = false -> User *)
    type piece = {
        rank : int;
        player : bool;
        hasBeenSeen : bool
    }

	type t = (piece option) BoardMap.t

    type dir = N | E | S | W

    let empty_board () = BoardMap.empty

    let search pos board = BoardMap.find pos board

    let is_member pos board = BoardMap.mem pos board

    let add_mapping pos piece board = BoardMap.add pos piece board 

    let board_fold f board acc = BoardMap.fold f board acc

    let board_iter f board = BoardMap.iter f board

    let rec step board b pos dir = 
        match dir with 
        | N -> 
            let (x, y) = pos in 
            if y=9 then [] else
            (match (BoardMap.find (x, y+1) board) with 
            | None -> (x, y+1)::(step board b (x, y+1) N)
            | Some piece -> 
                if (piece.player = b) then [] else [(x, y+1)])
        | E -> 
            let (x, y) = pos in 
            if x=9 then [] else
            (match (BoardMap.find (x+1, y) board) with 
            | None -> (x+1, y)::(step board b (x+1, y) E)
            | Some piece -> 
                if (piece.player = b) then [] else [(x+1, y)])
        | S -> 
            let (x, y) = pos in 
            if y=0 then [] else
            (match (BoardMap.find (x, y-1) board) with 
            | None -> (x, y-1)::(step board b (x, y-1) S)
            | Some piece -> 
                if (piece.player = b) then [] else [(x, y-1)])
        | W -> 
            let (x, y) = pos in 
            if x=0 then [] else
            (match (BoardMap.find (x-1, y) board) with 
            | None -> (x-1, y)::(step board b (x-1, y) W)
            | Some piece -> 
                if (piece.player = b) then [] else [(x-1, y)])

    let get_scout_moves board b pos = 
        let up_list = step board b pos N in 
        let right_list = step board b pos E in 
        let bot_list = step board b pos S in 
        let left_list = step board b pos W in 
        up_list @ right_list @ bot_list @ left_list

    let get_possible_moves board b piece pos =
        match piece.rank with 
        | 0 | 11 -> []
        | 2 -> get_scout_moves board b pos 
        | _ -> 
            let (x, y) = pos in 
            let left = if x=0 then [] else [(x-1, y)] in
            let right = if x=9 then [] else [(x+1, y)] in 
            let top = if y=9 then [] else [(x, y+1)] in 
            let bot = if y=0 then [] else [(x, y-1)] in 
            left @ right @ top @ bot

    let in_board pos = 
        if (fst pos) < 0 || (fst pos) > 9 then false
        else 
            if (snd pos) < 0 || (fst pos) > 9 then false
        else true

    let check_pos_one board b pos = 
         match (BoardMap.find pos board) with 
            | None -> (false, "There's nothing at (" ^ (string_of_int (fst pos))
                                ^ ", " ^ (string_of_int (snd pos)) ^ ")!")
            | Some x -> 
                if (not (x.player = b)) then (false, "That's not your piece!")
                else (true, "")

	let is_valid_move (board:t) (b:bool) (pos_one:position) (pos_two:position) 
            : bool * string =
		let within_board = (in_board pos_one) && (in_board pos_two) in 
        if (within_board = false) then (false, "Position outside of board") else
        let valid_pos_one = check_pos_one board b pos_one in 
        if (fst valid_pos_one) = false then valid_pos_one else 
        let pos_one_p = match (BoardMap.find pos_one board) with 
                        | None -> failwith "Nothing at pos_one for some reason"
                        | Some x -> x in 
        let possible_moves_list = get_possible_moves board b pos_one_p pos_one in
        if (List.mem pos_two possible_moves_list) = false 
            then (false, "That piece can't move there!") 
        else 
            match (BoardMap.find pos_two board) with 
            | None -> (true, "")
            | Some x -> 
                if (x.player = b) then (false, "Don't attack your own team!")
                else
                    (true, "")


	let make_move (board:t) (pos_one:position) (pos_two:position) 
            : t * piece list =
		failwith "Unimplemented"
end
