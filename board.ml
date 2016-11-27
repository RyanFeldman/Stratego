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
    val string_from_piece : piece -> string
    val get_possible_moves : t -> bool -> piece -> position -> position list
    val is_valid_move : t -> bool -> position -> position -> (bool * string)
    val make_move : t -> position -> position -> (t * piece list)
    val equal_board : t -> t -> bool
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

    let string_from_piece piece =
        match piece.rank with
        | 0 -> "Bomb"
        | 1 -> "Spy"
        | 2 -> "Scout"
        | 3 -> "Miner"
        | 4 -> "Sergeant"
        | 5 -> "Lieutenant"
        | 6 -> "Captain"
        | 7 -> "Major"
        | 8 -> "Colonel"
        | 9 -> "General"
        | 10 -> "Marshal"
        | 11 -> "Flag"
        | _ -> failwith "Not a valid piece"

    let rec step board b pos dir =
        match dir with
        | N ->
            let (x, y) = pos in
            if y=9 then [] else
            (match (search (x, y+1) board) with
            | None -> (x, y+1)::(step board b (x, y+1) N)
            | Some piece ->
                if (piece.player = b) then [] else [(x, y+1)])
        | E ->
            let (x, y) = pos in
            if x=9 then [] else
            (match (search (x+1, y) board) with
            | None -> (x+1, y)::(step board b (x+1, y) E)
            | Some piece ->
                if (piece.player = b) then [] else [(x+1, y)])
        | S ->
            let (x, y) = pos in
            if y=0 then [] else
            (match (search (x, y-1) board) with
            | None -> (x, y-1)::(step board b (x, y-1) S)
            | Some piece ->
                if (piece.player = b) then [] else [(x, y-1)])
        | W ->
            let (x, y) = pos in
            if x=0 then [] else
            (match (search (x-1, y) board) with
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

    (**
     * [check_pos_one board b pos] is a tuple t. Fst t is true iff a piece at
     * [pos] has field player that is equal to b. False otherwise. Snd t is
     * a string containing the reason why the move is not valid. "" if valid.
     *)
    let check_pos_one board b pos =
         match (search pos board) with
            | None -> (false, "There's nothing at (" ^ (string_of_int (fst pos))
                                ^ ", " ^ (string_of_int (snd pos)) ^ ")!")
            | Some x ->
                if x.player = b then (true, "")
                else (false, "That's not your piece!")

    (* See board.mli *)
    let is_valid_move (board:t) (b:bool) (pos_one:position) (pos_two:position)
            : bool * string =
        let within_board = (in_board pos_one) && (in_board pos_two) in
        if (within_board = false) then (false, "Position outside of board") else
        let valid_pos_one = check_pos_one board b pos_one in
        if (fst valid_pos_one) = false then valid_pos_one else
        let pos_one_p = match (search pos_one board) with
                        | None -> failwith "Nothing at pos_one for some reason"
                        | Some x -> x in
        let possible_moves_list = get_possible_moves board b pos_one_p pos_one in
        if (List.mem pos_two possible_moves_list) = false
            then (false, "That piece can't move there!")
        else
            match (search pos_two board) with
            | None -> (true, "")
            | Some x ->
                if (x.player = b) then (false, "Don't attack your own team!")
                else
                    (true, "")

    (**
     * [remove_optional piece] is p in [piece], which is (Some p).
     * Raises:
     *  - Failure if [piece] is None
     *)
    let remove_optional piece =
        match piece with
        | None -> failwith "None"
        | Some p -> p

    (**
     * [execute_conflict board p_two pos_one pos_two] is a tuple containing
     * the new board after executing the movement of the piece from [pos_one]
     * to [pos_two] on [board] and the list of pieces captured in the conflict.
     *)
    let execute_conflict board p_two pos_one pos_two =
        let p_one = remove_optional (search pos_one board) in
        match (p_one.rank - p_two.rank) with
        | p_one_worse when p_one_worse < 0 ->
            let temp_board = add_mapping pos_one None board in
            let p = remove_optional (search pos_two temp_board) in
            let seen_p = {p with hasBeenSeen=true} in
            let new_board = add_mapping pos_two (Some seen_p) temp_board in
            (new_board, [p_one])
        | p_one_equal when p_one_equal = 0 ->
            let temp_board = add_mapping pos_one None board in
            let new_board = add_mapping pos_two None temp_board in
            (new_board, [p_one; p_two])
        | p_one_better when p_one_better > 0 ->
            let temp_board = add_mapping pos_one None board in
            let seen_piece = {p_one with hasBeenSeen=true} in
            let new_board = add_mapping pos_two (Some seen_piece) temp_board in
            (new_board, [p_two])
        | _ -> failwith "invalid math"

    (**
     * [string_from_tuple t] is the string form of the tuple [t]. i.e. "(x, y)"
     *)
    let string_from_tuple t =
        let (x, y) = t in
        "("^(string_of_int x)^", "^(string_of_int y)^")"

    (**
     * [get_msg pos_one pos_two tup] is the message to be posted to the user
     * depending on how (or if) a conflict occured.
     *)
    let get_msg pos_one pos_two tup =
        match (snd tup) with
        | [] -> ""
        | h::[] ->
            if h.player then
                let p_win = remove_optional (search pos_two (fst tup)) in
                "User's "^(string_from_piece p_win)^" defeated the AI's"
                ^(string_from_piece h)^"! User's piece is at "
                ^(string_from_tuple pos_two)^" and AI's piece has been taken"
                ^" from the baord."
            else
                let p_win = remove_optional (search pos_two (fst tup)) in
                "AI's "^(string_from_piece p_win)^" defeated the User's"
                ^(string_from_piece h)^"! AI's piece is at "
                ^(string_from_tuple pos_two)^" and User's piece has been taken"
                ^" from the board."
        | h1::h2::[] ->
            "Both the user and the AI's "^(string_from_piece h1)^" have been"
            ^" taken from the board."
        | _ -> failwith "Invalid captured pieces list given"

    (* See board.mli file *)
    let make_move (board:t) (pos_one:position) (pos_two:position)
            : t * piece list =
        let changed_tuple =
            (match (search pos_two board) with
            | None ->
                let temp_board =
                            add_mapping pos_two (search pos_one board) board in
                ((add_mapping pos_one None temp_board), [])
            | Some piece -> execute_conflict board piece pos_one pos_two) in
        let msg = get_msg pos_one pos_two changed_tuple in
        let p_one = remove_optional (search pos_two (fst changed_tuple)) in
        let _ = print_endline ("Moved "
                                ^(string_from_piece p_one)^"from "
                                ^(string_from_tuple pos_one)
                                ^" to "^(string_from_tuple pos_two)^"with no "
                                ^"conflicts!") in
        let _ = print_endline msg in
        changed_tuple

    (*[equal_board b1 b2] is true when b1 are the same size and have the same
     * positions and pieces binded to positions and false otherwise
     *)
    let equal_board b1 b2 =
        let sizeb1 = board_fold (fun k v b -> b + 1) b1 0 in
        let sizeb2 = board_fold (fun k v b -> b + 1) b2 0 in
        let same_map = board_fold
            (fun k v b -> (try v = search k b2 with _ -> false) && b) b1 true in
        ((sizeb1 = sizeb2) && same_map)

end
