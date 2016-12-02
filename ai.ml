open Board.GameBoard
open Display.TextDisplay

module type AI = sig
  type board = t
  type victory = Board.GameBoard.victory
  type piece = Board.GameBoard.piece
  val ai_setup : board -> board
  val choose_best_board : board -> (victory * piece list * string)
end

module GameAI : AI = struct

  type board = t
  type victory = Board.GameBoard.victory
  type piece = Board.GameBoard.piece

 (*
  * [get_list_all_pieces] returns a piece list containing every piece that
  * the ai starts with.
  *)
let get_ai_pieces () =
  let pieces = get_list_all_pieces () in
  List.map (fun p -> (make_piece (get_rank p) false (get_been_seen p))) pieces

(* [ai_setup] takes in a board [board], on which the player has set up their
 * pieces, and sets up the AI's pieces. The AI will always put its flag in the
 * back row and place three bombs around it. Other than that, the placement
 * of every piece is totally random.
 *
 * Requires:
 * [board] : board
 * The player has set up their pieces on [board] already
 *)
  let ai_setup board =
    let () = Random.self_init () in
    let flag_x_pos = Random.int 10 in
    let flag_pos = (flag_x_pos, 9) in
    let bomb_one_x = match flag_x_pos with
                     |0 -> 4
                     |n -> n-1 in
    let bomb_two_x = match flag_x_pos with
                     |9 -> 6
                     |n -> n+1 in
    let flag_p = make_position (fst flag_pos) (snd flag_pos) in
    let bomb_one = make_position bomb_one_x 9 in
    let bomb_two = make_position bomb_two_x 9 in
    let bomb_three = make_position flag_x_pos 8 in
    let flag_board = add_mapping flag_p
        (Some(make_piece 11 false false)) board in
    let one_bomb_board = add_mapping bomb_one
        (Some (make_piece 0 false false)) flag_board in
    let two_bomb_board = add_mapping bomb_two
        (Some (make_piece 0 false false)) one_bomb_board in
    let three_bombs_board = add_mapping bomb_three
        (Some (make_piece 0 false false)) two_bomb_board in
    let filled = [flag_p; bomb_one; bomb_two; bomb_three] in
    let remaining = match get_ai_pieces () with
                    (*The following line removes the flag and three bombs
                     *from the list of pieces that need to be placed because
                     *they have already been put on the board*)
                    |f::b1::b2::b3::t -> t
                    |_ -> failwith "the function should match the above" in
    let shuffled = List.sort (fun x y -> Random.int 2) remaining in
    fill three_bombs_board filled shuffled (make_position 0 9) false

  (* [replace_pos board lst] is a new board with replacements made according
   * to the (pos,piece option) association list [lst], where the first of every
   * tuple is the pos that needs to be overwritten and the second is the piece
   * that goes in that position
    *)
  (*I made this beacuse I wasn't sure if add_mapping created a new board or just
  * updated the one given *)
  let replace_pos board lst =
    let newb = ref (empty_board ()) in
    let f k v =
        newb := (add_mapping k (try List.assoc k lst with _ -> v) !newb) in
    let () = board_iter (fun k v -> f k v) board in
    !newb

  (* [ai_battle p1 p2] is a piece option representing the piece that ai assumes
   * will win if the AI's piece, p1, battles the player's piece, p2. AI assumes
   * p1 will win under the following circumstances:
   *
   *  1) p1 is a miner and p2 is a bomb
   *  2) p1 is any piece and p2 is the player's flag
   *  3) p1 is a spy (rank 1) and p2 is the marshal (rank 10)
   *  4) The AI has not battled p2 in the past and the rank(p1) <= rank(p2) + 2
   *  5) The AI has battle p2 in the past and rank(p1) > rank(p2)
   *
   * In all other circumstances, the AI assumes p1 will lose to p2 in a battle.
   *
   * Requires:
   * [p1] and [p2] : piece
   *)
  let ai_battle p1 p2 =
    match (get_rank p1), (get_rank p2) with
    | _, _ when (not (get_player p2)) -> failwith "ai shouldn't battle it's own piece"
    | 3,0 -> Some p1
    | _ , 0 -> if get_been_seen p2 then Some p2 else Some p1
    | _ , 11 -> Some p1
    | 1, 10 -> Some p1
    | p1r, p2r when (get_been_seen p2) ->
          if p1r > p2r then Some p1 else if p1r < p2r then Some p2 else None
    | p1r, p2r when p1r >= p2r - 2 -> Some p1
    | _,_ -> Some p2


  (* [ai_move board pos1 pos2] is the board that the AI assumes will result when
   * the piece at position [pos1] is moved to position [pos2] on board [board].
   * If moving that piece causes a battle (i.e. there is an enemy piece at pos2)
   * then the board returned reflects what the AI thinks the outcome of the
   * battle will be according to [ai_battle].
   *
   * Requires:
   * [board] : Board
   * [pos1], [pos2] : Board.position
   *)
  let ai_move board pos1 pos2 =
    if pos1 = pos2 then failwith "can't move to same position" else
    match search pos1 board, search pos2 board with
    | None, _ -> failwith "there's no piece here to move"
    | p1 , None -> replace_pos board [(pos1, None);(pos2, p1)]
    | Some p1,Some p2 ->replace_pos board [(pos1, None);(pos2, ai_battle p1 p2)]


  (* [get_value rank] returns the AI's perceived value of a given piece's rank.
   * The values of pieces are how the AI determines which move it should make.
   *
   * For most pieces, the AI's perceived value of a rank is simply that rank.
   * However, there are some exceptions, such as spies. The reason for this is
   * as follows: despite the fact that a spy is rank 1 and it is defeated in
   * battle by nearly every piece, it is valuable because without it, it would
   * be very difficult to defeat the most powerful piece in the game, the
   * marshal. So losing a spy is very costly and so it is assigned a higher
   * value than 1. Similar arguments apply for the flag (obviously the most
   * important piece because its capture ends the game), bombs, and miners.
   *
   * Note that this function takes in an int, which is the integer representation
   * of rank described in the [get_rank] function of the Board module.
   *
   * Requires:
   * [rank] : int
   *)
  let get_value = function
    |0 -> 5
    |1 -> 6
    |3 -> 5
    |11 -> 1000
    |n -> n

  (* [score board] returns the AI's net score on [board] by going through
   * the AI's pieces, summing their values, doing the same for the player's pieces,
   * and subtracting player's score from the AI's score to find the net score.
   *
   * The scoring heuristic assigns each piece an integer value based on its rank.
   * The value of each rank is as described in [get_value]
   *
   * Requires: [board] : board
   *)
  let score board =
    let f piece a = (match piece with
        |None -> a
        |Some p when (get_player p) -> a-(get_value (get_rank p))
        |Some p -> a+(get_value (get_rank p))) in
    board_fold (fun k v ac -> f v ac) board 0

  (* [can_move_to board (x,y) player)] returns true if a piece belonging to
   * [player] (i.e. true if the user, false if the AI) can move to coordinate
   * [(x,y)] on [board]; that is, either there is an enemy piece at [(x,y)] or
   * no piece is at [(x,y)].
   *
   * Requires:
   * [(x,y)] : (int*int)
   * [board] : Board
   * player : bool
   *
   *)
  let can_move_to board (x,y) player =
    try
      match search (make_position x y) board with
      |None -> true
      |Some piece -> player <> (get_player piece)
    with
    |_ -> false

  (* [has_move piece] returns true iff there is 1 or more valid move that the
   * piece at position [pos] can make on [board] when it is the turn of [player].
   * i.e. [player] = true means it is the player's turn; [player] = false means
   * it is the AI's turn.
   *
   * Requires:
   * [board] : Board
   * [pos] : (int*int)
   * [player] : bool
   *
   *)
  let has_move board pos player=
    let piece = match search pos board with
               |Some p -> p
               | _ -> failwith "Should be a piece here" in
    if ((get_rank piece) = 0 || (get_rank piece) = 11) then
      false
    else
      let (x,y) = get_tuple pos in
        let can_up = (match (x,y+1) with
                   |(x',y') when y' > 10 -> false
                   |(x',y') -> can_move_to board (x',y') player) in
        let can_down = (match (x,y-1) with
                     |(x',y') when y' < 0 -> false
                     |(x',y') -> can_move_to board (x',y') player) in
        let can_left = (match (x-1,y) with
                     |(x',y') when x < 0 -> false
                     |(x',y') -> can_move_to board (x',y') player) in
        let can_right = (match (x+1,y) with
                      |(x',y') when x > 10 -> false
                      |(x',y') -> can_move_to board (x',y') player) in
        (can_up || can_down || can_left || can_right)


  (* [get_moveable_init board] returns the list of positions in [board] that
   * contain a piece that can make 1 or more valid moves. Note that positions
   * in this list are represented as (int*int).
   *
   * Requires:
   * [board] : Board
   * [player] : bool
   *)
  let get_moveable_init board player =
    let lst = ref [] in
    let f k = function
      | Some p when (get_player p) = player -> has_move board k player
      | _ -> false in
    let () = board_iter
      (fun k v -> if f k v then (lst := k::(!lst)) else ()) board in
   !lst

  (* [get_moves_piece board pos] is an (pos1,pos2) association list that
   * represents all of the posistions the piece at [pos] can move to. The
   * starting move is the first in the association list.
   *)
  let get_moves_piece board pos  =
    let moves = (match search pos board with
    | None -> failwith "there's no piece here"
    | Some p -> get_possible_moves board (get_player p) p pos) in
    List.fold_left (fun a x -> (pos, x)::a) [] moves

(**
   * [get_valid_boards board player] is all the possible boards that are valid
   * from the current board [board] when [player] moves.
   *)
let get_valid_boards board player =
    let moveable = get_moveable_init board player in
    let moves = List.fold_left
        (fun a x -> (*let () = print_endline "moveable" in*) ((get_moves_piece board x) @ a)) [] moveable in
    if (not player) then List.fold_left
        (fun a (p1,p2) -> (ai_move board p1 p2, (p1,p2))::a) [] moves
    else
        let new_board pos1 pos2 = match make_move board pos1 pos2 with
                        |(Active brd, _, _) -> brd
                        |_ -> failwith "make_move should return Active" in
        List.fold_left
        (fun a (p1,p2) -> (new_board p1 p2,(p1,p2))::a) [] moves



(* [minimax board min depth] (:int*(position*position)) is the resulting
 *(score, move) from the minimax algorithm. The move is either the move
 * that minimaxes the board or ((-1,-1),(-1,-1)) if there are no valid moves
 * Requires:
 *    min : bool,true when you want the minimum score (player = user)
 *    board: board
 *    depth : int
 * TODO: get rid of prints in make_move
 *)
  let rec minimax board min depth =
      let invalid = make_position (-1) (-1) in
      let no_move = (invalid, invalid) in
      let worst_min = (2000, no_move) in
      let worst_max = (-2000, no_move) in
      let tie = (0, no_move) in
      if depth = 0 then (score board, no_move) else
      match get_valid_boards board min, min with
      | [], true  -> if get_valid_boards board false =[] then tie else worst_min
      | [], false -> if get_valid_boards board true = [] then tie else worst_max
      | lst, true -> List.fold_left (fun a x -> get_min a x depth) worst_min lst
      | lst,false -> List.fold_left (fun a x -> get_max a x depth) worst_max lst


  (* [get_max (s1, m1) (b2, m2) depth] is a (score:int,move:(postion*position)
   * tuple that is the move ([m1] or [m2]) that gives the highest score ([s1] or
   * the score from minimax of [b2] at depth [depth] -1)
   * in the case of a tie, [m2] is chosen
    *)
  and get_max (s1, m1) (b2, m2) depth =
      let (s2, _) = minimax b2 true (depth - 1) in
      if s1 > s2 then (s1, m1) else (s2, m2)

  (* [get_min (s1, m1) (b2, m2) depth] is a (score:int,move:(postion*position)
   * tuple that is the move ([m1] or [m2]) that gives the lowest score ([s1] or
   * the score from minimax of [b2] at depth [depth] -1)
   * in the case of a tie, [m2] is chosen
  *)
  and get_min (s1, m1) (b2, m2) depth =
      let (s2, _) = minimax b2 false (depth-1) in
      if s1 < s2 then (s1, m1) else (s2, m2)

  (* [choose_best_board] takes in a list of boards available to the AI
   * and picks the one with the highest score (relative to the AI)
   *)
  let choose_best_board board =
    let move = snd (minimax board false 3) in
    let pos1 = fst move in
    let pos2 = snd move in
    if pos1 = (make_position (-1) (-1)) then
        (Victory true, [], "")
    else
        match make_move board pos1 pos2 with
        |(Active b, captured, str) -> (Active b, captured, str)
        |_ -> failwith "First element should be Active variant"

end