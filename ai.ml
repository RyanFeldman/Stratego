open Board.GameBoard

module type AI = sig
  type board
  val setup_board : board -> board
  val score_board : board -> int
  val get_valid_boards : board -> board list
  val choose_best_board : board list -> board
end

module GameAI : AI = struct

  type board = Board.GameBoard.t

  let ai_move board move =
    failwith "unimplemented"


  let setup_board board =
    failwith "unimplemented"

  (* [score_board] takes in a board and assigns it a score based on
   * how desirable it is for the AI
   *)
  let score_board board =
    failwith "unimplemented"

  (* [choose_best_board] takes in a list of boards available to the AI
   * and picks the one with the highest score (relative to the AI)
   *)
  let choose_best_board board_lst =
    failwith "unimplemented"

  (*[get_value rank] returns the value of a given rank.
   *)
  let get_value = function
    |0 -> 5
    |1 -> 6
    |3 -> 5
    |11 -> 1000
    |n -> n

  (* [get_score_init board] returns the AI's net score on [board] by going through
   * the AI's pieces, summing their values, doing the same for the player's pieces,
   * and subtracting player's score from the AI's score to find the net score.
   *
   * The scoring heuristic assigns each piece an integer value based on its rank.
   * The value of each piece is its rank except for the following exceptions:
   * A rank 1 piece (Spy) has a value of 6
   * A rank 3 piece (miner) has a value of 5
   * A bomb has a value of 5
   * A rank 10 piece (Marshall) has a value of 15
   * A flag has value 1000
   *
   * Requires: [board] : board
   *)
  let get_score_init board =
    let f piece a = (match piece with
        |None -> a
        |Some p when p.player -> a+(get_value p.rank)
        |Some p -> a-(get_value p.rank)) in
    BoardMap.fold (fun k v ac -> f v ac) board 0


  (* [get_score_from_move board move] gets the score of [board], which initially
   * has score orig_score, after the piece that is initially at position pos1
   * moves to position pos2.
   *
   * Requires:
   * board : board
   * pos1,pos2 : (char * int)
   *)
  let get_score_from_move board orig_score pos1 pos2 =
    let score = ref orig_score in
    let (new_board, captured) = make_move board pos1 pos2 in
    let () = List.iter
              (fun x-> if x.player = true then score := !score + x.rank else
              score := !score - x.rank) captured in
    score

  (* [is_enemy piece] returns true iff [piece] belongs to the player, not the AI
   *
   * Requires: [piece] : piece
   *)
  let is_enemy piece =
    if piece.player = false then true else false

  let can_move_to board (x,y) =
    try
      match BoardMap.find (x,y) board with
      |None -> true
      |Some piece ->
        if piece.player = true then true else false
    with
    |_ -> false
  (* [has_move piece] returns true iff there is 1 or more valid move that the
   * piece at position [pos] can make on [board].
   *)
  (*Needs to be fixed...this would return true even if a piece is surrounded
   *by friendly pieces.*)
  let has_move board pos =
    let (x,y) = pos in
    let can_up = (match (x,y+1) with
               |(x',y') when y' > 10 -> false
               |(x',y') -> can_move_to board (x',y')) in
    let can_down = (match (x,y-1) with
                 |(x',y') when y' < 0 -> false
                 |(x',y') -> can_move_to board (x',y')) in
    let can_left = (match (x-1,y) with
                 |(x',y') when x < 0 -> false
                 |(x',y') -> can_move_to board (x',y')) in
    let can_right = (match (x+1,y) with
                  |(x',y') when x > 10 -> false
                  |(x',y') -> can_move_to board (x',y')) in
    if (can_up || can_down || can_left || can_right) then true else false


  (* [get_moveable_init board] returns the list of positions in [board] that
   * contain a piece that can make 1 or more valid moves.
   *)
  let get_moveable_init board =
    let lst = ref [] in
    let () = BoardMap.iter
      (fun k v -> if (has_move board k) then (lst := k::(!lst)) else ()) board in
    lst

  (*[get_moves_piece board p] is a list of positions that piece p in position pos
   * can go to on the given board board
   *)
  let get_moves_piece (board:board) pos =
    match BoardMap.find pos board with
    | None -> failwith "there's no piece here"
    | Some p -> Board.GameBoard.get_possible_moves board p.player p pos
  
(* [get_moveable_from_move board] returns
 *
 *)
let get_moveable_from_move board =
  failwith "unimplemented"

let get_valid_boards board = failwith "unimplemented"
(*     let moveable = get_moveable_init board in
    let moves = List.fold_left (fun x a -> ((get_moves_piece board x) @ a)) moveable [] in
    if max then List.fold_left (fun a x -> a::(ai_move board x)) [] moves
    else List.fold_left (fun a x -> a::(GameBoard.make_move x)) [] moves  *)

(* [minimax board max depth] : (int, board) is the resulting (score, board) from
 * the minimax algorithm
 * Requires:
 *    max : bool,true when you want the maximum score
      board: board
      depth : int
 * Likely will need to be changed to keep track of move as well
 *)
  let rec minimax board max depth =
      if depth = 0 then (get_score_init board, board) else
      let fmax (score, _) b = let (s, b') = minimax b false (depth-1) in
          if score > s then (score, board) else (s,board) in
      let fmin (score, _) b = let (s, _) = minimax b true (depth-1) in
          if score < s then (score, board) else (s,board) in
      match get_valid_boards board with
      | [] -> failwith "there are no possible boards"
      | h::t when max ->
          List.fold_left (fun a x -> fmax a x) (minimax h false (depth-1)) t
      | h::t -> List.fold_left (fun a x -> fmin a x)(minimax h true (depth-1)) t

end