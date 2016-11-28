open Board.GameBoard
open Display.TextDisplay

module type AI = sig
  type board = t
  val setup_board : board -> board
  val choose_best_board : board -> board
end

module GameAI : AI = struct

  type board = t

 (*
  * [get_list_all_pieces] returns a piece list containing every piece that
  * the ai starts with.
  *)
let get_ai_pieces () =
  let pieces = get_list_all_pieces () in
  List.map (fun p -> {p with player=false}) pieces


let next_pos = function
  |(9,y) -> (0, y-1)
  |(x,y) -> (x+1, y)

let rec random_fill board filled remaining pos =
  match remaining with
  |[] -> board
  |h::t ->
    if List.mem pos filled then
      random_fill board filled remaining (next_pos pos)
    else
      let new_board = add_mapping pos (Some h) board in
      random_fill new_board (pos::filled) t (next_pos pos)

(* [setup_board] takes in a board where only the player has set up their
 * pieces and sets up the AI's pieces.
 *)
  let setup_board board =
    let () = Random.self_init () in
    let flag_x_pos = Random.int 10 in
    let flag_pos = (flag_x_pos, 9) in
    let bomb_one_x = match flag_x_pos with
                     |0 -> 4
                     |n -> n-1 in
    let bomb_two_x = match flag_x_pos with
                     |9 -> 6
                     |n -> n+1 in
    let flag_board = add_mapping flag_pos
        (Some {rank=11; player=false; hasBeenSeen=false}) board in
    let one_bomb_board = add_mapping (bomb_one_x, 9)
        (Some {rank=0; player=false; hasBeenSeen=false}) flag_board in
    let two_bomb_board = add_mapping (bomb_two_x, 9)
        (Some {rank=0; player=false; hasBeenSeen=false}) one_bomb_board in
    let three_bombs_board = add_mapping (flag_x_pos, 8)
        (Some {rank=0; player=false; hasBeenSeen=false}) two_bomb_board in
    let filled = [flag_pos;(bomb_one_x, 9);(bomb_two_x, 9); (flag_x_pos, 8)] in
    let remaining = match get_list_all_pieces () with
                    |f::b1::b2::b3::t -> t
                    |_ -> failwith "the function should match the above" in
    let shuffled = List.sort (fun x y -> Random.int 2) remaining in
    random_fill three_bombs_board filled shuffled (0,9)

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

  (* [ai_battle p1 p2] is the piece option that ai assumes will win if p1 and p2
   * battle ai assumes p1, its own piece, will win if p1 would win in normal gameplay
   * or if p1 is within 2 points of the rank of p2
   *)
  let ai_battle p1 p2 =
    match p1.rank, p2.rank with
    | _, _ when p2.player -> failwith "ai shouldn't battle it's own piece"
    | 3,0 -> Some p1
    | _ , 0 -> Some p2
    | _ , 11 -> Some p1
    | 1, 10 -> Some p1
    | p1r, p2r when p2.hasBeenSeen ->
          if p1r > p2r then Some p1 else if p1r < p2r then Some p2 else None
    | p1r, p2r when p1r >= p2r - 2 -> Some p1
    | _,_ -> Some p2


  (* [ai_move board pos1 pos 2] is a new board with the piece in pos1 moved to
  * pos2.  If there is a piece in pos2, the piece in pos2 of the new board is
  * the winner of ai_battle
  *)
  let ai_move board pos1 pos2 =
    if pos1 = pos2 then failwith "can't move to same position" else
    match search pos1 board, search pos2 board with
    | None, _ -> failwith "there's no piece here to move"
    | p1 , None -> replace_pos board [(pos1, None);(pos2, p1)]
    | Some p1,Some p2 ->replace_pos board [(pos1, None);(pos2, ai_battle p1 p2)]


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

  (* [score board] returns the AI's net score on [board] by going through
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
  let score board =
    let f piece a = (match piece with
        |None -> a
        |Some p when p.player -> a+(get_value p.rank)
        |Some p -> a-(get_value p.rank)) in
    board_fold (fun k v ac -> f v ac) board 0


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
              (fun x-> if x.player then score := !score + x.rank else
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
      match search (x,y) board with
      |None -> true
      |Some piece -> piece.player
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
    (can_up || can_down || can_left || can_right)


  (* [get_moveable_init board] returns the list of positions in [board] that
   * contain a piece that can make 1 or more valid moves.
   *)
  let get_moveable_init board player =
    let lst = ref [] in
    let f k = function | Some p when p.player = player -> true | _ -> false in
    let () = board_iter
      (fun k v -> if f k v then (lst := k::(!lst)) else ()) board in
    !lst

  (*[get_moves_piece board pos] is an (pos1,pos2) association list that
   *represents all of the posistions the piece at [pos] can move to. The
   * starting move is the first in the association list.
   *)
  let get_moves_piece board pos  =
    let moves = (match search pos board with
    | None -> failwith "there's no piece here"
    | Some p -> get_possible_moves board p.player p pos) in
    List.fold_left (fun a x -> (pos, x)::a) [] moves

(* [get_moveable_from_move board]
 *
 *)
let get_moveable_from_move board =
  failwith "unimplemented"

(**
   * [get_valid_boards board player] is all the possible boards that are valid
   * from the current board [board] when [player] moves.
   *)
let get_valid_boards board player =
    let moveable = get_moveable_init board player in
    let moves = List.fold_left
        (fun a x -> ((get_moves_piece board x) @ a)) [] moveable in
    if player then List.fold_left
        (fun a (p1,p2) -> (ai_move board p1 p2, (p1,p2))::a) [] moves
    else List.fold_left
        (fun a (p1,p2) -> (fst (make_move board p1 p2),(p1,p2))::a) [] moves



(* [minimax board max depth] :int*(board*((int*int)*(int*int))) is the resulting
 *(score, (board,move)) from the minimax algorithm. The move is either the move
 * that minimaxes the board or ((-1,-1),(-1,-1)) if there are no valid moves
 * Requires:
 *    max : bool,true when you want the maximum score
      board: board
      depth : int
 * COMPLETE: minimax algorithm
 * TODO: keep track of moves, figure out way to break ties?
 *)
  let rec minimax board max depth =
      let no_move = ((-1,-1), (-1,-1)) in
      let worst_min = (2000, no_move) in
      let worst_max = (-2000, no_move) in
      if depth = 0 then (score board, no_move) else
      match get_valid_boards board max, max with
      | [], true  -> worst_max
      | [], false -> worst_min
      | lst, true -> List.fold_left (fun a x -> get_max a x depth) worst_max lst
      | lst,false -> List.fold_left (fun a x -> get_min a x depth) worst_min lst

  (* [get_max (s1, m1) (b2, m2) depth] is a (score:int,move:(int*int)*(int*int))
   * tuple that is the move (m1 or m2) that gives the highest score (s1 or
   * the score from minimax of b2 at depth [depth] -1)
   * in the case of a tie, the second move is
   * chosen *)
  and get_max (s1, m1) (b2, m2) depth =
      let (s2, _) = minimax b2 false (depth-1) in
      if s1 > s2 then (s1, m1) else (s2, m2)

  (* [get_min (s1, m1) (b2, m2) depth] is a (score:int,move:(int*int)*(int*int))
   * tuple that is the move (m1 or m2) that gives the lowest score (s1 or
   * the score from minimax of b2 at depth [depth] -1)
   * in the case of a tie, the second move is
   * chosen *)
  and get_min (s1, m1) (b2, m2) depth =
      let (s2, _) = minimax b2 true (depth-1) in
      if s1 < s2 then (s1,m1) else (s2, m2)

end