open Board.GameBoard

module type AI = sig 
  
  (* [board] is the type of a Board *)
  type board

  (**
   * [setup_board] takes in a board and returns the same board with the AI's
   * stratego pieces placed on its side of the board.
   *)
  val setup_board : board -> board

  (** 
   * [score_board] takes in a board and assigns it a score based on 
   * how desirable it is for the AI 
   *)
  val score_board : board -> int

  (** 
   * [get_valid_boards] takes in a board and returns a list 
   * of possible boards that are valid from the current board. Used
   * only by the AI 
   *)
  val get_valid_boards : board -> board list

  (** 
   * [choose_best_board] takes in a list of boards available to the AI 
   * and returns the one with the highest score (relative to the AI). 
   *)
  val choose_best_board : board list -> board

end

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
  failwith "unimplemented"


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

(*[get_moves_piece board p]
 *
 *)
let get_moves_piece board piece =
  failwith "unimplemented"

(* [get_moveable_from_move board] returns
 *
 *)
let get_moveable_from_move board =
  failwith "unimplemented"