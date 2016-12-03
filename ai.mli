(* The [AI] module represents the computer opponent to the stratego user.
 * An AI chooses a move based on the board and its best interests *)
module type AI = sig

	(* [board] is the type of a Board *)
	type board = Board.GameBoard.t
	type victory = Board.GameBoard.victory
	type piece = Board.GameBoard.piece

(* [choose_best_board board] is a victory variant that represents the game
   * after ai has made a move. The vitory variant is:
   *      - Victory(true) if either player has run out of moves
   *      - Victory(false) if the ai has captured the user's flag
   *      - Active(board) if the game is ongoing
   * and is calculated based on the move ai decides is most optimal for the ai,
   * according to the minimax algorithm.
   * Requires: [board] : board
   *)
  val choose_best_board : board -> (victory * piece list * string)

end

module GameAI : AI