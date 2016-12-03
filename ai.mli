(* The [AI] module represents the computer opponent to the stratego user.
 * An AI chooses a move based on the board and its best interests *)
module type AI = sig

	(* [board] is the type of a Board *)
	type board = Board.GameBoard.t
	type victory = Board.GameBoard.victory
	type piece = Board.GameBoard.piece

	(**
	 * [choose_best_board] takes in a list of boards available to the AI
	 * and returns the one with the highest score (relative to the AI).
	 *)
  val choose_best_board : board -> (victory * piece list * string)

end

module GameAI : AI