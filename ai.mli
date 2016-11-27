(* The [AI] module represents the computer opponent to the stratego user.
 * An AI chooses a move based on the board and its best interests *)
module type AI = sig

	(* [board] is the type of a Board *)
	type board = Board.GameBoard.t

	(**
	 * [setup_board] takes in a board and returns the same board with the AI's
	 * stratego pieces placed on its side of the board.
	 *)
	val setup_board : board -> board

	(**
	 * [choose_best_board] takes in a list of boards available to the AI
	 * and returns the one with the highest score (relative to the AI).
	 *)
	val choose_best_board : board -> board

end

module GameAI : AI