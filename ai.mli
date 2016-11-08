(* The [AI] module represents the computer opponent to the stratego user.
 * An AI chooses a move based on the board and its best interests *)
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
	 * [choose_best_board] takes in a list of boards available to the AI 
	 * and returns the one with the highest score (relative to the AI). 
	 *)
	val choose_best_board : board list -> board

end