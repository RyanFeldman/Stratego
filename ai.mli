(* The [AI] module represents the computer opponent to the stratego user.
 * it takes in boards and chooses a move based on the user's move. *)
module type AI = sig 
	
	(* What would the board type be? Would that be public as opposed
	 * to abstracted? *)
	type board

	val setup_board : board -> board

	(* [score_board] takes in a board and assigns it a score based on 
	 * how desirable it is for the AI *)
	val score_board : board -> int

	(* [choose_best_board] takes in a list of boards available to the AI 
	 * and picks the one with the highest score (relative to the AI) *)
	val choose_best_board : board list -> board

end