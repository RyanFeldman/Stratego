(* A module type [Board] represents a stratego board with pieces
 * on tiles. 
 *)
module type Board = sig 

	(* The type of the board *)
	type t

	(* Type representing a location on the stratego board *)
	type position

	(* Type representing a stratego piece on the board *)
	type piece

	(**
	 * [instantiate_board] is an instance of a board to be used for a 
	 * stratego game 
	 *)
	val instantiate_board : unit -> t

	(**
	 * [is_valid_move] takes in a board and two positions and is true iff the 
	 * piece in the first position can be moved to the second position legally 
	 * by the rules of stratego. If position1 does not contain a piece, 
	 * [is_valid_move] is false.
	 *)
	val is_valid_move : t -> position -> position -> bool

	(** 
	 * [get_valid_boards] takes in a board and returns a list 
	 * of possible boards that are valid from the current board. Used
	 * only by the AI 
	 *)
	val get_valid_boards : t -> t list

	(** 
 	 * [make_move] takes in a board and a valid movement command from the player, 
 	 * and returns the resulting board. 
	 * Requires: 
 	 * 		- the movement from position1 -> position2 is valid 
 	 * 		- position1 contains a piece that can execute the movement
 	 *)
	val make_move : board -> position -> position -> board

end