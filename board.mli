(* A module type [Board] represents a stratego board with pieces
 * on tiles. 
 *)
module type Board = sig 

	(* For Reference: 
	 *     _________________________________________________
	 *    |    |    |    |    |    |    |    |    |    |    | 
	 *  J | AI | AI | AI | AI | AI | AI | AI | AI | AI | AI |
	 *    |____|____|____|____|____|____|____|____|____|____|
	 *    |    |    |    |    |    |    |    |    |    |    | 
	 *  I | AI | AI | AI | AI | AI | AI | AI | AI | AI | AI |
	 *    |____|____|____|____|____|____|____|____|____|____|
	 *    |    |    |    |    |    |    |    |    |    |    | 
	 *  H | AI | AI | AI | AI | AI | AI | AI | AI | AI | AI |
	 *    |____|____|____|____|____|____|____|____|____|____|
	 *    |    |    |    |    |    |    |    |    |    |    | 
	 *  G | AI | AI | AI | AI | AI | AI | AI | AI | AI | AI |
	 *    |____|____|____|____|____|____|____|____|____|____|
	 *    |    |    |    |    |    |    |    |    |    |    | 
	 *  F |    |    |    |    |    |    |    |    |    |    |
	 *    |____|____|____|____|____|____|____|____|____|____|
	 *    |    |    |    |    |    |    |    |    |    |    | 
	 *  E |    |    |    |    |    |    |    |    |    |    |
	 *    |____|____|____|____|____|____|____|____|____|____|
	 *    |    |    |    |    |    |    |    |    |    |    | 
	 *  D | US | US | US | US | US | US | US | US | US | US |
	 *    |____|____|____|____|____|____|____|____|____|____|
	 *    |    |    |    |    |    |    |    |    |    |    | 
	 *  C | US | US | US | US | US | US | US | US | US | US |
	 *    |____|____|____|____|____|____|____|____|____|____|
	 *    |    |    |    |    |    |    |    |    |    |    | 
	 *  B | US | US | US | US | US | US | US | US | US | US |
	 *    |____|____|____|____|____|____|____|____|____|____|
	 *    |    |    |    |    |    |    |    |    |    |    | 
	 *  A | US | US | US | US | US | US | US | US | US | US |
	 *    |____|____|____|____|____|____|____|____|____|____|
	 *      0     1    2    3    4    5    6    7    8   9
	 * 
	 * This is the board upon instantiation.
	 *)

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
	 * [make_move] takes in a board and a valid movement command from the player, 
	 * and returns the resulting board. 
	 * Requires: 
	 * 		- the movement from position1 -> position2 is valid 
	 * 		- position1 contains a piece that can execute the movement
	 *)
	val make_move : t -> position -> position -> (t * piece list)

end