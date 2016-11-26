

module type Tuple = sig
    type t
    val compare : t -> t -> int
end

module IntTuple : (Tuple with type t = (int * int))


(* A module type [Board] represents a stratego board with pieces
 * on tiles.
 *)
module type Board = sig
	(* For Reference:
	 *     _________________________________________________
	 *    |    |    |    |    |    |    |    |    |    |    |
	 *  9 | AI | AI | AI | AI | AI | AI | AI | AI | AI | AI |
	 *    |____|____|____|____|____|____|____|____|____|____|
	 *    |    |    |    |    |    |    |    |    |    |    |
	 *  8 | AI | AI | AI | AI | AI | AI | AI | AI | AI | AI |
	 *    |____|____|____|____|____|____|____|____|____|____|
	 *    |    |    |    |    |    |    |    |    |    |    |
	 *  7 | AI | AI | AI | AI | AI | AI | AI | AI | AI | AI |
	 *    |____|____|____|____|____|____|____|____|____|____|
	 *    |    |    |    |    |    |    |    |    |    |    |
	 *  6 | AI | AI | AI | AI | AI | AI | AI | AI | AI | AI |
	 *    |____|____|____|____|____|____|____|____|____|____|
	 *    |    |    |    |    |    |    |    |    |    |    |
	 *  5 |    |    |    |    |    |    |    |    |    |    |
	 *    |____|____|____|____|____|____|____|____|____|____|
	 *    |    |    |    |    |    |    |    |    |    |    |
	 *  4 |    |    |    |    |    |    |    |    |    |    |
	 *    |____|____|____|____|____|____|____|____|____|____|
	 *    |    |    |    |    |    |    |    |    |    |    |
	 *  3 | US | US | US | US | US | US | US | US | US | US |
	 *    |____|____|____|____|____|____|____|____|____|____|
	 *    |    |    |    |    |    |    |    |    |    |    |
	 *  2 | US | US | US | US | US | US | US | US | US | US |
	 *    |____|____|____|____|____|____|____|____|____|____|
	 *    |    |    |    |    |    |    |    |    |    |    |
	 *  1 | US | US | US | US | US | US | US | US | US | US |
	 *    |____|____|____|____|____|____|____|____|____|____|
	 *    |    |    |    |    |    |    |    |    |    |    |
	 *  0 | US | US | US | US | US | US | US | US | US | US |
	 *    |____|____|____|____|____|____|____|____|____|____|
	 *      0     1    2    3    4    5    6    7    8   9
	 *
	 * This is the board upon instantiation.
	 *)

    (*module BoardMap : Map.S with type key = IntTuple.t*)

    (* Type representing a location on the stratego board *)
    type position = (int * int)

    (* Type representing a stratego piece on the board *)
    type piece = {
        rank : int;
        player : bool;
        hasBeenSeen: bool
    }

	include Map.S
	(**
	 * [instantiate_board] is an instance of a board to be used for a
	 * stratego game
	 *)
	val instantiate_board : unit -> b

	(**
	 * [is_valid_move] takes in a board and two positions and is true iff the
	 * piece in the first position can be moved to the second position legally
	 * by the rules of stratego. If position1 does not contain a piece,
	 * [is_valid_move] is false. Returns an error string if [is_valid_move] is
     * false with the reason why to display to the user. Empty if true.
	 *)
	val is_valid_move : b -> position -> position -> (bool * string)

	(**
	 * [make_move] takes in a board and a valid movement command from the player,
	 * and returns the resulting board.
	 * Requires:
	 * 		- the movement from position1 -> position2 is valid
	 * 		- position1 contains a piece that can execute the movement
	 *)
	val make_move : b -> position -> position -> (b * piece list)

end

module GameBoard : Board
