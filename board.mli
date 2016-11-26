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

    (* Type representing a location on the stratego board *)
    type position = (int * int)

    (* Type representing a stratego piece on the board *)
    type piece = {
        rank : int;
        player : bool;
        hasBeenSeen: bool
    }

	(* The type of the board *)
	type t

    val empty_board : unit -> t

    val search : position -> t -> (piece option)

    val is_member : position -> t -> bool

    val add_mapping : position -> (piece option) -> t -> t

    val board_fold : (position -> piece option -> 'a -> 'a) -> t -> 'a -> 'a

    val board_iter : (position -> piece option -> unit) -> t -> unit
    (**
     * [get_possible_moves] takes in a board, a bool, a piece, and a position
     * and gives back a list of possible positions that piece can move.
     * Requires:
     *  - t : board 
     *  - bool : true if AI, false otherwise
     *  - piece : piece record that is on the board
     *  - position : position of piece on the board
     *)
    val get_possible_moves : t -> bool -> piece -> position -> position list

	(**
	 * [is_valid_move] takes in a board and two positions and is true iff the
	 * piece in the first position can be moved to the second position legally
	 * by the rules of stratego. If position1 does not contain a piece,
	 * [is_valid_move] is false. Returns an error string if [is_valid_move] is
     * false with the reason why to display to the user. Empty if true.
     * Requires: 
     *  - t : board object
     *  - bool : true if AI, false otherwise
     *  - pos1 : piece initial position
     *  - pos2 : piece final position
	 *)
	val is_valid_move : t -> bool -> position -> position -> (bool * string)

	(**
	 * [make_move] takes in a board and a valid movement command from the player,
	 * and returns the resulting board.
	 * Requires:
	 * 		- the movement from position1 -> position2 is valid
	 * 		- position1 contains a piece that can execute the movement
	 *)
	val make_move : t -> position -> position -> (t * piece list)

end

module GameBoard : Board
