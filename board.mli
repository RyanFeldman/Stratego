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
    type position

    (**
     * [make_position] takes in two ints and gives back a position
     *)
    val make_position : int -> int -> position

    (**
     * [get_tuple] turns a position into an int tuple
     *)
    val get_tuple : position -> int * int

    (* Type representing a stratego piece on the board *)
    type piece

    (**
     * [make_piece] takes in a rank, the player, and a bool
     * that is true iff the piece has been seen by the AI or not, and gives
     * back a corresponding piece.
     *
     * Note:
     *      piece.player = true -> User
     *      piece.player = false -> AI
     *
     * Note 2: For make_piece, the ranking for special pieces is as follows:
     *
     *      Rank 0: Bomb (i.e. making a piece of rank 0 makes a bomb)
     *      Rank 1: Spy
     *      Rank 11: Flag
     *)
    val make_piece : int -> bool -> bool -> piece

    (**
     * [get_rank] is the corresponding rank of a given piece.
     *
     * The ranking of special pieces (spies, flags, bombs) is the same as in
     * [make_piece]
     *)
    val get_rank : piece -> int

    (**
     * [get_player] is true iff the piece belongs to the player.
     * False otherwise.
     *)
    val get_player : piece -> bool

    (**
     * [get_been_seen] is true iff this piece has been seen by the AI. In other
     * words, true iff the piece has been in a conflict and survived.
     *)
    val get_been_seen : piece -> bool

    (* The type of the board *)
    type t

    (* The type of an active board and a board with a winner *)
    type victory = Active of t | Victory of bool

    (**
     * [empty_board] returns a completely empty board. No mappings.
     *)
    val empty_board : unit -> t

    val none_whole_board : t -> position -> t

    (**
     * [search pos board] is the piece option mapped to [pos] on [board].
     * Raises:
     *  - Not_found is the mapping does not exist.
     *)
    val search : position -> t -> (piece option)

    (**
     * [is_member pos board] is true iff [board] contains a binding for
     * [pos]. False otherwise
     *)
    val is_member : position -> t -> bool

    (**
     * [add_mapping pos piece board] is a map containing the same bindings
     * as [board] plus a binding of [pos] to [piece]. If [pos] was already
     * bound, this function overwrites the previous binding with [piece]
     *)
    val add_mapping : position -> (piece option) -> t -> t

    (**
     * [board_fold f board acc] computes (f kN dN ... (f k1 d1 a)... ) where
     * k1 ... kN are the positions of all the bindings in [board] in increasing
     * order, and d1 ... dN are the associated piece options
     *)
    val board_fold : (position -> piece option -> 'a -> 'a) -> t -> 'a -> 'a

    (**
     * [board_iter f board] applies [f] to all bindings in [board]. f
     * receives the position as the first argument and the piece option as
     * the second argument.
     *)
    val board_iter : (position -> piece option -> unit) -> t -> unit

    (**
     * [string_from_piece] is the corresponding name for the given piece
     * according to its rank.
     *)
    val string_from_piece : piece -> string

    (**
     * [get_possible_moves] takes in a board, a bool, a piece, and a position
     * and gives back a list of possible positions that piece can move.
     * Requires:
     *  - t : board
     *  - bool : true if user, false otherwise
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
     *  - t    : board object
     *  - bool : true if user, false otherwise
     *  - pos1 : piece initial position
     *  - pos2 : piece final position
     *)
    val is_valid_move : t -> bool -> position -> position -> (bool * string)

    (**
     * [make_move] takes in a board and a valid movement command from the player,
     * and returns the resulting board.
     * Requires:
     *  - the movement from position1 -> position2 is valid
     *  - position1 contains a piece that can execute the movement
     *)
    val make_move : t -> position -> position -> (victory * piece list * string)

    (*
     * [get_list_all_pieces player] returns a piece list containing every
     * piece that is present when the game starts. The pieces in the list
     * belong to the player iff [player].
     *)
    val get_list_all_pieces: bool -> piece list

    (**
     * [equal_board b1 b2] is true iff b1 maps the same positions to the same
     * piece options as b2. False otherwise.
     *)
    val equal_board: t -> t -> bool

    (**
    * [fill board filled remaining pos player] takes in [board], on which the tiles
    * in [filled] are filled. Then, it adds all of the pieces in [remaining] to
    * [board], starting at position [pos] and moving rightward from there.
    *)
    val fill: t -> position list -> piece list -> position -> bool -> t

    (**
    * [do_setup board player] sets up one half of [board].
    * If [player] then the player's pieces are set up on the
    * bottom 4 rows of the board. If not [player] then the AI's pieces are
    * set up on the top 4 rows of [board].
    *
    * The flags will go in the top and bottom rows for the AI and player,
    * respectively. Three bombs will be placed next to the flag. The placement
    * of the rest of the pieces is totally random.
    *)
    val do_setup: t -> bool -> t
end

module GameBoard : Board