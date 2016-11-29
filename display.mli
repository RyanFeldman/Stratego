(* [Display] handles the task of showing the player the board and interacting
 * with the user visually through the user interface *)
module type Display = sig

	(* [board] is the type of a Board *)
	type board = Board.GameBoard.t

	type piece = Board.GameBoard.piece

	(**
	 * [print_message] takes in a string message to the user and displays it
	 *)
	val print_message : string -> unit

	(**
	 * [display_board] displays the current board to the player
	 *)
	val display_board : board -> unit

  (**
	 * [print_list] displays a list of pieces to the player
	 *)
	val print_list : piece list -> unit

  (**
   * [display_table] displays a reference table of the pieces to the player
   *)
	val display_table : unit -> unit

  val display_rules : unit -> unit

end

(* [TextDisplay] is a module that displays the board via ASCII characters in the
 * terminal *)

module TextDisplay : Display
