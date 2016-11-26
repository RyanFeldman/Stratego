(* [board] is the type of a Board *)
type board

(* [Display] handles the task of showing the player the board and interacting
 * with the user visually through the user interface *)
module type Display = sig

	(**
	 * [print_message] takes in a string message to the user and displays it
	 *)
	val print_message : string -> unit

	(**
	 * [display_board] displays the current board to the player
	 *)
	val display_board : board -> unit

end

(* [TextDisplay] is a module that displays the board via ASCII characters in the
 * terminal *)
module TextDisplay : Display