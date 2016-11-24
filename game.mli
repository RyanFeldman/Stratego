(* [board] represents the type of a Board *)
type board

(**
 * [setup_game] creates the board that exists at the start of gameplay; it
 * allows the user to choose the start position of each of his/her pieces
 *)
val setup_game : unit -> board

(**
 * [play] handles the gameplay of stratego. It takes in a board, prompts the
 * player to input a move, checks that the move is valid, executes that move,
 * prompts the AI to make a move, executes that move, and then returns the final
 * board.
 *)
val play : board -> board
