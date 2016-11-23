type board

(*[setup_game] creates the board that exists at the start of gameplay, It
 * allows the user to choose the start position of each of her pieces*)
val setup_game : unit -> board

(*[play] handles the gameplay of stratego. It takes in a board, prompts the player
 * to input a move, executes that move, prompts the AI to make a move, executes
 * that move. Then this function recursively calls itself.
*)
val play : board -> board

(*[make_move board pos1 pos2] takes in [board] and a command from the player/AI
 *to move the piece at [pos1] to [pos2], and returns the resulting board. If
 *the move is not legal, this function will prompt the player again until they
 *give a legal move.
 *)
val make_move : board -> position -> position -> board
