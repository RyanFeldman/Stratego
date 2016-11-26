open Board.GameBoard

type board = t

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

module TextDisplay : Display = struct

  let print_message s = print_endline s

  let print_row b row =
    for col = 0 to 9 do
      (*let piece = Map.find*)
      print_string "___|_"
    done;
    print_endline ""

  let display_board b =
    print_endline "     _________________________________________________";
    print_endline "    |    |    |    |    |    |    |    |    |    |    | ";
    for row = 0 to 9 do
      print_string "    | ";
      print_row b row
    done

end