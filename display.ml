open Board.GameBoard

type board = (piece option) BoardMap.t

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

  let string_of_piece p = match p.rank with
  |x when x = 0 -> " B"
  |x when x = 1 -> " S"
  |x when x < 10 -> " "^(string_of_int x)
  |x when x = 11 -> " F"
  |x -> string_of_int x

  let print_row (b:board) row =
    for col = 0 to 9 do
      let p = BoardMap.find (col, row) b in ();
      (match p with
      |None -> print_string "  ";
      |Some x -> if x.player then print_string "  "
        else (print_string (string_of_piece x)));
      print_string " | "
    done;
    print_endline ""

  let display_board (b:board) =
    print_endline "     _________________________________________________";
    for row = 0 to 9 do

      print_endline "    |    |    |    |    |    |    |    |    |    |    | ";
      print_string  "    | ";
      print_row b row;
      print_endline "    |____|____|____|____|____|____|____|____|____|____|";
    done

end