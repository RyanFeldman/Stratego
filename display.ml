open Board.GameBoard

module type Display = sig
  type board = t
  val print_message : string -> unit
  val display_board : board -> unit
  val print_list : piece list -> unit
end

module TextDisplay : Display = struct

  type board = t

  let print_message s = print_endline s

  let string_of_piece p = match p.rank with
  |x when x = 0 -> " B"
  |x when x = 1 -> " S"
  |x when x < 10 -> " "^(string_of_int x)
  |x when x = 11 -> " F"
  |x -> string_of_int x

  let print_row (board:board) (row:int) =
    for col = 0 to 9 do
      let p = (search (col,row) board) in ();
      (match p with
      |None -> print_string "  ";
      |Some x -> if not x.player then print_string "AI"
        else (print_string (string_of_piece x)));
      print_string " | "
    done;
    print_endline ""

  let display_board (b:board) =
    print_endline "";
    print_endline "     _________________________________________________";
    for row = 9 downto 0 do
      print_endline "    |    |    |    |    |    |    |    |    |    |    | ";
      print_string  (" "^(string_of_int row)^"  | ");
      (print_row b row);
      print_endline "    |____|____|____|____|____|____|____|____|____|____|";
    done;
    print_endline "       0    1    2    3    4    5    6    7    8    9"

  let print_list (l:piece list) =
    print_string "Pieces: ";
    List.iter (fun x -> print_string ((string_of_piece x)^" | ")) l;
    print_endline ""

  let display_table () =
    print_endline "   __________________";
    print_endline "  |                  |";
    print_endline "  |     F - Flag     |";
    print_endline "  |     B - Bomb     |";
    print_endline "  |     1 - Spy      |";
    print_endline "  |     2 - Scout    |";
    print_endline "  |     3 -      |";
    print_endline "  |     4 - Bomb     |";
    print_endline "  |     5 - Bomb     |";
    print_endline "  |     6 - Bomb     |";
    print_endline "  |     7 - Bomb     |";
    print_endline "  |     8 - Bomb     |";
    print_endline "  |     9 - Bomb     |";
    print_endline "  |__________________|";

    print_endline ""

end