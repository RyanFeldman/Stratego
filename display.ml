open Board.GameBoard

module type Display = sig
  type board = t
  type piece = Board.GameBoard.piece
  val print_message : string -> unit
  val display_board : board -> unit
  val print_list : piece list -> unit
  val display_table : unit -> unit
  val display_rules : unit -> unit
end

module TextDisplay : Display = struct

  type board = t
  type piece = Board.GameBoard.piece

  let print_message s = print_endline s

  let string_of_piece p = match p.rank with
  |x when x = 0 -> " B"
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

  let display_rules () =
    print_endline
    "
    Stratego is a game in which you need to capture the flag of your opponent
    while defending your own flag.
    To capture the flag you use your army of 40 pieces.
    Pieces have a rank and represent individual officers and soldiers in an army.
    In addition to those ranked pieces you can use bombs to protect your flag.

    Pieces move 1 square per turn, horizontally or vertically.
    Only the scout can move over multiple empty squares per turn.
    Pieces cannot jump over another piece.

    If a piece is moved onto a square occupied by an opposing piece,
    The weaker piece is removed from the board,
    and the stronger piece is moved into the place formerly occupied by the weaker piece.
    If the engaging pieces are of equal rank, they are both removed.
    Pieces may not move onto a square already occupied by another piece without attacking.
    Exception to the rule of the higher rank winning is the spy.
    When the spy attacks the marshal, the spy defeats the higher ranked marshal.
    However, when the marshal attacks the spy, the spy loses.
    Bombs lose when they are defused by a miner.

    The bombs and the flag cannot be moved.
    A bomb defeats every piece that tries to attack it, except the miner.
    The flag loses from every other piece.
    When you capture the flag of your opponent you win the game.

    To move, type the position of the piece you want to move followed by the target
    location (ex. 00 01).
    At any time, type \"table\" to see the pieces reference table,
    or type \"captured\" to see the pieces that have been captured."

  let print_list (l:piece list) =
    print_string "Pieces: ";
    List.iter (fun x -> print_string ((string_of_piece x)^" | ")) l;
    print_endline ""

  let display_table () =
    print_endline "   __________________";
    print_endline "  |                  |";
    print_endline "  |   F - Flag       |";
    print_endline "  |   B - Bomb       |";
    print_endline "  |   1 - Spy        |";
    print_endline "  |   2 - Scout      |";
    print_endline "  |   3 - Miner      |";
    print_endline "  |   4 - Sergeant   |";
    print_endline "  |   5 - Lieutenant |";
    print_endline "  |   6 - Captain    |";
    print_endline "  |   7 - Major      |";
    print_endline "  |   8 - Colonel    |";
    print_endline "  |   9 - General    |";
    print_endline "  |  10 - Marshall   |";
    print_endline "  |__________________|";
    print_endline ""

end