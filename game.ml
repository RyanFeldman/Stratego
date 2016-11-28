open Board.GameBoard
open Display.TextDisplay
open Ai.GameAI

type board = t

let rec fill_rows board acc =
    if acc=10
        then board
    else
        let board_four = add_mapping (4, acc) None board in
        let board_five = add_mapping (5, acc) None board_four in
        fill_rows board_five (acc+1)

let parse_user_input (c:string) : position =
    let trimmed_c = c |> String.trim in
    let x_one = (String.get trimmed_c 0) |> int_of_char in
    let y_one = (String.get trimmed_c 1) |> int_of_char in
    (x_one, y_one)

let rec get_user_input (board:board) (piece:piece) : board =
    let _ = display_board board in
    let _ = (print_message ("What would you like to do with your "
                            ^(string_from_piece piece)^ "?"))  in
    let user_input = read_line () in
    let (x, y) = parse_user_input user_input in
    if (y > 4 || y < 0)
        then failwith "Invalid y"
    else
        if (x < 0 || x > 9)
            then failwith "Invalid x"
        else
            match (search (x, y) board) with
            | None -> (add_mapping (x, y) (Some piece) board)
            | Some p -> failwith "Something's there"

let rec instantiate_user_board board = function
| [] -> board
| h::t ->
    let new_board =
        (try (get_user_input board h) with
            | _ ->
                let _ = print_message ("Sorry, your input must be in the"
                    ^" form 'xy' to place your piece at (x, y)! As a reminder,"
                    ^" you must place your pieces in the first 4 rows and two "
                    ^"pieces cannot be placed on top of each other to start."
                    ^"\n\n\n") in
                    instantiate_user_board board (h::t)) in
    instantiate_user_board new_board t

let setup_game () =
    let new_board = empty_board () in
    let full_pieces = get_list_all_pieces () in
    let user_board = instantiate_user_board new_board full_pieces in
    let gap_filled_board = fill_rows user_board 0 in
    let start_board = setup_board gap_filled_board in
    start_board



let play (board:board) = failwith "Unimplemented"