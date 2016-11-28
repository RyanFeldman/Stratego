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

let parse_user_input c = 
    let trim_c = c |> String.trim |> String.lowercase_ascii in
        if (String.contains trim_c ' ') = false then (trim_c, "")
    else 
        let space_index = String.index trim_c ' ' in
        let first_word = (String.sub trim_c 0 space_index_ |> String.trim in 
        let second_half_length = (String.length trim_c) - space_index in
        let second_half = String.sub trim_c space_index second_half_length in
        let trim_second_half = String.trim second_half in
        (first_word, trim_second_half)

let tuple_from_string str = 
    let x = int_of_char (String.get str 0) in
    let y = int_of_char (String.get str 1) in 
    (x, y)

let execute_movement board num1 num2 = 
    let pos_one = tuple_from_string num1 in 
    let pos_two = tuple_from_string num2 in 
    failwith "Unimplemented"

let is_num pos_one pos_two = 
    if (String.length pos_one) = 2 then 
        if (String.length pos_two) = 2 then 
            let _ = try (int_of_string pos_one) with 
                        | _ -> false in 
            let _ = try (int_of_string pos_two) with 
                        | _ -> false in 
            true
        else false
    else false

let handle_user_input cmd board = 
    match cmd with 
    | ("table", "") -> failwith "Unimplemented"
    | ("captured", "") -> failwith "Unimplemented"
    | (p1, p2) when (is_num p1 p2) -> execute_movement board p1 p2
    | _ -> failwith "Unimplemented"

let play (board:board) = 
    let _ = display_board board in 
    let _ = print_message "It's your turn! What would you like to do?"
    let user_input = read_line () in 
    let user_tuple = parse_user_input user_input in 
    let user_board = handle_user_input user_tuple board in 
    user_board
