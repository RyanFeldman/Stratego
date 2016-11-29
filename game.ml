open Board.GameBoard
open Display.TextDisplay
open Ai.GameAI
exception Illegal

type board = t
type game_piece = piece

let user_counter =
    let c = ref (-1) in
    fun () -> c := !c+1; !c
let ai_counter =
    let c = ref (-1) in
    fun () -> c := !c+1; !c
let user_pieces_lost = Array.make 40 {rank=12; player=true; hasBeenSeen=false}
let ai_pieces_lost = Array.make 40 {rank=12; player=true; hasBeenSeen=false}

let rec none_whole_board board pos=
    match pos with
    |(9,9) -> board
    |(10,y) -> none_whole_board board (0, y+1)
    |(x,y) -> let brd = add_mapping (x,y) None board
                in none_whole_board brd (x+1,y)


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

    display_board board;
    let user_inpddut = read_line () in
    print_message ("Where would you like to place your "
                            ^(string_from_piece piece)^ "? (ex. 00)");
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
            | Some p -> failwith "A piece is already there!"

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
    let new_board = (*empty_board ()*)none_whole_board (empty_board ()) (0,0) in
    let () = print_endline("marker one") in
    (*let () = display_board new_board in*)
    let full_pieces = get_list_all_pieces () in
    let user_board = instantiate_user_board new_board full_pieces in
    (*let gap_filled_board = fill_rows user_board 0 in*)
    let start_board = setup_board (*gap_filled_board*)user_board in
    let () = display_board start_board in
    start_board

let parse_user_input c =
    let trim_c = c |> String.trim |> String.lowercase_ascii in
        if (String.contains trim_c ' ') = false then (trim_c, "")
    else
        let space_index = String.index trim_c ' ' in
        let first_word = (String.sub trim_c 0 space_index) |> String.trim in
        let second_half_length = (String.length trim_c) - space_index in
        let second_half = String.sub trim_c space_index second_half_length in
        let trim_second_half = String.trim second_half in
        (first_word, trim_second_half)

let tuple_from_string str =
    let x = int_of_char (String.get str 0) in
    let y = int_of_char (String.get str 1) in
    (x, y)

let append_to_cap lst =
    match lst with
    | [] -> ()
    | h::[] ->
        if h.player then
            let index = user_counter () in
            (user_pieces_lost.(index) <- h)
        else
            let index = ai_counter () in
            (ai_pieces_lost.(index) <- h)
    | h1::h2::[] ->
        if h1.player then
            let index = user_counter () in
            let index2 = user_counter () in
            (user_pieces_lost.(index) <- h1);
            (user_pieces_lost.(index2) <- h2)
        else
            let index = ai_counter () in
            let index2 = ai_counter () in
            (user_pieces_lost.(index) <- h1);
            (user_pieces_lost.(index2) <- h2)
    | _ -> failwith "Invalid captured pieces?"

let execute_movement board num1 num2 =
    let pos_one = tuple_from_string num1 in
    let pos_two = tuple_from_string num2 in
    let valid_move = is_valid_move board false pos_one pos_two in
    if (fst valid_move) then
        let (new_board, captured, str) = make_move board pos_one pos_two in
        let _ = append_to_cap (captured) in
        let _ = print_message str in
        new_board
    else
        let _ = print_message (snd valid_move) in
        raise Illegal

let is_num pos_one pos_two =
    if (String.length pos_one) = 2 then
        if (String.length pos_two) = 2 then
            let a = try (int_of_string pos_one)=(int_of_string pos_one) with
                        | _ -> false in
            let b = try (int_of_string pos_two)=(int_of_string pos_two) with
                        | _ -> false in
            a && b
        else false
    else false

let handle_user_input cmd board =
    match cmd with
    | ("table", "") ->
        let _ = display_table () in
        Active (board)
    | ("captured", "") ->
        print_message "User's Pieces Lost:";
        (print_list (Array.to_list user_pieces_lost));
        print_message "AI's Pieces Lost:";
        print_list (Array.to_list ai_pieces_lost);
        Active (board)
    | (p1, p2) when (is_num p1 p2) -> execute_movement board p1 p2
    | _ -> failwith "Unimplemented"

let check_winner b =
    match b with
    | Victory b -> true
    | _ -> false

let strip_variant var =
    match var with
    | Victory b -> failwith "Shouldn't be passing Victory"
    | Active board -> board

let rec play (board:board) : board =
    let _ = display_board board in
    let _ = print_message "It's your turn! What would you like to do?" in
    let user_input = read_line () in
    let user_tuple = parse_user_input user_input in
    let user_board = try (handle_user_input user_tuple board) with
                            | Illegal -> Active (play board) in
    let win = check_winner user_board in
    if win then
        board
    else
        let stripped_board = (strip_variant user_board) in
        let (ai_board, captured, msg) = choose_best_board stripped_board in
        let ai_win = check_winner ai_board in
        if ai_win then
            (strip_variant ai_board)
        else
            let _ = append_to_cap captured in
            let _ = print_message msg in
            play (strip_variant ai_board)
