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

let user_pieces_lost = Array.make 40 (make_piece 12 true false)

let ai_pieces_lost = Array.make 40 (make_piece 12 true false)

let parse_user_input (c:string) : position =
    let trimmed_c = c |> String.trim in
    if (String.length trimmed_c) = 2 then
        let x_one = (String.get trimmed_c 0) |> int_of_char in
        let y_one = (String.get trimmed_c 1) |> int_of_char in
        make_position (x_one-48) (y_one-48)
    else
        failwith "Invalid string length"

let rec get_user_input (board:board) (piece:piece) : board =
    print_message ("Where would you like to place your "
                            ^(string_from_piece piece)^ "? (ex. 00)");
    let user_input = read_line () in
    let (x, y) = get_position (parse_user_input user_input) in
    if (y > 3 || y < 0 || x < 0 || x > 9)
        then failwith "Invalid xy coordinate input"
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
                    let _ = print_message ("\nSorry, your input must be in the"
                        ^" form 'xy' to place your piece at (x, y)! As a reminder,"
                        ^" you must place your pieces in the first 4 rows and two "
                        ^"pieces cannot be placed on top of each other to start."
                        ^"\n\n") in
                        board) in
        if (equal_board new_board board) then
            instantiate_user_board board (h::t)
        else
            let () = display_board new_board in
            instantiate_user_board new_board t

let auto_setup () =
    let () = Random.self_init () in
    let all_pieces = get_list_all_pieces () in
    let shuffled = List.sort (fun x y -> Random.int 2) all_pieces in
    let init_board = none_whole_board (empty_board ()) (0,0) in
    let player_brd = fill init_board [] shuffled (0,0) true in
    let completed_board = ai_setup player_brd in
    let () = display_board completed_board in
    completed_board

let manual_setup () =
    let new_board = none_whole_board (empty_board ()) (0,0) in
    let full_pieces = get_list_all_pieces () in
    let user_board = instantiate_user_board new_board full_pieces in
    let start_board = ai_setup user_board in
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
    (x-48, y-48)

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
    let valid_move = is_valid_move board true pos_one pos_two in
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
        let user_lst = List.filter (fun x -> x.rank <> 12) 
                        (Array.to_list user_pieces_lost) in 
        let ai_lst = List.filter (fun x -> x.rank <> 12) 
                        (Array.to_list ai_pieces_lost) in 
        print_message "User's Pieces Lost:";
        (print_list (user_lst));
        print_message "AI's Pieces Lost:";
        print_list (ai_lst);
        Active (board)
    | ("quit", "") -> (print_message ("Did the 3110 students quit when their "
                        ^"final project was due in 9 days? Oh well, your choice.")); 
                        Victory (false)
    | (p1, p2) when (is_num p1 p2) -> execute_movement board p1 p2
    | _ ->
        let () = (print_message ("\n\nSorry, I don't quite understand your input.\n"^
                    "Remember: To move, type the position of the piece you want"^
                    " to move followed by the target location (ex. 00 01). At "^
                    "any time, type \"table\" to see the pieces reference table"^
                    ", or type \"captured\" to see the pieces that have been "^
                    "captured.\n")) in
        raise Illegal

let check_winner b =
    match b with
    | Victory b -> true
    | _ -> false

let strip_variant var =
    match var with
    | Victory b -> failwith "Shouldn't be passing Victory"
    | Active board -> board

let rec play (board:board) : board =
    let _ = print_message "It's your turn! What would you like to do?" in
    let user_input = read_line () in
    let user_tuple = parse_user_input user_input in
    let user_board = try (handle_user_input user_tuple board) with
                            | Illegal -> Active (play board) in
    let win = check_winner user_board in
    if win then
        board
    else
        if (equal_board (strip_variant user_board) board) then
            let _ = display_board board in
            play (strip_variant user_board)
        else
            let stripped_board = (strip_variant user_board) in
            let (ai_board, captured, msg) = choose_best_board stripped_board in
            let ai_win = check_winner ai_board in
            if ai_win then
                (strip_variant ai_board)
            else
                let _ = append_to_cap captured in
                let _ = display_board (strip_variant ai_board) in
                let _ = print_message msg in
                play (strip_variant ai_board)
