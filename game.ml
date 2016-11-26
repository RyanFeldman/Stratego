open Board.GameBoard
open Display.TextDisplay

type board = t

let parse_user_input (c:string) : position * position= 
	let trimmed_c = c |> String.trim in 
	let space_ind = trimmed_c |> String.index in 
	((0, 0),(0, 0))

let rec get_user_input board = 
    let _ = Display.TextDisplay.display_board board in 
    let user_input = read_line () in 
    let (pos_one, pos_two) = parse_user_input user_input in 
    board

let rec instantiate_user_board board = function
| [] -> board
| h::t ->
    let new_board = get_user_input board in
    instantiate_user_board new_board t

let get_list_all_pieces () = 
    let p       = {rank=0; player=false; hasBeenSeen=false} in 
    let col     = {p with rank=8} in 
    let major   = {p with rank=7} in 
    let cap     = {p with rank=6} in 
    let lieut   = {p with rank=5} in 
    let serg    = {p with rank=4} in 
    let miner   = {p with rank=3} in 
    let scout   = {p with rank=2} in 
    let bomb_list = [p; p; p; p; p; p] in 
    let marsh_list = [{p with rank=10}] in 
    let gen_list = [{p with rank=9}] in 
    let col_list = [col; col] in 
    let maj_list = [major; major; major] in 
    let cap_list = [cap; cap; cap; cap] in 
    let lieut_list = [lieut; lieut; lieut; lieut] in 
    let serg_list = [serg; serg; serg; serg] in 
    let mine_list = [miner; miner; miner; miner; miner] in 
    let sco_list = [scout; scout; scout; scout; scout; scout; 
                    scout; scout] in 
    let spy_list = [{p with rank=1}] in 
    let flag_lst = [{p with rank=11}] in 
    bomb_list @ marsh_list @ gen_list @ col_list @ maj_list @ cap_list @
    lieut_list @ serg_list @ mine_list @ sco_list @ spy_list @ flag_lst

let setup_game () = 
	let new_board = empty_board () in
    let full_pieces = get_list_all_pieces () in
    let user_board = instantiate_user_board new_board full_pieces in
    user_board
	
		

let play (board:board) = failwith "Unimplemented"