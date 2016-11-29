open Game

(* See main.mli *)
let play_game () =
  print_string "\n\nWelcome to Stratego.\n
    Please make your terminal full screen for the best experience.\n";
  print_endline "Type RULES to learn how to play or PLAY to start.";
  let user_input = read_line () in
  let trimmed = user_input |> String.trim |> String.lowercase_ascii in
  let () = if (trimmed = "rules") then (
    Display.TextDisplay.display_rules ();
    print_endline "Input anything to continue...";
    let _ = read_line () in ()) else () in
  let initial_board = setup_game () in
  let dumb = (fun x -> ()) in
  dumb (play initial_board)
