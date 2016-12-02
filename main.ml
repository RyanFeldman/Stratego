open Game

(* See main.mli *)
let () =
  print_string "\n\n  Welcome to Stratego.\n
  Please make your terminal full screen for the best experience.\n\n";
  print_endline "  Type RULES to learn how to play. Type anything else to start.\n";
  print_string ">";
  let user_input = read_line () in
  let trimmed = user_input |> String.trim |> String.lowercase_ascii in
  let () =
      if (trimmed = "rules") then (
          Display.TextDisplay.display_rules ();
          print_endline "\nType anything to continue...";
          let _ = read_line () in ())
      else () in
  print_endline "Type AUTO to automatically fill the board.\n";
  print_endline "Type anything else to manually setup of the board";
  print_string ">";
  let new_input = read_line () in
  let new_trimmed = new_input |> String.trim |> String.lowercase_ascii in
  let initial_board =
      if new_trimmed = "auto" then
          auto_setup ()
      else
          manual_setup () in
  let _ = play initial_board in
  ()
