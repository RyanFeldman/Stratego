type board

(*[Display] handles the task of showing the player the board*)
module type Display = sig

  (*[display_board] shows [board] to the player*)
	val display_board : board -> unit

end

(*[TextDisplay] is a module that displays the board via ASCII code in the
 * terminal*)
module TextDisplay : Display

(*[GuiDisplay] is a module that displays the board via a GUI*)
module GuiDisplay : Display
