type board 

module type Display = sig 

	val display_board : board -> unit

end

module TextDisplay : Display
module GuiDisplay : Display
