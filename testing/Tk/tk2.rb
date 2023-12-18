#!/usr/bin/ruby

# Import the library.
require 'tk'

# Root window.
root = TkRoot.new { title 'Push Me' 
  background '#111188'
}

# Add a label to the root window.
lab = TkLabel.new(root) { 
  text "Push the Button" 
  background '#3333AA'
  foreground '#CCCCFF'
}

# Make it appear.
lab.pack('side' => 'left', 'fill' => 'y')

# Here's a button.  Also added to root by default.
TkButton.new {
  text "PUSH"
  background '#EECCCC'
  activebackground '#FFEEEE'
  foreground '#990000'
  command { print "Arrrrrrg!\n" }
  pack('side' => 'right')
}

Tk.mainloop
