#!/usr/bin/ruby

# Import the library.
require 'tk'

# Root window.
root = TkRoot.new  { 
  title 'Push Me' 
  background '#111188'
}

# Add a label to the root window.
lab = TkLabel.new(root) { 
  text "Hey there,\nPush a button!"
  background '#3333AA'
  foreground '#CCCCFF'
}

# Make it appear.
#lab.pack('side' => 'left', 'fill' => 'both')

# A frame can be used to arrange buttons with the packer.
fr = TkFrame.new
#fr.pack('side' => 'right', 'fill' => 'both')

# Here's a button.  Added to the frame, not the root.
swapbut = TkButton.new(fr) {
  text "Swap"
  background '#EECCCC'
  activebackground '#FFEEEE'
  foreground '#990000'
  pack('side' => 'top', 'fill' => 'both')
}

# Another button
stopbut = TkButton.new(fr) {
  text "Exit"
  background '#CCEECC'
  activebackground '#EEFFEE'
  foreground '#009900'
  command { exit }
  pack('side' => 'bottom',  'fill' => 'both')
}

# Switch button colors.
def cswap(b1, b2)
  # Swap each color between the two buttons.
  for loc in ['background', 'foreground', 'activebackground']
    c = b1.cget(loc)
    b1.configure(loc => b2.cget(loc))
    b2.configure(loc => c)
  end
end

swapbut.configure( 'command' => proc { cswap(swapbut, stopbut) } )

Tk.mainloop
