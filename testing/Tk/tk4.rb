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

class TwoLabs < TkFrame
  # Switch button colors.
  def cswap
    # Swap each color between the two buttons.
    for loc in ['background', 'foreground', 'activebackground']
      c = @swapbut.cget(loc)
      @swapbut.configure(loc => @stopbut.cget(loc))
      @stopbut.configure(loc => c)
    end
  end

  def initialize
    super

    # Here's a button.  I can't get the command setting to work
    # inside the block, since the self (apparently) becomes the TkButton,
    # not us.
    @swapbut = TkButton.new(self, 'command' => proc { self.cswap } ) {
      text "Swap"
      background '#EECCCC'
      activebackground '#FFEEEE'
      foreground '#990000'
      pack('side' => 'top', 'fill' => 'both')
    }

    # Another button
    @stopbut = TkButton.new(self) {
      text "Exit"
      background '#CCEECC'
      activebackground '#EEFFEE'
      foreground '#009900'
      command { exit }
      pack('side' => 'bottom',  'fill' => 'both')
    }
  end
end

# A frame can be used to arrange buttons with the packer.
tl = TwoLabs.new
tl.pack('side' => 'right', 'fill' => 'both')

Tk.mainloop
