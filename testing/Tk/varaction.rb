#
# This shows a potential problem in providing button actions.  The code blocks
# are run in the context of the caller, but not until the button is pressed.  
# Therefore variable values are current as of the button press.  This is not
# always what you want.
#
require 'tk'

# Root window
root = TkRoot.new

# Three buttons created in a loop, numbering from 1 to 3.
bnum = 1
for fred in [ 17, 348, -48 ]
  TkButton.new(root) {
    text "Button " + bnum.to_s
    command proc { print "Button ", bnum, ", fred = ", fred, "\n" }
    grid('column' => 0, 'row' => bnum-1, 'sticky' => 'news')
  }

  bnum += 1
end

# This holds and prints two values.  The parms are evaluated when the
# object is created, so we get the right values.  The button command
# will use any object which understands the call method.
class Printer
  def initialize(num, fred)
    @num = num
    @fred = fred
  end
  def call
    print "Button ", @num, ", fred = ", @fred, "\n"
  end
end

# Three more buttons, but this time they use the Printer class and work
# correctly.
for fred in [ 'Dogbert', 97, 111 ]
  TkButton.new(root) {
    text "Button " + bnum.to_s
    command Printer.new(bnum, fred)
    grid('column' => 1, 'row' => bnum-4, 'sticky' => 'news')
  }

  bnum += 1
end

# This is a more generic solution
class Runner
  # Set a proc and some args.
  def initialize(method, *args)
    @method = method
    @args = args
  end

  # Run it with the args.
  def call
    @method.call(*@args)
  end
end

# Three more, but this time they work correctly.
for fred in [ 86, 12, 123 ]
  TkButton.new(root) {
    text "Button " + bnum.to_s
    command Runner.new(proc { |n, f| print "Button ", n, ", fred = ", f, "\n" }, bnum, fred)
    grid('column' => 2, 'row' => bnum-7, 'sticky' => 'news')
  }

  bnum += 1
end

# These are sort of button-like, but use binding.
lnum = 1
for fred in [ 2973, 'Nosebleed', 349 ]
  b = TkLabel.new(root) {
    text "Label " + lnum.to_s
    grid('column' => 3, 'row' => lnum-1, 'sticky' => 'news')
    relief 'raised'
    background '#999999'
    padx 10
  }
  b.bind('Button-1', proc { print "Label ", lnum, ", fred = ", fred, "\n" })

  lnum += 1
end

# This shows a nice feature of bind which allows extra parameters to be
# sent in.
for fred in [ 98733, 128, 'Norbert' ]
  b = TkLabel.new(root) {
    text "Label " + lnum.to_s
    grid('column' => 4, 'row' => lnum-4, 'sticky' => 'news')
    relief 'raised'
    background '#999999'
    padx 10
  }
  b.bind('Button-1', proc { | n, f | print "Label ", n, ", fred = ", f, "\n" }, lnum, fred)

  lnum += 1
end

Tk.mainloop
