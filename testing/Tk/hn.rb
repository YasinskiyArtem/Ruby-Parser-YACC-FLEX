#
# Host name lookup widget.
#
require 'tk'
require 'socket'

# Set colors.
BG = '#AAAAFF'
TkOption.add('*background', BG)
TkOption.add('*activeBackground', '#CCCCFF')
TkOption.add('*foreground', '#884400')

# A label which does the needed lookup.
class HostnameLabel < TkLabel
  
  # Look up host name in the assocated entry widget source, and display
  # in ourselves.
  def show
    hn = @source.get.strip
    if hn == ''
      ip = ''
    else
      begin
        ip = IPSocket.getaddress(hn)
      rescue
        ip = '[unknown]'
      end
    end
    configure('text' => ip)
  end

  # Create the widget, and bind the return key to run the lookup method (show).
  def initialize(root, entry)
    super(root, 'text' => '', 'width' => 15)
    @source = entry
    entry.bind('Return', proc { self.show })
  end      
end        

# Root window
root = TkRoot.new('background' => BG) { title 'Host Conversion' }

# Title label
tit = TkLabel.new {
  text "Host Name Conversion"
  relief 'groove' 
  grid('row' => 0, 'column' => 0, 'columnspan' => 2, 'sticky' => 'news')
}

# Name entry. 
entr = TkEntry.new {
  width 25
  grid('row' => 1, 'column' => 0, 'columnspan' => 2, 'sticky' => 'news')
}
dislab = nil	# This needs to exist since we refer to it in the bind.
entr.bind('Button-1', proc { |e|
            entr.delete(0,'end')
            dislab.configure('text' => '')
          })

# Reporting label.
dislab = HostnameLabel.new(root, entr)
dislab.grid('row' => 2, 'column' => 0, 'sticky' => 'news')

# Go button.
but = TkButton.new {
  text "Find"
  command { dislab.show }
  grid('row' => 2, 'column' => 1, 'sticky' => 'news')
}

Tk.mainloop

