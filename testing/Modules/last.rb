# The two small modules here are intended to contain generic facilities
# which can be used by classes.

# This follows using the next method until we get to the end of whatever it
# is.
module Follower
  def last
    at = self
    while true
      n = at.next
      if n == nil then return at end
      at = n
    end
  end
end

# This prints on one line using the each method.
module Printer
  def pr(newline = false)
    self.each { |x| print x, " " }
    print "\n" if newline
  end
end