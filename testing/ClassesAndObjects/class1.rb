# Class names must be capitalized.  Technically, it's a constant.
class Fred
  
  # The initialize method is the constructor.  The @val is
  # an object value.
  def initialize(v)
    @val = v
  end

  # Set it and get it.
  def set(v)
    @val = v
  end

  def get
    return @val
  end
end

# Objects are created by the new method of the class object.
a = Fred.new(10)
b = Fred.new(22)

print "A: ", a.get, " ", b.get,"\n";
b.set(34)
print "B: ", a.get, " ", b.get,"\n";

# Ruby classes are always unfinished works.  This does not
# re-define Fred, it adds more stuff to it.
class Fred 
  def inc
    @val += 1
  end
end

a.inc
b.inc
print "C: ", a.get, " ", b.get,"\n";

# Objects may have methods all to themselves.
def b.dec
  @val -= 1
end

begin
  b.dec
  a.dec
rescue StandardError = 1
  print "Error: ", msg, "\n"
end

print "D: ", a.get, " ", b.get,"\n";