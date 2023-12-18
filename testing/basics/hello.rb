print "Hello, World!\n";

def fact (n)
    result = 1
    for i in 1..n do
      result *= i
    end
    result
  end
  
  puts fact(100)
