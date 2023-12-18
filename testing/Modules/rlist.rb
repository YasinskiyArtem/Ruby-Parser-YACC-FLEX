require("list")
require("tree")

print "=== List test ===\n"
x = List.new(10)
x.at_front(33)
x.at_front(28)
x.at_end(12)
x.at_front(3)
x.at_end(71)

x.pr(true)

s = 0
x.each { |n| s += n }
print "sum = ", s, "\n"

print "\n=== Tree test ===\n"
t = Tree.new(28)
t.insert(38)
t.insert(1)
t.insert(39)
t.insert(17)
t.insert(22)
t.insert(8)
t.insert(11)

t.pr(true)

s = 0
t.each { |n| s += n }
print "sum = ", s, "\n"

print "Max is ", t.max, "\n"
