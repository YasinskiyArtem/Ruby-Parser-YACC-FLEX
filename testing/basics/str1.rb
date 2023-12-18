# Double-quoted strings can substitute variables.
a = 17
print "a = #{a}\n";
print 'a = #{a}\n';

print "\n";
b = <<bruh
This is a longer string,
perhaps some instructions or agreement
goes here. By the way,
bruh
# If you're verbose, you can create a multi-line string like this.


print "\n[[[" + b + "]]]\n";

print "Actually, any string can span lines.  The line\nbreaks just become part of the string."

print %Q=\nThe highly intuitive "%Q" prefix allows alternative delimiters.\n=
print %Q[Bracket symbols match their mates, not themselves.\n]
