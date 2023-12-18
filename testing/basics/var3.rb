# Assign three values.
a, b, c = 8, 10, 15
print "A: a = ", a, ", b = ", b, ", c = ", c, "\n"

# Compute three values, then assign three values.
a, b, c = 40, a + 11, a + b + c
print "B: a = ", a, ", b = ", b, ", c = ", c, "\n"

# Swap.
a, b = b, a
print "C: a = ", a, ", b = ", b, ", c = ", c, "\n"

# Extras on left get nil.
a, b, c = 2, 3
print "D: a = ", a, ", b = ", b, ", c = ", c, "\n"

# Extras on right get left behind
a, b, c = 11, 12, 13, 14, 15
print "E: a = ", a, ", b = ", b, ", c = ", c, "\n"

# The right can be an array, in which case the members are assigned to
# individual variables.
fred = [ 4, 5, 6, 7]
a, b, c = fred
print "F: a = ", a, ", b = ", b, ", c = ", c, "\n"