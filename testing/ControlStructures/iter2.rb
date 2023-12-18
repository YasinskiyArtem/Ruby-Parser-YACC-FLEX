fred = [ 4, 9, 18, 3, 87, 9, 12 ]
alex = [ 'Susan', 'Joe', 'Alex', 'Alice', 'Sam' ]

# Compute a new array with each member of fred doubled.
fred = fred.map { |x| 2 * x }
print fred.join(" "), "\n"

# Create a new alex adding " went away" to each member.  Then join and
# print the result.
print (alex.map { |z| z + " went away" }).join("  "), "\n"

# Print the members of fred which are more than five and less than 20.
print (fred.select { |z| z > 5 && z < 20 }).join(" "), "\n"

# Print the lengths of the members of alex that start with A or end with e.
print ((alex.select { |n| n =~ /^A/ || n =~ /e$/ }).map { |z| z.length }).join(" "), "\n"

# Update alex by surround each of its members with [ ]
alex.map! { |a| "[" + a + "]" }
print alex.join(" "), "\n"
