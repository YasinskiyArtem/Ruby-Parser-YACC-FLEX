require("csim")

S = SwitchBank.new
A = AndGate.new
#A.join(Tester.new("A"))
B = OrGate.new
#B.join(Tester.new("B"))
C = XorGate.new
#C.join(Tester.new("C"))
L = LED.new('Result');

S.join(A)
S.join(A)
A.join(B)
S.join(B)
B.join(C)
S.join(C)
C.join(L)

for x in (0..15)
  for s in [3,2,1,0]
    print (x>>s) & 1
  end
  print " => "
  S.value = x
end
