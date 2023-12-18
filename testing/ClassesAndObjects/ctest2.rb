require("csim")

S = SwitchBank.new
A = AndGate.new
#A.join(Tester.new("A"))
B = OrGate.new
#B.join(Tester.new("B"))
C = AndGate.new
#C.join(Tester.new("C"))
D = OrGate.new
#D.join(Tester.new("D"))
E = NotGate.new
L = LED.new('Result')

A.join(D)
B.join(D)
C.join(D)
D.join(E)
D.join(A)
D.join(C)

S.join(A)
S.join(B)
S.join(B)
S.join(C)

E.join(L)

for x in (0..15)
  for s in [3,2,1,0]
    print (x>>s) & 1
  end
  print " => "
  S.value = x
end
