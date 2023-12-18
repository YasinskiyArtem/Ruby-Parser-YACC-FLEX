require("csim")

# The circuit here should simulate a level-triggered D flip-flop
D = SwitchBank.new
C = Pulser.new
N = NotGate.new
A1, A2 = AndGate.many(2)
NR1, NR2 = NorGate.many(2)
Q = LED.new("  Q")
Qnot = LED.new("  ~Q")

D.join(N)
N.join(A1)
D.join(A2)
C.join(A1)
C.join(A2)
A1.join(NR1)
A2.join(NR2)
NR1.join(NR2)
NR2.join(NR1)
LED.shush(true)
NR1.join(Q)
NR2.join(Qnot)
LED.shush(false)

vals = [ true, false, false, true, false ]
10.times do
  3.times 
    dval = vals.shift
    vals.push(dval)
    print "Input: ", dval, "\n"
    D.value = dval

  print "=== TIC ===\n"
  C.pulse
end