#
# This defines a one-bit adder.
#

require "csim"
require "cgrp"

class OBA < Blueprint
  def initialize
    super("one-bit adder")

    # Get the parts.
    a,b,cin = Connector.many(3)
    gA = XorGate.new
    gB,gC,gD = AndGate.many(3)
    gE = OrGate.new

    # Hook 'em up.
    a.joinmany(gA,gB,gC)
    b.joinmany(gA,gB,gD)
    cin.joinmany(gA,gC,gD)
    [gB,gC,gD].each { |g| g.join(gE) }

    # Put it into a box.
    self.inputs(a,b,cin)
    self.outputs(gA, gE)
    self.lock
  end
end
