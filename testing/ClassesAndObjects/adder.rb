#
# An N-bit adder built of one-bit adders with simple carry propagation.
#

require "oba"

class Adder < Blueprint
  # This initializes the object,  If we already have a one-bit adder
  # blueprint lying around, we can reuse it with that second argument.
  def initialize(n, bp = nil)
    super(n.to_s + "-bit adder")

    # Blueprint for a the one-bit adder
    bp = OBA.new 
    @one_bit_bp = bp

    # Make all the adders, and specify them as inputs.
    addrs = bp.manyothers(n)
    self.inputs(addrs,addrs)

    # Create output connectors and chain the carries.
    prev = nil
    for addr in addrs 
      c = Connector.new
      addr.join(c)
      if prev then
        prev.out(1).join(addr)
      end
      self.outputs(c)

      prev = addr
    end

    # Overflow
    self.outputs(prev)

    self.lock
  end

  attr_reader :one_bit_bp
end
