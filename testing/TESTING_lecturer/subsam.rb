require "csim"

# This represents a compound of gates.  It has an interface similar enough
# to Gate to be used in place of one, though it isn't derived from Gate.
# Ruby thinks this is just fine.

# To build compound circuits, you must create a Blueprint.  To do so:
#   1. Create the Blueprint object.
#   2. Create the objects, and connect them to each other.  Don't make
#      any external connections.
#   3. As needed specify gates you create as input or output devices for
#      the compund.
#   4. Call lock().  This completes the Bluprint and makes it ready to
#      generate compounds.
# The resulting Blueprint is also a Compound.  It can be connected and used
# like a Gate (though it is not derived from Gate).  The another method
# of Blueprint creates a Compound, which can be used in a circuit, but
# lacks the building infrastructure.  The Compound remembers its Blueprint,
# and its another method simply calls the one in Blueprint.
#
# You should not add the same component to multiple compounds.  

class Compound
protected
  # This is called only by Blueprint.  Clients don't get to create
  # Compounds themselves.
  def initialize(ingates, outgates, blueprint)
    @ingates = ingates          # Initially array of input gates, replaced
                                # on each connect with a connection to it.
    @conndex = 1               # Index for connecting to it.
    @outgates = outgates        # Array of output gates.
    @joindex = 1               # Index for joining.
    @blueprint = blueprint      # Our blueprint object.
  end

public
  # Connect to a specific input port.
  def portconn(port, v)
    c = @ingates[port]
    c.signal(v)
    return GateLinkHandlenew(self,port)
  end

  # Connect to the next input port.
  def connect(v)
    return portconn(@conndex + 1, v)
  end

  # Join an output to another device.  Outputs are joined in the order 
  # specified by outputs, or out(n) may be used to join to a specific output.
  def out(n)
    return @outgates[n]
  end
#  def join(gate)
#    @outgates[@joindex + 1] 
#  end

  # Handle inbound signals.
  def signal(port, val)
    Gate.activate
    @ingates[port].signal(val)
    Gate.deactivate
  end

  # All the links on the output list, except for internal connections.
  def outlinks ()
    ret = [ ]
    for i in (0...@outgates.length)
      outs = @outgates[i].outlinks
      for j in (0...outs.length) 
        ret.push(outs[j]) if outs[j] && @blueprint.internmap[i][j] != 1
      end
    end
    return ret
  end

  # This creates another one of us.  This runs in the blueprint, which
  # has more information.
  def another
    return @blueprint.another
  end

  # Create several others of us in an array.
  def manyothers(n)
    ret = []
    n.times { ret.push(self.another) }
    return ret
  end

  # For prettier printing.
  def to_s
    ret = self.class.to_s
    st = @blueprint.subtype
    ret += " [" + st + "]" if st
    return ret + " " + self.object_id.to_s
  end

end

# Class Blueprint is used to describe a component made of other components.
class Blueprint < Compound
  # Note: Initialization is not really complete until the lock method is 
  # called.
  def initialize(subtype = nil)
    super([], [], self)
    @allgates = [ ]     # This is a list of connections to internal gates
                        #  which represent the device inputs.
    @allcons = [ ]      # List of [src, sink] in proper connect order.
    @internmap = [ ]    # For each output gate, a mask of connections which
                        #  are internal.
    @subtype = subtype
  end

  # Specify gates as inputs to the circuits.  You may specify any number of
  # Gates or Compounds, or arrays thereof.  Each input makes a connection to
  # the specified gate in the order given; gates may be repeated when the
  # they form more than one input.  If input order matters, be careful to
  # mix the specification of gates as inputs, and the joining of internal
  # connections, in the correct order.
  def inputs(*gates)
    for g in flatten(gates)
      @ingates.push(g.connect(false))
    end
  end

  # This specifies some gates which are outputs.  Gates, Compounds, or arrays
  # thereof may be specified.  Output connections are made to these gates
  # in the order given.  Gates may be repeated to supply multiple ouputs.
  # It is also possible to use a specific output connection number to
  # join multiple devices to the same port.
  def outputs(*gates)
    @outgates += flatten(gates)
  end

  # This closes the definition.  It finds all the objects in the collection,
  # and makes a list of pairs that can be used to reproduce all the connections
  # preserving relative order at each end.  This involves a topological sort.
  # Yecccch.
  def lock
    # Create the initial pending list of all input gates, plus all the
    # output gates (which should be redundant, but ...)
    gset = { }
    ((@ingates.map { |c| c.sinkg }) + @outgates).each { |g| gset[g] = true }
    pend = gset.keys

    # Process pending gates for outlinks.  Find all devices which are
    # connected downstream from an input or output.
    while g = pend.shift
      # Scan the receiving gates.  We take the list of outbound connections
      # and extract the sink gates therein, and run through that.
      for t in g.outlinks.map { |lnk| lnk.sinkg }
        if ! gset.has_key?(t) then
          # If not already in the set, add it to the set and the pending list.
          pend.push(t)
          gset[t] = true
        end
      end
    end

    # These are all the reachable gates; all the gates in the device.
    @allgates = gset.keys

    # Allocate graph nodes for each connection.  This allocates a node for
    # each connection between two contained gates.  Each node is an array
    # of the form
    #   [ srcnext, sinknext, predcnt, src, sink ]
    # Where the first two point to nodes that come later in the order at
    # the source or destination (respectively), predcnt is an integer number
    # of nodes which must come before this one, and src and sink are the
    # gates at the start and end of the connection.  This loop allocates
    # such nodes for each connection, adds links (only) for the source
    # ordering, and places them in a hash so we can find them by destination.
    nmap = { }
    n = 0
    for g in @allgates
      lnode = nil       # Previous node in source ordering.
      for c in g.outlinks
        # Create the node and store it in the hash.
        key = [ c.sinkg, c.sinkp ]
        node = [ nil, nil, (if lnode then 1 else 0 end), g, c.sinkg ]
        nmap[key] = node

        # Add a link here to the previous node.
        lnode[0] = node if lnode

        lnode = node
      end
      n += 1
    end

    # This adds more nodes to represent the array of input links.  These
    # nodes are like the above, except source position is nil.
    lnode = nil
    for c in @ingates do
        key = [ c.sinkg, c.sinkp ]
        node = [ nil, nil, (if lnode then 1 else 0 end), nil, c.sinkg ]
        nmap[key] = node
        lnode[0] = node if lnode
        lnode = node
    end

    # Now we go through and add links representing the ordering at the
    # destination.
    for k in nmap.keys          # Scan all node keys.
      gate, port = k            # Get the destination information
      targ = [gate, port+1]     # Get key of next node in destination order
      if nmap.has_key?(targ)    # See if such a node exists.
        tnode = nmap[targ]      # Get the sink successor from the hash.
        tnode[2] += 1           # Increment its pred. count for source node.
        nmap[k][1] = tnode      # Add a link from source to sink node.
      end
    end

    # Find all the roots (nodes without predecessors).  This is the initial
    # value of links which may be established, since all links which must
    # appear earlier have been created.
    ready = nmap.values.select { |n| n[2] == 0 }

    # Traverse the graph obeyong all the constratints and produce a list
    # of connections in an order which will preserve the order at each end.
    # That preserves the creation order of the links in case it matters.
    @allcons = [ ]
    while n = ready.shift
      # Extract the contents of the ready node and add the relevant information
      # to the output order list.
      srcnext, sinknext, count, source, sink, sinkp = n
      @allcons.push([source, sink])

      # Reduce the count of each successor, and add it each to the ready list 
      # if it has no more predecessors.
      if srcnext then
        srcnext[2] -= 1;
        ready.push(srcnext) if srcnext[2] == 0
      end
      if sinknext then
        sinknext[2] -= 1;
        ready.push(sinknext) if sinknext[2] == 0
      end
    end

    # This makes a record of all internal links outbound from output
    # devices.  These are not to be reported as connections out from the
    # compound.  They are recorded in an array parallel to @outgates.
    # For each output gate, they contain a bitmap showing ones for each
    # position occupied at this time.
    @internmap = @outgates.map do |g|
      ret = 0
      bit = 1
      g.outlinks.each { |c| if c then ret &= bit end; bit <<= 1; }
      ret
    end

    # Whew!
  end

  # Make a Compound like this one.
  def another
    # Make copies of all the objects, and keep a map from the original to the
    # copy, so we can copy the connections.
    copymap = { }
    @allgates.each { |g| copymap[g] = g.another }

    # Reproduce all the connections on the new gates.  Use the order computed
    # by close which preserves order of connections at each end.
    ingates = [ ]
    for c in @allcons
      if c[0] then
        copymap[c[0]].join(copymap[c[1]])
      else
        # nil source indicates an input connection
        ingates.push(copymap[c[1]].connect(false))
      end
    end

    # Construct the new compound using the new gates.
    return Compound.new(ingates, @outgates.map { |g| copymap[g] }, self)
  end

  attr_reader :internmap, :subtype

private
  # This flattens an list by opening component arrays.  
  def flatten(a)
    ret = []
    a.each { |x| if x.is_a?(Array) then ret += x else ret.push(x) end }
    return ret
  end
end