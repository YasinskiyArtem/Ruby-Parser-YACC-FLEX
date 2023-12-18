require("last")

# A BST class.  Again, the purpose is a demo, not because you need one when
# you already have the Ruby datatypes.

class Tree
  class Node
    # Again, we include the follower class.
    include Follower

    def initialize(d)
      @val = d
      @lft, @rgt = nil
    end
    attr_reader :lft, :rgt, :val
    attr_writer :lft, :rgt

    # Our next function just moves right.  (See below)
    def next
      return @rgt
    end

    # Insert a new node into the subtree rooted here.
    def insert(new)
      if new.val < @val then
        if @lft == nil then
          @lft = new
        else
          @lft.insert(new)
        end
      else
        if @rgt == nil then
          @rgt = new
        else
          @rgt.insert(new)
        end
      end
    end

    # This runs for each value in the tree in sorted order.  The block
    # parameter is an object known as a closure.  They contain executable 
    # code, and blocks can become closures (see below).
    def each(block)
      if @lft then @lft.each(block) end
      block.call(@val)
      if @rgt then @rgt.each(block) end
    end
  end

  # Get the printing facility.
  include Printer

  def initialize(first)
    @root = Node.new(first)
  end

  # Insert a value.  Most of the work in Node#insert
  def insert(v)
    @root.insert(Node.new(v))
  end

  # Stepping right from the root until nil gives the max value in the tree.
  def max
    return @root.last.val
  end

  # The &blk notation converts the block used in the iterator into a
  # closure object, which we send to Node#each.
  def each(&blk)
    @root.each(blk)
  end
end
