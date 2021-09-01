module ConnectionSets

using MetaModelica
using ExportAll

import DataStructures
import ListUtil

import ..NFConnector
import ..CONNECTOR
import ..NFConnections
import ..Main.CONNECTION
import ..Main.split

const Connector = NFConnector
const Connections = NFConnections

#= Using the Disjoint set data structure =#
const Sets = DataStructures.DisjointSets{Connector}

""" #= Creates a new DisjointSets from a list of connection and flow variables. =#"""
function fromConnections(connections)
#  @info "fromConnections: Our connections" connections
  local sets::Sets = Sets() # duh, is not possible to give sz size hint to DisjointSets! -Adrian Pop
  local sz = listLength(connections.connections) + listLength(connections.flows)

  # Add flow variable to the sets, unless disabled by flag.
  # Do this here if NF_SCALARIZE to use fast addList for scalarized flows.

  # if ! Flags.isSet(Flags.DISABLE_SINGLE_FLOW_EQ) && Flags.isSet(Flags.NF_SCALARIZE) then
  ListUtil.fold(connections.flows, addConnector, sets)
  # end

  # Add the connections.
  ListUtil.fold1(connections.connections, addConnection, connections.broken, sets)

  # Add remaining flow variables to the sets, unless disabled by flag.
  # Do this after addConnection if not NF_SCALARIZE to get array dims right.
  # if ! Flags.isSet(Flags.DISABLE_SINGLE_FLOW_EQ) && ! Flags.isSet(Flags.NF_SCALARIZE) then
  # sets = ListUtil.fold(connections.flows, addSingleConnector, sets)
  # end
  return sets
end

""" #= Adds a single connector to the connection sets. =#"""
function addScalarConnector(conn, sets)
  push!(sets, conn)
  return sets
end

    "Adds a connection to the sets, which means merging the two sets that the
     connectors belong to, unless they already belong to the same set."

function addConnection(connection, broken, sets)
  local  conns;
  conns = split(connection);
  if ! listEmpty(broken)
    conns = list(c for c in conns if !(isBroken(c.lhs, c.rhs, broken)))
  end
  #TODO: Check variability of connectors. It's an error if either
  #connector is constant/parameter while the other isn't.
  for conn in conns
    merge!(conn.lhs, conn.rhs, sets);
  end
  return sets
 end #=addConnection=#


function addConnector(conn, sets)
  sets = addList(split(conn), sets)
  return sets
end

""" #= Adds a connector to the sets if it does not already exist =#"""
function addSingleConnector(conn, sets)
  for c in
    #= Connector. =# split(conn) 
    sets = find(c, sets)
  end
  return sets
end

#= What is the definition here? =#
function isBroken(c1, c2, broken)::Bool
  local cr1 = name(c1)
  local cr2 = name(c2)
  local b = false
  # print("Check: connect(" + ComponentRef.toString(cr1) + ", " + ComponentRef.toString(cr2) + ")\n");
  for c in broken
    @match ((lhs, rhs, _)) = c
    if (isPrefix(lhs, cr1) && isPrefix(rhs, cr2)) || (isPrefix(lhs, cr2) && isPrefix(rhs, cr1))
      # print("Ignore broken: connect(" + Connector.toString(c1) + ", " + Connector.toString(c2) + ")\n")
      b = true
      break
    end
  end
  return b;
end

""" Merges the two sets that the given entry belong to """
function merge!(conn1, conn2, sets)
  #= Get the index of the two sets now. =#
  (set1Idx, sets) = findSet(conn1, sets);
  (set2Idx, sets) = findSet(conn2, sets);
  #= We need to access the integer internals of disjoint set.=#
  DataStructures.union!(sets.internal, set1Idx, set2Idx)
  #= Return the modified set=#
  return sets
end

""" This function finds and returns the set that the given entry belongs to. """
function findSet(entry, sets)::Tuple
  (updatedSets, index) = find(entry, sets)
  #= The old interface expects find_root to return an integer =#
  setIdx::Int = findRoot!(updatedSets, index)
  return (setIdx, updatedSets)
end

""" Replication of the find root function"""
function findRoot!(sets, idx)
  #= Calls find root on the internal structures. 
  Should return an integer=#
  local x = DataStructures.find_root!(sets.internal, idx)
  return x
end

""" Find function, replication of the functionality in DisjointSets.mo """
function find(entry, sets)::Tuple
  if haskey(sets.intmap, entry)
    idx::Int = sets.intmap[entry]
    return (sets, idx)
  end
  #=Otherwise if the node does not exist. Create a new node=#
  (sets, idx) = add(entry, sets)
  return(sets, idx)
end
"""
  Extracts all the sets from the disjoint sets structure, and returns
  them as an array.

John:
  My understandning is that we find each group. 
  For each group we create a list of the elements in said group
  in the end we return an array with size N, where N is the number of groups.
  Each segment in this array contains one group.

  TODO provide, two variants the original returned a tuple
"""
function extractSets2(sets)
  # output array<list<Entry>> setsArray "An array with all the sets.";
  # output Sets assignedSets "Sets with the roots assigned to sets.";
#  @info "Extracting sets.."
  local nodes::Vector{Int} = deepcopy(sets.internal.ranks)
  local set_idx::Int = 0
  @info "Internals start:" sets.internal
  dump(sets.internal)
  # Go through each node and assign a unique set index to each root node.
  # The index is stored as a negative number to mark the node as a root. - Per
  # In the underlaying set here in Julia it seems the root is stored as zero
  for i in 1:length(nodes) #sets.nodeCount
    if nodes[i] == 0 #= Changed from < 0 =#
      set_idx += 1
      nodes[i] = -set_idx #= Negate pos=#
    end
  end
#  @info "Value of set_idx" set_idx
#  @info "Value of nodes:" nodes
  # Create an array of lists to store the sets in, and fetch the list of
  # entry-index pairs stored in the hashtable.
  setsArray = []
  for i in 1:set_idx
    push!(setsArray, list())
  end
 # @info "typeof(setsArray):" typeof(setsArray)
  entries = sets.intmap#Dict()  #Using Julia dict here <Entry, Integer> entries
  #= Why does Per reverse? above? =#
  for (e, idx) in entries
    # Follow the parent indices until we find the root.
    set_idx = nodes[idx]
    while set_idx > 0
      println("Looping!")
      set_idx = nodes[set_idx];
      #@info "Value of setIdx:" set_idx
    end
    #= Now we have the parent =#
    set_idx = -set_idx #= Negate the set index to get the actual index. =#
    res = _cons(e, setsArray[set_idx])
#    @info listHead(res)
    setsArray[set_idx] = res
  end
  #@info "Done in extract sets! Value of nodes:" nodes
  res = filter((x) -> x < 0, nodes)
  @info "We had this many groups:" sets.internal.ngroups
  @info "Internals:" sets.internal
  @info "Result of res:" res
#  throw("Unimplemented exception")
  return setsArray
end

"""
Returns an array indexed by the group index
each pos contains a set array<list<Entry>>
Author:johti17
"""
function extractSets(sets)
  local parents::Vector{Int} = deepcopy(sets.internal.parents)
  # Go through each node and assign a unique set index to each root node.
  # The index is stored as a negative number to mark the node as a root. - Per
  local setsArray = []
  local numberOfGroups = sets.internal.ngroups #= Should be the number of element=#
  for i in 1:length(parents) #TMP!
    push!(setsArray, list())
  end
  local entries = sets.intmap
  #= Why does Per reverse? above? =#
  for (e, idx) in entries
    # Follow the parent indices until we find the root.
    parent_idx = parents[idx]
    while true
      tmp = parents[parent_idx];
      if tmp == parent_idx
        break
      end
    end
    #= Now we have the parent =#
    local set_idx = parent_idx
    setsArray[set_idx] = _cons(e, setsArray[set_idx])
  end
  #=Our list is slightly to big.. Reduce it!=#
  setsArray = filter(x -> !listEmpty(x), setsArray)
  return setsArray
end

"""
  Add a list of entries to the disjoint-sets forrest
"""
function addList(entries::List{T}, sets) where {T}
  #= Make a new set 
  see below in the original impl:
  Sets.DISJOINT_SETS(nodes, elements, node_count) := sets;=#
  for e in entries
    push!(sets, e)
  end
  return sets
end

""" 
  Replication of the add function in DisjointSets
"""
function add(entry::T1, sets::T2)::Tuple where {T1, T2}
  haskey(sets.intmap, entry) && return x
  id = push!(sets.internal) #=Get the integer index=#
  #= Update the internal structure =#
  sets.intmap[entry] = id 
  push!(sets.revmap, entry)
  return (sets, id)
end

@exportAll()
end #=NFConnectiosSets=#
