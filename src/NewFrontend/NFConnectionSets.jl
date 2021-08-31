module ConnectionSets

using MetaModelica
using ExportAll

import DataStructures
import ListUtil

import ..NFConnector
import ..NFConnections
import ..Main.CONNECTION
import ..Main.split

const Connector = NFConnector
const Connections = NFConnections

#= Using the Disjoint set data structure =#
const Sets = DataStructures.DisjointSets{Connector}

""" #= Creates a new DisjointSets from a list of connection and flow variables. =#"""
function fromConnections(connections)
  @info "fromConnections called."
#  @info "fromConnections: Our connections" connections
  local sets::Sets = Sets() # duh, is not possible to give sz size hint to DisjointSets! -Adrian Pop
  local sz = listLength(connections.connections) + listLength(connections.flows)

  # Add flow variable to the sets, unless disabled by flag.
  # Do this here if NF_SCALARIZE to use fast addList for scalarized flows.

  # if ! Flags.isSet(Flags.DISABLE_SINGLE_FLOW_EQ) && Flags.isSet(Flags.NF_SCALARIZE) then
  sets = ListUtil.fold(connections.flows, addConnector, sets)
  # end

  # Add the connections.
  sets = ListUtil.fold1(connections.connections, addConnection, connections.broken, sets)

  # Add remaining flow variables to the sets, unless disabled by flag.
  # Do this after addConnection if not NF_SCALARIZE to get array dims right.
  # if ! Flags.isSet(Flags.DISABLE_SINGLE_FLOW_EQ) && ! Flags.isSet(Flags.NF_SCALARIZE) then
  # sets = ListUtil.fold(connections.flows, addSingleConnector, sets)
  # end
  @info "Returning:" sets
  return sets
end

""" #= Adds a single connector to the connection sets. =#"""
function addScalarConnector(conn, sets)
  push!(sets, conn)
  return sets
end

function addConnector(conn, sets)
  sets = addList(Connector.split(conn), sets)
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

""" #= Adds a connection to the sets, which means merging the two sets that the
     connectors belong to, unless they already belong to the same set. =#"""
function addConnection(connection, broken, sets)
  @match CONNECTION(lhs, rhs) = connection
  println(typeof(lhs))
  local lhsl = split(lhs)
  local rhsl = split(lhs)
  for c1 in lhsl
    c2 = listHead(rhsl)
    if ! isDeleted(c1) || isDeleted(c2)
      if listEmpty(broken)
        sets = merge(c1, c2, sets)
      elseif isBroken(c1, c2, broken)
        #Do nothing
      else
        sets = merge(c1, c2, sets)
      end
    end
  end
  return sets
end

#= What is the definition here? =#
function isBroken(c1, c2, broken)::Bool
  cr1 = Connector.name(c1);
  cr2 = Connector.name(c2);
  # print("Check: connect(" + ComponentRef.toString(cr1) + ", " + ComponentRef.toString(cr2) + ")\n");

  for c in broken
    @match ((lhs, rhs, _)) = c
    if isPrefix(lhs, cr1) && isPrefix(rhs, cr2) || isPrefix(lhs, cr2) && isPrefix(rhs, cr1)
      # print("Ignore broken: connect(" + Connector.toString(c1) + ", " + Connector.toString(c2) + ")\n")
      b = true
      break
    end
  end
end

# Merges the two sets that the given entry belong to
function merge(conn1, conn2, sets)
  (set1, sets) = findSet(conn1, sets);
  (set2, sets) = findSet(conn2, sets);
  union!(set1, set2, sets)
end


#=TODO provide, two variants=#
function extractSets(sets)
  # output array<list<Entry>> setsArray "An array with all the sets.";
  # output Sets assignedSets "Sets with the roots assigned to sets.";
  @info sets
  local nodes::Vector{Int} = collect(sets) #= As above this should collect all nodes =#
  local set_idx::Int = 0
    # Go through each node and assign a unique set index to each root node.
    # The index is stored as a negative number to mark the node as a root.
  for i in 1:length(nodes) #sets.nodeCount
    if nodes[i] < 0 then
      set_idx = set_idx + 1
      nodes[i] = -set_idx
    end
  end
  # Create an array of lists to store the sets in, and fetch the list of
  # entry-index pairs stored in the hashtable.
  setsArray = [nil for i in 1:set_idx] #Vector{List}(nil, set_idx) #TODO Untyped for now -John set_idx is the size of the sets
  entries = sets.intmap#Dict()  #Using Julia dict here <Entry, Integer> entries
  local set_idx;
  for (e, idx) in entries
    # Follow the parent indices until we find the root.
    set_idx = nodes[idx]
    while set_idx > 0
      set_idx = nodes[set_idx];
    end
    #= Negate the set index to get the actual index. =#
    set_idx = -set_idx;
    setsArray[set_idx] = e <| setsArray[set_idx];
  end
#  assignedSets := Sets(nodes, sets.elements, sets.nodeCount);
#  osets = Sets()
  #  result = (listArray(nil,  osets))
  @info "Done in extract sets!"
  @info "This is our setsArray:" setsArray
#  throw("Unimplemented exception")
  return setsArray
end

@exportAll()
end
