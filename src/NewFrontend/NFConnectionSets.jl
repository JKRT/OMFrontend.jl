module ConnectionSets

using MetaModelica
using ExportAll

import DataStructures
import ListUtil

import ..NFConnector
import ..NFConnections

Connector = NFConnector
Connections = NFConnections

Sets = DataStructures.DisjointSets{Connector}

""" #= Creates a new DisjointSets from a list of connection and flow variables. =#"""
function fromConnections(connections)
  local sets::Sets = Sets() # duh, is not possible to give sz size hint to DisjointSets!
  sz = listLength(connections.connections) + listLength(connections.flows)

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
  for c in Connector.split(conn) 
    sets = find(c, sets)
  end
  return sets
end

""" #= Adds a connection to the sets, which means merging the two sets that the
     connectors belong to, unless they already belong to the same set. =#"""
function addConnection(connection, broken, sets)
  local conns = Connection.split(connection)

  if ! listEmpty(broken)
    for c in conns
      if ! isBroken(c.lhs, c.rhs, broken)
        conns = cons(c, conns)
      end
    end 
    conns = listReverse(conns)
  end

  # TODO: Check variability of connectors. It's an error if either
  #       connector is constant/parameter while the other isn't.
  for conn in conns loop
    sets = merge(conn.lhs, conn.rhs, sets);
  end
end

#= What is the definition here? =#
function isBroken(c1, c2, broken)::Bool
  cr1 = Connector.name(c1);
  cr2 = Connector.name(c2);
  # print("Check: connect(" + ComponentRef.toString(cr1) + ", " + ComponentRef.toString(cr2) + ")\n");

  for c in broken loop
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
#==
function find
  "This function finds and returns the node associated with a given entry.
   If the entry does not a have a node in the forest, then a new node will be
   added and returned.

   The reason why this function also returns the sets is because it does path
   compression, and the disjoint-set structure may therefore be changed during
   look up."
  input Entry entry;
  input output Sets sets;
        output Integer index;
algorithm
  try
    // Check if a node already exists in the forest.
    index := BaseHashTable.get(entry, sets.elements);
  else
    // If a node doesn't already exist, create a new one.
    (sets, index) := add(entry, sets);
  end try;
end find;

function add
  "Adds an entry to the disjoint-sets forest. This function assumes that the
   entry does not already exist in the forest. If the entry might exist already,
   use find instead."
  input Entry entry;
  input output Sets sets;
        output Integer index;
protected
  array<Integer> nodes;
  IndexTable elements;
  Integer node_count;
algorithm
  Sets.DISJOINT_SETS(nodes, elements, node_count) := sets;
  index := node_count + 1;

  // Make sure that we have enough space in the node array. New nodes have the
  // value -1, so we don't actually need to add a node to the array, just expand
  // it and fill the new places with -1.
  if index > arrayLength(nodes) then
    nodes := Array.expand(realInt(intReal(index) * 1.4), nodes, -1);
  end if;

  // Register the node index in the index table.
  elements := BaseHashTable.addNoUpdCheck((entry, index), elements);
  sets := Sets.DISJOINT_SETS(nodes, elements, index);
end add;

function addList
  "Adds a list of entries to the disjoint-sets forest, in a more efficient
   manner than calling add repeatedly. This function assumes that the entries
   does not already exist in the forest. If the entries might exist already, use
   find instead."
  input list<Entry> entries;
  input output Sets sets;
protected
  array<Integer> nodes;
  IndexTable elements;
  Integer node_count, sz, index;
algorithm
  Sets.DISJOINT_SETS(nodes, elements, node_count) := sets;
  sz := listLength(entries);
  index := node_count + 1;
  node_count := node_count + sz;

  if node_count > arrayLength(nodes) then
    nodes := Array.expand(realInt(intReal(node_count) * 1.4), nodes, -1);
  end if;

  for e in entries loop
    elements := BaseHashTable.addNoUpdCheck((e, index), elements);
    index := index + 1;
  end for;

  sets := Sets.DISJOINT_SETS(nodes, elements, node_count);
end addList;

function findSet
  "This function finds and returns the set that the given entry belongs to.
   The set is represented by the root node of the tree. If the entry does not
   have a corresponding node in the forest, then a new set with the entry as the
   only element will be added to the forest and returned.

   The reason why this function also returns the sets is because it does path
   compression, and the disjoint-set structure may therefore be changed during
   look up."
  input Entry entry;
  input Sets sets;
  output Integer set;
  output Sets updatedSets;
protected
  Integer index;
algorithm
  // Look up the index of the entry.
  (updatedSets, index) := find(entry, sets);
  // Return the index of the root of the tree that the entry belongs to.
  set := findRoot(index, updatedSets.nodes);
end findSet;

function extractSets
  "Extracts all the sets from the disjoint sets structure, and returns
   them as an array. The function also returns a new DisjointSets structure where
   all roots have been assigned a set index, which can be used for looking up
   sets in the array with findSetArrayIndex."
  input Sets sets;
  output array<list<Entry>> setsArray "An array with all the sets.";
  output Sets assignedSets "Sets with the roots assigned to sets.";
protected
  array<Integer> nodes;
  Integer set_idx = 0, idx;
  list<tuple<Entry, Integer>> entries;
  Entry e;
algorithm
  nodes := sets.nodes;

  // Go through each node and assign a unique set index to each root node.
  // The index is stored as a negative number to mark the node as a root.
  for i in 1:sets.nodeCount loop
    if nodes[i] < 0 then
      set_idx := set_idx + 1;
      nodes[i] := -set_idx;
    end if;
  end for;

  // Create an array of lists to store the sets in, and fetch the list of
  // entry-index pairs stored in the hashtable.
  setsArray := arrayCreate(set_idx, {});
  entries := BaseHashTable.hashTableListReversed(sets.elements);

  // Go through each entry-index pair.
  for p in entries loop
    (e, idx) := p;
    // Follow the parent indices until we find the root.
    set_idx := nodes[idx];

    while set_idx > 0 loop
      set_idx := nodes[set_idx];
    end while;

    // Negate the set index to get the actual index.
    set_idx := -set_idx;
    // Add the entry to the list pointed to by the set index.
    setsArray[set_idx] := e :: setsArray[set_idx];
  end for;

  assignedSets := Sets.DISJOINT_SETS(nodes, sets.elements, sets.nodeCount);
end extractSets;

==#

function extractSets(sets)
    # output array<list<Entry>> setsArray "An array with all the sets.";
    # output Sets assignedSets "Sets with the roots assigned to sets.";
  osets = Sets()
  @assign result = (listArray(nil()), osets)
  return result
end

@exportAll()
end
