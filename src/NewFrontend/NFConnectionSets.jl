
module ConnectionSets

using MetaModelica
using ExportAll

  #using DisjointSets #= Modelica extend clause =#
import DataStructures

#Why can't I find these two?

#Entry = Main.Connector
#Connections = Main.Connections
""" #= Creates a new DisjointSets from a list of connection and flow variables. =#"""
function fromConnections(connections)
 #  local sets::ConnectionSets.Sets
 #  ## =  Approximate the size of the sets using the connections and flow variables.  =#
 #  # @assign sets = ConnectionSets.emptySets(
 #  #   listLength(connections.connections) + listLength(connections.flows),
 #  # )
 #  #=  Add flow variable to the sets, unless disabled by flag.
 #  =#
 #  #=  Do this here if NF_SCALARIZE to use fast addList for scalarized flows.
 #  =#
 # # if !Flags.isSet(Flags.DISABLE_SINGLE_FLOW_EQ) && Flags.isSet(Flags.NF_SCALARIZE)
 #  @assign sets = ListUtil.fold(connections.flows, addConnector, sets)
 # # end
 #  #=  Add the connections.
 #  =#
 #  @assign sets =
 #    ListUtil.fold1(connections.connections, addConnection, connections.broken, sets)
 #  #=  Add remaining flow variables to the sets, unless disabled by flag.
 #  =#
 #  #=  Do this after addConnection if not NF_SCALARIZE to get array dims right.
 #  =#
 #  if !Flags.isSet(Flags.DISABLE_SINGLE_FLOW_EQ) && !Flags.isSet(Flags.NF_SCALARIZE)
 #    @assign sets = ListUtil.fold(connections.flows, addSingleConnector, sets)
 #  end
  #  return sets
  @error "No implemented"
end

""" #= Adds a single connector to the connection sets. =#"""
function addScalarConnector(conn, sets)
  @error "No implemented"
end

function addConnector(conn, sets)
    @error "No implemented"
end

""" #= Adds a connector to the sets if it does not already exist =#"""
function addSingleConnector(conn, sets)
  @error "Not implemented"
end

""" #= Adds a connection to the sets, which means merging the two sets that the
     connectors belong to, unless they already belong to the same set. =#"""
function addConnection(
  connection,
  broken,
  sets
)

  # local lhs
  # local rhs
  # local c2
  # local lhsl::List{Connector}
  # local rhsl::List{Connector}

  # @match P_Connection.Connection.CONNECTION(lhs = lhs, rhs = rhs) = connection
  # @assign lhsl = Connector.split(lhs)
  # @assign rhsl = Connector.split(rhs)
  # for c1 in lhsl
  #   @match _cons(c2, rhsl) = rhsl
  #   if !(Connector.isDeleted(c1) || Connector.isDeleted(c2))
  #     if listEmpty(broken)
  #       @assign sets = merge(c1, c2, sets)
  #     elseif isBroken(c1, c2, broken)
  #     else
  #       @assign sets = merge(c1, c2, sets)
  #     end
  #   end
#  end
  #=  Connections involving deleted conditional connectors are filtered out
  =#
  #=  when collecting the connections, but if the connectors themselves
  =#
  #=  contain connectors that have been deleted we need to remove them here.
  =#
  #=  TODO: Check variability of connectors. It's an error if either
  =#
  #=        connector is constant/parameter while the other isn't.
  =#
  #=  do nothing
  =#
  #=  print(\"Ignore broken: connect(\" + Connector.toString(c1) + \", \" + Connector.toString(c2) + \")\\n\");
  =#
  #return sets
end

#= What is the definition here? =#
function isBroken(c1, c2, broken)::Bool
  # local b::Bool = false
  # local lhs::ComponentRef
  # local rhs::ComponentRef
  # local cr1::ComponentRef
  # local cr2::ComponentRef

  # @assign cr1 = Connector.name(c1)
  # @assign cr2 = Connector.name(c2)
  # #=  print(\"Check: connect(\" + ComponentRef.toString(cr1) + \", \" + ComponentRef.toString(cr2) + \")\\n\");
  # =#
  # for c in broken
  #   @assign (lhs, rhs, _) = c
  #   if isPrefix(lhs, cr1) &&
  #      isPrefix(rhs, cr2) ||
  #      isPrefix(lhs, cr2) &&
  #      isPrefix(rhs, cr1)
  #     @assign b = true
  #     break
  #   end
  # end
  # return b
  @error "Not implemented"
end

@exportAll()
end
