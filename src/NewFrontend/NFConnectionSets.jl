module ConnectionSets

using MetaModelica
using ExportAll

  #using DisjointSets #= Modelica extend clause =#
import DataStructures

#Why can't I find these two?
Sets = Array
#Entry = Main.Connector
#Connections = Main.Connections

""" #= Creates a new DisjointSets from a list of connection and flow variables. =#"""
function fromConnections(connections)
  ## REENABLE @debug "No implemented"
end

""" #= Adds a single connector to the connection sets. =#"""
function addScalarConnector(conn, sets)
  ## REENABLE @debug "No implemented"
end

function addConnector(conn, sets)
    ## REENABLE @debug "No implemented"
end

""" #= Adds a connector to the sets if it does not already exist =#"""
function addSingleConnector(conn, sets)
  ## REENABLE @debug "Not implemented"
end

""" #= Adds a connection to the sets, which means merging the two sets that the
     connectors belong to, unless they already belong to the same set. =#"""
function addConnection(
  connection,
  broken,
  sets
)
end

#= What is the definition here? =#
function isBroken(c1, c2, broken)::Bool
  ## REENABLE @debug "Not implemented"
end

@exportAll()
end
