module P_NFConnection

using MetaModelica
using ExportAll
#= Forward declarations for uniontypes until Julia adds support for mutual recursion =#

@UniontypeDecl NFConnection

import ..P_NFConnector
= P_NFConnector
Connector = NFConnector
import ..P_NFConnection
P_Connection = P_NFConnection
Connection = P_NFConnection.NFConnection

function toString(conn::Connection)::String
  local str::String

  @assign str =
    "connect(" +
    Connector.toString(conn.lhs) +
    ", " +
    Connector.toString(conn.rhs) +
    ")"
  return str
end

@Uniontype NFConnection begin
  @Record CONNECTION begin

    #=  TODO: This should be Connector, but the import above doesn't work due to some compiler bug.
    =#
    lhs::NFConnector
    rhs::NFConnector
  end
end

@exportAll()
end
