using MetaModelica
using ExportAll
#= Forward declarations for uniontypes until Julia adds support for mutual recursion =#

@UniontypeDecl NFConnection
Connection = NFConnection

function toString(conn::Connection)::String
  local str::String
  @assign str =
    "connect(" +
    #=Connector.=#toString(conn.lhs) +
    ", " +
    #=Connector.=#toString(conn.rhs) +
    ")"
  return str
end

@Uniontype NFConnection begin
  @Record CONNECTION begin
    #=  TODO: This should be Connector, but the import above doesn't work due to some compiler bug.=#
    lhs::NFConnector
    rhs::NFConnector
  end
end

"""
  Split function for a connection.
   Given a connection split and return a List{Connection}
"""
function split(connection::CONNECTION)::List
  local connectionLstRight = split(connection.rhs)
  local connectionLstLeft = split(connection.lhs)
  if listLength(connectionLstLeft) != listLength(connectionLstRight) then
    #Error.assertion(false, getInstanceName() + " got unbalanced connection " + toString(conn) + " (lhs: " +
    #                    String(listLength(cls)) + ", rhs: " + String(listLength(crs)) + ")", sourceInfo());
    @error "Got unbalanced equation between $(connectionLstRight) and $(connectionLstLeft)"
    fail();
  end
  local conns = nil
  for cl in connectionLstLeft
    cr = listHead(connectionLstRight)
    # Connections involving deleted conditional connectors are filtered out
    # when collecting the connections, but if the connectors themselves
    # contain connectors that have been deleted we need to remove them here.
    if ! (isDeleted(cl) || isDeleted(cr))
      conns = CONNECTION(cl, cr) <| conns;
    end
  end            
  conns = listReverseInPlace(conns);
  return conns
end
