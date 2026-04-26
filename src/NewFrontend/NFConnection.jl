using MetaModelica
using ExportAll
#= Forward declarations for uniontypes until Julia adds support for mutual recursion =#

@UniontypeDecl NFConnection
const Connection = NFConnection

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

function Base.string(conn::Connection)::String
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
  if listLength(connectionLstLeft) != listLength(connectionLstRight)
    local str = "\nLeft:\n"
    for lL in connectionLstLeft
      str *= toString(lL) * "\n"
    end
    str =  str * "\nRight:\n"
    for lR in connectionLstRight
      str *= toString(lR) * "\n"
    end
    @info str

    Error.assertion(false, getInstanceName() + " got unbalanced connection " + toString(connection) + " (lhs: " +
      String(listLength(connectionLstLeft)) + ", rhs: " + String(listLength(connectionLstRight)) + ")", sourceInfo());

    fail();
  end
  local conns = nil
  for cl in connectionLstLeft
    cr = listHead(connectionLstRight)
    connectionLstRight = listRest(connectionLstRight)
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
