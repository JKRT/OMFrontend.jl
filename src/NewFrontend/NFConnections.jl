
@UniontypeDecl NFConnections

BrokenEdges = List
BrokenEdge = Tuple

@Uniontype NFConnections begin
  @Record CONNECTIONS begin
    connections::List{Connection}
    flows::List{Connector}
    broken::BrokenEdges
  end
end

Connections = NFConnections

function makeConnectors(
  cref::ComponentRef,
  ty::M_Type,
  source::DAE.ElementSource,
)::List{Connector}
  local connectors::List{Connector}

  local cref_exp::Expression
  local cr::ComponentRef
  local expanded::Bool

  @assign cr = evaluateSubscripts(cref)
  if !Flags.isSet(Flags.NF_SCALARIZE)
    @assign connectors = list(Connector.fromCref(
      cr,
      getSubscriptedType(cr),
      source,
    ))
    return connectors
  end
  @assign cref_exp =
    CREF_EXPRESSION(getSubscriptedType(cr), cr)
  @assign (cref_exp, expanded) = P_ExpandExp.ExpandExp.expand(cref_exp)
  if expanded
    @assign connectors = Connector.fromExp(cref_exp, source)
  else
    Error.assertion(
      false,
      getInstanceName() +
      " failed to expand connector `" +
      toString(cref) +
      "\\n",
      ElementSource_getInfo(source),
    )
  end
  #=  Connectors should only have structural parameter subscripts, so it
  =#
  #=  should always be possible to expand them.
  =#
  return connectors
end

function collect(flatModel::FlatModel)::Tuple{FlatModel, Connections}
  local conns::Connections = new()
  local comp::Component
  local cr::ComponentRef
  local lhs::ComponentRef
  local rhs::ComponentRef
  local c1::Connector
  local c2::Connector
  local source::DAE.ElementSource
  local eql::List{Equation} = nil
  local cl1::List{Connector}
  local cl2::List{Connector}
  local e1::Expression
  local e2::Expression
  local ty1::M_Type
  local ty2::M_Type
  local b1::Bool
  local b2::Bool
  #=  Collect all flow variables.
  =#
  for var in flatModel.variables
    @assign comp = component(node(var.name))
    if isFlow(comp)
      @assign c1 = Connector.fromFacedCref(
        var.name,
        var.ty,
        Face.INSIDE,
        ElementSource_createElementSource(P_Component.info(comp)),
      )
      @assign conns = addFlow(c1, conns)
    end
  end
  #=  Collect all connects.
  =#
  for eq in flatModel.equations
    @assign eql = begin
      @match eq begin
        EQUATION_CONNECT(
          lhs = CREF_EXPRESSION(ty = ty1, cref = lhs),
          rhs = CREF_EXPRESSION(ty = ty2, cref = rhs),
          source = source,
        ) => begin
          if !(
            isDeleted(lhs) ||
            isDeleted(rhs)
          )
            @assign cl1 = makeConnectors(lhs, ty1, source)
            @assign cl2 = makeConnectors(rhs, ty2, source)
            for c1 in cl1
              @match _cons(c2, cl2) = cl2
              @assign conns =
                addConnection(P_Connection.Connection.CONNECTION(c1, c2), conns)
            end
          end
          eql
        end

        _ => begin
          _cons(eq, eql)
        end
      end
    end
  end
  if !listEmpty(conns.connections)
    @assign flatModel.equations = listReverseInPlace(eql)
  end
  return (flatModel, conns)
end

function addBroken(broken::BrokenEdges, conns::Connections)::Connections
  @assign conns.broken = broken
  return conns
end

function addFlow(conn::Connector, conns::Connections)::Connections
  @assign conns.flows = _cons(conn, conns.flows)
  return conns
end

function addConnection(conn::Connection, conns::Connections)::Connections
  @assign conns.connections = _cons(conn, conns.connections)
  return conns
end

function fromConnectionList(connl::List{<:Connection})::Connections
  local conns::Connections
  @assign conns = CONNECTIONS(connl, nil, nil)
  return conns
end

function new()::Connections
  local conns::Connections = CONNECTIONS(nil, nil, nil)
  return conns
end
