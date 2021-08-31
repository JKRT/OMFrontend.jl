function elaborate(
  flatModel::FlatModel,
  connections::Connections,
)::Tuple{FlatModel, Connections}

  local expandable_conns::List{Connection}
  local undeclared_conns::List{Connection}
  local conns::List{Connection}
  local vars::List{Variable}
  local csets::ConnectionSets.Sets
  local csets_array::Array{List{Connector}}

  #=  Sort the connections based on whether they involve expandable connectors,
  =#
  #=  virtual/potentially present connectors, or only normal connectors.
  =#
  @assign (expandable_conns, undeclared_conns, conns) =
    sortConnections(connections.connections)
  #=  Don't do anything if there aren't any expandable connectors in the model.
  =#
  if listEmpty(expandable_conns) && listEmpty(undeclared_conns)
    return (flatModel, connections)
  end
  #=  Create a graph from the connections. Expandable connectors connect to
  =#
  #=  expandable connectors, while virtual/potentially present connectors connect
  =#
  #=  to the expandable connector they belong to.
  =#
  @assign csets =
    ConnectionSets.emptySets(listLength(expandable_conns) + listLength(undeclared_conns))
  @assign csets = addExpandableConnectorsToSets(expandable_conns, csets)
  @assign (undeclared_conns, csets) =
    ListUtil.mapFold(undeclared_conns, addUndeclaredConnectorToSets, csets)
  #=  Extract the sets of connected connectors.
  =#
  @assign csets_array = ConnectionSets.extractSets(csets)
  #= for set in csets_array loop
  =#
  #=   print(\"Expandable connection set:\\n\");
  =#
  #=   print(List.toString(set, Connector.toString, \"\", \"{\", \", \", \"}\", true) + \"\\n\");
  =#
  #= end for;
  =#
  #=  Augment the expandable connectors with the necessary elements, mark
  =#
  #=  connected potentially present variables as present, and add the
  =#
  #=  created variables to the flat model.
  =#
  @assign vars = flatModel.variables
  for set in csets_array
    @assign vars = elaborateExpandableSet(set, vars)
  end
  #=  Update the connections and put them back in the list of connections.
  =#
  @assign conns = ListUtil.fold(undeclared_conns, updateUndeclaredConnection, conns)
  @assign conns = ListUtil.fold(expandable_conns, updateExpandableConnection, conns)
  @assign connections.connections = conns
  #=  Update the attributes of potentially present variables so that they have
  =#
  #=  the same attributes as their node. Their connector type will have changed
  =#
  #=  if they've been marked as present.
  =#
  @assign vars = list(updatePotentiallyPresentVariable(v) for v in vars)
  @assign flatModel.variables = vars
  return (flatModel, connections)
end

module ExpandableSet

using MetaModelica
using ExportAll

import ..Main.NFConnector

Value = Int
Key = NFConnector
const Connector = NFConnector

#= Modelica extend clause =#
include("../Util/baseAvlTreeCode.jl")

function emptySet(size::Int)::HashSet
  local set::HashSet
  @assign set = BaseHashSet.emptyHashSetWork(
    size,
    (hashConnector, Connector.isNodeNameEqual, Connector.toString),
  )
  return set
end

function hashConnector(conn::Connector, mod::Int)::Int
  local res::Int

  @assign res = stringHashDjb2Mod(firstName(conn.name), mod)
  return res
end

@exportAll()
end

""" #= Sorts the connections into different categories of connectors based on
   whether they involve expandable connectors, virtual/potentially present
   connector, or only normal connectors. =#"""
function sortConnections(
  conns::List{<:Connection},
)::Tuple{List{Connection}, List{Connection}, List{Connection}}
  local normalConnections::List{Connection} = nil
  local undeclaredConnections::List{Connection} = nil
  local expandableConnections::List{Connection} = nil

  local c1::Connector
  local c2::Connector
  local err_msg::Option{Tuple{ErrorTypes.Message, List{Connector}}}
  local is_undeclared1::Bool
  local is_undeclared2::Bool
  local is_expandable1::Bool
  local is_expandable2::Bool

  for conn in conns
    @match P_Connection.Connection.CONNECTION(lhs = c1, rhs = c2) = conn
    @assign is_undeclared1 = ConnectorType.isUndeclared(c1.cty)
    @assign is_undeclared2 = ConnectorType.isUndeclared(c2.cty)
    @assign is_expandable1 = ConnectorType.isExpandable(c1.cty)
    @assign is_expandable2 = ConnectorType.isExpandable(c2.cty)
    if is_expandable1 || is_expandable2
      if is_expandable1 && is_expandable2
        @assign expandableConnections = _cons(conn, expandableConnections)
      else
        Error.addSourceMessageAndFail(
          Error.EXPANDABLE_NON_EXPANDABLE_CONNECTION,
          list(
            Connector.toString(if is_expandable1
              c1
            else
              c2
            end),
            Connector.toString(if is_expandable1
              c2
            else
              c1
            end),
          ),
          Connector_getInfo(c1),
        )
      end
    elseif is_undeclared1 || is_undeclared2
      if is_undeclared1 && is_undeclared2
        Error.addSourceMessageAndFail(
          Error.UNDECLARED_CONNECTION,
          list(Connector.toString(c1), Connector.toString(c2)),
          Connector_getInfo(c1),
        )
      else
        @assign undeclaredConnections = _cons(conn, undeclaredConnections)
      end
    else
      @assign normalConnections = _cons(conn, normalConnections)
    end
  end
  #=  An expandable connector may only connect to another expandable connector.
  =#
  #=  Both sides can't be undeclared, one must be a declared component.
  =#
  @assign normalConnections = listReverseInPlace(normalConnections)
  return (expandableConnections, undeclaredConnections, normalConnections)
end

function addExpandableConnectorsToSets(
  conns::List{<:Connection},
  csets::ConnectionSets.Sets,
)::ConnectionSets.Sets

  local c1::Connector
  local c2::Connector

  for conn in conns
    @match P_Connection.Connection.CONNECTION(lhs = c1, rhs = c2) = conn
    @assign csets = addConnectionToSets(c1, c2, csets)
    @assign csets = addNestedExpandableConnectorsToSets(c1, c2, csets)
  end
  return csets
end

function addNestedExpandableConnectorsToSets(
  c1::Connector,
  c2::Connector,
  csets::ConnectionSets.Sets,
)::ConnectionSets.Sets

  local ecl1::List{Connector}
  local ecl2::List{Connector}
  local oec::Option{Connector}

  @assign ecl1 = getExpandableConnectorsInConnector(c1)
  @assign ecl2 = getExpandableConnectorsInConnector(c2)
  if listEmpty(ecl1) && listEmpty(ecl2)
    return csets
  end
  for ec1 in ecl1
    @assign (ecl2, oec) =
      ListUtil.deleteMemberOnTrue(ec1, ecl2, Connector.isNodeNameEqual)
    if isSome(oec)
      @assign csets = addConnectionToSets(ec1, Util.getOption(oec), csets)
    end
  end
  return csets
end

function getExpandableConnectorsInConnector(c1::Connector)::List{Connector}
  local ecl::List{Connector}

  local nodes::List{InstNode}
  local par_name::ComponentRef
  local name::ComponentRef
  local ty::M_Type

  @assign ecl = begin
    @match c1 begin
      Connector.CONNECTOR(
        name = par_name,
        ty = TYPE_COMPLEX(
          complexTy = ComplexType.EXPANDABLE_CONNECTOR(expandableConnectors = nodes),
        ),
      ) => begin
        @assign ecl = nil
        for n in nodes
          @assign ty = getType(n)
          @assign name = prefixCref(n, ty, nil, par_name)
          @assign ecl = _cons(
            Connector.fromCref(
              name,
              ty,
              ElementSource_createElementSource(info(n)),
            ),
            ecl,
          )
        end
        ecl
      end

      _ => begin
        nil
      end
    end
  end
  return ecl
end

function addUndeclaredConnectorToSets(
  conn::Connection,
  csets::ConnectionSets.Sets,
)::Tuple{Connection, ConnectionSets.Sets}

  local c1::Connector
  local c2::Connector
  local c::Connector
  local ec::Connector

  @match P_Connection.Connection.CONNECTION(lhs = c1, rhs = c2) = conn
  #=  Figure out which connector to add, and create a virtual connector if necessary.
  =#
  if ConnectorType.isUndeclared(c1.cty)
    if ConnectorType.isVirtual(c1.cty)
      @assign c1 = makeVirtualConnector(c1, c2)
      @assign conn = P_Connection.Connection.CONNECTION(c1, c2)
    end
    @assign c = c1
  else
    if ConnectorType.isVirtual(c2.cty)
      @assign c2 = makeVirtualConnector(c2, c1)
      @assign conn = P_Connection.Connection.CONNECTION(c1, c2)
    end
    @assign c = c2
  end
  #=  Create a parent connector for the undeclared connector, i.e. the expandable
  =#
  #=  connector it should be added to. The type here is wrong, but it doesn't matter.
  =#
  @assign ec = Connector.CONNECTOR(
    rest(c.name),
    c.ty,
    c.face,
    ConnectorType.EXPANDABLE,
    c.source,
  )
  #=  Add a connection between the undeclared connector and the expandable connector.
  =#
  @assign csets = addConnectionToSets(c, ec, csets)
  return (conn, csets)
end

function addConnectionToSets(
  c1::Connector,
  c2::Connector,
  csets::ConnectionSets.Sets,
)::ConnectionSets.Sets

  #=  The connection sets are not used to represent actual connections here, only
  =#
  #=  to keep track of which expandable connectors that are associated. So to
  =#
  #=  make sure we only get one instance of each expandable connector in the sets
  =#
  #=  we make sure the face of all the connectors we add is the same.
  =#
  @assign csets = ConnectionSets.merge(
    Connector.setOutside(c1),
    Connector.setOutside(c2),
    csets,
  )
  return csets
end

function makeVirtualConnector(
  virtualConnector::Connector,
  normalConnector::Connector,
)::Connector
  local newConnector::Connector

  local virtual_cref::ComponentRef
  local normal_cref::ComponentRef
  local ty::M_Type
  local node::InstNode

  @assign virtual_cref = virtualConnector.name
  @assign normal_cref = normalConnector.name
  @assign ty = normalConnector.ty
  #=  TODO: Update the virtual connector with the created node.
  =#
  @assign node = node(normal_cref)
  @assign node = clone(node)
  @assign node =
    rename(firstName(virtual_cref), node)
  @assign node = setParent(
    node(rest(virtual_cref)),
    node,
  )
  @assign virtual_cref = prefixCref(
    node,
    ty,
    nil,
    rest(virtual_cref),
  )
  #=  TODO: This needs more work, the new connector might be a complex connector.
  =#
  @assign newConnector = Connector.CONNECTOR(
    virtual_cref,
    ty,
    virtualConnector.face,
    virtualConnector.cty,
    virtualConnector.source,
  )
  return newConnector
end

function elaborateExpandableSet(
  set::List{<:Connector},
  vars::List{<:Variable},
)::List{Variable}

  local exp_set::ExpandableSet.HashSet
  local exp_conns::List{Connector} = nil
  local exp_set_lst::List{Connector}

  @assign exp_set = ExpandableSet.emptySet(Util.nextPrime(listLength(set)))
  for c in set
    if ConnectorType.isExpandable(c.cty)
      @assign exp_conns = _cons(c, exp_conns)
    elseif ConnectorType.isUndeclared(c.cty)
      @assign exp_set = BaseHashSet.add(c, exp_set)
      markComponentPresent(node(Connector.name(c)))
    end
  end
  @assign exp_set_lst = BaseHashSet.hashSetList(exp_set)
  for ec in exp_conns
    @assign vars = augmentExpandableConnector(ec, exp_set_lst, vars)
  end
  return vars
end

function markComponentPresent(node::InstNode)
  local comp::Component
  local cty::ConnectorType.TYPE

  @assign comp = component(node)
  @assign cty = P_Component.connectorType(comp)
  return if ConnectorType.isPotentiallyPresent(cty)
    @assign cty = ConnectorType.setPresent(cty)
    @assign comp = P_Component.setConnectorType(cty, comp)
    updateComponent!(comp, node)
  end
end

function augmentExpandableConnector(
  conn::Connector,
  expandableSet::List{<:Connector},
  vars::List{<:Variable},
)::List{Variable}

  local exp_name::ComponentRef
  local elem_name::ComponentRef
  local exp_node::InstNode
  local comp_node::InstNode
  local cls_node::InstNode
  local node::InstNode
  local cls::Class
  local cls_tree::ClassTree
  local comp::Component
  local nodes::List{InstNode} = nil
  local var::Variable
  local ty::M_Type
  local complex_ty::ComplexType

  @assign exp_name = Connector.name(conn)
  @assign exp_node = node(exp_name)
  if isName(exp_node)
    Error.addInternalError(
      "Augmenting a virtual element in an expandable connector is not yet supported.",
      Connector_getInfo(conn),
    )
    fail()
  end
  @assign cls_node = classScope(exp_node)
  @assign cls = getClass(cls_node)
  @assign cls_tree = classTree(cls)
  #=  Go through the union of elements the expandable connector should have.
  =#
  for c in expandableSet
    @assign elem_name = Connector.name(c)
    @assign node = node(elem_name)
    try
      @assign comp_node = lookupElement(name(node), cls_tree)
    catch
      @assign comp_node = EMPTY_NODE()
    end
    if isEmpty(comp_node)
      @assign nodes = _cons(node, nodes)
      @assign ty = c.ty
      @assign elem_name = prefixCref(node, ty, nil, exp_name)
      @assign var = VARIABLE(
        elem_name,
        ty,
        EMPTY_BINDING,
        Visibility.PUBLIC,
        NFComponent.DEFAULT_ATTR,
        nil,
        SOME(SCode.COMMENT(NONE(), SOME("virtual variable in expandable connector"))),
        ElementSource_getInfo(c.source),
      )
      @assign vars = _cons(var, vars)
    else
      @assign comp_node = lookupElement(name(node), cls_tree)
      @assign comp_node = resolveInner(comp_node)
      if isComponent(comp_node)
        markComponentPresent(comp_node)
      else
        Error.addInternalError(
          getInstanceName() + " got non-component element",
          sourceInfo(),
        )
      end
    end
  end
  #=  If the element doesn't already exist, add it to the list of elements to be
  =#
  #=  added to the connector.
  =#
  #=  TODO: This needs more work, the new connector might be a complex connector.
  =#
  #=  If the element already exists and is a potentially present component,
  =#
  #=  change it to be present.
  =#
  if !listEmpty(nodes)
    @assign cls_tree = addElementsToFlatTree(nodes, cls_tree)
    @assign cls = setClassTree(cls_tree, cls)
  end
  #=  Create a normal non-expandable complex type for the augmented expandable connector.
  =#
  @assign complex_ty = makeConnectorType(cls_tree, isExpandable = false)
  @assign ty = TYPE_COMPLEX(cls_node, complex_ty)
  @assign cls = setType(ty, cls)
  updateClass(cls, cls_node)
  componentApply(exp_node, P_Component.setType, ty)
  return vars
end

function updateUndeclaredConnection(
  conn::Connection,
  conns::List{<:Connection},
)::List{Connection}

  @assign conns = _cons(conn, conns)
  return conns
end

function updateExpandableConnection(
  conn::Connection,
  conns::List{<:Connection},
)::List{Connection}

  local c1::Connector
  local c2::Connector
  local ty1::M_Type
  local ty2::M_Type
  local mk::MatchKind
  local e1::Expression
  local e2::Expression

  @match P_Connection.Connection.CONNECTION(lhs = c1, rhs = c2) = conn
  @assign (c1, ty1) = updateExpandableConnector(c1)
  @assign (c2, ty2) = updateExpandableConnector(c2)
  #=  Check that the types match now that the connectors have been augmented.
  =#
  @assign e1 = CREF_EXPRESSION(ty1, Connector.name(c1))
  @assign e2 = CREF_EXPRESSION(ty2, Connector.name(c2))
  @assign (_, _, _, mk) = TypeCheck.matchExpressions(e1, ty1, e2, ty2, allowUnknown = true)
  if TypeCheck.isIncompatibleMatch(mk)
    Error.addSourceMessageAndFail(
      Error.INVALID_CONNECTOR_VARIABLE,
      list(toString(e1), toString(e2)),
      Connector_getInfo(c1),
    )
  end
  @assign conns = _cons(P_Connection.Connection.CONNECTION(c1, c2), conns)
  return conns
end

function updateExpandableConnector(conn::Connector)::Tuple{Connector, M_Type}
  local ty::M_Type

  local name::ComponentRef

  @match Connector.CONNECTOR(name = name, ty = ty) = conn
  @assign name = updateNodeType(name)
  @assign ty = setArrayElementType(
    ty,
    arrayElementType(nodeType(name)),
  )
  @assign conn = Connector.CONNECTOR(name, ty, conn.face, conn.cty, conn.source)
  return (conn, ty)
end

function updatePotentiallyPresentVariable(var::Variable)::Variable
  if ConnectorType.isPotentiallyPresent(var.attributes.connectorType)
    @assign var.attributes =
      getAttributes(component(node(var.name)))
  end
  return var
end
