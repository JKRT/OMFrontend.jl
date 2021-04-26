Face = (() -> begin #= Enumeration =#
  INSIDE = 1
  OUTSIDE = 2
  () -> (INSIDE; OUTSIDE)
end)()
const FaceType = Integer

@Uniontype NFConnector begin
  @Record CONNECTOR begin
    name::ComponentRef
    ty::NFType
    face::FaceType
    cty::ConnectorType.TYPE
    source::DAE.ElementSource
  end
end
Connector = NFConnector

ScalarizeSetting = (() -> begin #= Enumeration =#
                    NONE = 1  #= a[2].b[2] => {a[2].b[2]} =#
                    PREFIX = 2  #= a[2].b[2] => {a[1].b[2], a[2].b[2]} =#
                    ALL = 3  #= a[2].b[2] => {a[1].b[1], a[1].b[2], a[2].b[1], a[2].b[2]} =#
                    () -> (NONE; PREFIX; ALL)  #= a[2].b[2] => {a[1].b[1], a[1].b[2], a[2].b[1], a[2].b[2]} =#
                    end)()

const ScalarizeSettingType = Integer

""" #= Splits a connector into its primitive components. =#"""
function split(
  conn::Connector,
  scalarize::ScalarizeSettingType = if Flags.isSet(Flags.NF_SCALARIZE)
    ScalarizeSetting.ALL
  else
    ScalarizeSetting.NONE
  end,
)::List{Connector}
  local connl::List{Connector}

  @assign connl = splitImpl(conn.name, conn.ty, conn.face, conn.source, conn.cty, scalarize)
  return connl
end

function hash(conn::Connector, mod::Integer)::Integer
  local hash::Integer = hash(conn.name, mod)
  return hash
end

function toString(conn::Connector)::String
  local str::String = toString(conn.name)
  return str
end

function name(conn::Connector)::ComponentRef
  local name::ComponentRef = conn.name
  return name
end

function isExpandable(conn::Connector)::Bool
  local isExpandable::Bool = ConnectorType.isExpandable(conn.cty)
  return isExpandable
end

function isDeleted(conn::Connector)::Bool
  local isDeleted::Bool = isDeleted(conn.name)
  return isDeleted
end

function setOutside(conn::Connector)::Connector

  if conn.face != Face.OUTSIDE
    @assign conn.face = Face.OUTSIDE
  end
  return conn
end

function isInside(conn::Connector)::Bool
  local isInside::Bool

  local f::FaceType = conn.face
  #=  Needed due to #4502
  =#

  @assign isInside = f == Face.INSIDE
  return isInside
end

function isOutside(conn::Connector)::Bool
  local isOutside::Bool

  local f::FaceType = conn.face
  #=  Needed due to #4502
  =#

  @assign isOutside = f == Face.OUTSIDE
  return isOutside
end

function isNodeNameEqual(conn1::Connector, conn2::Connector)::Bool
  local isEqual::Bool =
    name(node(conn1.name)) ==
    name(node(conn2.name))
  return isEqual
end

function isPrefix(conn1::Connector, conn2::Connector)::Bool
  local isPrefix::Bool = isPrefix(conn1.name, conn2.name)
  return isPrefix
end

function isEqual(conn1::Connector, conn2::Connector)::Bool
  local isEqual::Bool =
    isEqual(conn1.name, conn2.name) && conn1.face == conn2.face
  return isEqual
end

function variability(conn::Connector)::VariabilityType
  local var::VariabilityType =
    variability(component(node(conn.name)))
  return var
end

function Connector_getInfo(conn::Connector)::SourceInfo
  local info::SourceInfo = conn.source.info
  return info
end

function getType(conn::Connector)::M_Type
  local ty::M_Type = conn.ty
  return ty
end

""" #= Constructs a list of Connectors from a cref or an array of crefs. =#"""
function fromExp(
  exp::Expression,
  source::DAE.ElementSource,
  conns::List{<:Connector} = nil
)::List{Connector}

  @assign conns = begin
    @match exp begin
      CREF_EXPRESSION(__) => begin
        _cons(fromCref(exp.cref, exp.ty, source), conns)
      end

      ARRAY_EXPRESSION(__) => begin
        for e in listReverse(exp.elements)
          @assign conns = fromExp(e, source, conns)
        end
        conns
      end

      _ => begin
        Error.assertion(
          false,
          getInstanceName() +
          " got unknown expression " +
          toString(exp),
          sourceInfo(),
        )
        fail()
      end
    end
  end
  return conns
end

function fromFacedCref(
  cref::ComponentRef,
  ty::NFType,
  face::FaceType,
  source::DAE.ElementSource,
)::Connector
  local conn::Connector

  local nodeVar::InstNode = node(cref)
  local comp::Component
  local cty::ConnectorType.TYPE
  local res::Restriction

  if isComponent(nodeVar)
    @assign comp = component(nodeVar)
    @assign res = restriction(getClass(classInstance(comp)))
    @assign cty = connectorType(comp)
  else
    @assign cty = intBitOr(ConnectorType.VIRTUAL, ConnectorType.POTENTIAL)
  end
  @assign conn =
    CONNECTOR(simplifySubscripts(cref), ty, face, cty, source)
  return conn
end

function fromCref(cref::ComponentRef, ty::NFType, source::DAE.ElementSource)::Connector
  local conn::Connector = fromFacedCref(cref, ty, crefFace(cref), source)
  return conn
end

function splitImpl2(
  name::ComponentRef,
  face::FaceType,
  source::DAE.ElementSource,
  comps::List{<:InstNode},
  scalarize::ScalarizeSettingType,
  conns::List{<:Connector},
  dims::List{<:Dimension},
)::List{Connector}

  local c::Component
  local cref::ComponentRef
  local ty::M_Type
  local cty::ConnectorType.TYPE

  for comp in comps
    @assign c = component(comp)
    @assign ty = getType(c)
    @assign cty = P_Component.connectorType(c)
    if !ConnectorType.isPotentiallyPresent(cty)
      @assign cref = append(
        fromNode(comp, ty),
        name,
      )
      @assign conns = splitImpl(cref, ty, face, source, cty, scalarize, conns, dims)
    end
  end
  return conns
end

function splitImpl(
  name::ComponentRef,
  ty::M_Type,
  face::FaceType,
  source::DAE.ElementSource,
  cty::ConnectorType.TYPE,
  scalarize::ScalarizeSettingType,
  conns::List{<:Connector} = nil,
  dims::List{<:Dimension} = nil,
)::List{Connector} #= accumulated dimensions if splitArrays = false =#

  @assign conns = begin
    local ety::M_Type
    local ct::ComplexType
    local tree::ClassTree
    @match ty begin
      TYPE_COMPLEX(complexTy = ct && ComplexType.CONNECTOR(__)) => begin
        @assign conns =
          splitImpl2(name, face, source, ct.potentials, scalarize, conns, dims)
        @assign conns = splitImpl2(name, face, source, ct.flows, scalarize, conns, dims)
        @assign conns = splitImpl2(name, face, source, ct.streams, scalarize, conns, dims)
        conns
      end

      TYPE_COMPLEX(complexTy = COMPLEX_EXTERNAL_OBJECT(__)) => begin
        _cons(CONNECTOR(name, liftArrayLeftList(ty, dims), face, cty, source), conns)
      end

      TYPE_COMPLEX(__) => begin
        @assign tree = classTree(getClass(ty.cls))
        @assign conns = splitImpl2(
          name,
          face,
          source,
          arrayList(getComponents(tree)),
          scalarize,
          conns,
          dims,
        )
        conns
      end

      TYPE_ARRAY(
        elementType = ety && TYPE_COMPLEX(__),
      ) where {(scalarize >= ScalarizeSetting.PREFIX)} => begin
        for c in scalarize(name)
          @assign conns = splitImpl(c, ety, face, source, cty, scalarize, conns, dims)
        end
        conns
      end

      TYPE_ARRAY(elementType = ety) => begin
        if scalarize == ScalarizeSetting.ALL
          for c in scalarize(name)
            @assign conns = splitImpl(c, ety, face, source, cty, scalarize, conns, dims)
          end
        else
          if !Type.isEmptyArray(ty)
            @assign conns = splitImpl(
              name,
              ety,
              face,
              source,
              cty,
              scalarize,
              conns,
              listAppend(dims, ty.dimensions),
            )
          end
        end
        conns
      end

      _ => begin
        _cons(CONNECTOR(name, liftArrayLeftList(ty, dims), face, cty, source), conns)
      end
    end
  end
  return conns
end

""" #= Determines whether a cref refers to an inside or outside connector, where
     an outside connector is a connector where the first part of the cref is a
     connector, and an inside connector all other crefs. =#"""
function crefFace(cref::ComponentRef)::FaceType
  local face::FaceType
  @assign face = begin
    @match cref begin
      COMPONENT_REF_CREF(restCref = COMPONENT_REF_EMPTY(__)) => begin
        Face.OUTSIDE
      end

      _ => begin
        if isConnector(node(firstNonScope(
          cref,
        )))
          Face.OUTSIDE
        else
          Face.INSIDE
        end
      end
    end
  end
  #=  Simple identifiers must be connectors and thus outside.
  =#
  #=  Otherwise, check first part of the cref.
  =#
  return face
end
