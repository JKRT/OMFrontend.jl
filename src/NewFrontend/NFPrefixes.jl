@UniontypeDecl Replaceable


module ConnectorType

import ..Main.NFType

const TYPE = Int

using MetaModelica
using ExportAll

const NON_CONNECTOR = 0::Int
const POTENTIAL = intBitLShift(1, 0)::Int #= A connector element without a prefix. =#
const FLOW = intBitLShift(1, 1)::Int #= A connector element with flow prefix. =#
const STREAM = intBitLShift(1, 2)::Int #= A connector element with stream prefix. =#
const POTENTIALLY_PRESENT = intBitLShift(1, 3)::Int #= An element declared inside an expandable connector. =#
const VIRTUAL = intBitLShift(1, 4)::Int #= A virtual connector used in a connection. =#
const CONNECTOR = intBitLShift(1, 5)::Int #= A non-expandable connector that contains elements. =#
const EXPANDABLE = intBitLShift(1, 6)::Int #= An expandable connector. =#
#=  flow/stream =#
const FLOW_STREAM_MASK = intBitOr(FLOW, STREAM)::Int
#=  potential/flow/stream =#
const PREFIX_MASK = intBitOr(POTENTIAL, FLOW_STREAM_MASK)::Int
#=  Some kind of connector, where anything inside an expandable connector also counts. =#
const CONNECTOR_MASK = intBitOr(CONNECTOR, intBitOr(EXPANDABLE, POTENTIALLY_PRESENT))::Int
#=  An element in an expandable connector. =#
const UNDECLARED_MASK = intBitOr(VIRTUAL, POTENTIALLY_PRESENT)::Int
@exportAll()
end

function fromSCode(scodeCty::SCode.ConnectorType)::Int
  local cty::Int
  cty = begin
    @match scodeCty begin
      SCode.POTENTIAL(__) => begin
        0
      end
      SCode.FLOW(__) => begin
        ConnectorType.FLOW
      end
      SCode.STREAM(__) => begin
        throw("WRONG!")
        ConnectorType.STREAM
      end
    end
  end
  return cty
end

function toDAE(cty::Int)::DAE.ConnectorType
  local dcty::DAE.ConnectorType
  if intBitAnd(cty, ConnectorType.POTENTIAL) > 0
    dcty = DAE.POTENTIAL()
  elseif intBitAnd(cty, ConnectorType.FLOW) > 0
    dcty = DAE.FLOW()
  elseif intBitAnd(cty, ConnectorType.STREAM) > 0
    dcty = DAE.STREAM(NONE())
  else
    dcty = DAE.NON_CONNECTOR()
  end
  return dcty
end

function merge(
  outerCty::Int,
  innerCty::Int,
  node::InstNode,
  isClass::Bool = false,
)::Int
  local cty::Int
  #=  If both the outer and the inner has flow or stream, give an error.=#
  if (intBitAnd(outerCty, ConnectorType.FLOW_STREAM_MASK) > 0) && (intBitAnd(innerCty, ConnectorType.FLOW_STREAM_MASK) > 0)
    printPrefixError(toString(outerCty), toString(innerCty), node)
  end
  cty = intBitOr(outerCty, innerCty)
  return cty
end

function isPotential(cty::Int)::Bool
  b = intBitAnd(cty, ConnectorType.POTENTIAL) > 0
  return b
end

function setPotential(cty::Int)::Int
  cty = intBitOr(cty, ConnectorType.POTENTIAL)
  return cty
end

function isFlow(cty::Int)::Bool
  local b::Bool
  b = intBitAnd(cty, ConnectorType.FLOW) > 0
  return b
end

function isStream(cty::Int)::Bool
  local b::Bool
  b = intBitAnd(cty, ConnectorType.STREAM) > 0
  return b
end

function isFlowOrStream(cty::Int)::Bool
  local isFlowOrStream::Bool
  isFlowOrStream = intBitAnd(cty, ConnectorType.FLOW_STREAM_MASK) > 0
  return isFlowOrStream
end

function unsetFlowStream(cty::Int)::Int
  cty = intBitAnd(cty, intBitNot(ConnectorType.FLOW_STREAM_MASK))
  return cty
end

""" #= Returns true if the connector type has the connector bit set, otherwise false. =#"""
function isConnector(cty::Int)::Bool
  local isConnector::Bool

  @assign isConnector = intBitAnd(cty, ConnectorType.CONNECTOR) > 0
  return isConnector
end

function setConnector(cty::Int)::Int
  cty = intBitOr(cty, ConnectorType.CONNECTOR)
  return cty
end

""" #= Returns treu if the connector type has the connector, expandable, or
     potentially present bits set, otherwise false. =#"""
function isConnectorType(cty::Int)::Bool
  local isConnector::Bool
  @assign isConnector = intBitAnd(cty, ConnectorType.CONNECTOR_MASK) > 0
  return isConnector
end

function isExpandable(cty::Int)::Bool
  local isExpandable::Bool
  @assign isExpandable = intBitAnd(cty, ConnectorType.EXPANDABLE) > 0
  return isExpandable
end

function setExpandable(cty::Int)::Int
  @assign cty = intBitOr(cty, EXPANDABLE)
  return cty
end

""" #= Returns true if the connector type has the potentially present or virtual
     bits set, otherwise false. =#"""
function isUndeclared(cty::Int)::Bool
  local isExpandableElement::Bool

  @assign isExpandableElement = intBitAnd(cty, ConnectorType.UNDECLARED_MASK) > 0
  return isExpandableElement
end

function isVirtual(cty::Int)::Bool
  local isVirtual::Bool

  @assign isVirtual = intBitAnd(cty, VIRTUAL) > 0
  return isVirtual
end

function isPotentiallyPresent(cty::Int)::Bool
  local b::Bool
  b = intBitAnd(cty, ConnectorType.POTENTIALLY_PRESENT) > 0
  return b
end

function setPresent(cty::Int)::Int
  @assign cty = intBitAnd(cty, intBitNot(ConnectorType.POTENTIALLY_PRESENT))
  return cty
end

function toString(cty::Int)::String
  local str::String
  if intBitAnd(cty, ConnectorType.FLOW) > 0
    @assign str = "flow string(cty)"
  elseif intBitAnd(cty, ConnectorType.STREAM) > 0
    @assign str = "stream string(cty)"
  elseif intBitAnd(cty, ConnectorType.EXPANDABLE) > 0
    @assign str = "expandable string(cty)"
  elseif intBitAnd(cty, ConnectorType.POTENTIAL) > 0
    @assign str = "potential string(cty)"
  else
    str = "Unspecified"
  end
  return str
end

function unparse(cty::Int)::String
  local str::String

  if intBitAnd(cty, ConnectorType.FLOW) > 0
    @assign str = "flow "
  elseif intBitAnd(cty, ConnectorType.STREAM) > 0
    @assign str = "stream "
  else
    @assign str = ""
  end
  return str
end

function toDebugString(cty::Int)::String
  local str::String
  local strl::List{String} = nil
  suffix = "(Number:$cty)"
  if intBitAnd(cty, ConnectorType.POTENTIAL) > 0
    @assign strl = _cons("potential $suffix", strl)
  end
  if intBitAnd(cty, ConnectorType.FLOW) > 0
    @assign strl = _cons("flow $suffix", strl)
  end
  if intBitAnd(cty, ConnectorType.STREAM) > 0
    @assign strl = _cons("stream $suffix", strl)
  end
  if intBitAnd(cty, ConnectorType.POTENTIALLY_PRESENT) > 0
    @assign strl = _cons("potentially present $suffix", strl)
  end
  if intBitAnd(cty, ConnectorType.VIRTUAL) > 0
    @assign strl = _cons("virtual $suffix", strl)
  end
  if intBitAnd(cty, ConnectorType.CONNECTOR) > 0
    @assign strl = _cons("connector $suffix", strl)
  end
  if intBitAnd(cty, ConnectorType.EXPANDABLE) > 0
    @assign strl = _cons("expandable $suffix", strl)
  end
  @assign str = stringDelimitList(strl, " ")
  return str
end

module Parallelism
  const NON_PARALLEL = 1
  const GLOBAL = 2
  const LOCAL = 3
end

const ParallelismType = Int

module Variability
const CONSTANT = 1
const STRUCTURAL_PARAMETER = 2
const PARAMETER = 3
const NON_STRUCTURAL_PARAMETER = 4
const DISCRETE = 5
const IMPLICITLY_DISCRETE = 6
const CONTINUOUS = 7
end

const VariabilityType = Int

Direction = (() -> begin #= Enumeration =#
             NONE = 1
             INPUT = 2
             OUTPUT = 3
             () -> (NONE; INPUT; OUTPUT)
             end)()
const DirectionType = Int

InnerOuter = (() -> begin #= Enumeration =#
              NOT_INNER_OUTER = 1
              INNER = 2
              OUTER = 3
              INNER_OUTER = 4
              () -> (NOT_INNER_OUTER; INNER; OUTER; INNER_OUTER)
              end)()


Visibility = (() -> begin #= Enumeration =#
              PUBLIC = 1
              PROTECTED = 2
              () -> (PUBLIC; PROTECTED)
              end)()

const VisibilityType = Int

@Uniontype Replaceable begin
  @Record REPLACEABLE begin
    constrainingClass::Option{InstNode}
  end

  @Record NOT_REPLACEABLE begin
  end
end

function parallelismFromSCode(scodePar::SCode.Parallelism)::ParallelismType
  local par::ParallelismType
  @assign par = begin
    @match scodePar begin
      SCode.PARGLOBAL(__) => begin
        Parallelism.GLOBAL
      end
      SCode.PARLOCAL(__) => begin
        Parallelism.LOCAL
      end
      SCode.NON_PARALLEL(__) => begin
        Parallelism.NON_PARALLEL
      end
    end
  end
  return par
end


function parallelismToDAE(par)::DAE.VarParallelism
  local dpar::DAE.VarParallelism
  @assign dpar = begin
    @match par begin
      Parallelism.GLOBAL => begin
        DAE.PARGLOBAL()
      end
      Parallelism.LOCAL => begin
        DAE.PARLOCAL()
      end

      Parallelism.NON_PARALLEL => begin
        DAE.NON_PARALLEL()
      end
    end
  end
  return dpar
end

function parallelismString(par)::String
  local str::String

  @assign str = begin
    @match par begin
      Parallelism.GLOBAL => begin
        "parglobal"
      end

      Parallelism.LOCAL => begin
        "parlocal"
      end

      _ => begin
        ""
      end
    end
  end
  return str
end

function unparseParallelism(par)::String
  local str::String

  @assign str = begin
    @match par begin
      Parallelism.GLOBAL => begin
        "parglobal "
      end

      Parallelism.LOCAL => begin
        "parlocal "
      end

      _ => begin
        ""
      end
    end
  end
  return str
end

function mergeParallelism(
  outerPar,
  innerPar,
  node::InstNode,
)
  local par

  if outerPar == Parallelism.NON_PARALLEL
    @assign par = innerPar
  elseif innerPar == Parallelism.NON_PARALLEL
    @assign par = outerPar
  elseif innerPar == outerPar
    @assign par = innerPar
  else
    printPrefixError(parallelismString(outerPar), parallelismString(innerPar), node)
  end
  return par
end

function variabilityFromSCode(scodeVar::SCode.Variability)
  local var
  @assign var = begin
    @match scodeVar begin
      SCode.CONST(__) => begin
        Variability.CONSTANT
      end
      SCode.PARAM(__) => begin
        Variability.PARAMETER
      end
      SCode.DISCRETE(__) => begin
        Variability.DISCRETE
      end
      SCode.VAR(__) => begin
        Variability.CONTINUOUS
      end
    end
  end
  return var
end

function variabilityToSCode(var)::SCode.Variability
  local scodeVar::SCode.Variability

  @assign scodeVar = begin
    @match var begin
      Variability.CONSTANT => begin
        SCode.Variability.CONST()
      end

      Variability.STRUCTURAL_PARAMETER => begin
        SCode.Variability.PARAM()
      end

      Variability.PARAMETER => begin
        SCode.Variability.PARAM()
      end

      Variability.NON_STRUCTURAL_PARAMETER => begin
        SCode.Variability.PARAM()
      end

      Variability.DISCRETE => begin
        SCode.Variability.DISCRETE()
      end

      _ => begin
        SCode.VAR()
      end
    end
  end
  return scodeVar
end

function variabilityToDAE(var)::DAE.VarKind
  local varKind::DAE.VarKind

  @assign varKind = begin
    @match var begin
      Variability.CONSTANT => begin
        DAE.CONST()
      end

      Variability.STRUCTURAL_PARAMETER => begin
        DAE.PARAM()
      end

      Variability.PARAMETER => begin
        DAE.PARAM()
      end

      Variability.NON_STRUCTURAL_PARAMETER => begin
        DAE.PARAM()
      end

      Variability.DISCRETE => begin
        DAE.DISCRETE()
      end

      _ => begin
        DAE.VARIABLE()
      end
    end
  end
  return varKind
end

function variabilityToDAEConst(var)::DAE.Const
  local M_const::DAE.Const

  @assign M_const = begin
    @match var begin
      Variability.CONSTANT => begin
        DAE.Const.C_CONST()
      end

      Variability.STRUCTURAL_PARAMETER => begin
        DAE.Const.C_PARAM()
      end

      Variability.PARAMETER => begin
        DAE.Const.C_PARAM()
      end

      Variability.NON_STRUCTURAL_PARAMETER => begin
        DAE.Const.C_PARAM()
      end

      _ => begin
        DAE.Const.C_VAR()
      end
    end
  end
  return M_const
end

function variabilityString(var)::String
  local str::String

  @assign str = begin
    @match var begin
      Variability.CONSTANT => begin
        "constant"
      end

      Variability.STRUCTURAL_PARAMETER => begin
        "parameter"
      end

      Variability.PARAMETER => begin
        "parameter"
      end

      Variability.NON_STRUCTURAL_PARAMETER => begin
        "parameter"
      end

      Variability.DISCRETE => begin
        "discrete"
      end

      Variability.IMPLICITLY_DISCRETE => begin
        "discrete"
      end

      Variability.CONTINUOUS => begin
        "continuous"
      end
    end
  end
  return str
end

function unparseVariability(var::VariabilityType, ty::NFType)::String
  local str::String
  @assign str = begin
    @match var begin
      Variability.CONSTANT => begin
        "constant "
      end

      Variability.STRUCTURAL_PARAMETER => begin
        "parameter "
      end

      Variability.PARAMETER => begin
        "parameter "
      end

      Variability.NON_STRUCTURAL_PARAMETER => begin
        "parameter "
      end

      Variability.DISCRETE => begin
        if isDiscrete(ty)
          ""
        else
          "discrete "
        end
      end

      _ => begin
        ""
      end
    end
  end
  return str
end

function variabilityMax(var1, var2)
  local var = if var1 > var2
    var1
  else
    var2
  end
  return var
end

function variabilityMin(var1, var2)
  local var = if var1 > var2
    var2
  else
    var1
  end
  return var
end

function effectiveVariability(inVar)
  local outVar

  @assign outVar = begin
    @match inVar begin
      Variability.STRUCTURAL_PARAMETER => begin
        Variability.PARAMETER
      end

      Variability.NON_STRUCTURAL_PARAMETER => begin
        Variability.PARAMETER
      end

      Variability.IMPLICITLY_DISCRETE => begin
        Variability.DISCRETE
      end

      _ => begin
        inVar
      end
    end
  end
  return outVar
end

function directionFromSCode(scodeDir::Absyn.Direction)
  local dir

  @assign dir = begin
    @match scodeDir begin
      Absyn.INPUT(__) => begin
        Direction.INPUT
      end
      Absyn.OUTPUT(__) => begin
        Direction.OUTPUT
      end
      _ => begin
        Direction.NONE
      end
    end
  end
  return dir
end

function directionToDAE(dir)::DAE.VarDirection
  local ddir::DAE.VarDirection
  @assign ddir = begin
    @match dir begin
      Direction.INPUT => begin
        DAE.INPUT()
      end
      Direction.OUTPUT => begin
        DAE.OUTPUT()
      end
      _ => begin
        DAE.BIDIR()
      end
    end
  end
  return ddir
end

function directionToAbsyn(dir)::Absyn.Direction
  local adir::Absyn.Direction

  @assign adir = begin
    @match dir begin
      Direction.INPUT => begin
        Absyn.INPUT()
      end

      Direction.OUTPUT => begin
        Absyn.OUTPUT()
      end

      _ => begin
        Absyn.BIDIR()
      end
    end
  end
  return adir
end

function directionString(dir)::String
  local str::String

  @assign str = begin
    @match dir begin
      Direction.INPUT => begin
        "input"
      end

      Direction.OUTPUT => begin
        "output"
      end

      _ => begin
        ""
      end
    end
  end
  return str
end

function unparseDirection(dir)::String
  local str::String

  @assign str = begin
    @match dir begin
      Direction.INPUT => begin
        "input "
      end

      Direction.OUTPUT => begin
        "output "
      end

      _ => begin
        ""
      end
    end
  end
  return str
end

function mergeDirection(
  outerDir,
  innerDir,
  node::InstNode,
  allowSame::Bool = false,
)
  local dir

  if outerDir == Direction.NONE
    @assign dir = innerDir
  elseif innerDir == Direction.NONE
    @assign dir = outerDir
  elseif allowSame && outerDir == innerDir
    @assign dir = innerDir
  else
    printPrefixError(directionString(outerDir), directionString(innerDir), node)
  end
  return dir
end

function innerOuterFromSCode(scodeIO::Absyn.InnerOuter)
  local io
  @assign io = begin
    @match scodeIO begin
      Absyn.NOT_INNER_OUTER(__) => begin
        InnerOuter.NOT_INNER_OUTER
      end
      Absyn.INNER(__) => begin
        InnerOuter.INNER
      end
      Absyn.OUTER(__) => begin
        InnerOuter.OUTER
      end
      Absyn.INNER_OUTER(__) => begin
        InnerOuter.INNER_OUTER
      end
    end
  end
  return io
end

function innerOuterToAbsyn(inIO)::Absyn.InnerOuter
  local outIO::Absyn.InnerOuter

  @assign outIO = begin
    @match inIO begin
      InnerOuter.NOT_INNER_OUTER => begin
        Absyn.NOT_INNER_OUTER()
      end

      InnerOuter.INNER => begin
        Absyn.INNER()
      end

      InnerOuter.OUTER => begin
        Absyn.OUTER()
      end

      InnerOuter.INNER_OUTER => begin
        Absyn.INNER_OUTER()
      end
    end
  end
  return outIO
end

function innerOuterString(io)::String
  local str::String

  @assign str = begin
    @match io begin
      InnerOuter.INNER => begin
        "inner"
      end

      InnerOuter.OUTER => begin
        "outer"
      end

      InnerOuter.INNER_OUTER => begin
        "inner outer"
      end

      _ => begin
        ""
      end
    end
  end
  return str
end

function unparseInnerOuter(io)::String
  local str::String

  @assign str = begin
    @match io begin
      InnerOuter.INNER => begin
        "inner "
      end

      InnerOuter.OUTER => begin
        "outer "
      end

      InnerOuter.INNER_OUTER => begin
        "inner outer "
      end

      _ => begin
        ""
      end
    end
  end
  return str
end

function visibilityFromSCode(scodeVis::SCode.Visibility)
  local vis
  @assign vis = begin
    @match scodeVis begin
      SCode.PUBLIC(__) => begin
        Visibility.PUBLIC
      end
      _ => begin
        Visibility.PROTECTED
      end
    end
  end
  return vis
end

function visibilityToDAE(vis)::DAE.VarVisibility
  local dvis::DAE.VarVisibility = if vis == Visibility.PUBLIC
    DAE.PUBLIC()
  else
    DAE.PROTECTED()
  end
  return dvis
end

function visibilityToSCode(vis)::SCode.Visibility
  local scodeVis::SCode.Visibility = if vis == Visibility.PUBLIC
    SCode.Visibility.PUBLIC()
  else
    SCode.Visibility.PROTECTED()
  end
  return scodeVis
end

function visibilityString(vis)::String
  local str::String = if vis == Visibility.PUBLIC
    "public"
  else
    "protected"
  end
  return str
end

function unparseVisibility(vis)::String
  local str::String = if vis == Visibility.PROTECTED
    "protected "
  else
    ""
  end
  return str
end

function mergeVisibility(outerVis, innerVis)::VisibilityType
  local vis = if outerVis == Visibility.PROTECTED
    outerVis
  else
    innerVis
  end
  return vis
end

function unparseReplaceable(repl::Replaceable)::String
  local str::String
  str = begin
    @match repl begin
      REPLACEABLE(__) => begin
        "replaceable "
      end
      _ => begin
        ""
      end
    end
  end
  return str
end

function printPrefixError(outerPrefix::String, innerPrefix::String, node::InstNode)
  Error.addSourceMessage(
    Error.INVALID_TYPE_PREFIX,
    list(outerPrefix, typeName(node), name(node), innerPrefix),
    info(node),
  )
  return fail()
end
