"""
```
struct FLAT_MODEL <: FlatModel
  name::String
  variables::Vector{Variable}
  equations::Vector{Equation}
  initialEquations::Vector{Equation}
  algorithms::Vector{Algorithm}
  initialAlgorithms::Vector{Algorithm}
  #= VSS Modelica extension =#
  structuralSubmodels::List{FlatModel}
  scodeProgram::Option{SCode.CLASS}
  #= Dynamically Overconstrained connectors =#
  DOCC_equations::List{Equation}
  #= Contains the set of unresolved connect equations =#
  unresolvedConnectEquations::List{Equation}
  active_DOCC_Equations::Vector{Bool}
  #= End VSS Modelica extension =#
  comment::Option{SCode.Comment}
end
```

  The flat representation of a Modelica Model.
"""
struct FLAT_MODEL <: FlatModel
  name::String
  variables::Vector{Variable}
  equations::Vector{Equation}
  initialEquations::Vector{Equation}
  algorithms::Vector{Algorithm}
  initialAlgorithms::Vector{Algorithm}
  #= VSS Modelica extension =#
  structuralSubmodels::List{FlatModel}
  scodeProgram::Option{SCode.CLASS}
  #= Dynamically Overconstrained connectors =#
  DOCC_equations::List{Equation}
  #= Contains the set of unresolved connect equations =#
  unresolvedConnectEquations::List{Equation}
  active_DOCC_Equations::Vector{Bool}
  #= End VSS Modelica extension =#
  comment::Option{SCode.Comment}
end

module TypeTreeImpl
using MetaModelica
using ExportAll
import ..Absyn.Path
import ..AbsynUtil
import ..BaseAvlTree
import ..Main.M_Type
import ..Type
import Absyn
import ..toString

const Key = Absyn.Path
const Value = M_Type

include("../Util/baseAvlTreeCode.jl")

function keyStr(k)
  return AbsynUtil.pathString(k)
end

valueStr = (vs) -> begin
  return toString(vs)
end

addConflictDefault = addConflictKeep
#= John 2023-04-03:
  Default(Julia) string compare does not give the right result.
  MetaModelica string comp is used instead.
=#
keyCompare = (inKey1::Key, inKey2::Key) -> begin
  return stringCompare(keyStr(inKey1), keyStr(inKey2))
end

@exportAll()
end

const TypeTree = TypeTreeImpl.Tree

function reconstructRecordInstance(
  recordName::ComponentRef,
  variables::List{<:Variable},
)::Variable
  local recordVar::Variable
  local record_node::InstNode
  local record_comp::Component
  local record_ty::M_Type
  local field_exps::List{Expression}
  local record_exp::Expression
  local record_binding::Binding
  @assign record_node = node(recordName)
  @assign record_comp = component(record_node)
  @assign record_ty = nodeType(recordName)
  @assign field_exps = nil
  for v in variables
    if hasExp(v.binding)
      @assign field_exps = _cons(getExp(v.binding), field_exps)
    else
      @assign field_exps = nil
      break
    end
  end
  if listEmpty(field_exps)
    @assign record_binding = EMPTY_BINDING()
  else
    @assign field_exps = listReverseInPlace(field_exps)
    @assign record_exp = makeRecord(
      scopePath(classScope(record_node)),
      record_ty,
      field_exps,
    )
    @assign record_binding =
      FLAT_BINDING(record_exp, variability(record_comp))
  end
  @assign recordVar = VARIABLE(
    recordName,
    record_ty,
    record_binding,
    visibility(record_node),
    getAttributes(record_comp),
    nil,
    P_Component.comment(record_comp),
    info(record_node),
  )
  return recordVar
end

function reconstructRecordInstances(variables::List{<:Variable})::List{Variable}
  local outVariables::List{Variable} = nil

  local rest_vars::List{Variable} = variables
  local record_vars::List{Variable}
  local var::Variable
  local parent_cr::ComponentRef
  local parent_ty::M_Type
  local field_count::Int

  while !listEmpty(rest_vars)
    @match _cons(var, rest_vars) = rest_vars
    @assign parent_cr = rest(var.name)
    if !isEmpty(parent_cr)
      @assign parent_ty = nodeType(parent_cr)
      if isRecord(parent_ty)
        field_count = listLength(Type.recordFields(parent_ty))
        (record_vars, rest_vars) = ListUtil.split(rest_vars, field_count - 1)
        record_vars = _cons(var, record_vars)
        var = reconstructRecordInstance(parent_cr, record_vars)
      end
    end
    outVariables = _cons(var, outVariables)
  end
  outVariables = listReverseInPlace(outVariables)
  return outVariables
end

function collectSubscriptedFlatType(
  exp::Expression,
  subs::List{<:Subscript},
  subscriptedTy::M_Type,
  types::TypeTree,
)::TypeTree
  local exp_ty::M_Type
  local sub_tyl::List{M_Type}
  local dims::List{Dimension}
  local strl::List{String}
  local name::String
  exp_ty = typeOf(exp)
  dims = ListUtil.firstN(arrayDims(exp_ty), listLength(subs))
  sub_tyl = list(P_Dimension.Dimension.subscriptType(d) for d in dims)
  name = Type.subscriptedTypeName(exp_ty, sub_tyl)
  types = TypeTree.add(
    types,
    Absyn.IDENT(name),
    Type.SUBSCRIPTED(name, exp_ty, sub_tyl, subscriptedTy),
  )
  return types
end

function collectComponentFlatTypes(componentArg::InstNode, types::TypeTree)::TypeTree
  local comp::Component
  comp = component(componentArg)
  types = collectFlatType(getType(comp), types)
  types = collectBindingFlatTypes(getBinding(comp), types)
  return types
end

function collectFunctionFlatTypes(fn::M_Function, types::TypeTree)::TypeTree
  local body::Vector{Statement}
  types = foldComponents(
    classTree(getClass(fn.node)),
    collectComponentFlatTypes,
    types,
  )
  #= External functions does not have a function body. =#
  body = if ! isExternal(fn)
    getBody(fn)
  else
    nil
  end
  types = ListUtil.fold(body, collectStatementFlatTypes, types)
  return types
end

function collectExpFlatTypes_traverse(exp::Expression, types::TypeTree)::TypeTree
  @assign types = begin
    @match exp begin
      SUBSCRIPTED_EXP_EXPRESSION(__) => begin
        @assign types = collectSubscriptedFlatType(exp.exp, exp.subscripts, exp.ty, types)
        types
      end

      _ => begin
        collectFlatType(typeOf(exp), types)
      end
    end
  end
  return types
end

function collectExpFlatTypes(exp::Expression, types::TypeTree)::TypeTree

  @assign types = fold(exp, collectExpFlatTypes_traverse, types)
  return types
end

function collectStmtBranchFlatTypes(
  branch::Tuple{<:Expression, List{<:Statement}},
  types::TypeTree,
)::TypeTree
  @assign types = collectExpFlatTypes(Util.tuple21(branch), types)
  @assign types = ListUtil.fold(Util.tuple22(branch), collectStatementFlatTypes, types)
  return types
end

function collectStatementFlatTypes(stmt::Statement, types::TypeTree)::TypeTree
   () = begin
    @match stmt begin
      ALG_ASSIGNMENT(__) => begin
        @assign types = collectExpFlatTypes(stmt.lhs, types)
        @assign types = collectExpFlatTypes(stmt.rhs, types)
        @assign types = collectFlatType(stmt.ty, types)
        ()
      end
      ALG_FOR(__) => begin
        @assign types = ListUtil.fold(stmt.body, collectStatementFlatTypes, types)
        @assign types = collectExpFlatTypes(Util.getOption(stmt.range), types)
        ()
      end

      ALG_IF(__) => begin
        @assign types = ListUtil.fold(stmt.branches, collectStmtBranchFlatTypes, types)
        ()
      end

      ALG_WHEN(__) => begin
        @assign types = ListUtil.fold(stmt.branches, collectStmtBranchFlatTypes, types)
        ()
      end

      ALG_ASSERT(__) => begin
        @assign types = collectExpFlatTypes(stmt.condition, types)
        @assign types = collectExpFlatTypes(stmt.message, types)
        @assign types = collectExpFlatTypes(stmt.level, types)
        ()
      end

      ALG_TERMINATE(__) => begin
        @assign types = collectExpFlatTypes(stmt.message, types)
        ()
      end

      ALG_NORETCALL(__) => begin
        @assign types = collectExpFlatTypes(stmt.exp, types)
        ()
      end

      ALG_WHILE(__) => begin
        @assign types = collectExpFlatTypes(stmt.condition, types)
        @assign types = ListUtil.fold(stmt.body, collectStatementFlatTypes, types)
        ()
      end

      _ => begin
        ()
      end
    end
  end
  return types
end

function collectAlgorithmFlatTypes(alg::Algorithm, types::TypeTree)::TypeTree

  @assign types = ListUtil.fold(alg.statements, collectStatementFlatTypes, types)
  return types
end

function collectEqBranchFlatTypes(
  branch::Equation_Branch,
  types::TypeTree,
)::TypeTree
  () = begin
    @match branch begin
      EQUATION_BRANCH(__) => begin
        types = collectExpFlatTypes(branch.condition, types)
        types = ListUtil.fold(branch.body, collectEquationFlatTypes, types)
        ()
      end
      _ => begin
        ()
      end
    end
  end
  return types
end

function collectEquationFlatTypes(eq::Equation, types::TypeTree)::TypeTree

  () = begin
    @match eq begin
      EQUATION_EQUALITY(__) => begin
        @assign types = collectExpFlatTypes(eq.lhs, types)
        @assign types = collectExpFlatTypes(eq.rhs, types)
        @assign types = collectFlatType(eq.ty, types)
        ()
      end

      EQUATION_ARRAY_EQUALITY(__) => begin
        @assign types = collectExpFlatTypes(eq.lhs, types)
        @assign types = collectExpFlatTypes(eq.rhs, types)
        @assign types = collectFlatType(eq.ty, types)
        ()
      end

      EQUATION_FOR(__) => begin
        @assign types = ListUtil.fold(eq.body, collectEquationFlatTypes, types)
        ()
      end

      EQUATION_IF(__) => begin
        @assign types = ListUtil.fold(eq.branches, collectEqBranchFlatTypes, types)
        ()
      end

      EQUATION_WHEN(__) => begin
        @assign types = ListUtil.fold(eq.branches, collectEqBranchFlatTypes, types)
        ()
      end

      EQUATION_ASSERT(__) => begin
        @assign types = collectExpFlatTypes(eq.condition, types)
        @assign types = collectExpFlatTypes(eq.message, types)
        @assign types = collectExpFlatTypes(eq.level, types)
        ()
      end

      EQUATION_TERMINATE(__) => begin
        @assign types = collectExpFlatTypes(eq.message, types)
        ()
      end

      EQUATION_REINIT(__) => begin
        @assign types = collectExpFlatTypes(eq.reinitExp, types)
        ()
      end

      EQUATION_NORETCALL(__) => begin
        @assign types = collectExpFlatTypes(eq.exp, types)
        ()
      end

      _ => begin
        ()
      end
    end
  end
  return types
end

function collectBindingFlatTypes(binding::Binding, types::TypeTree)::TypeTree

  if isExplicitlyBound(binding)
    @assign types = collectExpFlatTypes(getTypedExp(binding), types)
  end
  return types
end

function collectFlatType(ty::M_Type, types::TypeTree)::TypeTree
  () = begin
    @match ty begin
      TYPE_ENUMERATION(__) => begin
        types = TypeTreeImpl.add(types, ty.typePath, ty)
        ()
      end
      TYPE_ARRAY(__) => begin
        types = foldExpList(
          ty.dimensions,
          collectExpFlatTypes_traverse,
          types,
        )
        types = collectFlatType(ty.elementType, types)
        ()
      end
      TYPE_COMPLEX(complexTy = COMPLEX_RECORD(__)) => begin
        types = TypeTreeImpl.add(types, scopePath(ty.cls), ty)
        ()
      end
      TYPE_COMPLEX(complexTy = COMPLEX_EXTERNAL_OBJECT(__)) =>  begin
        types = TypeTreeImpl.add(types, scopePath(ty.cls), ty)
      end
      _ => begin
        ()
      end
    end
  end
  return types
end

function collectVariableFlatTypes(var::Variable, types::TypeTree)::TypeTree
  types = collectFlatType(var.ty, types)
  types = collectBindingFlatTypes(var.binding, types)
  for attr in var.typeAttributes
    types = collectBindingFlatTypes(Util.tuple22(attr), types)
  end
  return types
end

function collectFlatTypes(flatModel::FlatModel, functions::List{<:M_Function})::TypeTree
  local types::TypeTree
  types = TypeTreeImpl.new()
  types = ListUtil.fold(flatModel.variables, collectVariableFlatTypes, types)
  types = ListUtil.fold(flatModel.equations, collectEquationFlatTypes, types)
  types = ListUtil.fold(flatModel.initialEquations, collectEquationFlatTypes, types)
  types = ListUtil.fold(flatModel.algorithms, collectAlgorithmFlatTypes, types)
  types =
    ListUtil.fold(flatModel.initialAlgorithms, collectAlgorithmFlatTypes, types)
  types = ListUtil.fold(functions, collectFunctionFlatTypes, types)
  println(TypeTreeImpl.printTreeStr(types))
  return types
end

function toFlatStream(flatModel::FlatModel,
  functions::List{<:M_Function},
  printBindingTypes::Bool = false,
  s = Nothing
)
  local str::String
  local flat_model::FlatModel = flatModel
  s = IOStream_M.append(s, "model '" + flat_model.name + "'\\n")
  @assign flat_model.variables = reconstructRecordInstances(flat_model.variables)
  for fn in functions
    if !isDefaultRecordConstructor(fn)
      s = toFlatStream(fn, s)
      s = IOStream_M.append(s, ";\\n\\n")
    end
  end
  for ty in TypeTreeImpl.listValues(collectFlatTypes(flat_model, functions))
    s = toFlatDeclarationStream(ty, s)
    s = IOStream_M.append(s, ";\\n\\n")
  end
  for v in flat_model.variables
    @assign s = toFlatStream(v, "  ", printBindingTypes, s)
    @assign s = IOStream_M.append(s, ";\\n")
  end
  if !listEmpty(flat_model.initialEquations)
    s = IOStream_M.append(s, "initial equation\\n")
    s = toFlatStreamList(flat_model.initialEquations, "  ", s)
  end
  if !listEmpty(flat_model.equations)
    s = IOStream_M.append(s, "equation\\n")
    s = toFlatStreamList(flat_model.equations, "  ", s)
  end
  for alg in flat_model.initialAlgorithms
    if !listEmpty(alg.statements)
      s = IOStream_M.append(s, "initial algorithm\\n")
      s = ALG_toFlatStreamList(alg.statements, "  ", s)
    end
  end
  for alg in flat_model.algorithms
    if !listEmpty(alg.statements)
      s = IOStream_M.append(s, "algorithm\\n")
      s = toFlatStreamList(alg.statements, "  ", s)
    end
  end
  s = IOStream_M.append(s, "end '" + flat_model.name + "';\\n")
  str = IOStream_M.string(s)
  IOStream_M.delete(s)
  return (s, str)
end

function printFlatString(
  flatModel::FlatModel,
  functions::List{<:M_Function},
  printBindingTypes::Bool = false,
)
  local s
  @assign s = IOStream_M.create(getInstanceName(), IOStream.IOStreamType.LIST())
  @assign s = toFlatStream(flatModel, functions, printBindingTypes, s)
  return IOStream_M.print(s, IOStream_M.stdOutput)
end

function toFlatString(
  flatModel::FlatModel,
  functions::List{<:M_Function},
  printBindingTypes::Bool = false,
)::String
  local str::String
  local s::IOStream_M.IOSTREAM
  s = IOStream_M.create(getInstanceName(), IOStream_M.LIST())
  (s, str) = toFlatStream(flatModel, functions, printBindingTypes, s)
  str = IOStream_M.string(s)
  return str
end

function toString(flatModel::FlatModel, printBindingTypes::Bool = false)::String
  local str::String
  local s::IOStream_M.IOSTREAM
  local modelName = replace(flatModel.name, "." => "_")
  s = IOStream_M.create(getInstanceName(), IOStream_M.LIST())
  s = IOStream_M.append(s, "class " + modelName + "\\n")
  for structuralMode in flatModel.structuralSubmodels
    structuralModelString = toString(structuralMode, printBindingTypes)
    s = IOStream_M.append(s, "structuralmode " * structuralModelString)
  end
  for v in flatModel.variables
    s = toStream(v, "  ", printBindingTypes, s)
    s = IOStream_M.append(s, ";\\n")
  end
  if !isempty(flatModel.initialEquations)
    s = IOStream_M.append(s, "initial equation\\n")
    s = toStreamList(flatModel.initialEquations, "  ", s)
  end
  if !isempty(flatModel.equations)
    s = IOStream_M.append(s, "equation\\n")
    s = toStreamList(flatModel.equations, "  ", s)
  end
  for alg in flatModel.initialAlgorithms
    if !isempty(alg.statements)
      s = IOStream_M.append(s, "initial algorithm\\n")
      s = toStreamList(alg.statements, "  ", s)
    end
  end
  for alg in flatModel.algorithms
    if !isempty(alg.statements)
      s = IOStream_M.append(s, "algorithm\\n")
      s = toStreamList(alg.statements, "  ", s)
    end
  end
  if !(flatModel.DOCC_equations isa Nil)
    doccs = flatModel.DOCC_equations
    for eq in doccs
      s = IOStream_M.append(s, "//dynamic overconstraint connector equation\\n")
      s = IOStream_M.append(s, toString(eq) * ";\\n")
    end
  end
  s = IOStream_M.append(s, "end " + modelName + ";\\n")
  str = IOStream_M.string(s)
  return str
end

"""
  This functions checks for a recompilation directive among all the equations.
  This function will also return true if the model contains another change that might
  lead to recompilation.
"""
function recompilationDirectiveExists(@nospecialize(eqs::Vector{Equation}))::Bool
  local hasRecompilationDirective = containsList(eqs, (eq) -> containsCallNamed(eq, "recompilation"))
  return hasRecompilationDirective
end


"""
  Returns true if the list of equations contains a branch directive.
"""
function branchDirectiveExists(@nospecialize(eqs::Vector{Equation}))::Bool
  local hasBranchDirective = containsList(eqs, (eq) -> containsCallNamed(eq, "Connections.branch"))
  return hasBranchDirective
end

"""
  Collect all DOCCS Equations.
"""
function collectDOCCS(@nospecialize(eqs::Vector{Equation}))
  Base.collect(Iterators.flatten([containsDOCC(eq) for eq in eqs]))
end

"""
  This function returns true if a EQUATION_NORETCALL is a func name.
"""
function containsCallNamed(@nospecialize(eq::Equation), funcName::String)::Bool
  local functionExistWithName = false
  @match eq begin
    EQUATION_NORETCALL(__) => begin
      @match eq.exp begin
        CALL_EXPRESSION(call = TYPED_CALL(fn, ty, var, arguments, attributes)) => begin
          local nameAsStr = AbsynUtil.pathString(name(fn))
          if funcName == nameAsStr
            functionExistWithName = true
          end
        end
      end
    end
    _ => begin
      functionExistWithName = false
    end
  end
  return functionExistWithName
end


"""
  Check if the model contains a Dynamically Overconstrained Connector (DOCC).
  returns a list with all if-equations containing DOCC.
"""
function containsDOCC(@nospecialize(eq::Equation))::Vector{Equation}
  local doccs = Equation[]
  @match eq begin
    EQUATION_IF(__) => begin
      for branch in eq.branches
        @assert branch isa EQUATION_BRANCH
        if branchDirectiveExists(branch.body)
          push!(doccs, eq)
        end
      end
    end
    _ => Equation[]
  end
  return doccs
end
