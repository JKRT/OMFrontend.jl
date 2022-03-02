struct FLAT_MODEL <: FlatModel
  name::String
  variables::List{Variable}
  equations::List{Equation}
  initialEquations::List{Equation}
  algorithms::List{Algorithm}
  initialAlgorithms::List{Algorithm}
  #= VSS Modelica extension =#
  structuralSubmodels::List{FlatModel}
  scodeProgram::Option{SCode.CLASS}
  #= End VSS Modelica extension =#
  comment::Option{SCode.Comment}
end

module TypeTreeImpl
using MetaModelica
using ExportAll
import Absyn
import ..BaseAvlTree
import ..Absyn.Path
import ..Type
import ..Main.M_Type
using ..BaseAvlTree #= Modelica extend clause =#
  const Key = Absyn.Path
  const Value = M_Type
  const addConflictDefault = addConflictKeep
@exportAll()
end

TypeTree = TypeTreeImpl.Tree

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
    @assign record_binding = EMPTY_BINDING
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
      if Type.isRecord(parent_ty)
        @assign field_count = listLength(Type.recordFields(parent_ty))
        @assign (record_vars, rest_vars) = ListUtil.split(rest_vars, field_count - 1)
        @assign record_vars = _cons(var, record_vars)
        @assign var = reconstructRecordInstance(parent_cr, record_vars)
      end
    end
    @assign outVariables = _cons(var, outVariables)
  end
  @assign outVariables = listReverseInPlace(outVariables)
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

  @assign exp_ty = typeOf(exp)
  @assign dims = ListUtil.firstN(arrayDims(exp_ty), listLength(subs))
  @assign sub_tyl = list(P_Dimension.Dimension.subscriptType(d) for d in dims)
  @assign name = Type.subscriptedTypeName(exp_ty, sub_tyl)
  @assign types = TypeTree.add(
    types,
    Absyn.IDENT(name),
    Type.SUBSCRIPTED(name, exp_ty, sub_tyl, subscriptedTy),
  )
  return types
end

function collectComponentFlatTypes(component::InstNode, types::TypeTree)::TypeTree
  local comp::Component
  @assign comp = component(component)
  @assign types = collectFlatType(getType(comp), types)
  @assign types = collectBindingFlatTypes(getBinding(comp), types)
  return types
end

function collectFunctionFlatTypes(fn::M_Function, types::TypeTree)::TypeTree
  local body::List{Statement}
  @assign types = foldComponents(
    classTree(getClass(fn.node)),
    collectComponentFlatTypes,
    types,
  )
  @assign body = P_Function.getBody(fn)
  @assign types = ListUtil.fold(body, collectStatementFlatTypes, types)
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
  @assign () = begin
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
  @assign () = begin
    @match branch begin
      P_Equation.P_Branch.Equation.BRANCH(__) => begin
        @assign types = collectExpFlatTypes(branch.condition, types)
        @assign types = ListUtil.fold(branch.body, collectEquationFlatTypes, types)
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
      Equation.EQUALITY(__) => begin
        @assign types = collectExpFlatTypes(eq.lhs, types)
        @assign types = collectExpFlatTypes(eq.rhs, types)
        @assign types = collectFlatType(eq.ty, types)
        ()
      end

      Equation.ARRAY_EQUALITY(__) => begin
        @assign types = collectExpFlatTypes(eq.lhs, types)
        @assign types = collectExpFlatTypes(eq.rhs, types)
        @assign types = collectFlatType(eq.ty, types)
        ()
      end

      Equation.FOR(__) => begin
        @assign types = ListUtil.fold(eq.body, collectEquationFlatTypes, types)
        ()
      end

      Equation.IF(__) => begin
        @assign types = ListUtil.fold(eq.branches, collectEqBranchFlatTypes, types)
        ()
      end

      Equation.WHEN(__) => begin
        @assign types = ListUtil.fold(eq.branches, collectEqBranchFlatTypes, types)
        ()
      end

      Equation.ASSERT(__) => begin
        @assign types = collectExpFlatTypes(eq.condition, types)
        @assign types = collectExpFlatTypes(eq.message, types)
        @assign types = collectExpFlatTypes(eq.level, types)
        ()
      end

      Equation.TERMINATE(__) => begin
        @assign types = collectExpFlatTypes(eq.message, types)
        ()
      end

      Equation.REINIT(__) => begin
        @assign types = collectExpFlatTypes(eq.reinitExp, types)
        ()
      end

      Equation.NORETCALL(__) => begin
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

  @assign () = begin
    @match ty begin
      TYPE_ENUMERATION(__) => begin
        @assign types = TypeTree.add(types, ty.typePath, ty)
        ()
      end
      TYPE_ARRAY(__) => begin
        @assign types = P_Dimension.Dimension.foldExpList(
          ty.dimensions,
          collectExpFlatTypes_traverse,
          types,
        )
        @assign types = collectFlatType(ty.elementType, types)
        ()
      end
      TYPE_COMPLEX(complexTy = COMPLEX_RECORD(__)) => begin
        @assign types = TypeTree.add(types, scopePath(ty.cls), ty)
        ()
      end
      _ => begin
        ()
      end
    end
  end
  return types
end

function collectVariableFlatTypes(var::Variable, types::TypeTree)::TypeTree
  @assign types = collectFlatType(var.ty, types)
  @assign types = collectBindingFlatTypes(var.binding, types)
  for attr in var.typeAttributes
    @assign types = collectBindingFlatTypes(Util.tuple22(attr), types)
  end
  return types
end

function collectFlatTypes(flatModel::FlatModel, functions::List{<:M_Function})::TypeTree
  local types::TypeTree
  @assign types = TypeTree.new()
  @assign types = ListUtil.fold(flatModel.variables, collectVariableFlatTypes, types)
  @assign types = ListUtil.fold(flatModel.equations, collectEquationFlatTypes, types)
  @assign types = ListUtil.fold(flatModel.initialEquations, collectEquationFlatTypes, types)
  @assign types = ListUtil.fold(flatModel.algorithms, collectAlgorithmFlatTypes, types)
  @assign types =
    ListUtil.fold(flatModel.initialAlgorithms, collectAlgorithmFlatTypes, types)
  @assign types = ListUtil.fold(functions, collectFunctionFlatTypes, types)
  return types
end

function toFlatStream(
  flatModel::FlatModel,
  functions::List{<:M_Function},
  printBindingTypes::Bool = false,
  s = Nothing
)::Tuple
  local str::String
  local flat_model::FlatModel = flatModel
  @assign flat_model.variables = reconstructRecordInstances(flat_model.variables)
  for fn in functions
    if !P_Function.isDefaultRecordConstructor(fn)
      @assign s = P_Function.toFlatStream(fn, s)
      @assign s = IOStream_M.append(s, ";\\n\\n")
    end
  end
  for ty in TypeTree.listValues(collectFlatTypes(flat_model, functions))
    @assign s = Type.toFlatDeclarationStream(ty, s)
    @assign s = IOStream_M.append(s, ";\\n\\n")
  end
  @assign s = IOStream_M.append(s, "class '" + flat_model.name + "'\\n")
  for v in flat_model.variables
    @assign s = P_Variable.Variable.toFlatStream(v, "  ", printBindingTypes, s)
    @assign s = IOStream_M.append(s, ";\\n")
  end
  if !listEmpty(flat_model.initialEquations)
    @assign s = IOStream_M.append(s, "initial equation\\n")
    @assign s = Equation.toFlatStreamList(flat_model.initialEquations, "  ", s)
  end
  if !listEmpty(flat_model.equations)
    @assign s = IOStream_M.append(s, "equation\\n")
    @assign s = Equation.toFlatStreamList(flat_model.equations, "  ", s)
  end
  for alg in flat_model.initialAlgorithms
    if !listEmpty(alg.statements)
      @assign s = IOStream_M.append(s, "initial algorithm\\n")
      @assign s = ALG_toFlatStreamList(alg.statements, "  ", s)
    end
  end
  for alg in flat_model.algorithms
    if !listEmpty(alg.statements)
      @assign s = IOStream_M.append(s, "algorithm\\n")
      @assign s = ALG_toFlatStreamList(alg.statements, "  ", s)
    end
  end
  @assign s = IOStream_M.append(s, "end '" + flat_model.name + "';\\n")
  @assign str = IOStream_M.string(s)
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
  local s::IOStream.IOStream
  @assign s = IOStream.create(getInstanceName(), IOStream.IOStreamType.LIST())
  @assign s = toFlatStream(flatModel, functions, printBindingTypes, s)
  @assign str = IOStream_M.string(s)
  return str
end

function toString(flatModel::FlatModel, printBindingTypes::Bool = false)::String
  local str::String
  local s::IOStream_M.IOSTREAM
  s = IOStream_M.create(getInstanceName(), IOStream_M.LIST())
  s = IOStream_M.append(s, "class " + flatModel.name + "\\n")
  for structuralMode in flatModel.structuralSubmodels
    structuralModelString = toString(structuralMode, printBindingTypes)
    s = IOStream_M.append(s, "structuralmode " * structuralModelString)
  end
  for v in flatModel.variables
    s = toStream(v, "  ", printBindingTypes, s)
    s = IOStream_M.append(s, ";\\n")
  end
  if !listEmpty(flatModel.initialEquations)
    s = IOStream_M.append(s, "initial equation\\n")
    s = toStreamList(flatModel.initialEquations, "  ", s)
  end
  if !listEmpty(flatModel.equations)
    s = IOStream_M.append(s, "equation\\n")
    s = toStreamList(flatModel.equations, "  ", s)
  end
  for alg in flatModel.initialAlgorithms
    if !listEmpty(alg.statements)
      s = IOStream_M.append(s, "initial algorithm\\n")
      s = ALG_toStreamList(alg.statements, "  ", s)
    end
  end
  for alg in flatModel.algorithms
    if !listEmpty(alg.statements)
      s = IOStream_M.append(s, "algorithm\\n")
      s = ALG_toStreamList(alg.statements, "  ", s)
    end
  end
  s = IOStream_M.append(s, "end " + flatModel.name + ";\\n")
  str = IOStream_M.string(s)
  return str
end

"""
  This functions checks for the recompilation directive among all the equations 
"""
function recompilationDirectiveExists(@nospecialize(eqs::List{Equation}))::Bool
  local hasRecompilationDirective = containsList(eqs, containsRecompilation)
  return hasRecompilationDirective
end

"""
  This function returns true if a EQUATION_NORETCALL is a recompilation directive. 
"""
function containsRecompilation(@nospecialize(eq::Equation))::Bool
  local recompilationDirectiveExists = false
  @match eq begin
    EQUATION_NORETCALL(__) => begin
      @debug "We have a call expression: " * toString(eq.exp) * " of type: $(typeof(eq.exp))" 
      @match eq.exp begin
        CALL_EXPRESSION(call = TYPED_CALL(fn, ty, var, arguments, attributes)) => begin
          @debug "Matched!" name(fn)
          local nameAsStr = AbsynUtil.pathString(name(fn))
          @debug nameAsStr
          if "recompilation" == nameAsStr
            @debug "matched on recompilation"
            recompilationDirectiveExists = true
          end
        end
      end
    end
    _ => begin
      recompilationDirectiveExists = false
    end
  end
  @debug "Returning:" recompilationDirectiveExists
  return recompilationDirectiveExists
end

