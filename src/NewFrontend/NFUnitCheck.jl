@Uniontype Functionargs begin
  @Record FUNCTIONUNITS begin
    name::String
    invars::List{String}
    outvars::List{String}
    inunits::List{String}
    outunits::List{String}
  end
end

module FunctionUnitCache

import ..Main. Functionargs

using MetaModelica
using ExportAll
#= Forward declarations for uniontypes until Julia adds support for mutual recursion =#
FuncHash = Function
FuncEq = Function
FuncKeyStr = Function
FuncValueStr = Function
Key = String
Value = Functionargs
Cache = Tuple
function dummyPrint(args::Functionargs)::String
  local res::String = ""
  return res
end
function emptyCache(size::Int)::Cache
  local table::Cache
  @assign table = BaseHashTable.emptyHashTableWork(
    size,
    (stringHashDjb2Mod, stringEq, Util.id, dummyPrint),
  )
  return table
end
@exportAll()
end

function checkUnits(flatModel::FlatModel)::FlatModel
  local htCr2U1::HashTableCrToUnit.HashTable
  local htCr2U2::HashTableCrToUnit.HashTable
  local htS2U::HashTableStringToUnit.HashTable
  local htU2S::HashTableUnitToString.HashTable
  local fn_cache::FunctionUnitCache.Cache
  if !(Flags.isSet(Flags.NF_UNITCHECK) || Flags.getConfigBool(Flags.CHECK_MODEL))
    return flatModel
  end
  try
    @assign htCr2U1 = HashTableCrToUnit.emptyHashTableSized(Util.nextPrime(integer(
      10 + 1.4 * listLength(flatModel.variables),
    )))
    @assign htS2U = Unit.getKnownUnits()
    @assign htU2S = Unit.getKnownUnitsInverse()
    @assign fn_cache = FunctionUnitCache.emptyCache(BaseHashTable.defaultBucketSize)
    for v in flatModel.variables
      @assign (htCr2U1, htS2U, htU2S) = convertUnitString2unit(v, htCr2U1, htS2U, htU2S)
    end
    @assign htCr2U2 = BaseHashTable.copy(htCr2U1)
    @assign (htCr2U2, htS2U, htU2S) = checkModelConsistency(
      flatModel.variables,
      flatModel.equations,
      flatModel.initialEquations,
      htCr2U2,
      htS2U,
      htU2S,
      fn_cache,
    )
    if Flags.isSet(Flags.DUMP_UNIT)
      BaseHashTable.dumpHashTable(htCr2U2)
      print("######## UnitCheck COMPLETED ########\\n")
    end
    notification(htCr2U1, htCr2U2, htU2S)
    @assign flatModel = updateModel(flatModel, htCr2U2, htU2S)
  catch
    Error.addInternalError(getInstanceName() + ": unit check module failed", sourceInfo())
  end
#  execStat(getInstanceName()) TODO
  return flatModel
end

""" #= Updates all variables without units with their calculated units. =#"""
function updateModel(
  flatModel::FlatModel,
  htCr2U::HashTableCrToUnit.HashTable,
  htU2S::HashTableUnitToString.HashTable,
)::FlatModel

  @assign flatModel.variables =
    list(updateVariable(v, htCr2U, htU2S) for v in flatModel.variables)
  return flatModel
end

""" #= Updates a variable without unit with its calculated unit. =#"""
function updateVariable(
  var::Variable,
  htCr2U::HashTableCrToUnit.HashTable,
  htU2S::HashTableUnitToString.HashTable,
)::Variable

  local name::String
  local unit_str::String
  local binding::Binding
  local unit_idx::Int = 0
  local unit::Unit.Unit

  if isReal(var.ty)
    for attr in var.typeAttributes
      @assign (name, binding) = attr
      @assign unit_idx = unit_idx + 1
      if name == "unit"
        if isBound(binding)
          return var
        else
          @assign var.typeAttributes = listDelete(var.typeAttributes, unit_idx)
          break
        end
      end
    end
    try
      @assign unit = BaseHashTable.get(var.name, htCr2U)
      if Unit.isUnit(unit)
        @assign unit_str = Unit.unitString(unit, htU2S)
        @assign binding = FLAT_BINDING(
          STRING_EXPRESSION(unit_str),
          Variability.CONSTANT,
        )
        @assign var.typeAttributes = _cons(("unit", binding), var.typeAttributes)
      end
    catch
    end
  end
  #=  Variable already has a unit, keep it.
  =#
  #=  Variable has an empty unit, replace it.
  =#
  #=  Look up the variable's unit in the table.
  =#
  #=  Add the unit string to the variable's type attributes.
  =#
  return var
end

""" #= dumps the calculated units =#"""
function notification(
  inHtCr2U1::HashTableCrToUnit.HashTable,
  inHtCr2U2::HashTableCrToUnit.HashTable,
  inHtU2S::HashTableUnitToString.HashTable,
)
  local str::String
  local lt1::List{Tuple{ComponentRef, Unit.Unit}}

  @assign lt1 = BaseHashTable.hashTableList(inHtCr2U1)
  @assign str = notification2(lt1, inHtCr2U2, inHtU2S)
  return if Flags.isSet(Flags.DUMP_UNIT) && str != ""
    Error.addCompilerNotification(str)
  end
end

""" #= help-function =#"""
function notification2(
  inLt1::List{<:Tuple{<:ComponentRef, Unit.Unit}},
  inHtCr2U2::HashTableCrToUnit.HashTable,
  inHtU2S::HashTableUnitToString.HashTable,
)::String
  local outS::String

  local cr1::ComponentRef = EMPTY()
  local factor1::AbstractFloat = 0
  local i1::Int = 0
  local i2::Int = 0
  local i3::Int = 0
  local i4::Int = 0
  local i5::Int = 0
  local i6::Int = 0
  local i7::Int = 0

  @assign outS = stringAppendList(List(
    "\\" +
    toString(cr1) +
    "\\ has the Unit \\" +
    Unit.unitString(Unit.UNIT(factor1, i1, i2, i3, i4, i5, i6, i7), inHtU2S) +
    "\\\\n"
    for
    t1 in inLt1 if
    begin
      #=  We already assigned the variables before
      =#
      #=  Do the filtering and unboxing stuff at the same time; then we only need one hashtable call
      =#
      #=  And we only use a try-block for MASTER nodes
      =#
      local b::Bool
      @match t1 begin
        (cr1, Unit.MASTER(__)) => begin
          @assign b = false
          try
            @match Unit.UNIT(factor1, i1, i2, i3, i4, i5, i6, i7) = BaseHashTable.get(
              stripSubscripts(cr1),
              inHtCr2U2,
            )
            @assign b = true
          catch
          end
          b
        end

        _ => begin
          false
        end
      end
    end
  ))
  return outS
end

function checkModelConsistency(
  variables::List{<:Variable},
  equations::List{<:Equation},
  initialEquations::List{<:Equation},
  htCr2U::HashTableCrToUnit.HashTable,
  htS2U::HashTableStringToUnit.HashTable,
  htU2S::HashTableUnitToString.HashTable,
  fnCache::FunctionUnitCache.Cache,
)::Tuple{
  HashTableCrToUnit.HashTable,
  HashTableStringToUnit.HashTable,
  HashTableUnitToString.HashTable,
  FunctionUnitCache.Cache,
}

  local dump_eq_unit::Bool = Flags.isSet(Flags.DUMP_EQ_UNIT_STRUCT)

  for v in variables
    @assign (htCr2U, htS2U, htU2S, fnCache) =
      foldBindingExp(v, htCr2U, htS2U, htU2S, fnCache, dump_eq_unit)
  end
  for eq in equations
    @assign (htCr2U, htS2U, htU2S, fnCache) =
      foldEquation(eq, htCr2U, htS2U, htU2S, fnCache, dump_eq_unit)
  end
  for ieq in initialEquations
    @assign (htCr2U, htS2U, htU2S, fnCache) =
      foldEquation(ieq, htCr2U, htS2U, htU2S, fnCache, dump_eq_unit)
  end
  return (htCr2U, htS2U, htU2S, fnCache)
end

function foldBindingExp(
  var::Variable,
  htCr2U::HashTableCrToUnit.HashTable,
  htS2U::HashTableStringToUnit.HashTable,
  htU2S::HashTableUnitToString.HashTable,
  fnCache::FunctionUnitCache.Cache,
  dumpEqInitStruct::Bool,
)::Tuple{
  HashTableCrToUnit.HashTable,
  HashTableStringToUnit.HashTable,
  HashTableUnitToString.HashTable,
  FunctionUnitCache.Cache,
}

  local binding_exp::Expression
  local eq::Equation

  if isReal(var.ty) && isBound(var.binding)
    @assign binding_exp = getTypedExp(var.binding)
    @assign eq = P_Equation.Equation.makeEquality(
      fromCref(var.name),
      binding_exp,
      var.ty,
      ElementSource_createElementSource(var.info),
    )
    @assign (htCr2U, htS2U, htU2S, fnCache) =
      foldEquation(eq, htCr2U, htS2U, htU2S, fnCache, dumpEqInitStruct)
  end
  return (htCr2U, htS2U, htU2S, fnCache)
end

""" #= Folds the equation or returns the error message of inconsistent equations. =#"""
function foldEquation(
  eq::Equation,
  htCr2U::HashTableCrToUnit.HashTable,
  htS2U::HashTableStringToUnit.HashTable,
  htU2S::HashTableUnitToString.HashTable,
  fnCache::FunctionUnitCache.Cache,
  dumpEqInitStruct::Bool,
)::Tuple{
  HashTableCrToUnit.HashTable,
  HashTableStringToUnit.HashTable,
  HashTableUnitToString.HashTable,
  FunctionUnitCache.Cache,
}

  local inconsistent_units::List{List{Tuple{Expression, Unit.Unit}}}

  @assign (htCr2U, htS2U, htU2S, fnCache, inconsistent_units) =
    foldEquation2(eq, dumpEqInitStruct, htCr2U, htS2U, htU2S, fnCache)
  for u in inconsistent_units
    Errorfunction(u, eq, htU2S)
  end
  return (htCr2U, htS2U, htU2S, fnCache)
end

""" #= help function to foldEquation =#"""
function foldEquation2(
  eq::Equation,
  dumpEqInitStruct::Bool,
  htCr2U::HashTableCrToUnit.HashTable,
  htS2U::HashTableStringToUnit.HashTable,
  htU2S::HashTableUnitToString.HashTable,
  fnCache::FunctionUnitCache.Cache,
)::Tuple{
  HashTableCrToUnit.HashTable,
  HashTableStringToUnit.HashTable,
  HashTableUnitToString.HashTable,
  FunctionUnitCache.Cache,
  List{List{Tuple{Expression, Unit.Unit}}},
}
  local inconsistentUnits::List{List{Tuple{Expression, Unit.Unit}}}

  @assign inconsistentUnits = begin
    local icu1::List{List{Tuple{Expression, Unit.Unit}}}
    local icu2::List{List{Tuple{Expression, Unit.Unit}}}
    local lhs::Expression
    local rhs::Expression
    local temp::Expression
    local fn_name::String
    local formal_args::String
    local formal_var::String
    local out_vars::List{String}
    local out_units::List{String}
    local unit1::Unit.Unit
    local unit2::Unit.Unit
    local eql::List{Equation}
    local b::Bool
    @match eq begin
      EQUATION_EQUALITY(
        lhs = lhs && TUPLE_EXPRESSION(__),
        rhs = rhs && CALL_EXPRESSION(__),
      ) where {(!isBuiltin(P_Call.typedFunction(rhs.call)))} => begin
        @assign fn_name =
          AbsynUtil.pathString(AbsynUtil.makeNotFullyQualified(P_Call.functionName(rhs.call)))
        @assign (_, out_vars, _, out_units) = getCallUnits(fn_name, rhs.call, fnCache)
        @assign (htCr2U, htS2U, htU2S, fnCache, icu1) = foldCallArg1(
          lhs.elements,
          htCr2U,
          htS2U,
          htU2S,
          fnCache,
          Unit.MASTER(nil),
          out_units,
          out_vars,
          fn_name,
        )
        @assign (_, htCr2U, htS2U, htU2S, fnCache, icu2) =
          insertUnitInEquation(rhs, Unit.MASTER(nil), htCr2U, htS2U, htU2S, fnCache)
        ListUtil.append_reverse(icu1, icu2)
      end

      EQUATION_EQUALITY(
        rhs = rhs && CALL_EXPRESSION(__),
      ) where {(!isBuiltin(P_Call.typedFunction(rhs.call)))} => begin
        @assign fn_name =
          AbsynUtil.pathString(AbsynUtil.makeNotFullyQualified(P_Call.functionName(rhs.call)))
        @assign (_, out_vars, _, out_units, fnCache) =
          getCallUnits(fn_name, rhs.call, fnCache)
        @assign (unit1, htCr2U, htS2U, htU2S, fnCache, _) =
          insertUnitInEquation(eq.lhs, Unit.MASTER(nil), htCr2U, htS2U, htU2S, fnCache)
        @assign formal_args = listHead(out_units)
        @assign formal_var = listHead(out_vars)
        @assign unit2 = if formal_args == "NONE"
          Unit.MASTER(nil)
        else
          Unit.parseUnitString(formal_args, htS2U)
        end
        @assign b = unitTypesEqual(unit1, unit2, htCr2U)
        if b
          @assign icu1 = nil
        else
          @assign icu1 =
            list(list((eq.lhs, unit1), (makeNewCref(formal_var, fn_name), unit2)))
        end
        @assign (_, htCr2U, htS2U, htU2S, fnCache, icu2) =
          insertUnitInEquation(rhs, Unit.MASTER(nil), htCr2U, htS2U, htU2S, fnCache)
        ListUtil.append_reverse(icu1, icu2)
      end

      EQUATION_EQUALITY(__) => begin
        @assign temp = BINARY_EXPRESSION(
          eq.rhs,
          makeSub(TYPE_REAL()),
          eq.lhs,
        )
        if dumpEqInitStruct
          ExpressionDump.dumpExp(toDAE(temp))
        end
        @assign (_, htCr2U, htS2U, htU2S, fnCache, inconsistentUnits) =
          insertUnitInEquation(temp, Unit.MASTER(nil), htCr2U, htS2U, htU2S, fnCache)
        inconsistentUnits
      end

      EQUATION_ARRAY_EQUALITY(__) => begin
        @assign temp = BINARY_EXPRESSION(
          eq.rhs,
          makeSub(TYPE_REAL()),
          eq.lhs,
        )
        if dumpEqInitStruct
          ExpressionDump.dumpExp(toDAE(temp))
        end
        @assign (_, htCr2U, htS2U, htU2S, fnCache, inconsistentUnits) =
          insertUnitInEquation(temp, Unit.MASTER(nil), htCr2U, htS2U, htU2S, fnCache)
        inconsistentUnits
      end

      EQUATION_WHEN(
        branches = EQUATION_BRANCH(body = eql) <| _,
      ) => begin
        @assign inconsistentUnits = nil
        for e in eql
          @assign (htCr2U, htS2U, htU2S, fnCache, icu1) =
            foldEquation2(e, dumpEqInitStruct, htCr2U, htS2U, htU2S, fnCache)
          @assign inconsistentUnits = ListUtil.append_reverse(icu1, inconsistentUnits)
        end
        inconsistentUnits
      end

      EQUATION_NORETCALL(__) => begin
        @assign (_, htCr2U, htS2U, htU2S, fnCache, inconsistentUnits) =
          insertUnitInEquation(eq.exp, Unit.MASTER(nil), htCr2U, htS2U, htU2S, fnCache)
        inconsistentUnits
      end

      _ => begin
        nil
      end
    end
  end
  return (htCr2U, htS2U, htU2S, fnCache, inconsistentUnits)
end

function makeNewCref(paramName::String, fnName::String)::Expression
  local outExp::Expression

  @assign outExp = CREF_EXPRESSION(
    TYPE_UNKNOWN(),
    STRING(
      paramName,
      STRING(
        fnName + "()",
        EMPTY(),
      ),
    ),
  )
  return outExp
end

""" #= Inserts the units in the equation and checks if the equation is consistent or not. =#"""
function insertUnitInEquation(
  eq::Expression,
  unit::Unit.Unit,
  htCr2U::HashTableCrToUnit.HashTable,
  htS2U::HashTableStringToUnit.HashTable,
  htU2S::HashTableUnitToString.HashTable,
  fnCache::FunctionUnitCache.Cache,
)::Tuple{
  Unit.Unit,
  HashTableCrToUnit.HashTable,
  HashTableStringToUnit.HashTable,
  HashTableUnitToString.HashTable,
  FunctionUnitCache.Cache,
  List{List{Tuple{Expression, Unit.Unit}}},
}
  local inconsistentUnits::List{List{Tuple{Expression, Unit.Unit}}}

  import ..P_NFOperator.Op

  @assign (unit, inconsistentUnits) = begin
    local exp1::Expression
    local exp2::Expression
    local unit1::Unit.Unit
    local unit2::Unit.Unit
    local op_unit::Unit.Unit
    local icu1::List{List{Tuple{Expression, Unit.Unit}}}
    local icu2::List{List{Tuple{Expression, Unit.Unit}}}
    local vars::List{ComponentRef}
    local i::Int
    local b::Bool
    #=  SUB equal summands
    =#
    @matchcontinue eq begin
      BINARY_EXPRESSION(exp1, OPERATOR(op = Op.SUB), exp2) => begin
        @match ((@match Unit.UNIT() = unit1), htCr2U, htS2U, htU2S, fnCache, icu1) =
          insertUnitInEquation(exp1, unit, htCr2U, htS2U, htU2S, fnCache)
        @assign (unit2, htCr2U, htS2U, htU2S, fnCache, icu2) =
          insertUnitInEquation(exp2, unit1, htCr2U, htS2U, htU2S, fnCache)
        @match (true, op_unit, htCr2U) = unitTypesEqual(unit1, unit2, htCr2U)
        (op_unit, ListUtil.append_reverse(icu1, icu2))
      end

      BINARY_EXPRESSION(
        exp1,
        OPERATOR(op = Op.SUB),
        exp2,
      ) => begin
        #=  SUB equal summands
        =#
        @assign (unit2, htCr2U, htS2U, htU2S, fnCache, icu1) =
          insertUnitInEquation(exp2, unit, htCr2U, htS2U, htU2S, fnCache)
        @assign (unit1, htCr2U, htS2U, htU2S, fnCache, icu2) =
          insertUnitInEquation(exp1, unit2, htCr2U, htS2U, htU2S, fnCache)
        @match (true, op_unit, htCr2U) = unitTypesEqual(unit1, unit2, htCr2U)
        (op_unit, ListUtil.append_reverse(icu1, icu2))
      end

      BINARY_EXPRESSION(
        exp1,
        OPERATOR(op = Op.SUB),
        exp2,
      ) => begin
        #=  SUB unequal summands
        =#
        @match ((@match Unit.UNIT() = unit1), htCr2U, htS2U, htU2S, fnCache, icu1) =
          insertUnitInEquation(exp1, unit, htCr2U, htS2U, htU2S, fnCache)
        @assign (unit2, htCr2U, htS2U, htU2S, fnCache, icu2) =
          insertUnitInEquation(exp2, unit1, htCr2U, htS2U, htU2S, fnCache)
        @match (false, _, _) = unitTypesEqual(unit1, unit2, htCr2U)
        (
          Unit.MASTER(nil),
          _cons(list((exp1, unit1), (exp2, unit2)), ListUtil.append_reverse(icu1, icu2)),
        )
      end

      BINARY_EXPRESSION(
        exp1,
        OPERATOR(op = Op.SUB),
        exp2,
      ) => begin
        #=  SUB unequal summands
        =#
        @assign (unit2, htCr2U, htS2U, htU2S, fnCache, icu1) =
          insertUnitInEquation(exp2, unit, htCr2U, htS2U, htU2S, fnCache)
        @assign (unit1, htCr2U, htS2U, htU2S, fnCache, icu2) =
          insertUnitInEquation(exp1, unit2, htCr2U, htS2U, htU2S, fnCache)
        @match (false, _, _) = unitTypesEqual(unit1, unit2, htCr2U)
        (
          Unit.MASTER(nil),
          _cons(list((exp1, unit1), (exp2, unit2)), ListUtil.append_reverse(icu1, icu2)),
        )
      end

      BINARY_EXPRESSION(
        exp1,
        OPERATOR(op = Op.ADD),
        exp2,
      ) => begin
        #=  ADD equal summands
        =#
        @match ((@match Unit.UNIT() = unit1), htCr2U, htS2U, htU2S, fnCache, icu1) =
          insertUnitInEquation(exp1, unit, htCr2U, htS2U, htU2S, fnCache)
        @assign (unit2, htCr2U, htS2U, htU2S, fnCache, icu2) =
          insertUnitInEquation(exp2, unit1, htCr2U, htS2U, htU2S, fnCache)
        @match (true, op_unit, htCr2U) = unitTypesEqual(unit1, unit2, htCr2U)
        (op_unit, ListUtil.append_reverse(icu1, icu2))
      end

      BINARY_EXPRESSION(
        exp1,
        OPERATOR(op = Op.ADD),
        exp2,
      ) => begin
        #=  ADD equal summands
        =#
        @assign (unit2, htCr2U, htS2U, htU2S, fnCache, icu1) =
          insertUnitInEquation(exp2, unit, htCr2U, htS2U, htU2S, fnCache)
        @assign (unit1, htCr2U, htS2U, htU2S, fnCache, icu2) =
          insertUnitInEquation(exp1, unit2, htCr2U, htS2U, htU2S, fnCache)
        @match (true, op_unit, htCr2U) = unitTypesEqual(unit1, unit2, htCr2U)
        (op_unit, ListUtil.append_reverse(icu1, icu2))
      end

      BINARY_EXPRESSION(
        exp1,
        OPERATOR(op = Op.ADD),
        exp2,
      ) => begin
        #=  ADD unequal summands
        =#
        @match ((@match Unit.UNIT() = unit1), htCr2U, htS2U, htU2S, fnCache, icu1) =
          insertUnitInEquation(exp1, unit, htCr2U, htS2U, htU2S, fnCache)
        @assign (unit2, htCr2U, htS2U, htU2S, fnCache, icu2) =
          insertUnitInEquation(exp2, unit1, htCr2U, htS2U, htU2S, fnCache)
        @match (false, _, _) = unitTypesEqual(unit1, unit2, htCr2U)
        (
          Unit.MASTER(nil),
          _cons(list((exp1, unit1), (exp2, unit2)), ListUtil.append_reverse(icu1, icu2)),
        )
      end

      BINARY_EXPRESSION(
        exp1,
        OPERATOR(op = Op.ADD),
        exp2,
      ) => begin
        #=  ADD unequal summands
        =#
        @assign (unit2, htCr2U, htS2U, htU2S, fnCache, icu1) =
          insertUnitInEquation(exp2, unit, htCr2U, htS2U, htU2S, fnCache)
        @assign (unit1, htCr2U, htS2U, htU2S, fnCache, icu2) =
          insertUnitInEquation(exp1, unit2, htCr2U, htS2U, htU2S, fnCache)
        @match (false, _, _) = unitTypesEqual(unit1, unit2, htCr2U)
        (
          Unit.MASTER(nil),
          _cons(list((exp1, unit1), (exp2, unit2)), ListUtil.append_reverse(icu1, icu2)),
        )
      end

      BINARY_EXPRESSION(
        exp1,
        OPERATOR(op = Op.MUL),
        exp2,
      ) => begin
        #=  MUL
        =#
        @match ((@match Unit.UNIT() = unit1), htCr2U, htS2U, htU2S, fnCache, icu1) =
          insertUnitInEquation(exp1, Unit.MASTER(nil), htCr2U, htS2U, htU2S, fnCache)
        @match ((@match Unit.UNIT() = unit2), htCr2U, htS2U, htU2S, fnCache, icu2) =
          insertUnitInEquation(exp2, Unit.MASTER(nil), htCr2U, htS2U, htU2S, fnCache)
        @assign op_unit = Unit.unitMul(unit1, unit2)
        @assign (htS2U, htU2S) = insertUnitString(op_unit, htS2U, htU2S)
        (op_unit, ListUtil.append_reverse(icu1, icu2))
      end

      BINARY_EXPRESSION(
        exp1,
        OPERATOR(op = Op.MUL),
        exp2,
      ) where {(Unit.isMaster(unit))} => begin
        @match ((@match Unit.MASTER() = unit1), htCr2U, htS2U, htU2S, fnCache, icu1) =
          insertUnitInEquation(exp1, Unit.MASTER(nil), htCr2U, htS2U, htU2S, fnCache)
        @match ((@match Unit.UNIT() = unit2), htCr2U, htS2U, htU2S, fnCache, icu2) =
          insertUnitInEquation(exp2, Unit.MASTER(nil), htCr2U, htS2U, htU2S, fnCache)
        (Unit.MASTER(nil), ListUtil.append_reverse(icu1, icu2))
      end

      BINARY_EXPRESSION(
        exp1,
        OPERATOR(op = Op.MUL),
        exp2,
      ) where {(Unit.isUnit(unit))} => begin
        @match (Unit.MASTER(varList = vars), htCr2U, htS2U, htU2S, fnCache, icu1) =
          insertUnitInEquation(exp1, Unit.MASTER(nil), htCr2U, htS2U, htU2S, fnCache)
        @match ((@match Unit.UNIT() = unit2), htCr2U, htS2U, htU2S, fnCache, icu2) =
          insertUnitInEquation(exp2, Unit.MASTER(nil), htCr2U, htS2U, htU2S, fnCache)
        @assign op_unit = Unit.unitDiv(unit, unit2)
        @assign htCr2U = ListUtil.fold1(vars, updateHtCr2U, op_unit, htCr2U)
        @assign (htS2U, htU2S) = insertUnitString(op_unit, htS2U, htU2S)
        (unit, ListUtil.append_reverse(icu1, icu2))
      end

      BINARY_EXPRESSION(
        exp1,
        OPERATOR(op = Op.MUL),
        exp2,
      ) where {(Unit.isMaster(unit))} => begin
        @match (Unit.UNIT(), htCr2U, htS2U, htU2S, fnCache, icu1) =
          insertUnitInEquation(exp1, Unit.MASTER(nil), htCr2U, htS2U, htU2S, fnCache)
        @match (Unit.MASTER(), htCr2U, htS2U, htU2S, fnCache, icu2) =
          insertUnitInEquation(exp2, Unit.MASTER(nil), htCr2U, htS2U, htU2S, fnCache)
        (Unit.MASTER(nil), ListUtil.append_reverse(icu1, icu2))
      end

      BINARY_EXPRESSION(
        exp1,
        OPERATOR(op = Op.MUL),
        exp2,
      ) where {(Unit.isUnit(unit))} => begin
        @match ((@match Unit.UNIT() = unit2), htCr2U, htS2U, htU2S, fnCache, icu1) =
          insertUnitInEquation(exp1, Unit.MASTER(nil), htCr2U, htS2U, htU2S, fnCache)
        @match (Unit.MASTER(varList = vars), htCr2U, htS2U, htU2S, fnCache, icu2) =
          insertUnitInEquation(exp2, Unit.MASTER(nil), htCr2U, htS2U, htU2S, fnCache)
        @assign op_unit = Unit.unitDiv(unit, unit2)
        @assign htCr2U = ListUtil.fold1(vars, updateHtCr2U, op_unit, htCr2U)
        @assign (htS2U, htU2S) = insertUnitString(op_unit, htS2U, htU2S)
        (unit, ListUtil.append_reverse(icu1, icu2))
      end

      BINARY_EXPRESSION(
        exp1,
        OPERATOR(op = Op.MUL),
        exp2,
      ) => begin
        @match (Unit.MASTER(), htCr2U, htS2U, htU2S, fnCache, icu1) =
          insertUnitInEquation(exp1, Unit.MASTER(nil), htCr2U, htS2U, htU2S, fnCache)
        @match (Unit.MASTER(), htCr2U, htS2U, htU2S, fnCache, icu2) =
          insertUnitInEquation(exp2, Unit.MASTER(nil), htCr2U, htS2U, htU2S, fnCache)
        (Unit.MASTER(nil), ListUtil.append_reverse(icu1, icu2))
      end

      BINARY_EXPRESSION(
        exp1,
        OPERATOR(op = Op.DIV),
        exp2,
      ) => begin
        #=  DIV
        =#
        @match ((@match Unit.UNIT() = unit1), htCr2U, htS2U, htU2S, fnCache, icu1) =
          insertUnitInEquation(exp1, Unit.MASTER(nil), htCr2U, htS2U, htU2S, fnCache)
        @match ((@match Unit.UNIT() = unit2), htCr2U, htS2U, htU2S, fnCache, icu2) =
          insertUnitInEquation(exp2, Unit.MASTER(nil), htCr2U, htS2U, htU2S, fnCache)
        @assign op_unit = Unit.unitDiv(unit1, unit2)
        @assign (htS2U, htU2S) = insertUnitString(op_unit, htS2U, htU2S)
        (op_unit, ListUtil.append_reverse(icu1, icu2))
      end

      BINARY_EXPRESSION(
        exp1,
        OPERATOR(op = Op.DIV),
        exp2,
      ) where {(Unit.isMaster(unit))} => begin
        @match (Unit.MASTER(), htCr2U, htS2U, htU2S, fnCache, icu1) =
          insertUnitInEquation(exp1, Unit.MASTER(nil), htCr2U, htS2U, htU2S, fnCache)
        @match (Unit.UNIT(), htCr2U, htS2U, htU2S, fnCache, icu2) =
          insertUnitInEquation(exp2, Unit.MASTER(nil), htCr2U, htS2U, htU2S, fnCache)
        @assign inconsistentUnits = ListUtil.append_reverse(icu1, icu2)
        (Unit.MASTER(nil), ListUtil.append_reverse(icu1, icu2))
      end

      BINARY_EXPRESSION(
        exp1,
        OPERATOR(op = Op.DIV),
        exp2,
      ) where {(Unit.isUnit(unit))} => begin
        @match (Unit.MASTER(varList = vars), htCr2U, htS2U, htU2S, fnCache, icu1) =
          insertUnitInEquation(exp1, Unit.MASTER(nil), htCr2U, htS2U, htU2S, fnCache)
        @match ((@match Unit.UNIT() = unit2), htCr2U, htS2U, htU2S, fnCache, icu2) =
          insertUnitInEquation(exp2, Unit.MASTER(nil), htCr2U, htS2U, htU2S, fnCache)
        @assign op_unit = Unit.unitMul(unit, unit2)
        @assign htCr2U = ListUtil.fold1(vars, updateHtCr2U, op_unit, htCr2U)
        @assign (htS2U, htU2S) = insertUnitString(op_unit, htS2U, htU2S)
        (unit, ListUtil.append_reverse(icu1, icu2))
      end

      BINARY_EXPRESSION(
        exp1,
        OPERATOR(op = Op.DIV),
        exp2,
      ) where {(Unit.isMaster(unit))} => begin
        @match (Unit.UNIT(), htCr2U, htS2U, htU2S, fnCache, icu1) =
          insertUnitInEquation(exp1, Unit.MASTER(nil), htCr2U, htS2U, htU2S, fnCache)
        @match (Unit.MASTER(), htCr2U, htS2U, htU2S, fnCache, icu2) =
          insertUnitInEquation(exp2, Unit.MASTER(nil), htCr2U, htS2U, htU2S, fnCache)
        (Unit.MASTER(nil), ListUtil.append_reverse(icu1, icu2))
      end

      BINARY_EXPRESSION(
        exp1,
        OPERATOR(op = Op.DIV),
        exp2,
      ) where {(Unit.isUnit(unit))} => begin
        @match ((@match Unit.UNIT() = unit2), htCr2U, htS2U, htU2S, fnCache, icu1) =
          insertUnitInEquation(exp1, Unit.MASTER(nil), htCr2U, htS2U, htU2S, fnCache)
        @match (Unit.MASTER(varList = vars), htCr2U, htS2U, htU2S, fnCache, icu2) =
          insertUnitInEquation(exp2, Unit.MASTER(nil), htCr2U, htS2U, htU2S, fnCache)
        @assign op_unit = Unit.unitDiv(unit2, unit)
        @assign htCr2U = ListUtil.fold1(vars, updateHtCr2U, op_unit, htCr2U)
        @assign (htS2U, htU2S) = insertUnitString(op_unit, htS2U, htU2S)
        (unit, ListUtil.append_reverse(icu1, icu2))
      end

      BINARY_EXPRESSION(
        exp1,
        OPERATOR(op = Op.DIV),
        exp2,
      ) => begin
        @match (Unit.MASTER(), htCr2U, htS2U, htU2S, fnCache, icu1) =
          insertUnitInEquation(exp1, Unit.MASTER(nil), htCr2U, htS2U, htU2S, fnCache)
        @match (Unit.MASTER(), htCr2U, htS2U, htU2S, fnCache, icu2) =
          insertUnitInEquation(exp2, Unit.MASTER(nil), htCr2U, htS2U, htU2S, fnCache)
        (Unit.MASTER(nil), ListUtil.append_reverse(icu1, icu2))
      end

      BINARY_EXPRESSION(
        exp1,
        OPERATOR(op = Op.POW),
        exp2 && REAL_EXPRESSION(__),
      ) => begin
        #=  POW
        =#
        @match ((@match Unit.UNIT() = unit1), htCr2U, htS2U, htU2S, fnCache, icu1) =
          insertUnitInEquation(exp1, Unit.MASTER(nil), htCr2U, htS2U, htU2S, fnCache)
        @assign i = realInt(exp2.value)
        @match true = realEq(exp2.value, i)
        @assign op_unit = Unit.unitPow(unit, i)
        @assign (htS2U, htU2S) = insertUnitString(op_unit, htS2U, htU2S)
        (op_unit, icu1)
      end

      BINARY_EXPRESSION(
        exp1,
        OPERATOR(op = Op.POW),
        exp2 && REAL_EXPRESSION(__),
      ) where {(Unit.isUnit(unit))} => begin
        @match (Unit.MASTER(varList = vars), htCr2U, htS2U, htU2S, fnCache, icu1) =
          insertUnitInEquation(exp1, Unit.MASTER(nil), htCr2U, htS2U, htU2S, fnCache)
        @assign op_unit = Unit.unitRoot(unit, exp2.value)
        @assign htCr2U = ListUtil.fold1(vars, updateHtCr2U, op_unit, htCr2U)
        @assign (htS2U, htU2S) = insertUnitString(op_unit, htS2U, htU2S)
        (unit, icu1)
      end

      BINARY_EXPRESSION(
        exp1,
        OPERATOR(op = Op.POW),
        REAL_EXPRESSION(__),
      ) => begin
        @assign (_, htCr2U, htS2U, htU2S, fnCache, icu1) =
          insertUnitInEquation(exp1, Unit.MASTER(nil), htCr2U, htS2U, htU2S, fnCache)
        (Unit.MASTER(nil), icu1)
      end

      CALL_EXPRESSION(__) => begin
        #=  Call
        =#
        @assign (op_unit, htCr2U, htS2U, htU2S, fnCache, icu1) =
          insertUnitInEquationCall(eq.call, unit, htCr2U, htS2U, htU2S, fnCache)
        (op_unit, icu1)
      end

      IF_EXPRESSION(__) => begin
        @assign (unit1, htCr2U, htS2U, htU2S, fnCache, icu1) =
          insertUnitInEquation(eq.trueBranch, unit, htCr2U, htS2U, htU2S, fnCache)
        @assign (unit2, htCr2U, htS2U, htU2S, fnCache, icu2) =
          insertUnitInEquation(eq.falseBranch, unit1, htCr2U, htS2U, htU2S, fnCache)
        @assign (b, op_unit, htCr2U) = unitTypesEqual(unit1, unit2, htCr2U)
        @assign inconsistentUnits = ListUtil.append_reverse(icu1, icu2)
        if !b
          @assign inconsistentUnits = _cons(
            list((eq.trueBranch, unit1), (eq.falseBranch, unit2)),
            inconsistentUnits,
          )
          @assign op_unit = Unit.MASTER(nil)
        end
        (op_unit, inconsistentUnits)
      end

      RELATION_EXPRESSION(__) => begin
        @assign (unit1, htCr2U, htS2U, htU2S, fnCache, icu1) =
          insertUnitInEquation(eq.exp1, unit, htCr2U, htS2U, htU2S, fnCache)
        @assign (unit2, htCr2U, htS2U, htU2S, fnCache, icu2) =
          insertUnitInEquation(eq.exp2, unit, htCr2U, htS2U, htU2S, fnCache)
        @assign (b, op_unit, htCr2U) = unitTypesEqual(unit1, unit2, htCr2U)
        @assign inconsistentUnits = ListUtil.append_reverse(icu1, icu2)
        if !b
          @assign inconsistentUnits =
            _cons(list((eq.exp1, unit1), (eq.exp2, unit2)), inconsistentUnits)
          @assign op_unit = Unit.MASTER(nil)
        end
        (op_unit, inconsistentUnits)
      end

      UNARY_EXPRESSION(
        operator = OPERATOR(op = Op.UMINUS),
      ) => begin
        @assign (op_unit, htCr2U, htS2U, htU2S, fnCache, icu1) =
          insertUnitInEquation(eq.exp, unit, htCr2U, htS2U, htU2S, fnCache)
        (op_unit, icu1)
      end

      CREF_EXPRESSION(
        __,
      ) where {(
        isSimple(eq.cref) &&
        firstName(eq.cref) == "time"
      )} => begin
        @assign op_unit = Unit.UNIT(1e0, 0, 0, 0, 1, 0, 0, 0)
        @assign htS2U = addUnit2HtS2U("time", op_unit, htS2U)
        @assign htU2S = addUnit2HtU2S("time", op_unit, htU2S)
        (op_unit, nil)
      end

      CREF_EXPRESSION(ty = TYPE_REAL(__)) => begin
        (BaseHashTable.get(stripSubscripts(eq.cref), htCr2U), nil)
      end

      _ => begin
        (Unit.MASTER(nil), nil)
      end
    end
  end
  return (unit, htCr2U, htS2U, htU2S, fnCache, inconsistentUnits)
end

""" #= Inserts the units in the equation and checks if the equation is consistent or not. =#"""
function insertUnitInEquationCall(
  call::Call,
  unit::Unit.Unit,
  htCr2U::HashTableCrToUnit.HashTable,
  htS2U::HashTableStringToUnit.HashTable,
  htU2S::HashTableUnitToString.HashTable,
  fnCache::FunctionUnitCache.Cache,
)::Tuple{
  Unit.Unit,
  HashTableCrToUnit.HashTable,
  HashTableStringToUnit.HashTable,
  HashTableUnitToString.HashTable,
  FunctionUnitCache.Cache,
  List{List{Tuple{Expression, Unit.Unit}}},
}
  local inconsistentUnits::List{List{Tuple{Expression, Unit.Unit}}}

  local fn_path::Absyn.Path
  local fn_name::String
  local call_args::List{Expression}
  local op_unit::Unit.Unit
  local vars::List{ComponentRef}
  local var_names::List{String}
  local unit_names::List{String}

  @assign fn_path = P_Call.functionName(call)
  @assign call_args = P_Call.arguments(call)
  @assign (unit, inconsistentUnits) = begin
    @matchcontinue fn_path begin
      Absyn.IDENT("pre") => begin
        @assign (op_unit, htCr2U, htS2U, htU2S, fnCache, inconsistentUnits) =
          insertUnitInEquation(listHead(call_args), unit, htCr2U, htS2U, htU2S, fnCache)
        (Unit.MASTER(nil), inconsistentUnits)
      end

      Absyn.IDENT("der") => begin
        @assign (op_unit, htCr2U, htS2U, htU2S, fnCache, inconsistentUnits) =
          insertUnitInEquation(
            listHead(call_args),
            Unit.MASTER(nil),
            htCr2U,
            htS2U,
            htU2S,
            fnCache,
          )
        if Unit.isUnit(op_unit)
          @assign op_unit = Unit.unitDiv(op_unit, Unit.UNIT(1e0, 0, 0, 0, 1, 0, 0, 0))
          @assign (htS2U, htU2S) = insertUnitString(op_unit, htS2U, htU2S)
        elseif Unit.isUnit(unit)
          @match Unit.MASTER(varList = vars) = op_unit
          @assign op_unit = Unit.unitMul(unit, Unit.UNIT(1e0, 0, 0, 0, 1, 0, 0, 0))
          @assign htCr2U = ListUtil.fold1(vars, updateHtCr2U, op_unit, htCr2U)
          @assign (htS2U, htU2S) = insertUnitString(op_unit, htS2U, htU2S)
        else
          @assign op_unit = Unit.MASTER(nil)
        end
        (op_unit, inconsistentUnits)
      end

      Absyn.IDENT("sqrt") => begin
        @assign (op_unit, htCr2U, htS2U, htU2S, fnCache, inconsistentUnits) =
          insertUnitInEquation(
            listHead(call_args),
            Unit.MASTER(nil),
            htCr2U,
            htS2U,
            htU2S,
            fnCache,
          )
        if Unit.isUnit(op_unit)
          @assign op_unit = Unit.unitRoot(op_unit, 2.0)
          @assign (htS2U, htU2S) = insertUnitString(op_unit, htS2U, htU2S)
        elseif Unit.isUnit(unit)
          @match Unit.MASTER(varList = vars) = op_unit
          @assign op_unit = Unit.unitPow(unit, 2)
          @assign htCr2U = ListUtil.fold1(vars, updateHtCr2U, op_unit, htCr2U)
          @assign (htS2U, htU2S) = insertUnitString(op_unit, htS2U, htU2S)
          @assign op_unit = unit
        else
          @assign op_unit = Unit.MASTER(nil)
        end
        (op_unit, inconsistentUnits)
      end

      Absyn.IDENT(__) where {(isBuiltin(P_Call.typedFunction(call)))} => begin
        @assign (htCr2U, htS2U, htU2S, fnCache, inconsistentUnits) =
          foldCallArg(call_args, htCr2U, htS2U, htU2S, fnCache)
        (Unit.MASTER(nil), inconsistentUnits)
      end

      _ => begin
        @assign fn_name = AbsynUtil.pathString(AbsynUtil.makeNotFullyQualified(fn_path))
        @assign (var_names, _, unit_names, _, fnCache) =
          getCallUnits(fn_name, call, fnCache)
        @assign (htCr2U, htS2U, htU2S, fnCache, inconsistentUnits) = foldCallArg1(
          call_args,
          htCr2U,
          htS2U,
          htU2S,
          fnCache,
          unit,
          unit_names,
          var_names,
          fn_name,
        )
        (Unit.MASTER(nil), inconsistentUnits)
      end

      _ => begin
        (Unit.MASTER(nil), nil)
      end
    end
  end
  return (unit, htCr2U, htS2U, htU2S, fnCache, inconsistentUnits)
end

function insertUnitString(
  unit::Unit.Unit,
  htS2U::HashTableStringToUnit.HashTable,
  htU2S::HashTableUnitToString.HashTable,
)::Tuple{HashTableStringToUnit.HashTable, HashTableUnitToString.HashTable}

  local unit_str::String

  @assign unit_str = Unit.unitString(unit, htU2S)
  @assign htS2U = addUnit2HtS2U(unit_str, unit, htS2U)
  @assign htU2S = addUnit2HtU2S(unit_str, unit, htU2S)
  return (htS2U, htU2S)
end

function getCallUnits(
  fnName::String,
  call::Call,
  fnCache::FunctionUnitCache.Cache,
)::Tuple{List{String}, List{String}, List{String}, List{String}, FunctionUnitCache.Cache}
  local outFnCache::FunctionUnitCache.Cache = fnCache
  local outputUnits::List{String}
  local inputUnits::List{String}
  local outputVars::List{String}
  local inputVars::List{String}

  local args::Functionargs

  try
    @assign args = BaseHashTable.get(fnName, fnCache)
  catch
    @assign args = parseFunctionUnits(fnName, P_Call.typedFunction(call))
    @assign outFnCache = BaseHashTable.addUnique((fnName, args), outFnCache)
  end
  @match Functionargs.FUNCTIONUNITS(_, inputVars, outputVars, inputUnits, outputUnits) =
    args
  return (inputVars, outputVars, inputUnits, outputUnits, outFnCache)
end

function parseFunctionUnits(funcName::String, func::M_Function)::Functionargs
  local outArgs::Functionargs

  local fn_name::String
  local in_units::List{String}
  local out_units::List{String}
  local in_args::List{String}
  local out_args::List{String}

  @assign in_units =
    list(P_Component.getUnitAttribute(component(p), "NONE") for p in func.inputs)
  @assign out_units = List(
    P_Component.getUnitAttribute(component(p), "NONE") for p in func.outputs
  )
  @assign in_args = list(name(p) for p in func.inputs)
  @assign out_args = list(name(p) for p in func.outputs)
  @assign outArgs = FUNCTIONUNITS(funcName, in_args, out_args, in_units, out_units)
  return outArgs
end

""" #= Checks equality of two units. =#"""
function unitTypesEqual(
  unit1::Unit.Unit,
  unit2::Unit.Unit,
  htCr2U::HashTableCrToUnit.HashTable,
)::Tuple{Bool, Unit.Unit, HashTableCrToUnit.HashTable}
  local outHtCr2U::HashTableCrToUnit.HashTable
  local outUnit::Unit.Unit
  local isEqual::Bool

  @assign (isEqual, outUnit, outHtCr2U) = begin
    local r::AbstractFloat
    local vars1::List{ComponentRef}
    local vars2::List{ComponentRef}
    local s1::String
    local s2::String
    @match (unit1, unit2) begin
      (Unit.UNIT(__), Unit.UNIT(__)) => begin
        @assign isEqual = realEq(unit1.factor, unit2.factor)
        if !isEqual
          @assign r = realMax(realAbs(unit1.factor), realAbs(unit2.factor))
          @assign isEqual =
            realLe(realDiv(realAbs(realSub(unit1.factor, unit2.factor)), r), 1e-3)
        end
        @assign isEqual =
          isEqual &&
          unit1.mol == unit2.mol &&
          unit1.cd == unit2.cd &&
          unit1.m == unit2.m &&
          unit1.s == unit2.s &&
          unit1.A == unit2.A &&
          unit1.K == unit2.K &&
          unit1.g == unit2.g
        (isEqual, unit1, htCr2U)
      end

      (Unit.UNIT(__), Unit.MASTER(varList = vars2)) => begin
        @assign outHtCr2U = ListUtil.fold1(vars2, updateHtCr2U, unit1, htCr2U)
        (true, unit1, outHtCr2U)
      end

      (Unit.MASTER(varList = vars1), Unit.UNIT(__)) => begin
        @assign outHtCr2U = ListUtil.fold1(vars1, updateHtCr2U, unit2, htCr2U)
        (true, unit2, outHtCr2U)
      end

      (Unit.MASTER(varList = vars1), Unit.MASTER(varList = vars2)) => begin
        @assign vars2 = ListUtil.append_reverse(vars1, vars2)
        (true, Unit.MASTER(vars2), htCr2U)
      end

      (Unit.UNKNOWN(unit = s1), Unit.UNKNOWN(unit = s2)) => begin
        (s1 == s2, unit1, htCr2U)
      end

      (Unit.UNKNOWN(__), _) => begin
        (true, unit1, htCr2U)
      end

      (_, Unit.UNKNOWN(__)) => begin
        (true, unit2, htCr2U)
      end

      _ => begin
        (false, unit1, htCr2U)
      end
    end
  end
  return (isEqual, outUnit, outHtCr2U)
end

function updateHtCr2U(
  cref::ComponentRef,
  unit::Unit.Unit,
  htCr2U::HashTableCrToUnit.HashTable,
)::HashTableCrToUnit.HashTable

  if !BaseHashTable.hasKey(NFUnit.UPDATECREF, htCr2U)
    @assign htCr2U = BaseHashTable.add((NFUnit.UPDATECREF, Unit.MASTER(nil)), htCr2U)
  end
  BaseHashTable.update((cref, unit), htCr2U)
  return htCr2U
end

""" #= returns the inconsistent Equation with sub-expression =#"""
function Errorfunction(
  inexpList::List{<:Tuple{<:Expression, Unit.Unit}},
  inEq::Equation,
  inHtU2S::HashTableUnitToString.HashTable,
)
  return @assign _ = begin
    local s::String
    local s1::String
    local s2::String
    local s3::String
    local s4::String
    local expList::List{Tuple{Expression, Unit.Unit}}
    local exp1::Expression
    local exp2::Expression
    local i::Int
    local info::SourceInfo
    @match (inexpList, inEq, inHtU2S) begin
      (expList, _, _) => begin
        @assign info = Equation_info(inEq)
        @assign s = P_Equation.Equation.toString(inEq)
        @assign s1 = Errorfunction2(expList, inHtU2S)
        @assign s2 =
          "The following equation is INCONSISTENT due to specified unit information: " +
          s +
          "\\n"
        Error.addSourceMessage(Error.COMPILER_WARNING, list(s2), info)
        Error.addCompilerWarning(
          "The units of following sub-expressions need to be equal:\\n" + s1,
        )
        ()
      end
    end
  end
  #= /*
         Error.addCompilerWarning(\"The following NEWFRONTEND UNIT CHECK equation is INCONSISTENT due to specified unit information: \" + s + \"\\n\" +
           \"The units of following sub-expressions need to be equal:\\n\" + s1 );*/ =#
end

""" #= help-function =#"""
function Errorfunction2(
  inexpList::List{<:Tuple{<:Expression, Unit.Unit}},
  inHtU2S::HashTableUnitToString.HashTable,
)::String
  local outS::String

  @assign outS = begin
    local expList::List{Tuple{Expression, Unit.Unit}}
    local exp::Expression
    local ut::Unit.Unit
    local s::String
    local s1::String
    local s2::String
    @match (inexpList, inHtU2S) begin
      ((exp, ut) <| nil(), _) => begin
        @assign s = toString(exp)
        @assign s1 = Unit.unitString(ut, inHtU2S)
        @assign s = "- sub-expression \\" + s + "\\ has unit \\" + s1 + "\\"
        s
      end

      ((exp, ut) <| expList, _) => begin
        @assign s = toString(exp)
        @assign s1 = Unit.unitString(ut, inHtU2S)
        @assign s2 = Errorfunction2(expList, inHtU2S)
        @assign s = "- sub-expression \\" + s + "\\ has unit \\" + s1 + "\\\\n" + s2
        s
      end
    end
  end
  return outS
end

""" #= help-function for CALL case in function insertUnitInEquation =#"""
function foldCallArg(
  args::List{<:Expression},
  htCr2U::HashTableCrToUnit.HashTable,
  htS2U::HashTableStringToUnit.HashTable,
  htU2S::HashTableUnitToString.HashTable,
  fnCache::FunctionUnitCache.Cache,
)::Tuple{
  HashTableCrToUnit.HashTable,
  HashTableStringToUnit.HashTable,
  HashTableUnitToString.HashTable,
  FunctionUnitCache.Cache,
  List{List{Tuple{Expression, Unit.Unit}}},
}
  local inconsistentUnits::List{List{Tuple{Expression, Unit.Unit}}} = nil

  local icu::List{List{Tuple{Expression, Unit.Unit}}}

  for exp in args
    @assign (_, htCr2U, htS2U, htU2S, fnCache, icu) =
      insertUnitInEquation(exp, Unit.MASTER(nil), htCr2U, htS2U, htU2S, fnCache)
    @assign inconsistentUnits = ListUtil.append_reverse(icu, inconsistentUnits)
  end
  @assign inconsistentUnits = listReverse(inconsistentUnits)
  return (htCr2U, htS2U, htU2S, fnCache, inconsistentUnits)
end

""" #= Help function for CALL case in userdefinde top level function insertUnitInEquation =#"""
function foldCallArg1(
  args::List{<:Expression},
  htCr2U::HashTableCrToUnit.HashTable,
  htS2U::HashTableStringToUnit.HashTable,
  htU2S::HashTableUnitToString.HashTable,
  fnCache::FunctionUnitCache.Cache,
  inUnit::Unit.Unit,
  units::List{<:String},
  vars::List{<:String},
  fnName::String,
)::Tuple{
  HashTableCrToUnit.HashTable,
  HashTableStringToUnit.HashTable,
  HashTableUnitToString.HashTable,
  FunctionUnitCache.Cache,
  List{List{Tuple{Expression, Unit.Unit}}},
}
  local inconsistentUnits::List{List{Tuple{Expression, Unit.Unit}}} = nil

  local unit::String
  local var::String
  local rest_units::List{String} = units
  local rest_vars::List{String} = vars
  local op_unit::Unit.Unit
  local op_unit2::Unit.Unit
  local icu::List{List{Tuple{Expression, Unit.Unit}}}
  local temp::Expression
  local b::Bool

  for arg in args
    @match _cons(var, rest_vars) = rest_vars
    @match _cons(unit, rest_units) = rest_units
    @assign (op_unit, htCr2U, htS2U, htU2S, fnCache, icu) =
      insertUnitInEquation(arg, inUnit, htCr2U, htS2U, htU2S, fnCache)
    if unit == "NONE"
      @assign op_unit2 = Unit.MASTER(nil)
    else
      @assign op_unit2 = Unit.parseUnitString(unit, htS2U)
    end
    @assign (b, op_unit) = unitTypesEqual(op_unit, op_unit2, htCr2U)
    if b
      @assign icu = nil
    else
      @assign temp = makeNewCref(unit, fnName)
      @assign icu = list(list((arg, op_unit), (temp, op_unit2)))
    end
    @assign inconsistentUnits = ListUtil.append_reverse(icu, inconsistentUnits)
  end
  return (htCr2U, htS2U, htU2S, fnCache, inconsistentUnits)
end

function addUnit2HtS2U(
  name::String,
  unit::Unit.Unit,
  inHtS2U::HashTableStringToUnit.HashTable,
)::HashTableStringToUnit.HashTable
  local outHtS2U::HashTableStringToUnit.HashTable

  @assign outHtS2U = BaseHashTable.add((name, unit), inHtS2U)
  return outHtS2U
end

function addUnit2HtU2S(
  name::String,
  unit::Unit.Unit,
  htU2S::HashTableUnitToString.HashTable,
)::HashTableUnitToString.HashTable

  try
    @assign htU2S = BaseHashTable.addUnique((unit, name), htU2S)
  catch
  end
  return htU2S
end

""" #= converts String to unit =#"""
function convertUnitString2unit(
  var::Variable,
  htCr2U::HashTableCrToUnit.HashTable,
  htS2U::HashTableStringToUnit.HashTable,
  htU2S::HashTableUnitToString.HashTable,
)::Tuple{
  HashTableCrToUnit.HashTable,
  HashTableStringToUnit.HashTable,
  HashTableUnitToString.HashTable,
}

  local unit_binding::Binding
  local unit_exp::Option{Expression}
  local unit_string::String
  local unit::Unit.Unit

  @assign unit_binding = P_Variable.Variable.lookupTypeAttribute("unit", var)
  @assign unit_exp = typedExp(unit_binding)
  @assign () = begin
    @match unit_exp begin
      SOME(STRING_EXPRESSION(
        value = unit_string,
      )) where {(!stringEmpty(unit_string))} => begin
        @assign (unit, htS2U, htU2S) = parse(unit_string, var.name, htS2U, htU2S)
        @assign htCr2U = BaseHashTable.add((var.name, unit), htCr2U)
        ()
      end

      _ => begin
        @assign htCr2U = BaseHashTable.add((var.name, Unit.MASTER(list(var.name))), htCr2U)
        @assign htS2U = addUnit2HtS2U("-", Unit.MASTER(list(var.name)), htS2U)
        @assign htU2S = addUnit2HtU2S("-", Unit.MASTER(list(var.name)), htU2S)
        ()
      end
    end
  end
  return (htCr2U, htS2U, htU2S)
end

""" #= author: lochel =#"""
function parse(
  unitString::String,
  cref::ComponentRef,
  htS2U::HashTableStringToUnit.HashTable,
  htU2S::HashTableUnitToString.HashTable,
)::Tuple{Unit.Unit, HashTableStringToUnit.HashTable, HashTableUnitToString.HashTable}
  local unit::Unit.Unit
  if stringEmpty(unitString)
    @assign unit = Unit.MASTER(list(cref))
    return (unit, htS2U, htU2S)
  end
  try
    @assign unit = BaseHashTable.get(unitString, htS2U)
  catch
    try
      @assign unit = Unit.parseUnitString(unitString, htS2U)
    catch
      @assign unit = Unit.UNKNOWN(unitString)
    end
    @assign htS2U = addUnit2HtS2U(unitString, unit, htS2U)
    @assign htU2S = addUnit2HtU2S(unitString, unit, htU2S)
  end
  return (unit, htS2U, htU2S)
end
