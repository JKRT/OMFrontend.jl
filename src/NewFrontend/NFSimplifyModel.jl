module NFSimplifyModel

using MetaModelica
using ExportAll
#= Forward declarations for uniontypes until Julia adds support for mutual recursion =#

MakeElement = Function

MakeFunc = Function
SimplifyFunc = Function

#= /*
* This file is part of OpenModelica.
*
* Copyright (c) 1998-2014, Open Source Modelica Consortium (OSMC),
* c/o Linköpings universitet, Department of Computer and Information Science,
* SE-58183 Linköping, Sweden.
*
* All rights reserved.
*
* THIS PROGRAM IS PROVIDED UNDER THE TERMS OF GPL VERSION 3 LICENSE OR
* THIS OSMC PUBLIC LICENSE (OSMC-PL) VERSION 1.2.
* ANY USE, REPRODUCTION OR DISTRIBUTION OF THIS PROGRAM CONSTITUTES
* RECIPIENT'S ACCEPTANCE OF THE OSMC PUBLIC LICENSE OR THE GPL VERSION 3,
* ACCORDING TO RECIPIENTS CHOICE.
*
* The OpenModelica software and the Open Source Modelica
* Consortium (OSMC) Public License (OSMC-PL) are obtained
* from OSMC, either from the above address,
* from the URLs: http:www.ida.liu.se/projects/OpenModelica or
* http:www.openmodelica.org, and in the OpenModelica distribution.
* GNU version 3 is obtained from: http:www.gnu.org/copyleft/gpl.html.
*
* This program is distributed WITHOUT ANY WARRANTY; without
* even the implied warranty of  MERCHANTABILITY or FITNESS
* FOR A PARTICULAR PURPOSE, EXCEPT AS EXPRESSLY SET FORTH
* IN THE BY RECIPIENT SELECTED SUBSIDIARY LICENSE CONDITIONS OF OSMC-PL.
*
* See the full OSMC Public License conditions for more details.
*
*/ =#
import ..NFFlatModel
FlatModel = NFFlatModel
import ..P_NFEquation
P_Equation = P_NFEquation
Equation = P_NFEquation.NFEquation
import ..P_NFStatement
P_Statement = P_NFStatement
Statement = P_NFStatement.NFStatement
import ..P_NFExpression
P_Expression = P_NFExpression
Expression = P_NFExpression.NFExpression
import ..P_NFType
P_M_Type = P_NFType
M_Type = NFType
import ..P_NFComponentRef
P_ComponentRef = P_NFComponentRef
ComponentRef = P_NFComponentRef.NFComponentRef
import ..NFFlatten.FunctionTree
import ..NFClass.P_Class
import ..NFInstNode.P_InstNode
import ..NFFunction.P_Function
import ..P_NFSections
P_Sections = P_NFSections
Sections = P_NFSections.NFSections
import ..NFBinding.P_Binding
import ..P_NFVariable
P_Variable = P_NFVariable
Variable = P_NFVariable.NFVariable
import ..P_NFAlgorithm
P_Algorithm = P_NFAlgorithm
Algorithm = P_NFAlgorithm.NFAlgorithm
import ..P_NFDimension
P_Dimension = P_NFDimension
Dimension = P_NFDimension.NFDimension
import ..P_NFSubscript
P_Subscript = P_NFSubscript
Subscript = P_NFSubscript.NFSubscript

using MetaModelica.Dangerous
import ..ExecStat.execStat
import ..NFSimplifyExp
SimplifyExp = NFSimplifyExp
import ..NFPrefixes.Variability
import ..NFCeval
Ceval = NFCeval

function simplify(flatModel::FlatModel)::FlatModel

  @assign flatModel.variables = List(simplifyVariable(v) for v in flatModel.variables)
  @assign flatModel.equations = simplifyEquations(flatModel.equations)
  @assign flatModel.initialEquations = simplifyEquations(flatModel.initialEquations)
  @assign flatModel.algorithms = simplifyAlgorithms(flatModel.algorithms)
  @assign flatModel.initialAlgorithms = simplifyAlgorithms(flatModel.initialAlgorithms)
  execStat(getInstanceName())
  return flatModel
end

function simplifyVariable(var::Variable)::Variable

  @assign var.binding = simplifyBinding(var.binding)
  @assign var.typeAttributes = List(simplifyTypeAttribute(a) for a in var.typeAttributes)
  return var
end

function simplifyBinding(binding::Binding)::Binding

  local exp::Expression
  local sexp::Expression

  if isBound(binding)
    @assign exp = getTypedExp(binding)
    @assign sexp = SimplifyExp.simplify(exp)
    @assign sexp = removeEmptyFunctionArguments(sexp)
    if !referenceEq(exp, sexp)
      @assign binding = setTypedExp(sexp, binding)
    end
  end
  return binding
end

function simplifyTypeAttribute(attribute::Tuple{<:String, Binding})::Tuple{String, Binding}

  local name::String
  local binding::Binding
  local sbinding::Binding

  @assign (name, binding) = attribute
  @assign sbinding = simplifyBinding(binding)
  if !referenceEq(binding, sbinding)
    @assign attribute = (name, sbinding)
  end
  return attribute
end

function simplifyDimension(dim::Dimension)::Dimension
  local outDim::Dimension

  @assign outDim = begin
    local e::Expression
    @match dim begin
      P_Dimension.Dimension.EXP(__) => begin
        @assign e = SimplifyExp.simplify(dim.exp)
        if referenceEq(e, dim.exp)
          dim
        else
          P_Dimension.Dimension.fromExp(e, dim.var)
        end
      end

      _ => begin
        dim
      end
    end
  end
  return outDim
end

function simplifyEquations(eql::List{<:Equation})::List{Equation}
  local outEql::List{Equation} = nil

  for eq in eql
    @assign outEql = simplifyEquation(eq, outEql)
  end
  @assign outEql = listReverseInPlace(outEql)
  return outEql
end

function simplifyEquation(eq::Equation, equations::List{<:Equation})::List{Equation}

  @assign equations = begin
    local e::Expression
    local lhs::Expression
    local rhs::Expression
    local ty::M_Type
    @match eq begin
      EQUATION_EQUALITY(__) => begin
        simplifyEqualityEquation(eq, equations)
      end

      EQUATION_ARRAY_EQUALITY(__) => begin
        @assign ty = Type.mapDims(eq.ty, simplifyDimension)
        if !Type.isEmptyArray(ty)
          @assign rhs = removeEmptyFunctionArguments(SimplifyExp.simplify(eq.rhs))
          @assign equations = _cons(
            EQUATION_ARRAY_EQUALITY(eq.lhs, rhs, ty, eq.source),
            equations,
          )
        end
        equations
      end

      EQUATION_IF(__) => begin
        simplifyIfEqBranches(eq.branches, eq.source, equations)
      end

      P_Equation.Equation.WHEN(__) => begin
        @assign eq.branches = List(
          begin
            @match b begin
              P_Equation.Equation.BRANCH(__) => begin
                @assign b.condition = SimplifyExp.simplify(b.condition)
                @assign b.body = simplifyEquations(b.body)
                b
              end
            end
          end for b in eq.branches
        )
        _cons(eq, equations)
      end

      P_Equation.Equation.ASSERT(__) => begin
        @assign eq.condition = SimplifyExp.simplify(eq.condition)
        if P_Expression.Expression.isTrue(eq.condition)
          equations
        else
          _cons(eq, equations)
        end
      end

      EQUATION_REINIT(__) => begin
        @assign eq.reinitExp = SimplifyExp.simplify(eq.reinitExp)
        _cons(eq, equations)
      end

      P_Equation.Equation.NORETCALL(__) => begin
        @assign e = SimplifyExp.simplify(eq.exp)
        if P_Expression.Expression.isCall(e)
          @assign eq.exp = removeEmptyFunctionArguments(e)
          @assign equations = _cons(eq, equations)
        end
        equations
      end

      _ => begin
        _cons(eq, equations)
      end
    end
  end
  return equations
end

function simplifyEqualityEquation(eq::Equation, equations::List{<:Equation})::List{Equation}

  local lhs::Expression
  local rhs::Expression
  local ty::M_Type
  local src::DAE.ElementSource

  @match EQUATION_EQUALITY(lhs = lhs, rhs = rhs, ty = ty, source = src) = eq
  @assign ty = Type.mapDims(ty, simplifyDimension)
  if Type.isEmptyArray(ty)
    return equations
  end
  @assign lhs = SimplifyExp.simplify(lhs)
  @assign lhs = removeEmptyTupleElements(lhs)
  @assign rhs = SimplifyExp.simplify(rhs)
  @assign rhs = removeEmptyFunctionArguments(rhs)
  @assign equations = begin
    @match (lhs, rhs) begin
      (TUPLE_EXPRESSION(__), TUPLE_EXPRESSION(__)) => begin
        simplifyTupleElement(
          lhs.elements,
          rhs.elements,
          ty,
          src,
          P_Equation.Equation.makeEquality,
          equations,
        )
      end

      _ => begin
        _cons(EQUATION_EQUALITY(lhs, rhs, ty, src), equations)
      end
    end
  end
  return equations
end

function simplifyAlgorithms(algs::List{<:Algorithm})::List{Algorithm}
  local outAlgs::List{Algorithm} = nil

  for alg in algs
    @assign alg = simplifyAlgorithm(alg)
    if !listEmpty(alg.statements)
      @assign outAlgs = _cons(alg, outAlgs)
    end
  end
  @assign outAlgs = listReverseInPlace(outAlgs)
  return outAlgs
end

function simplifyAlgorithm(alg::Algorithm)::Algorithm

  @assign alg.statements = simplifyStatements(alg.statements)
  return alg
end

function simplifyStatements(stmts::List{<:Statement})::List{Statement}
  local outStmts::List{Statement} = nil

  for s in stmts
    @assign outStmts = simplifyStatement(s, outStmts)
  end
  @assign outStmts = listReverseInPlace(outStmts)
  return outStmts
end

function simplifyStatement(stmt::Statement, statements::List{<:Statement})::List{Statement}

  @assign statements = begin
    local e::Expression
    local lhs::Expression
    local rhs::Expression
    local ty::M_Type
    local dim::Dimension
    local body::List{Statement}
    @match stmt begin
      P_Statement.Statement.ASSIGNMENT(__) => begin
        simplifyAssignment(stmt, statements)
      end

      P_Statement.Statement.FOR(range = SOME(e)) => begin
        @assign ty = P_Expression.Expression.typeOf(e)
        @assign dim = Type.nthDimension(ty, 1)
        #= if Dimension.isOne(dim) then
        =#
        #=   e := Expression.applySubscript(Subscript.INDEX(Expression.INTEGER(1)), e);
        =#
        #=   body := Statement.mapExpList(stmt.body,
        =#
        #=     function Expression.replaceIterator(iterator = stmt.iterator, iteratorValue = e));
        =#
        #=   body := simplifyStatements(body);
        =#
        #=   statements := listAppend(listReverse(body), statements);
        =#
        #= elseif not Dimension.isZero(dim) then
        =#
        if !P_Dimension.Dimension.isZero(dim)
          @assign stmt.range = SOME(SimplifyExp.simplify(e))
          @assign stmt.body = simplifyStatements(stmt.body)
          @assign statements = _cons(stmt, statements)
        end
        statements
      end

      P_Statement.Statement.IF(__) => begin
        simplifyIfStmtBranches(
          stmt.branches,
          stmt.source,
          P_Statement.Statement.makeIf,
          simplifyStatements,
          statements,
        )
      end

      P_Statement.Statement.WHEN(__) => begin
        @assign stmt.branches = List(
          (SimplifyExp.simplify(Util.tuple21(b)), simplifyStatements(Util.tuple22(b))) for b in stmt.branches
        )
        _cons(stmt, statements)
      end

      P_Statement.Statement.NORETCALL(__) => begin
        @assign e = SimplifyExp.simplify(stmt.exp)
        if P_Expression.Expression.isCall(e)
          @assign stmt.exp = removeEmptyFunctionArguments(e)
          @assign statements = _cons(stmt, statements)
        end
        statements
      end

      _ => begin
        _cons(stmt, statements)
      end
    end
  end
  return statements
end

function simplifyAssignment(stmt::Statement, statements::List{<:Statement})::List{Statement}

  local lhs::Expression
  local rhs::Expression
  local rhs_exp::Expression
  local rhs_rest::List{Expression}
  local ty::M_Type
  local src::DAE.ElementSource

  @match P_Statement.Statement.ASSIGNMENT(lhs = lhs, rhs = rhs, ty = ty, source = src) =
    stmt
  @assign ty = Type.mapDims(ty, simplifyDimension)
  if Type.isEmptyArray(ty)
    return statements
  end
  @assign lhs = SimplifyExp.simplify(lhs)
  @assign lhs = removeEmptyTupleElements(lhs)
  @assign rhs = SimplifyExp.simplify(rhs)
  @assign rhs = removeEmptyFunctionArguments(rhs)
  @assign statements = begin
    @match (lhs, rhs) begin
      (TUPLE_EXPRESSION(__), TUPLE_EXPRESSION(__)) => begin
        simplifyTupleElement(
          lhs.elements,
          rhs.elements,
          ty,
          src,
          P_Statement.Statement.makeAssignment,
          statements,
        )
      end

      _ => begin
        _cons(P_Statement.Statement.ASSIGNMENT(lhs, rhs, ty, src), statements)
      end
    end
  end
  return statements
end

""" #= Helper function to simplifyEqualityEquation/simplifyAssignment.
   Handles Expression.TUPLE() := Expression.TUPLE() assignments by splitting
   them into a separate assignment statement for each pair of tuple elements. =#"""
function simplifyTupleElement(
  lhsTuple::List{Expression},
  rhsTuple::List{Expression},
  ty::M_Type,
  src::DAE.ElementSource,
  makeFn::MakeElement,
  statements::List{ElementT},
) where {ElementT}

  local rhs::Expression
  local rest_rhs::List{Expression} = rhsTuple
  local ety::M_Type
  local rest_ty::List{M_Type}

  @match TYPE_TUPLE(types = rest_ty) = ty
  for lhs in lhsTuple
    @match _cons(rhs, rest_rhs) = rest_rhs
    @match _cons(ety, rest_ty) = rest_ty
    if !P_Expression.Expression.isWildCref(lhs)
      @assign statements = _cons(makeFn(lhs, rhs, ety, src), statements)
    end
  end
  return statements
end

""" #= Replaces tuple elements that has one or more zero dimension with _. =#"""
function removeEmptyTupleElements(exp::Expression)::Expression

  @assign () = begin
    local tyl::List{M_Type}
    @match exp begin
      TUPLE_EXPRESSION(ty = TYPE_TUPLE(types = tyl)) => begin
        @assign exp.elements = List(@do_threaded_for if Type.isEmptyArray(t)
          CREF_EXPRESSION(t, WILD())
        else
          e
        end (e, t) (exp.elements, tyl))
        ()
      end

      _ => begin
        ()
      end
    end
  end
  return exp
end

function removeEmptyFunctionArguments(exp::Expression, isArg::Bool = false)::Expression
  local outExp::Expression

  local is_arg::Bool

  if isArg
    @assign () = begin
      @match exp begin
        CREF_EXPRESSION(__) where {(Type.isEmptyArray(exp.ty))} => begin
          @assign outExp =
            P_Expression.Expression.fillType(exp.ty, INTEGER_EXPRESSION(0))
          return
          ()
        end

        _ => begin
          ()
        end
      end
    end
  end
  @assign is_arg = isArg || P_Expression.Expression.isCall(exp)
  @assign outExp = P_Expression.Expression.mapShallow(
    exp,
    (is_arg) -> removeEmptyFunctionArguments(isArg = is_arg),
  )
  return outExp
end

function simplifyIfEqBranches(
  branches::List{<:Equation},
  src::DAE.ElementSource,
  elements::List{<:Equation},
)::List{Equation}

  local cond::Expression
  local body::List{Equation}
  local var::Variability
  local accum::List{P_Equation.Equation} = nil

  for branch in branches
    @assign accum = begin
      @match branch begin
        P_Equation.Equation.BRANCH(cond, var, body) => begin
          @assign cond = SimplifyExp.simplify(cond)
          #=  A branch with condition true will always be selected when encountered.
          =#
          if P_Expression.Expression.isTrue(cond)
            if listEmpty(accum)
              for eq in body
                @assign elements = simplifyEquation(eq, elements)
              end
              return
            else
              @assign accum = _cons(
                P_Equation.Equation.makeBranch(cond, simplifyEquations(body)),
                accum,
              )
              @assign elements = _cons(
                P_Equation.Equation.makeIf(listReverseInPlace(accum), src),
                elements,
              )
              return
            end
          elseif !P_Expression.Expression.isFalse(cond)
            @assign accum = _cons(
              P_Equation.Equation.makeBranch(cond, simplifyEquations(body)),
              accum,
            )
          end
          #=  If it's the first branch, remove the if and keep only the branch body.
          =#
          #=  Otherwise just discard the rest of the branches.
          =#
          #=  Keep branches that are neither literal true or false.
          =#
          accum
        end

        P_Equation.Equation.INVALID_BRANCH(
          branch = P_Equation.Equation.BRANCH(
            condition = cond,
            conditionVar = var,
          ),
        ) => begin
          if var <= Variability.STRUCTURAL_PARAMETER
            @assign cond = Ceval.evalExp(cond)
          end
          #=  An invalid branch that can't be removed will trigger the errors
          =#
          #=  stored in it.
          =#
          if !P_Expression.Expression.isFalse(cond)
            P_Equation.Equation.triggerErrors(branch)
          end
          accum
        end

        _ => begin
          _cons(branch, accum)
        end
      end
    end
  end
  if !listEmpty(accum)
    @assign elements =
      _cons(P_Equation.Equation.makeIf(listReverseInPlace(accum), src), elements)
  end
  return elements
end

function simplifyIfStmtBranches(
  branches::List{Tuple{Expression, List{ElemT}}},
  src::DAE.ElementSource,
  makeFunc::MakeFunc,
  simplifyFunc::SimplifyFunc,
  elements::List{ElemT},
) where {ElemT}

  local cond::Expression
  local body::List{ElemT}
  local accum::List{Tuple{Expression, List{ElemT}}} = nil

  for branch in branches
    @assign (cond, body) = branch
    @assign cond = SimplifyExp.simplify(cond)
    if P_Expression.Expression.isTrue(cond)
      if listEmpty(accum)
        @assign elements = listAppend(listReverse(simplifyFunc(body)), elements)
        return elements
      else
        @assign accum = _cons((cond, simplifyFunc(body)), accum)
        break
      end
    elseif !P_Expression.Expression.isFalse(cond)
      @assign accum = _cons((cond, simplifyFunc(body)), accum)
    end
  end
  #=  A branch with condition true will always be selected when encountered.
  =#
  #=  If it's the first branch, remove the if and keep only the branch body.
  =#
  #=  Otherwise just discard the rest of the branches.
  =#
  #=  Keep branches that are neither literal true or false.
  =#
  if !listEmpty(accum)
    @assign elements = _cons(makeFunc(listReverseInPlace(accum), src), elements)
  end
  return elements
end

function simplifyFunction(func::M_Function)
  local cls::Class
  local fn_body::Algorithm
  local sections::Sections

  return if !P_Function.isSimplified(func)
    P_Function.markSimplified(func)
    P_Function.mapExp(func, SimplifyExp.simplify, mapBody = false)
    @assign cls = getClass(func.node)
    @assign () = begin
      @match cls begin
        INSTANCED_CLASS(sections = sections) => begin
          @assign () = begin
            @match sections begin
              P_Sections.Sections.SECTIONS(algorithms = fn_body <| nil()) => begin
                @assign fn_body.statements = simplifyStatements(fn_body.statements)
                @assign sections.algorithms = list(fn_body)
                @assign cls.sections = sections
                updateClass(cls, func.node)
                ()
              end

              _ => begin
                ()
              end
            end
          end
          ()
        end

        _ => begin
          ()
        end
      end
    end
    for fn_der in func.derivatives
      for der_fn in P_Function.getCachedFuncs(fn_der.derivativeFn)
        simplifyFunction(der_fn)
      end
    end
  end
end

@exportAll()
end
