module NFEvalConstants

using MetaModelica
using ExportAll

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
import ..NFCall.P_Call
import ..P_NFEquation.P_Branch
import ..P_NFDimension
P_Dimension = P_NFDimension
Dimension = P_NFDimension.NFDimension

using MetaModelica.Dangerous
import ..ExecStat.execStat
import ..NFPrefixes.Variability
import ..NFCeval
Ceval = NFCeval
import ..NFPackage
Package = NFPackage

function evaluate(flatModel::FlatModel)::FlatModel

  local const_var::Variability = Variability.STRUCTURAL_PARAMETER

  @assign flatModel.variables =
    List(evaluateVariable(v, const_var) for v in flatModel.variables)
  @assign flatModel.equations = evaluateEquations(flatModel.equations, const_var)
  @assign flatModel.initialEquations =
    evaluateEquations(flatModel.initialEquations, const_var)
  @assign flatModel.algorithms = evaluateAlgorithms(flatModel.algorithms, const_var)
  @assign flatModel.initialAlgorithms =
    evaluateAlgorithms(flatModel.initialAlgorithms, const_var)
  execStat(getInstanceName())
  return flatModel
end

function evaluateVariable(var::Variable, constVariability::Variability)::Variable

  local binding::Binding

  @assign binding = evaluateBinding(
    var.binding,
    P_Variable.Variable.variability(var) <= constVariability,
    constVariability,
  )
  if !referenceEq(binding, var.binding)
    @assign var.binding = binding
  end
  @assign var.typeAttributes = List(
    evaluateTypeAttribute(a, Variability.STRUCTURAL_PARAMETER) for a in var.typeAttributes
  )
  return var
end

function evaluateBinding(
  binding::Binding,
  structural::Bool,
  constVariability::Variability,
)::Binding

  local exp::Expression
  local eexp::Expression

  if isBound(binding)
    @assign exp = getTypedExp(binding)
    if structural
      @assign eexp = Ceval.evalExp(exp, Ceval.P_EvalTarget.ATTRIBUTE(binding))
    else
      @assign eexp = evaluateExp(exp, constVariability)
    end
    if !referenceEq(exp, eexp)
      @assign binding = setTypedExp(eexp, binding)
    end
  end
  return binding
end

function evaluateTypeAttribute(
  attribute::Tuple{<:String, Binding},
  constVariability::Variability,
)::Tuple{String, Binding}

  local name::String
  local binding::Binding
  local sbinding::Binding
  local structural::Bool

  @assign (name, binding) = attribute
  @assign structural = name == "fixed" || name == "stateSelect"
  @assign sbinding = evaluateBinding(binding, structural, constVariability)
  if !referenceEq(binding, sbinding)
    @assign attribute = (name, sbinding)
  end
  return attribute
end

function evaluateExp(exp::Expression, constVariability::Variability)::Expression
  local outExp::Expression

  @assign outExp = evaluateExpTraverser(exp, constVariability, false)
  return outExp
end

function evaluateExpTraverser(
  exp::Expression,
  constVariability::Variability,
  changed::Bool,
)::Tuple{Expression, Bool}
  local outChanged::Bool
  local outExp::Expression

  local e::Expression
  local cref::ComponentRef
  local ty::M_Type
  local var::Variability

  @assign outExp = begin
    @match exp begin
      CREF_EXPRESSION(__) => begin
        @match (
          (@match CREF_EXPRESSION(cref = cref, ty = ty) = outExp),
          outChanged,
        ) = P_Expression.Expression.mapFoldShallow(
          exp,
          (constVariability) -> evaluateExpTraverser(constVariability = constVariability),
          false,
        )
        #=  Evaluate constants and structural parameters.
        =#
        if nodeVariability(cref) <= constVariability
          @assign outExp = Ceval.evalCref(
            cref,
            outExp,
            Ceval.P_EvalTarget.IGNORE_ERRORS(),
            evalSubscripts = false,
          )
          @assign outExp = P_Expression.Expression.stripBindingInfo(outExp)
          @assign outChanged = true
        elseif outChanged
          @assign outExp = CREF_EXPRESSION(
            getSubscriptedType(cref),
            cref,
          )
        end
        #=  Evaluate all constants and structural parameters.
        =#
        #=  If the cref's subscripts changed, recalculate its type.
        =#
        outExp
      end

      _ => begin
        @assign (outExp, outChanged) = P_Expression.Expression.mapFoldShallow(
          exp,
          (constVariability) -> evaluateExpTraverser(constVariability = constVariability),
          false,
        )
        if outChanged
          P_Expression.Expression.retype(outExp)
        else
          outExp
        end
      end
    end
  end
  @assign outChanged = changed || outChanged
  return (outExp, outChanged)
end

function evaluateDimension(dim::Dimension)::Dimension
  local outDim::Dimension

  @assign outDim = begin
    local e::Expression
    @match dim begin
      P_Dimension.Dimension.EXP(__) => begin
        @assign e =
          evaluateExp(dim.exp, constVariability = Variability.STRUCTURAL_PARAMETER)
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

function evaluateEquations(
  eql::List{<:Equation},
  constVariability::Variability,
)::List{Equation}
  local outEql::List{Equation} = List(evaluateEquation(e, constVariability) for e in eql)
  return outEql
end

function evaluateEquation(eq::Equation, constVariability::Variability)::Equation

  @assign eq = begin
    local e1::Expression
    local e2::Expression
    local e3::Expression
    local ty::M_Type
    @match eq begin
      EQUATION_EQUALITY(__) => begin
        @assign ty = Type.mapDims(eq.ty, evaluateDimension)
        @assign e1 = evaluateExp(eq.lhs, constVariability)
        @assign e2 = evaluateExp(eq.rhs, constVariability)
        EQUATION_EQUALITY(e1, e2, ty, eq.source)
      end

      EQUATION_ARRAY_EQUALITY(__) => begin
        @assign ty = Type.mapDims(eq.ty, evaluateDimension)
        @assign e2 = evaluateExp(eq.rhs, constVariability)
        EQUATION_ARRAY_EQUALITY(eq.lhs, e2, ty, eq.source)
      end

      EQUATION_FOR(__) => begin
        @assign eq.range = Util.applyOption(
          eq.range,
          (constVariability) -> evaluateExp(constVariability = constVariability),
        )
        @assign eq.body = evaluateEquations(eq.body, constVariability)
        eq
      end

      EQUATION_IF(__) => begin
        @assign eq.branches =
          List(evaluateEqBranch(b, constVariability) for b in eq.branches)
        eq
      end

      P_Equation.Equation.WHEN(__) => begin
        @assign eq.branches =
          List(evaluateEqBranch(b, constVariability) for b in eq.branches)
        eq
      end

      P_Equation.Equation.ASSERT(__) => begin
        @assign e1 = evaluateExp(eq.condition, constVariability)
        @assign e2 = evaluateExp(eq.message, constVariability)
        @assign e3 = evaluateExp(eq.level, constVariability)
        P_Equation.Equation.ASSERT(e1, e2, e3, eq.source)
      end

      P_Equation.Equation.TERMINATE(__) => begin
        @assign eq.message = evaluateExp(eq.message, constVariability)
        eq
      end

      EQUATION_REINIT(__) => begin
        @assign eq.reinitExp = evaluateExp(eq.reinitExp, constVariability)
        eq
      end

      P_Equation.Equation.NORETCALL(__) => begin
        @assign eq.exp = evaluateExp(eq.exp, constVariability)
        eq
      end

      _ => begin
        eq
      end
    end
  end
  return eq
end

function evaluateEqBranch(branch::Branch, constVariability::Variability)::Branch
  local outBranch::Branch

  @assign outBranch = begin
    local condition::Expression
    local body::List{Equation}
    @match branch begin
      BRANCH(condition = condition, body = body) => begin
        @assign condition =
          evaluateExp(condition, constVariability = Variability.STRUCTURAL_PARAMETER)
        @assign body = evaluateEquations(body, constVariability)
        BRANCH(condition, branch.conditionVar, body)
      end

      _ => begin
        branch
      end
    end
  end
  return outBranch
end

function evaluateAlgorithms(
  algs::List{<:Algorithm},
  constVariability::Variability,
)::List{Algorithm}
  local outAlgs::List{Algorithm} =
    List(evaluateAlgorithm(a, constVariability) for a in algs)
  return outAlgs
end

function evaluateAlgorithm(alg::Algorithm, constVariability::Variability)::Algorithm

  @assign alg.statements = evaluateStatements(alg.statements, constVariability)
  return alg
end

function evaluateStatements(
  stmts::List{<:Statement},
  constVariability::Variability,
)::List{Statement}
  local outStmts::List{Statement} =
    List(evaluateStatement(s, constVariability) for s in stmts)
  return outStmts
end

function evaluateStatement(stmt::Statement, constVariability::Variability)::Statement

  @assign stmt = begin
    local e1::Expression
    local e2::Expression
    local e3::Expression
    local ty::M_Type
    @match stmt begin
      P_Statement.Statement.ASSIGNMENT(__) => begin
        @assign ty = Type.mapDims(stmt.ty, evaluateDimension)
        @assign e1 = evaluateExp(stmt.lhs, constVariability)
        @assign e2 = evaluateExp(stmt.rhs, constVariability)
        P_Statement.Statement.ASSIGNMENT(e1, e2, ty, stmt.source)
      end

      P_Statement.Statement.FOR(__) => begin
        @assign stmt.range = Util.applyOption(
          stmt.range,
          (constVariability) -> evaluateExp(constVariability = constVariability),
        )
        @assign stmt.body = evaluateStatements(stmt.body, constVariability)
        stmt
      end

      P_Statement.Statement.IF(__) => begin
        @assign stmt.branches =
          List(evaluateStmtBranch(b, constVariability) for b in stmt.branches)
        stmt
      end

      P_Statement.Statement.WHEN(__) => begin
        @assign stmt.branches =
          List(evaluateStmtBranch(b, constVariability) for b in stmt.branches)
        stmt
      end

      P_Statement.Statement.ASSERT(__) => begin
        @assign e1 = evaluateExp(stmt.condition, constVariability)
        @assign e2 = evaluateExp(stmt.message, constVariability)
        @assign e3 = evaluateExp(stmt.level, constVariability)
        P_Statement.Statement.ASSERT(e1, e2, e3, stmt.source)
      end

      P_Statement.Statement.TERMINATE(__) => begin
        @assign stmt.message = evaluateExp(stmt.message, constVariability)
        stmt
      end

      P_Statement.Statement.NORETCALL(__) => begin
        @assign stmt.exp = evaluateExp(stmt.exp, constVariability)
        stmt
      end

      P_Statement.Statement.WHILE(__) => begin
        @assign stmt.condition = evaluateExp(stmt.condition, constVariability)
        @assign stmt.body = evaluateStatements(stmt.body, constVariability)
        stmt
      end

      _ => begin
        stmt
      end
    end
  end
  return stmt
end

function evaluateStmtBranch(
  branch::Tuple{<:Expression, List{<:Statement}},
  constVariability::Variability,
)::Tuple{Expression, List{Statement}}
  local outBranch::Tuple{Expression, List{Statement}}

  local cond::Expression
  local body::List{Statement}

  @assign (cond, body) = branch
  @assign cond = evaluateExp(cond, constVariability = Variability.STRUCTURAL_PARAMETER)
  @assign body = evaluateStatements(body, constVariability)
  @assign outBranch = (cond, body)
  return outBranch
end

function evaluateFunction(func::M_Function)::M_Function

  local cls::Class
  local fn_body::Algorithm
  local sections::Sections

  if !P_Function.isEvaluated(func)
    P_Function.markEvaluated(func)
    @assign func =
      P_Function.mapExp(func, (func.node) -> evaluateFuncExp(fnNode = func.node))
    for fn_der in func.derivatives
      for der_fn in P_Function.getCachedFuncs(fn_der.derivativeFn)
        evaluateFunction(der_fn)
      end
    end
  end
  return func
end

function evaluateFuncExp(exp::Expression, fnNode::InstNode)::Expression
  local outExp::Expression

  @assign outExp = evaluateFuncExpTraverser(exp, fnNode, false)
  return outExp
end

function evaluateFuncExpTraverser(
  exp::Expression,
  fnNode::InstNode,
  changed::Bool,
)::Tuple{Expression, Bool}
  local outChanged::Bool
  local outExp::Expression

  local e::Expression

  @assign (e, outChanged) = P_Expression.Expression.mapFoldShallow(
    exp,
    (fnNode) -> evaluateFuncExpTraverser(fnNode = fnNode),
    false,
  )
  @assign outExp = begin
    @match e begin
      CREF_EXPRESSION(__) => begin
        if !isLocalFunctionVariable(e.cref, fnNode)
          @assign outExp = Ceval.evalCref(
            e.cref,
            e,
            Ceval.P_EvalTarget.IGNORE_ERRORS(),
            evalSubscripts = false,
          )
          @assign outExp = P_Expression.Expression.stripBindingInfo(outExp)
          @assign outChanged = true
        elseif outChanged
          @assign outExp = CREF_EXPRESSION(
            getSubscriptedType(e.cref),
            e.cref,
          )
        else
          @assign outExp = e
        end
        #=  If the cref's subscripts changed, recalculate its type.
        =#
        outExp
      end

      _ => begin
        if outChanged
          P_Expression.Expression.retype(e)
        else
          e
        end
      end
    end
  end
  @assign outChanged = changed || outChanged
  return (outExp, outChanged)
end

function isLocalFunctionVariable(cref::ComponentRef, fnNode::InstNode)::Bool
  local res::Bool

  local node::InstNode

  if isPackageConstant(cref)
    @assign res = false
  elseif nodeVariability(cref) <= Variability.PARAMETER
    @assign node =
      derivedParent(node(firstNonScope(
        cref,
      )))
    @assign res = refEqual(fnNode, node)
  else
    @assign res = true
  end
  return res
end

@exportAll()
end
