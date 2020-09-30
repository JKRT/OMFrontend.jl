module NFScalarize

using MetaModelica
using ExportAll

#= /*
* This file is part of OpenModelica.
*
* Copyright (c) 1998-CurrentYear, Linköping University,
* Department of Computer and Information Science,
* SE-58183 Linköping, Sweden.
*
* All rights reserved.
*
* THIS PROGRAM IS PROVIDED UNDER THE TERMS OF GPL VERSION 3
* AND THIS OSMC PUBLIC LICENSE (OSMC-PL).
* ANY USE, REPRODUCTION OR DISTRIBUTION OF THIS PROGRAM CONSTITUTES RECIPIENT'S
* ACCEPTANCE OF THE OSMC PUBLIC LICENSE.
*
* The OpenModelica software and the Open Source Modelica
* Consortium (OSMC) Public License (OSMC-PL) are obtained
* from Linköping University, either from the above address,
* from the URLs: http:www.ida.liu.se/projects/OpenModelica or
* http:www.openmodelica.org, and in the OpenModelica distribution.
* GNU version 3 is obtained from: http:www.gnu.org/copyleft/gpl.html.
*
* This program is distributed WITHOUT ANY WARRANTY; without
* even the implied warranty of  MERCHANTABILITY or FITNESS
* FOR A PARTICULAR PURPOSE, EXCEPT AS EXPRESSLY SET FORTH
* IN THE BY RECIPIENT SELECTED SUBSIDIARY LICENSE CONDITIONS
* OF OSMC-PL.
*
* See the full OSMC Public License conditions for more details.
*
*/ =#
import ..NFFlatModel
FlatModel = NFFlatModel
import ..NFFlatten.FunctionTree

import ..ExecStat.execStat
import ..P_NFComponentRef
P_ComponentRef = P_NFComponentRef
ComponentRef = P_NFComponentRef.NFComponentRef
import ..P_NFType
P_M_Type = P_NFType
M_Type = NFType
import ..P_NFExpression
P_Expression = P_NFExpression
Expression = P_NFExpression.NFExpression
import ..NFBinding.P_Binding
import ..P_NFEquation
P_Equation = P_NFEquation
Equation = P_NFEquation.NFEquation
import ..P_NFExpressionIterator
P_ExpressionIterator = P_NFExpressionIterator
ExpressionIterator = P_NFExpressionIterator.NFExpressionIterator
import ..P_NFDimension
P_Dimension = P_NFDimension
Dimension = P_NFDimension.NFDimension
import ..MetaModelica.Dangerous.listReverseInPlace
import ..MetaModelica.Dangerous.arrayCreateNoInit
import ..P_NFVariable
P_Variable = P_NFVariable
Variable = P_NFVariable.NFVariable
import ..NFComponent.P_Component
import ..NFPrefixes.Visibility
import ..NFPrefixes.Variability
import ListUtil
import ..ElementSource
import DAE
import ..P_NFStatement
P_Statement = P_NFStatement
Statement = P_NFStatement.NFStatement
import ..P_NFAlgorithm
P_Algorithm = P_NFAlgorithm
Algorithm = P_NFAlgorithm.NFAlgorithm
import ..P_NFExpandExp
P_ExpandExp = P_NFExpandExp
ExpandExp = P_NFExpandExp.NFExpandExp

function scalarize(flatModel::FlatModel, name::String)::FlatModel

  local vars::List{Variable} = nil
  local eql::List{Equation} = nil
  local ieql::List{Equation} = nil
  local alg::List{Algorithm} = nil
  local ialg::List{Algorithm} = nil

  for c in flatModel.variables
    @assign vars = scalarizeVariable(c, vars)
  end
  @assign flatModel.variables = listReverseInPlace(vars)
  @assign flatModel.equations =
    mapExpList(flatModel.equations, expandComplexCref)
  @assign flatModel.equations = scalarizeEquations(flatModel.equations)
  @assign flatModel.initialEquations =
    mapExpList(flatModel.initialEquations, expandComplexCref)
  @assign flatModel.initialEquations = scalarizeEquations(flatModel.initialEquations)
  @assign flatModel.algorithms = List(scalarizeAlgorithm(a) for a in flatModel.algorithms)
  @assign flatModel.initialAlgorithms =
    List(scalarizeAlgorithm(a) for a in flatModel.initialAlgorithms)
  execStat(getInstanceName() + "(" + name + ")")
  return flatModel
end

function scalarizeVariable(var::Variable, vars::List{<:Variable})::List{Variable}

  local name::ComponentRef
  local binding::Binding
  local ty::M_Type
  local vis::VisibilityType
  local attr::Attributes
  local ty_attr::List{Tuple{String, Binding}}
  local cmt::Option{SCode.Comment}
  local info::SourceInfo
  local binding_iter::ExpressionIterator
  local crefs::List{ComponentRef}
  local exp::Expression
  local v::Variable
  local ty_attr_names::List{String}
  local ty_attr_iters::Array{ExpressionIterator}
  local bind_var::VariabilityType

  if Type.isArray(var.ty)
    try
      @match VARIABLE(
        name,
        ty,
        binding,
        vis,
        attr,
        ty_attr,
        cmt,
        info,
      ) = var
      @assign crefs = scalarize(name)
      if listEmpty(crefs)
        return vars
      end
      @assign ty = arrayElementType(ty)
      @assign (ty_attr_names, ty_attr_iters) = scalarizeTypeAttributes(ty_attr)
      if isBound(binding)
        @assign binding_iter =
          P_ExpressionIterator.ExpressionIterator.fromExp(expandComplexCref(getTypedExp(
            binding,
          )))
        @assign bind_var = variability(binding)
        for cr in crefs
          @assign (binding_iter, exp) =
            P_ExpressionIterator.ExpressionIterator.next(binding_iter)
          @assign binding = FLAT_BINDING(exp, bind_var)
          @assign ty_attr = nextTypeAttributes(ty_attr_names, ty_attr_iters)
          @assign vars = _cons(
            VARIABLE(cr, ty, binding, vis, attr, ty_attr, cmt, info),
            vars,
          )
        end
      else
        for cr in crefs
          @assign ty_attr = nextTypeAttributes(ty_attr_names, ty_attr_iters)
          @assign vars = _cons(
            VARIABLE(cr, ty, binding, vis, attr, ty_attr, cmt, info),
            vars,
          )
        end
      end
    catch
      Error.assertion(
        false,
        getInstanceName() +
        " failed on " +
        P_Variable.Variable.toString(var, printBindingType = true),
        var.info,
      )
    end
  else
    @assign var.binding = mapExp(var.binding, expandComplexCref_traverser)
    @assign vars = _cons(var, vars)
  end
  return vars
end

function scalarizeTypeAttributes(
  attrs::List{<:Tuple{<:String, Binding}},
)::Tuple{List{String}, Array{ExpressionIterator}}
  local iters::Array{ExpressionIterator}
  local names::List{String} = nil

  local len::Integer
  local i::Integer
  local name::String
  local binding::Binding

  @assign len = listLength(attrs)
  @assign iters =
    arrayCreateNoInit(len, P_ExpressionIterator.ExpressionIterator.NONE_ITERATOR())
  @assign i = len
  for attr in attrs
    @assign (name, binding) = attr
    @assign names = _cons(name, names)
    arrayUpdate(iters, i, P_ExpressionIterator.ExpressionIterator.fromBinding(binding))
    @assign i = i - 1
  end
  return (names, iters)
end

function nextTypeAttributes(
  names::List{<:String},
  iters::Array{<:ExpressionIterator},
)::List{Tuple{String, Binding}}
  local attrs::List{Tuple{String, Binding}} = nil

  local i::Integer = 1
  local iter::ExpressionIterator
  local exp::Expression

  for name in names
    @assign (iter, exp) = P_ExpressionIterator.ExpressionIterator.next(iters[i])
    arrayUpdate(iters, i, iter)
    @assign i = i + 1
    @assign attrs = _cons((name, FLAT_BINDING(exp, Variability.PARAMETER)), attrs)
  end
  return attrs
end

function expandComplexCref(exp::Expression)::Expression

  @assign exp = map(exp, expandComplexCref_traverser)
  return exp
end

function expandComplexCref_traverser(exp::Expression)::Expression

  @assign () = begin
    @match exp begin
      CREF_EXPRESSION(ty = ARRAY_TYPE(__)) => begin
        #=  Expand crefs where any of the prefix nodes are arrays. For example if
        =#
        #=  b in a.b.c is SomeType[2] we expand it into {a.b[1].c, a.b[2].c}.
        =#
        #=  TODO: This is only done due to backend issues and shouldn't be
        =#
        #=        necessary.
        =#
        if isComplexArray(exp.cref)
          @assign exp = P_ExpandExp.ExpandExp.expand(exp)
        end
        ()
      end

      _ => begin
        ()
      end
    end
  end
  return exp
end

function scalarizeEquations(eql::List{<:Equation})::List{Equation}
  local equations::List{Equation} = nil

  for eq in eql
    @assign equations = scalarizeEquation(eq, equations)
  end
  @assign equations = listReverseInPlace(equations)
  return equations
end

function scalarizeEquation(eq::Equation, equations::List{<:Equation})::List{Equation}

  @assign equations = begin
    local lhs_iter::ExpressionIterator
    local rhs_iter::ExpressionIterator
    local lhs::Expression
    local rhs::Expression
    local ty::M_Type
    local src::DAE.ElementSource
    local info::SourceInfo
    local eql::List{Equation}
    @match eq begin
      EQUATION_EQUALITY(
        lhs = lhs,
        rhs = rhs,
        ty = ty,
        source = src,
      ) where {(Type.isArray(ty))} => begin
        if P_Expression.Expression.hasArrayCall(lhs) ||
           P_Expression.Expression.hasArrayCall(rhs)
          @assign equations =
            _cons(EQUATION_ARRAY_EQUALITY(lhs, rhs, ty, src), equations)
        else
          @assign lhs_iter = P_ExpressionIterator.ExpressionIterator.fromExp(lhs)
          @assign rhs_iter = P_ExpressionIterator.ExpressionIterator.fromExp(rhs)
          @assign ty = arrayElementType(ty)
          while P_ExpressionIterator.ExpressionIterator.hasNext(lhs_iter)
            if !P_ExpressionIterator.ExpressionIterator.hasNext(rhs_iter)
              Error.addInternalError(
                getInstanceName() +
                " could not expand rhs " +
                toString(eq.rhs),
                ElementSource.getInfo(src),
              )
            end
            @assign (lhs_iter, lhs) =
              P_ExpressionIterator.ExpressionIterator.next(lhs_iter)
            @assign (rhs_iter, rhs) =
              P_ExpressionIterator.ExpressionIterator.next(rhs_iter)
            @assign equations =
              _cons(EQUATION_EQUALITY(lhs, rhs, ty, src), equations)
          end
        end
        equations
      end

      EQUATION_ARRAY_EQUALITY(__) => begin
        _cons(EQUATION_ARRAY_EQUALITY(eq.lhs, eq.rhs, eq.ty, eq.source), equations)
      end

      EQUATION_CONNECT(__) => begin
        equations
      end

      EQUATION_IF(__) => begin
        scalarizeIfEquation(eq.branches, eq.source, equations)
      end

      P_Equation.Equation.WHEN(__) => begin
        scalarizeWhenEquation(eq.branches, eq.source, equations)
      end

      _ => begin
        _cons(eq, equations)
      end
    end
  end
  return equations
end

function scalarizeIfEquation(
  branches::List{<:Equation},
  source::DAE.ElementSource,
  equations::List{<:Equation},
)::List{Equation}

  local bl::List{P_Equation.Equation} = nil
  local cond::Expression
  local body::List{Equation}
  local var::VariabilityType

  for b in branches
    @match P_Equation.Equation.BRANCH(cond, var, body) = b
    @assign body = scalarizeEquations(body)
    if !listEmpty(body)
      @assign bl = _cons(P_Equation.Equation.makeBranch(cond, body, var), bl)
    end
  end
  #=  Remove branches with no equations after scalarization.
  =#
  #=  Add the scalarized if equation to the list of equations unless we don't
  =#
  #=  have any branches left.
  =#
  if !listEmpty(bl)
    @assign equations =
      _cons(EQUATION_IF(listReverseInPlace(bl), source), equations)
  end
  return equations
end

function scalarizeWhenEquation(
  branches::List{<:Equation},
  source::DAE.ElementSource,
  equations::List{<:Equation},
)::List{Equation}

  local bl::List{P_Equation.Equation} = nil
  local cond::Expression
  local body::List{Equation}
  local var::VariabilityType

  for b in branches
    @match P_Equation.Equation.BRANCH(cond, var, body) = b
    @assign body = scalarizeEquations(body)
    if Type.isArray(typeOf(cond))
      @assign cond = P_ExpandExp.ExpandExp.expand(cond)
    end
    @assign bl = _cons(P_Equation.Equation.makeBranch(cond, body, var), bl)
  end
  @assign equations =
    _cons(P_Equation.Equation.WHEN(listReverseInPlace(bl), source), equations)
  return equations
end

function scalarizeAlgorithm(alg::Algorithm)::Algorithm

  @assign alg.statements = scalarizeStatements(alg.statements)
  return alg
end

function scalarizeStatements(stmts::List{<:Statement})::List{Statement}
  local statements::List{Statement} = nil

  for s in stmts
    @assign statements = scalarizeStatement(s, statements)
  end
  @assign statements = listReverseInPlace(statements)
  return statements
end

function scalarizeStatement(stmt::Statement, statements::List{<:Statement})::List{Statement}

  @assign statements = begin
    @match stmt begin
      P_Statement.Statement.FOR(__) => begin
        _cons(
          P_Statement.Statement.FOR(
            stmt.iterator,
            stmt.range,
            scalarizeStatements(stmt.body),
            stmt.source,
          ),
          statements,
        )
      end

      P_Statement.Statement.IF(__) => begin
        scalarizeIfStatement(stmt.branches, stmt.source, statements)
      end

      P_Statement.Statement.WHEN(__) => begin
        scalarizeWhenStatement(stmt.branches, stmt.source, statements)
      end

      P_Statement.Statement.WHILE(__) => begin
        _cons(
          P_Statement.Statement.WHILE(
            stmt.condition,
            scalarizeStatements(stmt.body),
            stmt.source,
          ),
          statements,
        )
      end

      _ => begin
        _cons(stmt, statements)
      end
    end
  end
  return statements
end

function scalarizeIfStatement(
  branches::List{<:Tuple{<:Expression, List{<:Statement}}},
  source::DAE.ElementSource,
  statements::List{<:Statement},
)::List{Statement}

  local bl::List{Tuple{Expression, List{Statement}}} = nil
  local cond::Expression
  local body::List{Statement}

  for b in branches
    @assign (cond, body) = b
    @assign body = scalarizeStatements(body)
    if !listEmpty(body)
      @assign bl = _cons((cond, body), bl)
    end
  end
  #=  Remove branches with no statements after scalarization.
  =#
  #=  Add the scalarized if statement to the list of statements unless we don't
  =#
  #=  have any branches left.
  =#
  if !listEmpty(bl)
    @assign statements =
      _cons(P_Statement.Statement.IF(listReverseInPlace(bl), source), statements)
  end
  return statements
end

function scalarizeWhenStatement(
  branches::List{<:Tuple{<:Expression, List{<:Statement}}},
  source::DAE.ElementSource,
  statements::List{<:Statement},
)::List{Statement}

  local bl::List{Tuple{Expression, List{Statement}}} = nil
  local cond::Expression
  local body::List{Statement}

  for b in branches
    @assign (cond, body) = b
    @assign body = scalarizeStatements(body)
    if Type.isArray(typeOf(cond))
      @assign cond = P_ExpandExp.ExpandExp.expand(cond)
    end
    @assign bl = _cons((cond, body), bl)
  end
  @assign statements =
    _cons(P_Statement.Statement.WHEN(listReverseInPlace(bl), source), statements)
  return statements
end

@exportAll()
end
