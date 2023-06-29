#=
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
=#

function scalarize(flatModel::FlatModel, name::String)::FlatModel
  local vars::Vector{Variable} = Variable[]
  for c in flatModel.variables
    vars = scalarizeVariable(c, vars)
  end
  @assign flatModel.variables = vars
  @assign flatModel.equations = mapExpList(flatModel.equations, expandComplexCref)
  @assign flatModel.equations = scalarizeEquations(flatModel.equations)
  @assign flatModel.initialEquations = mapExpList(flatModel.initialEquations, expandComplexCref)
  @assign flatModel.initialEquations = scalarizeEquations(flatModel.initialEquations)
  @assign flatModel.algorithms =
    Algorithm[scalarizeAlgorithm(a) for a in flatModel.algorithms]
  @assign flatModel.initialAlgorithms =
    Algorithm[scalarizeAlgorithm(a) for a in flatModel.initialAlgorithms]
  #execStat(getInstanceName() + "(" + name + ")")
  return flatModel
end

function scalarizeVariable(var::Variable, vars::Vector{Variable})
  local name::ComponentRef
  local binding::Binding
  local ty::M_Type
  local vis::VisibilityType
  local attr::Attributes
  local ty_attr::Vector{Tuple{String, Binding}}
  local cmt::Option{SCode.Comment}
  local info::SourceInfo
  local binding_iter::ExpressionIterator
  local crefs::List{ComponentRef}
  local exp::Expression
  local v::Variable
  local ty_attr_names::Vector{String}
  local ty_attr_iters::Vector{ExpressionIterator}
  local bind_var::VariabilityType
  if isArray(var.ty)
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
      crefs = scalarize(name)
      if listEmpty(crefs)
        return vars
      end
      ty = arrayElementType(ty)
      (ty_attr_names, ty_attr_iters) = scalarizeTypeAttributes(ty_attr)
      if isBound(binding)
        binding_iter = fromExpToExpressionIterator(expandComplexCref(getTypedExp(binding,)))
        bind_var = variability(binding)
        for cr in crefs
          if hasNext(binding_iter)
            (binding_iter, exp) = next(binding_iter)
            binding = FLAT_BINDING(exp, bind_var)
            ty_attr = nextTypeAttributes(ty_attr_names, ty_attr_iters)
            vars = push!(
              vars,
              VARIABLE(cr, ty, binding, vis, attr, ty_attr, cmt, info)
            )
          end
        end
      else
        for cr in crefs
          ty_attr = nextTypeAttributes(ty_attr_names, ty_attr_iters)
          vars = push!(vars,
                       VARIABLE(cr, ty, binding, vis, attr, ty_attr, cmt, info))
        end
      end
    catch e
      # Error.assertion(
      #   false,
      #   getInstanceName() +
      #   " failed on " +
      #   toString(var, printBindingType = true),
      #   var.info,
      #)
      errorStr = toString(var, "", false)
      @error "scalarization failed on the following component: " * errorStr e
      throw(e)
    end
  else
    @assign var.binding = mapExp(var.binding, expandComplexCref_traverser)
    push!(vars, var)
  end
  return vars
end

function scalarizeTypeAttributes(
  attrs::Vector{Tuple{String, Binding}},
)
  local iters::Vector{ExpressionIterator}
  local names::Vector{String} = String[]
  local len::Int
  local i::Int
  local name::String
  local binding::Binding
  len = length(attrs)
  iters = arrayCreateNoInit(len, EXPRESSION_NONE_ITERATOR())
  i = len
  for attr in attrs
    (name, binding) = attr
    push!(names, name)
    iters[i] = fromBinding(binding)
    i = i - 1
  end
  return (names, iters)
end

function nextTypeAttributes(
  names::Vector{String},
  iters::Vector{ExpressionIterator},
)::Vector{Tuple{String, Binding}}
  local attrs = Tuple{String, Binding}[]
  local i::Int = 1
  local iter::ExpressionIterator
  local exp::Expression
  for name in names
    (iter, exp) = next(iters[i])
    iters[i] = iter
    i = i + 1
    attrs = push!(attrs, (name, FLAT_BINDING(exp, Variability.PARAMETER)))
  end
  return attrs
end

function expandComplexCref(exp::Expression)
  exp = map(exp, expandComplexCref_traverser)
  return exp
end

function expandComplexCref_traverser(exp::Expression)
  () = begin
    @match exp begin
      CREF_EXPRESSION(ty = TYPE_ARRAY(__)) => begin
        #=  Expand crefs where any of the prefix nodes are arrays. For example if
        =#
        #=  b in a.b.c is SomeType[2] we expand it into {a.b[1].c, a.b[2].c}.
        =#
        #=  TODO: This is only done due to backend issues and shouldn't be
        =#
        #=        necessary.
        =#
        if isComplexArray(exp.cref)
          exp = expand(exp)
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

function scalarizeEquations(eql::Vector{Equation})
  local equations::Vector{Equation} = Equation[]
  for eq in eql
    equations = scalarizeEquation(eq, equations)
  end
  return equations
end

function scalarizeEquation(@nospecialize(eq::Equation), equations::Vector{Equation})
  equations = begin
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
      ) where {(isArray(ty))} => begin
        if hasArrayCall(lhs) ||
           hasArrayCall(rhs)
          equations =
            push!(equations, EQUATION_ARRAY_EQUALITY(lhs, rhs, ty, src))
        else
          lhs_iter = fromExpToExpressionIterator(lhs)
          rhs_iter = fromExpToExpressionIterator(rhs)
          ty = arrayElementType(ty)
          while hasNext(lhs_iter)
            if !hasNext(rhs_iter)
              Error.addInternalError(
                getInstanceName() +
                " could not expand rhs " +
                toString(eq.rhs),
                ElementSource_getInfo(src),
              )
            end
            (lhs_iter, lhs) = next(lhs_iter)
            (rhs_iter, rhs) = next(rhs_iter)
            equations =
              push!(equations, EQUATION_EQUALITY(lhs, rhs, ty, src))
          end
        end
        equations
      end

      EQUATION_ARRAY_EQUALITY(__) => begin
        push!(equations, EQUATION_ARRAY_EQUALITY(eq.lhs, eq.rhs, eq.ty, eq.source))
      end

      EQUATION_CONNECT(__) => begin
        equations
      end

      EQUATION_IF(__) => begin
        scalarizeIfEquation(eq.branches, eq.source, equations)
      end

      EQUATION_WHEN(__) => begin
        scalarizeWhenEquation(eq.branches, eq.source, equations)
      end

      _ => begin
        push!(equations, eq)
      end
    end
  end
  return equations
end

"""
Remove branches with no equations after scalarization.
Add the scalarized if equation to the list of equations unless we don't
have any branches left.
"""
function scalarizeIfEquation(
  branches::Vector{Equation_Branch},
  source::DAE.ElementSource,
  equations::Vector{Equation},
)
  local bl::Vector{Equation_Branch} = Equation_Branch[]
  local cond::Expression
  local body::Vector{Equation}
  local var::VariabilityType
  for b in branches
    @match EQUATION_BRANCH(cond, var, body) = b
    body = scalarizeEquations(body)
    if !isempty(body)
      push!(bl, makeBranch(cond, body, var))
    end
  end
  if !isempty(bl)
    push!(equations, EQUATION_IF(bl, source))
  end
  return equations
end

function scalarizeWhenEquation(
  branches::Vector{Equation_Branch},
  source::DAE.ElementSource,
  equations::Vector{Equation},
  )
  local bl::Vector{Equation_Branch} = Equation_Branch[]
  local cond::Expression
  local body::Vector{Equation}
  local var::VariabilityType
  for b in branches
    @match EQUATION_BRANCH(cond, var, body) = b
    body = scalarizeEquations(body)
    if isArray(typeOf(cond))
      (cond, _) = expand(cond)
    end
    push!(bl, makeBranch(cond, body, var))
  end
  push!(equations, EQUATION_WHEN(bl, source))
  return equations
end

function scalarizeAlgorithm(alg::Algorithm)::Algorithm
  @assign alg.statements = scalarizeStatements(alg.statements)
  return alg
end

function scalarizeStatements(stmts::Vector{Statement})
  local statements::Vector{Statement} = Statement[]
  for s in stmts
    statements = scalarizeStatement(s, statements)
  end
  statements = statements
  return statements
end

function scalarizeStatement(stmt::Statement, statements::Vector{Statement})
  statements = begin
    @match stmt begin
      ALG_FOR(__) => begin
        push!(
          statements,
          ALG_FOR(
            stmt.iterator,
            stmt.range,
            scalarizeStatements(stmt.body),
            stmt.source,
          ),
        )
      end
      ALG_IF(__) => begin
        scalarizeIfStatement(stmt.branches, stmt.source, statements)
      end
      ALG_WHEN(__) => begin
        scalarizeWhenStatement(stmt.branches, stmt.source, statements)
      end
      ALG_WHILE(__) => begin
        push!(
          statements,
          ALG_WHILE(
            stmt.condition,
            scalarizeStatements(stmt.body),
            stmt.source,
          ),
        )
      end
      _ => begin
        push!(statements, stmt)#_cons(stmt, statements)
      end
    end
  end
  return statements
end

function scalarizeIfStatement(
  branches::Vector{Tuple{Expression, Vector{Statement}}},
  source::DAE.ElementSource,
  statements::Vector{Statement},
)
  local bl::List{Tuple{Expression, Vector{Statement}}} = Tuple{Expression, Vector{Statement}}[]
  local cond::Expression
  local body::Vector{Statement}
  for b in branches
    (cond, body) = b
    body = scalarizeStatements(body)
    if !isempty(body)
      push!(bl, (cond, body))
    end
  end
  #=  Remove branches with no statements after scalarization.
  =#
  #=  Add the scalarized if statement to the list of statements unless we don't
  =#
  #=  have any branches left.
  =#
  if !isempty(bl)
    push!(statements, ALG_IF(bl, source))
  end
  return statements
end

"""
  Scalarizes a when statement.
"""
function scalarizeWhenStatement(
  branches::Vector{Tuple{Expression, Vector{Statement}}},
  source::DAE.ElementSource,
  statements::Vector{Statement},
)
  local bl::Vector{Tuple{Expression, Vector{Statement}}} = Tuple{Expression, Vector{Statement}}[]
  local cond::Expression
  local body::Vector{Statement}
  for b in branches
    (cond, body) = b
    body = scalarizeStatements(body)
    if isArray(typeOf(cond))
      cond = expand(cond)
    end
    push!(bl, (cond, body))
  end
  statements = push!(statements, ALG_WHEN(bl, source))
  return statements
end
