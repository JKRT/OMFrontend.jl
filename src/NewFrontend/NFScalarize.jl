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
  local vars::List{Variable} = nil
  local eql::List{Equation} = nil
  local ieql::List{Equation} = nil
  local alg::List{Algorithm} = nil
  local ialg::List{Algorithm} = nil
  for c in flatModel.variables
    vars = scalarizeVariable(c, vars)
  end
  @assign flatModel.variables = listReverseInPlace(vars)
  @assign flatModel.equations = mapExpList(flatModel.equations, expandComplexCref)
  @assign flatModel.equations = scalarizeEquations(flatModel.equations)
  @assign flatModel.initialEquations = mapExpList(flatModel.initialEquations, expandComplexCref)
  @assign flatModel.initialEquations = scalarizeEquations(flatModel.initialEquations)
  @assign flatModel.algorithms = list(scalarizeAlgorithm(a) for a in flatModel.algorithms)
  @assign flatModel.initialAlgorithms =
    list(scalarizeAlgorithm(a) for a in flatModel.initialAlgorithms)
  #execStat(getInstanceName() + "(" + name + ")")
  return flatModel
end

function scalarizeVariable(var::Variable, vars::List{<:Variable})
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
            vars = _cons(
              VARIABLE(cr, ty, binding, vis, attr, ty_attr, cmt, info),
              vars,
            )
          end
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
    @assign vars = _cons(var, vars)
  end
  return vars
end

function scalarizeTypeAttributes(
  attrs::List{<:Tuple{<:String, Binding}},
)::Tuple{List{String}, Array{ExpressionIterator}}
  local iters::Vector{ExpressionIterator}
  local names::List{String} = nil
  local len::Int
  local i::Int
  local name::String
  local binding::Binding
  len = listLength(attrs)
  iters = arrayCreateNoInit(len, EXPRESSION_NONE_ITERATOR())
  i = len
  for attr in attrs
    (name, binding) = attr
    names = _cons(name, names)
    arrayUpdate(iters, i, fromBinding(binding))
    i = i - 1
  end
  return (names, iters)
end

function nextTypeAttributes(
  names::List{<:String},
  iters::Vector{<:ExpressionIterator},
)::List{Tuple{String, Binding}}
  local attrs::List{Tuple{String, Binding}} = nil
  local i::Int = 1
  local iter::ExpressionIterator
  local exp::Expression
  for name in names
    (iter, exp) = next(iters[i])
    arrayUpdate(iters, i, iter)
    i = i + 1
    attrs = _cons((name, FLAT_BINDING(exp, Variability.PARAMETER)), attrs)
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

function scalarizeEquations(eql::List{<:Equation})
  local equations::List{Equation} = nil
  for eq in eql
    equations = scalarizeEquation(eq, equations)
  end
  equations = listReverseInPlace(equations)
  return equations
end

function scalarizeEquation(eq::Equation, equations::List{<:Equation})
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
            _cons(EQUATION_ARRAY_EQUALITY(lhs, rhs, ty, src), equations)
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

      EQUATION_WHEN(__) => begin
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
  branches::List{<:Equation_Branch},
  source::DAE.ElementSource,
  equations::List{<:Equation},
)::List{Equation}
  local bl::List{Equation_Branch} = nil
  local cond::Expression
  local body::List{Equation}
  local var::VariabilityType
  for b in branches
    @match EQUATION_BRANCH(cond, var, body) = b
    body = scalarizeEquations(body)
    if !listEmpty(body)
      bl = _cons(makeBranch(cond, body, var), bl)
    end
  end
  #=  Remove branches with no equations after scalarization.
  =#
  #=  Add the scalarized if equation to the list of equations unless we don't
  =#
  #=  have any branches left.
  =#
  if !listEmpty(bl)
    equations =
      _cons(EQUATION_IF(listReverseInPlace(bl), source), equations)
  end
  return equations
end

function scalarizeWhenEquation(
  branches::List{<:Equation_Branch},
  source::DAE.ElementSource,
  equations::List{<:Equation},
)::List{Equation}
  local bl::List{Equation_Branch} = nil
  local cond::Expression
  local body::List{Equation}
  local var::VariabilityType
  for b in branches
    @match EQUATION_BRANCH(cond, var, body) = b
    body = scalarizeEquations(body)
    if isArray(typeOf(cond))
      (cond, _) = expand(cond)
    end
    bl = _cons(makeBranch(cond, body, var), bl)
  end
  equations =
    _cons(EQUATION_WHEN(listReverseInPlace(bl), source), equations)
  return equations
end

function scalarizeAlgorithm(alg::Algorithm)::Algorithm
  @assign alg.statements = scalarizeStatements(alg.statements)
  return alg
end

function scalarizeStatements(stmts::List{<:Statement})::List{Statement}
  local statements::List{Statement} = nil
  for s in stmts
    statements = scalarizeStatement(s, statements)
  end
  statements = listReverseInPlace(statements)
  return statements
end

function scalarizeStatement(stmt::Statement, statements::List{<:Statement})::List{Statement}
  statements = begin
    @match stmt begin
      ALG_FOR(__) => begin
        _cons(
          ALG_FOR(
            stmt.iterator,
            stmt.range,
            scalarizeStatements(stmt.body),
            stmt.source,
          ),
          statements,
        )
      end
      ALG_IF(__) => begin
        scalarizeIfStatement(stmt.branches, stmt.source, statements)
      end
      ALG_WHEN(__) => begin
        scalarizeWhenStatement(stmt.branches, stmt.source, statements)
      end
      ALG_WHILE(__) => begin
        _cons(
          ALG_WHILE(
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
    (cond, body) = b
    body = scalarizeStatements(body)
    if !listEmpty(body)
      bl = _cons((cond, body), bl)
    end
  end
  #=  Remove branches with no statements after scalarization.
  =#
  #=  Add the scalarized if statement to the list of statements unless we don't
  =#
  #=  have any branches left.
  =#
  if !listEmpty(bl)
    statements =
      _cons(ALG_IF(listReverseInPlace(bl), source), statements)
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
    (cond, body) = b
    body = scalarizeStatements(body)
    if isArray(typeOf(cond))
      cond = expand(cond)
    end
    bl = _cons((cond, body), bl)
  end
  statements =
    _cons(ALG_WHEN(listReverseInPlace(bl), source), statements)
  return statements
end
