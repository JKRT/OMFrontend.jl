const SimplifyFunc = Function
const MakeElement = Function
const MakeFunc = Function

function simplifyFlatModel(flatModel::FlatModel)::FlatModel
  @assign flatModel.variables = Variable[simplifyVariable(v) for v in flatModel.variables]
  @assign flatModel.equations = simplifyEquations(flatModel.equations)
  @assign flatModel.initialEquations = simplifyEquations(flatModel.initialEquations)
  @assign flatModel.algorithms = simplifyAlgorithms(flatModel.algorithms)
  @assign flatModel.initialAlgorithms = simplifyAlgorithms(flatModel.initialAlgorithms)
#  execStat(getInstanceName()) TODO
  return flatModel
end

function simplifyVariable(var::Variable)::Variable
  @assign var.binding = simplifyBinding(var.binding)
  @assign var.typeAttributes = [simplifyTypeAttribute(a) for a in var.typeAttributes]
  return var
end

function simplifyBinding(binding::Binding)::Binding

  local exp::Expression
  local sexp::Expression

  if isBound(binding)
    @assign exp = getTypedExp(binding)
    @assign sexp = simplify(exp)
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

   (name, binding) = attribute
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
      DIMENSION_EXP(__) => begin
        @assign e = simplify(dim.exp)
        if referenceEq(e, dim.exp)
          dim
        else
          fromExp(e, dim.var)
        end
      end

      _ => begin
        dim
      end
    end
  end
  return outDim
end

function simplifyEquations(eql::Vector{<:Equation})
  local outEql::Vector{Equation} = Equation[]
  for eq in eql
    outEql = simplifyEquation(eq, outEql)
  end
  outEql = outEql
  return outEql
end

function simplifyEquation(@nospecialize(eq::Equation), equations::Vector{Equation})
  equations = begin
    local e::Expression
    local lhs::Expression
    local rhs::Expression
    local ty::M_Type
    @match eq begin
      EQUATION_EQUALITY(__) => begin
        simplifyEqualityEquation(eq, equations)
      end

      EQUATION_ARRAY_EQUALITY(__) => begin
        @assign ty = mapDims(eq.ty, simplifyDimension)
        if !Type.isEmptyArray(ty)
          rhs = removeEmptyFunctionArguments(simplify(eq.rhs))
          push!(equations,
                EQUATION_ARRAY_EQUALITY(eq.lhs, rhs, ty, eq.source))
        end
        equations
      end

      EQUATION_IF(__) => begin
        simplifyIfEqBranches(eq.branches, eq.source, equations)
      end

      EQUATION_WHEN(__) => begin
        @assign eq.branches = begin
          [
            @match b begin
              EQUATION_BRANCH(__) =>
                begin
                  @assign b.condition = simplify(b.condition)
                  @assign b.body = simplifyEquations(b.body)
                  b
                end
            end
            for b in eq.branches
          ]
        end
        push!(equations, eq)
      end

      EQUATION_ASSERT(__) => begin
        @assign eq.condition = simplify(eq.condition)
        if isTrue(eq.condition)
          equations
        else
          push!(equations, eq)
        end
      end

      EQUATION_REINIT(__) => begin
        @assign eq.reinitExp = simplify(eq.reinitExp)
        push!(equations, eq)
      end

      EQUATION_NORETCALL(__) => begin
        e = simplify(eq.exp)
        if isCall(e)
          @assign eq.exp = removeEmptyFunctionArguments(e)
          push!(equations, eq)
        end
        equations
      end

      _ => begin
        push!(equations, eq)
      end
    end
  end
  return equations
end

function simplifyEqualityEquation(eq::EQUATION_EQUALITY, equations::Vector{Equation})
  local lhs::Expression
  local rhs::Expression
  local ty::M_Type
  local src::DAE.ElementSource
  if typeof(eq.rhs) == ARRAY_EXPRESSION
    if isEmptyArray(eq.rhs)
      return equations
    end
  end
  @match EQUATION_EQUALITY(lhs = lhs, rhs = rhs, ty = ty, source = src) = eq
  ty = mapDims(ty, simplifyDimension)
  if isEmptyArray(ty)
    return equations
  end
  lhs = simplify(lhs)
  lhs = removeEmptyTupleElements(lhs)
  rhs = simplify(rhs)
  rhs = removeEmptyFunctionArguments(rhs)
  equations = begin
    @match (lhs, rhs) begin
      (TUPLE_EXPRESSION(__), TUPLE_EXPRESSION(__)) => begin
        simplifyTupleElement(
          lhs.elements,
          rhs.elements,
          ty,
          src,
          makeEquality,
          equations,
        )
      end

      _ => begin
        push!(equations, EQUATION_EQUALITY(lhs, rhs, ty, src))
      end
    end
  end
  return equations
end

function simplifyAlgorithms(algs::Vector{Algorithm})
  local outAlgs::Vector{Algorithm} = Algorithm[]
  for alg in algs
    alg = simplifyAlgorithm(alg)
    if !isempty(alg.statements)
      push!(outAlgs, alg)
    end
  end
  return outAlgs
end

function simplifyAlgorithm(alg::Algorithm)::Algorithm

  @assign alg.statements = simplifyStatements(alg.statements)
  return alg
end

function simplifyStatements(stmts::Vector{Statement})
  local outStmts::Vector{Statement} = Statement[]
  for s in stmts
    outStmts = simplifyStatement(s, outStmts)
  end
  return outStmts
end

function simplifyStatement(stmt::Statement, statements::Vector{Statement})
  statements = begin
    local e::Expression
    local lhs::Expression
    local rhs::Expression
    local ty::M_Type
    local dim::Dimension
    local body::Vector{Statement}
    @match stmt begin
      ALG_ASSIGNMENT(__) => begin
        simplifyAssignment(stmt, statements)
      end
      ALG_FOR(range = SOME(e)) => begin
        ty = typeOf(e)
        dim = nthDimension(ty, 1)
        if !isZero(dim)
          @assign stmt.range = SOME(simplify(e))
          @assign stmt.body = simplifyStatements(stmt.body)
          push!(statements, stmt)
        end
        statements
      end
      ALG_IF(__) => begin
        simplifyIfStmtBranches(
          stmt.branches,
          stmt.source,
          makeIf,
          simplifyStatements,
          statements,
        )
      end
      ALG_WHEN(__) => begin
        @assign stmt.branches = [
          (simplify(Util.tuple21(b)), simplifyStatements(Util.tuple22(b))) for b in stmt.branches
            ]
        push!(statements, stmt)
      end
      ALG_NORETCALL(__) => begin
        e = simplify(stmt.exp)
        if isCall(e)
          @assign stmt.exp = removeEmptyFunctionArguments(e)
          push!(statements, stmt)
        end
        statements
      end
      _ => begin
        push!(statements, stmt)
      end
    end
  end
  return statements
end

function simplifyAssignment(stmt::Statement, statements::Vector{Statement})
  local lhs::Expression
  local rhs::Expression
  local rhs_exp::Expression
  local rhs_rest::List{Expression}
  local ty::M_Type
  local src::DAE.ElementSource
  @match ALG_ASSIGNMENT(lhs = lhs, rhs = rhs, ty = ty, source = src) = stmt
  ty = mapDims(ty, simplifyDimension)
  if isEmptyArray(ty)
    return statements
  end
  lhs = simplify(lhs)
  lhs = removeEmptyTupleElements(lhs)
  rhs = simplify(rhs)
  rhs = removeEmptyFunctionArguments(rhs)
  statements = begin
    @match (lhs, rhs) begin
      (TUPLE_EXPRESSION(__), TUPLE_EXPRESSION(__)) => begin
        simplifyTupleElement(
          lhs.elements,
          rhs.elements,
          ty,
          src,
          makeAssignment,
          statements,
        )
      end
      _ => begin
        push!(statements, ALG_ASSIGNMENT(lhs, rhs, ty, src))
      end
    end
  end
  return statements
end

"""
   Helper function to simplifyEqualityEquation/simplifyAssignment.
   Handles Expression.TUPLE() := Expression.TUPLE() assignments by splitting
   them into a separate assignment statement for each pair of tuple elements.
"""
function simplifyTupleElement(
  lhsTuple::List{Expression},
  rhsTuple::List{Expression},
  ty::M_Type,
  src::DAE.ElementSource,
  makeFn::MakeElement,
  statements::Vector{Statement},
)
  local rhs::Expression
  local rest_rhs::List{Expression} = rhsTuple
  local ety::M_Type
  local rest_ty::List{M_Type}
  @match TYPE_TUPLE(types = rest_ty) = ty
  for lhs in lhsTuple
    @match _cons(rhs, rest_rhs) = rest_rhs
    @match _cons(ety, rest_ty) = rest_ty
    if !isWildCref(lhs)
      #@assign statements = _cons(makeFn(lhs, rhs, ety, src), statements)
      push!(statements, makeFn(lhs, rhs, ety, src))
    end
  end
  return statements
end

""" #= Replaces tuple elements that has one or more zero dimension with _. =#"""
function removeEmptyTupleElements(exp::Expression)::Expression

   () = begin
    local tyl::List{M_Type}
    @match exp begin
      TUPLE_EXPRESSION(ty = TYPE_TUPLE(types = tyl)) => begin
        @assign exp.elements = list(@do_threaded_for if Type.isEmptyArray(t)
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

function removeEmptyFunctionArguments(@nospecialize(exp::Expression), isArg::Bool = false)::Expression
  local outExp::Expression
  local is_arg::Bool
  if isArg
     () = begin
      @match exp begin
        CREF_EXPRESSION(__) where {(isEmptyArray(exp.ty))} => begin
          @assign outExp =
            fillType(exp.ty, INTEGER_EXPRESSION(0))
          return
          ()
        end
        _ => begin
          ()
        end
      end
    end
  end
  @assign is_arg = isArg || isCall(exp)
  @assign outExp = mapShallow(
    exp,
    (x, y = is_arg) -> removeEmptyFunctionArguments(x, y)
  )
  return outExp
end

function simplifyIfEqBranches(
  branches::Vector{Equation_Branch},
  src::DAE.ElementSource,
  elements::Vector{Equation},
)::Vector{Equation}
  local cond::Expression
  local body::Vector{Equation}
  local var::VariabilityType
  local accum::Vector{Equation_Branch} = Equation_Branch[]
  for branch in branches
    accum = begin
      @match branch begin
        EQUATION_BRANCH(cond, var, body) => begin
          cond = simplify(cond)
          #=  A branch with condition true will always be selected when encountered. =#
          if isTrue(cond)
            if isempty(accum)
              for eq in body
                elements = simplifyEquation(eq, elements)
              end
              return elements
            else
              accum = push!(accum, makeBranch(cond, simplifyEquations(body)))
              elements = push!(
                elements,
                makeIf(accum, src)
              )
              return elements
            end
          elseif !isFalse(cond)
            @assign accum = push!(accum, makeBranch(cond, simplifyEquations(body)))
          end
          #=  If it's the first branch, remove the if and keep only the branch body.
          =#
          #=  Otherwise just discard the rest of the branches.
          =#
          #=  Keep branches that are neither literal true or false.
          =#
          accum
        end
        EQUATION_INVALID_BRANCH(
          branch = EQUATION_BRANCH(
            condition = cond,
            conditionVar = var,
          ),
        ) => begin
          if var <= Variability.STRUCTURAL_PARAMETER
            @assign cond = evalExp(cond)
          end
          #=  An invalid branch that can't be removed will trigger the errors
          =#
          #=  stored in it.
          =#
          if !isFalse(cond)
            triggerErrors(branch)
          end
          accum
        end

        _ => begin
          push!(accum, branch)
        end
      end
    end
  end
  if !isempty(accum)
    push!(elements, makeIf(accum, src))
  end
  return elements
end

"""
Note possible recheck to make sure the order is right
"""
function simplifyIfStmtBranches(
  branches::Vector{Tuple{Expression, Vector{Statement}}},
  src::DAE.ElementSource,
  makeFunc::MakeFunc,
  simplifyFunc::SimplifyFunc,
  elements::Vector{Statement},
)
  local cond::Expression
  local body::Vector{Statement}
  local accum = Tuple{Expression, Vector{Statement}}[]
  for branch in branches
    (cond, body) = branch
    cond = simplify(cond)
    #=  A branch with condition true will always be selected when encountered. =#
    if isTrue(cond)
      #=  If it's the first branch, remove the if and keep only the branch body. =#
      if isempty(accum)
        append!(simplifyFunc(body), elements)
        return elements
      else   #=  Keep branches that are neither literal true or false. =#
        push!(accum, (cond, simplifyFunc(body)))
        break
      end
    elseif !isFalse(cond)
      push!(accum, (cond, simplifyFunc(body)))
    end
  end
  #=  Otherwise just discard the rest of the branches. =#
  if !isempty(accum)
    push!(elements, makeFunc(accum, src))
  end
  return elements
end

function simplifyFunction(func::M_Function)
  local cls::Class
  local fn_body::Algorithm
  local sections::Sections
  return if !isSimplified(func)
    markSimplified(func)
    mapExp(func, simplify, false)
    cls = getClass(func.node)
    () = begin
      @match cls begin
        INSTANCED_CLASS(sections = sections) => begin
          () = begin
            @match sections begin
              SECTIONS(algorithms = [fn_body]) => begin
                @assign fn_body.statements = simplifyStatements(fn_body.statements)
                @assign sections.algorithms = [fn_body]
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
      for der_fn in getCachedFuncs(fn_der.derivativeFn)
        simplifyFunction(der_fn)
      end
    end
  end
end
