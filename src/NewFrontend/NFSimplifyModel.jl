const SimplifyFunc = Function
const MakeElement = Function
const MakeFunc = Function

function simplifyFlatModel(flatModel::FlatModel)::FlatModel
  #= Complex assign=#@assign flatModel.variables = list(simplifyVariable(v) for v in flatModel.variables)
  #= Complex assign=#@assign flatModel.equations = simplifyEquations(flatModel.equations)
  #= Complex assign=#@assign flatModel.initialEquations = simplifyEquations(flatModel.initialEquations)
  #= Complex assign=#@assign flatModel.algorithms = simplifyAlgorithms(flatModel.algorithms)
  #= Complex assign=#@assign flatModel.initialAlgorithms = simplifyAlgorithms(flatModel.initialAlgorithms)
#  execStat(getInstanceName()) TODO
  return flatModel
end

function simplifyVariable(var::Variable)::Variable
  #= complex assign=#@assign var.binding = simplifyBinding(var.binding)
  #= Complex assign=#@assign var.typeAttributes = list(simplifyTypeAttribute(a) for a in var.typeAttributes)
  return var
end

function simplifyBinding(binding::Binding)::Binding

  local exp::Expression
  local sexp::Expression

  if isBound(binding)
    exp = getTypedExp(binding)
    sexp = simplify(exp)
    sexp = removeEmptyFunctionArguments(sexp)
    if !referenceEq(exp, sexp)
      binding = setTypedExp(sexp, binding)
    end
  end
  return binding
end

function simplifyTypeAttribute(attribute::Tuple{<:String, Binding})::Tuple{String, Binding}

  local name::String
  local binding::Binding
  local sbinding::Binding

  (name, binding) = attribute
  sbinding = simplifyBinding(binding)
  if !referenceEq(binding, sbinding)
    attribute = (name, sbinding)
  end
  return attribute
end

function simplifyDimension(dim::Dimension)::Dimension
  local outDim::Dimension

  outDim = begin
    local e::Expression
    @match dim begin
      DIMENSION_EXP(__) => begin
        e = simplify(dim.exp)
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

function simplifyEquations(eql::List{<:Equation})
  local outEql::List{Equation} = nil
  for eq in eql
    outEql = simplifyEquation(eq, outEql)
  end
  outEql = listReverseInPlace(outEql)
  return outEql
end

function simplifyEquation(eq::Equation, equations::List{<:Equation})
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
        ty = mapDims(eq.ty, simplifyDimension)
        if !Type.isEmptyArray(ty)
          rhs = removeEmptyFunctionArguments(simplify(eq.rhs))
          equations = _cons(
            EQUATION_ARRAY_EQUALITY(eq.lhs, rhs, ty, eq.source),
            equations,
          )
        end
        equations
      end

      EQUATION_IF(__) => begin
        simplifyIfEqBranches(eq.branches, eq.source, equations)
      end

      EQUATION_WHEN(__) => begin
        #= complex assign=#@assign eq.branches = list(
          begin
            @match b begin
              EQUATION_BRANCH(__) => begin
                #= complex assign=#@assign b.condition = simplify(b.condition)
                #= complex assign=#@assign b.body = simplifyEquations(b.body)
                b
              end
            end
          end for b in eq.branches
        )
        _cons(eq, equations)
      end

      EQUATION_ASSERT(__) => begin
        #= complex assign=#@assign eq.condition = simplify(eq.condition)
        if isTrue(eq.condition)
          equations
        else
          _cons(eq, equations)
        end
      end

      EQUATION_REINIT(__) => begin
        #= Complex assign=#@assign eq.reinitExp = simplify(eq.reinitExp)
        _cons(eq, equations)
      end

      EQUATION_NORETCALL(__) => begin
        e = simplify(eq.exp)
        if isCall(e)
          #= complex assign=#@assign eq.exp = removeEmptyFunctionArguments(e)
           equations = _cons(eq, equations)
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

function simplifyAlgorithms(algs::List{<:Algorithm})
  local outAlgs::List{Algorithm} = nil
  for alg in algs
    alg = simplifyAlgorithm(alg)
    if !listEmpty(alg.statements)
      outAlgs = _cons(alg, outAlgs)
    end
  end
  outAlgs = listReverseInPlace(outAlgs)
  return outAlgs
end

function simplifyAlgorithm(alg::Algorithm)::Algorithm

  #= complex assign=#@assign alg.statements = simplifyStatements(alg.statements)
  return alg
end

function simplifyStatements(stmts::List{<:Statement})::List{Statement}
  local outStmts::List{Statement} = nil

  for s in stmts
    outStmts = simplifyStatement(s, outStmts)
  end
  outStmts = listReverseInPlace(outStmts)
  return outStmts
end

function simplifyStatement(stmt::Statement, statements::List{<:Statement})::List{Statement}

  statements = begin
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
        ty = typeOf(e)
        dim = Type.nthDimension(ty, 1)
        #= if Dimension.isOne(dim) then
        =#
        #=   e := Expression.applySubscript(Subscript.INDEX(Expression.INTEGER_EXPRESSION(1)), e);
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
          #= complex assign=#@assign stmt.range = SOME(simplify(e))
          #= complex assign=#@assign stmt.body = simplifyStatements(stmt.body)
          statements = _cons(stmt, statements)
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
        #= complex assign=#@assign stmt.branches = list(
          (simplify(Util.tuple21(b)), simplifyStatements(Util.tuple22(b))) for b in stmt.branches
        )
        _cons(stmt, statements)
      end

      P_Statement.Statement.NORETCALL(__) => begin
        e = simplify(stmt.exp)
        if isCall(e)
          #= complex assign=#@assign stmt.exp = removeEmptyFunctionArguments(e)
          statements = _cons(stmt, statements)
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
  ty = mapDims(ty, simplifyDimension)
  if Type.isEmptyArray(ty)
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
    if !isWildCref(lhs)
      statements = _cons(makeFn(lhs, rhs, ety, src), statements)
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
        #= complex assign=#@assign exp.elements = list(@do_threaded_for if Type.isEmptyArray(t)
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
          outExp =
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
  is_arg = isArg || isCall(exp)
  outExp = mapShallow(
    exp,
    (x, y = is_arg) -> removeEmptyFunctionArguments(x, y)
  )
  return outExp
end

function simplifyIfEqBranches(
  branches::List{<:Equation_Branch},
  src::DAE.ElementSource,
  elements::List{<:Equation},
)::List{Equation}

  local cond::Expression
  local body::List{Equation}
  local var::VariabilityType
  local accum::List{<:Equation_Branch} = nil

  for branch in branches
    accum = begin
      @match branch begin
        EQUATION_BRANCH(cond, var, body) => begin
          cond = simplify(cond)
          #=  A branch with condition true will always be selected when encountered. =#
          if isTrue(cond)
            if listEmpty(accum)
              for eq in body
                elements = simplifyEquation(eq, elements)
              end
              return elements
            else
              accum = _cons(
                makeBranch(cond, simplifyEquations(body)),
                accum,
              )
              elements = _cons(
                makeIf(listReverseInPlace(accum), src),
                elements,
              )
              return elements #TODO: John WTF?
            end
          elseif !isFalse(cond)
            accum = _cons(
              makeBranch(cond, simplifyEquations(body)),
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
        EQUATION_INVALID_BRANCH(
          branch = EQUATION_BRANCH(
            condition = cond,
            conditionVar = var,
          ),
        ) => begin
          if var <= Variability.STRUCTURAL_PARAMETER
            cond = evalExp(cond)
          end
          #=  An invalid branch that can't be removed will trigger the errors
          =#
          #=  stored in it.
          =#
          if !isFalse(cond)
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
    elements =
      _cons(makeIf(listReverseInPlace(accum), src), elements)
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
    (cond, body) = branch
    cond = simplify(cond)
    if isTrue(cond)
      if listEmpty(accum)
        elements = listAppend(listReverse(simplifyFunc(body)), elements)
        return elements
      else
        accum = _cons((cond, simplifyFunc(body)), accum)
        break
      end
    elseif !isFalse(cond)
      accum = _cons((cond, simplifyFunc(body)), accum)
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
    elements = _cons(makeFunc(listReverseInPlace(accum), src), elements)
  end
  return elements
end

function simplifyFunction(func::M_Function)
  local cls::Class
  local fn_body::Algorithm
  local sections::Sections

  return if !P_Function.isSimplified(func)
    P_Function.markSimplified(func)
    P_Function.mapExp(func, simplify, mapBody = false)
    cls = getClass(func.node)
    () = begin
      @match cls begin
        INSTANCED_CLASS(sections = sections) => begin
          () = begin
            @match sections begin
              P_Sections.Sections.SECTIONS(algorithms = fn_body <| nil()) => begin
                #= complex assign=#@assign fn_body.statements = simplifyStatements(fn_body.statements)
                #= complex assign=#@assign sections.algorithms = list(fn_body)
                #= complex assign=#@assign cls.sections = sections
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
