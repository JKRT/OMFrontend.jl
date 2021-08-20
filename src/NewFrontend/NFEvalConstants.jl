
function evaluate(flatModel::FlatModel)::FlatModel
  local const_var::VariabilityType = Variability.STRUCTURAL_PARAMETER
  #= Complex assign=#@assign flatModel.variables =
    list(evaluateVariable(v, const_var) for v in flatModel.variables)
  #= Complex assign=#@assign flatModel.equations = evaluateEquations(flatModel.equations, const_var)
  #= Complex assign=#@assign flatModel.initialEquations =
    evaluateEquations(flatModel.initialEquations, const_var)
  #= Complex assign=#@assign flatModel.algorithms = evaluateAlgorithms(flatModel.algorithms, const_var)
  #= Complex assign=#@assign flatModel.initialAlgorithms =
    evaluateAlgorithms(flatModel.initialAlgorithms, const_var)
#  execStat(getInstanceName())
  return flatModel
end

function evaluateVariable(var::Variable, constVariability::VariabilityType)::Variable
  local binding::Binding
  binding = evaluateBinding(
    var.binding,
    variability(var) <= constVariability,
    constVariability,
  )
  if !referenceEq(binding, var.binding)
    #= complex assign=#@assign var.binding = binding
  end
  #= Complex assign=#@assign var.typeAttributes = list(
    evaluateTypeAttribute(a, Variability.STRUCTURAL_PARAMETER) for a in var.typeAttributes
  )
  return var
end

function evaluateBinding(
  binding::Binding,
  structural::Bool,
  constVariability::VariabilityType,
)::Binding
  local exp::Expression
  local eexp::Expression
  if isBound(binding)
    exp = getTypedExp(binding)
    if structural
      eexp = evalExp(exp, EVALTARGET_ATTRIBUTE(binding))
    else
      eexp = evaluateExp(exp, constVariability)
    end
    if !referenceEq(exp, eexp)
      binding = setTypedExp(eexp, binding)
    end
  end
  return binding
end

function evaluateTypeAttribute(
  attribute::Tuple{<:String, Binding},
  constVariability::VariabilityType,
)::Tuple{String, Binding}
  local name::String
  local binding::Binding
  local sbinding::Binding
  local structural::Bool
  (name, binding) = attribute
  structural = name == "fixed" || name == "stateSelect"
  sbinding = evaluateBinding(binding, structural, constVariability)
  if !referenceEq(binding, sbinding)
    attribute = (name, sbinding)
  end
  return attribute
end

function evaluateExp(exp::Expression, constVariability::VariabilityType)::Expression
  local outExp::Expression
  (outExp, _) = evaluateExpTraverser(exp, constVariability, false)
  return outExp
end

function evaluateExpTraverser(
  exp::Expression,
  constVariability::VariabilityType,
  changed::Bool,
)::Tuple{Expression, Bool}
  local outChanged::Bool
  local outExp::Expression
  local e::Expression
  local cref::ComponentRef
  local ty::M_Type
  local var::VariabilityType
  outExp = begin
    @match exp begin
      CREF_EXPRESSION(__) => begin
        @debug "Evaluate exp traverser"
        (outExp, outChanged) = mapFoldShallow(exp, (x) -> evaluateExpTraverser(x, constVariability, changed), false)
        cref = outExp.cref
        ty = outExp.ty
        #=  Evaluate constants and structural parameters.=#
        if nodeVariability(cref) <= constVariability
          outExp = evalCref(
            cref,
            outExp,
            EVALTARGET_IGNORE_ERRORS(),
            evalSubscripts = false,
          )
          outExp = stripBindingInfo(outExp)
          outChanged = true
        elseif outChanged
          outExp = CREF_EXPRESSION(
            getSubscriptedType(cref),
            cref,
          )
        end
        #=  Evaluate all constants and structural parameters. =#
        #=  If the cref's subscripts changed, recalculate its type.=#
        outExp
      end
      _ => begin
        func = (x, y) -> evaluateExpTraverser(x, constVariability, false)
        (outExp, outChanged) = mapFoldShallow(exp, func, false)
        if outChanged
          retype(outExp)
        else
          outExp
        end
      end
    end
  end
  outChanged = changed || outChanged
  return (outExp, outChanged)
end

function evaluateDimension(dim::Dimension)::Dimension
  local outDim::Dimension
  outDim = begin
    local e::Expression
    @match dim begin
      DIMENSION_EXP(__) => begin
        e =
          evaluateExp(dim.exp, constVariability = Variability.STRUCTURAL_PARAMETER)
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

function evaluateEquations(
  eql::List{<:Equation},
  constVariability::VariabilityType,
)::List{Equation}
  local outEql::List{Equation} =list(evaluateEquation(e, constVariability) for e in eql)
  return outEql
end

function evaluateEquation(eq::Equation, constVariability::VariabilityType)::Equation
  eq = begin
    local e1::Expression
    local e2::Expression
    local e3::Expression
    local ty::M_Type
    @match eq begin
      EQUATION_EQUALITY(__) => begin
        ty = mapDims(eq.ty, evaluateDimension)
        e1 = evaluateExp(eq.lhs, constVariability)
        e2 = evaluateExp(eq.rhs, constVariability)
        EQUATION_EQUALITY(e1, e2, ty, eq.source)
      end
      EQUATION_ARRAY_EQUALITY(__) => begin
        ty = mapDims(eq.ty, evaluateDimension)
        e2 = evaluateExp(eq.rhs, constVariability)
        EQUATION_ARRAY_EQUALITY(eq.lhs, e2, ty, eq.source)
      end
      EQUATION_FOR(__) => begin
        #= complex assign=#@assign eq.range = Util.applyOption(
          eq.range,
          (constVariability) -> evaluateExp(constVariability = constVariability),
        )
        #= complex assign=#@assign eq.body = evaluateEquations(eq.body, constVariability)
        eq
      end
      EQUATION_IF(__) => begin
        #= complex assign=#@assign eq.branches =
          list(evaluateEqBranch(b, constVariability) for b in eq.branches)
        eq
      end
      EQUATION_WHEN(__) => begin
        #= complex assign=#@assign eq.branches =
          list(evaluateEqBranch(b, constVariability) for b in eq.branches)
        eq
      end
      EQUATION_ASSERT(__) => begin
        e1 = evaluateExp(eq.condition, constVariability)
        e2 = evaluateExp(eq.message, constVariability)
        e3 = evaluateExp(eq.level, constVariability)
        EQUATION_ASSERT(e1, e2, e3, eq.source)
      end
      EQUATION_TERMINATE(__) => begin
        #= complex assign=#@assign eq.message = evaluateExp(eq.message, constVariability)
        eq
      end
      EQUATION_REINIT(__) => begin
        #= Complex assign=#@assign eq.reinitExp = evaluateExp(eq.reinitExp, constVariability)
        eq
      end
      EQUATION_NORETCALL(__) => begin
        #= complex assign=#@assign eq.exp = evaluateExp(eq.exp, constVariability)
        eq
      end
      _ => begin
        eq
      end
    end
  end
  return eq
end

function evaluateEqBranch(branch::Equation_Branch, constVariability::VariabilityType)::Equation_Branch
  local outBranch::Equation_Branch

  outBranch = begin
    local condition::Expression
    local body::List{Equation}
    @match branch begin
      EQUATION_BRANCH(condition = condition, body = body) => begin
        condition =
          evaluateExp(condition, Variability.STRUCTURAL_PARAMETER)
        body = evaluateEquations(body, constVariability)
        EQUATION_BRANCH(condition, branch.conditionVar, body)
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
  constVariability::VariabilityType,
)::List{Algorithm}
  local outAlgs::List{Algorithm} =
    list(evaluateAlgorithm(a, constVariability) for a in algs)
  return outAlgs
end

function evaluateAlgorithm(alg::Algorithm, constVariability::VariabilityType)::Algorithm

  #= complex assign=#@assign alg.statements = evaluateStatements(alg.statements, constVariability)
  return alg
end

function evaluateStatements(
  stmts::List{<:Statement},
  constVariability::VariabilityType,
)::List{Statement}
  local outStmts::List{Statement} =
    list(evaluateStatement(s, constVariability) for s in stmts)
  return outStmts
end

function evaluateStatement(stmt::Statement, constVariability::VariabilityType)::Statement

  stmt = begin
    local e1::Expression
    local e2::Expression
    local e3::Expression
    local ty::M_Type
    @match stmt begin
      P_Statement.Statement.ASSIGNMENT(__) => begin
        ty = mapDims(stmt.ty, evaluateDimension)
        e1 = evaluateExp(stmt.lhs, constVariability)
        e2 = evaluateExp(stmt.rhs, constVariability)
        P_Statement.Statement.ASSIGNMENT(e1, e2, ty, stmt.source)
      end

      P_Statement.Statement.FOR(__) => begin
        #= complex assign=#@assign stmt.range = Util.applyOption(
          stmt.range,
          (constVariability) -> evaluateExp(constVariability = constVariability),
        )
        #= complex assign=#@assign stmt.body = evaluateStatements(stmt.body, constVariability)
        stmt
      end

      P_Statement.Statement.IF(__) => begin
        #= complex assign=#@assign stmt.branches =
          list(evaluateStmtBranch(b, constVariability) for b in stmt.branches)
        stmt
      end

      P_Statement.Statement.WHEN(__) => begin
        #= complex assign=#@assign stmt.branches =
          list(evaluateStmtBranch(b, constVariability) for b in stmt.branches)
        stmt
      end

      P_Statement.Statement.ASSERT(__) => begin
        e1 = evaluateExp(stmt.condition, constVariability)
        e2 = evaluateExp(stmt.message, constVariability)
        e3 = evaluateExp(stmt.level, constVariability)
        P_Statement.Statement.ASSERT(e1, e2, e3, stmt.source)
      end

      P_Statement.Statement.TERMINATE(__) => begin
        #= complex assign=#@assign stmt.message = evaluateExp(stmt.message, constVariability)
        stmt
      end

      P_Statement.Statement.NORETCALL(__) => begin
        #= complex assign=#@assign stmt.exp = evaluateExp(stmt.exp, constVariability)
        stmt
      end

      P_Statement.Statement.WHILE(__) => begin
        #= complex assign=#@assign stmt.condition = evaluateExp(stmt.condition, constVariability)
        #= complex assign=#@assign stmt.body = evaluateStatements(stmt.body, constVariability)
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
  constVariability::VariabilityType,
)::Tuple{Expression, List{Statement}}
  local outBranch::Tuple{Expression, List{Statement}}
  local cond::Expression
  local body::List{Statement}
  (cond, body) = branch
  cond = evaluateExp(cond, constVariability = Variability.STRUCTURAL_PARAMETER)
  body = evaluateStatements(body, constVariability)
  outBranch = (cond, body)
  return outBranch
end

function evaluateFunction(func::M_Function)::M_Function
  local cls::Class
  local fn_body::Algorithm
  local sections::Sections
  if !isEvaluated(func)
    markEvaluated(func)
    func =
      mapExp(func, (x) -> evaluateFuncExp(x, fnNode = func.node))
    for fn_der in func.derivatives
      for der_fn in getCachedFuncs(fn_der.derivativeFn)
        evaluateFunction(der_fn)
      end
    end
  end
  return func
end

function evaluateFuncExp(exp::Expression, fnNode::InstNode)::Expression
  local outExp::Expression
  outExp = evaluateFuncExpTraverser(exp, fnNode, false)
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

  (e, outChanged) = mapFoldShallow(
    exp,
    (fnNode) -> evaluateFuncExpTraverser(fnNode = fnNode),
    false,
  )
  outExp = begin
    @match e begin
      CREF_EXPRESSION(__) => begin
        if !isLocalFunctionVariable(e.cref, fnNode)
          outExp = Ceval.evalCref(
            e.cref,
            e,
            Ceval.EVALTARGET_IGNORE_ERRORS(),
            evalSubscripts = false,
          )
          outExp = stripBindingInfo(outExp)
          outChanged = true
        elseif outChanged
          outExp = CREF_EXPRESSION(
            getSubscriptedType(e.cref),
            e.cref,
          )
        else
          outExp = e
        end
        #=  If the cref's subscripts changed, recalculate its type.
        =#
        outExp
      end

      _ => begin
        if outChanged
          retype(e)
        else
          e
        end
      end
    end
  end
  outChanged = changed || outChanged
  return (outExp, outChanged)
end

function isLocalFunctionVariable(cref::ComponentRef, fnNode::InstNode)::Bool
  local res::Bool
  local node::InstNode
  if isPackageConstant(cref)
    res = false
  elseif nodeVariability(cref) <= Variability.PARAMETER
    node =
      derivedParent(node(firstNonScope(
        cref,
      )))
    res = refEqual(fnNode, node)
  else
    res = true
  end
  return res
end
