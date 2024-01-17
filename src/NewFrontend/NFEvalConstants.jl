"""
```
evaluate(flatModel::FlatModel)
```

Evaluates the flat model.
Basically this routine does constant evaluations by resolving parts of the model that is known statically.
"""
function evaluate(flatModel::FlatModel)::FlatModel
  local const_var::VariabilityType = Variability.STRUCTURAL_PARAMETER
  @assign flatModel.variables = Variable[evaluateVariable(v, const_var) for v in flatModel.variables]
  @assign flatModel.initialEquations = evaluateEquations(flatModel.initialEquations, const_var)
  @assign flatModel.equations = evaluateEquations(flatModel.equations, const_var)
  @assign flatModel.algorithms = evaluateAlgorithms(flatModel.algorithms, const_var)
  @assign flatModel.initialAlgorithms =
    evaluateAlgorithms(flatModel.initialAlgorithms, const_var)
#  execStat(getInstanceName())
  return flatModel
end

function evaluateVariable(var::Variable, constVariability::VariabilityType)::Variable
  local binding::Binding
  @assign binding = evaluateBinding(
    var.binding,
    variability(var) <= constVariability,
    constVariability,
  )
  if !referenceEq(binding, var.binding)
    @assign var.binding = binding
  end
  @assign var.typeAttributes = [
    evaluateTypeAttribute(a, Variability.STRUCTURAL_PARAMETER) for a in var.typeAttributes
      ]
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
    @assign exp = getTypedExp(binding)
    if structural
      @assign eexp = evalExp(exp, EVALTARGET_ATTRIBUTE(binding))
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
  constVariability::VariabilityType,
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
        #@debug "Evaluate exp traverser"
        (outExp, outChanged) = mapFoldShallow(exp, (x, boolArg) -> evaluateExpTraverser(x, constVariability, boolArg), false)
        cref = outExp.cref
        ty = outExp.ty
        #=  Evaluate constants and structural parameters.=#
        if nodeVariability(cref) <= constVariability
          @assign outExp = evalCref(
            cref,
            outExp,
            EVALTARGET_IGNORE_ERRORS(),
            evalSubscripts = false,
          )
          @assign outExp = stripBindingInfo(outExp)
          @assign outChanged = true
        elseif outChanged
          @assign outExp = CREF_EXPRESSION(
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
  @assign outChanged = changed || outChanged
  return (outExp, outChanged)
end

function evaluateDimension(dim::Dimension)::Dimension
  local outDim::Dimension
  @assign outDim = begin
    local e::Expression
    @match dim begin
      DIMENSION_EXP(__) => begin
        @assign e =
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
  eql::Vector{Equation},
  constVariability::VariabilityType,
)
  local outEql = Equation[evaluateEquation(e, constVariability) for e in eql]
  return outEql
end

function evaluateEquation(@nospecialize(eq::Equation), constVariability::VariabilityType)::Equation
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
        @assign eq.range = Util.applyOption(
          eq.range,
          (constVariability) -> evaluateExp(constVariability = constVariability),
        )
        @assign eq.body = evaluateEquations(eq.body, constVariability)
        eq
      end
      EQUATION_IF(__) => begin
        @assign eq.branches = EQUATION_BRANCH[evaluateEqBranch(b, constVariability) for b in eq.branches]
        eq
      end
      EQUATION_WHEN(__) => begin
        @assign eq.branches = EQUATION_BRANCH[evaluateEqBranch(b, constVariability) for b in eq.branches]
        eq
      end
      EQUATION_ASSERT(__) => begin
        e1 = evaluateExp(eq.condition, constVariability)
        e2 = evaluateExp(eq.message, constVariability)
        e3 = evaluateExp(eq.level, constVariability)
        EQUATION_ASSERT(e1, e2, e3, eq.source)
      end
      EQUATION_TERMINATE(__) => begin
        @assign eq.message = evaluateExp(eq.message, constVariability)
        eq
      end
      EQUATION_REINIT(__) => begin
        @assign eq.reinitExp = evaluateExp(eq.reinitExp, constVariability)
        eq
      end
      EQUATION_NORETCALL(__) => begin
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

function evaluateEqBranch(branch::Equation_Branch, constVariability::VariabilityType)
  local outBranch::Equation_Branch
  outBranch = begin
    local condition::Expression
    local body
    @match branch begin
      EQUATION_BRANCH(condition = condition, body = body) => begin
        condition = evaluateExp(condition, Variability.STRUCTURAL_PARAMETER)
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
  algs::Vector{Algorithm},
  constVariability::VariabilityType,
)
  local outAlgs::Vector{Algorithm} = [evaluateAlgorithm(a, constVariability) for a in algs]
  return outAlgs
end

function evaluateAlgorithm(alg::Algorithm, constVariability::VariabilityType)
  @assign alg.statements = evaluateStatements(alg.statements, constVariability)
  return alg
end

function evaluateStatements(
  stmts::Vector{Statement},
  constVariability::VariabilityType,
  )
  local outStmts::Vector{Statement} = [evaluateStatement(s, constVariability) for s in stmts]
  return outStmts
end

function evaluateStatement(stmt::Statement, constVariability::VariabilityType)::Statement
  stmt = begin
    local e1::Expression
    local e2::Expression
    local e3::Expression
    local ty::M_Type
    @match stmt begin
      ALG_ASSIGNMENT(__) => begin
        ty = mapDims(stmt.ty, evaluateDimension)
        e1 = evaluateExp(stmt.lhs, constVariability)
        e2 = evaluateExp(stmt.rhs, constVariability)
        ALG_ASSIGNMENT(e1, e2, ty, stmt.source)
      end

      ALG_FOR(__) => begin
        @assign stmt.range = Util.applyOption(
          stmt.range,
          (x) -> evaluateExp(x, constVariability),
        )
        @assign stmt.body = evaluateStatements(stmt.body, constVariability)
        stmt
      end

      ALG_IF(__) => begin
        @assign stmt.branches =
          [evaluateStmtBranch(b, constVariability) for b in stmt.branches]
        stmt
      end

      ALG_WHEN(__) => begin
        @assign stmt.branches =
          [evaluateStmtBranch(b, constVariability) for b in stmt.branches]
        stmt
      end

      ALG_ASSERT(__) => begin
        @assign e1 = evaluateExp(stmt.condition, constVariability)
        @assign e2 = evaluateExp(stmt.message, constVariability)
        @assign e3 = evaluateExp(stmt.level, constVariability)
        ALG_ASSERT(e1, e2, e3, stmt.source)
      end

      ALG_TERMINATE(__) => begin
        @assign stmt.message = evaluateExp(stmt.message, constVariability)
        stmt
      end

      ALG_NORETCALL(__) => begin
        @assign stmt.exp = evaluateExp(stmt.exp, constVariability)
        stmt
      end

      ALG_WHILE(__) => begin
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
  branch::Tuple{Expression, Vector{Statement}},
  constVariability::VariabilityType,
)::Tuple{Expression, Vector{Statement}}
  local outBranch::Tuple{Expression, Vector{Statement}}
  local cond::Expression
  local body::Vector{Statement}
  (cond, body) = branch
  cond = evaluateExp(cond, Variability.STRUCTURAL_PARAMETER)
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
    func = mapExp(func, (x) -> evaluateFuncExp(x, func.node))
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
  (outExp, _) = evaluateFuncExpTraverser(exp, fnNode, false)
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
    (x, y) -> evaluateFuncExpTraverser(x, fnNode, y),
    false,
  )
  outExp = begin
    @match e begin
      CREF_EXPRESSION(__) => begin
        if !isLocalFunctionVariable(e.cref, fnNode)
          outExp = evalCref(
            e.cref,
            e,
            EVALTARGET_IGNORE_ERRORS(),
            evalSubscripts = false,
          )
          outExp = stripBindingInfo(outExp)
          outChanged = true
        elseif outChanged
          #=  If the cref's subscripts changed, recalculate its type. =#
          outExp = CREF_EXPRESSION(
            getSubscriptedType(e.cref),
            e.cref,
          )
        else
          @assign outExp = e
        end
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
  local nodeVar::InstNode
  if isPackageConstant(cref)
    res = false
  elseif nodeVariability(cref) <= Variability.PARAMETER
    nodeVar =
      derivedParent(node(firstNonScope(
        cref,
      )))
    res = refEqual(fnNode, nodeVar)
  else
    res = true
  end
  return res
end
