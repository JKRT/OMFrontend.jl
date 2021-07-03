
function evaluate(flatModel::FlatModel)::FlatModel
  local const_var::VariabilityType = Variability.STRUCTURAL_PARAMETER
  @assign flatModel.variables =
    list(evaluateVariable(v, const_var) for v in flatModel.variables)
  @assign flatModel.equations = evaluateEquations(flatModel.equations, const_var)
  @assign flatModel.initialEquations =
    evaluateEquations(flatModel.initialEquations, const_var)
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
  @assign var.typeAttributes = list(
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
  @assign outExp = begin
    @match exp begin
      CREF_EXPRESSION(__) => begin
        @debug "Evaluate exp traverser"
        (outExp, outChanged) = mapFoldShallow(exp, (x) -> evaluateExpTraverser(x, constVariability, changed), false)
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
        func = (x, y=constVariability) -> evaluateExpTraverser(x, y, false)
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
  eql::List{<:Equation},
  constVariability::VariabilityType,
)::List{Equation}
  local outEql::List{Equation} =list(evaluateEquation(e, constVariability) for e in eql)
  return outEql
end

function evaluateEquation(eq::Equation, constVariability::VariabilityType)::Equation
  @assign eq = begin
    local e1::Expression
    local e2::Expression
    local e3::Expression
    local ty::M_Type
    @match eq begin
      EQUATION_EQUALITY(__) => begin
        @assign ty = mapDims(eq.ty, evaluateDimension)
        @assign e1 = evaluateExp(eq.lhs, constVariability)
        @assign e2 = evaluateExp(eq.rhs, constVariability)
        EQUATION_EQUALITY(e1, e2, ty, eq.source)
      end
      EQUATION_ARRAY_EQUALITY(__) => begin
        @assign ty = mapDims(eq.ty, evaluateDimension)
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
          list(evaluateEqBranch(b, constVariability) for b in eq.branches)
        eq
      end
      EQUATION_WHEN(__) => begin
        @assign eq.branches =
          list(evaluateEqBranch(b, constVariability) for b in eq.branches)
        eq
      end
      EQUATION_ASSERT(__) => begin
        @assign e1 = evaluateExp(eq.condition, constVariability)
        @assign e2 = evaluateExp(eq.message, constVariability)
        @assign e3 = evaluateExp(eq.level, constVariability)
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

function evaluateEqBranch(branch::Equation_Branch, constVariability::VariabilityType)::Equation_Branch
  local outBranch::Equation_Branch

  @assign outBranch = begin
    local condition::Expression
    local body::List{Equation}
    @match branch begin
      EQUATION_BRANCH(condition = condition, body = body) => begin
        @assign condition =
          evaluateExp(condition, Variability.STRUCTURAL_PARAMETER)
        @assign body = evaluateEquations(body, constVariability)
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

  @assign alg.statements = evaluateStatements(alg.statements, constVariability)
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

  @assign stmt = begin
    local e1::Expression
    local e2::Expression
    local e3::Expression
    local ty::M_Type
    @match stmt begin
      P_Statement.Statement.ASSIGNMENT(__) => begin
        @assign ty = mapDims(stmt.ty, evaluateDimension)
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
          list(evaluateStmtBranch(b, constVariability) for b in stmt.branches)
        stmt
      end

      P_Statement.Statement.WHEN(__) => begin
        @assign stmt.branches =
          list(evaluateStmtBranch(b, constVariability) for b in stmt.branches)
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
  constVariability::VariabilityType,
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
  if !isEvaluated(func)
    markEvaluated(func)
    @assign func =
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

  @assign (e, outChanged) = mapFoldShallow(
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
            Ceval.EVALTARGET_IGNORE_ERRORS(),
            evalSubscripts = false,
          )
          @assign outExp = stripBindingInfo(outExp)
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
          retype(e)
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
