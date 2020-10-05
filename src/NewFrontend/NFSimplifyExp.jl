
function simplify(exp::Expression)::Expression
  @assign exp = begin
    @match exp begin
      CREF_EXPRESSION(__) => begin
        @assign exp.cref = simplifySubscripts(exp.cref)
        @assign exp.ty = getSubscriptedType(exp.cref)
        exp
      end

      ARRAY_EXPRESSION(__) => begin
        @assign exp.elements = list(simplify(e) for e in exp.elements)
        exp
      end

      RANGE_EXPRESSION(__) => begin
        simplifyRange(exp)
      end

      RECORD_EXPRESSION(__) => begin
        @assign exp.elements = list(simplify(e) for e in exp.elements)
        exp
      end

      CALL_EXPRESSION(__) => begin
        simplifyCall(exp)
      end

      SIZE_EXPRESSION(__) => begin
        simplifySize(exp)
      end

      BINARY_EXPRESSION(__) => begin
        simplifyBinary(exp)
      end

      UNARY_EXPRESSION(__) => begin
        simplifyUnary(exp)
      end

      LBINARY_EXPRESSION(__) => begin
        simplifyLogicBinary(exp)
      end

      LUNARY_EXPRESSION(__) => begin
        simplifyLogicUnary(exp)
      end

      RELATION_EXPRESSION(__) => begin
        simplifyRelation(exp)
      end

      IF_EXPRESSION(__) => begin
        simplifyIf(exp)
      end

      CAST_EXPRESSION(__) => begin
        simplifyCast(simplify(exp.exp), exp.ty)
      end

      UNBOX_EXPRESSION(__) => begin
        UNBOX_EXPRESSION(simplify(exp.exp), exp.ty)
      end

      SUBSCRIPTED_EXP_EXPRESSION(__) => begin
        simplifySubscriptedExp(exp)
      end

      TUPLE_ELEMENT_EXPRESSION(__) => begin
        simplifyTupleElement(exp)
      end

      BOX_EXPRESSION(__) => begin
        BOX_EXPRESSION(simplify(exp.exp))
      end

      MUTABLE_EXPRESSION(__) => begin
        simplify(P_Pointer.access(exp.exp))
      end
      _ => begin
        exp
      end
    end
  end
  return exp
end

function simplifyOpt(exp::Option{<:Expression})::Option{Expression}

  local e::Expression

  @assign exp = begin
    @match exp begin
      SOME(e) => begin
        SOME(simplify(e))
      end

      _ => begin
        exp
      end
    end
  end
  return exp
end

function simplifyRange(range::Expression)::Expression
  local exp::Expression

  local start_exp1::Expression
  local stop_exp1::Expression
  local start_exp2::Expression
  local stop_exp2::Expression
  local step_exp1::Option{Expression}
  local step_exp2::Option{Expression}
  local ty::M_Type

  @match RANGE_EXPRESSION(
    ty = ty,
    start = start_exp1,
    step = step_exp1,
    stop = stop_exp1,
  ) = range
  @assign start_exp2 = simplify(start_exp1)
  @assign step_exp2 = simplifyOpt(step_exp1)
  @assign stop_exp2 = simplify(stop_exp1)
  if referenceEq(start_exp1, start_exp2) &&
     referenceEq(step_exp1, step_exp2) &&
     referenceEq(stop_exp1, stop_exp2)
    @assign exp = range
  else
    @assign ty = TypeCheck.getRangeType(
      start_exp2,
      step_exp2,
      stop_exp2,
      arrayElementType(ty),
      AbsynUtil.dummyInfo,
    )
    @assign exp = RANGE_EXPRESSION(ty, start_exp2, step_exp2, stop_exp2)
  end
  return exp
end

function simplifyCall(callExp::Expression)::Expression

  local call::Call
  local args::List{Expression}
  local builtin::Bool
  local is_pure::Bool

  @match CALL_EXPRESSION(call = call) = callExp
  @assign callExp = begin
    @match call begin
      P_Call.TYPED_CALL(arguments = args) where {(!P_Call.isExternal(call))} => begin
        if Flags.isSet(Flags.NF_EXPAND_FUNC_ARGS)
          @assign args = list(if P_Expression.Expression.hasArrayCall(arg)
            arg
          else
            P_ExpandExp.ExpandExp.expand(arg)
          end for arg in args)
        end
        #=  HACK, TODO, FIXME! handle DynamicSelect properly in OMEdit, then disable this stuff!
        =#
        if Flags.isSet(Flags.NF_API) && !Flags.isSet(Flags.NF_API_DYNAMIC_SELECT)
          if stringEq(
            "DynamicSelect",
            AbsynUtil.pathString(nameConsiderBuiltin(call.fn)),
          )
            @assign callExp = simplify(listHead(args))
            return
          end
        end
        @assign args = list(simplify(arg) for arg in args)
        @assign call.arguments = args
        @assign builtin = P_Function.isBuiltin(call.fn)
        @assign is_pure = !P_Function.isImpure(call.fn)
        #=  Use Ceval for builtin pure functions with literal arguments.
        =#
        if builtin
          if is_pure && ListUtil.all(args, isLiteral)
            try
              @assign callExp = Ceval.evalCall(call, P_EvalTarget.IGNORE_ERRORS())
              @assign callExp = P_Expression.Expression.stripBindingInfo(callExp)
            catch
            end
          else
            if Flags.isSet(Flags.NF_SCALARIZE)
              @assign callExp =
                simplifynameConsiderBuiltin(call.fn, args, call)
            end
          end
        elseif Flags.isSet(Flags.NF_EVAL_CONST_ARG_FUNCS) &&
               is_pure &&
               ListUtil.all(args, isLiteral)
          @assign callExp = simplifyCall2(call)
        else
          @assign callExp = CALL_EXPRESSION(call)
        end
        #=  do not expand builtin calls if we should not scalarize
        =#
        #=  nothing
        =#
        callExp
      end

      P_Call.TYPED_ARRAY_CONSTRUCTOR(__) => begin
        simplifyArrayConstructor(call)
      end

      P_Call.TYPED_REDUCTION(__) => begin
        @assign call.exp = simplify(call.exp)
        @assign call.iters =
          list((Util.tuple21(i), simplify(Util.tuple22(i))) for i in call.iters)
        CALL_EXPRESSION(call)
      end

      _ => begin
        callExp
      end
    end
  end
  return callExp
end

function simplifyCall2(call::Call)::Expression
  local outExp::Expression

  ErrorExt.setCheckpoint(getInstanceName())
  try
    @assign outExp = Ceval.evalCall(call, P_EvalTarget.IGNORE_ERRORS())
    @assign outExp = P_Expression.Expression.stripBindingInfo(outExp)
    ErrorExt.delCheckpoint(getInstanceName())
  catch
    if Flags.isSet(Flags.FAILTRACE)
      ErrorExt.delCheckpoint(getInstanceName())
      Debug.traceln(
        "- " + getInstanceName() + " failed to evaluate " + P_Call.toString(call) + "\\n",
      )
    else
      ErrorExt.rollBack(getInstanceName())
    end
    @assign outExp = CALL_EXPRESSION(call)
  end
  return outExp
end

function simplify(
  name::Absyn.Path,
  args::List{<:Expression},
  call::Call,
)::Expression
  local exp::Expression

  @assign exp = begin
    @match AbsynUtil.pathFirstIdent(name) begin
      "cat" => begin
        @assign exp = P_ExpandExp.ExpandExp.expandBuiltinCat(args, call)
        exp
      end

      "sum" => begin
        simplifySumProduct(listHead(args), call, isSum = true)
      end

      "product" => begin
        simplifySumProduct(listHead(args), call, isSum = false)
      end

      "transpose" => begin
        simplifyTranspose(listHead(args), call)
      end

      _ => begin
        CALL_EXPRESSION(call)
      end
    end
  end
  return exp
end

function simplifySumProduct(arg::Expression, call::Call, isSum::Bool)::Expression
  local exp::Expression

  local expanded::Bool
  local args::List{Expression}
  local ty::M_Type
  local op::Operator

  @assign (exp, expanded) = P_ExpandExp.ExpandExp.expand(arg)
  if expanded
    @assign args = P_Expression.Expression.arrayScalarElements(exp)
    @assign ty = arrayElementType(typeOf(arg))
    if listEmpty(args)
      @assign exp = if isSum
        P_Expression.Expression.makeZero(ty)
      else
        P_Expression.Expression.makeOne(ty)
      end
    else
      @match _cons(exp, args) = args
      @assign op = if isSum
        P_Operator.Operator.makeAdd(ty)
      else
        P_Operator.Operator.makeMul(ty)
      end
      for e in args
        @assign exp = BINARY_EXPRESSION(exp, op, e)
      end
    end
  else
    @assign exp = CALL_EXPRESSION(call)
  end
  return exp
end

function simplifyTranspose(arg::Expression, call::Call)::Expression
  local exp::Expression

  local e::Expression

  @assign e = if P_Expression.Expression.hasArrayCall(arg)
    arg
  else
    P_ExpandExp.ExpandExp.expand(arg)
  end
  @assign exp = begin
    @match e begin
      ARRAY_EXPRESSION(
        __,
      ) where {(ListUtil.all(e.elements, P_Expression.Expression.isArray))} => begin
        P_Expression.Expression.transposeArray(e)
      end

      _ => begin
        CALL_EXPRESSION(call)
      end
    end
  end
  return exp
end

function simplifyArrayConstructor(call::Call)::Expression
  local outExp::Expression

  local ty::M_Type
  local var::VariabilityType
  local exp::Expression
  local e::Expression
  local iters::List{Tuple{InstNode, Expression}}
  local iter::InstNode
  local dim::Dimension
  local dim_size::Integer
  local expanded::Bool

  @match P_Call.TYPED_ARRAY_CONSTRUCTOR(ty, var, exp, iters) = call
  @assign iters = list((Util.tuple21(i), simplify(Util.tuple22(i))) for i in iters)
  @assign outExp = begin
    @matchcontinue iters begin
      (iter, e) <| nil() => begin
        @match ARRAY_TYPE(dimensions = list(dim)) = typeOf(e)
        @assign dim_size = P_Dimension.Dimension.size(dim)
        if dim_size == 0
          @assign outExp = P_Expression.Expression.makeEmptyArray(ty)
        elseif dim_size == 1
          @match (ARRAY_EXPRESSION(elements = list(e)), _) =
            P_ExpandExp.ExpandExp.expand(e)
          @assign exp = P_Expression.Expression.replaceIterator(exp, iter, e)
          @assign exp = P_Expression.Expression.makeArray(ty, list(exp))
          @assign outExp = simplify(exp)
        else
          fail()
        end
        #=  Result is Array[0], return empty array expression.
        =#
        #=  Result is Array[1], return array with the single element.
        =#
        outExp
      end

      _ => begin
        @assign exp = simplify(exp)
        CALL_EXPRESSION(P_Call.TYPED_ARRAY_CONSTRUCTOR(ty, var, exp, iters))
      end
    end
  end
  return outExp
end

function simplifySize(sizeExp::Expression)::Expression
  @assign sizeExp = begin
    local exp::Expression
    local index::Expression
    local dim::Dimension
    local dims::List{Dimension}
    @match sizeExp begin
      SIZE_EXPRESSION(exp, SOME(index)) => begin
        @assign index = simplify(index)
        if isLiteral(index)
          @assign dim = listGet(
            arrayDims(typeOf(exp)),
            P_Expression.Expression.toInteger(index),
          )
          if P_Dimension.Dimension.isKnown(dim)
            @assign exp = INTEGER_EXPRESSION(P_Dimension.Dimension.size(dim))
          else
            @assign exp = SIZE_EXPRESSION(exp, SOME(index))
          end
        else
          @assign exp = SIZE_EXPRESSION(exp, SOME(index))
        end
        exp
      end
      SIZE_EXPRESSION(__) => begin
        @assign dims = arrayDims(typeOf(sizeExp.exp))
        if listUtil.all(dims, (x, y=true) -> P_Dimension.Dimension.isKnown(x, y))
          @assign exp = P_Expression.Expression.makeArray(
            ARRAY_TYPE(
              TYPE_INTEGER(),
              list(P_Dimension.Dimension.fromInteger(listLength(dims))),
            ),
            list(P_Dimension.Dimension.sizeExp(d) for d in dims),
          )
        else
          @assign exp = sizeExp
        end
        exp
      end
    end
  end
  return sizeExp
end

function simplifyBinary(binaryExp::Expression)::Expression

  local e1::Expression
  local e2::Expression
  local se1::Expression
  local se2::Expression
  local op::Operator

  @match BINARY_EXPRESSION(e1, op, e2) = binaryExp
  @assign se1 = simplify(e1)
  @assign se2 = simplify(e2)
  @assign binaryExp = simplifyBinaryOp(se1, op, se2)
#  if Flags.isSet(Flags.NF_EXPAND_OPERATIONS) && TODO: John
#     !P_Expression.Expression.hasArrayCall(binaryExp)
#    @assign binaryExp = P_ExpandExp.ExpandExp.expand(binaryExp)
#  end
  return binaryExp
end

function simplifyBinaryOp(exp1::Expression, op::Operator, exp2::Expression)::Expression
  local outExp::Expression

  if isLiteral(exp1) && isLiteral(exp2)
    @assign outExp = Ceval.evalBinaryOp(
      P_ExpandExp.ExpandExp.expand(exp1),
      op,
      P_ExpandExp.ExpandExp.expand(exp2),
    )
    @assign outExp = P_Expression.Expression.stripBindingInfo(outExp)
  else
    @assign outExp = begin
      @match op.op begin
        Op.ADD => begin
          simplifyBinaryAdd(exp1, op, exp2)
        end

        Op.SUB => begin
          simplifyBinarySub(exp1, op, exp2)
        end

        Op.MUL => begin
          simplifyBinaryMul(exp1, op, exp2)
        end

        Op.DIV => begin
          simplifyBinaryDiv(exp1, op, exp2)
        end

        Op.POW => begin
          simplifyBinaryPow(exp1, op, exp2)
        end

        _ => begin
          BINARY_EXPRESSION(exp1, op, exp2)
        end
      end
    end
  end
  return outExp
end

function simplifyBinaryAdd(exp1::Expression, op::Operator, exp2::Expression)::Expression
  local outExp::Expression

  if P_Expression.Expression.isZero(exp1)
    @assign outExp = exp2
  elseif P_Expression.Expression.isZero(exp2)
    @assign outExp = exp1
  elseif P_Expression.Expression.isNegated(exp2)
    @assign outExp = BINARY_EXPRESSION(
      exp1,
      P_Operator.Operator.negate(op),
      P_Expression.Expression.negate(exp2),
    )
  else
    @assign outExp = BINARY_EXPRESSION(exp1, op, exp2)
  end
  #=  0 + e = e
  =#
  #=  e + 0 = e
  =#
  #=  e1 + -(e2) = e1 - e2
  =#
  return outExp
end

function simplifyBinarySub(exp1::Expression, op::Operator, exp2::Expression)::Expression
  local outExp::Expression

  if P_Expression.Expression.isZero(exp1)
    @assign outExp = UNARY_EXPRESSION(
      P_Operator.Operator.makeUMinus(P_Operator.Operator.typeOf(op)),
      exp2,
    )
  elseif P_Expression.Expression.isZero(exp2)
    @assign outExp = exp1
  elseif P_Expression.Expression.isNegated(exp2)
    @assign outExp = BINARY_EXPRESSION(
      exp1,
      P_Operator.Operator.negate(op),
      P_Expression.Expression.negate(exp2),
    )
  else
    @assign outExp = BINARY_EXPRESSION(exp1, op, exp2)
  end
  #=  0 - e = -e
  =#
  #=  e - 0 = e
  =#
  #=  e1 - -(e2) = e1 + e2
  =#
  return outExp
end

function simplifyBinaryMul(
  exp1::Expression,
  op::Operator,
  exp2::Expression,
  switched::Bool = false,
)::Expression
  local outExp::Expression

  @assign outExp = begin
    @match exp1 begin
      INTEGER_EXPRESSION(value = 0) => begin
        exp1
      end

      REAL_EXPRESSION(value = 0.0) => begin
        exp1
      end

      INTEGER_EXPRESSION(value = 1) => begin
        exp2
      end

      REAL_EXPRESSION(value = 1.0) => begin
        exp2
      end

      _ => begin
        if switched
          BINARY_EXPRESSION(exp2, op, exp1)
        else
          simplifyBinaryMul(exp2, op, exp1, true)
        end
      end
    end
  end
  #=  0 * e = 0
  =#
  #=  1 * e = e
  =#
  return outExp
end

function simplifyBinaryDiv(exp1::Expression, op::Operator, exp2::Expression)::Expression
  local outExp::Expression

  #=  e / 1 = e
  =#
  if P_Expression.Expression.isOne(exp2)
    @assign outExp = exp1
  else
    @assign outExp = BINARY_EXPRESSION(exp1, op, exp2)
  end
  return outExp
end

function simplifyBinaryPow(exp1::Expression, op::Operator, exp2::Expression)::Expression
  local outExp::Expression

  if P_Expression.Expression.isZero(exp2)
    @assign outExp = P_Expression.Expression.makeOne(P_Operator.Operator.typeOf(op))
  elseif P_Expression.Expression.isOne(exp2)
    @assign outExp = exp1
  else
    @assign outExp = BINARY_EXPRESSION(exp1, op, exp2)
  end
  return outExp
end

function simplifyUnary(unaryExp::Expression)::Expression

  local e::Expression
  local se::Expression
  local op::Operator

  @match UNARY_EXPRESSION(op, e) = unaryExp
  @assign se = simplify(e)
  @assign unaryExp = simplifyUnaryOp(se, op)
#  if Flags.isSet(Flags.NF_EXPAND_OPERATIONS) && TODO John
#     !P_Expression.Expression.hasArrayCall(unaryExp)
#    @assign unaryExp = P_ExpandExp.ExpandExp.expand(unaryExp)
#  end
  return unaryExp
end

function simplifyUnaryOp(exp::Expression, op::Operator)::Expression
  local outExp::Expression

  if isLiteral(exp)
    @assign outExp = Ceval.evalUnaryOp(exp, op)
    @assign outExp = P_Expression.Expression.stripBindingInfo(outExp)
  else
    @assign outExp = UNARY_EXPRESSION(op, exp)
  end
  return outExp
end

function simplifyLogicBinary(binaryExp::Expression)::Expression

  local e1::Expression
  local e2::Expression
  local se1::Expression
  local se2::Expression
  local op::Operator

  @match LBINARY_EXPRESSION(e1, op, e2) = binaryExp
  @assign se1 = simplify(e1)
  @assign se2 = simplify(e2)
  @assign binaryExp = begin
    @match op.op begin
      Op.AND => begin
        simplifyLogicBinaryAnd(se1, op, se2)
      end

      Op.OR => begin
        simplifyLogicBinaryOr(se1, op, se2)
      end
    end
  end
  return binaryExp
end

function simplifyLogicBinaryAnd(
  exp1::Expression,
  op::Operator,
  exp2::Expression,
)::Expression
  local exp::Expression

  @assign exp = begin
    local expl::List{Expression}
    local o::Operator
    #=  false and e => false
    =#
    @match (exp1, exp2) begin
      (P_Expression.BOOLEAN_EXPRESSION(false), _) => begin
        exp1
      end

      (_, P_Expression.BOOLEAN_EXPRESSION(false)) => begin
        exp2
      end

      (P_Expression.BOOLEAN_EXPRESSION(true), _) => begin
        exp2
      end

      (_, P_Expression.BOOLEAN_EXPRESSION(true)) => begin
        exp1
      end

      (ARRAY_EXPRESSION(__), ARRAY_EXPRESSION(__)) => begin
        #=  e and false => false
        =#
        #=  true and e => e
        =#
        #=  e and true => e
        =#
        @assign o = P_Operator.Operator.unlift(op)
        @assign expl =
          list(@do_threaded_for simplifyLogicBinaryAnd(e1, o, e2) (e1, e2) (
            exp1.elements,
            exp2.elements,
          ))
        P_Expression.Expression.makeArray(P_Operator.Operator.typeOf(op), expl)
      end

      _ => begin
        LBINARY_EXPRESSION(exp1, op, exp2)
      end
    end
  end
  return exp
end

function simplifyLogicBinaryOr(exp1::Expression, op::Operator, exp2::Expression)::Expression
  local exp::Expression

  @assign exp = begin
    local expl::List{Expression}
    local o::Operator
    #=  true or e => true
    =#
    @match (exp1, exp2) begin
      (P_Expression.BOOLEAN_EXPRESSION(true), _) => begin
        exp1
      end

      (_, P_Expression.BOOLEAN_EXPRESSION(true)) => begin
        exp2
      end

      (P_Expression.BOOLEAN_EXPRESSION(false), _) => begin
        exp2
      end

      (_, P_Expression.BOOLEAN_EXPRESSION(false)) => begin
        exp1
      end

      (ARRAY_EXPRESSION(__), ARRAY_EXPRESSION(__)) => begin
        #=  e or true => true
        =#
        #=  false or e => e
        =#
        #=  e or false => e
        =#
        @assign o = P_Operator.Operator.unlift(op)
        @assign expl =
          list(@do_threaded_for simplifyLogicBinaryAnd(e1, o, e2) (e1, e2) (
            exp1.elements,
            exp2.elements,
          ))
        makeArray(P_Operator.Operator.typeOf(op), expl)
      end

      _ => begin
        LBINARY_EXPRESSION(exp1, op, exp2)
      end
    end
  end
  return exp
end

function simplifyLogicUnary(unaryExp::Expression)::Expression

  local e::Expression
  local se::Expression
  local op::Operator

  @match LUNARY_EXPRESSION(op, e) = unaryExp
  @assign se = simplify(e)
  if isLiteral(se)
    @assign unaryExp = Ceval.evalLogicUnaryOp(se, op)
    @assign unaryExp = P_Expression.Expression.stripBindingInfo(unaryExp)
  elseif !referenceEq(e, se)
    @assign unaryExp = LUNARY_EXPRESSION(op, se)
  end
  return unaryExp
end

function simplifyRelation(relationExp::Expression)::Expression

  local e1::Expression
  local e2::Expression
  local se1::Expression
  local se2::Expression
  local op::Operator

  @match RELATION_EXPRESSION(e1, op, e2) = relationExp
  @assign se1 = simplify(e1)
  @assign se2 = simplify(e2)
  if isLiteral(se1) && isLiteral(se2)
    @assign relationExp = Ceval.evalRelationOp(se1, op, se2)
    @assign relationExp = P_Expression.Expression.stripBindingInfo(relationExp)
  elseif !(referenceEq(e1, se1) && referenceEq(e2, se2))
    @assign relationExp = RELATION_EXPRESSION(se1, op, se2)
  end
  return relationExp
end

function simplifyIf(ifExp::Expression)::Expression

  local cond::Expression
  local tb::Expression
  local fb::Expression

  @match IF_EXPRESSION(cond, tb, fb) = ifExp
  @assign cond = simplify(cond)
  @assign ifExp = begin
    @match cond begin
      P_Expression.BOOLEAN_EXPRESSION(__) => begin
        simplify(if cond.value
          tb
        else
          fb
        end)
      end

      _ => begin
        @assign tb = simplify(tb)
        @assign fb = simplify(fb)
        if P_Expression.Expression.isEqual(tb, fb)
          tb
        else
          IF_EXPRESSION(cond, tb, fb)
        end
      end
    end
  end
  return ifExp
end

function simplifyCast(exp::Expression, ty::M_Type)::Expression
  local castExp::Expression

  @assign castExp = begin
    local ety::M_Type
    @match (ty, exp) begin
      (TYPE_REAL(__), INTEGER_EXPRESSION(__)) => begin
        P_Expression.REAL_EXPRESSION(intReal(exp.value))
      end

      (ARRAY_TYPE(elementType = TYPE_REAL(__)), ARRAY_EXPRESSION(__)) =>
        begin
          @assign ety = Type.unliftArray(ty)
          @assign exp.elements = list(simplifyCast(e, ety) for e in exp.elements)
          @assign exp.ty = setArrayElementType(exp.ty, arrayElementType(ty))
          exp
        end

      _ => begin
        P_Expression.Expression.CAST(ty, exp)
      end
    end
  end
  return castExp
end

function simplifySubscriptedExp(subscriptedExp::Expression)::Expression
  local e::Expression
  local subs::List{Subscript}
  local ty::NFtype
  @match SUBSCRIPTED_EXP_EXPRESSION(e, subs, ty) = subscriptedExp
  @assign subscriptedExp = simplify(e)
  @assign subscriptedExp = applySubscripts(
    list(simplify(s) for s in subs),
    subscriptedExp
  )
  return subscriptedExp
end

function simplifyTupleElement(tupleExp::Expression)::Expression

  local e::Expression
  local index::Integer
  local ty::M_Type

  @match TUPLE_EXPRESSION_ELEMENT(e, index, ty) = tupleExp
  @assign e = simplify(e)
  @assign tupleExp = P_Expression.Expression.tupleElement(e, ty, index)
  return tupleExp
end
