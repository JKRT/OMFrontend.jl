function simplify(@nospecialize(exp::Expression))
  exp = begin
    @match exp begin
      CREF_EXPRESSION(__) => begin
        local expCref = simplifySubscripts(exp.cref)
        local expTy = getSubscriptedType(exp.cref)
        CREF_EXPRESSION(expTy, expCref)
      end

      ARRAY_EXPRESSION(__) => begin
        expElements = list(simplify(e) for e in exp.elements)
        ARRAY_EXPRESSION(exp.ty, expElements, exp.literal)
      end

      RANGE_EXPRESSION(__) => begin
        simplifyRange(exp)
      end

      RECORD_EXPRESSION(__) => begin
        exp.elements = Expression[simplify(e) for e in exp.elements]
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

function simplifyOpt(exp::Option{<:Expression})
  local e::Expression
   exp = begin
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

function simplifyRange(range::Expression)
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
   start_exp2 = simplify(start_exp1)
   step_exp2 = simplifyOpt(step_exp1)
   stop_exp2 = simplify(stop_exp1)
  if referenceEq(start_exp1, start_exp2) &&
     referenceEq(step_exp1, step_exp2) &&
     referenceEq(stop_exp1, stop_exp2)
     exp = range
  else
     ty = TypeCheck.getRangeType(
      start_exp2,
      step_exp2,
      stop_exp2,
      arrayElementType(ty),
      AbsynUtil.dummyInfo,
    )
     exp = RANGE_EXPRESSION(ty, start_exp2, step_exp2, stop_exp2)
  end
  return exp
end

function simplifyCall(callExp::Expression)
  local call::Call
  local args::List{Expression}
  local builtin::Bool
  local is_pure::Bool
  @match CALL_EXPRESSION(call = call) = callExp
  callExp = begin
    @match call begin
      TYPED_CALL(arguments = args) where {(!isExternal(call))} => begin
        if Flags.isSet(Flags.NF_EXPAND_FUNC_ARGS)
          args = list(if hasArrayCall(arg)
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
             callExp = simplify(listHead(args))
            return
          end
        end
        args = list(simplify(arg) for arg in args)
        callArgs = args
        call = TYPED_CALL(call.fn, call.ty, call.var, callArgs, call.attributes)
        builtin = isBuiltin(call.fn)
        is_pure = !isImpure(call.fn)
        #=  Use Ceval for builtin pure functions with literal arguments.
        =#
        if builtin
          if is_pure && ListUtil.all(args, isLiteral)
            try
              callExp = Ceval.evalCall(call, P_EvalTarget.IGNORE_ERRORS())
              callExp = stripBindingInfo(callExp)
            catch
            end
          else
            if Flags.isSet(Flags.NF_SCALARIZE)
              callExp =
                simplify(nameConsiderBuiltin(call.fn), args, call)
            end
          end
        elseif Flags.isSet(Flags.NF_EVAL_CONST_ARG_FUNCS) &&
               is_pure &&
               ListUtil.all(args, isLiteral)
          callExp = simplifyCall2(call)
        else
          callExp = CALL_EXPRESSION(call)
        end
        #=  do not expand builtin calls if we should not scalarize
        =#
        #=  nothing
        =#
        callExp
      end

      TYPED_ARRAY_CONSTRUCTOR(__) => begin
        simplifyArrayConstructor(call)
      end

      TYPED_REDUCTION(__) => begin
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

function simplifyCall2(call::Call)
  local outExp::Expression

  ErrorExt.setCheckpoint(getInstanceName())
  try
     outExp = Ceval.evalCall(call, P_EvalTarget.IGNORE_ERRORS())
     outExp = stripBindingInfo(outExp)
    ErrorExt.delCheckpoint(getInstanceName())
  catch
    if Flags.isSet(Flags.FAILTRACE)
      ErrorExt.delCheckpoint(getInstanceName())
      Debug.traceln(
        "- " + getInstanceName() + " failed to evaluate " + toString(call) + "\\n",
      )
    else
      ErrorExt.rollBack(getInstanceName())
    end
     outExp = CALL_EXPRESSION(call)
  end
  return outExp
end

function simplify(
  name::Absyn.Path,
  args::List{<:Expression},
  call::Call,
)
  local exp::Expression
  exp = begin
    @match AbsynUtil.pathFirstIdent(name) begin
      "cat" => begin
        (exp, _) = expandBuiltinCat(args, call)
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

function simplifySumProduct(arg::Expression, call::Call, isSum::Bool)
  local exp::Expression

  local expanded::Bool
  local args::List{Expression}
  local ty::M_Type
  local op::Operator

   (exp, expanded) = P_ExpandExp.ExpandExp.expand(arg)
  if expanded
     args = arrayScalarElements(exp)
     ty = arrayElementType(typeOf(arg))
    if listEmpty(args)
       exp = if isSum
        makeZero(ty)
      else
        makeOne(ty)
      end
    else
      @match _cons(exp, args) = args
       op = if isSum
        makeAdd(ty)
      else
        makeMul(ty)
      end
      for e in args
         exp = BINARY_EXPRESSION(exp, op, e)
      end
    end
  else
     exp = CALL_EXPRESSION(call)
  end
  return exp
end

function simplifyTranspose(arg::Expression, call::Call)
  local exp::Expression
  local e::Expression
  if hasArrayCall(arg)
    e = arg
  else
    (e, _) = expand(arg)
  end
  exp = begin
    @match e begin
      ARRAY_EXPRESSION(
        __,
      ) where {(ListUtil.all(e.elements, isArray))} => begin
        transposeArray(e)
      end
      _ => begin
        CALL_EXPRESSION(call)
      end
    end
  end
  return exp
end

function simplifyArrayConstructor(call::Call)
  local outExp::Expression
  local ty::M_Type
  local var::VariabilityType
  local exp::Expression
  local e::Expression
  local iters::List{Tuple{InstNode, Expression}}
  local iter::InstNode
  local dim::Dimension
  local dim_size::Int
  local expanded::Bool
  @match TYPED_ARRAY_CONSTRUCTOR(ty, var, exp, iters) = call
  iters = list((Util.tuple21(i), simplify(Util.tuple22(i))) for i in iters)
  outExp = begin
    @matchcontinue iters begin
      (iter, e) <| nil() => begin
        @match TYPE_ARRAY(dimensions = dim <| nil) = typeOf(e)
        dim_size = size(dim)
        if dim_size == 0
          outExp = makeEmptyArray(ty)
        elseif dim_size == 1
          @match (ARRAY_EXPRESSION(elements = list(e)), _) =
            expand(e)
          exp = replaceIterator(exp, iter, e)
          exp = makeArray(ty, list(exp))
          outExp = simplify(exp)
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
         exp = simplify(exp)
        CALL_EXPRESSION(TYPED_ARRAY_CONSTRUCTOR(ty, var, exp, iters))
      end
    end
  end
  return outExp
end

function simplifySize(sizeExp::Expression)
   sizeExp = begin
    local exp::Expression
    local index::Expression
    local dim::Dimension
    local dims::List{Dimension}
    @match sizeExp begin
      SIZE_EXPRESSION(exp, SOME(index)) => begin
        index = simplify(index)
        if isLiteral(index)
          dim = listGet(
            arrayDims(typeOf(exp)),
            toInteger(index),
          )
          if isKnown(dim)
            exp = INTEGER_EXPRESSION(size(dim))
          else
            exp = SIZE_EXPRESSION(exp, SOME(index))
          end
        else
          exp = SIZE_EXPRESSION(exp, SOME(index))
        end
        exp
      end
      SIZE_EXPRESSION(__) => begin
         dims = arrayDims(typeOf(sizeExp.exp))
        if listUtil.all(dims, (x, y=true) -> P_Dimension.Dimension.isKnown(x, y))
           exp = makeArray(
            TYPE_ARRAY(
              TYPE_INTEGER(),
              list(P_Dimension.Dimension.fromInteger(listLength(dims))),
            ),
            list(P_Dimension.Dimension.sizeExp(d) for d in dims),
          )
        else
           exp = sizeExp
        end
        exp
      end
    end
  end
  return sizeExp
end

function simplifyBinary(binaryExp::Expression)
  local e1::Expression
  local e2::Expression
  local se1::Expression
  local se2::Expression
  local op::Operator
  @match BINARY_EXPRESSION(e1, op, e2) = binaryExp
   se1 = simplify(e1)
   se2 = simplify(e2)
   binaryExp = simplifyBinaryOp(se1, op, se2)
#  if Flags.isSet(Flags.NF_EXPAND_OPERATIONS) && TODO: John
#     !hasArrayCall(binaryExp)
#    @assign binaryExp = P_ExpandExp.ExpandExp.expand(binaryExp)
#  end
  return binaryExp
end

function simplifyBinaryOp(exp1::Expression, op::Operator, exp2::Expression)
  local outExp::Expression

  if isLiteral(exp1) && isLiteral(exp2)
    outExp = evalBinaryOp(
      expand(exp1)[1],
      op,
      expand(exp2)[1],
    )
    outExp = stripBindingInfo(outExp)
  else
    outExp = begin
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

function simplifyBinaryAdd(exp1::Expression, op::Operator, exp2::Expression)
  local outExp::Expression

  if isZero(exp1)
    outExp = exp2
  elseif isZero(exp2)
    outExp = exp1
  elseif isNegated(exp2)
    outExp = BINARY_EXPRESSION(
      exp1,
      negate(op),
      negate(exp2),
    )
  else
    outExp = BINARY_EXPRESSION(exp1, op, exp2)
  end
  #=  0 + e = e
  =#
  #=  e + 0 = e
  =#
  #=  e1 + -(e2) = e1 - e2
  =#
  return outExp
end

function simplifyBinarySub(exp1::Expression, op::Operator, exp2::Expression)
  local outExp::Expression

  if isZero(exp1)
     outExp = UNARY_EXPRESSION(
      makeUMinus(typeOf(op)),
      exp2,
    )
  elseif isZero(exp2)
     outExp = exp1
  elseif isNegated(exp2)
     outExp = BINARY_EXPRESSION(
      exp1,
      negate(op),
      negate(exp2),
    )
  else
     outExp = BINARY_EXPRESSION(exp1, op, exp2)
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
)
  local outExp::Expression

   outExp = begin
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

function simplifyBinaryDiv(exp1::Expression, op::Operator, exp2::Expression)
  local outExp::Expression

  #=  e / 1 = e
  =#
  if isOne(exp2)
     outExp = exp1
  else
     outExp = BINARY_EXPRESSION(exp1, op, exp2)
  end
  return outExp
end

function simplifyBinaryPow(exp1::Expression, op::Operator, exp2::Expression)
  local outExp::Expression

  if isZero(exp2)
     outExp = makeOne(typeOf(op))
  elseif isOne(exp2)
     outExp = exp1
  else
     outExp = BINARY_EXPRESSION(exp1, op, exp2)
  end
  return outExp
end

function simplifyUnary(unaryExp::Expression)

  local e::Expression
  local se::Expression
  local op::Operator

  @match UNARY_EXPRESSION(op, e) = unaryExp
   se = simplify(e)
   unaryExp = simplifyUnaryOp(se, op)
#  if Flags.isSet(Flags.NF_EXPAND_OPERATIONS) && TODO John
#     !hasArrayCall(unaryExp)
#     unaryExp = P_ExpandExp.ExpandExp.expand(unaryExp)
#  end
  return unaryExp
end

function simplifyUnaryOp(exp::Expression, op::Operator)
  local outExp::Expression
  if isLiteral(exp)
    outExp = evalUnaryOp(exp, op)
    outExp = stripBindingInfo(outExp)
  else
    outExp = UNARY_EXPRESSION(op, exp)
  end
  return outExp
end

function simplifyLogicBinary(binaryExp::Expression)

  local e1::Expression
  local e2::Expression
  local se1::Expression
  local se2::Expression
  local op::Operator

  @match LBINARY_EXPRESSION(e1, op, e2) = binaryExp
   se1 = simplify(e1)
   se2 = simplify(e2)
   binaryExp = begin
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
)
  local exp::Expression

   exp = begin
    local expl::List{Expression}
    local o::Operator
    #=  false and e => false
    =#
    @match (exp1, exp2) begin
      (BOOLEAN_EXPRESSION(false), _) => begin
        exp1
      end

      (_, BOOLEAN_EXPRESSION(false)) => begin
        exp2
      end

      (BOOLEAN_EXPRESSION(true), _) => begin
        exp2
      end

      (_, BOOLEAN_EXPRESSION(true)) => begin
        exp1
      end

      (ARRAY_EXPRESSION(__), ARRAY_EXPRESSION(__)) => begin
        #=  e and false => false
        =#
        #=  true and e => e
        =#
        #=  e and true => e
        =#
         o = unlift(op)
         expl =
          list(@do_threaded_for simplifyLogicBinaryAnd(e1, o, e2) (e1, e2) (
            exp1.elements,
            exp2.elements,
          ))
        makeArray(typeOf(op), expl)
      end

      _ => begin
        LBINARY_EXPRESSION(exp1, op, exp2)
      end
    end
  end
  return exp
end

function simplifyLogicBinaryOr(exp1::Expression, op::Operator, exp2::Expression)
  local exp::Expression

   exp = begin
    local expl::List{Expression}
    local o::Operator
    #=  true or e => true
    =#
    @match (exp1, exp2) begin
      (BOOLEAN_EXPRESSION(true), _) => begin
        exp1
      end

      (_, BOOLEAN_EXPRESSION(true)) => begin
        exp2
      end

      (BOOLEAN_EXPRESSION(false), _) => begin
        exp2
      end

      (_, BOOLEAN_EXPRESSION(false)) => begin
        exp1
      end

      (ARRAY_EXPRESSION(__), ARRAY_EXPRESSION(__)) => begin
        #=  e or true => true
        =#
        #=  false or e => e
        =#
        #=  e or false => e
        =#
         o = unlift(op)
         expl =
          list(@do_threaded_for simplifyLogicBinaryAnd(e1, o, e2) (e1, e2) (
            exp1.elements,
            exp2.elements,
          ))
        makeArray(typeOf(op), expl)
      end

      _ => begin
        LBINARY_EXPRESSION(exp1, op, exp2)
      end
    end
  end
  return exp
end

function simplifyLogicUnary(unaryExp::Expression)
  local e::Expression
  local se::Expression
  local op::Operator
  @match LUNARY_EXPRESSION(op, e) = unaryExp
  se = simplify(e)
  if isLiteral(se)
    unaryExp = evalLogicUnaryOp(se, op)
    unaryExp = stripBindingInfo(unaryExp)
  elseif !referenceEq(e, se)
    unaryExp = LUNARY_EXPRESSION(op, se)
  end
  return unaryExp
end

function simplifyRelation(relationExp::Expression)

  local e1::Expression
  local e2::Expression
  local se1::Expression
  local se2::Expression
  local op::Operator

  @match RELATION_EXPRESSION(e1, op, e2) = relationExp
   se1 = simplify(e1)
   se2 = simplify(e2)
  if isLiteral(se1) && isLiteral(se2)
     relationExp = evalRelationOp(se1, op, se2)
     relationExp = stripBindingInfo(relationExp)
  elseif !(referenceEq(e1, se1) && referenceEq(e2, se2))
     relationExp = RELATION_EXPRESSION(se1, op, se2)
  end
  return relationExp
end

function simplifyIf(ifExp::Expression)

  local cond::Expression
  local tb::Expression
  local fb::Expression

  @match IF_EXPRESSION(cond, tb, fb) = ifExp
   cond = simplify(cond)
   ifExp = begin
    @match cond begin
      BOOLEAN_EXPRESSION(__) => begin
        simplify(if cond.value
          tb
        else
          fb
        end)
      end
      _ => begin
         tb = simplify(tb)
         fb = simplify(fb)
        if isEqual(tb, fb)
          tb
        else
          IF_EXPRESSION(cond, tb, fb)
        end
      end
    end
  end
  return ifExp
end

function simplifyCast(exp::Expression, ty::NFType)
  local castExp::Expression
  castExp = begin
    local ety::NFType
    @match (ty, exp) begin
      (TYPE_REAL(__), INTEGER_EXPRESSION(__)) => begin
        REAL_EXPRESSION(intReal(exp.value))
      end
      (TYPE_ARRAY(elementType = TYPE_REAL(__)), ARRAY_EXPRESSION(__)) =>
        begin
          ety = unliftArray(ty)
          expElements = list(simplifyCast(e, ety) for e in exp.elements)
          expTy = setArrayElementType(exp.ty, arrayElementType(ty))
          ARRAY_EXPRESSION(expTy, expElements, exp.literal)
        end
      _ => begin
        CAST_EXPRESSION(ty, exp)
      end
    end
  end
  return castExp
end

function simplifySubscriptedExp(subscriptedExp::Expression)
  local e::Expression
  local subs::List{Subscript}
  local ty::NFType
  @match SUBSCRIPTED_EXP_EXPRESSION(e, subs, ty) = subscriptedExp
  subscriptedExp = simplify(e)
  subscriptedExp = applySubscripts(
    list(simplifySubscript(s) for s in subs),
    subscriptedExp
  )
  return subscriptedExp
end

function simplifyTupleElement(tupleExp::Expression)
  local e::Expression
  local index::Int
  local ty::M_Type
  @match TUPLE_ELEMENT_EXPRESSION(e, index, ty) = tupleExp
  e = simplify(e)
  tupleExp = tupleElement(e, ty, index)
  return tupleExp
end
