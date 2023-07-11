struct MatchKindStruct{T <: Number}
  EXACT::T #= Exact match =#
  CAST::T  #= Matched by casting, e.g. Integer to Real =#
  UNKNOWN_EXPECTED::T #= The expected type was unknown =#
  UNKNOWN_ACTUAL::T #= The actual type was unknown =#
  GENERIC::T #= Matched with a generic type e.g. function F<T> input T i; end F; F(1) =#
  PLUG_COMPATIBLE::T  #= Component by component matching, e.g. class A R r; end A; is plug compatible with class B R r; end B; =#
  NOT_COMPATIBLE::T
end

const MatchKind::MatchKindStruct{Int} = MatchKindStruct(1,2,3,4,5,6,7)
const MatchKindType = Int

function isCompatibleMatch(kind::MatchKindType)::Bool
  local isCompatible::Bool = kind != MatchKind.NOT_COMPATIBLE
  return isCompatible
end

function isIncompatibleMatch(kind::MatchKindType)::Bool
  local isIncompatible::Bool = kind == MatchKind.NOT_COMPATIBLE
  return isIncompatible
end

function isExactMatch(kind::MatchKindType)::Bool
  local isCompatible::Bool = kind == MatchKind.EXACT
  return isCompatible
end

function isCastMatch(kind::MatchKindType)::Bool
  local isCast::Bool = kind == MatchKind.CAST
  return isCast
end

function isGenericMatch(kind::MatchKindType)::Bool
  local isCast::Bool = kind == MatchKind.GENERIC
  return isCast
end

function isValidAssignmentMatch(kind::MatchKindType)::Bool
  local v::Bool =
    kind == MatchKind.EXACT || kind == MatchKind.CAST || kind == MatchKind.PLUG_COMPATIBLE
  return v
end

function isValidArgumentMatch(kind::MatchKindType)::Bool
  local v::Bool =
    kind == MatchKind.EXACT ||
    kind == MatchKind.CAST ||
    kind == MatchKind.GENERIC ||
    kind == MatchKind.PLUG_COMPATIBLE
  return v
end

function isValidPlugCompatibleMatch(kind::MatchKindType)::Bool
  local v::Bool = kind == MatchKind.EXACT || kind == MatchKind.PLUG_COMPATIBLE
  return v
end

function checkBinaryOperation(
  exp1::Expression,
  type1::M_Type,
  var1::VariabilityType,
  operator::Operator,
  exp2::Expression,
  type2::M_Type,
  var2::VariabilityType,
  info::SourceInfo,
)::Tuple{Expression, M_Type}
  local resultType::M_Type
  local binaryExp::Expression

  if isComplex(arrayElementType(type1)) ||
     isComplex(arrayElementType(type2))
    @assign (binaryExp, resultType) =
      checkOverloadedBinaryOperator(exp1, type1, var1, operator, exp2, type2, var2, info)
  elseif isBoxed(type1) && isBoxed(type2)
    @assign (binaryExp, resultType) =
      checkBinaryOperationBoxed(exp1, type1, var1, operator, exp2, type2, var2, info)
  else
    @assign (binaryExp, resultType) = begin
      @match operator.op begin
        Op.ADD => begin
          checkBinaryOperationAdd(exp1, type1, exp2, type2, info)
        end

        Op.SUB => begin
          checkBinaryOperationSub(exp1, type1, exp2, type2, info)
        end

        Op.MUL => begin
          checkBinaryOperationMul(exp1, type1, exp2, type2, info)
        end

        Op.DIV => begin
          checkBinaryOperationDiv(exp1, type1, exp2, type2, info, false)
        end

        Op.POW => begin
          checkBinaryOperationPow(exp1, type1, exp2, type2, info)
        end

        Op.ADD_EW => begin
          checkBinaryOperationEW(exp1, type1, exp2, type2, Op.ADD, info)
        end

        Op.SUB_EW => begin
          checkBinaryOperationEW(exp1, type1, exp2, type2, Op.SUB, info)
        end

        Op.MUL_EW => begin
          checkBinaryOperationEW(exp1, type1, exp2, type2, Op.MUL, info)
        end

        Op.DIV_EW => begin
          checkBinaryOperationDiv(exp1, type1, exp2, type2, info, isElementWise = true)
        end

        Op.POW_EW => begin
          checkBinaryOperationPowEW(exp1, type1, exp2, type2, info)
        end
      end
    end
  end
  return (binaryExp, resultType)
end

function checkOverloadedBinaryOperator(
  exp1::Expression,
  type1::M_Type,
  var1::VariabilityType,
  op::Operator,
  exp2::Expression,
  type2::M_Type,
  var2::VariabilityType,
  info::SourceInfo,
)::Tuple{Expression, M_Type}
  local outType::M_Type
  local outExp::Expression

  local op_str::String
  local candidates::List{M_Function}
  local ety1::M_Type
  local ety2::M_Type

  op_str = symbol(stripEW(op), "'")
  ety1 = arrayElementType(type1)
  ety2 = arrayElementType(type2)
  candidates = lookupOperatorFunctionsInType(op_str, ety1)
  #=  Only collect operators from both types if they're not the same type.
  =#
  if !isEqual(ety1, ety2)
    candidates = listAppend(
      lookupOperatorFunctionsInType(op_str, ety2),
      candidates,
    )
  end
  #=  Give up if no operator functions could be found. =#
  if listEmpty(candidates)
    printUnresolvableTypeError(
      BINARY_EXPRESSION(exp1, op, exp2),
      list(type1, type2),
      info,
    )
  end
  if isElementWise(op)
    @assign (outExp, outType) = checkOverloadedBinaryArrayEW(
      exp1,
      type1,
      var1,
      stripEW(op),
      exp2,
      type2,
      var2,
      candidates,
      info,
    )
  else
    @assign (outExp, outType) = matchOverloadedBinaryOperator(
      exp1,
      type1,
      var1,
      op,
      exp2,
      type2,
      var2,
      candidates,
      info,
    )
  end
  return (outExp, outType)
end

function matchOverloadedBinaryOperator(
  exp1::Expression,
  type1::M_Type,
  var1::VariabilityType,
  op::Operator,
  exp2::Expression,
  type2::M_Type,
  var2::VariabilityType,
  candidates::List{<:M_Function},
  info::SourceInfo,
  showErrors::Bool = true,
)::Tuple{Expression, M_Type}
  local outType::M_Type
  local outExp::Expression

  local args::List{TypedArg}
  local matchKind::FunctionMatchKind
  local matchedFunc::MatchedFunction
  local matchedFunctions::List{MatchedFunction}
  local exactMatches::List{MatchedFunction}
  local fn::M_Function
  local oop::Op

  @assign args = list((exp1, type1, var1), (exp2, type2, var2))
  @assign matchedFunctions = matchFunctionsSilent(candidates, args, nil, info)
  #=  We only allow exact matches for operator overloading. e.g. no casting or generic matches.
  =#
  @assign exactMatches = getExactMatches(matchedFunctions)
  if listEmpty(exactMatches)
    ErrorExt.setCheckpoint("NFTypeCheck:implicitConstruction")
    try
      @assign (outExp, outType) =
        implicitConstructAndMatch(candidates, exp1, type1, op, exp2, type2, info)
      if showErrors
        ErrorExt.delCheckpoint("NFTypeCheck:implicitConstruction")
      else
        ErrorExt.rollBack("NFTypeCheck:implicitConstruction")
      end
    catch
      ErrorExt.rollBack("NFTypeCheck:implicitConstruction")
      if isArray(type1) || isArray(type2)
        @assign (outExp, outType) = begin
          @match op.op begin
            Op.ADD => begin
              checkOverloadedBinaryArrayAddSub(
                exp1,
                type1,
                var1,
                op,
                exp2,
                type2,
                var2,
                candidates,
                info,
              )
            end

            Op.SUB => begin
              checkOverloadedBinaryArrayAddSub(
                exp1,
                type1,
                var1,
                op,
                exp2,
                type2,
                var2,
                candidates,
                info,
              )
            end

            Op.MUL => begin
              checkOverloadedBinaryArrayMul(
                exp1,
                type1,
                var1,
                op,
                exp2,
                type2,
                var2,
                candidates,
                info,
              )
            end

            Op.DIV => begin
              checkOverloadedBinaryArrayDiv(
                exp1,
                type1,
                var1,
                op,
                exp2,
                type2,
                var2,
                candidates,
                info,
              )
            end

            _ => begin
              #=  TODO: new error mentioning overloaded operators.
              =#
              printUnresolvableTypeError(
                BINARY_EXPRESSION(exp1, op, exp2),
                list(type1, type2),
                info,
                showErrors,
              )
              fail()
            end
          end
        end
      else
        printUnresolvableTypeError(
          BINARY_EXPRESSION(exp1, op, exp2),
          list(type1, type2),
          info,
          showErrors,
        )
      end
    end
  elseif listLength(exactMatches) == 1
    @match _cons(matchedFunc, _) = exactMatches
    fn = matchedFunc.func
    outType = returnType(fn)
    outExp = CALL_EXPRESSION(makeTypedCall(
      matchedFunc.func,
      list(Util.tuple31(a) for a in matchedFunc.args),
      variabilityMax(var1, var2),
      outType,
    ))
  else
    if showErrors
      Error.addSourceMessage(
        Error.AMBIGUOUS_MATCHING_OPERATOR_FUNCTIONS_NFINST,
        list(
          toString(BINARY_EXPRESSION(exp1, op, exp2)),
          P_Function.candidateFuncListString(list(mfn.func for mfn in matchedFunctions)),
        ),
        info,
      )
    end
    fail()
  end
  return (outExp, outType)
end

function checkBinaryOperationBoxed(
  exp1::Expression,
  type1::M_Type,
  var1::VariabilityType,
  op::Operator,
  exp2::Expression,
  type2::M_Type,
  var2::VariabilityType,
  info::SourceInfo,
)::Tuple{Expression, M_Type}
  local outType::M_Type
  local outExp::Expression

  local e1::Expression
  local e2::Expression
  local ty1::M_Type
  local ty2::M_Type

  @assign (e1, ty1) = matchTypes(type1, Type.unbox(type1), exp1)
  @assign (e2, ty2) = matchTypes(type2, Type.unbox(type2), exp2)
  @assign (outExp, outType) = checkBinaryOperation(e1, ty1, var1, op, e2, ty2, var2, info)
  return (outExp, outType)
end

function checkOverloadedBinaryArrayAddSub(
  exp1::Expression,
  type1::M_Type,
  var1::VariabilityType,
  op::Operator,
  exp2::Expression,
  type2::M_Type,
  var2::VariabilityType,
  candidates::List{<:M_Function},
  info::SourceInfo,
)::Tuple{Expression, M_Type}
  local outType::M_Type
  local outExp::Expression

  local e1::Expression
  local e2::Expression
  local mk::MatchKindType

  #=  For addition or subtraction both sides must have the same type.
  =#
  @assign (e1, e2, _, mk) = matchExpressions(exp1, type1, exp2, type2, true)
  if !isCompatibleMatch(mk)
    printUnresolvableTypeError(
      BINARY_EXPRESSION(e1, op, e2),
      list(type1, type2),
      info,
    )
  end
  @assign e1 = P_ExpandExp.ExpandExp.expand(e1)
  @assign e2 = P_ExpandExp.ExpandExp.expand(e2)
  @assign (outExp, outType) = checkOverloadedBinaryArrayAddSub2(
    e1,
    type1,
    var1,
    op,
    e2,
    type2,
    var2,
    candidates,
    info,
  )
  return (outExp, outType)
end

function checkOverloadedBinaryArrayAddSub2(
  exp1::Expression,
  type1::M_Type,
  var1::VariabilityType,
  op::Operator,
  exp2::Expression,
  type2::M_Type,
  var2::VariabilityType,
  candidates::List{<:M_Function},
  info::SourceInfo,
)::Tuple{Expression, M_Type}
  local outType::M_Type
  local outExp::Expression

  @assign (outExp, outType) = begin
    local ty::M_Type
    local ty1::M_Type
    local ty2::M_Type
    local e::Expression
    local e2::Expression
    local expl::List{Expression}
    local expl1::List{Expression}
    local expl2::List{Expression}
    @match (exp1, exp2) begin
      (
        ARRAY_EXPRESSION(elements = expl1),
        ARRAY_EXPRESSION(elements = expl2),
      ) => begin
        @assign expl = nil
        if listEmpty(expl1)
          @assign ty1 = arrayElementType(type1)
          @assign ty2 = arrayElementType(type2)
          try
            @assign (_, ty) = matchOverloadedBinaryOperator(
              EMPTY(ty1),
              ty1,
              var1,
              op,
              EMPTY(ty2),
              ty2,
              var2,
              candidates,
              info,
              showErrors = false,
            )
          catch
            printUnresolvableTypeError(
              BINARY_EXPRESSION(exp1, op, exp2),
              list(type1, type2),
              info,
            )
          end
        else
          @assign ty1 = Type.unliftArray(type1)
          @assign ty2 = Type.unliftArray(type2)
          for e1 in expl1
            @match _cons(e2, expl2) = expl2
            @assign (e, ty) = checkOverloadedBinaryArrayAddSub2(
              e1,
              ty1,
              var1,
              op,
              e2,
              ty2,
              var2,
              candidates,
              info,
            )
            @assign expl = _cons(e, expl)
          end
          @assign expl = listReverseInPlace(expl)
        end
        #=  If the arrays are empty, match against the element types to get the expected return type.
        =#
        @assign outType = setArrayElementType(type1, ty)
        @assign outExp = makeArray(outType, expl)
        (outExp, outType)
      end

      _ => begin
        matchOverloadedBinaryOperator(
          exp1,
          type1,
          var1,
          op,
          exp2,
          type2,
          var2,
          candidates,
          info,
        )
      end
    end
  end
  return (outExp, outType)
end

function checkOverloadedBinaryArrayMul(
  exp1::Expression,
  type1::M_Type,
  var1::VariabilityType,
  op::Operator,
  exp2::Expression,
  type2::M_Type,
  var2::VariabilityType,
  candidates::List{<:M_Function},
  info::SourceInfo,
)::Tuple{Expression, M_Type}
  local outType::M_Type
  local outExp::Expression

  local valid::Bool
  local dims1::List{Dimension}
  local dims2::List{Dimension}
  local dim11::Dimension
  local dim12::Dimension
  local dim21::Dimension
  local dim22::Dimension

  @assign dims1 = arrayDims(type1)
  @assign dims2 = arrayDims(type2)
  @assign (valid, outExp) = begin
    @match (dims1, dims2) begin
      (nil(), _ <| nil()) => begin
        #=  scalar * array = array
        =#
        @assign outExp = checkOverloadedBinaryScalarArray(
          exp1,
          type1,
          var1,
          op,
          exp2,
          type2,
          var2,
          candidates,
          info,
        )
        (true, outExp)
      end

      (_ <| nil(), nil()) => begin
        #=  array * scalar = array
        =#
        @assign outExp = checkOverloadedBinaryArrayScalar(
          exp1,
          type1,
          var1,
          op,
          exp2,
          type2,
          var2,
          candidates,
          info,
        )
        (true, outExp)
      end

      (dim11 <| dim12 <| nil(), dim21 <| nil()) => begin
        #=  matrix[n, m] * vector[m] = vector[n]
        =#
        @assign valid = isEqual(dim12, dim21)
        #=  TODO: Implement me!
        =#
        @assign outExp = BINARY_EXPRESSION(exp1, op, exp2)
        @assign valid = false
        (valid, outExp)
      end

      (dim11 <| dim12 <| nil(), dim21 <| dim22 <| nil()) => begin
        #=  matrix[n, m] * matrix[m, p] = vector[n, p]
        =#
        @assign valid = isEqual(dim12, dim21)
        #=  TODO: Implement me!
        =#
        @assign outExp = BINARY_EXPRESSION(exp1, op, exp2)
        @assign valid = false
        (valid, outExp)
      end

      _ => begin
        (false, BINARY_EXPRESSION(exp1, op, exp2))
      end
    end
  end
  #=  scalar * scalar should never get here.
  =#
  #=  vector * vector and vector * matrix are undefined for overloaded operators.
  =#
  if !valid
    printUnresolvableTypeError(outExp, list(type1, type2), info)
  end
  @assign outType = typeOf(outExp)
  return (outExp, outType)
end

function checkOverloadedBinaryScalarArray(
  exp1::Expression,
  type1::M_Type,
  var1::VariabilityType,
  op::Operator,
  exp2::Expression,
  type2::M_Type,
  var2::VariabilityType,
  candidates::List{<:M_Function},
  info::SourceInfo,
)::Tuple{Expression, M_Type}
  local outType::M_Type
  local outExp::Expression

  @assign (outExp, outType) = checkOverloadedBinaryScalarArray2(
    exp1,
    type1,
    var1,
    op,
    P_ExpandExp.ExpandExp.expand(exp2),
    type2,
    var2,
    candidates,
    info,
  )
  return (outExp, outType)
end

function checkOverloadedBinaryScalarArray2(
  exp1::Expression,
  type1::M_Type,
  var1::VariabilityType,
  op::Operator,
  exp2::Expression,
  type2::M_Type,
  var2::VariabilityType,
  candidates::List{<:M_Function},
  info::SourceInfo,
)::Tuple{Expression, M_Type}
  local outType::M_Type
  local outExp::Expression

  local expl::List{Expression}
  local ty::M_Type

  @assign (outExp, outType) = begin
    @match exp2 begin
      ARRAY_EXPRESSION(elements = nil()) => begin
        try
          @assign ty = Type.unliftArray(type2)
          @assign (_, outType) = matchOverloadedBinaryOperator(
            exp1,
            type1,
            var1,
            op,
            EMPTY(type2),
            ty,
            var2,
            candidates,
            info,
            showErrors = false,
          )
        catch
          printUnresolvableTypeError(
            BINARY_EXPRESSION(exp1, op, exp2),
            list(type1, exp2.ty),
            info,
          )
        end
        @assign outType = setArrayElementType(exp2.ty, outType)
        (makeArray(outType, nil), outType)
      end

      ARRAY_EXPRESSION(elements = expl) => begin
        @assign ty = Type.unliftArray(type2)
        @assign expl = list(
          checkOverloadedBinaryScalarArray2(
            exp1,
            type1,
            var1,
            op,
            e,
            ty,
            var2,
            candidates,
            info,
          ) for e in expl
        )
        @assign outType = setArrayElementType(
          exp2.ty,
          typeOf(listHead(expl)),
        )
        (makeArray(outType, expl), outType)
      end

      _ => begin
        matchOverloadedBinaryOperator(
          exp1,
          type1,
          var1,
          op,
          exp2,
          type2,
          var2,
          candidates,
          info,
        )
      end
    end
  end
  return (outExp, outType)
end

function checkOverloadedBinaryArrayScalar(
  exp1::Expression,
  type1::M_Type,
  var1::VariabilityType,
  op::Operator,
  exp2::Expression,
  type2::M_Type,
  var2::VariabilityType,
  candidates::List{<:M_Function},
  info::SourceInfo,
)::Tuple{Expression, M_Type}
  local outType::M_Type
  local outExp::Expression

  @assign (outExp, outType) = checkOverloadedBinaryArrayScalar2(
    P_ExpandExp.ExpandExp.expand(exp1),
    type1,
    var1,
    op,
    exp2,
    type2,
    var2,
    candidates,
    info,
  )
  return (outExp, outType)
end

function checkOverloadedBinaryArrayScalar2(
  exp1::Expression,
  type1::M_Type,
  var1::VariabilityType,
  op::Operator,
  exp2::Expression,
  type2::M_Type,
  var2::VariabilityType,
  candidates::List{<:M_Function},
  info::SourceInfo,
)::Tuple{Expression, M_Type}
  local outType::M_Type
  local outExp::Expression

  local e1::Expression
  local expl::List{Expression}
  local ty::M_Type

  @assign (outExp, outType) = begin
    @match exp1 begin
      ARRAY_EXPRESSION(elements = nil()) => begin
        try
          @assign ty = Type.unliftArray(type1)
          @assign (_, outType) = matchOverloadedBinaryOperator(
            EMPTY(type1),
            ty,
            var1,
            op,
            exp2,
            type2,
            var2,
            candidates,
            info,
            showErrors = false,
          )
        catch
          printUnresolvableTypeError(
            BINARY_EXPRESSION(exp1, op, exp2),
            list(type1, exp1.ty),
            info,
          )
        end
        @assign outType = setArrayElementType(exp1.ty, outType)
        (makeArray(outType, nil), outType)
      end

      ARRAY_EXPRESSION(elements = expl) => begin
        @assign ty = Type.unliftArray(type1)
        @assign expl = list(
          checkOverloadedBinaryArrayScalar2(
            e,
            ty,
            var1,
            op,
            exp2,
            type2,
            var2,
            candidates,
            info,
          ) for e in expl
        )
        @assign outType = setArrayElementType(
          exp1.ty,
          typeOf(listHead(expl)),
        )
        (makeArray(outType, expl), outType)
      end

      _ => begin
        matchOverloadedBinaryOperator(
          exp1,
          type1,
          var1,
          op,
          exp2,
          type2,
          var2,
          candidates,
          info,
        )
      end
    end
  end
  return (outExp, outType)
end

function checkOverloadedBinaryArrayDiv(
  exp1::Expression,
  type1::M_Type,
  var1::VariabilityType,
  op::Operator,
  exp2::Expression,
  type2::M_Type,
  var2::VariabilityType,
  candidates::List{<:M_Function},
  info::SourceInfo,
)::Tuple{Expression, M_Type}
  local outType::M_Type
  local outExp::Expression

  if isArray(type1) && Type.isScalar(type2)
    @assign (outExp, outType) = checkOverloadedBinaryArrayScalar(
      exp1,
      type1,
      var1,
      op,
      exp2,
      type2,
      var2,
      candidates,
      info,
    )
  else
    printUnresolvableTypeError(
      BINARY_EXPRESSION(exp1, op, exp2),
      list(type1, type2),
      info,
    )
  end
  return (outExp, outType)
end

function checkOverloadedBinaryArrayEW(
  exp1::Expression,
  type1::M_Type,
  var1::VariabilityType,
  op::Operator,
  exp2::Expression,
  type2::M_Type,
  var2::VariabilityType,
  candidates::List{<:M_Function},
  info::SourceInfo,
)::Tuple{Expression, M_Type}
  local outType::M_Type
  local outExp::Expression

  local e1::Expression
  local e2::Expression
  local mk::MatchKindType
  local expl1::List{Expression}
  local expl2::List{Expression}
  local ty::M_Type

  if isArray(type1) && isArray(type2)
    @assign (e1, e2, _, mk) = matchExpressions(exp1, type1, exp2, type2, true)
  else
    @assign (e1, e2, _, mk) = matchExpressions(
      exp1,
      arrayElementType(type1),
      exp2,
      arrayElementType(type2),
      true,
    )
  end
  if !isCompatibleMatch(mk)
    printUnresolvableTypeError(
      BINARY_EXPRESSION(e1, op, e2),
      list(type1, type2),
      info,
    )
  end
  @assign e1 = P_ExpandExp.ExpandExp.expand(exp1)
  @assign e2 = P_ExpandExp.ExpandExp.expand(exp2)
  @assign (outExp, outType) =
    checkOverloadedBinaryArrayEW2(e1, type1, var1, op, e2, type2, var2, candidates, info)
  return (outExp, outType)
end

function checkOverloadedBinaryArrayEW2(
  exp1::Expression,
  type1::M_Type,
  var1::VariabilityType,
  op::Operator,
  exp2::Expression,
  type2::M_Type,
  var2::VariabilityType,
  candidates::List{<:M_Function},
  info::SourceInfo,
)::Tuple{Expression, M_Type}
  local outType::M_Type
  local outExp::Expression

  local e2::Expression
  local expl::List{Expression}
  local expl1::List{Expression}
  local expl2::List{Expression}
  local ty::M_Type
  local ty1::M_Type
  local ty2::M_Type
  local is_array1::Bool
  local is_array2::Bool

  @assign is_array1 = isArray(type1)
  @assign is_array2 = isArray(type2)
  if is_array1 || is_array2
    @assign expl = nil
    if isEmptyArray(exp1) ||
       isEmptyArray(exp2)
      @assign ty1 = arrayElementType(type1)
      @assign ty2 = arrayElementType(type2)
      try
        @assign (_, ty) = matchOverloadedBinaryOperator(
          EMPTY(ty1),
          ty1,
          var1,
          op,
          EMPTY(ty2),
          ty2,
          var2,
          candidates,
          info,
        )
      catch
        printUnresolvableTypeError(
          BINARY_EXPRESSION(exp1, op, exp2),
          list(type1, type2),
          info,
        )
      end
    elseif is_array1 && is_array2
      @assign ty1 = Type.unliftArray(type1)
      @assign ty2 = Type.unliftArray(type2)
      @assign expl1 = arrayElements(exp1)
      @assign expl2 = arrayElements(exp2)
      for e in expl1
        @match _cons(e2, expl2) = expl2
        @assign (e, ty) =
          checkOverloadedBinaryArrayEW2(e, ty1, var1, op, e2, ty2, var2, candidates, info)
        @assign expl = _cons(e, expl)
      end
    elseif is_array1
      @assign ty1 = Type.unliftArray(type1)
      @assign expl1 = arrayElements(exp1)
      for e in expl1
        @assign (e, ty) = checkOverloadedBinaryArrayEW2(
          e,
          ty1,
          var1,
          op,
          exp2,
          type2,
          var2,
          candidates,
          info,
        )
        @assign expl = _cons(e, expl)
      end
    elseif is_array2
      @assign ty2 = Type.unliftArray(type2)
      @assign expl2 = arrayElements(exp2)
      for e in expl2
        @assign (e, ty) = checkOverloadedBinaryArrayEW2(
          exp1,
          type1,
          var1,
          op,
          e,
          ty2,
          var2,
          candidates,
          info,
        )
        @assign expl = _cons(e, expl)
      end
    end
    @assign outType = setArrayElementType(type1, ty)
    @assign outExp = makeArray(outType, listReverseInPlace(expl))
  else
    @assign (outExp, outType) = matchOverloadedBinaryOperator(
      exp1,
      type1,
      var1,
      op,
      exp2,
      type2,
      var2,
      candidates,
      info,
    )
  end
  return (outExp, outType)
end

function implicitConstructAndMatch(
  candidates::List{<:M_Function},
  inExp1::Expression,
  inType1::M_Type,
  op::Operator,
  inExp2::Expression,
  inType2::M_Type,
  info::SourceInfo,
)::Tuple{Expression, M_Type}
  local outType::M_Type
  local outExp::Expression

  local inputs::List{InstNode}
  local in1::InstNode
  local in2::InstNode
  local scope::InstNode
  local mk1::MatchKindType
  local mk2::MatchKindType
  local fn_ref::ComponentRef
  local operfn::M_Function
  local matchedfuncs::List{Tuple{M_Function, List{Expression}, Variability}} = nil
  local exp1::Expression
  local exp2::Expression
  local ty::M_Type
  local arg1_ty::M_Type
  local arg2_ty::M_Type
  local var::VariabilityType
  local matched::Bool
  local arg1_info::SourceInfo
  local arg2_info::SourceInfo

  @assign exp1 = inExp1
  @assign exp2 = inExp2
  for fn in candidates
    if listLength(fn.inputs) != 2
      continue
    end
    @match _cons(in1, _cons(in2, _)) = fn.inputs
    @assign arg1_ty = getType(in1)
    @assign arg2_ty = getType(in2)
    @assign arg1_info = info(in1)
    @assign arg2_info = info(in2)
    @assign (matchedfuncs, matched) = implicitConstructAndMatch2(
      inExp1,
      inType1,
      inExp2,
      arg1_ty,
      arg1_info,
      arg2_ty,
      arg2_info,
      classScope(in2),
      fn,
      false,
      matchedfuncs,
    )
    if matched
      continue
    end
    @assign (matchedfuncs, matched) = implicitConstructAndMatch2(
      inExp2,
      inType2,
      inExp1,
      arg2_ty,
      arg2_info,
      arg1_ty,
      arg1_info,
      classScope(in1),
      fn,
      true,
      matchedfuncs,
    )
  end
  #=  Try to implicitly construct a matching record from the first argument.
  =#
  #=  Try to implicitly construct a matching record from the second argument.
  =#
  if listLength(matchedfuncs) == 1
    @match _cons((operfn, list(exp1, exp2), var), _) = matchedfuncs
    @assign outType = returnType(operfn)
    @assign outExp = CALL_EXPRESSION(P_Call.makeTypedCall(
      operfn,
      list(exp1, exp2),
      var,
      outType,
    ))
  else
    Error.addSourceMessage(
      Error.AMBIGUOUS_MATCHING_OPERATOR_FUNCTIONS_NFINST,
      list(
        toString(BINARY_EXPRESSION(exp1, op, exp2)),
        P_Function.candidateFuncListString(list(Util.tuple31(fn) for fn in matchedfuncs)),
      ),
      info,
    )
    fail()
  end
  return (outExp, outType)
end

function implicitConstructAndMatch2(
  exp1::Expression,
  type1::M_Type,
  exp2::Expression,
  paramType1::M_Type,
  paramInfo1::SourceInfo,
  paramType2::M_Type,
  paramInfo2::SourceInfo,
  scope::InstNode,
  fn::M_Function,
  reverseArgs::Bool,
  matchedFns::List{<:Tuple{<:M_Function, List{<:Expression}, VariabilityType}},
)::Tuple{List{Tuple{M_Function, List{Expression}, VariabilityType}}, Bool}
  local matched::Bool

  local fn_ref::ComponentRef
  local e1::Expression
  local e2::Expression
  local mk::MatchKindType
  local var::VariabilityType
  local ty::M_Type

  @assign (e1, _, mk) = matchTypes(paramType1, type1, exp1, false)
  #=  We only want overloaded constructors when trying to implicitly construct.
  =#
  #=  Default constructors are not considered.
  =#
  if mk == MatchKind.EXACT
    @assign fn_ref =
      P_Function.instFunction(Absyn.CREF_IDENT("'constructor'", nil), scope, paramInfo2)
    @assign e2 =
      CALL_EXPRESSION(NFCall.UNTYPED_CALL(fn_ref, list(exp2), nil, scope))
    @assign (e2, ty, var) = P_Call.typeCall(e2, 0, paramInfo1)
    @assign (_, _, mk) = matchTypes(paramType2, ty, e2, false)
    if mk == MatchKind.EXACT
      @assign matchedFns = _cons((fn, if reverseArgs
        list(e2, e1)
      else
        list(e1, e2)
      end, var), matchedFns)
      @assign matched = true
    else
      @assign matched = false
    end
  else
    @assign matched = false
  end
  return (matchedFns, matched)
end

function checkBinaryOperationAdd(
  exp1::Expression,
  type1::M_Type,
  exp2::Expression,
  type2::M_Type,
  info::SourceInfo,
)::Tuple{Expression, M_Type}
  local resultType::M_Type
  local binaryExp::Expression

  local e1::Expression
  local e2::Expression
  local mk::MatchKindType
  local valid::Bool

  @assign (e1, e2, resultType, mk) = matchExpressions(exp1, type1, exp2, type2, true)
  @assign valid = isCompatibleMatch(mk)
  @assign valid = begin
    @match arrayElementType(resultType) begin
      TYPE_INTEGER(__) => begin
        valid
      end

      TYPE_REAL(__) => begin
        valid
      end

      TYPE_STRING(__) => begin
        valid
      end

      _ => begin
        false
      end
    end
  end
  @assign binaryExp =
    BINARY_EXPRESSION(e1, makeAdd(resultType), e2)
  if !valid
    printUnresolvableTypeError(binaryExp, list(type1, type2), info)
  end
  return (binaryExp, resultType)
end

function checkBinaryOperationSub(
  exp1::Expression,
  type1::M_Type,
  exp2::Expression,
  type2::M_Type,
  info::SourceInfo,
)::Tuple{Expression, M_Type}
  local resultType::M_Type
  local binaryExp::Expression

  local e1::Expression
  local e2::Expression
  local mk::MatchKindType
  local valid::Bool

  @assign (e1, e2, resultType, mk) = matchExpressions(exp1, type1, exp2, type2, true)
  @assign valid = isCompatibleMatch(mk)
  @assign valid = begin
    @match arrayElementType(resultType) begin
      TYPE_INTEGER(__) => begin
        valid
      end

      TYPE_REAL(__) => begin
        valid
      end

      _ => begin
        false
      end
    end
  end
  @assign binaryExp =
    BINARY_EXPRESSION(e1, makeSub(resultType), e2)
  if !valid
    printUnresolvableTypeError(binaryExp, list(type1, type2), info)
  end
  return (binaryExp, resultType)
end

function checkBinaryOperationMul(
  exp1::Expression,
  type1::M_Type,
  exp2::Expression,
  type2::M_Type,
  info::SourceInfo,
)::Tuple{Expression, NFType}
  local resultType::NFType
  local binaryExp::Expression

  local e1::Expression
  local e2::Expression
  local ty1::M_Type
  local ty2::M_Type
  local dims1::List{Dimension}
  local dims2::List{Dimension}
  local dim11::Dimension
  local dim12::Dimension
  local dim21::Dimension
  local dim22::Dimension
  local mk::MatchKindType
  local op::OpType
  local valid::Bool

  ty1 = arrayElementType(type1)
  ty2 = arrayElementType(type2)
  (e1, e2, resultType, mk) = matchExpressions(exp1, ty1, exp2, ty2, true)
  valid = isCompatibleMatch(mk)
  valid = begin
    @match resultType begin
      TYPE_INTEGER(__) => begin
        valid
      end

      TYPE_REAL(__) => begin
        valid
      end

      _ => begin
        false
      end
    end
  end
  dims1 = arrayDims(type1)
  dims2 = arrayDims(type2)
  (resultType, op) = begin
    @match (dims1, dims2) begin
      (nil(), nil()) => begin
        (resultType, Op.MUL)
      end

      (nil(), _) => begin
        (TYPE_ARRAY(resultType, dims2), Op.MUL_SCALAR_ARRAY)
      end

      (_, nil()) => begin
        (TYPE_ARRAY(resultType, dims1), Op.MUL_ARRAY_SCALAR)
      end

      (dim11 <| nil(), dim21 <| nil()) => begin
        #=  scalar * scalar = scalar
        =#
        #=  scalar * array = array
        =#
        #=  array * scalar = array
        =#
        #=  vector[n] * vector[n] = scalar
        =#
        @assign valid = isEqual(dim11, dim21)
        (resultType, Op.SCALAR_PRODUCT)
      end

      (dim11 <| nil(), dim21 <| dim22 <| nil()) => begin
        #=  vector[n] * matrix[n, m] = vector[m]
        =#
        @assign valid = isEqual(dim11, dim21)
        (TYPE_ARRAY(resultType, list(dim22)), Op.MUL_VECTOR_MATRIX)
      end

      (dim11 <| dim12 <| nil(), dim21 <| nil()) => begin
        #=  matrix[n, m] * vector[m] = vector[n]
        =#
        @assign valid = isEqual(dim12, dim21)
        (TYPE_ARRAY(resultType, list(dim11)), Op.MUL_MATRIX_VECTOR)
      end

      (dim11 <| dim12 <| nil(), dim21 <| dim22 <| nil()) => begin
        #=  matrix[n, m] * matrix[m, p] = vector[n, p]
        =#
        @assign valid = isEqual(dim12, dim21)
        (TYPE_ARRAY(resultType, list(dim11, dim22)), Op.MATRIX_PRODUCT)
      end

      _ => begin
        @assign valid = false
        (resultType, Op.MUL)
      end
    end
  end
  @assign binaryExp =
    BINARY_EXPRESSION(e1, OPERATOR(resultType, op), e2)
  if !valid
    printUnresolvableTypeError(binaryExp, list(type1, type2), info)
  end
  return (binaryExp, resultType)
end

function checkBinaryOperationDiv(
  exp1::Expression,
  type1::NFType,
  exp2::Expression,
  type2::NFType,
  info::SourceInfo,
  isElementWise::Bool,
)::Tuple{Expression, NFType}
  local resultType::NFType
  local binaryExp::Expression
  local e1::Expression
  local e2::Expression
  local ty1::NFType
  local ty2::NFType
  local mk::MatchKindType
  local valid::Bool
  local op::Operator
  @assign (e1, ty1, mk) =
    matchTypes(type1, setArrayElementType(type1, TYPE_REAL()), exp1; allowUnknown = true)
  @assign valid = isCompatibleMatch(mk)
  @assign (e2, ty2, mk) =
    matchTypes(type2, setArrayElementType(type2, TYPE_REAL()), exp2; allowUnknown = true)
  @assign valid = valid && isCompatibleMatch(mk)
  #=  Division is always element-wise, the only difference between / and ./ is
  =#
  #=  which operands they accept.
  =#
  @assign (resultType, op) = begin
    @match (isArray(ty1), isArray(ty2), isElementWise) begin
      (false, false, _) => begin
        (ty1, makeDiv(ty1))
      end

      (_, false, _) => begin
        (ty1, OPERATOR(ty1, Op.DIV_ARRAY_SCALAR))
      end

      (false, _, true) => begin
        (ty2, OPERATOR(ty2, Op.DIV_SCALAR_ARRAY))
      end

      (true, _, true) => begin
        #=  scalar / scalar or scalar ./ scalar
        =#
        #=  array / scalar or array ./ scalar
        =#
        #=  scalar ./ array
        =#
        #=  array ./ array
        =#
        #=  If both operands are arrays, check that their dimensions are compatible.
        =#
        @assign (_, _, mk) = matchArrayTypes(ty1, ty2, e1, true)
        @assign valid = valid && isCompatibleMatch(mk)
        (ty1, makeDiv(ty1))
      end

      _ => begin
        #=  Anything else is an error.
        =#
        @assign valid = false
        (ty1, makeDiv(ty1))
      end
    end
  end
  @assign binaryExp = BINARY_EXPRESSION(e1, op, e2)
  if !valid
    printUnresolvableTypeError(binaryExp, list(type1, type2), info)
  end
  return (binaryExp, resultType)
end

function checkBinaryOperationPow(
  exp1::Expression,
  type1::NFType,
  exp2::Expression,
  type2::NFType,
  info::SourceInfo,
)::Tuple{Expression, NFType}
  local resultType::NFType
  local binaryExp::Expression

  local e1::Expression
  local e2::Expression
  local mk::MatchKindType
  local valid::Bool
  local op::Operator

  #=  The first operand of ^ should be Real.
  =#
  @assign (e1, resultType, mk) =
    matchTypes(type1, setArrayElementType(type1, TYPE_REAL()), exp1; allowUnknown = true)
  @assign valid = isCompatibleMatch(mk)
  if isArray(resultType)
    @assign valid = valid && Type.isSquareMatrix(resultType)
    @assign valid = valid && isInteger(type2)
    @assign op = OPERATOR(resultType, Op.POW_MATRIX)
    @assign e2 = exp2
  else
    @assign (e2, _, mk) = matchTypes(type2, TYPE_REAL(), exp2; allowUnknown = true)
    @assign valid = valid && isCompatibleMatch(mk)
    @assign op = OPERATOR(resultType, Op.POW)
  end
  #=  Real[n, n] ^ Integer
  =#
  #=  Real ^ Real
  =#
  @assign binaryExp = BINARY_EXPRESSION(e1, op, e2)
  if !valid
    printUnresolvableTypeError(binaryExp, list(type1, type2), info)
  end
  return (binaryExp, resultType)
end

function checkBinaryOperationPowEW(
  exp1::Expression,
  type1::NFType,
  exp2::Expression,
  type2::NFType,
  info::SourceInfo,
)::Tuple{Expression, NFType}
  local resultType::NFType
  local binaryExp::Expression

  local e1::Expression
  local e2::Expression
  local ty1::NFType
  local ty2::NFType
  local mk::MatchKindType
  local valid::Bool
  local op::Operator

  #=  Exponentiation always returns a Real value, so instead of checking if the types
  =#
  #=  are compatible with ecah other we check if each type is compatible with Real.
  =#
  @assign (e1, ty1, mk) =
    matchTypes(type1, setArrayElementType(type1, TYPE_REAL()), exp1, allowUnknown = true)
  @assign valid = isCompatibleMatch(mk)
  @assign (e2, ty2, mk) =
    matchTypes(type2, setArrayElementType(type2, TYPE_REAL()), exp2, allowUnknown = true)
  @assign valid = valid && isCompatibleMatch(mk)
  @assign (resultType, op) = begin
    @match (isArray(ty1), isArray(ty2)) begin
      (false, false) => begin
        (ty1, makePow(ty1))
      end

      (_, false) => begin
        (ty1, OPERATOR(ty1, Op.POW_ARRAY_SCALAR))
      end

      (false, _) => begin
        (ty2, OPERATOR(ty2, Op.POW_SCALAR_ARRAY))
      end

      _ => begin
        #=  scalar .^ scalar
        =#
        #=  array .^ scalar
        =#
        #=  scalar .^ array
        =#
        #=  array .^ array
        =#
        #=  If both operands are arrays, check that their dimensions are compatible.
        =#
        @assign (_, _, mk) = matchArrayTypes(ty1, ty2, e1, true)
        @assign valid = valid && isCompatibleMatch(mk)
        (ty1, makePow(ty1))
      end
    end
  end
  @assign binaryExp = BINARY_EXPRESSION(e1, op, e2)
  if !valid
    printUnresolvableTypeError(binaryExp, list(type1, type2), info)
  end
  return (binaryExp, resultType)
end

function checkBinaryOperationEW(
  exp1::Expression,
  type1::NFType,
  exp2::Expression,
  type2::NFType,
  elemOp::OpType,
  info::SourceInfo,
)::Tuple{Expression, NFType}
  local resultType::NFType
  local binaryExp::Expression
  local e1::Expression
  local e2::Expression
  local ty1::NFType
  local ty2::NFType
  local mk::MatchKindType
  local valid::Bool
  local is_arr1::Bool
  local is_arr2::Bool
  local op::Operator
  is_arr1 = isArray(type1)
  is_arr2 = isArray(type2)
  if is_arr1 && is_arr2
    (e1, e2, resultType, mk) = matchExpressions(exp1, type1, exp2, type2, true)
  else
    ty1 = arrayElementType(type1)
    ty2 = arrayElementType(type2)
    (e1, e2, resultType, mk) = matchExpressions(exp1, ty1, exp2, ty2, true)
  end
  #=  The expressions must be type compatible if they are both arrays.
  =#
  #=  Otherwise it's enough if their element types are compatible.
  =#
  @assign valid = isCompatibleMatch(mk)
  #=  Check that the type is valid for the operation.
  =#
  @assign valid = begin
    @match (arrayElementType(resultType), elemOp) begin
      (TYPE_INTEGER(__), _) => begin
        valid
      end

      (TYPE_REAL(__), _) => begin
        valid
      end

      (TYPE_STRING(__), Op.ADD) => begin
        valid
      end

      _ => begin
        false
      end
    end
  end
  (resultType, op) = begin
    @match (is_arr1, is_arr2) begin
      (true, false) => begin
        #=  array * scalar => Op.{elemOp}_ARRAY_SCALAR.
        =#
        resultType = copyDims(type1, resultType)
        op = makeArrayScalar(resultType, elemOp)
        (resultType, op)
      end

      (false, true) => begin
        #=  scalar * array => Op.{elemOp}_SCALAR_ARRAY;
        =#
        resultType = copyDims(type2, resultType)
        op = makeScalarArray(resultType, elemOp)
        (resultType, op)
      end

      _ => begin
        (resultType, OPERATOR(resultType, elemOp))
      end
    end
  end
  #=  scalar * scalar and array * array => elemOp.
  =#
  @assign binaryExp = BINARY_EXPRESSION(e1, op, e2)
  if !valid
    printUnresolvableTypeError(binaryExp, list(type1, type2), info)
  end
  return (binaryExp, resultType)
end

function checkUnaryOperation(
  exp1::Expression,
  type1::NFType,
  var1::VariabilityType,
  operator::Operator,
  info::SourceInfo,
)::Tuple{Expression, NFType}
  local unaryType::NFType
  local unaryExp::Expression

  local valid::Bool = true
  local op::Operator

  if isComplex(arrayElementType(type1))
    @assign (unaryExp, unaryType) =
      checkOverloadedUnaryOperator(exp1, type1, var1, operator, info)
    return (unaryExp, unaryType)
  end
  @assign unaryType = type1
  @assign op = setType(unaryType, operator)
  @assign unaryExp = begin
    @match operator.op begin
      Op.ADD => begin
        exp1
      end

      _ => begin
        UNARY_EXPRESSION(op, exp1)
      end
    end
  end
  #=  + is a no-op for arithmetic unary operations.
  =#
  if !isNumeric(type1)
    printUnresolvableTypeError(unaryExp, list(type1), info)
  end
  return (unaryExp, unaryType)
end

function checkOverloadedUnaryOperator(
  inExp1::Expression,
  inType1::NFType,
  var::VariabilityType,
  inOp::Operator,
  info::SourceInfo,
)::Tuple{Expression, NFType}
  local outType::NFType
  local outExp::Expression

  local opstr::String
  local operfn::M_Function
  local node1::InstNode
  local fn_node::InstNode
  local fn_ref::ComponentRef
  local candidates::List{M_Function}
  local matched::Bool
  local args::List{TypedArg}
  local matchKind::FunctionMatchKind
  local matchedFunc::MatchedFunction
  local matchedFunctions::List{MatchedFunction} = nil
  local exactMatches::List{MatchedFunction}

  @assign opstr = symbol(inOp, "'")
  @assign candidates = OperatorOverloading.lookupOperatorFunctionsInType(opstr, inType1)
  #= for fn in candidates loop
  =#
  #=   checkValidOperatorOverload(opstr, fn, node1);
  =#
  #= end for;
  =#
  @assign args = list((inExp1, inType1, var))
  @assign matchedFunctions =
    matchFunctionsSilent(candidates, args, nil, info, vectorize = false)
  #=  We only allow exact matches for operator overloading. e.g. no casting or generic matches.
  =#
  @assign exactMatches = getExactMatches(matchedFunctions)
  if listEmpty(exactMatches)
    printUnresolvableTypeError(
      UNARY_EXPRESSION(inOp, inExp1),
      list(inType1),
      info,
    )
    fail()
  end
  if listLength(exactMatches) == 1
    @match _cons(matchedFunc, _) = exactMatches
    @assign outType = returnType(matchedFunc.func)
    @assign outExp = CALL_EXPRESSION(P_Call.makeTypedCall(
      matchedFunc.func,
      list(Util.tuple31(a) for a in matchedFunc.args),
      var,
      outType,
    ))
  else
    Error.addSourceMessage(
      Error.AMBIGUOUS_MATCHING_OPERATOR_FUNCTIONS_NFINST,
      list(
        toString(UNARY_EXPRESSION(inOp, inExp1)),
        P_Function.candidateFuncListString(list(mfn.func for mfn in matchedFunctions)),
      ),
      info,
    )
    fail()
  end
  return (outExp, outType)
end

function checkLogicalBinaryOperation(
  exp1::Expression,
  type1::NFType,
  var1::VariabilityType,
  operator::Operator,
  exp2::Expression,
  type2::NFType,
  var2::VariabilityType,
  info::SourceInfo,
)::Tuple{Expression, NFType}
  local resultType::NFType
  local outExp::Expression

  local e1::Expression
  local e2::Expression
  local mk::MatchKindType

  if isComplex(arrayElementType(type1)) ||
     isComplex(arrayElementType(type2))
    @assign (outExp, resultType) =
      checkOverloadedBinaryOperator(exp1, type1, var1, operator, exp2, type2, var2, info)
    return (outExp, resultType)
  end
  @assign (e1, e2, resultType, mk) = matchExpressions(exp1, type1, exp2, type2, true)
  @assign outExp = LBINARY_EXPRESSION(
    e1,
    setType(resultType, operator),
    e2,
  )
  if !isCompatibleMatch(mk) || !isBoolean(arrayElementType(resultType))
    printUnresolvableTypeError(outExp, list(type1, type2), info)
  end
  return (outExp, resultType)
end

function checkLogicalUnaryOperation(
  exp1::Expression,
  type1::NFType,
  var1::VariabilityType,
  operator::Operator,
  info::SourceInfo,
)::Tuple{Expression, NFType}
  local resultType::NFType = type1
  local outExp::Expression

  local e1::Expression
  local e2::Expression
  local mk::MatchKindType

  if isComplex(arrayElementType(type1))
    @assign (outExp, resultType) =
      checkOverloadedUnaryOperator(exp1, type1, var1, operator, info)
    return (outExp, resultType)
  end
  @assign outExp =
    LUNARY_EXPRESSION(setType(type1, operator), exp1)
  if !isBoolean(arrayElementType(type1))
    printUnresolvableTypeError(outExp, list(type1), info)
  end
  return (outExp, resultType)
end

function checkRelationOperation(
  exp1::Expression,
  type1::NFType,
  var1::VariabilityType,
  operator::Operator,
  exp2::Expression,
  type2::NFType,
  var2::VariabilityType,
  origin::ORIGIN_Type,
  info::SourceInfo,
)::Tuple{Expression, NFType}
  local resultType::NFType
  local outExp::Expression

  local e1::Expression
  local e2::Expression
  local ty::NFType
  local mk::MatchKindType
  local valid::Bool
  local o::OpType

  if isComplex(arrayElementType(type1)) ||
     isComplex(arrayElementType(type2))
    @assign (outExp, resultType) =
      checkOverloadedBinaryOperator(exp1, type1, var1, operator, exp2, type2, var2, info)
    return (outExp, resultType)
  end
  @assign (e1, e2, ty, mk) = matchExpressions(exp1, type1, exp2, type2)
  @assign valid = isCompatibleMatch(mk)
  @assign resultType = TYPE_BOOLEAN()
  @assign outExp =
    RELATION_EXPRESSION(e1, setType(ty, operator), e2)
  @assign valid = begin
    @match ty begin
      TYPE_INTEGER(__) => begin
        valid
      end

      TYPE_REAL(__) => begin
        #=  Print a warning for == or <> with Real operands in a model. =#
        @assign o = operator.op
        if flagNotSet(origin, ORIGIN_FUNCTION) &&
           (o == Op.EQUAL || o == Op.NEQUAL)
          Error.addStrictMessage(
            Error.WARNING_RELATION_ON_REAL,
            list(
              toString(outExp),
              symbol(operator, ""),
            ),
            info,
          )
        end
        valid
      end

      TYPE_STRING(__) => begin
        valid
      end

      TYPE_BOOLEAN(__) => begin
        valid
      end

      TYPE_ENUMERATION(__) => begin
        valid
      end

      _ => begin
        false
      end
    end
  end
  if !valid
    printUnresolvableTypeError(outExp, list(type1, type2), info)
  end
  return (outExp, resultType)
end

function printUnresolvableTypeError(
  exp::Expression,
  types::List{<:NFType},
  info::SourceInfo,
  printError::Bool = true,
)
  local exp_str::String
  local ty_str::String
  if printError
    exp_str = toString(exp)
    ty_str = ListUtil.toString(types, toString, "", "", ", ", "", false)
    # Error.addSourceMessage(
    #   Error.UNRESOLVABLE_TYPE,
    #   list(exp_str, ty_str, "<NO_COMPONENT>"),
    #   info,
    # )
  end
  @error "Unresolvable type for: " * exp_str * "\n Type was: " * ty_str
  return fail()
end


function matchExpressions(
  @nospecialize(exp1::Expression),
  @nospecialize(type1::NFType),
  @nospecialize(exp2::Expression),
  @nospecialize(type2::NFType),
  allowUnknown::Bool = false,
)::Tuple{Expression, Expression, NFType, MatchKindType}
  local matchKind::MatchKindType
  local compatibleType::NFType

  #=  Return true if the references are the same.
  =#
  if referenceEq(type1, type2)
    @assign compatibleType = type1
    @assign matchKind = MatchKind.EXACT
    return (exp1, exp2, compatibleType, matchKind)
  end
  #=  Check if the types are different kinds of types.
  =#
  if valueConstructor(type1) != valueConstructor(type2)
    @assign (exp1, exp2, compatibleType, matchKind) =
      matchExpressions_cast(exp1, type1, exp2, type2, allowUnknown)
    return (exp1, exp2, compatibleType, matchKind)
  end
  #=  If the types are not of the same kind we might need to type cast one of
  =#
  #=  the expressions to make them compatible.
  =#
  #=  The types are of the same kind, so we only need to match on one of them.
  =#
  @assign matchKind = MatchKind.EXACT
  @assign compatibleType = begin
    @match type1 begin
      TYPE_INTEGER(__) => begin
        type1
      end

      TYPE_REAL(__) => begin
        type1
      end

      TYPE_STRING(__) => begin
        type1
      end

      TYPE_BOOLEAN(__) => begin
        type1
      end

      TYPE_CLOCK(__) => begin
        type1
      end

      TYPE_ENUMERATION(__) => begin
        @assign matchKind = matchEnumerationTypes(type1, type2)
        type1
      end

      TYPE_ENUMERATION_ANY(__) => begin
        type1
      end

      TYPE_ARRAY(__) => begin
        @assign (exp1, exp2, compatibleType, matchKind) =
          matchArrayExpressions(exp1, type1, exp2, type2, allowUnknown)
        compatibleType
      end

      TYPE_TUPLE(__) => begin
        @assign (exp2, compatibleType, matchKind) =
          matchTupleTypes(type2, type1, exp2, allowUnknown)
        compatibleType
      end

      TYPE_UNKNOWN(__) => begin
        @assign matchKind = if allowUnknown
          MatchKind.EXACT
        else
          MatchKind.NOT_COMPATIBLE
        end
        type1
      end

      TYPE_COMPLEX(__) => begin
        #=  TODO: This needs more work to handle e.g. type casting of complex expressions.
        =#
        @assign (exp1, compatibleType, matchKind) =
          matchComplexTypes(type1, type2, exp1, allowUnknown)
        compatibleType
      end

      TYPE_METABOXED(__) => begin
        @assign (exp1, exp2, compatibleType, matchKind) =
          matchBoxedExpressions(exp1, type1, exp2, type2, allowUnknown)
        compatibleType
      end

      _ => begin
        Error.assertion(false, getInstanceName() + " got unknown type.", sourceInfo())
        fail()
      end
    end
  end
  return (exp1, exp2, compatibleType, matchKind)
end

function matchTypes(
  actualType::NFType,
  expectedType::NFType,
  expression::Expression;
  allowUnknown::Bool = false,
)::Tuple{Expression, NFType, MatchKindType}
  local matchKind::MatchKindType
  local compatibleType::NFType
  #=  Return true if the references are the same. =#
  if referenceEq(actualType, expectedType)
    @assign compatibleType = actualType
    @assign matchKind = MatchKind.EXACT
    return (expression, compatibleType, matchKind)
  end
  #=  Check if the types are different kinds of types. =#
  if valueConstructor(actualType) != valueConstructor(expectedType)
    (expression, compatibleType, matchKind) =
      matchTypes_cast(actualType, expectedType, expression, allowUnknown)
    return (expression, compatibleType, matchKind)
  end
  #=  If the types are not of the same kind we might need to type cast the
  =#
  #=  expression to make it compatible.
  =#
  #=  The types are of the same kind, so we only need to match on one of them.
  =#
  @assign matchKind = MatchKind.EXACT
  @assign compatibleType = begin
    @match actualType begin
      TYPE_INTEGER(__) => begin
        actualType
      end

      TYPE_REAL(__) => begin
        actualType
      end

      TYPE_STRING(__) => begin
        actualType
      end

      TYPE_BOOLEAN(__) => begin
        actualType
      end

      TYPE_CLOCK(__) => begin
        actualType
      end

      TYPE_ENUMERATION(__) => begin
        @assign matchKind = matchEnumerationTypes(actualType, expectedType)
        actualType
      end

      TYPE_ENUMERATION_ANY(__) => begin
        actualType
      end

      TYPE_ARRAY(__) => begin
        @assign (expression, compatibleType, matchKind) =
          matchArrayTypes(actualType, expectedType, expression, allowUnknown)
        compatibleType
      end

      TYPE_TUPLE(__) => begin
        @assign (expression, compatibleType, matchKind) =
          matchTupleTypes(actualType, expectedType, expression, allowUnknown)
        compatibleType
      end

      TYPE_UNKNOWN(__) => begin
        @assign matchKind = if allowUnknown
          MatchKind.EXACT
        else
          MatchKind.NOT_COMPATIBLE
        end
        actualType
      end

      TYPE_COMPLEX(__) => begin
        @assign (expression, compatibleType, matchKind) =
          matchComplexTypes(actualType, expectedType, expression, allowUnknown)
        compatibleType
      end

      TYPE_FUNCTION(__) => begin
        @assign (expression, compatibleType, matchKind) =
          matchFunctionTypes(actualType, expectedType, expression, allowUnknown)
        compatibleType
      end

      TYPE_METABOXED(__) => begin
        @assign (expression, compatibleType, matchKind) = matchTypes(
          actualType.ty,
          Type.unbox(expectedType),
          unbox(expression),
          allowUnknown,
        )
        @assign expression = box(expression)
        @assign compatibleType = Type.box(compatibleType)
        compatibleType
      end

      _ => begin
        #Error.assertion(false, getInstanceName() + " got unknown type.", sourceInfo())
        @error "Got unknown type"
        fail()
      end
    end
  end
  return (expression, compatibleType, matchKind)
end

function matchExpressions_cast(
  exp1::Expression,
  type1::NFType,
  exp2::Expression,
  type2::NFType,
  allowUnknown::Bool,
)::Tuple{Expression, Expression, NFType, MatchKindType}
  local matchKind::MatchKindType
  local compatibleType::NFType

  @assign (compatibleType, matchKind) = begin
    @match (type1, type2) begin
      (TYPE_INTEGER(__), TYPE_REAL(__)) => begin
        #=  Integer can be cast to Real.
        =#
        @assign exp1 = typeCast(exp1, type2)
        (type2, MatchKind.CAST)
      end

      (TYPE_REAL(__), TYPE_INTEGER(__)) => begin
        @assign exp2 = typeCast(exp2, type1)
        (type1, MatchKind.CAST)
      end

      (TYPE_BOOLEAN(__), TYPE_REAL(__)) where {(Flags.isSet(Flags.NF_API))} => begin
        #=  Boolean can be cast to Real (only if -d=nfAPI is on)
        =#
        #=  as there are annotations having expressions such as Boolean x > 0.5
        =#
        Error.addCompilerWarning(
          "Allowing casting of boolean expression: " +
          toString(exp1) +
          " to Real.",
        )
        @assign exp1 = typeCast(exp1, type2)
        (type2, MatchKind.CAST)
      end

      (TYPE_REAL(__), TYPE_BOOLEAN(__)) where {(Flags.isSet(Flags.NF_API))} => begin
        Error.addCompilerWarning(
          "Allowing casting of boolean expression: " +
          toString(exp2) +
          " to Real.",
        )
        @assign exp2 = typeCast(exp2, type1)
        (type1, MatchKind.CAST)
      end

      (_, TYPE_TUPLE(types = compatibleType <| _)) => begin
        #=  This case takes care of equations where the lhs is a non-tuple and the rhs a
        =#
        #=  function call returning a tuple, in which case only the first element of the
        =#
        #=  tuple is used. exp1 should never be a tuple here, since any tuple expression
        =#
        #=  not alone on the rhs of an equation is \"tuple subscripted\" by typeExp.
        =#
        exp2 = tupleElement(exp2, compatibleType, 1)
        (exp2, compatibleType, matchKind) =
          matchTypes(compatibleType, type1, exp2, allowUnknown = allowUnknown)
        if isCompatibleMatch(matchKind)
          @assign matchKind = MatchKind.CAST
        end
        (compatibleType, matchKind)
      end

      (TYPE_UNKNOWN(__), _) => begin
        (type2, if allowUnknown
          MatchKind.EXACT
        else
          MatchKind.NOT_COMPATIBLE
        end)
      end

      (_, TYPE_UNKNOWN(__)) => begin
        (type1, if allowUnknown
          MatchKind.EXACT
        else
          MatchKind.NOT_COMPATIBLE
        end)
      end

      (TYPE_METABOXED(__), _) => begin
        @assign (exp1, exp2, compatibleType, matchKind) = matchExpressions(
          unbox(exp1),
          type1.ty,
          exp2,
          type2,
          allowUnknown,
        )
        (compatibleType, matchKind)
      end

      (_, TYPE_METABOXED(__)) => begin
        @assign (exp1, exp2, compatibleType, matchKind) = matchExpressions(
          exp1,
          type1,
          unbox(exp2),
          type2.ty,
          allowUnknown,
        )
        (compatibleType, matchKind)
      end

      (_, TYPE_POLYMORPHIC(__)) => begin
        @assign exp1 = box(exp1)
        (Type.box(type1), MatchKind.GENERIC)
      end

      (TYPE_POLYMORPHIC(__), _) => begin
        @assign exp2 = box(exp2)
        (Type.box(type2), MatchKind.GENERIC)
      end

      _ => begin
        (TYPE_UNKNOWN(), MatchKind.NOT_COMPATIBLE)
      end
    end
  end
  return (exp1, exp2, compatibleType, matchKind)
end

function matchComplexTypes(
  actualType::NFType,
  expectedType::NFType,
  expression::Expression,
  allowUnknown::Bool,
)::Tuple{Expression, NFType, MatchKindType}
  local matchKind::MatchKindType = MatchKind.NOT_COMPATIBLE
  local compatibleType::NFType = actualType

  local cls1::Class
  local cls2::Class
  local anode::InstNode
  local enode::InstNode
  local comps1::Vector{InstNode}
  local comps2::Vector{InstNode}
  local path::Absyn.Path
  local ty::NFType
  local cty1::ComplexType
  local cty2::ComplexType
  local e::Expression
  local elements::List{Expression}
  local matched_elements::List{Expression} = nil
  local mk::MatchKindType
  local comp1::Component
  local comp2::Component

  @match TYPE_COMPLEX(cls = anode) = actualType
  @match TYPE_COMPLEX(cls = enode) = expectedType
  #=  TODO: revise this.
  =#
  if isSame(anode, enode)
    @assign matchKind = MatchKind.EXACT
    return (expression, compatibleType, matchKind)
  end
  @assign cls1 = getClass(anode)
  @assign cls2 = getClass(enode)
  @assign () = begin
    @match (cls1, cls2, expression) begin
      (
        INSTANCED_CLASS(elements = CLASS_TREE_FLAT_TREE(components = comps1)),
        INSTANCED_CLASS(elements = CLASS_TREE_FLAT_TREE(components = comps2)),
        RECORD_EXPRESSION(elements = elements),
      ) => begin
        @assign matchKind = MatchKind.PLUG_COMPATIBLE
        if arrayLength(comps1) != arrayLength(comps2) ||
           arrayLength(comps1) != listLength(elements)
          @assign matchKind = MatchKind.NOT_COMPATIBLE
        else
          for i = 1:arrayLength(comps1)
            @match _cons(e, elements) = elements
            @assign (e, _, mk) = matchTypes(
              getType(comps1[i]),
              getType(comps2[i]),
              e,
              allowUnknown,
            )
            @assign matched_elements = _cons(e, matched_elements)
            if mk == MatchKind.CAST
              @assign matchKind = mk
            elseif !isValidPlugCompatibleMatch(mk)
              @assign matchKind = MatchKind.NOT_COMPATIBLE
              break
            end
          end
          if matchKind == MatchKind.CAST
            @assign expression.elements = listReverse(matched_elements)
          end
        end
        if matchKind != MatchKind.NOT_COMPATIBLE
          @assign matchKind = MatchKind.PLUG_COMPATIBLE
        end
        ()
      end

      (
        INSTANCED_CLASS(
          ty = TYPE_COMPLEX(complexTy = cty1 && COMPLEX_CONNECTOR(__)),
        ),
        INSTANCED_CLASS(
          ty = TYPE_COMPLEX(complexTy = cty2 && COMPLEX_CONNECTOR(__)),
        ),
        _,
      ) => begin
        @assign matchKind =
          matchComponentList(cty1.potentials, cty2.potentials, allowUnknown)
        if matchKind != MatchKind.NOT_COMPATIBLE
          @assign matchKind = matchComponentList(cty1.flows, cty2.flows, allowUnknown)
          if matchKind != MatchKind.NOT_COMPATIBLE
            @assign matchKind = matchComponentList(cty1.streams, cty2.streams, allowUnknown)
          end
        end
        if matchKind != MatchKind.NOT_COMPATIBLE
          @assign matchKind = MatchKind.PLUG_COMPATIBLE
        end
        ()
      end

      (
        INSTANCED_CLASS(elements = CLASS_TREE_FLAT_TREE(components = comps1)),
        INSTANCED_CLASS(elements = CLASS_TREE_FLAT_TREE(components = comps2)),
        _,
      ) => begin
        @assign matchKind = MatchKind.PLUG_COMPATIBLE
        if arrayLength(comps1) != arrayLength(comps2)
          @assign matchKind = MatchKind.NOT_COMPATIBLE
        else
          for i = 1:arrayLength(comps1)
            @assign comp1 = component(comps1[i])
            @assign comp2 = component(comps2[i])
            if P_Component.isTyped(comp2)
              @assign (_, _, mk) = matchTypes(
                getType(comp1),
                getType(comp2),
                expression,
                allowUnknown,
              )
              if !isValidPlugCompatibleMatch(mk)
                @assign matchKind = MatchKind.NOT_COMPATIBLE
                break
              end
            end
          end
        end
        ()
      end

      _ => begin
        @assign matchKind = MatchKind.NOT_COMPATIBLE
        ()
      end
    end
  end
  return (expression, compatibleType, matchKind)
end

function matchComponentList(
  comps1::List{<:InstNode},
  comps2::List{<:InstNode},
  allowUnknown::Bool,
)::MatchKindType
  local matchKind::MatchKindType

  local c2::InstNode
  local rest_c2::List{InstNode} = comps2
  local dummy::Expression = INTEGER_EXPRESSION(0)

  if listLength(comps1) != listLength(comps2)
    @assign matchKind = MatchKind.NOT_COMPATIBLE
  else
    for c1 in comps1
      @match _cons(c2, rest_c2) = rest_c2
      if name(c1) != name(c2)
        @assign matchKind = MatchKind.NOT_COMPATIBLE
        return matchKind
      end
      @assign (_, _, matchKind) =
        matchTypes(getType(c1), getType(c2), dummy; allowUnknown = allowUnknown)
      if matchKind == MatchKind.NOT_COMPATIBLE
        return matchKind
      end
    end
  end
  @assign matchKind = MatchKind.PLUG_COMPATIBLE
  return matchKind
end

function matchFunctionTypes(
  actualType::NFType,
  expectedType::NFType,
  expression::Expression,
  allowUnknown::Bool,
)::Tuple{Expression, NFType, MatchKindType}
  local matchKind::MatchKindType = MatchKind.EXACT
  local compatibleType::NFType = actualType
  local inputs1::List{InstNode}
  local inputs2::List{InstNode}
  local remaining_inputs::List{InstNode}
  local outputs1::List{InstNode}
  local outputs2::List{InstNode}
  local slots1::List{Slot}
  local slots2::List{Slot}
  local input2::InstNode
  local output2::InstNode
  local slot1::Slot
  local slot2::Slot
  local matching::Bool

  @match TYPE_FUNCTION(
    fn = P_Function.FUNCTION(inputs = inputs1, outputs = outputs1, slots = slots1),
  ) = actualType
  @match TYPE_FUNCTION(
    fn = P_Function.FUNCTION(inputs = inputs2, outputs = outputs2, slots = slots2),
  ) = expectedType
  #=  The functions must have the same number of outputs.
  =#
  if listLength(outputs1) != listLength(outputs2)
    @assign matchKind = MatchKind.NOT_COMPATIBLE
    return (expression, compatibleType, matchKind)
  end
  if !matchFunctionParameters(outputs1, outputs2, allowUnknown)
    @assign matchKind = MatchKind.NOT_COMPATIBLE
    return (expression, compatibleType, matchKind)
  end
  if !matchFunctionParameters(inputs1, inputs2, allowUnknown)
    @assign matchKind = MatchKind.NOT_COMPATIBLE
    return (expression, compatibleType, matchKind)
  end
  #=  An input in the actual type must have a default argument if the
  =#
  #=  corresponding input in the expected type has one.
  =#
  for i in inputs2
    @match _cons(slot1, slots1) = slots1
    @match _cons(slot2, slots2) = slots2
    if isSome(slot2.default) && !isSome(slot1.default)
      @assign matchKind = MatchKind.NOT_COMPATIBLE
      return (expression, compatibleType, matchKind)
    end
  end
  #=  The actual type can have more inputs than expected if the extra inputs have
  =#
  #=  default arguments.
  =#
  for slot in slots1
    if !isSome(slot.default)
      @assign matchKind = MatchKind.NOT_COMPATIBLE
      return (expression, compatibleType, matchKind)
    end
  end
  return (expression, compatibleType, matchKind)
end

function matchFunctionParameters(
  params1::List{<:InstNode},
  params2::List{<:InstNode},
  allowUnknown::Bool,
)::Bool
  local matching::Bool = true

  local pl1::List{InstNode} = params1
  local pl2::List{InstNode} = params2
  local p1::InstNode
  local dummy::Expression = INTEGER_EXPRESSION(0)
  local mk::MatchKindType

  for p2 in pl2
    if listEmpty(pl1)
      @assign matching = false
      break
    end
    @match _cons(p1, pl1) = pl1
    if name(p1) != name(p2)
      @assign matching = false
      break
    end
    @assign (_, _, mk) = matchTypes(
      Type.unbox(getType(p1)),
      Type.unbox(getType(p2)),
      dummy,
      allowUnknown,
    )
    if mk != MatchKind.EXACT
      @assign matching = false
      break
    end
  end
  return matching
end

function matchEnumerationTypes(type1::NFType, type2::NFType)::MatchKindType
  local matchKind::MatchKindType

  local lits1::List{String}
  local lits2::List{String}

  @match TYPE_ENUMERATION(literals = lits1) = type1
  @match TYPE_ENUMERATION(literals = lits2) = type2
  @assign matchKind = if ListUtil.isEqualOnTrue(lits1, lits2, stringEqual)
    MatchKind.EXACT
  else
    MatchKind.NOT_COMPATIBLE
  end
  return matchKind
end

function matchArrayExpressions(
  exp1::Expression,
  type1::NFType,
  exp2::Expression,
  type2::NFType,
  allowUnknown::Bool,
)::Tuple{Expression, Expression, NFType, MatchKindType}
  local matchKind::MatchKindType
  local compatibleType::NFType

  local ety1::NFType
  local ety2::NFType
  local dims1::List{Dimension}
  local dims2::List{Dimension}

  @match TYPE_ARRAY(elementType = ety1, dimensions = dims1) = type1
  @match TYPE_ARRAY(elementType = ety2, dimensions = dims2) = type2
  #=  Check that the element types are compatible.
  =#
  @assign (exp1, exp2, compatibleType, matchKind) =
    matchExpressions(exp1, ety1, exp2, ety2, allowUnknown)
  #=  If the element types are compatible, check the dimensions too.
  =#
  @assign (compatibleType, matchKind) =
    matchArrayDims(dims1, dims2, compatibleType, matchKind, allowUnknown)
  return (exp1, exp2, compatibleType, matchKind)
end

function matchArrayTypes(
  arrayType1::NFType,
  arrayType2::NFType,
  expression::Expression,
  allowUnknown::Bool,
)::Tuple{Expression, NFType, MatchKindType}
  local matchKind::MatchKindType
  local compatibleType::NFType

  local ety1::NFType
  local ety2::NFType
  local dims1::List{Dimension}
  local dims2::List{Dimension}

  @match TYPE_ARRAY(elementType = ety1, dimensions = dims1) = arrayType1
  @match TYPE_ARRAY(elementType = ety2, dimensions = dims2) = arrayType2
  #=  Check that the element types are compatible.
  =#
  @assign (expression, compatibleType, matchKind) =
    matchTypes(ety1, ety2, expression; allowUnknown = allowUnknown)
  #=  If the element types are compatible, check the dimensions too.
  =#
  @assign (compatibleType, matchKind) =
    matchArrayDims(dims1, dims2, compatibleType, matchKind, allowUnknown)
  return (expression, compatibleType, matchKind)
end

function matchArrayDims(
  dims1::List{<:Dimension},
  dims2::List{<:Dimension},
  ty::NFType,
  matchKind::MatchKindType,
  allowUnknown::Bool,
)::Tuple{NFType, MatchKindType}

  local rest_dims2::List{Dimension} = dims2
  local cdims::List{Dimension} = nil
  local dim2::Dimension
  local compat::Bool

  if !isCompatibleMatch(matchKind)
    return (ty, matchKind)
  end
  #=  The array types must have the same number of dimensions.
  =#
  if listLength(dims1) != listLength(dims2)
    @assign matchKind = MatchKind.NOT_COMPATIBLE
    return (ty, matchKind)
  end
  #=  The dimensions of both array types must be compatible.
  =#
  for dim1 in dims1
    @match _cons(dim2, rest_dims2) = rest_dims2
    @assign (dim1, compat) = matchDimensions(dim1, dim2, allowUnknown)
    if !compat
      @assign matchKind = MatchKind.NOT_COMPATIBLE
      break
    end
    @assign cdims = _cons(dim1, cdims)
  end
  @assign ty = TYPE_ARRAY(ty, listReverseInPlace(cdims))
  return (ty, matchKind)
end

function matchDimensions(
  dim1::Dimension,
  dim2::Dimension,
  allowUnknown::Bool,
)::Tuple{Dimension, Bool}
  local compatible::Bool
  local compatibleDim::Dimension

  if isEqual(dim1, dim2)
    compatibleDim = dim1
    compatible = true
  else
    if !isKnown(dim1)
      compatibleDim = dim2
      compatible = true
    elseif !isKnown(dim2)
      compatibleDim = dim1
      compatible = true
    else
      compatibleDim = dim1
      compatible = false
    end
  end
  return (compatibleDim, compatible)
end

function matchTupleTypes(
  tupleType1::NFType,
  tupleType2::NFType,
  expression::Expression,
  allowUnknown::Bool,
)::Tuple{Expression, NFType, MatchKindType}
  local matchKind::MatchKindType = MatchKind.EXACT
  local compatibleType::NFType = tupleType1

  local tyl1::List{NFType}
  local tyl2::List{NFType}
  local ty1::NFType

  @match TYPE_TUPLE(types = tyl1) = tupleType1
  @match TYPE_TUPLE(types = tyl2) = tupleType2
  if listLength(tyl1) < listLength(tyl2)
    @assign matchKind = MatchKind.NOT_COMPATIBLE
    return (expression, compatibleType, matchKind)
  end
  for ty2 in tyl2
    @match _cons(ty1, tyl1) = tyl1
    if Type.isUnknown(ty2)
      continue
    end
    @assign (_, _, matchKind) = matchTypes(ty1, ty2, expression, allowUnknown)
    if matchKind != MatchKind.EXACT
      break
    end
  end
  #=  Skip matching if the rhs is _.
  =#
  return (expression, compatibleType, matchKind)
end

function matchBoxedExpressions(
  exp1::Expression,
  type1::NFType,
  exp2::Expression,
  type2::NFType,
  allowUnknown::Bool,
)::Tuple{Expression, Expression, NFType, MatchKindType}
  local matchKind::MatchKindType
  local compatibleType::NFType

  local e1::Expression
  local e2::Expression

  @assign e1 = unbox(exp1)
  @assign e2 = unbox(exp2)
  @assign (e1, e2, compatibleType, matchKind) =
    matchExpressions(e1, Type.unbox(type1), e2, Type.unbox(type2), allowUnknown)
  if isCastMatch(matchKind)
    @assign exp1 = box(e1)
    @assign exp2 = box(e2)
  end
  @assign compatibleType = Type.box(compatibleType)
  return (exp1, exp2, compatibleType, matchKind)
end

function matchTypes_cast(
  actualType::NFType,
  expectedType::NFType,
  expression::Expression,
  allowUnknown::Bool = false,
)::Tuple{Expression, NFType, MatchKindType}
  local matchKind::MatchKindType
  local compatibleType::NFType

  @assign (compatibleType, matchKind) = begin
    @match (actualType, expectedType) begin
      (TYPE_INTEGER(__), TYPE_REAL(__)) => begin
        #=  Integer can be cast to Real.
        =#
        @assign expression = typeCast(expression, expectedType)
        (expectedType, MatchKind.CAST)
      end

      (TYPE_ENUMERATION(__), TYPE_ENUMERATION_ANY(__)) => begin
        #=  Any enumeration is compatible with enumeration(:).
        =#
        #=  TODO: FIXME: Maybe this should be generic match
        =#
        (actualType, MatchKind.CAST)
      end

      (TYPE_TUPLE(types = _ <| _), _) => begin
        #=  If the actual type is a tuple but the expected type isn't,
        =#
        #=  try to use the first type in the tuple.
        =#
        @assign (expression, compatibleType, matchKind) =
          matchTypes(listHead(actualType.types), expectedType, expression, allowUnknown)
        if isCompatibleMatch(matchKind)
          @assign expression = begin
            @match expression begin
              TUPLE_EXPRESSION(__) => begin
                listHead(expression.elements)
              end

              _ => begin
                TUPLE_ELEMENT_EXPRESSION(
                  expression,
                  1,
                  setArrayElementType(
                    typeOf(expression),
                    compatibleType,
                  ),
                )
              end
            end
          end
          @assign matchKind = MatchKind.CAST
        end
        (compatibleType, matchKind)
      end

      (TYPE_UNKNOWN(__), _) => begin
        (expectedType, if allowUnknown
          MatchKind.UNKNOWN_ACTUAL
        else
          MatchKind.NOT_COMPATIBLE
        end)
      end

      (_, TYPE_UNKNOWN(__)) => begin
        (actualType, if allowUnknown
          MatchKind.UNKNOWN_EXPECTED
        else
          MatchKind.NOT_COMPATIBLE
        end)
      end

      (TYPE_METABOXED(__), _) => begin
        #=  Allow unknown types in some cases, e.g. () has type METALIST(UNKNOWN)
        =#
        @assign expression = unbox(expression)
        @assign (expression, compatibleType, matchKind) =
          matchTypes(actualType.ty, expectedType, expression, allowUnknown)
        (compatibleType, if isCompatibleMatch(matchKind)
          MatchKind.CAST
        else
          matchKind
        end)
      end

      (_, TYPE_METABOXED(__)) => begin
        @assign (expression, compatibleType, matchKind) =
          matchTypes(actualType, expectedType.ty, expression, allowUnknown)
        @assign expression = box(expression)
        @assign compatibleType = Type.box(compatibleType)
        (compatibleType, if isCompatibleMatch(matchKind)
          MatchKind.CAST
        else
          matchKind
        end)
      end

      (_, TYPE_POLYMORPHIC(__)) => begin
        @assign expression = BOX_EXPRESSION(expression)
        #=  matchKind := MatchKind.GENERIC(expectedType.b,actualType);
        =#
        (TYPE_METABOXED(actualType), MatchKind.GENERIC)
      end

      (TYPE_POLYMORPHIC(__), _) => begin
        #=  expression := Expression.UNBOX(expression, Expression.typeOf(expression));
        =#
        #=  matchKind := MatchKind.GENERIC(expectedType.b,actualType);
        =#
        (expectedType, MatchKind.GENERIC)
      end

      (_, TYPE_ANY(__)) => begin
        (expectedType, MatchKind.EXACT)
      end

      _ => begin
        (TYPE_UNKNOWN(), MatchKind.NOT_COMPATIBLE)
      end
    end
  end
  #=  Expected type is any, any actual type matches.
  =#
  #=  Anything else is not compatible.
  =#
  return (expression, compatibleType, matchKind)
end

function getRangeType(
  startExp::Expression,
  stepExp::Option{<:Expression},
  stopExp::Expression,
  rangeElemType::NFType,
  info::SourceInfo,
)::NFType
  local rangeType::NFType

  local step_exp::Expression
  local dim::Dimension

  @assign dim = begin
    @match rangeElemType begin
      TYPE_INTEGER(__) => begin
        getRangeTypeInt(startExp, stepExp, stopExp, info)
      end

      TYPE_REAL(__) => begin
        getRangeTypeReal(startExp, stepExp, stopExp, info)
      end

      TYPE_BOOLEAN(__) => begin
        if isSome(stepExp)
          Error.addSourceMessageAndFail(
            Error.RANGE_INVALID_STEP,
            list(Type.toString(rangeElemType)),
            info,
          )
        end
        getRangeTypeBool(startExp, stopExp)
      end

      TYPE_ENUMERATION(__) => begin
        if isSome(stepExp)
          Error.addSourceMessageAndFail(
            Error.RANGE_INVALID_STEP,
            list(Type.toString(rangeElemType)),
            info,
          )
        end
        getRangeTypeEnum(startExp, stopExp)
      end

      _ => begin
        Error.addSourceMessage(
          Error.RANGE_INVALID_TYPE,
          list(Type.toString(rangeElemType)),
          info,
        )
        fail()
      end
    end
  end
  @assign rangeType = TYPE_ARRAY(rangeElemType, list(dim))
  return rangeType
end

function getRangeTypeInt(
  startExp::Expression,
  stepExp::Option{<:Expression},
  stopExp::Expression,
  info::SourceInfo,
)::Dimension
  local dim::Dimension

  @assign dim = begin
    local step::Int
    local step_exp::Expression
    local dim_exp::Expression
    local var::VariabilityType
    @match (startExp, stepExp, stopExp) begin
      (INTEGER_EXPRESSION(__), NONE(), INTEGER_EXPRESSION(__)) => begin
        fromInteger(max(stopExp.value - startExp.value + 1, 0))
      end

      (
        INTEGER_EXPRESSION(__),
        SOME(INTEGER_EXPRESSION(value = step)),
        INTEGER_EXPRESSION(__),
      ) => begin
        #=  Don't allow infinite ranges.
        =#
        if step == 0
          Error.addSourceMessageAndFail(Error.RANGE_TOO_SMALL_STEP, list(String(step)), info)
        end
        P_Dimension.Dimension.fromInteger(max(
          intDiv(stopExp.value - startExp.value, step) + 1,
          0,
        ))
      end

      (INTEGER_EXPRESSION(1), NONE(), _) => begin
        #=  Ranges like 1:n have size n.
        =#
        @assign dim_exp = simplify(stopExp)
        fromExp(dim_exp, variability(dim_exp))
      end

      (_, NONE(), _) where {(isEqual(startExp, stopExp))} => begin
        P_Dimension.Dimension.fromInteger(1)
      end

      _ => begin
        #=  Ranges like n:n have size 1.
        =#
        #=  For other ranges, create the appropriate expression as dimension.
        =#
        #=  max(stop - start + 1, 0) or max(((stop - start) / step) + 1, 0)
        =#
        @assign dim_exp = BINARY_EXPRESSION(
          stopExp,
          makeSub(TYPE_INTEGER()),
          startExp,
        )
        @assign var = variabilityMax(
          variability(stopExp),
          variability(startExp),
        )
        if isSome(stepExp)
          @match SOME(step_exp) = stepExp
          @assign var = variabilityMax(
            var,
            variability(step_exp),
          )
          @assign dim_exp = CALL_EXPRESSION(P_Call.makeTypedCall(
            NFBuiltinFuncs.DIV_INT,
            list(dim_exp, step_exp),
            var,
          ))
        end
        @assign dim_exp = BINARY_EXPRESSION(
          dim_exp,
          makeAdd(TYPE_INTEGER()),
          INTEGER_EXPRESSION(1),
        )
        @assign dim_exp = CALL_EXPRESSION(makeTypedCall(
          NFBuiltinFuncs.MAX_INT,
          list(dim_exp, INTEGER_EXPRESSION(0)),
          var,
        ))
        @assign dim_exp = simplify(dim_exp)
        fromExp(dim_exp, var)
      end
    end
  end
  return dim
end

function getRangeTypeReal(
  startExp::Expression,
  stepExp::Option{<:Expression},
  stopExp::Expression,
  info::SourceInfo,
)::Dimension
  local dim::Dimension

  @assign dim = begin
    local start::AbstractFloat
    local step::AbstractFloat
    local dim_exp::Expression
    local step_exp::Expression
    local var::VariabilityType
    @match (startExp, stepExp, stopExp) begin
      (REAL_EXPRESSION(__), NONE(), REAL_EXPRESSION(__)) =>
        begin
          P_Dimension.Dimension.fromInteger(Util.realRangeSize(
            startExp.value,
            1.0,
            stopExp.value,
          ))
        end

      (
        REAL_EXPRESSION(value = start),
        SOME(REAL_EXPRESSION(value = step)),
        REAL_EXPRESSION(__),
      ) => begin
        #=  Check that adding step to start actually produces a different value,
        =#
        #=  otherwise the step size is too small.
        =#
        if start == start + step
          Error.addSourceMessageAndFail(Error.RANGE_TOO_SMALL_STEP, list(String(step)), info)
        end
        P_Dimension.Dimension.fromInteger(Util.realRangeSize(
          startExp.value,
          step,
          stopExp.value,
        ))
      end

      (_, NONE(), _) where {(isEqual(startExp, stopExp))} => begin
        P_Dimension.Dimension.fromInteger(1)
      end

      _ => begin
        @assign dim_exp = BINARY_EXPRESSION(
          stopExp,
          makeSub(TYPE_REAL()),
          startExp,
        )
        @assign var = variabilityMax(
          variability(stopExp),
          variability(startExp),
        )
        if isSome(stepExp)
          @match SOME(step_exp) = stepExp
          @assign var = variabilityMax(
            var,
            variability(step_exp),
          )
          @assign dim_exp = BINARY_EXPRESSION(
            dim_exp,
            makeDiv(TYPE_REAL()),
            step_exp,
          )
          @assign dim_exp = BINARY_EXPRESSION(
            dim_exp,
            makeAdd(TYPE_REAL()),
            REAL_EXPRESSION(5e-15),
          )
        end
        dim_exp = CALL_EXPRESSION(makeTypedCall(
          NFBuiltinFuncs.FLOOR,
          list(dim_exp),
          var,
        ))
        dim_exp = CALL_EXPRESSION(makeTypedCall(
          NFBuiltinFuncs.INTEGER_REAL,
          list(dim_exp),
          var,
        ))
        dim_exp = BINARY_EXPRESSION(
          dim_exp,
          makeAdd(TYPE_INTEGER()),
          INTEGER_EXPRESSION(1),
        )
        dim_exp = simplify(dim_exp)
        fromExp(dim_exp, var)
      end
    end
  end
  return dim
end

function getRangeTypeBool(startExp::Expression, stopExp::Expression)::Dimension
  local dim::Dimension
  @assign dim = begin
    local sz::Int
    local dim_exp::Expression
    local var::VariabilityType
    @match (startExp, stopExp) begin
      (P_Expression.BOOLEAN_EXPRESSION(__), P_Expression.BOOLEAN_EXPRESSION(__)) => begin
        @assign sz = if startExp.value == stopExp.value
          1
        elseif (startExp.value < stopExp.value)
          2
        else
          0
        end
        P_Dimension.Dimension.fromInteger(sz)
      end

      _ => begin
        if isEqual(startExp, stopExp)
          @assign dim = P_Dimension.Dimension.fromInteger(1)
        else
          @assign var = variabilityMax(
            variability(startExp),
            variability(stopExp),
          )
          @assign dim_exp = IF_EXPRESSION(
            RELATION_EXPRESSION(
              startExp,
              makeEqual(TYPE_BOOLEAN()),
              stopExp,
            ),
            INTEGER_EXPRESSION(1),
            IF_EXPRESSION(
              RELATION_EXPRESSION(
                startExp,
                makeLess(TYPE_BOOLEAN()),
                stopExp,
              ),
              INTEGER_EXPRESSION(2),
              INTEGER_EXPRESSION(0),
            ),
          )
          @assign dim_exp = SimplifyExp.simplify(dim_exp)
          @assign dim = fromExp(dim_exp, var)
        end
        dim
      end
    end
  end
  return dim
end

function getRangeTypeEnum(startExp::Expression, stopExp::Expression)::Dimension
  local dim::Dimension

  @assign dim = begin
    local dim_exp::Expression
    local var::VariabilityType
    @match (startExp, stopExp) begin
      (ENUM_LITERAL_EXPRESSION(__), ENUM_LITERAL_EXPRESSION(__)) => begin
        P_Dimension.Dimension.fromInteger(max(stopExp.index - startExp.index + 1, 0))
      end

      (ENUM_LITERAL_EXPRESSION(index = 1), _) => begin
        fromExp(stopExp, variability(stopExp))
      end

      _ => begin
        if isEqual(startExp, stopExp)
          @assign dim = P_Dimension.Dimension.fromInteger(1)
        else
          @assign var = variabilityMax(
            variability(startExp),
            variability(stopExp),
          )
          @assign dim_exp = BINARY_EXPRESSION(
            enumIndexExp(startExp),
            makeSub(TYPE_INTEGER()),
            enumIndexExp(stopExp),
          )
          @assign dim_exp = BINARY_EXPRESSION(
            dim_exp,
            makeAdd(TYPE_INTEGER()),
            INTEGER_EXPRESSION(1),
          )
          @assign dim_exp = SimplifyExp.simplify(dim_exp)
          @assign dim = fromExp(dim_exp, var)
        end
        dim
      end
    end
  end
  return dim
end

function matchBinding(
  binding::Binding,
  componentType::NFType,
  name::String,
  component::InstNode,
)::Binding
  @assign () = begin
    local ty_match::MatchKindType
    local exp::Expression
    local ty::NFType
    local exp_ty::NFType
    local comp_ty::NFType
    local dims::List{List{Dimension}}
    @match binding begin
      TYPED_BINDING(bindingExp = exp) => begin
        @assign (exp_ty, comp_ty) = begin
          @match exp begin
            BINDING_EXP(
              __,
            ) where {(binding.eachType == EachType.NOT_EACH)} => begin
              dims = list(
                arrayDims(getType(p)) for p in listRest(exp.parents)
              )
              (
                exp.expType,
                liftArrayLeftList(componentType, ListUtil.flattenReverse(dims)),
              )
            end

            _ => begin
              (binding.bindingType, componentType)
            end
          end
        end
        (exp, ty, ty_match) = matchTypes(exp_ty, comp_ty, exp, allowUnknown = true)
        if !isValidAssignmentMatch(ty_match)
          printBindingTypeError(name, binding, comp_ty, exp_ty, component)
          fail()
        elseif isCastMatch(ty_match)
          binding = TYPED_BINDING(
            exp,
            ty,
            binding.variability,
            binding.eachType,
            binding.evaluated,
            binding.isFlattened,
            binding.info,
          )
        end
        ()
      end

      UNBOUND(__) => begin
        ()
      end
      _ => begin
        # Error.assertion(
        #   false,
        #   getInstanceName() + " got untyped binding " + toString(binding),
        #   sourceInfo(),
        # )
        @error "Untyped binding"
        fail()
      end
    end
  end
  return binding
end

function printBindingTypeError(
  name::String,
  binding::Binding,
  componentType::NFType,
  bindingType::NFType,
  component::InstNode,
)
  local binding_info::SourceInfo
  local comp_info::SourceInfo
  local bind_ty_str::String
  local comp_ty_str::String
  local mk::MatchKindType

  @assign binding_info = Binding_getInfo(binding)
  @assign comp_info = InstNode_info(component)
  return if isScalar(bindingType) && isArray(componentType)
    Error.addMultiSourceMessage(
      Error.MODIFIER_NON_TYPE_ARRAY_ERROR,
      list(toString(binding), name),
      list(binding_info, comp_info),
    )
  else
    @assign (_, _, mk) = matchTypes(
      arrayElementType(bindingType),
      arrayElementType(componentType),
      EMPTY_BINDING(bindingType),
      true,
    )
    if !Config.getGraphicsExpMode()
      if isValidAssignmentMatch(mk)
        Error.addMultiSourceMessage(
          Error.VARIABLE_BINDING_DIMS_MISMATCH,
          list(
            name,
            toString(binding),
            P_Dimension.Dimension.toStringList(arrayDims(componentType)),
            P_Dimension.Dimension.toStringList(arrayDims(bindingType)),
          ),
          list(binding_info, comp_info),
        )
      else
        Error.addMultiSourceMessage(
          Error.VARIABLE_BINDING_TYPE_MISMATCH,
          list(
            name,
            toString(binding),
            Type.toString(componentType),
            Type.toString(bindingType),
          ),
          list(binding_info, comp_info),
        )
      end
    end
  end
  #=  forget errors when handling annotations
  =#
end

""" #= Checks that an expression used as a dimension has a valid type for a
   dimension, otherwise prints an error and fails. =#"""
function checkDimensionType(exp::Expression, ty::NFType, info::SourceInfo)
  return if !isInteger(ty)
    @assign () = begin
      @match exp begin
        TYPENAME_EXPRESSION(ty = TYPE_ARRAY(elementType = TYPE_BOOLEAN(__))) => begin
          ()
        end

        TYPENAME_EXPRESSION(
          ty = TYPE_ARRAY(elementType = TYPE_ENUMERATION(__)),
        ) => begin
          ()
        end

        _ => begin
          Error.addSourceMessage(
            Error.INVALID_DIMENSION_TYPE,
            list(toString(exp), Type.toString(ty)),
            info,
          )
          fail()
        end
      end
    end
  end
end

function checkReductionType(ty::NFType, name::Absyn.Path, exp::Expression, info::SourceInfo)
  local ety::NFType
  local err::String
  @assign err = begin
    @match name begin
      Absyn.IDENT("sum") => begin
        begin
          @match arrayElementType(ty) begin
            TYPE_INTEGER(__) => begin
              ""
            end

            TYPE_REAL(__) => begin
              ""
            end

            TYPE_COMPLEX(__) where {(checkSumComplexType(ty, exp, info))} => begin
              ""
            end

            _ => begin
              "Integer or Real, or operator record"
            end
          end
        end
      end

      Absyn.IDENT("product") => begin
        begin
          @match ty begin
            TYPE_INTEGER(__) => begin
              ""
            end

            TYPE_REAL(__) => begin
              ""
            end

            _ => begin
              "scalar Integer or Real"
            end
          end
        end
      end

      Absyn.IDENT("min") => begin
        begin
          @match ty begin
            TYPE_INTEGER(__) => begin
              ""
            end

            TYPE_REAL(__) => begin
              ""
            end

            TYPE_BOOLEAN(__) => begin
              ""
            end

            TYPE_ENUMERATION(__) => begin
              ""
            end

            _ => begin
              "scalar enumeration, Boolean, Integer, or Real"
            end
          end
        end
      end

      Absyn.IDENT("max") => begin
        begin
          @match ty begin
            TYPE_INTEGER(__) => begin
              ""
            end

            TYPE_REAL(__) => begin
              ""
            end

            TYPE_BOOLEAN(__) => begin
              ""
            end

            TYPE_ENUMERATION(__) => begin
              ""
            end

            _ => begin
              "scalar enumeration, Boolean, Integer, or Real"
            end
          end
        end
      end

      _ => begin
        ""
      end
    end
  end
  return if !stringEmpty(err)
    Error.addSourceMessageAndFail(
      Error.INVALID_REDUCTION_TYPE,
      list(
        toString(exp),
        Type.toString(ty),
        AbsynUtil.pathString(name),
        err,
      ),
      info,
    )
  end
end

function checkSumComplexType(ty::NFType, exp::Expression, info::SourceInfo)::Bool
  local valid::Bool = true
  local cls_node::InstNode
  local op_node::InstNode
  local cls::Class
  @match TYPE_COMPLEX(cls = cls_node) = ty
  cls = getClass(cls_node)
  for op in list("'+'", "'0'")
    if !hasOperator(op, cls)
      Error.addSourceMessage(
        Error.OPERATOR_RECORD_MISSING_OPERATOR,
        list(Type.toString(ty), toString(exp), "sum", op),
        info,
      )
      @assign valid = false
    end
  end
  return valid
end
