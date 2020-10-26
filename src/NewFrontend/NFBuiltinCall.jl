
function needSpecialHandling(call::Call) ::Bool
  local special::Bool

  @assign () = begin
    @match call begin
      UNTYPED_CALL(__)  => begin
        @match C_FUNCTION(specialBuiltin = special) = getFuncCache(classScope(node(call.ref)))
        ()
      end

      _  => begin
        Error.assertion(false, getInstanceName() + " got unknown call: " + P_Call.toString(call), sourceInfo())
        fail()
      end
    end
  end
  special
end

function typeSpecial(call::Call, origin::ORIGIN_Type, info::SourceInfo) ::Tuple{Expression, M_Type, VariabilityType}
  local variability::VariabilityType
  local ty::M_Type
  local callExp::Expression

  local cref::ComponentRef
  local fn_node::InstNode
  local first::Expression
  local rest::List{Expression}
  local name::String
  local next_origin::ORIGIN_Type

  @match UNTYPED_CALL(ref = cref) = call
  @assign next_origin = setFlag(origin, ORIGIN_SUBEXPRESSION)
  @assign (callExp, ty, variability) = begin
    @match firstName(cref) begin
      "String"  => begin
        typeStringCall(call, next_origin, info)
      end

      "actualStream"  => begin
        typeActualInStreamCall("actualStream", call, next_origin, info)
      end

      "branch"  => begin
        typeBranchCall(call, next_origin, info)
      end

      "cardinality"  => begin
        typeCardinalityCall(call, next_origin, info)
      end

      "cat"  => begin
        typeCatCall(call, next_origin, info)
      end

      "change"  => begin
        typeChangeCall(call, next_origin, info)
      end

      "der"  => begin
        typeDerCall(call, next_origin, info)
      end

      "diagonal"  => begin
        typeDiagonalCall(call, next_origin, info)
      end

      "edge"  => begin
        typeEdgeCall(call, next_origin, info)
      end

      "fill"  => begin
        typeFillCall(call, next_origin, info)
      end

      "getInstanceName"  => begin
        typeGetInstanceName(call)
      end

      "initial"  => begin
        typeDiscreteCall(call, next_origin, info)
      end

      "inStream"  => begin
        typeActualInStreamCall("inStream", call, next_origin, info)
      end

      "isRoot"  => begin
        typeIsRootCall(call, next_origin, info)
      end

      "matrix"  => begin
        typeMatrixCall(call, next_origin, info)
      end

      "max"  => begin
        typeMinMaxCall("max", call, next_origin, info)
      end

      "min"  => begin
        typeMinMaxCall("min", call, next_origin, info)
      end

      "ndims"  => begin
        typeNdimsCall(call, next_origin, info)
      end

      "noEvent"  => begin
        typeNoEventCall(call, next_origin, info)
      end

      "ones"  => begin
        typeZerosOnesCall("ones", call, next_origin, info)
      end

      "potentialRoot"  => begin
        typePotentialRootCall(call, next_origin, info)
      end

      "pre"  => begin
        typePreCall(call, next_origin, info)
      end

      "product"  => begin
        typeProductCall(call, next_origin, info)
      end

      "root"  => begin
        typeRootCall(call, next_origin, info)
      end

      "rooted"  => begin
        typeRootedCall(call, next_origin, info)
      end

      "uniqueRoot"  => begin
        typeUniqueRootCall(call, next_origin, info)
      end

      "uniqueRootIndices"  => begin
        typeUniqueRootIndicesCall(call, next_origin, info)
      end

      "scalar"  => begin
        typeScalarCall(call, next_origin, info)
      end

      "smooth"  => begin
        typeSmoothCall(call, next_origin, info)
      end

      "sum"  => begin
        typeSumCall(call, next_origin, info)
      end

      "symmetric"  => begin
        typeSymmetricCall(call, next_origin, info)
      end

      "terminal"  => begin
        typeDiscreteCall(call, next_origin, info)
      end

      "transpose"  => begin
        typeTransposeCall(call, next_origin, info)
      end

      "vector"  => begin
        typeVectorCall(call, next_origin, info)
      end

      "zeros"  => begin
        typeZerosOnesCall("zeros", call, next_origin, info)
      end

      "Clock" where (Config.synchronousFeaturesAllowed())  => begin
        typeClockCall(call, next_origin, info)
      end

      "sample"  => begin
        typeSampleCall(call, next_origin, info)
      end

      "DynamicSelect"  => begin
        typeDynamicSelectCall("DynamicSelect", call, next_origin, info)
      end

      _  => begin
        #= /*
        case \"hold\" guard Config.synchronousFeaturesAllowed() then typeHoldCall(call, next_origin, info);
        case \"shiftSample\" guard Config.synchronousFeaturesAllowed() then typeShiftSampleCall(call, next_origin, info);
        case \"backSample\" guard Config.synchronousFeaturesAllowed() then typeBackSampleCall(call, next_origin, info);
        case \"noClock\" guard Config.synchronousFeaturesAllowed() then typeNoClockCall(call, next_origin, info);
        case \"transition\" guard Config.synchronousFeaturesAllowed() then typeTransitionCall(call, next_origin, info);
        case \"initialState\" guard Config.synchronousFeaturesAllowed() then typeInitialStateCall(call, next_origin, info);
        case \"activeState\" guard Config.synchronousFeaturesAllowed() then typeActiveStateCall(call, next_origin, info);
        case \"ticksInState\" guard Config.synchronousFeaturesAllowed() then typeTicksInStateCall(call, next_origin, info);
        case \"timeInState\" guard Config.synchronousFeaturesAllowed() then typeTimeInStateCall(call, next_origin, info);
        */ =#
        Error.assertion(false, getInstanceName() + " got unhandled builtin function: " + P_Call.toString(call), sourceInfo())
        fail()
      end
    end
  end
(callExp, ty, variability)
end

function makeSizeExp(posArgs::List{<:Expression}, namedArgs::List{<:NamedArg}, info::SourceInfo) ::Expression
  local callExp::Expression

  local argc::Integer = listLength(posArgs)
  local arg1::Expression
  local arg2::Expression

  assertNoNamedParams("size", namedArgs, info)
  @assign callExp = begin
    @match posArgs begin
      arg1 <|  nil()  => begin
        SIZE_EXPRESSION(arg1, NONE())
      end

      arg1 <| arg2 <|  nil()  => begin
        SIZE_EXPRESSION(arg1, SOME(arg2))
      end

      _  => begin
        Error.addSourceMessage(Error.NO_MATCHING_FUNCTION_FOUND_NFINST, list("size" + ListUtil.toString(posArgs, toString, "", "(", ", ", ")", true), "size(Any[:, ...]) => Integer[:]\\n  size(Any[:, ...], Integer) => Integer"), info)
        fail()
      end
    end
  end
  callExp
end

function makeArrayExp(posArgs::List{<:Expression}, namedArgs::List{<:NamedArg}, info::SourceInfo) ::Expression
  local arrayExp::Expression

  local fn_ref::ComponentRef
  local args::List{Expression}
  local named_args::List{NamedArg}
  local ty::M_Type

  assertNoNamedParams("array", namedArgs, info)
  #=  array can take any number of arguments, but needs at least one.
  =#
  if listEmpty(posArgs)
    Error.addSourceMessage(Error.NO_MATCHING_FUNCTION_FOUND_NFINST, list("array" + ListUtil.toString(posArgs, toString, "", "(", ", ", ")", true), "array(Any, Any, ...) => Any[:]"), info)
    fail()
  end
  @assign arrayExp = makeArray(TYPE_UNKNOWN(), posArgs)
  arrayExp
end

function makeCatExp(n::Integer, args::List{<:Expression}, tys::List{<:M_Type}, variability::VariabilityType, info::SourceInfo) ::Tuple{Expression, M_Type}
  local ty::M_Type
  local callExp::Expression

  local arg2::Expression
  local args2::List{Expression} = nil
  local res::List{Expression} = nil
  local tys2::List{M_Type} = tys
  local tys3::List{M_Type}
  local dimsLst::List{List{Dimension}} = nil
  local dims::List{Dimension}
  local resTy::M_Type = TYPE_UNKNOWN()
  local ty1::M_Type
  local ty2::M_Type
  local resTyToMatch::M_Type
  local mk::TypeCheck.MatchKind
  local maxn::Integer
  local pos::Integer
  local sumDim::Dimension

  Error.assertion(listLength(args) == listLength(tys) && listLength(args) >= 1, getInstanceName() + " got wrong input sizes", sourceInfo())
  #=  First: Get the number of dimensions and the element type
  =#
  for arg in args
    @match _cons(ty, tys2) = tys2
    @assign dimsLst = _cons(arrayDims(ty), dimsLst)
    if Type.isEqual(resTy, TYPE_UNKNOWN())
      @assign resTy = arrayElementType(ty)
    else
      @assign (_, _, ty1, mk) = TypeCheck.matchExpressions(INTEGER_EXPRESSION(0), arrayElementType(ty), INTEGER_EXPRESSION(0), resTy)
      if TypeCheck.isCompatibleMatch(mk)
        @assign resTy = ty1
      end
    end
  end
  @assign maxn = max(listLength(d) for d in dimsLst)
  if maxn != min(listLength(d) for d in dimsLst)
    Error.addSourceMessageAndFail(Error.NF_DIFFERENT_NUM_DIM_IN_ARGUMENTS, list(stringDelimitList(list(String(listLength(d)) for d in dimsLst), ", "), "cat"), info)
  end
  if n < 1 || n > maxn
    Error.addSourceMessageAndFail(Error.NF_CAT_WRONG_DIMENSION, list(String(maxn), String(n)), info)
  end
  @assign tys2 = tys
  @assign tys3 = nil
  @assign args2 = nil
  @assign pos = listLength(args) + 2
  #=  Second: Try to match the element type of all the arguments
  =#
  for arg in args
    @match _cons(ty, tys2) = tys2
    @assign pos = pos - 1
    @assign ty2 = setArrayElementType(ty, resTy)
    @assign (arg2, ty1, mk) = matchTypes(ty, ty2, arg, allowUnknown = true)
    if TypeCheck.isIncompatibleMatch(mk)
      Error.addSourceMessageAndFail(Error.ARG_TYPE_MISMATCH, list(String(pos), "cat", "arg", toString(arg), Type.toString(ty), Type.toString(ty2)), info)
    end
    @assign args2 = _cons(arg2, args2)
    @assign tys3 = _cons(ty1, tys3)
  end
  #=  Third: We now have matched the element types of all arguments
  =#
  #=         Try to match the dimensions as well
  =#
  @assign resTy = TYPE_UNKNOWN()
  @assign tys2 = tys3
  for arg in args2
    @match _cons(ty, tys2) = tys2
    if Type.isEqual(resTy, TYPE_UNKNOWN())
      @assign resTy = ty
    else
      @assign (_, _, ty1, mk) = TypeCheck.matchExpressions(INTEGER_EXPRESSION(0), ty, INTEGER_EXPRESSION(0), resTy)
      if TypeCheck.isCompatibleMatch(mk)
        @assign resTy = ty1
      end
    end
  end
  #=  Got the supertype of the dimensions; trying to match all arguments
  =#
  #=  with the concatenated dimension set to unknown.
  =#
  @assign dims = arrayDims(resTy)
  @assign resTyToMatch = ARRAY_TYPE(arrayElementType(resTy), ListUtil.set(dims, n, P_Dimension.Dimension.UNKNOWN()))
  @assign dims = list(listGet(lst, n) for lst in dimsLst)
  @assign sumDim = P_Dimension.Dimension.fromInteger(0)
  for d in dims
    @assign sumDim = P_Dimension.Dimension.add(sumDim, d)
  end
  #=  Create the concatenated dimension
  =#
  @assign resTy = ARRAY_TYPE(arrayElementType(resTy), ListUtil.set(arrayDims(resTy), n, sumDim))
  @assign tys2 = tys3
  @assign tys3 = nil
  @assign res = nil
  @assign pos = listLength(args) + 2
  for arg in args2
    @match _cons(ty, tys2) = tys2
    @assign pos = pos - 1
    @assign (arg2, ty1, mk) = matchTypes(ty, resTyToMatch, arg, allowUnknown = true)
    if TypeCheck.isIncompatibleMatch(mk)
      Error.addSourceMessageAndFail(Error.ARG_TYPE_MISMATCH, list(String(pos), "cat", "arg", toString(arg), Type.toString(ty), Type.toString(resTyToMatch)), info)
    end
    @assign res = _cons(arg2, res)
    @assign tys3 = _cons(ty1, tys3)
  end
  #=  We have all except dimension n having equal sizes; with matching types
  =#
  @assign ty = resTy
  @assign callExp = CALL_EXPRESSION(P_Call.makeTypedCall(NFBuiltinFuncs.CAT, _cons(INTEGER_EXPRESSION(n), res), variability, resTy))
  (callExp, ty)
end

function assertNoNamedParams(fnName::String, namedArgs::List{<:NamedArg}, info::SourceInfo)
  if ! listEmpty(namedArgs)
    Error.addSourceMessage(Error.NO_SUCH_PARAMETER, list(fnName, Util.tuple21(listHead(namedArgs))), info)
    fail()
  end
end

function typeStringCall(call::Call, origin::ORIGIN_Type, info::SourceInfo) ::Tuple{Expression, M_Type, Variability}
  local var::VariabilityType
  local outType::M_Type
  local callExp::Expression

  local arg_ty::M_Type
  local args::List{TypedArg}
  local named_args::List{TypedNamedArg}
  local ty_call::Call

  @match (@match ARG_TYPED_CALL(_, args, named_args) = ty_call) = P_Call.typeNormalCall(call, origin, info)
  @match _cons((_, arg_ty, _), _) = args
  @assign arg_ty = arrayElementType(arg_ty)
  if isComplex(arg_ty)
    @assign (callExp, outType, var) = typeOverloadedStringCall(arg_ty, args, named_args, ty_call, origin, info)
  else
    @assign (callExp, outType, var) = typeBuiltinStringCall(ty_call, origin, info)
  end
  (callExp, outType, var)
end

function typeBuiltinStringCall(call::Call, origin::ORIGIN_Type, info::SourceInfo) ::Tuple{Expression, M_Type, Variability}
  local var::VariabilityType
  local ty::M_Type
  local callExp::Expression

  local ty_call::Call

  @assign ty_call = P_Call.matchTypedNormalCall(call, origin, info)
  @assign ty = typeOf(ty_call)
  @assign var = P_Call.variability(ty_call)
  @assign callExp = CALL_EXPRESSION(ty_call)
  (callExp, ty, var)
end

function typeOverloadedStringCall(overloadedType::M_Type, args::List{<:TypedArg}, namedArgs::List{<:TypedNamedArg}, call::Call, origin::ORIGIN_Type, info::SourceInfo) ::Tuple{Expression, M_Type, Variability}
  local var::VariabilityType = Variability.CONSTANT
  local outType::M_Type
  local callExp::Expression

  local fn_ref::ComponentRef
  local candidates::List{M_Function}
  local recopnode::InstNode
  local matchedFunc::MatchedFunction
  local matchedFunctions::List{MatchedFunction}
  local exactMatches::List{MatchedFunction}

  @match TYPE_COMPLEX(cls = recopnode) = overloadedType
  try
    @assign fn_ref = lookupFunctionSimple("'String'", recopnode)
  catch e
    @error "Error $e"
    typeBuiltinStringCall(call, origin, info)
    fail()
  end
  #=  If there's no 'String' overload, let the normal String handler print the error.
  =#
  @assign fn_ref = instFunctionRef(fn_ref, InstNode_info(recopnode))
  @assign candidates = typeRefCache(fn_ref)
  #= for fn in candidates loop
  =#
  #=   TypeCheck.checkValidOperatorOverload(\"'String'\", fn, recopnode);
  =#
  #= end for;
  =#
  @assign matchedFunctions = matchFunctionsSilent(candidates, args, namedArgs, info)
  @assign exactMatches = P_MatchedFunction.getExactMatches(matchedFunctions)
  if listEmpty(exactMatches)
    Error.addSourceMessage(Error.NO_MATCHING_FUNCTION_FOUND_NFINST, list(P_Call.typedString(call), P_Function.candidateFuncListString(candidates)), info)
    fail()
  end
  if listLength(exactMatches) == 1
    @match _cons(matchedFunc, _) = exactMatches
    @assign outType = P_Function.returnType(matchedFunc.func)
    for arg in matchedFunc.args
      @assign var = variabilityMax(var, Util.tuple33(arg))
    end
    @assign callExp = CALL_EXPRESSION(P_Call.makeTypedCall(matchedFunc.func, list(Util.tuple31(a) for a in matchedFunc.args), var, outType))
    return (callExp, outType, var)
  else
    Error.addSourceMessage(Error.AMBIGUOUS_MATCHING_FUNCTIONS_NFINST, list(P_Call.typedString(call), P_Function.candidateFuncListString(list(mfn.func for mfn in matchedFunctions))), info)
    fail()
  end
  (callExp, outType, var)
end

""" #= Types a function call that can be typed normally, but which always has
               discrete variability regardless of the variability of the arguments. =#"""
                 function typeDiscreteCall(call::Call, origin::ORIGIN_Type, info::SourceInfo) ::Tuple{Expression, M_Type, Variability}
                   local var::VariabilityType = Variability.DISCRETE
                   local ty::M_Type
                   local callExp::Expression

                   local argtycall::Call
                   local fn::M_Function
                   local args::List{TypedArg}
                   local start::TypedArg
                   local interval::TypedArg

                   @assign argtycall = P_Call.typeMatchNormalCall(call, origin, info)
                   @assign ty = typeOf(argtycall)
                   @assign callExp = CALL_EXPRESSION(unboxArgs(argtycall))
                   (callExp, ty, var)
                 end

function typeNdimsCall(call::Call, origin::ORIGIN_Type, info::SourceInfo) ::Tuple{Expression, M_Type, Variability}
  local variability::VariabilityType = Variability.PARAMETER
  local ty::M_Type = TYPE_INTEGER()
  local callExp::Expression

  local args::List{Expression}
  local named_args::List{NamedArg}
  local arg_ty::M_Type

  @match UNTYPED_CALL(arguments = args, named_args = named_args) = call
  assertNoNamedParams("ndims", named_args, info)
  if listLength(args) != 1
    Error.addSourceMessage(Error.NO_MATCHING_FUNCTION_FOUND_NFINST, list(P_Call.toString(call), "ndims(Any) => Integer"), info)
    fail()
  end
  #=  The number of dimensions an expression has is always known,
  =#
  #=  so we might as well evaluate the ndims call here.
  =#
  @assign (_, arg_ty, _) = typeExp(listHead(args), origin, info)
  @assign callExp = INTEGER_EXPRESSION(Type.dimensionCount(arg_ty))
  (callExp, ty, variability)
end

function typePreCall(call::Call, origin::ORIGIN_Type, info::SourceInfo)::Tuple{Expression, NFType, VariabilityType}
  local variability::VariabilityType
  local ty::M_Type
  local callExp::Expression
  @assign (callExp, ty, variability) = typePreChangeCall("pre", call, origin, info)
  (callExp, ty, variability)
end

function typeChangeCall(call::Call, origin::ORIGIN_Type, info::SourceInfo) ::Tuple{Expression, NFType, VariabilityType}
  local variability::VariabilityType
  local ty::M_Type
  local callExp::Expression

  @assign (callExp, ty, variability) = typePreChangeCall("change", call, origin, info)
  @assign ty = setArrayElementType(ty, TYPE_BOOLEAN())
  (callExp, ty, variability)
end

function typePreChangeCall(@nospecialize(name::String), @nospecialize(call::Call), origin::ORIGIN_Type, info::SourceInfo) ::Tuple{Expression, NFType, VariabilityType}
  local variability::VariabilityType = Variability.DISCRETE
  local ty::M_Type
  local callExp::Expression

  local fn_ref::ComponentRef
  local args::List{Expression}
  local named_args::List{NamedArg}
  local arg::Expression
  local var::VariabilityType
  local fn::M_Function

  @match UNTYPED_CALL(ref = fn_ref, arguments = args, named_args = named_args) = call
  assertNoNamedParams(name, named_args, info)
  if listLength(args) != 1
    Error.addSourceMessageAndFail(Error.NO_MATCHING_FUNCTION_FOUND_NFINST, list(P_Call.toString(call), toString(fn_ref) + "(Any) => Any"), info)
  end
  #=  pre/change may not be used in a function context.
  =#
  if flagSet(origin, ORIGIN_FUNCTION)
    Error.addSourceMessageAndFail(Error.EXP_INVALID_IN_FUNCTION, list(toString(fn_ref)), info)
  end
  @assign (arg, ty, var) = typeExp(listHead(args), origin, info)
  if ! isCref(arg)
    Error.addSourceMessage(Error.ARGUMENT_MUST_BE_VARIABLE, list("First", toString(fn_ref), "<REMOVE ME>"), info)
    fail()
  end
  if var == Variability.CONTINUOUS
    Error.addSourceMessageAndFail(Error.INVALID_ARGUMENT_VARIABILITY, list("1", toString(fn_ref), P_Prefixes.variabilityString(Variability.DISCRETE), toString(arg), P_Prefixes.variabilityString(var)), info)
  end
    #@match list(fn) = typeRefCache(fn_ref)
  fn = listHead(typeRefCache(fn_ref))
  callExp = CALL_EXPRESSION(makeTypedCall(fn, list(arg), var, ty))
  (callExp, ty, variability)
end

function typeDerCall(call::Call, origin::ORIGIN_Type, info::SourceInfo) ::Tuple{Expression, NFType, VariabilityType}
  local variability::VariabilityType
  local ty::M_Type
  local callExp::Expression

  local fn_ref::ComponentRef
  local args::List{Expression}
  local named_args::List{NamedArg}
  local arg::Expression
  local fn::M_Function
  local ety::M_Type

  #=  der may not be used in a function context.
  =#
  if flagSet(origin, ORIGIN_FUNCTION)
    Error.addSourceMessage(Error.EXP_INVALID_IN_FUNCTION, list("der"), info)
    fail()
  end
  @match UNTYPED_CALL(ref = fn_ref, arguments = args, named_args = named_args) = call
  assertNoNamedParams("der", named_args, info)
  if listLength(args) != 1
    Error.addSourceMessageAndFail(Error.NO_MATCHING_FUNCTION_FOUND_NFINST, list(P_Call.toString(call), "der(Real) => Real"), info)
  end
  @error "@match list(arg) = args"
  arg = listHead(args)
  @assign (arg, ty, variability) = typeExp(arg, origin, info)
  @assign ety = arrayElementType(ty)
  if isInteger(ety)
    @assign ty = setArrayElementType(ty, TYPE_REAL())
    @assign arg = typeCast(arg, TYPE_REAL())
  elseif ! isReal(ety)
    Error.addSourceMessageAndFail(Error.ARG_TYPE_MISMATCH, list("1", toString(fn_ref), "", toString(arg), Type.toString(ty), "Real"), info)
  end
  @error "@match list(fn) = typeRefCache(fn_ref) TODO"
  fn = listHead(typeRefCache(fn_ref))
  @assign callExp = CALL_EXPRESSION(makeTypedCall(fn, list(arg), variability, ty))
  (callExp, ty, variability)
end

function typeDiagonalCall(call::Call, origin::ORIGIN_Type, info::SourceInfo) ::Tuple{Expression, M_Type, Variability}
  local variability::VariabilityType
  local ty::M_Type
  local callExp::Expression

  local fn_ref::ComponentRef
  local args::List{Expression}
  local named_args::List{NamedArg}
  local arg::Expression
  local dim::Dimension
  local fn::M_Function

  @match UNTYPED_CALL(ref = fn_ref, arguments = args, named_args = named_args) = call
  assertNoNamedParams("diagonal", named_args, info)
  if listLength(args) != 1
    Error.addSourceMessageAndFail(Error.NO_MATCHING_FUNCTION_FOUND_NFINST, list(P_Call.toString(call), "diagonal(Any[n]) => Any[n, n]"), info)
  end
  @assign (arg, ty, variability) = typeExp(listHead(args), origin, info)
  @assign ty = begin
    @match ty begin
      ARRAY_TYPE(dimensions = dim <|  nil())  => begin
        ARRAY_TYPE(ty.elementType, list(dim, dim))
      end

      _  => begin
        Error.addSourceMessage(Error.ARG_TYPE_MISMATCH, list("1", toString(fn_ref), "", toString(arg), Type.toString(ty), "Any[:]"), info)
        fail()
      end
    end
  end
  @match list(fn) = typeRefCache(fn_ref)
  @assign callExp = CALL_EXPRESSION(P_Call.makeTypedCall(fn, list(arg), variability, ty))
  (callExp, ty, variability)
end

function typeEdgeCall(call::Call, origin::ORIGIN_Type, info::SourceInfo) ::Tuple{Expression, NFType, VariabilityType}
  local variability::VariabilityType = Variability.DISCRETE
  local ty::M_Type
  local callExp::Expression

  local argtycall::Call
  local fn::M_Function
  local args::List{TypedArg}
  local arg::TypedArg
  local fn_node::InstNode
  local ca::CallAttributes

  #=  edge may not be used in a function context.
  =#
  if flagSet(origin, ORIGIN_FUNCTION)
    Error.addSourceMessage(Error.EXP_INVALID_IN_FUNCTION, list("edge"), info)
    fail()
  end
   argtycall = typeNormalCall(call, origin, info)
    #  @match (@match ARG_TYPED_CALL(CREF_EXPRESSION(node = fn_node), args, _) = argtycall) = typeNormalCall(call, origin, info)
  args = argtycall.arguments
  @assign argtycall = matchTypedNormalCall(argtycall, origin, info)
  @assign ty = typeOf(argtycall)
  @assign callExp = CALL_EXPRESSION(unboxArgs(argtycall))
    #@match list(arg) = args
  arg = listHead(args)
  if ! isCref(Util.tuple31(arg))
    Error.addSourceMessage(Error.ARGUMENT_MUST_BE_VARIABLE, list("First", "edge", "<REMOVE ME>"), info)
    fail()
  end
  (callExp, ty, variability)
end

function typeMinMaxCall(name::String, call::Call, origin::ORIGIN_Type, info::SourceInfo) ::Tuple{Expression, M_Type, Variability}
  local var::VariabilityType
  local ty::M_Type
  local callExp::Expression

  local fn_ref::ComponentRef
  local args::List{Expression}
  local named_args::List{NamedArg}
  local arg::Expression
  local fn::M_Function
  local arg1::Expression
  local arg2::Expression
  local ty1::M_Type
  local ty2::M_Type
  local var1::VariabilityType
  local var2::VariabilityType
  local mk::TypeCheck.MatchKind

  @match UNTYPED_CALL(ref = fn_ref, arguments = args, named_args = named_args) = call
  assertNoNamedParams(name, named_args, info)
  @assign (args, ty, var) = begin
    @match args begin
      arg1 <|  nil()  => begin
        @assign (arg1, ty1, var) = typeExp(arg1, origin, info)
        @assign ty = arrayElementType(ty1)
        if ! (isArray(ty1) && Type.isBasic(ty))
          Error.addSourceMessageAndFail(Error.ARG_TYPE_MISMATCH, list("1", name, "", toString(arg1), Type.toString(ty1), "Any[:, ...]"), info)
        end
        #=  If the argument is an array with a single element we can just
        =#
        #=  return that element instead of making a min/max call.
        =#
        if Type.isSingleElementArray(ty1)
          @assign callExp = applySubscript(first(listHead(arrayDims(ty1))), arg1)
          return
        end
        (list(arg1), ty, var)
      end

      arg1 <| arg2 <|  nil()  => begin
        @assign (arg1, ty1, var1) = typeExp(arg1, origin, info)
        @assign (arg2, ty2, var2) = typeExp(arg2, origin, info)
        if ! Type.isBasic(ty1)
          Error.addSourceMessageAndFail(Error.ARG_TYPE_MISMATCH, list("1", name, "", toString(arg1), Type.toString(ty1), "Any"), info)
        end
        if ! Type.isBasic(ty2)
          Error.addSourceMessageAndFail(Error.ARG_TYPE_MISMATCH, list("2", name, "", toString(arg2), Type.toString(ty2), "Any"), info)
        end
        @assign (arg1, arg2, ty, mk) = TypeCheck.matchExpressions(arg1, ty1, arg2, ty2)
        if ! TypeCheck.isValidArgumentMatch(mk)
          Error.addSourceMessage(Error.NO_MATCHING_FUNCTION_FOUND_NFINST, list(P_Call.toString(call), name + "(Any[:, ...]) => Any\\n" + name + "(Any, Any) => Any"), info)
        end
        (list(arg1, arg2), ty, variabilityMax(var1, var2))
      end

      _  => begin
        Error.addSourceMessage(Error.NO_MATCHING_FUNCTION_FOUND_NFINST, list(P_Call.toString(call), name + "(Any[:, ...]) => Any\\n" + name + "(Any, Any) => Any"), info)
        fail()
      end
    end
  end
  @assign fn = listHead(typeRefCache(fn_ref))
  @assign callExp = CALL_EXPRESSION(P_Call.makeTypedCall(fn, args, var, ty))
  (callExp, ty, var)
end

function typeSumCall(call::Call, origin::ORIGIN_Type, info::SourceInfo) ::Tuple{Expression, M_Type, Variability}
  local variability::VariabilityType
  local ty::M_Type
  local callExp::Expression

  local fn_ref::ComponentRef
  local args::List{Expression}
  local named_args::List{NamedArg}
  local arg::Expression
  local fn::M_Function
  local expanded::Bool
  local op::Operator

  @match UNTYPED_CALL(ref = fn_ref, arguments = args, named_args = named_args) = call
  assertNoNamedParams("sum", named_args, info)
  if listLength(args) != 1
    Error.addSourceMessageAndFail(Error.NO_MATCHING_FUNCTION_FOUND_NFINST, list(P_Call.toString(call), "sum(Any[:, ...]) => Any"), info)
  end
  @assign (arg, ty, variability) = typeExp(listHead(args), origin, info)
  @assign ty = arrayElementType(ty)
  @match list(fn) = typeRefCache(fn_ref)
  @assign callExp = CALL_EXPRESSION(P_Call.makeTypedCall(fn, list(arg), variability, ty))
  (callExp, ty, variability)
end

function typeProductCall(call::Call, origin::ORIGIN_Type, info::SourceInfo) ::Tuple{Expression, M_Type, Variability}
  local variability::VariabilityType
  local ty::M_Type
  local callExp::Expression

  local fn_ref::ComponentRef
  local args::List{Expression}
  local named_args::List{NamedArg}
  local arg::Expression
  local fn::M_Function
  local expanded::Bool
  local op::Operator

  @match UNTYPED_CALL(ref = fn_ref, arguments = args, named_args = named_args) = call
  assertNoNamedParams("product", named_args, info)
  if listLength(args) != 1
    Error.addSourceMessageAndFail(Error.NO_MATCHING_FUNCTION_FOUND_NFINST, list(P_Call.toString(call), "product(Any[:, ...]) => Any"), info)
  end
  @assign (arg, ty, variability) = typeExp(listHead(args), origin, info)
  @assign ty = arrayElementType(ty)
  @match list(fn) = typeRefCache(fn_ref)
  @assign callExp = CALL_EXPRESSION(P_Call.makeTypedCall(fn, list(arg), variability, ty))
  (callExp, ty, variability)
end

function typeSmoothCall(call::Call, origin::ORIGIN_Type, info::SourceInfo) ::Tuple{Expression, M_Type, Variability}
  local variability::VariabilityType
  local ty::M_Type
  local callExp::Expression

  local fn_ref::ComponentRef
  local args::List{Expression}
  local named_args::List{NamedArg}
  local arg1::Expression
  local arg2::Expression
  local ty1::M_Type
  local ty2::M_Type
  local var::VariabilityType
  local fn::M_Function
  local mk::TypeCheck.MatchKind

  @match UNTYPED_CALL(ref = fn_ref, arguments = args, named_args = named_args) = call
  assertNoNamedParams("smooth", named_args, info)
  if listLength(args) != 2
    Error.addSourceMessageAndFail(Error.NO_MATCHING_FUNCTION_FOUND_NFINST, list(P_Call.toString(call), "smooth(Integer, Any) => Any"), info)
  end
  @match list(arg1, arg2) = args
  @assign (arg1, ty1, var) = typeExp(arg1, origin, info)
  @assign (arg2, ty2, variability) = typeExp(arg2, origin, info)
  #=  First argument must be Integer.
  =#
  if ! isInteger(ty1)
    Error.addSourceMessageAndFail(Error.ARG_TYPE_MISMATCH, list("1", toString(fn_ref), "", toString(arg1), Type.toString(ty1), "Integer"), info)
  end
  #=  First argument must be a parameter expression.
  =#
  if var > Variability.PARAMETER
    Error.addSourceMessageAndFail(Error.INVALID_ARGUMENT_VARIABILITY, list("1", toString(fn_ref), P_Prefixes.variabilityString(Variability.PARAMETER), toString(arg1), P_Prefixes.variabilityString(variability)), info)
  end
  #=  Second argument must be Real, array of allowed expressions or record
  =#
  #=  containing only components of allowed expressions.
  =#
  #=  TODO: Also handle records here.
  =#
  @assign (arg2, ty, mk) = matchTypes(ty2, setArrayElementType(ty2, TYPE_REAL()), arg2, true)
  if ! TypeCheck.isValidArgumentMatch(mk)
    Error.addSourceMessageAndFail(Error.ARG_TYPE_MISMATCH, list("2", toString(fn_ref), "", toString(arg2), Type.toString(ty2), "Real\\n  Real[:, ...]\\n  Real record\\n  Real record[:, ...]"), info)
  end
  @match list(fn) = typeRefCache(fn_ref)
  @assign callExp = CALL_EXPRESSION(P_Call.makeTypedCall(fn, list(arg1, arg2), var, ty))
  (callExp, ty, variability)
end

function typeFillCall(call::Call, origin::ORIGIN_Type, info::SourceInfo) ::Tuple{Expression, M_Type, Variability}
  local variability::VariabilityType
  local ty::M_Type
  local callExp::Expression

  local fn_ref::ComponentRef
  local args::List{Expression}
  local named_args::List{NamedArg}
  local fill_arg::Expression

  @match UNTYPED_CALL(ref = fn_ref, arguments = args, named_args = named_args) = call
  assertNoNamedParams("fill", named_args, info)
  #=  fill can take any number of arguments, but needs at least two.
  =#
  if listLength(args) < 2
    Error.addSourceMessageAndFail(Error.NO_MATCHING_FUNCTION_FOUND_NFINST, list(P_Call.toString(call), "fill(Any, Integer, ...) => Any[:, ...]"), info)
  end
  @match _cons(fill_arg, args) = args
  #=  Type the first argument, which is the fill value.
  =#
  @assign (fill_arg, ty, _) = typeExp(fill_arg, origin, info)
  @assign (callExp, ty, variability) = typeFillCall2(fn_ref, ty, fill_arg, args, origin, info)
  (callExp, ty, variability)
end

function typeFillCall2(fnRef::ComponentRef, fillType::M_Type, fillArg::Expression, dimensionArgs::List{<:Expression}, origin::ORIGIN_Type, info::SourceInfo) ::Tuple{Expression, M_Type, Variability}
  local variability::VariabilityType = Variability.CONSTANT
  local ty::M_Type
  local callExp::Expression

  local fill_arg::Expression
  local ty_args::List{Expression}
  local arg_var::VariabilityType
  local arg_ty::M_Type
  local fn::M_Function
  local dims::List{Dimension}
  local evaluated::Bool

  @assign ty_args = list(fillArg)
  @assign dims = nil
  @assign evaluated = true
  #=  Type the dimension arguments.
  =#
  for arg in dimensionArgs
    @assign (arg, arg_ty, arg_var) = typeExp(arg, origin, info)
    if arg_var <= Variability.STRUCTURAL_PARAMETER && ! flagSet(origin, ORIGIN_FUNCTION) && ! containsIterator(arg, origin)
      @assign arg = Ceval.evalExp(arg)
      @assign arg_ty = typeOf(arg)
    else
      @assign evaluated = false
    end
    if ! isInteger(arg_ty)
      Error.addSourceMessageAndFail(Error.ARG_TYPE_MISMATCH, list(intString(listLength(ty_args) + 1), toString(fnRef), "", toString(arg), Type.toString(arg_ty), "Integer"), info)
    end
    @assign variability = variabilityMax(variability, arg_var)
    @assign ty_args = _cons(arg, ty_args)
    @assign dims = _cons(fromExp(arg, arg_var), dims)
  end
  #=  Each dimension argument must be an Integer expression.
  =#
  @assign ty_args = listReverseInPlace(ty_args)
  @assign dims = listReverseInPlace(dims)
  @match list(fn) = typeRefCache(fnRef)
  @assign ty = liftArrayLeftList(fillType, dims)
  if evaluated
    @assign callExp = Ceval.evalBuiltinFill(ty_args)
  else
    @assign callExp = CALL_EXPRESSION(P_Call.makeTypedCall(NFBuiltinFuncs.FILL_FUNC, ty_args, variability, ty))
  end
  (callExp, ty, variability)
end

function typeZerosOnesCall(name::String, call::Call, origin::ORIGIN_Type, info::SourceInfo) ::Tuple{Expression, M_Type, Variability}
  local variability::VariabilityType
  local ty::M_Type
  local callExp::Expression

  local fn_ref::ComponentRef
  local args::List{Expression}
  local named_args::List{NamedArg}
  local fill_arg::Expression

  @match UNTYPED_CALL(ref = fn_ref, arguments = args, named_args = named_args) = call
  assertNoNamedParams(name, named_args, info)
  #=  zeros/ones can take any number of arguments, but needs at least one.
  =#
  if listEmpty(args)
    Error.addSourceMessageAndFail(Error.NO_MATCHING_FUNCTION_FOUND_NFINST, list(P_Call.toString(call), toString(fn_ref) + "(Integer, ...) => Integer[:, ...]"), info)
  end
  @assign fill_arg = INTEGER_EXPRESSION(if name == "ones"
                                        1
                                        else
                                        0
                                        end)
  @assign (callExp, ty, variability) = typeFillCall2(fn_ref, TYPE_INTEGER(), fill_arg, args, origin, info)
  (callExp, ty, variability)
end

function typeScalarCall(call::Call, origin::ORIGIN_Type, info::SourceInfo) ::Tuple{Expression, M_Type, Variability}
  local variability::VariabilityType
  local ty::M_Type
  local callExp::Expression

  local fn_ref::ComponentRef
  local args::List{Expression}
  local named_args::List{NamedArg}
  local arg::Expression
  local fn::M_Function
  local expanded::Bool

  @match UNTYPED_CALL(ref = fn_ref, arguments = args, named_args = named_args) = call
  assertNoNamedParams("scalar", named_args, info)
  if listLength(args) != 1
    Error.addSourceMessageAndFail(Error.NO_MATCHING_FUNCTION_FOUND_NFINST, list(P_Call.toString(call), "scalar(Any[1, ...]) => Any"), info)
  end
  @assign (arg, ty, variability) = typeExp(listHead(args), origin, info)
  #=  scalar requires all dimensions of the array to be 1.
  =#
  for dim in arrayDims(ty)
    if P_Dimension.Dimension.isKnown(dim) && ! P_Dimension.Dimension.size(dim) == 1
      Error.addSourceMessageAndFail(Error.INVALID_ARRAY_DIM_IN_SCALAR_OP, list(Type.toString(ty)), info)
    end
  end
  @assign (arg, expanded) = P_ExpandExp.ExpandExp.expand(arg)
  @assign ty = arrayElementType(ty)
  if expanded
    @assign args = arrayScalarElements(arg)
    if listLength(args) != 1
      Error.assertion(false, getInstanceName() + " failed to expand scalar(" + toString(arg) + ") correctly", info)
    end
    @assign callExp = listHead(args)
  else
    @match list(fn) = typeRefCache(fn_ref)
    @assign callExp = CALL(P_Call.makeTypedCall(fn, list(arg), variability, ty))
  end
  (callExp, ty, variability)
end

function typeVectorCall(call::Call, origin::ORIGIN_Type, info::SourceInfo) ::Tuple{Expression, M_Type, Variability}
  local variability::VariabilityType
  local ty::M_Type
  local callExp::Expression

  local fn_ref::ComponentRef
  local args::List{Expression}
  local named_args::List{NamedArg}
  local arg::Expression
  local var::VariabilityType
  local fn::M_Function
  local vector_dim::Dimension = P_Dimension.Dimension.fromInteger(1)
  local dim_found::Bool = false

  @match UNTYPED_CALL(ref = fn_ref, arguments = args, named_args = named_args) = call
  assertNoNamedParams("vector", named_args, info)
  if listLength(args) != 1
    Error.addSourceMessageAndFail(Error.NO_MATCHING_FUNCTION_FOUND_NFINST, list(P_Call.toString(call), "vector(Any) => Any[:]\\n  vector(Any[:, ...]) => Any[:]"), info)
  end
  @assign (arg, ty, variability) = typeExp(listHead(args), origin, info)
  #=  vector requires that at most one dimension is > 1, and that dimension
  =#
  #=  determines the type of the vector call.
  =#
  for dim in arrayDims(ty)
    if ! P_Dimension.Dimension.isKnown(dim) || P_Dimension.Dimension.size(dim) > 1
      if dim_found
        Error.addSourceMessageAndFail(Error.NF_VECTOR_INVALID_DIMENSIONS, list(Type.toString(ty), P_Call.toString(call)), info)
      else
        @assign vector_dim = dim
        @assign dim_found = true
      end
    end
  end
  @assign ty = ARRAY_TYPE(arrayElementType(ty), list(vector_dim))
  @match list(fn) = typeRefCache(fn_ref)
  @assign callExp = CALL_EXPRESSION(P_Call.makeTypedCall(fn, list(arg), variability, ty))
  (callExp, ty, variability)
end

function typeMatrixCall(call::Call, origin::ORIGIN_Type, info::SourceInfo) ::Tuple{Expression, M_Type, Variability}
  local variability::VariabilityType
  local ty::M_Type
  local callExp::Expression

  local fn_ref::ComponentRef
  local args::List{Expression}
  local named_args::List{NamedArg}
  local arg::Expression
  local var::VariabilityType
  local fn::M_Function
  local dims::List{Dimension}
  local dim1::Dimension
  local dim2::Dimension
  local i::Integer
  local ndims::Integer

  @match UNTYPED_CALL(ref = fn_ref, arguments = args, named_args = named_args) = call
  assertNoNamedParams("matrix", named_args, info)
  if listLength(args) != 1
    Error.addSourceMessageAndFail(Error.NO_MATCHING_FUNCTION_FOUND_NFINST, list(P_Call.toString(call), "vector(Any) => Any[:]\\n  vector(Any[:, ...]) => Any[:]"), info)
  end
  @assign (arg, ty, variability) = typeExp(listHead(args), origin, info)
  @assign dims = arrayDims(ty)
  @assign ndims = listLength(dims)
  if ndims < 2
    @assign (callExp, ty) = promote(arg, ty, 2)
  elseif ndims == 2
    @assign callExp = arg
  else
    @match _cons(dim1, _cons(dim2, dims)) = dims
    @assign i = 3
    for dim in dims
      if P_Dimension.Dimension.isKnown(dim) && P_Dimension.Dimension.size(dim) > 1
        Error.addSourceMessageAndFail(Error.INVALID_ARRAY_DIM_IN_CONVERSION_OP, list(String(i), "matrix", "1", P_Dimension.Dimension.toString(dim)), info)
      end
      @assign i = i + 1
    end
    @assign ty = ARRAY_TYPE(arrayElementType(ty), list(dim1, dim2))
    @match list(fn) = typeRefCache(fn_ref)
    @assign callExp = CALL_EXPRESSION(P_Call.makeTypedCall(fn, list(arg), variability, ty))
  end
  #=  matrix(A) where A is a scalar or vector returns promote(A, 2).
  =#
  #=  matrix(A) where A is a matrix just returns A.
  =#
  #=  matrix requires all but the first two dimensions to have size 1.
  =#
  (callExp, ty, variability)
end

function typeCatCall(call::Call, origin::ORIGIN_Type, info::SourceInfo) ::Tuple{Expression, M_Type, Variability}
  local variability::VariabilityType
  local ty::M_Type
  local callExp::Expression

  local fn_ref::ComponentRef
  local args::List{Expression}
  local res::List{Expression}
  local named_args::List{NamedArg}
  local tys::List{M_Type}
  local arg::Expression
  local var::VariabilityType
  local mk::TypeCheck.MatchKind
  local fn::M_Function
  local n::Integer

  @match UNTYPED_CALL(ref = fn_ref, arguments = args, named_args = named_args) = call
  assertNoNamedParams("cat", named_args, info)
  if listLength(args) < 2
    Error.addSourceMessageAndFail(Error.NO_MATCHING_FUNCTION_FOUND_NFINST, list(P_Call.toString(call), "cat(Integer, Any[:,:], ...) => Any[:]"), info)
  end
  @match _cons(arg, args) = args
  @assign (arg, ty, variability) = typeExp(arg, origin, info)
  @assign (arg, ty, mk) = matchTypes(ty, TYPE_INTEGER(), arg)
  if variability > Variability.PARAMETER
    Error.addSourceMessageAndFail(Error.NF_CAT_FIRST_ARG_EVAL, list(toString(arg), P_Prefixes.variabilityString(variability)), info)
  end
  @match INTEGER_EXPRESSION(n) = Ceval.evalExp(arg, Ceval.P_EvalTarget.GENERIC(info))
  @assign res = nil
  @assign tys = nil
  for a in args
    @assign (arg, ty, var) = typeExp(a, origin, info)
    @assign variability = variabilityMax(var, variability)
    @assign res = _cons(arg, res)
    @assign tys = _cons(ty, tys)
  end
  @assign (callExp, ty) = makeCatExp(n, listReverse(res), listReverse(tys), variability, info)
  (callExp, ty, variability)
end

function typeSymmetricCall(call::Call, origin::ORIGIN_Type, info::SourceInfo) ::Tuple{Expression, M_Type, Variability}
  local variability::VariabilityType
  local ty::M_Type
  local callExp::Expression

  local fn_ref::ComponentRef
  local args::List{Expression}
  local named_args::List{NamedArg}
  local arg::Expression
  local fn::M_Function

  @match UNTYPED_CALL(ref = fn_ref, arguments = args, named_args = named_args) = call
  assertNoNamedParams("symmetric", named_args, info)
  if listLength(args) != 1
    Error.addSourceMessageAndFail(Error.NO_MATCHING_FUNCTION_FOUND_NFINST, list(P_Call.toString(call), "symmetric(Any[n, n]) => Any[n, n]"), info)
  end
  @assign (arg, ty, variability) = typeExp(listHead(args), origin, info)
  if ! Type.isSquareMatrix(ty)
    Error.addSourceMessageAndFail(Error.ARG_TYPE_MISMATCH, list("1", toString(fn_ref), "", toString(arg), Type.toString(ty), "Any[n, n]"), info)
  end
  @match list(fn) = typeRefCache(fn_ref)
  @assign callExp = CALL_EXPRESSION(P_Call.makeTypedCall(fn, list(arg), variability, ty))
  (callExp, ty, variability)
end

function typeTransposeCall(call::Call, origin::ORIGIN_Type, info::SourceInfo) ::Tuple{Expression, M_Type, Variability}
  local variability::VariabilityType
  local ty::M_Type
  local callExp::Expression

  local fn_ref::ComponentRef
  local args::List{Expression}
  local named_args::List{NamedArg}
  local arg::Expression
  local dim1::Dimension
  local dim2::Dimension
  local rest_dims::List{Dimension}
  local fn::M_Function

  @match UNTYPED_CALL(ref = fn_ref, arguments = args, named_args = named_args) = call
  assertNoNamedParams("transpose", named_args, info)
  if listLength(args) != 1
    Error.addSourceMessageAndFail(Error.NO_MATCHING_FUNCTION_FOUND_NFINST, list(P_Call.toString(call), "transpose(Any[n, m, ...]) => Any[m, n, ...]"), info)
  end
  @assign (arg, ty, variability) = typeExp(listHead(args), origin, info)
  @assign ty = begin
    @match ty begin
      ARRAY_TYPE(dimensions = dim1 <| dim2 <| rest_dims)  => begin
        ARRAY_TYPE(ty.elementType, _cons(dim2, _cons(dim1, rest_dims)))
      end

      _  => begin
        Error.addSourceMessage(Error.ARG_TYPE_MISMATCH, list("1", toString(fn_ref), "", toString(arg), Type.toString(ty), "Any[:, :, ...]"), info)
        fail()
      end
    end
  end
  @match list(fn) = typeRefCache(fn_ref)
  @assign callExp = CALL_EXPRESSION(P_Call.makeTypedCall(fn, list(arg), variability, ty))
  (callExp, ty, variability)
end

function typeCardinalityCall(call::Call, origin::ORIGIN_Type, info::SourceInfo) ::Tuple{Expression, M_Type, Variability}
  local var::VariabilityType = Variability.PARAMETER
  local ty::M_Type
  local callExp::Expression

  local fn_ref::ComponentRef
  local args::List{Expression}
  local named_args::List{NamedArg}
  local arg::Expression
  local fn::M_Function
  local node::InstNode

  #=  cardinality may only be used in a condition of an assert or
  =#
  #=  if-statement/equation (the specification says only if-statement,
  =#
  #=  but e.g. the MSL only uses them in if-equations and asserts).
  =#
  if ! (flagSet(origin, ExpOrigin.CONDITION) && (flagSet(origin, ExpOrigin.IF) || flagSet(origin, ExpOrigin.ASSERT)))
    Error.addSourceMessageAndFail(Error.INVALID_CARDINALITY_CONTEXT, nil, info)
  end
  @match UNTYPED_CALL(ref = fn_ref, arguments = args, named_args = named_args) = call
  assertNoNamedParams("cardinality", named_args, info)
  if listLength(args) != 1
    Error.addSourceMessageAndFail(Error.NO_MATCHING_FUNCTION_FOUND_NFINST, list(P_Call.toString(call), toString(fn_ref) + "(Connector) => Integer"), info)
  end
  if flagSet(origin, ORIGIN_FUNCTION)
    Error.addSourceMessageAndFail(Error.EXP_INVALID_IN_FUNCTION, list(toString(fn_ref)), info)
  end
  @assign (arg, ty) = typeExp(listHead(args), origin, info)
  if ! isCref(arg)
    Error.addSourceMessageAndFail(Error.ARGUMENT_MUST_BE_VARIABLE, list("First", toString(fn_ref), "<REMOVE ME>"), info)
  end
  @assign node = node(toCref(arg))
  if ! (Type.isScalar(ty) && isComponent(node) && P_Component.isConnector(component(node)))
    Error.addSourceMessageAndFail(Error.ARG_TYPE_MISMATCH, list("1", toString(fn_ref), "", toString(arg), Type.toString(ty), "connector"), info)
  end
  @match list(fn) = typeRefCache(fn_ref)
  @assign ty = TYPE_INTEGER()
  @assign callExp = CALL_EXPRESSION(P_Call.makeTypedCall(fn, list(arg), var, ty))
  #=  TODO: Check cardinality restrictions, 3.7.2.3.
  =#
  System.setUsesCardinality(true)
  (callExp, ty, var)
end

function typeBranchCall(call::Call, origin::ORIGIN_Type, info::SourceInfo) ::Tuple{Expression, M_Type, Variability}
  local var::VariabilityType = Variability.PARAMETER
  local ty::M_Type
  local callExp::Expression

  local fn_ref::ComponentRef
  local args::List{Expression}
  local named_args::List{NamedArg}
  local arg1::Expression
  local arg2::Expression
  local fn::M_Function

  @match UNTYPED_CALL(ref = fn_ref, arguments = args, named_args = named_args) = call
  assertNoNamedParams("Connections.branch", named_args, info)
  if listLength(args) != 2
    Error.addSourceMessageAndFail(Error.NO_MATCHING_FUNCTION_FOUND_NFINST, list(P_Call.toString(call), toString(fn_ref) + "(Connector, Connector)"), info)
  end
  if flagSet(origin, ORIGIN_FUNCTION)
    Error.addSourceMessageAndFail(Error.EXP_INVALID_IN_FUNCTION, list(toString(fn_ref)), info)
  end
  @match list(arg1, arg2) = args
  @assign (arg1, ty) = typeExp(arg1, origin, info)
  checkConnectionsArgument(arg1, ty, fn_ref, 1, info)
  @assign (arg2, ty) = typeExp(arg2, origin, info)
  checkConnectionsArgument(arg2, ty, fn_ref, 2, info)
  @match list(fn) = typeRefCache(fn_ref)
  @assign ty = TYPE_NORETCALL()
  @assign callExp = CALL_EXPRESSION(P_Call.makeTypedCall(fn, list(arg1, arg2), var, ty))
  (callExp, ty, var)
end

function typeIsRootCall(call::Call, origin::ORIGIN_Type, info::SourceInfo) ::Tuple{Expression, M_Type, Variability}
  local var::VariabilityType = Variability.PARAMETER
  local ty::M_Type
  local callExp::Expression

  local fn_ref::ComponentRef
  local args::List{Expression}
  local named_args::List{NamedArg}
  local arg::Expression
  local fn::M_Function

  @match UNTYPED_CALL(ref = fn_ref, arguments = args, named_args = named_args) = call
  assertNoNamedParams("Connections.isRoot", named_args, info)
  if listLength(args) != 1
    Error.addSourceMessageAndFail(Error.NO_MATCHING_FUNCTION_FOUND_NFINST, list(P_Call.toString(call), toString(fn_ref) + "(Connector)"), info)
  end
  if flagSet(origin, ORIGIN_FUNCTION)
    Error.addSourceMessageAndFail(Error.EXP_INVALID_IN_FUNCTION, list(toString(fn_ref)), info)
  end
  @assign (arg, ty) = typeExp(listHead(args), origin, info)
  checkConnectionsArgument(arg, ty, fn_ref, 1, info)
  @match list(fn) = typeRefCache(fn_ref)
  @assign ty = TYPE_BOOLEAN()
  @assign callExp = CALL_EXPRESSION(P_Call.makeTypedCall(fn, list(arg), var, ty))
  (callExp, ty, var)
end

function typePotentialRootCall(call::Call, origin::ORIGIN_Type, info::SourceInfo) ::Tuple{Expression, M_Type, Variability}
  local var::VariabilityType = Variability.PARAMETER
  local ty::M_Type
  local callExp::Expression

  local fn_ref::ComponentRef
  local args::List{Expression}
  local named_args::List{NamedArg}
  local arg1::Expression
  local arg2::Expression
  local fn::M_Function
  local args_len::Integer
  local name::String

  @match UNTYPED_CALL(ref = fn_ref, arguments = args, named_args = named_args) = call
  for narg in named_args
    @assign (name, arg2) = narg
    if name == "priority"
      @assign args = ListUtil.appendElt(arg2, args)
    else
      Error.addSourceMessageAndFail(Error.NO_SUCH_PARAMETER, list(toString(fn_ref), name), info)
    end
  end
  @assign args_len = listLength(args)
  if args_len < 1 || args_len > 2
    Error.addSourceMessageAndFail(Error.NO_MATCHING_FUNCTION_FOUND_NFINST, list(P_Call.toString(call), toString(fn_ref) + "(Connector, Integer = 0)"), info)
  end
  if flagSet(origin, ORIGIN_FUNCTION)
    Error.addSourceMessageAndFail(Error.EXP_INVALID_IN_FUNCTION, list(toString(fn_ref)), info)
  end
  @match _cons(arg1, args) = args
  @assign (arg1, ty) = typeExp(arg1, origin, info)
  checkConnectionsArgument(arg1, ty, fn_ref, 1, info)
  if args_len == 2
    @assign arg2 = listHead(args)
    @assign (arg2, ty) = typeExp(arg2, origin, info)
    if ! isInteger(ty)
      Error.addSourceMessageAndFail(Error.ARG_TYPE_MISMATCH, list("2", toString(fn_ref), "", toString(arg2), Type.toString(ty), "Integer"), info)
    end
  else
    @assign arg2 = INTEGER_EXPRESSION(0)
  end
  @match list(fn) = typeRefCache(fn_ref)
  @assign ty = TYPE_NORETCALL()
  @assign callExp = CALL_EXPRESSION(P_Call.makeTypedCall(fn, list(arg1, arg2), var, ty))
  (callExp, ty, var)
end

function typeRootCall(call::Call, origin::ORIGIN_Type, info::SourceInfo) ::Tuple{Expression, M_Type, Variability}
  local var::VariabilityType = Variability.PARAMETER
  local ty::M_Type
  local callExp::Expression

  local fn_ref::ComponentRef
  local args::List{Expression}
  local named_args::List{NamedArg}
  local arg::Expression
  local fn::M_Function

  @match UNTYPED_CALL(ref = fn_ref, arguments = args, named_args = named_args) = call
  assertNoNamedParams("Connections.root", named_args, info)
  if listLength(args) != 1
    Error.addSourceMessageAndFail(Error.NO_MATCHING_FUNCTION_FOUND_NFINST, list(P_Call.toString(call), toString(fn_ref) + "(Connector)"), info)
  end
  if flagSet(origin, ORIGIN_FUNCTION)
    Error.addSourceMessageAndFail(Error.EXP_INVALID_IN_FUNCTION, list(toString(fn_ref)), info)
  end
  @assign (arg, ty) = typeExp(listHead(args), origin, info)
  checkConnectionsArgument(arg, ty, fn_ref, 1, info)
  @match list(fn) = typeRefCache(fn_ref)
  @assign ty = TYPE_NORETCALL()
  @assign callExp = CALL_EXPRESSION(P_Call.makeTypedCall(fn, list(arg), var, ty))
  (callExp, ty, var)
end

function typeRootedCall(call::Call, origin::ORIGIN_Type, info::SourceInfo) ::Tuple{Expression, M_Type, Variability}
  local var::VariabilityType = Variability.PARAMETER
  local ty::M_Type
  local callExp::Expression

  local fn_ref::ComponentRef
  local args::List{Expression}
  local named_args::List{NamedArg}
  local arg::Expression
  local fn::M_Function

  @match UNTYPED_CALL(ref = fn_ref, arguments = args, named_args = named_args) = call
  assertNoNamedParams("Connections.rooted", named_args, info)
  if listLength(args) != 1
    Error.addSourceMessageAndFail(Error.NO_MATCHING_FUNCTION_FOUND_NFINST, list(P_Call.toString(call), toString(fn_ref) + "(Connector)"), info)
  end
  if flagSet(origin, ORIGIN_FUNCTION)
    Error.addSourceMessageAndFail(Error.EXP_INVALID_IN_FUNCTION, list(toString(fn_ref)), info)
  end
  @assign (arg, ty) = typeExp(listHead(args), origin, info)
  checkConnectionsArgument(arg, ty, fn_ref, 1, info)
  if isSimple(fn_ref)
    Error.addSourceMessage(Error.DEPRECATED_API_CALL, list("rooted", "Connections.rooted"), info)
  end
  @match list(fn) = typeRefCache(fn_ref)
  @assign ty = TYPE_BOOLEAN()
  @assign callExp = CALL_EXPRESSION(P_Call.makeTypedCall(fn, list(arg), var, ty))
  (callExp, ty, var)
end

""" #= see also typeUniqueRootIndicesCall =#"""
function typeUniqueRootCall(call::Call, origin::ORIGIN_Type, info::SourceInfo) ::Tuple{Expression, M_Type, Variability}
  local var::VariabilityType = Variability.PARAMETER
  local ty::M_Type
  local callExp::Expression

  local fn_ref::ComponentRef
  local args::List{Expression}
  local named_args::List{NamedArg}
  local arg1::Expression
  local arg2::Expression
  local fn::M_Function
  local args_len::Integer
  local name::String

  Error.addSourceMessage(Error.NON_STANDARD_OPERATOR, list("Connections.uniqueRoot"), info)
  @match UNTYPED_CALL(ref = fn_ref, arguments = args, named_args = named_args) = call
  for narg in named_args
    @assign (name, arg2) = narg
    if name == "message"
      @assign args = ListUtil.appendElt(arg2, args)
    else
      Error.addSourceMessageAndFail(Error.NO_SUCH_PARAMETER, list(toString(fn_ref), name), info)
    end
  end
  @assign args_len = listLength(args)
  if args_len < 1 || args_len > 2
    Error.addSourceMessageAndFail(Error.NO_MATCHING_FUNCTION_FOUND_NFINST, list(P_Call.toString(call), toString(fn_ref) + "(Connector, String = \\\\)"), info)
  end
  if flagSet(origin, ORIGIN_FUNCTION)
    Error.addSourceMessageAndFail(Error.EXP_INVALID_IN_FUNCTION, list(toString(fn_ref)), info)
  end
  @match _cons(arg1, args) = args
  @assign (arg1, ty) = typeExp(arg1, origin, info)
  checkConnectionsArgument(arg1, ty, fn_ref, 1, info)
  if args_len == 2
    @assign arg2 = listHead(args)
    @assign (arg2, ty) = typeExp(arg2, origin, info)
    if ! Type.isString(ty)
      Error.addSourceMessageAndFail(Error.ARG_TYPE_MISMATCH, list("2", toString(fn_ref), "", toString(arg2), Type.toString(ty), "String"), info)
    end
  else
    @assign arg2 = STRING_EXPRESSION("")
  end
  @match list(fn) = typeRefCache(fn_ref)
  @assign ty = TYPE_NORETCALL()
  @assign callExp = CALL_EXPRESSION(P_Call.makeTypedCall(fn, list(arg1, arg2), var, ty))
  (callExp, ty, var)
end

""" #= See Modelica_StateGraph2:
              https:github.com/modelica/Modelica_StateGraph2
              and
              https:trac.modelica.org/Modelica/ticket/984
              and
              http:www.ep.liu.se/ecp/043/041/ecp09430108.pdf
              for a specification of this operator =#"""
                function typeUniqueRootIndicesCall(call::Call, origin::ORIGIN_Type, info::SourceInfo) ::Tuple{Expression, M_Type, Variability}
                  local var::VariabilityType = Variability.PARAMETER
                  local ty::M_Type
                  local callExp::Expression

                  local fn_ref::ComponentRef
                  local args::List{Expression}
                  local named_args::List{NamedArg}
                  local arg1::Expression
                  local arg2::Expression
                  local arg3::Expression
                  local fn::M_Function
                  local args_len::Integer
                  local name::String
                  local ty1::M_Type
                  local ty2::M_Type
                  local ty3::M_Type

#                  Error.addSourceMessage(Error.NON_STANDARD_OPERATOR, list("Connections.uniqueRootIndices"), info) TODO
                  @match UNTYPED_CALL(ref = fn_ref, arguments = args, named_args = named_args) = call
                  for narg in named_args
                    @assign (name, arg3) = narg
                    if name == "message"
                      @assign args = ListUtil.appendElt(arg3, args)
                    else
#                      Error.addSourceMessageAndFail(Error.NO_SUCH_PARAMETER, list(toString(fn_ref), name), info) TODO
                    end
                  end
                  @assign args_len = listLength(args)
                  if args_len < 2 || args_len > 3
                    Error.addSourceMessageAndFail(Error.NO_MATCHING_FUNCTION_FOUND_NFINST, list(P_Call.toString(call), toString(fn_ref) + "(Connector, Connector, String = \\\\)"), info)
                  end
                  if flagSet(origin, ORIGIN_FUNCTION)
                    Error.addSourceMessageAndFail(Error.EXP_INVALID_IN_FUNCTION, list(toString(fn_ref)), info)
                  end
                  @match _cons(arg1, _cons(arg2, args)) = args
                  @assign (arg1, ty1) = typeExp(arg1, origin, info)
                  checkConnectionsArgument(arg1, ty1, fn_ref, 1, info)
                  @assign (arg2, ty2) = typeExp(arg2, origin, info)
                  checkConnectionsArgument(arg2, ty2, fn_ref, 1, info)
                  if args_len == 3
                    @assign arg3 = listHead(args)
                    @assign (arg3, ty3) = typeExp(arg3, origin, info)
                    if ! Type.isString(ty3)
                      Error.addSourceMessageAndFail(Error.ARG_TYPE_MISMATCH, list("3", toString(fn_ref), "", toString(arg2), Type.toString(ty3), "String"), info)
                    end
                  else
                    @assign arg2 = STRING_EXPRESSION("")
                  end
                  @match list(fn) = typeRefCache(fn_ref)
                  assert(listLength(arrayDims(ty1)) == listLength(arrayDims(ty2)), "the first two parameters need to have the same size")
                  @assign ty = ARRAY_TYPE(Type.TYPE_INTEGER(), arrayDims(ty1))
                  @assign callExp = CALL_EXPRESSION(P_Call.makeTypedCall(fn, list(arg1, arg2), var, ty))
                  (callExp, ty, var)
                end

function checkConnectionsArgument(arg::Expression, ty::M_Type, fnRef::ComponentRef, argIndex::Integer, info::SourceInfo)
  @assign () = begin
    local ty2::M_Type
    local node::InstNode
    local valid_cref::Bool
    local isConnector::Bool
    @match arg begin
      CREF_EXPRESSION(__)  => begin
        @assign (valid_cref, isConnector) = begin
          @match arg.cref begin
            CREF(node = node, origin = P_NFComponentRef.Origin.CREF, restCref = CREF(ty = ty2, origin = P_NFComponentRef.Origin.CREF))  => begin
              #=  check form A.R
              =#
              @assign ty2 = begin
                @match ty2 begin
                  ARRAY_TYPE(__) where (listLength(subscriptsAllFlat(arg.cref)) == listLength(ty2.dimensions))  => begin
                    ty2.elementType
                  end

                  _  => begin
                    ty2
                  end
                end
              end
              (isOverdetermined(getClass(node)), Type.isConnector(ty2))
            end

            CREF(node = node, ty = ty2)  => begin
              #=  adrpo #5821, allow for R only instead of A.R and issue a warning
              =#
              @assign ty2 = begin
                @match ty2 begin
                  ARRAY_TYPE(__) where (listLength(subscriptsAllFlat(arg.cref)) == listLength(ty2.dimensions))  => begin
                    ty2.elementType
                  end

                  _  => begin
                    ty2
                  end
                end
              end
              (isOverdetermined(getClass(node)), Type.isConnector(ty2))
            end

            _  => begin
              (false, false)
            end
          end
        end
        if ! (valid_cref && isConnector)
          if valid_cref
            Error.addSourceMessage(if argIndex == 1
                                   Error.W_INVALID_ARGUMENT_TYPE_BRANCH_FIRST
                                   else
                                   Error.W_INVALID_ARGUMENT_TYPE_BRANCH_SECOND
                                   end, list(toString(arg.cref), toString(fnRef)), info)
          else
            Error.addSourceMessageAndFail(if argIndex == 1
                                          Error.INVALID_ARGUMENT_TYPE_BRANCH_FIRST
                                          else
                                          Error.INVALID_ARGUMENT_TYPE_BRANCH_SECOND
                                          end, list(toString(arg.cref), toString(fnRef)), info)
          end
        end
        ()
      end

      _  => begin
        Error.addSourceMessage(Error.ARG_TYPE_MISMATCH, list(String(argIndex), toString(fnRef), "", toString(arg), Type.toString(ty), "overconstrained type/record"), info)
        fail()
      end
    end
  end
end

function typeNoEventCall(call::Call, origin::ORIGIN_Type, info::SourceInfo) ::Tuple{Expression, M_Type, Variability}
  local variability::VariabilityType
  local ty::M_Type
  local callExp::Expression

  local fn_ref::ComponentRef
  local args::List{Expression}
  local named_args::List{NamedArg}
  local arg::Expression
  local fn::M_Function

  @match UNTYPED_CALL(ref = fn_ref, arguments = args, named_args = named_args) = call
  assertNoNamedParams("noEvent", named_args, info)
  #=  noEvent takes exactly one argument.
  =#
  if listLength(args) != 1
    Error.addSourceMessageAndFail(Error.NO_MATCHING_FUNCTION_FOUND_NFINST, list(P_Call.toString(call), "noEvent(Any) => Any"), info)
  end
  @match list(arg) = args
  @assign (arg, ty, variability) = typeExp(arg, setFlag(origin, ORIGIN_NOEVENT), info)
  @match list(fn) = typeRefCache(fn_ref)
  @assign callExp = CALL_EXPRESSION(P_Call.makeTypedCall(fn, list(arg), variability, ty))
  (callExp, ty, variability)
end

function typeGetInstanceName(call::Call) ::Tuple{Expression, M_Type, Variability}
  local var::VariabilityType = Variability.CONSTANT
  local ty::M_Type = TYPE_STRING()
  local result::Expression

  local scope::InstNode

  @match UNTYPED_CALL(call_scope = scope) = call
  @assign result = STRING_EXPRESSION(AbsynUtil.pathString(scopePath(scope, includeRoot = true)))
  (result, ty, var)
end

function typeClockCall(call::Call, origin::ORIGIN_Type, info::SourceInfo) ::Tuple{Expression, M_Type, Variability}
  local var::VariabilityType = Variability.PARAMETER
  local outType::M_Type = TYPE_CLOCK()
  local callExp::Expression

  local ty_call::Call
  local args::List{Expression}
  local args_count::Integer
  local e1::Expression
  local e2::Expression

  @match P_Call.TYPED_CALL(arguments = args) = P_Call.typeMatchNormalCall(call, origin, info)
  @assign args_count = listLength(args)
  @assign callExp = begin
    @match args begin
      nil()  => begin
        CLKCONST(P_Expression.P_ClockKind.Expression.INFERRED_CLOCK())
      end

      e1 <|  nil()  => begin
        CLKCONST(P_Expression.P_ClockKind.REAL_EXPRESSION_CLOCK(e1))
      end

      e1 <| e2 <|  nil()  => begin
        #=  Clock() - inferred clock.
        =#
        #=  Clock(interval) - real clock.
        =#
        @assign e2 = Ceval.evalExp(e2)
        @assign callExp = begin
          @match typeOf(e2) begin
            TYPE_INTEGER(__)  => begin
              #=  Clock(intervalCounter, resolution) - integer clock.
              =#
              Error.assertionOrAddSourceMessage(integerValue(e2) >= 1, Error.WRONG_VALUE_OF_ARG, list("Clock", "resolution", toString(e2), "=> 1"), info)
              CLKCONST(INTEGER_EXPRESSION_CLOCK(e1, e2))
            end

            TYPE_REAL(__)  => begin
              CLKCONST(BOOLEAN_EXPRESSION_CLOCK(e1, e2))
            end

            TYPE_STRING(__)  => begin
              CLKCONST(SOLVER_CLOCK(e1, e2))
            end
          end
        end
        #=  Clock(condition, startInterval) - boolean clock.
        =#
        #=  Clock(c, solverMethod) - solver clock.
        =#
        callExp
      end
    end
  end
  (callExp, outType, var)
end

function typeSampleCall(call::Call, origin::ORIGIN_Type, info::SourceInfo) ::Tuple{Expression, M_Type, Variability}
  local var::VariabilityType
  local outType::M_Type
  local callExp::Expression

  local ty_call::Call
  local arg_ty::M_Type
  local args::List{TypedArg}
  local namedArgs::List{TypedNamedArg}
  local e::Expression
  local e1::Expression
  local e2::Expression
  local t::M_Type
  local t1::M_Type
  local t2::M_Type
  local v::VariabilityType
  local v1::VariabilityType
  local v2::VariabilityType
  local fn_ref::ComponentRef
  local normalSample::M_Function
  local clockedSample::M_Function
  local recopnode::InstNode

  @match ARG_TYPED_CALL(fn_ref, args, namedArgs) = P_Call.typeNormalCall(call, origin, info)
  @assign recopnode = node(fn_ref)
  @assign fn_ref = instFunctionRef(fn_ref, InstNode_info(recopnode))
  @match list(normalSample, clockedSample) = typeRefCache(fn_ref)
  @assign (callExp, outType, var) = begin
    @match (args, namedArgs) begin
      ((e, t, v) <| (e1, TYPE_INTEGER(__), v1) <|  nil(),  nil())  => begin
        #=  sample(start, Real interval) - the usual stuff
        =#
        if valueEq(t, TYPE_INTEGER())
          @assign e = CAST_EXPRESSION(TYPE_REAL(), e)
        end
        @assign ty_call = P_Call.makeTypedCall(normalSample, list(e, CAST_EXPRESSION(TYPE_REAL(), e1)), Variability.PARAMETER, TYPE_BOOLEAN())
        (CALL_EXPRESSION(ty_call), TYPE_BOOLEAN(), Variability.PARAMETER)
      end

      ((e, t, v) <| (e1, TYPE_REAL(__), v1) <|  nil(),  nil())  => begin
        #=  sample(start, Real interval) - the usual stuff
        =#
        if valueEq(t, TYPE_INTEGER())
          @assign e = CAST_EXPRESSION(TYPE_REAL(), e)
        end
        @assign ty_call = P_Call.makeTypedCall(normalSample, list(e, e1), Variability.PARAMETER, TYPE_BOOLEAN())
        (CALL_EXPRESSION(ty_call), TYPE_BOOLEAN(), Variability.PARAMETER)
      end

      ((e, t, v) <|  nil(), ("interval", e1, TYPE_REAL(__), v1) <|  nil())  => begin
        #=  sample(start, Real interval = value) - the usual stuff
        =#
        if valueEq(t, TYPE_INTEGER())
          @assign e = CAST_EXPRESSION(TYPE_REAL(), e)
        end
        @assign ty_call = P_Call.makeTypedCall(normalSample, list(e, e1), Variability.PARAMETER, TYPE_BOOLEAN())
        (CALL_EXPRESSION(ty_call), TYPE_BOOLEAN(), Variability.PARAMETER)
      end

      ((e, t, v) <|  nil(),  nil()) where (Config.synchronousFeaturesAllowed())  => begin
        #=  sample(u) - inferred clock
        =#
        @assign ty_call = P_Call.makeTypedCall(clockedSample, list(e, CLKCONST(P_Expression.P_ClockKind.Expression.INFERRED_CLOCK())), v, t)
        (CALL_EXPRESSION(ty_call), t, v)
      end

      ((e, t, v) <| (e1, TYPE_CLOCK(__), v1) <|  nil(),  nil()) where (Config.synchronousFeaturesAllowed())  => begin
        #=  sample(u, c) - specified clock
        =#
        @assign ty_call = P_Call.makeTypedCall(clockedSample, list(e, e1), v, t)
        (CALL_EXPRESSION(ty_call), t, v)
      end

      ((e, t, v) <|  nil(), ("c", e1, TYPE_CLOCK(__), v1) <|  nil()) where (Config.synchronousFeaturesAllowed())  => begin
        #=  sample(u, Clock c = c) - specified clock
        =#
        @assign ty_call = P_Call.makeTypedCall(clockedSample, list(e, e1), v, t)
        (CALL_EXPRESSION(ty_call), t, v)
      end

      _  => begin
        Error.addSourceMessage(Error.WRONG_TYPE_OR_NO_OF_ARGS, list(P_Call.toString(call), "<NO COMPONENT>"), info)
        fail()
      end
    end
  end
  (callExp, outType, var)
end

function typeActualInStreamCall(name::String, call::Call, origin::ORIGIN_Type, info::SourceInfo) ::Tuple{Expression, M_Type, Variability}
  local variability::VariabilityType = Variability.DISCRETE
  local ty::M_Type
  local callExp::Expression

  local fn_ref::ComponentRef
  local arg_ref::ComponentRef
  local args::List{Expression}
  local named_args::List{NamedArg}
  local arg::Expression
  local var::VariabilityType
  local fn::M_Function
  local arg_node::InstNode

  @match UNTYPED_CALL(ref = fn_ref, arguments = args, named_args = named_args) = call
  assertNoNamedParams(name, named_args, info)
  if listLength(args) != 1
    Error.addSourceMessageAndFail(Error.NO_MATCHING_FUNCTION_FOUND_NFINST, list(P_Call.toString(call), toString(fn_ref) + "(stream variable) => Real"), info)
  end
  @assign (arg, ty, var) = typeExp(listHead(args), origin, info)
  @assign arg = P_ExpandExp.ExpandExp.expand(arg)
  @match list(fn) = typeRefCache(fn_ref)
  @assign callExp = typeActualInStreamCall2(name, fn, arg, var, info)
  (callExp, ty, variability)
end

function typeActualInStreamCall2(name::String, fn::M_Function, arg::Expression, var::VariabilityType, info::SourceInfo) ::Expression
  local callExp::Expression

  @assign callExp = begin
    local arg_node::InstNode
    @match arg begin
      CREF_EXPRESSION(__)  => begin
        @assign arg_node = node(arg.cref)
        #=  The argument of actualStream/inStream must be a stream variable.
        =#
        if ! isComponent(arg_node) || ! ConnectorType.isStream(P_Component.connectorType(component(arg_node)))
          Error.addSourceMessageAndFail(Error.NON_STREAM_OPERAND_IN_STREAM_OPERATOR, list(toString(arg.cref), name), info)
        end
        #=  The argument of actualStream/inStream must have subscripts that can be evaluated.
        =#
        for sub in subscriptsAllFlat(arg.cref)
          if variability(sub) > Variability.PARAMETER
            Error.addSourceMessageAndFail(Error.CONNECTOR_NON_PARAMETER_SUBSCRIPT, list(toString(arg.cref), toString(sub)), info)
          end
        end
        CALL_EXPRESSION(P_Call.makeTypedCall(fn, list(arg), var, arg.ty))
      end

      ARRAY_EXPRESSION(__)  => begin
        @assign arg.elements = list(typeActualInStreamCall2(name, fn, e, var, info) for e in arg.elements)
        arg
      end

      _  => begin
        Error.addSourceMessage(Error.NON_STREAM_OPERAND_IN_STREAM_OPERATOR, list(toString(arg), name), info)
        fail()
      end
    end
  end
  callExp
end

function typeDynamicSelectCall(name::String, call::Call, origin::ORIGIN_Type, info::SourceInfo) ::Tuple{Expression, M_Type, Variability}
  local variability::VariabilityType = Variability.CONTINUOUS
  local ty::M_Type
  local callExp::Expression

  local fn_ref::ComponentRef
  local arg_ref::ComponentRef
  local args::List{Expression}
  local named_args::List{NamedArg}
  local arg1::Expression
  local arg2::Expression
  local var1::VariabilityType
  local var2::VariabilityType
  local fn::M_Function
  local arg_node::InstNode
  local ty1::M_Type
  local ty2::M_Type
  local expStatic::Expression
  local expDynamic::Expression

  @match UNTYPED_CALL(ref = fn_ref, arguments = args, named_args = named_args) = call
  assertNoNamedParams(name, named_args, info)
  if listLength(args) != 2
    Error.addSourceMessageAndFail(Error.NO_MATCHING_FUNCTION_FOUND_NFINST, list(P_Call.toString(call), toString(fn_ref) + "(static expression, dynamic expression)"), info)
  end
  @match list(expStatic, expDynamic) = list(unbox(arg) for arg in args)
  @assign (arg1, ty1, var1) = typeExp(expStatic, origin, info)
  @assign arg1 = P_ExpandExp.ExpandExp.expand(arg1)
  #=  if we cannot typecheck the dynamic part, ignore it!
  =#
  #=  https:trac.openmodelica.org/OpenModelica/ticket/5631
  =#
  try
    @assign (arg2, ty2, var2) = typeExp(expDynamic, origin, info)
  catch e
    @error "DBG error: $e"
    @assign variability = var1
    @assign callExp = arg1
    return (callExp, ty, variability)
  end
  @assign arg2 = P_ExpandExp.ExpandExp.expand(arg2)
  @assign ty = ty1
  @assign variability = var2
  @match list(fn) = typeRefCache(fn_ref)
  if Flags.isSet(Flags.NF_API_DYNAMIC_SELECT)
    @assign callExp = CALL_EXPRESSION(P_Call.makeTypedCall(fn, list(arg1, arg2), variability, ty1))
  else
    @assign variability = var1
    @assign callExp = arg1
  end
  (callExp, ty, variability)
end
