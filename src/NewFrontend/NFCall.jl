module ParameterTreeImpl

using MetaModelica
using ExportAll
import ..Main.Expression
import ..Main.toString

#= Modelica extend clause =#
const Key = String
const Value = Expression

#= Define the printing functions. =#
function keyStr(key)
  return key
end

include("../Util/baseAvlTreeCode.jl")
addConflictDefault = addConflictReplace
#= Add comp function =#
keyCompare = (inKey1::Key, inKey2::Key) -> begin
  #Should it default to ==== instead since types are in many cases immutable?
  res = stringCompare(inKey1, inKey2)
  return res
end

end #=ParameterTreeImpl=#

const ParameterTree = ParameterTreeImpl.Tree

@Uniontype Call begin
  @Record TYPED_REDUCTION begin
    fn::M_Function
    ty::NFType
    var::VariabilityType
    exp::Expression
    iters::List{Tuple{InstNode, Expression}}
    defaultExp::Option{Expression}
    foldExp::Tuple{Option{Expression}, String, String}
  end

  @Record UNTYPED_REDUCTION begin
    ref::ComponentRef
    exp::Expression
    iters::List{Tuple{InstNode, Expression}}
  end

  @Record TYPED_ARRAY_CONSTRUCTOR begin
    ty::NFType
    var::VariabilityType
    exp::Expression
    iters::List{Tuple{InstNode, Expression}}
  end

  @Record UNTYPED_ARRAY_CONSTRUCTOR begin
    exp::Expression
    iters::List{Tuple{InstNode, Expression}}
  end

  @Record TYPED_CALL begin
    fn::M_Function
    ty::NFType
    var::VariabilityType
    arguments::List{Expression}
    attributes::CallAttributes
  end

  @Record ARG_TYPED_CALL begin
    ref::ComponentRef
    arguments::List{TypedArg}
    named_args::List{TypedNamedArg}
    call_scope::InstNode
  end

  @Record UNTYPED_CALL begin
    ref::ComponentRef
    arguments::List{Expression}
    named_args::List{NamedArg}
    call_scope::InstNode
  end
end

@UniontypeDecl CallAttributes

function toDAE(attr::CallAttributes, returnType::NFType)::DAE.CallAttributes
  local fattr::DAE.CallAttributes
   fattr = DAE.CALL_ATTR(
    toDAE(returnType),
    attr.tuple_,
    attr.builtin,
    attr.isImpure,
    attr.isFunctionPointerCall,
    attr.inlineType,
    attr.tailCall,
  )
  return fattr
end

@Uniontype CallAttributes begin
  @Record CALL_ATTR begin
    tuple_::Bool #= tuple =#
    builtin::Bool #= builtin Function call =#
    isImpure::Bool #= if the function has prefix *impure* is true, else false =#
    isFunctionPointerCall::Bool
    inlineType::DAE.InlineType
    tailCall::DAE.TailCall #= Input variables of the function if the call is tail-recursive =#
  end
end

function typeCast(callExp::CALL_EXPRESSION, ty::NFType)::Expression
  local call::Call
  local cast_ty::NFType
  @match CALL_EXPRESSION(call = call) = callExp
   callExp = begin
    @match call begin
      TYPED_CALL(__) where {(isBuiltin(call.fn))} => begin
         cast_ty = setArrayElementType(call.ty, ty)
        begin
          @match AbsynUtil.pathFirstIdent(name(call.fn)) begin
            "fill" => begin
              #=  For 'fill' we can type cast the first argument rather than the
              =#
              #=  whole array that 'fill' constructs.
              =#
              @assign call.arguments = _cons(
                typeCast(listHead(call.arguments), ty),
                listRest(call.arguments),
              )
              @assign call.ty = cast_ty
              CALL_EXPRESSION(call)
            end
            #=  For diagonal we can type cast the argument rather than the =#
            "diagonal" => begin
              #=  matrix that diagonal constructs.=#
              @assign call.arguments =
                list(typeCast(listHead(call.arguments), ty))
              @assign call.ty = cast_ty
              CALL_EXPRESSION(call)
            end

            _ => begin
              CAST_EXPRESSION(cast_ty, callExp)
            end
          end
        end
      end

      _ => begin
        CAST_EXPRESSION(setArrayElementType(typeOf(call), ty), callExp)
      end
    end
  end
  return callExp
end

function retype(call::Call)::Call

   () = begin
    local ty::NFType
    local dims::List{Dimension}
    @match call begin
      TYPED_ARRAY_CONSTRUCTOR(__) => begin
         dims = nil
        for i in listReverse(call.iters)
           dims = listAppend(
            arrayDims(typeOf(Util.tuple22(i))),
            dims,
          )
        end
        @assign call.ty = liftArrayLeftList(arrayElementType(call.ty), dims)
        ()
      end

      _ => begin
        ()
      end
    end
  end
  return call
end

function isVectorizeable(@nospecialize(call::Call))::Bool
  isVect = begin
    local name::String
    @match call begin
      TYPED_CALL(fn = M_FUNCTION(path = Absyn.IDENT(name = name))) => begin
        begin
          @match name begin #TODO add other special functions here.
            "der" => begin
              false
            end
            "pre" => begin
              false
            end
            "previous" => begin
              false
            end
            _ => begin
              true
            end
          end
        end
      end

      _ => begin
        true
      end
    end
  end
  return isVect
end

function toDAE(@nospecialize(call::Call))
  local daeCall::DAE.Exp
   daeCall = begin
    local fold_id::String
    local res_id::String
    local fold_exp::Option{Expression}
    @match call begin
      TYPED_CALL(__) => begin
        DAE.CALL(
          nameConsiderBuiltin(call.fn),
          list(toDAE(e) for e in call.arguments),
          toDAE(call.attributes, call.ty),
        )
      end
      TYPED_ARRAY_CONSTRUCTOR(__) => begin
         fold_id = Util.getTempVariableIndex()
         res_id = Util.getTempVariableIndex()
        DAE.REDUCTION(
          DAE.REDUCTIONINFO(
            name(NFBuiltinFuncs.ARRAY_FUNC),
            Absyn.COMBINE(),
            toDAE(call.ty),
            NONE(),
            fold_id,
            res_id,
            NONE(),
          ),
          toDAE(call.exp),
          list(iteratorToDAE(iter) for iter in call.iters),
        )
      end

      TYPED_REDUCTION(__) => begin
         (fold_exp, fold_id, res_id) = call.foldExp
        DAE.REDUCTION(
          DAE.REDUCTIONINFO(
            P_Function.name(call.fn),
            Absyn.COMBINE(),
            toDAE(call.ty),
            toDAEValueOpt(call.defaultExp),
            fold_id,
            res_id,
            toDAEOpt(fold_exp),
          ),
          toDAE(call.exp),
          list(iteratorToDAE(iter) for iter in call.iters),
        )
      end

      _ => begin
        Error.assertion(false, getInstanceName() + " got untyped call", sourceInfo())
        fail()
      end
    end
  end
  return daeCall
end

""" #= Like toString, but prefixes each argument with its type as a comment. =#"""
function typedString(call::Call)::String
  local str::String

  local name::String
  local arg_str::String
  local c::String
  local argexp::Expression

   str = begin
    @match call begin
      ARG_TYPED_CALL(__) => begin
         name = toString(call.ref)
         arg_str = stringDelimitList(
          list(
            "/*" +
            toString(Util.tuple32(arg)) +
            "*/ " +
            toString(Util.tuple31(arg))
            for arg in call.arguments
          ),
          ", ",
        )
        for arg in call.named_args
           c = if arg_str == ""
            ""
          else
            ", "
          end
           arg_str =
            arg_str +
            c +
            Util.tuple41(arg) +
            " = /*" +
            toString(Util.tuple43(arg)) +
            "*/ " +
            toString(Util.tuple42(arg))
        end
        name + "(" + arg_str + ")"
      end

      TYPED_CALL(__) => begin
         name = AbsynUtil.pathString(P_Function.name(call.fn))
         arg_str = stringDelimitList(
          list(toStringTyped(arg) for arg in call.arguments),
          ", ",
        )
        name + "(" + arg_str + ")"
      end

      _ => begin
        toString(call)
      end
    end
  end
  return str
end

function toFlatString(call::Call)::String
  local str::String
  local nameVar::String
  local arg_str::String
  local c::String
  local argexp::Expression
  local iters::List{InstNode}

   str = begin
    @match call begin
      TYPED_CALL(__) => begin
        nameVar = AbsynUtil.pathString(name(call.fn))
         arg_str = stringDelimitList(
          list(toFlatString(arg) for arg in call.arguments),
          ", ",
        )
        if isBuiltin(call.fn)
          stringAppendList(list(nameVar, "(", arg_str, ")"))
        else
          stringAppendList(list("'", nameVar, "'(", arg_str, ")"))
        end
      end

      TYPED_ARRAY_CONSTRUCTOR(__) => begin
        if isVectorized(call)
           str = toFlatString(devectorizeCall(call))
        else
           nameVar = AbsynUtil.pathString(P_Function.name(NFBuiltinFuncs.ARRAY_FUNC))
           arg_str = toFlatString(call.exp)
           c = stringDelimitList(
            list(
              name(Util.tuple21(iter)) +
              " in " +
              toFlatString(Util.tuple22(iter))
              for iter in call.iters
            ),
            ", ",
          )
           str = stringAppendList(list("{", arg_str, " for ", c, "}"))
        end
        #=  Vectorized calls contains iterators with illegal Modelica names
        =#
        #=  (to avoid name conflicts), to make the flat output legal such
        =#
        #=  calls are reverted to their original form here.
        =#
        str
      end

      TYPED_REDUCTION(__) => begin
         nameVar = AbsynUtil.pathString(P_Function.name(call.fn))
         arg_str = toFlatString(call.exp)
         c = stringDelimitList(
          list(
            name(Util.tuple21(iter)) +
            " in " +
            toFlatString(Util.tuple22(iter))
            for iter in call.iters
          ),
          ", ",
        )
        if isBuiltin(call.fn)
          stringAppendList(list(nameVar, "(", arg_str, " for ", c, ")"))
        else
          stringAppendList(list("'", nameVar, "'(", arg_str, " for ", c, ")"))
        end
      end
    end
  end
  return str
end

function toString(call::Call)::String
  local str::String

  local nameStr::String
  local arg_str::String
  local c::String
  local argexp::Expression
  local iters::List{InstNode}

   str = begin
    @match call begin
      UNTYPED_CALL(__) => begin
        nameStr = toString(call.ref)
         arg_str = stringDelimitList(
          list(toString(arg) for arg in call.arguments),
          ", ",
        )
        nameStr * "(" * arg_str * ")"
      end

      ARG_TYPED_CALL(__) => begin
        nameStr = toString(call.ref)
         arg_str = stringDelimitList(
          list(
            toString(Util.tuple31(arg)) for arg in call.arguments
          ),
          ", ",
        )
        for arg in call.named_args
           c = if arg_str == ""
            ""
          else
            ", "
          end
           arg_str =
            arg_str *
            c *
            Util.tuple41(arg) +
            " = " *
            toString(Util.tuple42(arg))
        end
        name * "(" * arg_str * ")"
      end

      UNTYPED_ARRAY_CONSTRUCTOR(__) => begin
        nameStr = AbsynUtil.pathString(name(NFBuiltinFuncs.ARRAY_FUNC))
        arg_str = toString(call.exp)
        c = stringDelimitList(
          list(
            name(Util.tuple21(iter)) *
            " in " *
            toString(Util.tuple22(iter))
            for iter in call.iters
          ),
          ", ",
        )
        "{" * arg_str * " for " * c * "}"
      end

      UNTYPED_REDUCTION(__) => begin
        nameStr = toString(call.ref)
         arg_str = toString(call.exp)
         c = stringDelimitList(
          list(
            name(Util.tuple21(iter)) +
            " in " +
            toString(Util.tuple22(iter))
            for iter in call.iters
          ),
          ", ",
        )
        name + "(" + arg_str + " for " + c + ")"
      end

      TYPED_CALL(__) => begin
        nameStr = AbsynUtil.pathString(name(call.fn))
        arg_str = stringDelimitList(
          list(toString(arg) for arg in call.arguments),
          ", ",
        )
        nameStr + "(" + arg_str + ")"
      end

      TYPED_ARRAY_CONSTRUCTOR(__) => begin
        nameStr = AbsynUtil.pathString(name(NFBuiltinFuncs.ARRAY_FUNC))
        arg_str = toString(call.exp)
        c = stringDelimitList(
          list(
            name(Util.tuple21(iter)) *
            " in " *
            toString(Util.tuple22(iter))
            for iter in call.iters
          ),
          ", ",
        )
        "{" * arg_str * " for " * c * "}"
      end

      TYPED_REDUCTION(__) => begin
nameStr = AbsynUtil.pathString(P_Function.name(call.fn))
         arg_str = toString(call.exp)
         c = stringDelimitList(
          list(
            name(Util.tuple21(iter)) +
            " in " +
            toString(Util.tuple22(iter))
            for iter in call.iters
          ),
          ", ",
        )
        name + "(" + arg_str + " for " + c + ")"
      end
    end
  end
  return str
end

function toRecordExpression(call::Call, ty::NFType)::Expression
  local exp::Expression

   exp = begin
    @match call begin
      TYPED_CALL(__) => begin
        evaluateRecordConstructor(call.fn, ty, call.arguments; evaluate = false)
      end

      _ => begin
        Error.assertion(false, getInstanceName() + " got unknown call", sourceInfo())
        fail()
      end
    end
  end
  return exp
end

function arguments(call::Call)::List{Expression}
  local arguments::List{Expression}

   arguments = begin
    @match call begin
      UNTYPED_CALL(__) => begin
        call.arguments
      end

      TYPED_CALL(__) => begin
        call.arguments
      end
    end
  end
  return arguments
end

function functionName(call::Call)::Absyn.Path
  local nameV::Absyn.Path

  nameV = begin
    @match call begin
      UNTYPED_CALL(__) => begin
        toPath(call.ref)
      end

      ARG_TYPED_CALL(__) => begin
        toPath(call.ref)
      end

      TYPED_CALL(__) => begin
        name(call.fn)
      end

      UNTYPED_ARRAY_CONSTRUCTOR(__) => begin
        Absyn.IDENT("array")
      end

      TYPED_ARRAY_CONSTRUCTOR(__) => begin
        Absyn.IDENT("array")
      end

      UNTYPED_REDUCTION(__) => begin
        toPath(call.ref)
      end

      TYPED_REDUCTION(__) => begin
        name(call.fn)
      end
    end
  end
  return nameV
end

function typedFunction(call::Call)::M_Function
  local fn::M_Function

   fn = begin
    @match call begin
      TYPED_CALL(__) => begin
        call.fn
      end

      TYPED_ARRAY_CONSTRUCTOR(__) => begin
        NFBuiltinFuncs.ARRAY_FUNC
      end

      TYPED_REDUCTION(__) => begin
        call.fn
      end

      _ => begin
        Error.assertion(false, getInstanceName() + " got untyped function", sourceInfo())
        fail()
      end
    end
  end
  return fn
end

function inlineType(call::Call)::DAE.InlineType
  local inlineTy::DAE.InlineType

   inlineTy = begin
    @match call begin
      TYPED_CALL(attributes = CALL_ATTR(inlineType = inlineTy)) => begin
        inlineTy
      end

      _ => begin
        DAE.InlineType.NO_INLINE()
      end
    end
  end
  return inlineTy
end

function isRecordConstructor(call::Call)::Bool
  local isConstructor::Bool

   isConstructor = begin
    @match call begin
      UNTYPED_CALL(__) => begin
        SCodeUtil.isRecord(definition(node(call.ref)))
      end

      TYPED_CALL(__) => begin
        SCodeUtil.isRecord(definition(call.fn.node))
      end

      _ => begin
        false
      end
    end
  end
  return isConstructor
end

function isImpure(call::Call)::Bool
  local impure::Bool
  impure = begin
    @match call begin
      UNTYPED_CALL(__) => begin
        isImpure(listHead(getRefCache(call.ref)))
      end
      TYPED_CALL(__) => begin
        isImpure(call.fn) || isOMImpure(call.fn)
      end
      _ => begin
        false
      end
    end
  end
  return impure
end

function isNotImpure(@nospecialize(call::Call))::Bool
  return !(isImpure(call))
end

function isExternal(call::Call)::Bool
  local isExt::Bool
  isExt = begin
    @match call begin
      UNTYPED_CALL(__) => begin
        isExternalFunction(getClass(node(call.ref)))
      end

      ARG_TYPED_CALL(__) => begin
        isExternalFunction(getClass(node(call.ref)))
      end

      TYPED_CALL(__) => begin
        isExternal(call.fn) #Changed from just returning true johti17 2023-03-26
      end

      _ => begin
        false
      end
    end
  end
  return isExt
end

function compare(call1::Call, call2::Call)::Int
  local comp::Int

   comp = begin
    @match (call1, call2) begin
      (UNTYPED_CALL(__), UNTYPED_CALL(__)) => begin
        compare(call1.ref, call2.ref)
      end

      (TYPED_CALL(__), TYPED_CALL(__)) => begin
        AbsynUtil.pathCompare(P_Function.name(call1.fn), P_Function.name(call2.fn))
      end

      (UNTYPED_CALL(__), TYPED_CALL(__)) => begin
        AbsynUtil.pathCompare(
          toPath(call1.ref),
          P_Function.name(call2.fn),
        )
      end

      (TYPED_CALL(__), UNTYPED_CALL(__)) => begin
        AbsynUtil.pathCompare(
          P_Function.name(call1.fn),
          toPath(call2.ref),
        )
      end
    end
  end
  if comp == 0
     comp = compareList(arguments(call1), arguments(call2))
  end
  return comp
end

function variability(call::Call)::VariabilityType
  local var::VariabilityType

   var = begin
    local var_set::Bool
    @match call begin
      UNTYPED_CALL(__) => begin
         var_set = true
        if isSimple(call.ref)
           var = begin
            @match firstName(call.ref) begin
              "change" => begin
                Variability.DISCRETE
              end

              "edge" => begin
                Variability.DISCRETE
              end

              "pre" => begin
                Variability.DISCRETE
              end

              "ndims" => begin
                Variability.PARAMETER
              end

              "cardinality" => begin
                Variability.PARAMETER
              end

              _ => begin
                 var_set = false
                Variability.CONTINUOUS
              end
            end
          end
        end
        if !var_set
           var = variabilityList(call.arguments)
          for narg in call.named_args
             var = variabilityMax(
              var,
              variability(Util.tuple22(narg)),
            )
          end
        end
        var
      end

      UNTYPED_ARRAY_CONSTRUCTOR(__) => begin
        variability(call.exp)
      end

      UNTYPED_REDUCTION(__) => begin
        variability(call.exp)
      end

      TYPED_CALL(__) => begin
        call.var
      end

      TYPED_ARRAY_CONSTRUCTOR(__) => begin
        call.var
      end

      TYPED_REDUCTION(__) => begin
        call.var
      end

      _ => begin
        Error.assertion(false, getInstanceName() + " got untyped call", sourceInfo())
        fail()
      end
    end
  end
  return var
end

function setType(@nospecialize(call::Call), @nospecialize(ty::NFType))
  local callWithNewType = if call isa TYPED_CALL
    TYPED_CALL(call.fn, ty, call.var, call.arguments, call.attributes)
  elseif call isa TYPED_ARRAY_CONSTRUCTOR
    TYPED_ARRAY_CONSTRUCTOR(ty, call.var, call.exp, call.iters)
  elseif call isa TYPED_REDUCTION
    TYPED_REDUCTION(call.fn, ty, call.var, call.exp,
                    call.iters, call.defaultExp, call.foldExp)
  end
  return callWithNewType
end

function typeOf(call::Call)::NFType
  local ty::NFType
   ty = begin
    @match call begin
      TYPED_CALL(__) => begin
        call.ty
      end

      TYPED_ARRAY_CONSTRUCTOR(__) => begin
        call.ty
      end

      TYPED_REDUCTION(__) => begin
        call.ty
      end

      _ => begin
        TYPE_UNKNOWN()
      end
    end
  end
  return ty
end

function matchTypedNormalCall(call::Call, origin::ORIGIN_Type, info::SourceInfo)::Call
  local func::M_Function
  local args::List{Expression}
  local typed_args::List{TypedArg}
  local matchedFunc::MatchedFunction
  local scope::InstNode
  local var::VariabilityType
  local arg_var::VariabilityType
  local ty::NFType
  local arg_exp::Expression

  @match ARG_TYPED_CALL(call_scope = scope) = call
   matchedFunc = checkMatchingFunctions(call, info)
   func = matchedFunc.func
   typed_args = matchedFunc.args
   args = nil
  #=  if is impure, make it a parameter expression
  =#
  #=  see https:trac.openmodelica.org/OpenModelica/ticket/5133
  =#
  var = if isImpure(func) || isOMImpure(func)
    Variability.PARAMETER
  else
    Variability.CONSTANT
  end
  for a in typed_args
    (arg_exp, _, arg_var) = a
    args = _cons(arg_exp, args)
    var = variabilityMax(var, arg_var)
  end
  args = listReverseInPlace(args)
  ty = returnType(func)
  #=  Hack to fix return type of some builtin functions.
  =#
  if isPolymorphic(ty)
    ty = getSpecialReturnType(func, args)
  end
  if var == Variability.PARAMETER && isExternal(func)
    var = Variability.NON_STRUCTURAL_PARAMETER
  elseif isDiscrete(ty) && var == Variability.CONTINUOUS
    var = Variability.IMPLICITLY_DISCRETE
  end
  #=  Mark external functions with parameter expressions as non-structural,
  =#
  #=  to avoid them being marked as structural unnecessarily.
  =#
  #=  Functions that return a discrete type, e.g. Integer, should probably be
  =#
  #=  treated as implicitly discrete if the arguments are continuous.
  =#
  (ty, _) = evaluateCallType(ty, func, args)
  call = makeTypedCall(func, args, var, ty)
  #=  If the matching was a vectorized one then create a map call
  =#
  #=  using the vectorization dim. This means going through each argument
  =#
  #=  and subscipting it with an iterator for each dim and creating a map call.
  =#
  if isVectorized(matchedFunc)
     call = vectorizeCall(call, matchedFunc.mk, scope, info)
  end
  return call
end

function typeMatchNormalCall(call::Call, origin::ORIGIN_Type, info::SourceInfo)::Call
  local argtycall::Call
  argtycall = typeNormalCall(call, origin, info)
  call = matchTypedNormalCall(argtycall, origin, info)
  return call
end

function unboxArgs(call::TYPED_CALL)
  local args = list(unbox(arg) for arg in call.arguments)
  return TYPED_CALL(call.fn, call.ty, call.var, args, call.attributes)
end

function makeTypedCall(
  @nospecialize(fn::M_Function),
  @nospecialize(args::List{<:Expression}),
  @nospecialize(  variability::VariabilityType),
  returnType::NFType = fn.returnType,
)::Call
  local call::Call
  local ca::CallAttributes
   ca = CALL_ATTR(
    isTuple(returnType),
    isBuiltin(fn),
    isImpure(fn),
    isFunctionPointer(fn),
    inlineBuiltin(fn),
    DAE.NO_TAIL(),
  )
   call = TYPED_CALL(fn, returnType, variability, args, ca)
  return call
end

function typeNormalCall(call::Call, origin::ORIGIN_Type, info::SourceInfo)::Call

   call = begin
    local fnl::List{M_Function}
    local is_external::Bool
    @match call begin
      UNTYPED_CALL(__) => begin
         fnl = typeRefCache(call.ref)
        typeArgs(call, origin, info)
      end

      _ => begin
        Error.assertion(
          false,
          getInstanceName() + " got invalid function call expression",
          sourceInfo(),
        )
        fail()
      end
    end
  end
  return call
end


function typeCall(
  @nospecialize(callExp::Expression),
  @nospecialize(origin::ORIGIN_Type),
  @nospecialize(info::SourceInfo),
  )
  arg1 = callExp::Expression
  arg2 = origin::ORIGIN_Type
  arg3 = info
  typeCall2(
    arg1,
    arg2,
    arg3,
  )
end

function typeCall2(
  @nospecialize(callExp::Expression),
  @nospecialize(origin::ORIGIN_Type),
  @nospecialize(info::SourceInfo),
  )
  @nospecialize
  local var::VariabilityType
  local ty::NFType
  local outExp::Expression
  local call::Call
  local ty_call::Call
  local args::List{Expression}
  local cref::ComponentRef
  @match CALL_EXPRESSION(call = call) = callExp
  outExp = begin
    @match call begin
      UNTYPED_CALL(ref = cref) => begin
        if needSpecialHandling(call)
          (outExp, ty, var) = typeSpecial(call, origin, info)
        else
          ty_call = typeMatchNormalCall(call, origin, info)
          ty = typeOf(ty_call)
          var = variability(ty_call)
          if isRecordConstructor(ty_call)
            outExp = toRecordExpression(ty_call, ty)
          else
            if hasUnboxArgs(typedFunction(ty_call))
              outExp = CALL_EXPRESSION(unboxArgs(ty_call))
            else
              outExp = CALL_EXPRESSION(ty_call)
            end
            outExp = inlineCallExp(outExp)
          end
        end
        outExp
      end

      UNTYPED_ARRAY_CONSTRUCTOR(__) => begin
        (ty_call, ty, var) = typeArrayConstructor(call, origin, info)
        CALL_EXPRESSION(ty_call)
      end

      UNTYPED_REDUCTION(__) => begin
        (ty_call, ty, var) = typeReduction(call, origin, info)
        CALL_EXPRESSION(ty_call)
      end

      TYPED_CALL(__) => begin
        ty = call.ty
        var = call.var
        callExp
      end

      TYPED_ARRAY_CONSTRUCTOR(__) => begin
        ty = call.ty
        var = call.var
        callExp
      end

      TYPED_REDUCTION(__) => begin
        ty = call.ty
        var = call.var
        callExp
      end

      _ => begin
        Error.assertion(
          false,
          getInstanceName() + ": " + toString(callExp),
          sourceInfo(),
        )
        fail()
      end
    end
  end
  return (outExp, ty, var)
end

function instantiate(
  functionName::Absyn.ComponentRef,
  functionArgs::Absyn.FunctionArgs,
  scope::InstNode,
  info::SourceInfo
)::Expression
  local callExp::Expression
   callExp = begin
    @match functionArgs begin
      Absyn.FUNCTIONARGS(__) => begin
        instNormalCall(functionName, functionArgs, scope, info)
      end
      Absyn.FOR_ITER_FARG(__) => begin
        instIteratorCall(functionName, functionArgs, scope, info)
      end
      _ => begin
        Error.assertion(false, getInstanceName() + " got unknown call type", sourceInfo())
        fail()
      end
    end
  end
  return callExp
end

function getSpecialReturnType(fn::M_Function, args::List{<:Expression})::NFType
  local ty::NFType

   ty = begin
    @match fn.path begin
      Absyn.IDENT("min") => begin
        arrayElementType(typeOf(unbox(listHead(
          args,
        ))))
      end

      Absyn.IDENT("max") => begin
        arrayElementType(typeOf(unbox(listHead(
          args,
        ))))
      end

      Absyn.IDENT("sum") => begin
        arrayElementType(typeOf(unbox(listHead(
          args,
        ))))
      end

      Absyn.IDENT("product") => begin
        arrayElementType(typeOf(unbox(listHead(
          args,
        ))))
      end

      Absyn.IDENT("previous") => begin
        typeOf(unbox(listHead(args)))
      end

      Absyn.IDENT("shiftSample") => begin
        typeOf(unbox(listHead(args)))
      end

      Absyn.IDENT("backSample") => begin
        typeOf(unbox(listHead(args)))
      end

      Absyn.IDENT("hold") => begin
        typeOf(unbox(listHead(args)))
      end

      Absyn.IDENT("superSample") => begin
        typeOf(unbox(listHead(args)))
      end

      Absyn.IDENT("subSample") => begin
        typeOf(unbox(listHead(args)))
      end

      Absyn.IDENT("DynamicSelect") => begin
        typeOf(unbox(listHead(args)))
      end

      _ => begin
        Error.assertion(
          false,
          getInstanceName() + ": unhandled case for " + AbsynUtil.pathString(fn.path),
          sourceInfo(),
        )
        fail()
      end
    end
  end
  return ty
end

function evaluateCallTypeDimExp(exp::Expression, ptree::ParameterTree)::Expression
  local outExp::Expression
  outExp = begin
    local node::InstNode
    local oexp::Option{Expression}
    local e::Expression
    @match exp begin
      CREF_EXPRESSION(
        cref = COMPONENT_REF_CREF(
          node = node,
          restCref = COMPONENT_REF_EMPTY(__),
        ),
      ) => begin
        oexp = ParameterTreeImpl.getOpt(ptree, name(node))
        if isSome(oexp)
          @match SOME(outExp) = oexp
        end
        #=  TODO: Apply subscripts. =#
        outExp
      end
      _ => begin
        exp
      end
    end
  end
  return outExp
end

function buildParameterTree(
  fnArgs::Tuple{<:M_Function, List{<:Expression}},
  ptree::ParameterTree,
)::ParameterTree
  local fn::M_Function
  local args::List{Expression}
  local arg::Expression
  if !(ParameterTreeImpl.isEmpty(ptree))
    return ptree
  end
   (fn, args) = fnArgs
  for i in fn.inputs
    @match _cons(arg, args) = args
     ptree = ParameterTreeImpl.add(ptree, name(i), arg)
  end
  #=  TODO: Add local variable bindings. =#
  return ptree
end

function evaluateCallTypeDim(
  dim::Dimension,
  fnArgs::Tuple{<:M_Function, List{<:Expression}},
  ptree::ParameterTree,
)::Tuple{Dimension, ParameterTree}

   dim = begin
    local exp::Expression
    @match dim begin
      DIMENSION_EXP(__) => begin
        ptree = buildParameterTree(fnArgs, ptree)
        exp = map(
          dim.exp,
          (expArg) -> evaluateCallTypeDimExp(expArg, ptree),
        )
        ErrorExt.setCheckpoint(getInstanceName())
        try
           exp = Ceval.evalExp(exp, Ceval.EVALTARGET_IGNORE_ERRORS())
        catch
        end
        ErrorExt.rollBack(getInstanceName())
        fromExp(exp, Variability.CONSTANT)
      end

      _ => begin
        dim
      end
    end
  end
  return (dim, ptree)
end

function evaluateCallType(
  ty::NFType,
  fn::M_Function,
  args::List{<:Expression},
  ptree::ParameterTree = ParameterTreeImpl.EMPTY(),
  )::Tuple{NFType, ParameterTree}
   ty = begin
    local dims::List{Dimension}
    local tys::List{NFType}
    @match ty begin
      TYPE_ARRAY(__) => begin
        (dims, ptree) =
          ListUtil.map1Fold(ty.dimensions, evaluateCallTypeDim, (fn, args), ptree)
        @assign ty.dimensions = dims
        ty
      end
      TYPE_TUPLE(__) => begin
        (tys, ptree) =
          ListUtil.map2Fold(ty.types, evaluateCallType, fn, args, ptree)
        @assign ty.types = tys
        ty
      end
      _ => begin
        ty
      end
    end
  end
  return (ty, ptree)
end

""" #= Transforms a vectorized call into a non-vectorized one. This function is
     used as a helper to output valid flat Modelica, and should probably not
     be used where e.g. correct types are required. =#"""
function devectorizeCall(call::Call)::Call
  local outCall::Call
  local exp::Expression
  local iter_exp::Expression
  local iters::List{Tuple{InstNode, Expression}}
  local iter_node::InstNode
  @match TYPED_ARRAY_CONSTRUCTOR(exp = exp, iters = iters) = call
  for i in iters
     (iter_node, iter_exp) = i
     exp = replaceIterator(exp, iter_node, iter_exp)
  end
  @match CALL_EXPRESSION(call = outCall) = exp
  return outCall
end

function isVectorized(call::Call)::Bool
  local vectorized::Bool

   vectorized = begin
    @match call begin
      TYPED_ARRAY_CONSTRUCTOR(exp = CALL_EXPRESSION(__)) => begin
        stringGet(name(Util.tuple21(listHead(call.iters))), 1) == 36
      end

      _ => begin
        false
      end
    end
  end
  #=  A call is considered to be vectorized if the first iterator has a name
  =#
  #=  beginning with $.
  =#
  #= /* $ */ =#
  return vectorized
end

function vectorizeCall(
  base_call::Call,
  mk::FunctionMatchKind,
  scope::InstNode,
  info::SourceInfo,
)::Call
  local vectorized_call::Call
  local ty::NFType
  local vect_ty::NFType
  local exp::Expression
  local iters::List{Tuple{InstNode, Expression}}
  local iter::InstNode
  local i::Int
  local vect_idx::Int
  local b::Bool
  local call_args::List{Expression}
  local vect_args::List{Expression}
  local sub::Subscript
  local vect_idxs::List{Int}
  vectorized_call = begin
    @match (base_call, mk) begin
      (TYPED_CALL(arguments = call_args), VECTORIZED_MATCH_KIND(__)) => begin
         iters = nil
         i = 1
        for dim in mk.vectDims
          Error.assertion(
            isKnown(dim, true),
            getInstanceName() + " got unknown dimension for vectorized call",
            info,
          )
          ty = TYPE_ARRAY(TYPE_INTEGER(), list(dim))
          exp = RANGE_EXPRESSION(
            ty,
            INTEGER_EXPRESSION(1),
            NONE(),
            sizeExp(dim),
          )
          iter = fromComponent(
            "i" + intString(i),
            ITERATOR_COMPONENT(TYPE_INTEGER(), Variability.CONSTANT, info),
            scope,
          )
          iters = _cons((iter, exp), iters)
          exp = CREF_EXPRESSION(
            TYPE_INTEGER(),
            makeIterator(iter, TYPE_INTEGER()),
          )
          sub = SUBSCRIPT_INDEX(exp)
          call_args = ListUtil.mapIndices(
            call_args,
            mk.vectorizedArgs,
            (x) -> applySubscript(
              sub,
              x,
              list(),
            ),
          )
          i = i + 1
        end
        #=  Create the range on which we will iterate to vectorize.
        =#
        #=  Create the iterator.
        =#
        #=  Now that iterator is ready apply it, as a subscript, to each argument that is supposed to be vectorized
        =#
        #=  Make a cref expression from the iterator =#
        vect_ty = liftArrayLeftList(base_call.ty, mk.vectDims)
        base_call = TYPED_CALL(base_call.fn, base_call.ty, base_call.var, call_args, base_call.attributes)
        TYPED_ARRAY_CONSTRUCTOR(
          vect_ty,
          base_call.var,
          CALL_EXPRESSION(base_call),
          iters,
        )
      end

      _ => begin
        Error.addInternalError(getInstanceName() + " got unknown call", info)
        fail()
      end
    end
  end
  return vectorized_call
end

function iteratorToDAE(iter::Tuple{<:InstNode, Expression})::DAE.ReductionIterator
  local diter::DAE.ReductionIterator

  local iter_node::InstNode
  local iter_range::Expression
  local c::Component
  local b::Binding

   (iter_node, iter_range) = iter
   diter = DAE.REDUCTIONITER(
    name(iter_node),
    toDAE(iter_range),
    NONE(),
    toDAE(getType(iter_node)),
  )
  return diter
end

function checkMatchingFunctions(call::Call, info::SourceInfo)
  local matchedFunc::MatchedFunction

  local matchedFunctions::List{MatchedFunction}
  local exactMatches::List{MatchedFunction}
  local func::M_Function
  local allfuncs::List{M_Function}
  local fn_node::InstNode
  local numerr::Int = 0 # TODO Error.getNumErrorMessages()
  local errors::List{Int}

  ErrorExt.setCheckpoint("NFCall:checkMatchingFunctions")
   matchedFunctions = begin
    @match call begin
      ARG_TYPED_CALL(ref = COMPONENT_REF_CREF(node = fn_node)) => begin
         allfuncs = getCachedFuncs(fn_node)
        if listLength(allfuncs) > 1
           allfuncs =
            list(fn for fn in allfuncs if !isDefaultRecordConstructor(fn))
        end
        matchFunctions(allfuncs, call.arguments, call.named_args, info)
      end
    end
  end
  if listEmpty(matchedFunctions)
    if listLength(allfuncs) > 1
      ErrorExt.rollBack("NFCall:checkMatchingFunctions")
      Error.addSourceMessage(
        Error.NO_MATCHING_FUNCTION_FOUND_NFINST,
        list(typedString(call), P_Function.candidateFuncListString(allfuncs)),
        info,
      )
    elseif numerr == 0 #Error.getNumErrorMessages() TODO
      #ErrorExt.rollBack("NFCall:checkMatchingFunctions")
      # Error.addSourceMessage(
      #   Error.NO_MATCHING_FUNCTION_FOUND_NFINST,
      #   list(typedString(call), P_Function.candidateFuncListString(allfuncs)),
      #   info,
      # )
      @error "No matching function found for $(typedString(call)). Candidates where" candidateFuncListString(allfuncs)
    else
      ErrorExt.delCheckpoint("NFCall:checkMatchingFunctions")
    end
    fail()
  end
  #=  Don't show error messages for overloaded functions, it leaks
  =#
  #=  implementation details and usually doesn't provide any more info than
  =#
  #=  what the \"no match found\" error gives anyway.
  =#
  #=  Only show the error message for no matching functions if no other error
  =#
  #=  was shown.
  =#
  #=  functions that for some reason failed to match without giving any error.
  =#
  #=  If we have at least one matching function then we discard all error messages
  =#
  #=  about matching. We have one matching func if we reach here.
  =#
  ErrorExt.rollBack("NFCall:checkMatchingFunctions")
  if listLength(matchedFunctions) > 1
     exactMatches = getExactMatches(matchedFunctions)
    if listEmpty(exactMatches)
       exactMatches = P_MatchedFunction.getExactVectorizedMatches(matchedFunctions)
    end
    if listLength(exactMatches) > 1
      Error.addSourceMessage(
        Error.AMBIGUOUS_MATCHING_FUNCTIONS_NFINST,
        list(
          typedString(call),
          P_Function.candidateFuncListString(list(mfn.func for mfn in matchedFunctions)),
        ),
        info,
      )
      fail()
    end
     matchedFunc = listHead(exactMatches)
  else
     matchedFunc = listHead(matchedFunctions)
  end
  #=  Overwrite the actual function name with the overload name for builtin functions.
  =#
  if isBuiltin(matchedFunc.func)
    func = matchedFunc.func
    local funcPath = nameConsiderBuiltin(func)
    func = M_FUNCTION(funcPath,
               func.node,
               func.inputs,
               func.outputs,
               func.locals,
               func.slots,
               func.returnType,
               func.attributes,
               func.derivatives,
               func.status,
               func.callCounter)
    #@assign matchedFunc.func = func
    matchedFunc = MATCHED_FUNC(func, matchedFunc.args, matchedFunc.mk)
  end
  return matchedFunc
end

function typeArgs(call::Call, origin::ORIGIN_Type, info::SourceInfo)::Call

   call = begin
    local arg::Expression
    local arg_ty::NFType
    local arg_var::VariabilityType
    local typedArgs::List{TypedArg}
    local typedNamedArgs::List{TypedNamedArg}
    local name::String
    local next_origin::ORIGIN_Type
    @match call begin
      UNTYPED_CALL(__) => begin
         typedArgs = nil
         next_origin = setFlag(origin, ORIGIN_SUBEXPRESSION)
        for arg in call.arguments
           (arg, arg_ty, arg_var) = typeExp(arg, next_origin, info)
           typedArgs = _cons((arg, arg_ty, arg_var), typedArgs)
        end
         typedArgs = listReverse(typedArgs)
         typedNamedArgs = nil
        for narg in call.named_args
           (name, arg) = narg
           (arg, arg_ty, arg_var) = typeExp(arg, next_origin, info)
           typedNamedArgs = _cons((name, arg, arg_ty, arg_var), typedNamedArgs)
        end
         typedNamedArgs = listReverse(typedNamedArgs)
        ARG_TYPED_CALL(call.ref, typedArgs, typedNamedArgs, call.call_scope)
      end
    end
  end
  return call
end

function reductionFoldIterator(name::String, ty::NFType)::Expression
  local iterExp::Expression

   iterExp = CREF_EXPRESSION(
    ty,
    makeIterator(NAME_NODE(name), ty),
  )
  return iterExp
end

function reductionFoldExpression(
  reductionFn::M_Function,
  reductionType::NFType,
  reductionVar::VariabilityType,
  foldId::String,
  resultId::String,
)::Option{Expression}
  local foldExp::Option{Expression}

  local ty::NFType
  local op_node::InstNode
  local fn::M_Function

  if isComplex(reductionType)
     foldExp = begin
      @match AbsynUtil.pathFirstIdent(P_Function.name(reductionFn)) begin
        "sum" => begin
          @match TYPE_COMPLEX(cls = op_node) = reductionType
           op_node = lookupElement("'+'", getClass(op_node))
          instFunctionNode(op_node)
          @match list(fn) = P_Function.typeNodeCache(op_node)
          SOME(CALL_EXPRESSION(makeTypedCall(
            fn,
            list(
              reductionFoldIterator(resultId, reductionType),
              reductionFoldIterator(foldId, reductionType),
            ),
            reductionVar,
          )))
        end

        _ => begin
          NONE()
        end
      end
    end
  else
     foldExp = begin
      @match AbsynUtil.pathFirstIdent(name(reductionFn)) begin
        "sum" => begin
          SOME(BINARY_EXPRESSION(
            reductionFoldIterator(resultId, reductionType),
            makeAdd(reductionType),
            reductionFoldIterator(foldId, reductionType),
          ))
        end

        "product" => begin
          SOME(BINARY_EXPRESSION(
            reductionFoldIterator(resultId, reductionType),
            makeMul(reductionType),
            reductionFoldIterator(foldId, reductionType),
          ))
        end

        "array" => begin
          NONE()
        end

        "array" => begin
          NONE()
        end

        "list" => begin
          NONE()
        end

        "listReverse" => begin
          NONE()
        end

        _ => begin
          SOME(CALL_EXPRESSION(P_Call.makeTypedCall(
            reductionFn,
            list(
              reductionFoldIterator(foldId, reductionType),
              reductionFoldIterator(resultId, reductionType),
            ),
            reductionVar,
            reductionType,
          )))
        end
      end
    end
  end
  return foldExp
end

function reductionDefaultValue(fn::M_Function, ty::NFType)::Option{Expression}
  local defaultValue::Option{Expression}

  if isArray(ty)
     defaultValue = NONE()
  else
     defaultValue = begin
      @match AbsynUtil.pathFirstIdent(P_Function.name(fn)) begin
        "sum" => begin
          SOME(makeZero(ty))
        end

        "product" => begin
          SOME(makeOne(ty))
        end

        "min" => begin
          SOME(makeMaxValue(ty))
        end

        "max" => begin
          SOME(makeMinValue(ty))
        end

        _ => begin
          Error.addSourceMessage(
            Error.INTERNAL_ERROR,
            list(
              getInstanceName() +
              " got unknown reduction name " +
              AbsynUtil.pathFirstIdent(P_Function.name(fn)),
            ),
            sourceInfo(),
          )
          fail()
        end
      end
    end
  end
  return defaultValue
end

function typeReduction(
  call::Call,
  origin::ORIGIN_Type,
  info::SourceInfo,
)::Tuple{Call, NFType, Variability}
  local variability::VariabilityType
  local ty::NFType
  local range::Expression
  local arg::Expression
  local default_exp::Option{Expression}
  local fold_exp::Option{Expression}
  local iter::InstNode
  local iter_var::VariabilityType
  local exp_var::VariabilityType
  local iters::List{Tuple{InstNode, Expression}} = nil
  local next_origin::ORIGIN_Type
  local fn::M_Function
  local fold_id::String
  local res_id::String
  local fold_tuple::Tuple{Option{Expression}, String, String}
  return (call, ty, variability) = begin
    @match call begin
      UNTYPED_REDUCTION(__) => begin
         variability = Variability.CONSTANT
         next_origin = setFlag(origin, ORIGIN_SUBEXPRESSION)
        for i in call.iters
           (iter, range) = i
           (range, _, iter_var) =
            typeIterator(iter, range, origin, structural = false)
           variability = Variability.variabilityMax(variability, iter_var)
           iters = _cons((iter, range), iters)
        end
         iters = listReverseInPlace(iters)
        #=  ExpOrigin.FOR is used here as a marker that this expression may contain iterators.
        =#
         next_origin = intBitOr(next_origin, ORIGIN_FOR)
         (arg, ty, exp_var) = typeExp(call.exp, next_origin, info)
         variability = Variability.variabilityMax(variability, exp_var)
        @match list(fn) = typeRefCache(call.ref)
        TypeCheck.checkReductionType(ty, P_Function.name(fn), call.exp, info)
         fold_id = Util.getTempVariableIndex()
         res_id = Util.getTempVariableIndex()
         default_exp = reductionDefaultValue(fn, ty)
         fold_exp = reductionFoldExpression(fn, ty, variability, fold_id, res_id)
         fold_tuple = (fold_exp, fold_id, res_id)
        (
          TYPED_REDUCTION(fn, ty, variability, arg, iters, default_exp, fold_tuple),
          ty,
          variability,
        )
      end
      _ => begin
        Error.assertion(
          false,
          getInstanceName() + " got invalid reduction call",
          sourceInfo(),
        )
        fail()
      end
    end
  end
end

function typeArrayConstructor(
  @nospecialize(call::Call),
  @nospecialize(origin::ORIGIN_Type),
  @nospecialize(info::SourceInfo),
)::Tuple{Call, NFType, VariabilityType}
  local variability::VariabilityType
  local ty::NFType
  local arg::Expression
  local range::Expression
  local iter_ty::NFType
  local iter_var::VariabilityType
  local exp_var::VariabilityType
  local iter::InstNode
  local dims::List{Dimension} = nil
  local iters::List{Tuple{InstNode, Expression}} = nil
  local next_origin::ORIGIN_Type
  local is_structural::Bool

  (call, ty, variability) = begin
    @match call begin
      UNTYPED_ARRAY_CONSTRUCTOR(__) => begin
        variability = Variability.CONSTANT
        #=  The size of the expression must be known unless we're in a function. =#
        is_structural = flagNotSet(origin, ORIGIN_FUNCTION)
        next_origin = setFlag(origin, ORIGIN_SUBEXPRESSION)
        for i in call.iters
          (iter, range) = i
          (range, iter_ty, iter_var) =
            Base.inferencebarrier(typeIterator(iter, range, next_origin, is_structural)::Tuple{Expression, NFType, VariabilityType})
          if is_structural
            range = evalExp(range, EVALTARGET_RANGE(info))
            iter_ty = typeOf(range)
          end
          dims = listAppend(arrayDims(iter_ty), dims)
          variability = variabilityMax(variability, iter_var)
          iters = _cons((iter, range), iters)
        end
        iters = listReverseInPlace(iters)
        #=  ExpOrigin.FOR is used here as a marker that this expression may contain iterators.
        =#
        next_origin = intBitOr(next_origin, ORIGIN_FOR)
        (arg, ty, exp_var) = typeExp(call.exp, next_origin, info)
        variability = variabilityMax(variability, exp_var)
        ty = liftArrayLeftList(ty, dims)
        variability = variabilityMax(variability, exp_var)
        (TYPED_ARRAY_CONSTRUCTOR(ty, variability, arg, iters), ty, variability)
      end

      _ => begin
        Error.assertion(
          false,
          getInstanceName() + " got invalid function call expression",
          sourceInfo(),
        )
        fail()
      end
    end
  end
  return (call, ty, variability)
end

function instIterators(
  inIters::List{<:Absyn.ForIterator},
  scope::InstNode,
  info::SourceInfo,
)::Tuple{InstNode, List{Tuple{InstNode, Expression}}}
  local outIters::List{Tuple{InstNode, Expression}} = nil
  local outScope::InstNode = scope
  local range::Expression
  local iter::InstNode
  for i in inIters
    range = instExp(Util.getOption(i.range), outScope, info)
    (outScope, iter) = addIteratorToScope(i.name, outScope, info)
    outIters = _cons((iter, range), outIters)
  end
  outIters = listReverse(outIters)
  return (outScope, outIters)
end

function instIteratorCallArgs(
  args::Absyn.FunctionArgs,
  scope::InstNode,
  info::SourceInfo,
)::Tuple{Expression, List{Tuple{InstNode, Expression}}}
  local iters::List{Tuple{InstNode, Expression}}
  local exp::Expression
   _ = begin
    local for_scope::InstNode
    @match args begin
      Absyn.FOR_ITER_FARG(__) => begin
        (for_scope, iters) = instIterators(args.iterators, scope, info)
        exp = instExp(args.exp, for_scope, info)
        ()
      end
    end
  end
  return (exp, iters)
end

function instIteratorCall(
  functionName::Absyn.ComponentRef,
  functionArgs::Absyn.FunctionArgs,
  scope::InstNode,
  info::SourceInfo,
  )::Expression
  local callExp::Expression
  local fn_name::Absyn.ComponentRef
  local fn_ref::ComponentRef
  local exp::Expression
  local iters::List{Tuple{InstNode, Expression}}
  local is_array::Bool
  #=  The parser turns {exp for i in ...} into $array(exp for i in ...), but we
  =#
  #=  change it to just array here so we can handle array constructors uniformly.=#
   fn_name = begin
    @match functionName begin
      Absyn.CREF_IDENT("\$array") => begin
        Absyn.CREF_IDENT("array", nil)
      end
      _ => begin
        functionName
      end
    end
  end
  (exp, iters) = instIteratorCallArgs(functionArgs, scope, info)
  if AbsynUtil.crefFirstIdent(fn_name) == "array"
    callExp = CALL_EXPRESSION(UNTYPED_ARRAY_CONSTRUCTOR(exp, iters))
  else
    fn_ref = instFunction(fn_name, scope, info)
    callExp = CALL_EXPRESSION(UNTYPED_REDUCTION(fn_ref, exp, iters))
  end
  return callExp
end

function instNamedArg(absynArg::Absyn.NamedArg, scope::InstNode, info::SourceInfo)::NamedArg
  local arg::NamedArg
  local name::String
  local exp::Absyn.Exp
  @match Absyn.NAMEDARG(argName = name, argValue = exp) = absynArg
  arg = (name, instExp(exp, scope, info))
  return arg
end

function instArgs(
  args::Absyn.FunctionArgs,
  scope::InstNode,
  info::SourceInfo
)::Tuple{List{Expression}, List{NamedArg}}
  local namedArgs::List{NamedArg}
  local posArgs::List{Expression}
  #@debug "Calling inst args for $args"
   (posArgs, namedArgs) = begin
    @match args begin
      Absyn.FUNCTIONARGS(__) => begin
        #@debug "Matched function args"
         posArgs = list(instExp(a, scope, info) for a in args.args)
        #@debug "Positional arguments done"
         namedArgs = list(instNamedArg(a, scope, info) for a in args.argNames)
        #@debug "Named arguments done"
        (posArgs, namedArgs)
      end
      _ => begin
        #Error.assertion(false, getInstanceName() + " got unknown function args", sourceInfo())
        @error "Got unknown args! for $info"
        fail()
      end
    end
  end
  return (posArgs, namedArgs)
end

function instNormalCall(
  @nospecialize(functionName::Absyn.ComponentRef),
  @nospecialize(functionArgs::Absyn.FunctionArgs),
  @nospecialize(scope::InstNode),
  @nospecialize(info::SourceInfo),
)
  local callExp::Expression

  local fn_ref::ComponentRef
  local args::List{Expression}
  local named_args::List{NamedArg}
  local name::String

  name = AbsynUtil.crefFirstIdent(functionName)
  #=  try to inst the parameters =#
  try
    (args, named_args) = instArgs(functionArgs, scope, info)
 catch e
    if false #=Config.getGraphicsExpMode() && stringEq(name, "DynamicSelect") TODO =#
      callExp = begin
        @match functionArgs begin
          Absyn.FUNCTIONARGS(__) => begin
            instExp(listHead(functionArgs.args), scope, info)
          end
        end
      end
      return callExp
    else
      @error "Failed with the following error $e"
      fail()
    end
  end
  #=  didn't work, is this DynamicSelect dynamic part?! #5631
  =#
  #=  return just the first part of DynamicSelect
  =#
   callExp = begin
    @match name begin
      "size" => begin
        makeSizeExp(args, named_args, info)
      end
      "array" => begin
        makeArrayExp(args, named_args, info)
      end
      _ => begin
        #=  size creates Expression.SIZE instead of Expression.CALL.
        =#
        #=  array() call with no iterators creates Expression.ARRAY instead of Expression.CALL.
        =#
        #=  If it had iterators then it will not reach here. The args would have been parsed to
        =#
        #=  Absyn.FOR_ITER_FARG and that is handled in instIteratorCall.
        =#
         (fn_ref, _, _) = instFunction(functionName, scope, info)
        CALL_EXPRESSION(UNTYPED_CALL(fn_ref, args, named_args, scope))
      end
    end
  end
  return callExp
end
