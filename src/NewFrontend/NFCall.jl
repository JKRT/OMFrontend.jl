module NFCall

using MetaModelica
using ExportAll

#= /*
* This file is part of OpenModelica.
*
* Copyright (c) 1998-2014, Open Source Modelica Consortium (OSMC),
* c/o Linköpings universitet, Department of Computer and Information Science,
* SE-58183 Linköping, Sweden.
*
* All rights reserved.
*
* THIS PROGRAM IS PROVIDED UNDER THE TERMS OF GPL VERSION 3 LICENSE OR
* THIS OSMC PUBLIC LICENSE (OSMC-PL) VERSION 1.2.
* ANY USE, REPRODUCTION OR DISTRIBUTION OF THIS PROGRAM CONSTITUTES
* RECIPIENT'S ACCEPTANCE OF THE OSMC PUBLIC LICENSE OR THE GPL VERSION 3,
* ACCORDING TO RECIPIENTS CHOICE.
*
* The OpenModelica software and the Open Source Modelica
* Consortium (OSMC) Public License (OSMC-PL) are obtained
* from OSMC, either from the above address,
* from the URLs: http:www.ida.liu.se/projects/OpenModelica or
* http:www.openmodelica.org, and in the OpenModelica distribution.
* GNU version 3 is obtained from: http:www.gnu.org/copyleft/gpl.html.
*
* This program is distributed WITHOUT ANY WARRANTY; without
* even the implied warranty of  MERCHANTABILITY or FITNESS
* FOR A PARTICULAR PURPOSE, EXCEPT AS EXPRESSLY SET FORTH
* IN THE BY RECIPIENT SELECTED SUBSIDIARY LICENSE CONDITIONS OF OSMC-PL.
*
* See the full OSMC Public License conditions for more details.
*
*/ =#
import Absyn
import ..AbsynUtil
import DAE
import ..P_NFExpression
P_Expression = P_NFExpression
Expression = P_NFExpression.NFExpression
import ..NFInstNode.P_InstNode
import ..NFPrefixes.Variability
import ..P_NFType
P_M_Type = P_NFType
M_Type = NFType
import ..NFRecord
Record = NFRecord

import ..NFBuiltinCall
BuiltinCall = NFBuiltinCall
import ..NFCeval
Ceval = NFCeval
import ..P_NFComponentRef
P_ComponentRef = P_NFComponentRef
ComponentRef = P_NFComponentRef.NFComponentRef
import ..Config
import ..P_NFDimension
P_Dimension = P_NFDimension
Dimension = P_NFDimension.NFDimension
import ..ErrorExt
import ..NFInline
Inline = NFInline
import ..NFInst
Inst = NFInst
import ListUtil
import ..NFLookup
Lookup = NFLookup
import ..MetaModelica.Dangerous.listReverseInPlace
import ..NFBinding.P_Binding
import ..NFClass.P_Class
import ..NFComponent.P_Component
import ..NFFunction.P_Function
import ..NFFunction.P_FunctionMatchKind
import ..NFFunction.P_MatchedFunction
import ..NFFunction.NamedArg
import ..NFFunction.TypedArg
import ..NFFunction.TypedNamedArg
import ..NFInstNode.P_CachedData
import ..NFTyping.ExpOrigin
import ..NFPrefixes
P_Prefixes = NFPrefixes
Prefixes = NFPrefixes.Prefixes
import ..SCodeUtil
import ..NFTypeCheck
TypeCheck = NFTypeCheck
import ..NFTyping
Typing = NFTyping
import ..Util
import ..P_NFSubscript
P_Subscript = P_NFSubscript
Subscript = P_NFSubscript.NFSubscript
import ..P_NFOperator
P_Operator = P_NFOperator
Operator = P_NFOperator.NFOperator
import ..NFEvalFunction
EvalFunction = NFEvalFunction

module P_CallAttributes

using MetaModelica
using ExportAll
#= Forward declarations for uniontypes until Julia adds support for mutual recursion =#

@UniontypeDecl CallAttributes

function toDAE(attr::CallAttributes, returnType::M_Type)::DAE.P_CallAttributes
  local fattr::DAE.P_CallAttributes

  @assign fattr = DAE.CALL_ATTR(
    Type.toDAE(returnType),
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

@exportAll()
end

ParameterTree = ParameterTreeImpl.Tree

module ParameterTreeImpl

using MetaModelica
using ExportAll

import ..BaseAvlTree
import ..P_NFExpression
P_Expression = P_NFExpression
Expression = P_NFExpression.NFExpression
using BaseAvlTree #= Modelica extend clause =#
Key = String
Value = Expression

@exportAll()
end

module P_Call

using MetaModelica
using ExportAll
#= Forward declarations for uniontypes until Julia adds support for mutual recursion =#

@UniontypeDecl Call

function typeCast(callExp::Expression, ty::M_Type)::Expression

  local call::Call
  local cast_ty::M_Type

  @match P_Expression.Expression.CALL(call = call) = callExp
  @assign callExp = begin
    @match call begin
      TYPED_CALL(__) where {(P_Function.isBuiltin(call.fn))} => begin
        @assign cast_ty = Type.setArrayElementType(call.ty, ty)
        begin
          @match AbsynUtil.pathFirstIdent(P_Function.name(call.fn)) begin
            "fill" => begin
              #=  For 'fill' we can type cast the first argument rather than the
              =#
              #=  whole array that 'fill' constructs.
              =#
              @assign call.arguments = _cons(
                P_Expression.Expression.typeCast(listHead(call.arguments), ty),
                listRest(call.arguments),
              )
              @assign call.ty = cast_ty
              P_Expression.Expression.CALL(call)
            end

            "diagonal" => begin
              #=  For diagonal we can type cast the argument rather than the
              =#
              #=  matrix that diagonal constructs.
              =#
              @assign call.arguments =
                list(P_Expression.Expression.typeCast(listHead(call.arguments), ty))
              @assign call.ty = cast_ty
              P_Expression.Expression.CALL(call)
            end

            _ => begin
              P_Expression.Expression.CAST(cast_ty, callExp)
            end
          end
        end
      end

      _ => begin
        P_Expression.Expression.CAST(Type.setArrayElementType(typeOf(call), ty), callExp)
      end
    end
  end
  return callExp
end

function retype(call::Call)::Call

  @assign () = begin
    local ty::M_Type
    local dims::List{Dimension}
    @match call begin
      TYPED_ARRAY_CONSTRUCTOR(__) => begin
        @assign dims = nil
        for i in listReverse(call.iters)
          @assign dims = listAppend(
            Type.arrayDims(P_Expression.Expression.typeOf(Util.tuple22(i))),
            dims,
          )
        end
        @assign call.ty = Type.liftArrayLeftList(Type.arrayElementType(call.ty), dims)
        ()
      end

      _ => begin
        ()
      end
    end
  end
  return call
end

function isVectorizeable(call::Call)::Bool
  local isVect::Bool

  @assign isVect = begin
    local name::String
    @match call begin
      TYPED_CALL(fn = P_Function.FUNCTION(path = Absyn.IDENT(name = name))) => begin
        begin
          @match name begin
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

function toDAE(call::Call)::DAE.Exp
  local daeCall::DAE.Exp

  @assign daeCall = begin
    local fold_id::String
    local res_id::String
    local fold_exp::Option{Expression}
    @match call begin
      TYPED_CALL(__) => begin
        DAE.CALL(
          P_Function.nameConsiderBuiltin(call.fn),
          List(P_Expression.Expression.toDAE(e) for e in call.arguments),
          P_CallAttributes.toDAE(call.attributes, call.ty),
        )
      end

      TYPED_ARRAY_CONSTRUCTOR(__) => begin
        @assign fold_id = Util.getTempVariableIndex()
        @assign res_id = Util.getTempVariableIndex()
        DAE.REDUCTION(
          DAE.REDUCTIONINFO(
            P_Function.name(NFBuiltinFuncs.ARRAY_FUNC),
            Absyn.COMBINE(),
            Type.toDAE(call.ty),
            NONE(),
            fold_id,
            res_id,
            NONE(),
          ),
          P_Expression.Expression.toDAE(call.exp),
          List(iteratorToDAE(iter) for iter in call.iters),
        )
      end

      TYPED_REDUCTION(__) => begin
        @assign (fold_exp, fold_id, res_id) = call.foldExp
        DAE.REDUCTION(
          DAE.REDUCTIONINFO(
            P_Function.name(call.fn),
            Absyn.COMBINE(),
            Type.toDAE(call.ty),
            P_Expression.Expression.toDAEValueOpt(call.defaultExp),
            fold_id,
            res_id,
            P_Expression.Expression.toDAEOpt(fold_exp),
          ),
          P_Expression.Expression.toDAE(call.exp),
          List(iteratorToDAE(iter) for iter in call.iters),
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

  @assign str = begin
    @match call begin
      ARG_TYPED_CALL(__) => begin
        @assign name = toString(call.ref)
        @assign arg_str = stringDelimitList(
          List(
            "/*" +
            Type.toString(Util.tuple32(arg)) +
            "*/ " +
            P_Expression.Expression.toString(Util.tuple31(arg))
            for arg in call.arguments
          ),
          ", ",
        )
        for arg in call.named_args
          @assign c = if arg_str == ""
            ""
          else
            ", "
          end
          @assign arg_str =
            arg_str +
            c +
            Util.tuple41(arg) +
            " = /*" +
            Type.toString(Util.tuple43(arg)) +
            "*/ " +
            P_Expression.Expression.toString(Util.tuple42(arg))
        end
        name + "(" + arg_str + ")"
      end

      TYPED_CALL(__) => begin
        @assign name = AbsynUtil.pathString(P_Function.name(call.fn))
        @assign arg_str = stringDelimitList(
          List(P_Expression.Expression.toStringTyped(arg) for arg in call.arguments),
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

  local name::String
  local arg_str::String
  local c::String
  local argexp::Expression
  local iters::List{InstNode}

  @assign str = begin
    @match call begin
      TYPED_CALL(__) => begin
        @assign name = AbsynUtil.pathString(P_Function.name(call.fn))
        @assign arg_str = stringDelimitList(
          List(P_Expression.Expression.toFlatString(arg) for arg in call.arguments),
          ", ",
        )
        if P_Function.isBuiltin(call.fn)
          stringAppendList(list(name, "(", arg_str, ")"))
        else
          stringAppendList(list("'", name, "'(", arg_str, ")"))
        end
      end

      TYPED_ARRAY_CONSTRUCTOR(__) => begin
        if isVectorized(call)
          @assign str = toFlatString(devectorizeCall(call))
        else
          @assign name = AbsynUtil.pathString(P_Function.name(NFBuiltinFuncs.ARRAY_FUNC))
          @assign arg_str = P_Expression.Expression.toFlatString(call.exp)
          @assign c = stringDelimitList(
            List(
              name(Util.tuple21(iter)) +
              " in " +
              P_Expression.Expression.toFlatString(Util.tuple22(iter))
              for iter in call.iters
            ),
            ", ",
          )
          @assign str = stringAppendList(list("{", arg_str, " for ", c, "}"))
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
        @assign name = AbsynUtil.pathString(P_Function.name(call.fn))
        @assign arg_str = P_Expression.Expression.toFlatString(call.exp)
        @assign c = stringDelimitList(
          List(
            name(Util.tuple21(iter)) +
            " in " +
            P_Expression.Expression.toFlatString(Util.tuple22(iter))
            for iter in call.iters
          ),
          ", ",
        )
        if P_Function.isBuiltin(call.fn)
          stringAppendList(list(name, "(", arg_str, " for ", c, ")"))
        else
          stringAppendList(list("'", name, "'(", arg_str, " for ", c, ")"))
        end
      end
    end
  end
  return str
end

function toString(call::Call)::String
  local str::String

  local name::String
  local arg_str::String
  local c::String
  local argexp::Expression
  local iters::List{InstNode}

  @assign str = begin
    @match call begin
      UNTYPED_CALL(__) => begin
        @assign name = toString(call.ref)
        @assign arg_str = stringDelimitList(
          List(P_Expression.Expression.toString(arg) for arg in call.arguments),
          ", ",
        )
        name + "(" + arg_str + ")"
      end

      ARG_TYPED_CALL(__) => begin
        @assign name = toString(call.ref)
        @assign arg_str = stringDelimitList(
          List(
            P_Expression.Expression.toString(Util.tuple31(arg)) for arg in call.arguments
          ),
          ", ",
        )
        for arg in call.named_args
          @assign c = if arg_str == ""
            ""
          else
            ", "
          end
          @assign arg_str =
            arg_str +
            c +
            Util.tuple41(arg) +
            " = " +
            P_Expression.Expression.toString(Util.tuple42(arg))
        end
        name + "(" + arg_str + ")"
      end

      UNTYPED_ARRAY_CONSTRUCTOR(__) => begin
        @assign name = AbsynUtil.pathString(P_Function.name(NFBuiltinFuncs.ARRAY_FUNC))
        @assign arg_str = P_Expression.Expression.toString(call.exp)
        @assign c = stringDelimitList(
          List(
            name(Util.tuple21(iter)) +
            " in " +
            P_Expression.Expression.toString(Util.tuple22(iter))
            for iter in call.iters
          ),
          ", ",
        )
        "{" + arg_str
        +" for " + c + "}"
      end

      UNTYPED_REDUCTION(__) => begin
        @assign name = toString(call.ref)
        @assign arg_str = P_Expression.Expression.toString(call.exp)
        @assign c = stringDelimitList(
          List(
            name(Util.tuple21(iter)) +
            " in " +
            P_Expression.Expression.toString(Util.tuple22(iter))
            for iter in call.iters
          ),
          ", ",
        )
        name + "(" + arg_str + " for " + c + ")"
      end

      TYPED_CALL(__) => begin
        @assign name = AbsynUtil.pathString(P_Function.name(call.fn))
        @assign arg_str = stringDelimitList(
          List(P_Expression.Expression.toString(arg) for arg in call.arguments),
          ", ",
        )
        name + "(" + arg_str + ")"
      end

      TYPED_ARRAY_CONSTRUCTOR(__) => begin
        @assign name = AbsynUtil.pathString(P_Function.name(NFBuiltinFuncs.ARRAY_FUNC))
        @assign arg_str = P_Expression.Expression.toString(call.exp)
        @assign c = stringDelimitList(
          List(
            name(Util.tuple21(iter)) +
            " in " +
            P_Expression.Expression.toString(Util.tuple22(iter))
            for iter in call.iters
          ),
          ", ",
        )
        "{" + arg_str
        +" for " + c + "}"
      end

      TYPED_REDUCTION(__) => begin
        @assign name = AbsynUtil.pathString(P_Function.name(call.fn))
        @assign arg_str = P_Expression.Expression.toString(call.exp)
        @assign c = stringDelimitList(
          List(
            name(Util.tuple21(iter)) +
            " in " +
            P_Expression.Expression.toString(Util.tuple22(iter))
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

function toRecordExpression(call::Call, ty::M_Type)::Expression
  local exp::Expression

  @assign exp = begin
    @match call begin
      TYPED_CALL(__) => begin
        EvalFunction.evaluateRecordConstructor(call.fn, ty, call.arguments, evaluate = false)
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

  @assign arguments = begin
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
  local name::Absyn.Path

  @assign name = begin
    @match call begin
      UNTYPED_CALL(__) => begin
        toPath(call.ref)
      end

      ARG_TYPED_CALL(__) => begin
        toPath(call.ref)
      end

      TYPED_CALL(__) => begin
        P_Function.name(call.fn)
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
        P_Function.name(call.fn)
      end
    end
  end
  return name
end

function typedFunction(call::Call)::M_Function
  local fn::M_Function

  @assign fn = begin
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

  @assign inlineTy = begin
    @match call begin
      TYPED_CALL(attributes = P_CallAttributes.CALL_ATTR(inlineType = inlineTy)) => begin
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

  @assign isConstructor = begin
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
  local isImpure::Bool

  @assign isImpure = begin
    @match call begin
      UNTYPED_CALL(__) => begin
        P_Function.isImpure(listHead(P_Function.getRefCache(call.ref)))
      end

      TYPED_CALL(__) => begin
        P_Function.isImpure(call.fn) || P_Function.isOMImpure(call.fn)
      end

      _ => begin
        false
      end
    end
  end
  return isImpure
end

function isExternal(call::Call)::Bool
  local isExternal::Bool

  @assign isExternal = begin
    @match call begin
      UNTYPED_CALL(__) => begin
        isExternalFunction(getClass(node(call.ref)))
      end

      ARG_TYPED_CALL(__) => begin
        isExternalFunction(getClass(node(call.ref)))
      end

      TYPED_CALL(__) => begin
        P_Function.isExternal(call.fn)
      end

      _ => begin
        false
      end
    end
  end
  return isExternal
end

function compare(call1::Call, call2::Call)::Integer
  local comp::Integer

  @assign comp = begin
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
    @assign comp = P_Expression.Expression.compareList(arguments(call1), arguments(call2))
  end
  return comp
end

function variability(call::Call)::Variability
  local var::Variability

  @assign var = begin
    local var_set::Bool
    @match call begin
      UNTYPED_CALL(__) => begin
        @assign var_set = true
        if isSimple(call.ref)
          @assign var = begin
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
                @assign var_set = false
                Variability.CONTINUOUS
              end
            end
          end
        end
        if !var_set
          @assign var = P_Expression.Expression.variabilityList(call.arguments)
          for narg in call.named_args
            @assign var = P_Prefixes.variabilityMax(
              var,
              P_Expression.Expression.variability(Util.tuple22(narg)),
            )
          end
        end
        var
      end

      UNTYPED_ARRAY_CONSTRUCTOR(__) => begin
        P_Expression.Expression.variability(call.exp)
      end

      UNTYPED_REDUCTION(__) => begin
        P_Expression.Expression.variability(call.exp)
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

function setType(call::Call, ty::M_Type)::Call

  @assign call = begin
    @match call begin
      TYPED_CALL(__) => begin
        @assign call.ty = ty
        call
      end

      TYPED_ARRAY_CONSTRUCTOR(__) => begin
        @assign call.ty = ty
        call
      end

      TYPED_REDUCTION(__) => begin
        @assign call.ty = ty
        call
      end
    end
  end
  return call
end

function typeOf(call::Call)::M_Type
  local ty::M_Type

  @assign ty = begin
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
        Type.UNKNOWN()
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
  local var::Variability
  local arg_var::Variability
  local ty::M_Type
  local arg_exp::Expression

  @match ARG_TYPED_CALL(call_scope = scope) = call
  @assign matchedFunc = checkMatchingFunctions(call, info)
  @assign func = matchedFunc.func
  @assign typed_args = matchedFunc.args
  @assign args = nil
  #=  if is impure, make it a parameter expression
  =#
  #=  see https:trac.openmodelica.org/OpenModelica/ticket/5133
  =#
  @assign var = if P_Function.isImpure(func) || P_Function.isOMImpure(func)
    Variability.PARAMETER
  else
    Variability.CONSTANT
  end
  for a in typed_args
    @assign (arg_exp, _, arg_var) = a
    @assign args = _cons(arg_exp, args)
    @assign var = P_Prefixes.variabilityMax(var, arg_var)
  end
  @assign args = listReverseInPlace(args)
  @assign ty = P_Function.returnType(func)
  #=  Hack to fix return type of some builtin functions.
  =#
  if Type.isPolymorphic(ty)
    @assign ty = getSpecialReturnType(func, args)
  end
  if var == Variability.PARAMETER && P_Function.isExternal(func)
    @assign var = Variability.NON_STRUCTURAL_PARAMETER
  elseif Type.isDiscrete(ty) && var == Variability.CONTINUOUS
    @assign var = Variability.IMPLICITLY_DISCRETE
  end
  #=  Mark external functions with parameter expressions as non-structural,
  =#
  #=  to avoid them being marked as structural unnecessarily.
  =#
  #=  Functions that return a discrete type, e.g. Integer, should probably be
  =#
  #=  treated as implicitly discrete if the arguments are continuous.
  =#
  @assign ty = evaluateCallType(ty, func, args)
  @assign call = makeTypedCall(func, args, var, ty)
  #=  If the matching was a vectorized one then create a map call
  =#
  #=  using the vectorization dim. This means going through each argument
  =#
  #=  and subscipting it with an iterator for each dim and creating a map call.
  =#
  if P_MatchedFunction.isVectorized(matchedFunc)
    @assign call = vectorizeCall(call, matchedFunc.mk, scope, info)
  end
  return call
end

function typeMatchNormalCall(call::Call, origin::ORIGIN_Type, info::SourceInfo)::Call

  local argtycall::Call

  @assign argtycall = typeNormalCall(call, origin, info)
  @assign call = matchTypedNormalCall(argtycall, origin, info)
  return call
end

function unboxArgs(call::Call)::Call

  @assign () = begin
    @match call begin
      TYPED_CALL(__) => begin
        @assign call.arguments =
          List(P_Expression.Expression.unbox(arg) for arg in call.arguments)
        ()
      end
    end
  end
  return call
end

function makeTypedCall(
  fn::M_Function,
  args::List{<:Expression},
  variability::Variability,
  returnType::M_Type = fn.returnType,
)::Call
  local call::Call

  local ca::CallAttributes

  @assign ca = P_CallAttributes.CALL_ATTR(
    Type.isTuple(returnType),
    P_Function.isBuiltin(fn),
    P_Function.isImpure(fn),
    P_Function.isFunctionPointer(fn),
    P_Function.inlineBuiltin(fn),
    DAE.NO_TAIL(),
  )
  @assign call = TYPED_CALL(fn, returnType, variability, args, ca)
  return call
end

function typeNormalCall(call::Call, origin::ORIGIN_Type, info::SourceInfo)::Call

  @assign call = begin
    local fnl::List{M_Function}
    local is_external::Bool
    @match call begin
      UNTYPED_CALL(__) => begin
        @assign fnl = P_Function.typeRefCache(call.ref)
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
  callExp::Expression,
  origin::ORIGIN_Type,
  info::SourceInfo,
)::Tuple{Expression, M_Type, Variability}
  local var::Variability
  local ty::M_Type
  local outExp::Expression

  local call::Call
  local ty_call::Call
  local args::List{Expression}
  local cref::ComponentRef

  @match P_Expression.Expression.CALL(call = call) = callExp
  @assign outExp = begin
    @match call begin
      UNTYPED_CALL(ref = cref) => begin
        if BuiltinCall.needSpecialHandling(call)
          @assign (outExp, ty, var) = BuiltinCall.typeSpecial(call, origin, info)
        else
          @assign ty_call = typeMatchNormalCall(call, origin, info)
          @assign ty = typeOf(ty_call)
          @assign var = variability(ty_call)
          if isRecordConstructor(ty_call)
            @assign outExp = toRecordExpression(ty_call, ty)
          else
            if P_Function.hasUnboxArgs(P_Call.typedFunction(ty_call))
              @assign outExp = P_Expression.Expression.CALL(P_Call.unboxArgs(ty_call))
            else
              @assign outExp = P_Expression.Expression.CALL(ty_call)
            end
            @assign outExp = Inline.inlineCallExp(outExp)
          end
        end
        outExp
      end

      UNTYPED_ARRAY_CONSTRUCTOR(__) => begin
        @assign (ty_call, ty, var) = typeArrayConstructor(call, origin, info)
        P_Expression.Expression.CALL(ty_call)
      end

      UNTYPED_REDUCTION(__) => begin
        @assign (ty_call, ty, var) = typeReduction(call, origin, info)
        P_Expression.Expression.CALL(ty_call)
      end

      TYPED_CALL(__) => begin
        @assign ty = call.ty
        @assign var = call.var
        callExp
      end

      TYPED_ARRAY_CONSTRUCTOR(__) => begin
        @assign ty = call.ty
        @assign var = call.var
        callExp
      end

      TYPED_REDUCTION(__) => begin
        @assign ty = call.ty
        @assign var = call.var
        callExp
      end

      _ => begin
        Error.assertion(
          false,
          getInstanceName() + ": " + P_Expression.Expression.toString(callExp),
          sourceInfo(),
        )
        fail()
      end
    end
  end
  return (outExp, ty, var)
end

function instantiate(
  functionName::Absyn.
  functionArgs::Absyn.FunctionArgs,
  scope::InstNode,
  info::SourceInfo,
)::Expression
  local callExp::Expression

  @assign callExp = begin
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

function getSpecialReturnType(fn::M_Function, args::List{<:Expression})::M_Type
  local ty::M_Type

  @assign ty = begin
    @match fn.path begin
      Absyn.IDENT("min") => begin
        Type.arrayElementType(P_Expression.Expression.typeOf(P_Expression.Expression.unbox(listHead(
          args,
        ))))
      end

      Absyn.IDENT("max") => begin
        Type.arrayElementType(P_Expression.Expression.typeOf(P_Expression.Expression.unbox(listHead(
          args,
        ))))
      end

      Absyn.IDENT("sum") => begin
        Type.arrayElementType(P_Expression.Expression.typeOf(P_Expression.Expression.unbox(listHead(
          args,
        ))))
      end

      Absyn.IDENT("product") => begin
        Type.arrayElementType(P_Expression.Expression.typeOf(P_Expression.Expression.unbox(listHead(
          args,
        ))))
      end

      Absyn.IDENT("previous") => begin
        P_Expression.Expression.typeOf(P_Expression.Expression.unbox(listHead(args)))
      end

      Absyn.IDENT("shiftSample") => begin
        P_Expression.Expression.typeOf(P_Expression.Expression.unbox(listHead(args)))
      end

      Absyn.IDENT("backSample") => begin
        P_Expression.Expression.typeOf(P_Expression.Expression.unbox(listHead(args)))
      end

      Absyn.IDENT("hold") => begin
        P_Expression.Expression.typeOf(P_Expression.Expression.unbox(listHead(args)))
      end

      Absyn.IDENT("superSample") => begin
        P_Expression.Expression.typeOf(P_Expression.Expression.unbox(listHead(args)))
      end

      Absyn.IDENT("subSample") => begin
        P_Expression.Expression.typeOf(P_Expression.Expression.unbox(listHead(args)))
      end

      Absyn.IDENT("DynamicSelect") => begin
        P_Expression.Expression.typeOf(P_Expression.Expression.unbox(listHead(args)))
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

  @assign outExp = begin
    local node::InstNode
    local oexp::Option{Expression}
    local e::Expression
    @match exp begin
      CREF_EXPRESSION(
        cref = CREF(
          node = node,
          restCref = EMPTY(__),
        ),
      ) => begin
        @assign oexp = ParameterTree.getOpt(ptree, name(node))
        if isSome(oexp)
          @match SOME(outExp) = oexp
        end
        #=  TODO: Apply subscripts.
        =#
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

  if !ParameterTree.isEmpty(ptree)
    return ptree
  end
  @assign (fn, args) = fnArgs
  for i in fn.inputs
    @match _cons(arg, args) = args
    @assign ptree = ParameterTree.add(ptree, name(i), arg)
  end
  #=  TODO: Add local variable bindings.
  =#
  return ptree
end

function evaluateCallTypeDim(
  dim::Dimension,
  fnArgs::Tuple{<:M_Function, List{<:Expression}},
  ptree::ParameterTree,
)::Tuple{Dimension, ParameterTree}

  @assign dim = begin
    local exp::Expression
    @match dim begin
      P_Dimension.Dimension.EXP(__) => begin
        @assign ptree = buildParameterTree(fnArgs, ptree)
        @assign exp = P_Expression.Expression.map(
          dim.exp,
          (ptree) -> evaluateCallTypeDimExp(ptree = ptree),
        )
        ErrorExt.setCheckpoint(getInstanceName())
        try
          @assign exp = Ceval.evalExp(exp, Ceval.P_EvalTarget.IGNORE_ERRORS())
        catch
        end
        ErrorExt.rollBack(getInstanceName())
        P_Dimension.Dimension.fromExp(exp, Variability.CONSTANT)
      end

      _ => begin
        dim
      end
    end
  end
  return (dim, ptree)
end

function evaluateCallType(
  ty::M_Type,
  fn::M_Function,
  args::List{<:Expression},
  ptree::ParameterTree = ParameterTree.EMPTY(),
)::Tuple{M_Type, ParameterTree}

  @assign ty = begin
    local dims::List{Dimension}
    local tys::List{M_Type}
    @match ty begin
      Type.ARRAY(__) => begin
        @assign (dims, ptree) =
          ListUtil.map1Fold(ty.dimensions, evaluateCallTypeDim, (fn, args), ptree)
        @assign ty.dimensions = dims
        ty
      end

      TYPE_TUPLE(__) => begin
        @assign (tys, ptree) =
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
    @assign (iter_node, iter_exp) = i
    @assign exp = P_Expression.Expression.replaceIterator(exp, iter_node, iter_exp)
  end
  @match P_Expression.Expression.CALL(call = outCall) = exp
  return outCall
end

function isVectorized(call::Call)::Bool
  local vectorized::Bool

  @assign vectorized = begin
    @match call begin
      TYPED_ARRAY_CONSTRUCTOR(exp = P_Expression.Expression.CALL(__)) => begin
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

  local ty::M_Type
  local vect_ty::M_Type
  local exp::Expression
  local iters::List{Tuple{InstNode, Expression}}
  local iter::InstNode
  local i::Integer
  local vect_idx::Integer
  local b::Bool
  local call_args::List{Expression}
  local vect_args::List{Expression}
  local sub::Subscript
  local vect_idxs::List{Integer}

  @assign vectorized_call = begin
    @match (base_call, mk) begin
      (TYPED_CALL(arguments = call_args), VECTORIZED(__)) => begin
        @assign iters = nil
        @assign i = 1
        for dim in mk.vectDims
          Error.assertion(
            P_Dimension.Dimension.isKnown(dim, allowExp = true),
            getInstanceName() + " got unknown dimension for vectorized call",
            info,
          )
          @assign ty = Type.ARRAY(TYPE_INTEGER(), list(dim))
          @assign exp = P_Expression.Expression.RANGE(
            ty,
            P_Expression.Expression.INTEGER(1),
            NONE(),
            P_Dimension.Dimension.sizeExp(dim),
          )
          @assign iter = fromComponent(
            "i" + intString(i),
            P_Component.ITERATOR(TYPE_INTEGER(), Variability.CONSTANT, info),
            scope,
          )
          @assign iters = _cons((iter, exp), iters)
          @assign exp = CREF_EXPRESSION(
            TYPE_INTEGER(),
            makeIterator(iter, TYPE_INTEGER()),
          )
          @assign sub = SUBSCRIPT_INDEX(exp)
          @assign call_args = ListUtil.mapIndices(
            call_args,
            mk.vectorizedArgs,
            (sub, nil()) -> P_Expression.Expression.applySubscript(
              subscript = sub,
              restSubscripts = nil,
            ),
          )
          @assign i = i + 1
        end
        #=  Create the range on which we will iterate to vectorize.
        =#
        #=  Create the iterator.
        =#
        #=  Now that iterator is ready apply it, as a subscript, to each argument that is supposed to be vectorized
        =#
        #=  Make a cref expression from the iterator
        =#
        @assign vect_ty = Type.liftArrayLeftList(base_call.ty, mk.vectDims)
        @assign base_call.arguments = call_args
        TYPED_ARRAY_CONSTRUCTOR(
          vect_ty,
          base_call.var,
          P_Expression.Expression.CALL(base_call),
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

  @assign (iter_node, iter_range) = iter
  @assign diter = DAE.REDUCTIONITER(
    name(iter_node),
    P_Expression.Expression.toDAE(iter_range),
    NONE(),
    Type.toDAE(getType(iter_node)),
  )
  return diter
end

function checkMatchingFunctions(call::Call, info::SourceInfo)::MatchedFunction
  local matchedFunc::MatchedFunction

  local matchedFunctions::List{MatchedFunction}
  local exactMatches::List{MatchedFunction}
  local func::M_Function
  local allfuncs::List{M_Function}
  local fn_node::InstNode
  local numerr::Integer = Error.getNumErrorMessages()
  local errors::List{Integer}

  ErrorExt.setCheckpoint("NFCall:checkMatchingFunctions")
  @assign matchedFunctions = begin
    @match call begin
      ARG_TYPED_CALL(ref = CREF(node = fn_node)) => begin
        @assign allfuncs = P_Function.getCachedFuncs(fn_node)
        if listLength(allfuncs) > 1
          @assign allfuncs =
            List(fn for fn in allfuncs if !P_Function.isDefaultRecordConstructor(fn))
        end
        P_Function.matchFunctions(allfuncs, call.arguments, call.named_args, info)
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
    elseif numerr == Error.getNumErrorMessages()
      ErrorExt.rollBack("NFCall:checkMatchingFunctions")
      Error.addSourceMessage(
        Error.NO_MATCHING_FUNCTION_FOUND_NFINST,
        list(typedString(call), P_Function.candidateFuncListString(allfuncs)),
        info,
      )
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
    @assign exactMatches = P_MatchedFunction.getExactMatches(matchedFunctions)
    if listEmpty(exactMatches)
      @assign exactMatches = P_MatchedFunction.getExactVectorizedMatches(matchedFunctions)
    end
    if listLength(exactMatches) > 1
      Error.addSourceMessage(
        Error.AMBIGUOUS_MATCHING_FUNCTIONS_NFINST,
        list(
          typedString(call),
          P_Function.candidateFuncListString(List(mfn.func for mfn in matchedFunctions)),
        ),
        info,
      )
      fail()
    end
    @assign matchedFunc = listHead(exactMatches)
  else
    @assign matchedFunc = listHead(matchedFunctions)
  end
  #=  Overwrite the actual function name with the overload name for builtin functions.
  =#
  if P_Function.isBuiltin(matchedFunc.func)
    @assign func = matchedFunc.func
    @assign func.path = P_Function.nameConsiderBuiltin(func)
    @assign matchedFunc.func = func
  end
  return matchedFunc
end

function typeArgs(call::Call, origin::ORIGIN_Type, info::SourceInfo)::Call

  @assign call = begin
    local arg::Expression
    local arg_ty::M_Type
    local arg_var::Variability
    local typedArgs::List{TypedArg}
    local typedNamedArgs::List{TypedNamedArg}
    local name::String
    local next_origin::ORIGIN_Type
    @match call begin
      UNTYPED_CALL(__) => begin
        @assign typedArgs = nil
        @assign next_origin = ExpOrigin.setFlag(origin, ExpOrigin.SUBEXPRESSION)
        for arg in call.arguments
          @assign (arg, arg_ty, arg_var) = Typing.typeExp(arg, next_origin, info)
          @assign typedArgs = _cons((arg, arg_ty, arg_var), typedArgs)
        end
        @assign typedArgs = listReverse(typedArgs)
        @assign typedNamedArgs = nil
        for narg in call.named_args
          @assign (name, arg) = narg
          @assign (arg, arg_ty, arg_var) = Typing.typeExp(arg, next_origin, info)
          @assign typedNamedArgs = _cons((name, arg, arg_ty, arg_var), typedNamedArgs)
        end
        @assign typedNamedArgs = listReverse(typedNamedArgs)
        ARG_TYPED_CALL(call.ref, typedArgs, typedNamedArgs, call.call_scope)
      end
    end
  end
  return call
end

function reductionFoldIterator(name::String, ty::M_Type)::Expression
  local iterExp::Expression

  @assign iterExp = CREF_EXPRESSION(
    ty,
    makeIterator(NAME_NODE(name), ty),
  )
  return iterExp
end

function reductionFoldExpression(
  reductionFn::M_Function,
  reductionType::M_Type,
  reductionVar::Variability,
  foldId::String,
  resultId::String,
)::Option{Expression}
  local foldExp::Option{Expression}

  local ty::M_Type
  local op_node::InstNode
  local fn::M_Function

  if Type.isComplex(reductionType)
    @assign foldExp = begin
      @match AbsynUtil.pathFirstIdent(P_Function.name(reductionFn)) begin
        "sum" => begin
          @match TYPE_COMPLEX(cls = op_node) = reductionType
          @assign op_node = lookupElement("'+'", getClass(op_node))
          P_Function.instFunctionNode(op_node)
          @match list(fn) = P_Function.typeNodeCache(op_node)
          SOME(P_Expression.Expression.CALL(makeTypedCall(
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
    @assign foldExp = begin
      @match AbsynUtil.pathFirstIdent(P_Function.name(reductionFn)) begin
        "sum" => begin
          SOME(BINARY_EXPRESSION(
            reductionFoldIterator(resultId, reductionType),
            P_Operator.Operator.makeAdd(reductionType),
            reductionFoldIterator(foldId, reductionType),
          ))
        end

        "product" => begin
          SOME(BINARY_EXPRESSION(
            reductionFoldIterator(resultId, reductionType),
            P_Operator.Operator.makeMul(reductionType),
            reductionFoldIterator(foldId, reductionType),
          ))
        end

        "$array" => begin
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
          SOME(P_Expression.Expression.CALL(P_Call.makeTypedCall(
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

function reductionDefaultValue(fn::M_Function, ty::M_Type)::Option{Expression}
  local defaultValue::Option{Expression}

  if Type.isArray(ty)
    @assign defaultValue = NONE()
  else
    @assign defaultValue = begin
      @match AbsynUtil.pathFirstIdent(P_Function.name(fn)) begin
        "sum" => begin
          SOME(P_Expression.Expression.makeZero(ty))
        end

        "product" => begin
          SOME(P_Expression.Expression.makeOne(ty))
        end

        "min" => begin
          SOME(P_Expression.Expression.makeMaxValue(ty))
        end

        "max" => begin
          SOME(P_Expression.Expression.makeMinValue(ty))
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
)::Tuple{Call, M_Type, Variability}
  local variability::Variability
  local ty::M_Type

  local range::Expression
  local arg::Expression
  local default_exp::Option{Expression}
  local fold_exp::Option{Expression}
  local iter::InstNode
  local iter_var::Variability
  local exp_var::Variability
  local iters::List{Tuple{InstNode, Expression}} = nil
  local next_origin::ORIGIN_Type
  local fn::M_Function
  local fold_id::String
  local res_id::String
  local fold_tuple::Tuple{Option{Expression}, String, String}

  @assign (call, ty, variability) = begin
    @match call begin
      UNTYPED_REDUCTION(__) => begin
        @assign variability = Variability.CONSTANT
        @assign next_origin = ExpOrigin.setFlag(origin, ExpOrigin.SUBEXPRESSION)
        for i in call.iters
          @assign (iter, range) = i
          @assign (range, _, iter_var) =
            Typing.typeIterator(iter, range, origin, structural = false)
          @assign variability = Variability.variabilityMax(variability, iter_var)
          @assign iters = _cons((iter, range), iters)
        end
        @assign iters = listReverseInPlace(iters)
        #=  ExpOrigin.FOR is used here as a marker that this expression may contain iterators.
        =#
        @assign next_origin = intBitOr(next_origin, ExpOrigin.FOR)
        @assign (arg, ty, exp_var) = Typing.typeExp(call.exp, next_origin, info)
        @assign variability = Variability.variabilityMax(variability, exp_var)
        @match list(fn) = P_Function.typeRefCache(call.ref)
        TypeCheck.checkReductionType(ty, P_Function.name(fn), call.exp, info)
        @assign fold_id = Util.getTempVariableIndex()
        @assign res_id = Util.getTempVariableIndex()
        @assign default_exp = reductionDefaultValue(fn, ty)
        @assign fold_exp = reductionFoldExpression(fn, ty, variability, fold_id, res_id)
        @assign fold_tuple = (fold_exp, fold_id, res_id)
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
  return (call, ty, variability)
end

function typeArrayConstructor(
  call::Call,
  origin::ORIGIN_Type,
  info::SourceInfo,
)::Tuple{Call, M_Type, Variability}
  local variability::Variability
  local ty::M_Type

  local arg::Expression
  local range::Expression
  local iter_ty::M_Type
  local iter_var::Variability
  local exp_var::Variability
  local iter::InstNode
  local dims::List{Dimension} = nil
  local iters::List{Tuple{InstNode, Expression}} = nil
  local next_origin::ORIGIN_Type
  local is_structural::Bool

  @assign (call, ty, variability) = begin
    @match call begin
      UNTYPED_ARRAY_CONSTRUCTOR(__) => begin
        @assign variability = Variability.CONSTANT
        #=  The size of the expression must be known unless we're in a function.
        =#
        @assign is_structural = ExpOrigin.flagNotSet(origin, ExpOrigin.FUNCTION)
        @assign next_origin = ExpOrigin.setFlag(origin, ExpOrigin.SUBEXPRESSION)
        for i in call.iters
          @assign (iter, range) = i
          @assign (range, iter_ty, iter_var) =
            Typing.typeIterator(iter, range, next_origin, is_structural)
          if is_structural
            @assign range = Ceval.evalExp(range, Ceval.P_EvalTarget.RANGE(info))
            @assign iter_ty = P_Expression.Expression.typeOf(range)
          end
          @assign dims = listAppend(Type.arrayDims(iter_ty), dims)
          @assign variability = Variability.variabilityMax(variability, iter_var)
          @assign iters = _cons((iter, range), iters)
        end
        @assign iters = listReverseInPlace(iters)
        #=  ExpOrigin.FOR is used here as a marker that this expression may contain iterators.
        =#
        @assign next_origin = intBitOr(next_origin, ExpOrigin.FOR)
        @assign (arg, ty, exp_var) = Typing.typeExp(call.exp, next_origin, info)
        @assign variability = Variability.variabilityMax(variability, exp_var)
        @assign ty = Type.liftArrayLeftList(ty, dims)
        @assign variability = Variability.variabilityMax(variability, exp_var)
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
    @assign range = Inst.instExp(Util.getOption(i.range), outScope, info)
    @assign (outScope, iter) = Inst.addIteratorToScope(i.name, outScope, info)
    @assign outIters = _cons((iter, range), outIters)
  end
  @assign outIters = listReverse(outIters)
  return (outScope, outIters)
end

function instIteratorCallArgs(
  args::Absyn.FunctionArgs,
  scope::InstNode,
  info::SourceInfo,
)::Tuple{Expression, List{Tuple{InstNode, Expression}}}
  local iters::List{Tuple{InstNode, Expression}}
  local exp::Expression

  @assign _ = begin
    local for_scope::InstNode
    @match args begin
      Absyn.FOR_ITER_FARG(__) => begin
        @assign (for_scope, iters) = instIterators(args.iterators, scope, info)
        @assign exp = Inst.instExp(args.exp, for_scope, info)
        ()
      end
    end
  end
  return (exp, iters)
end

function instIteratorCall(
  functionName::Absyn.
  functionArgs::Absyn.FunctionArgs,
  scope::InstNode,
  info::SourceInfo,
)::Expression
  local callExp::Expression

  local fn_name::Absyn.P_ComponentRef.ComponentRef
  local fn_ref::ComponentRef
  local exp::Expression
  local iters::List{Tuple{InstNode, Expression}}
  local is_array::Bool

  #=  The parser turns {exp for i in ...} into $array(exp for i in ...), but we
  =#
  #=  change it to just array here so we can handle array constructors uniformly.
  =#
  @assign fn_name = begin
    @match functionName begin
      Absyn.CREF_IDENT("$array") => begin
        Absyn.CREF_IDENT("array", nil)
      end

      _ => begin
        functionName
      end
    end
  end
  @assign (exp, iters) = instIteratorCallArgs(functionArgs, scope, info)
  if AbsynUtil.crefFirstIdent(fn_name) == "array"
    @assign callExp = P_Expression.Expression.CALL(UNTYPED_ARRAY_CONSTRUCTOR(exp, iters))
  else
    @assign fn_ref = P_Function.instFunction(fn_name, scope, info)
    @assign callExp = P_Expression.Expression.CALL(UNTYPED_REDUCTION(fn_ref, exp, iters))
  end
  return callExp
end

function instNamedArg(absynArg::Absyn.NamedArg, scope::InstNode, info::SourceInfo)::NamedArg
  local arg::NamedArg

  local name::String
  local exp::Absyn.Exp

  @match Absyn.NAMEDARG(argName = name, argValue = exp) = absynArg
  @assign arg = (name, Inst.instExp(exp, scope, info))
  return arg
end

function instArgs(
  args::Absyn.FunctionArgs,
  scope::InstNode,
  info::SourceInfo,
)::Tuple{List{Expression}, List{NamedArg}}
  local namedArgs::List{NamedArg}
  local posArgs::List{Expression}

  @assign (posArgs, namedArgs) = begin
    @match args begin
      Absyn.FUNCTIONARGS(__) => begin
        @assign posArgs = List(Inst.instExp(a, scope, info) for a in args.args)
        @assign namedArgs = List(instNamedArg(a, scope, info) for a in args.argNames)
        (posArgs, namedArgs)
      end

      _ => begin
        Error.assertion(false, getInstanceName() + " got unknown function args", sourceInfo())
        fail()
      end
    end
  end
  return (posArgs, namedArgs)
end

function instNormalCall(
  functionName::Absyn.
  functionArgs::Absyn.FunctionArgs,
  scope::InstNode,
  info::SourceInfo,
)::Expression
  local callExp::Expression

  local fn_ref::ComponentRef
  local args::List{Expression}
  local named_args::List{NamedArg}
  local name::String

  @assign name = AbsynUtil.crefFirstIdent(functionName)
  #=  try to inst the parameters
  =#
  try
    @assign (args, named_args) = instArgs(functionArgs, scope, info)
  catch
    if Config.getGraphicsExpMode() && stringEq(name, "DynamicSelect")
      @assign callExp = begin
        @match functionArgs begin
          Absyn.FUNCTIONARGS(__) => begin
            Inst.instExp(listHead(functionArgs.args), scope, info)
          end
        end
      end
      return callExp
    else
      fail()
    end
  end
  #=  didn't work, is this DynamicSelect dynamic part?! #5631
  =#
  #=  return just the first part of DynamicSelect
  =#
  @assign callExp = begin
    @match name begin
      "size" => begin
        BuiltinCall.makeSizeExp(args, named_args, info)
      end

      "array" => begin
        BuiltinCall.makeArrayExp(args, named_args, info)
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
        @assign fn_ref = P_Function.instFunction(functionName, scope, info)
        P_Expression.Expression.CALL(UNTYPED_CALL(fn_ref, args, named_args, scope))
      end
    end
  end
  return callExp
end

@Uniontype Call begin
  @Record TYPED_REDUCTION begin

    fn::M_Function
    ty::M_Type
    var::Variability
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

    ty::M_Type
    var::Variability
    exp::Expression
    iters::List{Tuple{InstNode, Expression}}
  end

  @Record UNTYPED_ARRAY_CONSTRUCTOR begin

    exp::Expression
    iters::List{Tuple{InstNode, Expression}}
  end

  @Record TYPED_CALL begin

    fn::M_Function
    ty::M_Type
    var::Variability
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

@exportAll()
end

@exportAll()
end
