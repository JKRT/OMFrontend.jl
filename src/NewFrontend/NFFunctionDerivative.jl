@UniontypeDecl NFFunctionDerivative
FunctionDerivative = NFFunctionDerivative

Condition = (() -> begin #= Enumeration =#
  ZERO_DERIVATIVE = 1
  NO_DERIVATIVE = 2
  () -> (ZERO_DERIVATIVE; NO_DERIVATIVE)
             end)()
ConditionType = Int


@Uniontype NFFunctionDerivative begin
  @Record FUNCTION_DER begin
    derivativeFn::InstNode
    derivedFn::InstNode
    order::Expression
    conditions::List{Tuple{Integer, ConditionType}}
    lowerOrderDerivatives::List{InstNode}
  end
end

import ..SCodeUtil
import ..SCodeDump

function conditionToDAE(
  cond::Tuple{<:Integer, ConditionType},
)::Tuple{Integer, DAE.derivativeCond}
  local daeCond::Tuple{Integer, DAE.derivativeCond}

  local idx::Integer
  local c::ConditionType

  @assign (idx, c) = cond
  @assign daeCond = begin
    @match c begin
      Condition.ZERO_DERIVATIVE => begin
        (idx, DAE.derivativeCond.ZERO_DERIVATIVE())
      end

      Condition.NO_DERIVATIVE => begin
        (idx, DAE.derivativeCond.NO_DERIVATIVE(DAE.Exp.ICONST(99)))
      end
    end
  end
  return daeCond
end

function toDAE(fnDer::FunctionDerivative)::DAE.FunctionDefinition
  local derDef::DAE.FunctionDefinition

  local order::Integer

  @match INTEGER_EXPRESSION(order) = fnDer.order
  @assign derDef = DAE.FunctionDefinition.FUNCTION_DER_MAPPER(
    P_Function.name(listHead(P_Function.getCachedFuncs(fnDer.derivedFn))),
    P_Function.name(listHead(P_Function.getCachedFuncs(fnDer.derivativeFn))),
    order,
    List(conditionToDAE(c) for c in fnDer.conditions),
    NONE(),
    List(
      P_Function.name(listHead(P_Function.getCachedFuncs(fn)))
      for fn in fnDer.lowerOrderDerivatives
    ),
  )
  #=  TODO: Figure out if the two fields below are needed. =#
  return derDef
end

function typeDerivative(fnDer::FunctionDerivative)
  local mk::MatchKind
  local order::Expression
  local order_ty::M_Type
  local var::VariabilityType
  local info::SourceInfo

  P_Function.typeNodeCache(fnDer.derivativeFn)
  @assign info = info(fnDer.derivedFn)
  @assign (order, order_ty, var) = typeExp(fnDer.order, ORIGIN_FUNCTION, info)
  @assign (order, _, mk) = TypeCheck.matchTypes(order_ty, TYPE_INTEGER(), order)
  if TypeCheck.isIncompatibleMatch(mk)
    Error.addSourceMessage(
      Error.VARIABLE_BINDING_TYPE_MISMATCH,
      list(
        "order",
        toString(order),
        "Integer",
        Type.toString(order_ty),
      ),
      info,
    )
    fail()
  end
  if var > Variability.CONSTANT
    Error.addSourceMessage(
      Error.HIGHER_VARIABILITY_BINDING,
      list(
        "order",
        P_Prefixes.variabilityString(Variability.CONSTANT),
        toString(order),
        P_Prefixes.variabilityString(var),
      ),
      info,
    )
    fail()
  end
  return @assign order = Ceval.evalExp(order, P_EvalTarget.GENERIC(info))
end

function instDerivatives(fnNode::InstNode, fn::M_Function)::List{FunctionDerivative}
  local ders::List{FunctionDerivative} = nil

  local der_mods::List{SCode.Mod}
  local scope::InstNode

  @assign der_mods = getDerivativeAnnotations(definition(fnNode))
  @assign scope = parent(fnNode)
  for m in der_mods
    @assign ders = instDerivativeMod(m, fnNode, fn, scope, ders)
  end
  return ders
end

function addLowerOrderDerivative2(fn::M_Function, lowerDerNode::InstNode)::M_Function

  @assign fn.derivatives = List(
    begin
      @match fn_der begin
        FUNCTION_DER(__) => begin
          @assign fn_der.lowerOrderDerivatives =
            _cons(lowerDerNode, fn_der.lowerOrderDerivatives)
          fn_der
        end
      end
    end for fn_der in fn.derivatives
  )
  return fn
end

function addLowerOrderDerivative(fnNode::InstNode, lowerDerNode::InstNode)
  return P_Function.mapCachedFuncs(
    fnNode,
    (lowerDerNode) -> addLowerOrderDerivative2(lowerDerNode = lowerDerNode),
  )
end

function getInputIndex(name::String, fn::M_Function, info::SourceInfo)::Integer
  local index::Integer = 1

  for i in fn.inputs
    if name(i) == name
      return index
    end
    @assign index = index + 1
  end
  Error.addSourceMessage(
    Error.INVALID_FUNCTION_DERIVATIVE_INPUT,
    list(name, AbsynUtil.pathString(P_Function.name(fn))),
    info,
  )
  fail()
  return index
end

function getDerivativeAttributes(
  attrs::List{<:SCode.SubMod},
  fn::M_Function,
  scope::InstNode,
  info::SourceInfo,
)::Tuple{Expression, List{Tuple{Integer, ConditionType}}}
  local conditions::List{Tuple{Integer, ConditionType}} = nil
  local order::Expression = P_Expression.Expression.EMPTY(TYPE_UNKNOWN())

  local id::String
  local mod::SCode.Mod
  local aexp::Absyn.Exp
  local acref::Absyn.P_ComponentRef.ComponentRef
  local index::Integer

  for attr in attrs
    @match SCode.SubMod.NAMEMOD(id, mod) = attr
    @assign () = begin
      @match (id, mod) begin
        ("order", SCode.Mod.MOD(binding = SOME(aexp))) => begin
          if !P_Expression.Expression.isEmpty(order)
            Error.addSourceMessage(
              Error.DUPLICATE_MODIFICATIONS,
              list(id, "derivative"),
              info,
            )
          end
          @assign order = Inst.instExp(aexp, scope, info)
          ()
        end

        (
          "noDerivative",
          SCode.Mod.MOD(
            binding = SOME(Absyn.CREF(componentRef = Absyn.CREF_IDENT(name = id))),
          ),
        ) => begin
          @assign index = getInputIndex(id, fn, info)
          @assign conditions = _cons((index, Condition.NO_DERIVATIVE), conditions)
          ()
        end

        (
          "zeroDerivative",
          SCode.Mod.MOD(
            binding = SOME(Absyn.CREF(componentRef = Absyn.CREF_IDENT(name = id))),
          ),
        ) => begin
          @assign index = getInputIndex(id, fn, info)
          @assign conditions = _cons((index, Condition.ZERO_DERIVATIVE), conditions)
          ()
        end

        _ => begin
          Error.addStrictMessage(
            Error.INVALID_FUNCTION_DERIVATIVE_ATTR,
            list(id + (
              if SCodeUtil.isEmptyMod(mod)
                ""
              else
                " = " + SCodeDump.printModStr(mod)
              end
            )),
            info,
          )
          ()
        end
      end
    end
  end
  if P_Expression.Expression.isEmpty(order)
    @assign order = INTEGER_EXPRESSION(1)
  end
  return (order, conditions)
end

function instDerivativeMod(
  mod::SCode.Mod,
  fnNode::InstNode,
  fn::M_Function,
  scope::InstNode,
  fnDers::List{<:FunctionDerivative},
)::List{FunctionDerivative}

  @assign fnDers = begin
    local attrs::List{SCode.SubMod}
    local acref::Absyn.P_ComponentRef.ComponentRef
    local der_node::InstNode
    local order::Expression
    local conds::List{Tuple{Integer, Condition}}
    @match mod begin
      SCode.Mod.MOD(subModLst = attrs, binding = SOME(Absyn.CREF(acref))) => begin
        @assign (_, der_node) = P_Function.instFunction(acref, scope, mod.info)
        addLowerOrderDerivative(der_node, fnNode)
        @assign (order, conds) = getDerivativeAttributes(attrs, fn, fnNode, mod.info)
        _cons(FUNCTION_DER(der_node, fnNode, order, conds, nil), fnDers)
      end

      SCode.Mod.MOD(__) => begin
        #=  Give a warning if the derivative annotation doesn't specify a function name.
        =#
        Error.addStrictMessage(
          Error.MISSING_FUNCTION_DERIVATIVE_NAME,
          list(AbsynUtil.pathString(P_Function.name(fn))),
          mod.info,
        )
        fnDers
      end

      _ => begin
        #=  We shouldn't get any NOMODs here since they're filtered out when
        =#
        #=  translating Absyn to SCode, and redeclare isn't allowed by the syntax.
        =#
        Error.assertion(false, getInstanceName() + " got invalid modifier", sourceInfo())
        fail()
      end
    end
  end
  return fnDers
end

function getDerivativeAnnotations(definition::SCode.Element)::List{SCode.Mod}
  local derMods::List{SCode.Mod}

  @assign derMods = begin
    local ann::SCode.Annotation
    @match definition begin
      SCode.CLASS(
        classDef = SCode.PARTS(
          externalDecl = SOME(SCode.EXTERNALDECL(annotation_ = SOME(ann))),
        ),
      ) => begin
        SCodeUtil.lookupNamedAnnotations(ann, "derivative")
      end

      SCode.CLASS(cmt = SCode.COMMENT(annotation_ = SOME(ann))) => begin
        SCodeUtil.lookupNamedAnnotations(ann, "derivative")
      end

      _ => begin
        nil
      end
    end
  end
  return derMods
end
