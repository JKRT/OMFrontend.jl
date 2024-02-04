@UniontypeDecl NFFunctionDerivative
FunctionDerivative = NFFunctionDerivative

Condition = (() -> begin #= Enumeration =#
  ZERO_DERIVATIVE = 1
  NO_DERIVATIVE = 2
  () -> (ZERO_DERIVATIVE; NO_DERIVATIVE)
             end)()
const ConditionType = Int


@Uniontype NFFunctionDerivative begin
  @Record FUNCTION_DER begin
    derivativeFn::InstNode
    derivedFn::InstNode
    order::Expression
    conditions::List{Tuple{Int, ConditionType}}
    lowerOrderDerivatives::List{InstNode}
  end
end

#import ..SCodeUtil
#import ..SCodeDump

function conditionToDAE(
  cond::Tuple{<:Int, ConditionType},
)::Tuple{Int, DAE.derivativeCond}
  local daeCond::Tuple{Int, DAE.derivativeCond}

  local idx::Int
  local c::ConditionType

   (idx, c) = cond
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

  local order::Int

  @match INTEGER_EXPRESSION(order) = fnDer.order
  @assign derDef = DAE.FunctionDefinition.FUNCTION_DER_MAPPER(
    P_Function.name(listHead(getCachedFuncs(fnDer.derivedFn))),
    P_Function.name(listHead(getCachedFuncs(fnDer.derivativeFn))),
    order,
    list(conditionToDAE(c) for c in fnDer.conditions),
    NONE(),
    list(
      P_Function.name(listHead(getCachedFuncs(fn)))
      for fn in fnDer.lowerOrderDerivatives
    ),
  )
  #=  TODO: Figure out if the two fields below are needed. =#
  return derDef
end

function typeDerivative(fnDer::FunctionDerivative)
  local mk::MatchKindType
  local order::Expression
  local order_ty::M_Type
  local var::VariabilityType
  local infoVar::SourceInfo

  typeNodeCache(fnDer.derivativeFn)
  infoVar = sourceInfo() #info(fnDer.derivedFn)
  (order, order_ty, var) = typeExp(fnDer.order, ORIGIN_FUNCTION, infoVar)
  (order, _, mk) = matchTypes(order_ty, TYPE_INTEGER(), order)
  if isIncompatibleMatch(mk)
    Error.addSourceMessage(
      Error.VARIABLE_BINDING_TYPE_MISMATCH,
      list(
        "order",
        toString(order),
        "Int",
        Type.toString(order_ty),
      ),
      infoVar,
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
      infoVar,
    )
    fail()
  end
  order = evalExp(order, EVALTARGET_GENERIC(infoVar))
  return order
end

function instDerivatives(fnNode::InstNode, fn::M_Function)::List{FunctionDerivative}
  local ders::List{FunctionDerivative} = nil
  local der_mods::List{SCode.Mod}
  local scope::InstNode
  der_mods = getDerivativeAnnotations(definition(fnNode))
  scope = parent(fnNode)
  for m in der_mods
    ders = instDerivativeMod(m, fnNode, fn, scope, ders)
  end
  return ders
end

function addLowerOrderDerivative2(fn::M_Function, lowerDerNode::InstNode)
  fnDerivatives = list(
    begin
      if fn_der isa FUNCTION_DER
        fn_derLowerOrderDerivatives = _cons(lowerDerNode, fn_der.lowerOrderDerivatives)
        FUNCTION_DER(fn_der.derivativeFn,
                     fn_der.derivedFn,
                     fn_der.order,
                     fn_der.conditions,
                     fn_derLowerOrderDerivatives)
      end
    end
    for fn_der in fn.derivatives)
  local f = M_FUNCTION(fn.path,
                       fn.node,
                       fn.inputs,
                       fn.outputs,
                       fn.locals,
                       fn.slots,
                       fn.returnType,
                       fn.attributes,
                       fnDerivatives,
                       fn.status,
                       fn.callCounter)
  return f
end

function addLowerOrderDerivative(fnNode::InstNode, lowerDerNode::InstNode)
  return mapCachedFuncs(
    fnNode,
    (fn) -> addLowerOrderDerivative2(fn, lowerDerNode),
  )
end

function getInputIndex(nameArg::String, fn::M_Function, info::SourceInfo)::Int
  local index::Int = 1

  for i in fn.inputs
    if name(i) == nameArg
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
)::Tuple{Expression, List{Tuple{Int, ConditionType}}}
  local conditions::List{Tuple{Int, ConditionType}} = nil
  local order::Expression = EMPTY_EXPRESSION(TYPE_UNKNOWN())

  local id::String
  local mod::SCode.Mod
  local aexp::Absyn.Exp
  local acref::Absyn.P_ComponentRef.ComponentRef
  local index::Int

  for attr in attrs
    @match SCode.NAMEMOD(id, mod) = attr
     () = begin
      @match (id, mod) begin
        ("order", SCode.MOD(binding = SOME(aexp))) => begin
          if !isEmpty(order)
            Error.addSourceMessage(
              Error.DUPLICATE_MODIFICATIONS,
              list(id, "derivative"),
              info,
            )
          end
          order = instExp(aexp, scope, info)
          ()
        end

        (
          "noDerivative",
          SCode.MOD(
            binding = SOME(Absyn.CREF(componentRef = Absyn.CREF_IDENT(name = id))),
          ),
        ) => begin
          @assign index = getInputIndex(id, fn, info)
          @assign conditions = _cons((index, Condition.NO_DERIVATIVE), conditions)
          ()
        end

        (
          "zeroDerivative",
          SCode.MOD(
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
  if isEmpty(order)
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
    local acref::Absyn.ComponentRef
    local der_node::InstNode
    local order::Expression
    local conds::List{Tuple{Int, ConditionType}}
    @match mod begin
      SCode.MOD(subModLst = attrs, binding = SOME(Absyn.CREF(acref))) => begin
         (_, der_node) = instFunction(acref, scope, mod.info)
        addLowerOrderDerivative(der_node, fnNode)
        (order, conds) = getDerivativeAttributes(attrs, fn, fnNode, mod.info)
        _cons(FUNCTION_DER(der_node, fnNode, order, conds, nil), fnDers)
      end

      SCode.MOD(__) => begin
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
