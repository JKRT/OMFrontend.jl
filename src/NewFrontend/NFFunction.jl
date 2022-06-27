NamedArg = Tuple
TypedArg = Tuple
TypedNamedArg = Tuple

SlotType = (() -> begin #= Enumeration =#
  POSITIONAL = 1  #= Only accepts positional arguments. =#
  NAMED = 2  #= Only accepts named argument. =#
  GENERIC = 3  #= Accepts both positional and named arguments. =#
  #= Determines which type of argument a slot accepts. =#
  () -> (POSITIONAL; NAMED; GENERIC)  #= Accepts both positional and named arguments. =#
end)()
const SlotTypeType = Int

SlotEvalStatus = (() -> begin #= Enumeration =#
  NOT_EVALUATED = 1
  EVALUATING = 2
  EVALUATED = 3
  () -> (NOT_EVALUATED; EVALUATING; EVALUATED)
end)()
const SlotEvalStatusType = Int

@UniontypeDecl Slot

function hasName(name::String, slot::Slot)::Bool
  local hasName::Bool = name == slot.name
  return hasName
end

function named(slot::Slot)::Bool
  local pos::Bool
  @assign pos = begin
    @match slot.ty begin
      SlotType.NAMED => begin
        true
      end
      SlotType.GENERIC => begin
        true
      end
      _ => begin
        false
      end
    end
  end
  return pos
end

function positional(slot::Slot)::Bool
  local pos::Bool
  @assign pos = begin
    @match slot.ty begin
      SlotType.POSITIONAL => begin
        true
      end
      SlotType.GENERIC => begin
        true
      end
      _ => begin
        false
      end
    end
  end
  return pos
end

@Uniontype Slot begin
  @Record SLOT begin
    name::String
    ty::SlotTypeType
    default::Option{Expression}
    arg::Option{TypedArg}
    index::Int
    evalStatus::SlotEvalStatusType
  end
end

@UniontypeDecl FunctionMatchKind

function isExactVectorized(mk::FunctionMatchKind)::Bool
  local b::Bool

  @assign b = begin
    @match mk begin
      VECTORIZED(baseMatch = EXACT(__)) => begin
        true
      end
      _ => begin
        false
      end
    end
  end
  return b
end

function isVectorized(mk::FunctionMatchKind)::Bool
  local b::Bool
  @assign b = begin
    @match mk begin
      VECTORIZED_MATCH_KIND(__) => begin
        true
      end
      _ => begin
        false
      end
    end
  end
  return b
end

function isExact(mk::FunctionMatchKind)::Bool
  local b::Bool
  @assign b = begin
    @match mk begin
      EXACT_MATCH_KIND(__) => begin
        true
      end
      _ => begin
        false
      end
    end
  end
  return b
end

function isValid(mk::FunctionMatchKind)::Bool
  local b::Bool
  @assign b = begin
    @match mk begin
      NOT_COMPATIBLE_MATCH_KIND(__) => begin
        false
      end
      _ => begin
        true
      end
    end
  end
  return b
end

@Uniontype FunctionMatchKind begin
  @Record NOT_COMPATIBLE_MATCH_KIND begin
  end
  @Record VECTORIZED_MATCH_KIND begin
    vectDims::List{Dimension}
    #=  When vectorizing a call exact argument matches are allowed to not be vectorized =#
    #=  Instead they are added to each call as is. =#
    #=  This list represents which args should be vectorized. =#
    vectorizedArgs::List{Int}
    baseMatch::FunctionMatchKind
  end
  @Record GENERIC_MATCH_KIND begin
  end
  @Record CAST_MATCH_KIND begin
  end
  @Record EXACT_MATCH_KIND begin
  end
end

const EXACT_MATCH = EXACT_MATCH_KIND()::FunctionMatchKind
const CAST_MATCH = CAST_MATCH_KIND()::FunctionMatchKind
const GENERIC_MATCH = GENERIC_MATCH_KIND()::FunctionMatchKind
const NO_MATCH = NOT_COMPATIBLE_MATCH_KIND()::FunctionMatchKind

@UniontypeDecl MatchedFunction

function isVectorized(mf::MatchedFunction)::Bool
  local b::Bool = isVectorized(mf.mk)
  return b
end

function getExactVectorizedMatches(
  matchedFunctions::List{<:MatchedFunction},
)::List{MatchedFunction}
  local outFuncs::List{MatchedFunction} =
    list(mf for mf in matchedFunctions if isExactVectorized(mf.mk))
  return outFuncs
end

function getExactMatches(matchedFunctions::List{<:MatchedFunction})::List{MatchedFunction}
  local outFuncs::List{MatchedFunction} =
    list(mf for mf in matchedFunctions if isExact(mf.mk))
  return outFuncs
end

@Uniontype MatchedFunction begin
  @Record MATCHED_FUNC begin
    func::M_Function
    args::List{TypedArg}
    mk::FunctionMatchKind
  end
end

FunctionStatus = (() -> begin #= Enumeration =#
  BUILTIN = 1  #= A builtin function. =#
  INITIAL = 2  #= The initial status. =#
  EVALUATED = 3  #= Constants in the function has been evaluated by EvalConstants. =#
  SIMPLIFIED = 4  #= The function has been simplified by SimplifyModel. =#
  COLLECTED = 5  #= The function has been added to the function tree. =#
  () -> (BUILTIN; INITIAL; EVALUATED; SIMPLIFIED; COLLECTED)  #= The function has been added to the function tree. =#
end)()

const FunctionStatusType = Int


using MetaModelica
using ExportAll
#= Forward declarations for uniontypes until Julia adds support for mutual recursion =#
FoldFunc = Function
FoldFunc = Function
MapFunc = Function
MapFunc = Function
MapFn = Function
@UniontypeDecl M_Function
@Uniontype M_Function begin
  @Record M_FUNCTION begin
    path::Absyn.Path
    node::InstNode
    inputs::List{InstNode}
    outputs::List{InstNode}
    locals::List{InstNode}
    slots::List{Slot}
    returnType::NFType
    attributes::DAE.FunctionAttributes
    derivatives::List{FunctionDerivative}
    status::Pointer
    callCounter::Pointer
  end
end

function getLocalArguments(fn::M_Function)::List{Expression}
  local localArgs::List{Expression} = nil
  local binding::Binding
  for l in fn.locals
    if isComponent(l)
      @assign binding = getBinding(component(l))
      # Error.assertion(
      #   hasExp(binding),
      #   getInstanceName() + " got local component without binding",
      #   sourceInfo(),
      # )
      @assign localArgs = _cons(getExp(binding), localArgs)
    end
  end
  @assign localArgs = listReverseInPlace(localArgs)
  return localArgs
end

function isPartial(fn::M_Function)::Bool
  local isPartial::Bool = isPartial(fn.node)
  return isPartial
end

function foldExpParameter(node::InstNode, foldFn::FoldFunc, arg::ArgT) where {ArgT}

  local comp::Component
  local cls::Class

  @assign comp = component(node)
  @assign arg = foldExp(getBinding(comp), foldFn, arg)
  @assign () = begin
    @match comp begin
      TYPED_COMPONENT(__) => begin
        @assign arg = Type.foldDims(
          comp.ty,
          (foldFn) -> P_Dimension.Dimension.foldExp(func = foldFn),
          arg,
        )
        @assign cls = getClass(comp.classInst)
        @assign arg = foldComponents(
          classTree(cls),
          (foldFn) -> foldExpParameter(foldFn = foldFn),
          arg,
        )
        ()
      end

      _ => begin
        ()
      end
    end
  end
  return arg
end

function foldExp(
  fn::M_Function,
  foldFn::FoldFunc,
  arg::ArgT,
  mapParameters::Bool = true,
  mapBody::Bool = true,
) where {ArgT}

  local cls::Class

  @assign cls = getClass(fn.node)
  if mapParameters
    @assign arg = foldComponents(
      classTree(cls),
      (foldFn) -> foldExpParameter(foldFn = foldFn),
      arg,
    )
  end
  if mapBody
    @assign arg = P_Sections.Sections.foldExp(getSections(cls), foldFn, arg)
  end
  return arg
end

function mapExpParameter(node::InstNode, mapFn::MapFunc)
  local comp::Component
  local binding::Binding
  local binding2::Binding
  local cls::Class
  local ty::M_Type
  local dirty::Bool = false

  @assign comp = component(node)
  @assign binding = getBinding(comp)
  @assign binding2 = mapExp(binding, mapFn)
  if !referenceEq(binding, binding2)
    @assign comp = P_Component.setBinding(binding2, comp)
    @assign dirty = true
  end
  @assign () = begin
    @match comp begin
      TYPED_COMPONENT(__) => begin
        @assign ty =
          mapDims(comp.ty, (mapFn) -> mapExp(func = mapFn))
        if !referenceEq(ty, comp.ty)
          @assign comp.ty = ty
          @assign dirty = true
        end
        @assign cls = getClass(comp.classInst)
        applyComponents(
          classTree(cls),
          (nodeArg) -> mapExpParameter(nodeArg, mapFn),
        )
        ()
      end

      _ => begin
        ()
      end
    end
  end
  return if dirty
    updateComponent!(comp, node)
  end
end

function mapExp(
  fn::M_Function,
  mapFn::MapFunc,
  mapParameters::Bool = true,
  mapBody::Bool = true,
)::M_Function

  local cls::Class
  local ctree::ClassTree
  local comps::Vector{InstNode}
  local sections::Sections
  local comp::Component
  local binding::Binding
  local binding2::Binding

  @assign cls = getClass(fn.node)
  if mapParameters
    @assign ctree = classTree(cls)
    applyComponents(ctree, (nodeArg) -> mapExpParameter(nodeArg, mapFn))
    @assign fn.returnType = makeReturnType(fn)
  end
  if mapBody
    sections = mapExp(getSections(cls), mapFn)
    cls = setSections(sections, cls)
    updateClass(cls, fn.node)
  end
  return fn
end

function hasOptionalArgument(component::SCode.Element)::Bool
  local res::Bool = SCodeUtil.hasBooleanNamedAnnotationInComponent(
    component,
    "__OpenModelica_optionalArgument",
  )
  return res
end

function hasUnboxArgsAnnotation(cmt::SCode.Comment)::Bool
  local res::Bool =
    SCodeUtil.commentHasBooleanNamedAnnotation(cmt, "__OpenModelica_UnboxArguments")
  return res
end

""" #= Returns true if the function has the __OpenModelica_UnboxArguments annotation, otherwise false. =#"""
function hasUnboxArgs(fn::M_Function)::Bool
  local res::Bool

  @assign res = begin
    @match fn.attributes begin
      DAE.FUNCTION_ATTRIBUTES(
        isBuiltin = DAE.FUNCTION_BUILTIN(unboxArgs = res),
      ) => begin
        res
      end

      _ => begin
        false
      end
    end
  end
  return res
end

function getBody(fn::M_Function)::List{Statement}
  local body::List{Statement} = getBody2(fn.node)
  return body
end

function makeDAEType(fn::M_Function, boxTypes::Bool = false)::DAE.Type
  local outType::DAE.Type

  local params::List{DAE.FuncArg} = nil
  local pname::String
  local ty::M_Type
  local ptype::DAE.Type
  local pconst::DAE.Const
  local ppar::DAE
  local pdefault::Option{DAE.Exp}
  local comp::Component

  for param in fn.inputs
    @assign comp = component(param)
    @assign pname = name(param)
    @assign ty = getType(comp)
    @assign ptype = toDAE(if boxTypes
      Type.box(ty)
    else
      ty
    end)
    @assign pconst = P_Prefixes.variabilityToDAEConst(variability(comp))
    @assign ppar = P_Prefixes.parallelismToDAE(P_Component.parallelism(comp))
    @assign pdefault = Util.applyOption(
      typedExp(getBinding(comp)),
      toDAE,
    )
    @assign params =
      _cons(DAE.FuncArg.FUNCARG(pname, ptype, pconst, ppar, pdefault), params)
  end
  @assign params = listReverse(params)
  @assign ty = if boxTypes
    Type.box(fn.returnType)
  else
    fn.returnType
  end
  @assign outType = DAE.T_FUNCTION(params, toDAE(ty), fn.attributes, fn.path)
  return outType
end

function toDAE(fn::M_Function, def::DAE.FunctionDefinition)::DAE.P_Function
  local daeFn::DAE.P_Function

  local vis::SCode.Visibility
  local par::Bool
  local impr::Bool
  local ity::DAE.InlineType
  local ty::DAE.Type
  local defs::List{DAE.FunctionDefinition}
  local unused_inputs::List{Int}

  @assign vis = SCode.PUBLIC()
  #=  TODO: Use the actual visibility.
  =#
  @assign par = false
  #=  TODO: Use the actual partial prefix.
  =#
  @assign impr = fn.attributes.isImpure
  @assign ity = fn.attributes.inline
  @assign ty = makeDAEType(fn)
  @assign unused_inputs = analyseUnusedParameters(fn)
  @assign defs = _cons(
    def,
    list(
      P_FunctionDerivative.FunctionDerivative.toDAE(fn_der) for fn_der in fn.derivatives
    ),
  )
  @assign daeFn = DAE.FUNCTION(
    fn.path,
    defs,
    ty,
    vis,
    par,
    impr,
    ity,
    unused_inputs,
    ElementSource_createElementSource(info(fn.node)),
    SCodeUtil.getElementComment(definition(fn.node)),
  )
  return daeFn
end

function isDefaultRecordConstructor(fn::M_Function)::Bool
  local isConstructor::Bool
  @assign isConstructor = begin
    @match restriction(getClass(fn.node)) begin
      RESTRICTION_RECORD_CONSTRUCTOR(__) => begin
        true
      end
      _ => begin
        false
      end
    end
  end
  return isConstructor
end

function inlineBuiltin(fn::M_Function)::DAE.InlineType
  local inlineType::DAE.InlineType
  @assign inlineType = begin
    @match fn.attributes.isBuiltin begin
      DAE.FUNCTION_BUILTIN_PTR(__) => begin
        DAE.BUILTIN_EARLY_INLINE()
      end
      _ => begin
        fn.attributes.inline
      end
    end
  end
  return inlineType
end

function isExternal(fn::M_Function)::Bool
  local isExternal::Bool =
    !isEmpty(fn.node) && isExternalFunction(getClass(fn.node))
  return isExternal
end

function setFunctionPointer(isPointer::Bool, fn::M_Function)::M_Function
  local attr::DAE.FunctionAttributes = fn.attributes
  @assign attr.isFunctionPointer = isPointer
  @assign fn.attributes = attr
  return fn
end

function isFunctionPointer(fn::M_Function)::Bool
  local isPointer::Bool = fn.attributes.isFunctionPointer
  return isPointer
end

function isOMImpure(fn::M_Function)::Bool
  local isImpure::Bool = !fn.attributes.isOpenModelicaPure
  return isImpure
end

function isImpure(fn::M_Function)::Bool
  local isImpure::Bool = fn.attributes.isImpure
  return isImpure
end

function isSubscriptableBuiltin(fn::M_Function)::Bool
  local scalarBuiltin::Bool
  if !isBuiltin(fn)
    @assign scalarBuiltin = false
  else
    @assign scalarBuiltin = begin
      @match AbsynUtil.pathFirstIdent(nameConsiderBuiltin(fn)) begin
        "change" => begin
          true
        end

        "der" => begin
          true
        end

        "pre" => begin
          true
        end

        _ => begin
          false
        end
      end
    end
  end
  return scalarBuiltin
end

"""
  This function checks if a function is a special builtin function
  and needs to be handled in a different way.
  Examples of such functions is der
"""
function isSpecialBuiltin(fn::M_Function)::Bool
  local special::Bool
  local path::Absyn.Path
  if !isBuiltin(fn)
    @assign special = false
  else
    @assign path = nameConsiderBuiltin(fn)
    if !AbsynUtil.pathIsIdent(path)
      @assign special = false
    else
      @assign special = begin
        @match AbsynUtil.pathFirstIdent(path) begin
          "array" => begin
            true
          end

          "actualStream" => begin
            true
          end

          "branch" => begin
            true
          end

          "cardinality" => begin
            true
          end

          "cat" => begin
            true
          end

          "change" => begin
            true
          end

          "der" => begin
            true
          end

          "diagonal" => begin
            true
          end

          "edge" => begin
            true
          end

          "fill" => begin
            true
          end

          "getInstanceName" => begin
            true
          end

          "initial" => begin
            true
          end

          "inStream" => begin
            true
          end

          "isRoot" => begin
            true
          end

          "matrix" => begin
            true
          end

          "max" => begin
            true
          end

          "min" => begin
            true
          end

          "ndims" => begin
            true
          end

          "noEvent" => begin
            true
          end

          "ones" => begin
            true
          end

          "potentialRoot" => begin
            true
          end

          "pre" => begin
            true
          end

          "product" => begin
            true
          end

          "root" => begin
            true
          end

          "rooted" => begin
            true
          end

          "uniqueRoot" => begin
            true
          end

          "uniqueRootIndices" => begin
            true
          end

          "scalar" => begin
            true
          end

          "size" => begin
            true
          end

          "smooth" => begin
            true
          end

          "sum" => begin
            true
          end

          "symmetric" => begin
            true
          end

          "terminal" => begin
            true
          end

          "transpose" => begin
            true
          end

          "vector" => begin
            true
          end

          "zeros" => begin
            true
          end

          "sample" => begin
            true
          end
          #= Extensions  =#
          "initialStructuralState" => true
          "structuralTransition" => true
          "recompilation" => true
          _ => begin
            false
          end
        end
      end
    end
  end
  return special
end

function isBuiltinAttr(attrs::DAE.FunctionAttributes)::Bool
  local isBuiltin::Bool

  @assign isBuiltin = begin
    @match attrs.isBuiltin begin
      DAE.FUNCTION_NOT_BUILTIN(__) => begin
        false
      end
      _ => begin
        true
      end
    end
  end
  return isBuiltin
end

function isBuiltin(fn::M_Function)::Bool
  local isBuiltin::Bool = isBuiltinAttr(fn.attributes)
  return isBuiltin
end

function applyPartialApplicationArg(
  argName::String,
  argExp::Expression,
  argType::M_Type,
  inputs::List{<:InstNode},
  slots::List{<:Slot},
  fn::M_Function,
  info::SourceInfo,
)::Tuple{Expression, List{InstNode}, List{Slot}}
  local outSlots::List{Slot} = nil
  local outInputs::List{InstNode} = nil

  local i::InstNode
  local rest_inputs::List{InstNode} = inputs
  local s::Slot
  local rest_slots::List{Slot} = slots
  local mk::MatchKindType

  while !listEmpty(rest_inputs)
    @match _cons(i, rest_inputs) = rest_inputs
    @match _cons(s, rest_slots) = rest_slots
    if s.name == argName
      @assign (argExp, _, mk) =
        matchTypes(argType, getType(i), argExp, true)
      if TypeCheck.isIncompatibleMatch(mk)
        Error.addSourceMessage(
          Error.NAMED_ARG_TYPE_MISMATCH,
          list(
            AbsynUtil.pathString(name(fn)),
            argName,
            toString(argExp),
            Type.toString(argType),
            Type.toString(getType(i)),
          ),
          info,
        )
        fail()
      end
      @assign outInputs = listAppend(listReverseInPlace(outInputs), rest_inputs)
      @assign outSlots = listAppend(listReverseInPlace(outSlots), rest_slots)
      return (argExp, outInputs, outSlots)
    end
    @assign outInputs = _cons(i, outInputs)
    @assign outSlots = _cons(s, outSlots)
  end
  Error.addSourceMessage(
    Error.NO_SUCH_INPUT_PARAMETER,
    list(AbsynUtil.pathString(name(fn)), argName),
    info,
  )
  fail()
  return (argExp, outInputs, outSlots)
end

function typePartialApplication(
  exp::Expression,
  origin::ORIGIN_Type,
  info::SourceInfo,
)::Tuple{Expression, M_Type, Variability}
  local variability::VariabilityType
  local ty::M_Type

  local fn_ref::ComponentRef
  local args::List{Expression}
  local ty_args::List{Expression} = nil
  local arg_names::List{String}
  local rest_names::List{String}
  local arg_name::String
  local arg_exp::Expression
  local arg_ty::M_Type
  local arg_var::VariabilityType
  local fn::M_Function
  local next_origin::ORIGIN_Type = setFlag(origin, ORIGIN_SUBEXPRESSION)
  local inputs::List{InstNode}
  local slots::List{Slot}

  @match PARTIAL_FUNCTION_APPLICATION_EXPRESSION(
    fn = fn_ref,
    args = args,
    argNames = arg_names,
  ) = exp
  #=  TODO: Handle overloaded functions?
  =#
  @match _cons(fn, _) = typeRefCache(fn_ref)
  @assign inputs = fn.inputs
  @assign slots = fn.slots
  @assign rest_names = arg_names
  @assign variability = if isImpure(fn) || isOMImpure(fn)
    Variability.PARAMETER
  else
    Variability.CONSTANT
  end
  for arg in args
    @assign (arg, arg_ty, arg_var) = typeExp(arg, origin, info)
    @match _cons(arg_name, rest_names) = rest_names
    @assign (arg, inputs, slots) =
      applyPartialApplicationArg(arg_name, arg, arg_ty, inputs, slots, fn, info)
    @assign ty_args = _cons(box(arg), ty_args)
    @assign variability = variabilityMax(variability, arg_var)
  end
  @assign fn.inputs = inputs
  @assign fn.slots = slots
  @assign ty = TYPE_FUNCTION(fn, FunctionTYPE_FUNCTIONAL_VARIABLE)
  @assign exp = PARTIAL_FUNCTION_APPLICATION_EXPRESSION(
    fn_ref,
    listReverseInPlace(ty_args),
    arg_names,
    ty,
  )
  return (exp, ty, variability)
end

function boxFunctionParameter(component::InstNode)
  local comp::Component

  @assign comp = component(component)
  @assign comp = P_Component.setType(Type.box(getType(comp)), comp)
  return updateComponent!(comp, component)
end

""" #= Types the body of a function, along with any bindings of local variables
     and outputs. =#"""
function typeFunctionBody(fn::M_Function)::M_Function
  #=  Type the bindings of the outputs and local variables. =#
  for c in fn.outputs
    typeComponentBinding(c, ORIGIN_FUNCTION)
  end
  for c in fn.locals
    typeComponentBinding(c, ORIGIN_FUNCTION)
  end
  #=  Type the algorithm section of the function, if it has one. =#
  typeFunctionSections(fn.node, ORIGIN_FUNCTION)
  #=  Type any derivatives of the function.
  =#
  for fn_der in fn.derivatives
    P_FunctionDerivative.FunctionDerivative.typeDerivative(fn_der)
  end
  return fn
end

""" #= Types a function's parameters, local components and default arguments. =#"""
function typeFunctionSignature(fn::M_Function)::M_Function
  local attr::DAE.FunctionAttributes
  local node::InstNode = fn.node
  if !isTyped(fn)
    typeClassType(node, EMPTY_BINDING, ORIGIN_FUNCTION, node)
    typeComponents(node, ORIGIN_FUNCTION)
    if isPartial(node)
      applyComponents(
        classTree(getClass(node)),
        boxFunctionParameter,
      )
    end
    for c in fn.inputs
      typeComponentBinding(c, ORIGIN_FUNCTION)
    end
    @assign fn.slots = makeSlots(fn.inputs)
    checkParamTypes(fn)
    @assign fn.returnType = makeReturnType(fn)
  end
  return fn
end

function typeFunction(fn::M_Function)::M_Function
  fn = typeFunctionSignature(fn)
  fn = typeFunctionBody(fn)
  return fn
end

function getRefCache(fnRef::ComponentRef)::List{M_Function}
  local functions::List{M_Function}

  local fn_node::InstNode

  @assign fn_node = classScope(node(fnRef))
  @match C_FUNCTION(funcs = functions) = getFuncCache(fn_node)
  return functions
end

""" #= Returns the function(s) in the cache of the given node, and types them if
     they are not already typed. =#"""
function typeNodeCache(@nospecialize(functionNode::InstNode))::List{M_Function}
  local functions::List{M_Function}
  local fn_node::InstNode
  local typed::Bool
  local special::Bool
  local name::String
  @assign fn_node = classScope(functionNode)
  @match C_FUNCTION(functions, typed, special) = getFuncCache(fn_node)
  #=  Type the function(s) if not already done.
  =#
  if !typed
    functions = list(typeFunctionSignature(f) for f in functions)
    setFuncCache(fn_node, C_FUNCTION(functions, true, special))
    functions = list(typeFunctionBody(f) for f in functions)
    setFuncCache(fn_node, C_FUNCTION(functions, true, special))
  end
  return functions
end

""" #= Returns the function(s) referenced by the given cref, and types them if
     they are not already typed. =#"""
function typeRefCache(@nospecialize(functionRef::ComponentRef))::List{M_Function}
  local functions::List{M_Function}
  @assign functions = begin
    @match functionRef begin
      COMPONENT_REF_CREF(__) => begin
        typeNodeCache(functionRef.node)
      end
      _ => begin
        Error.assertion(
          false,
          getInstanceName() + " got invalid function call reference",
          sourceInfo(),
        )
        fail()
      end
    end
  end
  return functions
end

function isTyped(fn::M_Function)::Bool
  local isTyped::Bool
  @assign isTyped = begin
    @match fn.returnType begin
      TYPE_UNKNOWN(__) => begin
        false
      end
      _ => begin
        true
      end
    end
  end
  return isTyped
end

function matchFunctionsSilent(
  funcs::List{<:M_Function},
  args::List{<:TypedArg},
  named_args::List{<:TypedNamedArg},
  info::SourceInfo,
  vectorize::Bool = true,
)::List{MatchedFunction}
  local matchedFunctions::List{MatchedFunction}

  ErrorExt.setCheckpoint("NFFunction:matchFunctions")
  @assign matchedFunctions = matchFunctions(funcs, args, named_args, info, vectorize)
  ErrorExt.rollBack("NFFunction:matchFunctions")
  return matchedFunctions
end

function matchFunctions(
  funcs::List{<:M_Function},
  args::List{<:TypedArg},
  named_args::List{<:TypedNamedArg},
  info::SourceInfo,
  vectorize::Bool = true,
)::List{MatchedFunction}
  local matchedFunctions::List{MatchedFunction}

  local m_args::List{TypedArg}
  local matchKind::FunctionMatchKind
  local matched::Bool

  @assign matchedFunctions = nil
  for func in funcs
    @assign (m_args, matchKind) = matchFunction(func, args, named_args, info, vectorize)
    if isValid(matchKind)
      @assign matchedFunctions =
        _cons(MATCHED_FUNC(func, m_args, matchKind), matchedFunctions)
    end
  end
  return matchedFunctions
end

function matchFunction(
  func::M_Function,
  args::List{<:TypedArg},
  named_args::List{<:TypedNamedArg},
  info::SourceInfo,
  vectorize::Bool = true,
)::Tuple{List{TypedArg}, FunctionMatchKind}
  local matchKind::FunctionMatchKind = NO_MATCH
  local out_args::List{TypedArg}

  local slot_matched::Bool

  @assign (out_args, slot_matched) = fillArgs(args, named_args, func, info)
  if slot_matched
    @assign (out_args, matchKind) = matchArgs(func, out_args, info, vectorize)
  end
  return (out_args, matchKind)
end

""" #= Helper function to matchArgVectorized. Replaces unknown dimensions in the
     list with size(argExp, dimension index), so that vectorized calls involving
     unknown dimensions (e.g. in functions) can be handled correctly. =#"""
function fillUnknownVectorizedDims(
  dims::List{<:Dimension},
  argExp::Expression,
)::List{Dimension}
  local outDims::List{Dimension} = nil

  local i::Int = 1

  for dim in dims
    if P_Dimension.Dimension.isUnknown(dim)
      @assign dim = DIMENSION_EXP(
        SIZE_EXPRESSION(argExp, SOME(INTEGER_EXPRESSION(i))),
        Variability.CONTINUOUS,
      )
    end
    @assign outDims = _cons(dim, outDims)
    @assign i = i + 1
  end
  @assign outDims = listReverseInPlace(outDims)
  return outDims
end

function matchArgVectorized(
  argExp::Expression,
  argTy::M_Type,
  inputTy::M_Type,
  vectArg::Expression,
  vectDims::List{<:Dimension},
  info::SourceInfo,
)::Tuple{Expression, M_Type, Expression, List{Dimension}, MatchKindType}
  local matchKind::MatchKindType
  local arg_dims::List{Dimension}
  local input_dims::List{Dimension}
  local vect_dims::List{Dimension}
  local rest_dims::List{Dimension}
  local rest_ty::M_Type
  local mk::MatchKindType
  local vect_dims_count::Int
  arg_dims = arrayDims(argTy)
  input_dims = arrayDims(inputTy)
  vect_dims_count = listLength(arg_dims) - listLength(input_dims)
  #=  Only try to vectorize if the argument has more dimensions than the input parameter.
  =#
  if vect_dims_count < 1
    matchKind = MatchKind.NOT_COMPATIBLE
    return (argExp, argTy, vectArg, vectDims, matchKind)
  end
  (vect_dims, rest_dims) = ListUtil.split(arg_dims, vect_dims_count)
  #=  Make sure the vectorization dimensions are consistent.
  =#
  if listEmpty(vectDims)
    vectDims = fillUnknownVectorizedDims(vect_dims, argExp)
    vectArg = argExp
  elseif !ListUtil.isEqualOnTrue(vectDims, vect_dims, isEqual)
    Error.addSourceMessage(
      Error.VECTORIZE_CALL_DIM_MISMATCH,
      list(
        "",
        toString(vectArg),
        "",
        toString(argExp),
        P_Dimension.Dimension.toStringList(vectDims),
        P_Dimension.Dimension.toStringList(vect_dims),
      ),
      info,
    )
  end
  #=  Check that the argument and the input parameter are type compatible when
  =#
  #=  the dimensions to vectorize over has been removed from the argument's type.
  =#
  @assign rest_ty = liftArrayLeftList(arrayElementType(argTy), rest_dims)
  @assign (argExp, argTy, matchKind) =
    matchTypes(rest_ty, inputTy, argExp, allowUnknown = false)
  return (argExp, argTy, vectArg, vectDims, matchKind)
end

function matchArgs(
  func::M_Function,
  args::List{<:TypedArg},
  info::SourceInfo,
  vectorize::Bool = true,
)::Tuple{List{TypedArg}, FunctionMatchKind}
  local funcMatchKind::FunctionMatchKind = EXACT_MATCH

  local comp::Component
  local inputs::List{InstNode} = func.inputs
  local input_node::InstNode
  local arg_idx::Int = 1
  local checked_args::List{TypedArg} = nil
  local arg_exp::Expression
  local arg_ty::M_Type
  local input_ty::M_Type
  local ty::M_Type
  local arg_var::VariabilityType
  local mk::MatchKindType
  local vect_arg::Expression = INTEGER_EXPRESSION(0)
  local vect_dims::List{Dimension} = nil
  local matched::Bool
  local vectorized_args::List{Int} = nil

  for arg in args
    @assign (arg_exp, arg_ty, arg_var) = arg
    @match _cons(input_node, inputs) = inputs
    @assign comp = component(input_node)
    if arg_var > variability(comp)
      # Error.addSourceMessage(
      #   Error.FUNCTION_SLOT_VARIABILITY,
      #   list(
      #     name(input_node),
      #     toString(arg_exp),
      #     AbsynUtil.pathString(P_Function.name(func)),
      #     P_Prefixes.variabilityString(arg_var),
      #     P_Prefixes.variabilityString(variability(comp)),
      #   ),
      #   info,
      # )
      @error "Function argument \"$(toString(arg_exp))\" in call to $(AbsynUtil.pathString(name(func))) has variability 
              $(variabilityString(arg_var)) which is not a $(variabilityString(variability(comp)))"
      @assign funcMatchKind = NO_MATCH
      return (args, funcMatchKind)
    end
    input_ty = getType(comp)
    @assign (arg_exp, ty, mk) =
      matchTypes(arg_ty, input_ty, arg_exp, allowUnknown = true)
    @assign matched = isValidArgumentMatch(mk)
    if !matched && vectorize
      @assign (arg_exp, ty, vect_arg, vect_dims, mk) =
        matchArgVectorized(arg_exp, arg_ty, input_ty, vect_arg, vect_dims, info)
      @assign vectorized_args = _cons(arg_idx, vectorized_args)
      @assign matched = isValidArgumentMatch(mk)
    end
    if !matched
      # Error.addSourceMessage(
      #   Error.ARG_TYPE_MISMATCH,
      #   list(
      #     intString(arg_idx),
      #     AbsynUtil.pathString(func.path),
      #     name(input_node),
      #     toString(arg_exp),
      #     toString(arg_ty),
      #     toString(input_ty),
      #   ),
      #   info,
      # )
      local msg = AbsynUtil.pathString(func.path)
      local expectedType = toString(input_ty)
      local actualType = toString(arg_ty)
      local msgPart1 = "Arg type mismatch for $msg for $(name(input_node)) with exp: $(toString(arg_exp))"
      local msgPart2 = "\nExpected type: $expectedType, argument type $actualType"
      @error msgPart1 * msgPart2
      @assign funcMatchKind = NO_MATCH
      return (args, funcMatchKind)
    end
    if isCastMatch(mk)
      @assign funcMatchKind = CAST_MATCH
    elseif isGenericMatch(mk)
      @assign funcMatchKind = GENERIC_MATCH
    end
    @assign checked_args = _cons((arg_exp, ty, arg_var), checked_args)
    @assign arg_idx = arg_idx + 1
  end
  #=  Check that the variability of the argument and input parameter matches.
  =#
  #=  Check if the type of the argument and the input parameter matches exactly.
  =#
  #=  If the types don't match, try to vectorize the argument.
  =#
  #=  Print an error if the types match neither exactly nor vectorized.
  =#
  #=  TODO: This should be a running reduction of the matches. Not just based on the
  =#
  #=  last match.
  =#
  if !listEmpty(vectorized_args)
    @assign funcMatchKind =
      VECTORIZED(vect_dims, listReverse(vectorized_args), funcMatchKind)
  end
  @assign args = listReverse(checked_args)
  return (args, funcMatchKind)
end

function lookupSlotInArray(slotName::String, slots::Vector{<:Slot})::Option{Slot}
  local outSlot::Option{Slot}

  local slot::Slot

  try
    @assign slot = ArrayUtil.getMemberOnTrue(slotName, slots, P_Slot.hasName)
    @assign outSlot = SOME(slot)
  catch
    @assign outSlot = NONE()
  end
  return outSlot
end

function evaluateSlotExp_traverser(
  exp::Expression,
  slots::Vector{<:Slot},
  info::SourceInfo,
)::Expression
  local outExp::Expression
  outExp = begin
    local cref::ComponentRef
    local slot::Option{Slot}
    @match exp begin
      CREF_EXPRESSION(
        cref = cref && COMPONENT_REF_CREF(
          restCref = EMPTY(__),
        ),
      ) => begin
        slot = lookupSlotInArray(firstName(cref), slots)
        if isSome(slot)
          Util.tuple31(fillDefaultSlot(Util.getOption(slot), slots, info))
        else
          exp
        end
      end

      _ => begin
        exp
      end
    end
  end
  return outExp
end

function evaluateSlotExp(
  exp::Expression,
  slots::Vector{<:Slot},
  info::SourceInfo,
)::Expression
  local outExp::Expression
  outExp = map(
    exp,
    (x) -> evaluateSlotExp_traverser(x, slots, info)
  )
  return outExp
end

function fillDefaultSlot2(slot::Slot, slots::Vector{<:Slot}, info::SourceInfo)::TypedArg
  local outArg::TypedArg

  @assign outArg = begin
    local exp::Expression
    #=  An already evaluated slot, return its binding.
    =#
    @match slot.evalStatus begin
      SlotEvalStatus.EVALUATED => begin
        Util.getOption(slot.arg)
      end

      SlotEvalStatus.EVALUATING => begin
        #=  A slot in the process of being evaluated => cyclic bindings.
        =#
        Error.addSourceMessage(Error.CYCLIC_DEFAULT_VALUE, list(slot.name), info)
        fail()
      end

      SlotEvalStatus.NOT_EVALUATED => begin
        #=  A slot with a not evaluated binding, evaluate the binding and return it.
        =#
        @assign slot.evalStatus = SlotEvalStatus.EVALUATING
        arrayUpdate(slots, slot.index, slot)
        @assign exp = evaluateSlotExp(Util.getOption(slot.default), slots, info)
        @assign outArg = (
          exp,
          typeOf(exp),
          variability(exp),
        )
        @assign slot.arg = SOME(outArg)
        @assign slot.evalStatus = SlotEvalStatus.EVALUATED
        arrayUpdate(slots, slot.index, slot)
        outArg
      end
    end
  end
  return outArg
end

function fillDefaultSlot(slot::Slot, slots::Vector{<:Slot}, info::SourceInfo)::TypedArg
  local outArg::TypedArg

  @assign outArg = begin
    @match slot begin
      SLOT(arg = SOME(outArg)) => begin
        outArg
      end

      SLOT(default = SOME(_)) => begin
        fillDefaultSlot2(slot, slots, info)
      end

      _ => begin
        #=  Slot already filled by function argument.
        =#
        #=  Slot not filled by function argument, but has default value.
        =#
        #=  Give an error if no argument was given and there's no default argument.
        =#
        Error.addSourceMessage(Error.UNFILLED_SLOT, list(slot.name), info)
        fail()
      end
    end
  end
  return outArg
end

""" #= Collects the arguments from the given slots. =#"""
function collectArgs(slots::Vector{<:Slot}, info::SourceInfo)::Tuple{List{TypedArg}, Bool}
  local matching::Bool = true
  local args::List{TypedArg} = nil

  local default::Option{Expression}
  local e::Expression
  local arg::Option{TypedArg}
  local a::TypedArg
  local name::String

  for s in slots
    @match SLOT(name = name, default = default, arg = arg) = s
    @assign args = begin
      @matchcontinue arg begin
        SOME(a) => begin
          _cons(a, args)
        end

        _ => begin
          _cons(fillDefaultSlot(s, slots, info), args)
        end

        _ => begin
          #=  Use the argument from the call if one was given.
          =#
          #=  Otherwise, try to fill the slot with its default argument.
          =#
          @assign matching = false
          args
        end
      end
    end
  end
  @assign args = listReverse(args)
  return (args, matching)
end

""" #= Looks up a slot with the given name and tries to fill it with the given
     argument expression. =#"""
function fillNamedArg(
  inArg::TypedNamedArg,
  slots::Vector{<:Slot},
  fn::M_Function,
  info::SourceInfo,
)::Tuple{Array{Slot}, Bool} #= For error reporting =#
  local matching::Bool = true

  local s::Slot
  local argName::String
  local ty::M_Type
  local argExp::Expression
  local var::VariabilityType

  #=  Try to find a slot and fill it with the argument expression.
  =#
  #=  Positional arguments fill the slots from the start of the array, so
  =#
  #=  searching backwards will generally be a bit more efficient.
  =#
  for i = arrayLength(slots):(-1):1
    @assign s = slots[i]
    @assign (argName, argExp, ty, var) = inArg
    if s.name == argName
      if !P_Slot.named(s)
        @assign matching = false
      elseif isNone(s.arg)
        @assign s.arg = SOME((argExp, ty, var))
        @assign slots[i] = s
      else
        Error.addSourceMessage(Error.FUNCTION_SLOT_ALREADY_FILLED, list(argName, ""), info)
        @assign matching = false
      end
      return (slots, matching)
    end
  end
  #=  Slot doesn't allow named argument (used for some builtin functions).
  =#
  #=  TODO: Improve the error message, should mention function name.
  =#
  #=  No slot could be found.
  =#
  @assign matching = false
  #=  A slot with the given name couldn't be found. This means it doesn't
  =#
  #=  exist, or we removed it when handling positional argument. We need to
  =#
  #=  search through all slots to be sure.
  =#
  for s in fn.slots
    if argName == s.name
      Error.addSourceMessage(Error.FUNCTION_SLOT_ALREADY_FILLED, list(argName, ""), info)
      return (slots, matching)
    end
    Error.addSourceMessage(
      Error.NO_SUCH_PARAMETER,
      list(name(instance(fn)), argName),
      info,
    )
  end
  #=  We found a slot, so it must have already been filled.
  =#
  #=  No slot could be found, so it doesn't exist.
  =#
  return (slots, matching)
end

""" #= Matches the given arguments to the slots in a function, and returns the
     arguments sorted in the order of the function parameters. =#"""
function fillArgs(
  posArgs::List{<:TypedArg},
  namedArgs::List{<:TypedNamedArg},
  fn::M_Function,
  info::SourceInfo,
)::Tuple{List{TypedArg}, Bool}
  local matching::Bool
  local args::List{TypedArg} = posArgs

  local slot::Slot
  local slots::List{Slot}
  local remaining_slots::List{Slot}
  local filled_named_args::List{TypedArg}
  local slots_arr::Vector{Slot}
  local pos_arg_count::Int
  local slot_count::Int
  local index::Int = 1

  @assign slots = fn.slots
  @assign pos_arg_count = listLength(posArgs)
  @assign slot_count = listLength(slots)
  if pos_arg_count > slot_count
    @assign matching = false
    return (args, matching)
  elseif pos_arg_count == slot_count && listEmpty(namedArgs)
    @assign matching = true
    return (args, matching)
  end
  #=  If we have too many positional arguments it can't possibly match.
  =#
  #=  If we have exactly as many positional arguments as slots and no named
  =#
  #=  arguments we can just return the list of arguments as it is.
  =#
  @assign slots_arr = listArray(slots)
  for arg in args
    @assign slot = slots_arr[index]
    if !positional(slot)
      @assign matching = false
      return (args, matching)
    end
    @assign slot.arg = SOME(arg)
    arrayUpdate(slots_arr, index, slot)
    @assign index = index + 1
  end
  #=  Slot doesn't allow positional arguments (used for some builtin functions).
  =#
  for narg in namedArgs
    @assign (slots_arr, matching) = fillNamedArg(narg, slots_arr, fn, info)
    if !matching
      return (args, matching)
    end
  end
  @assign (args, matching) = collectArgs(slots_arr, info)
  return (args, matching)
end

function getSlots(fn::M_Function)::List{Slot}
  local slots::List{Slot} = fn.slots
  return slots
end

function setReturnType(ty::M_Type, fn::M_Function)::M_Function

  @assign fn.returnType = ty
  return fn
end

function returnType(fn::M_Function)::M_Type
  local ty::M_Type = fn.returnType
  return ty
end

function instance(fn::M_Function)::InstNode
  local node::InstNode = fn.node
  return node
end

function toFlatString(fn::M_Function)::String
  local str::String

  local s

  @assign s = IOStream.create(getInstanceName(), IOStream.IOStreamType.LIST())
  @assign s = toFlatStream(fn, s)
  @assign str = IOStream.string(s)
  IOStream.delete(s)
  return str
end

function toFlatStream(fn::M_Function, s)

  local fn_name::String
  local fn_body::List{Statement}

  if isDefaultRecordConstructor(fn)
    @assign s = IOStream.append(s, toFlatString(fn.node))
  else
    @assign fn_name = AbsynUtil.pathString(fn.path)
    @assign s = IOStream.append(s, "function '")
    @assign s = IOStream.append(s, fn_name)
    @assign s = IOStream.append(s, "'\\n")
    for i in fn.inputs
      @assign s = IOStream.append(s, "  ")
      @assign s = IOStream.append(s, toFlatString(i))
      @assign s = IOStream.append(s, ";\\n")
    end
    for o in fn.outputs
      @assign s = IOStream.append(s, "  ")
      @assign s = IOStream.append(s, toFlatString(o))
      @assign s = IOStream.append(s, ";\\n")
    end
    if !listEmpty(fn.locals)
      @assign s = IOStream.append(s, "protected\\n")
      for l in fn.locals
        @assign s = IOStream.append(s, "  ")
        @assign s = IOStream.append(s, toFlatString(l))
        @assign s = IOStream.append(s, ";\\n")
      end
    end
    @assign fn_body = getBody(fn)
    if !listEmpty(fn_body)
      @assign s = IOStream.append(s, "algorithm\\n")
      @assign s = P_Statement.Statement.toFlatStreamList(fn_body, "  ", s)
    end
    @assign s = IOStream.append(s, "end '")
    @assign s = IOStream.append(s, fn_name)
    @assign s = IOStream.append(s, "'")
  end
  return s
end

function paramTypeString(param::InstNode)::String
  local str::String = Type.toString(getType(param))
  return str
end

""" #= Constructs a string representing the type of the function, on the form
     function_name<function>(input types) => output type =#"""
function typeString(fn::M_Function)::String
  local str::String

  @assign str = ListUtil.toString(
    fn.inputs,
    paramTypeString,
    AbsynUtil.pathString(name(fn)) + "<function>",
    "(",
    ", ",
    ") => " + Type.toString(fn.returnType),
    true,
  )
  return str
end

""" #= Constructs a string representing a call, for use in error messages. =#"""
function callString(
  fn::M_Function,
  posArgs::List{<:Expression},
  namedArgs::List{<:NamedArg},
)::String
  local str::String

  @assign str =
    stringDelimitList(list(toString(arg) for arg in posArgs), ", ")
  if !listEmpty(namedArgs)
    @assign str =
      str +
      ", " +
      stringDelimitList(
        list(
          Util.tuple21(arg) + " = " + toString(Util.tuple22(arg))
          for arg in namedArgs
        ),
        ", ",
      )
  end
  @assign str = AbsynUtil.pathString(fn.path) + "(" + str + ")"
  return str
end

function candidateFuncListString(fns::List{<:M_Function})::String
  local s::String =
    stringDelimitList(list(signatureString(fn, true) for fn in fns), "\\n  ")
  return s
end


""" #= Constructs a signature string for a function, e.g. Real func(Real x, Real y) =#"""
function signatureString(fn::M_Function, printTypes::Bool = true)::String
  local str::String

  local fn_name::Absyn.Path
  local input_str::String
  local output_str::String
  local var_s::String
  local inputs_strl::List{String} = nil
  local inputs::List{InstNode} = fn.inputs
  local c::Component
  local def_exp::Expression
  local ty::M_Type

  for s in fn.slots
    @assign input_str = ""
    @assign c = component(listHead(inputs))
    @assign inputs = listRest(inputs)
    if isSome(s.default)
      @match SOME(def_exp) = s.default
      @assign input_str = " = " + toString(def_exp)
    end
    @assign input_str = s.name + input_str
    @assign input_str = begin
      @match s.ty begin
        SlotType.POSITIONAL => begin
          "" + input_str
        end

        _ => begin
          input_str
        end
      end
    end
    if printTypes && P_Component.isTyped(c)
      @assign ty = getType(c)
      @assign var_s = P_Prefixes.unparseVariability(variability(c), ty)
      @assign input_str = var_s + Type.toString(ty) + " " + input_str
    end
    @assign inputs_strl = _cons(input_str, inputs_strl)
  end
  #=  Add the default expression if it has any.
  =#
  #=  Add the name from the slot and not the node, since some builtin
  =#
  #=  functions don't bother using proper names for the nodes.
  =#
  #=  Add a $ in front of the name if the parameter only takes positional
  =#
  #=  arguments.
  =#
  #=  Add the type if the parameter has been typed.
  =#
  input_str = stringDelimitList(listReverse(inputs_strl), ", ")
  output_str = if printTypes && isTyped(fn)
    " => " + toString(fn.returnType)
  else
    ""
  end
  fn_name = nameConsiderBuiltin(fn)
  #=  if isSome(display_name) then Util.getOption(display_name) else fn.path;
  =#
  str = AbsynUtil.pathString(fn_name) + "(" + input_str + ")" + output_str
  return str
end

""" #= Handles the DAE.mo structure where builtin calls are replaced by their simpler name =#"""
function nameConsiderBuiltin(fn::M_Function)::Absyn.Path
  local path::Absyn.Path

  @assign path = begin
    local name::String
    @match fn.attributes.isBuiltin begin
      DAE.FUNCTION_BUILTIN(name = SOME(name)) => begin
        Absyn.IDENT(name)
      end

      DAE.FUNCTION_BUILTIN(__) => begin
        AbsynUtil.pathLast(fn.path)
      end

      _ => begin
        fn.path
      end
    end
  end
  return path
end

function setName(name::Absyn.Path, fn::M_Function)::M_Function

  @assign fn.path = name
  return fn
end

function name(fn::M_Function)::Absyn.Path
  local path::Absyn.Path = fn.path
  return path
end

""" #= Marks this function as collected for addition to the function tree. =#"""
function collect(fn::M_Function)
  #=  The pointer might be immutable, check before assigning to it.
  =#
  return if P_Pointer.access(fn.status) != FunctionStatus.BUILTIN
    P_Pointer.update(fn.status, FunctionStatus.COLLECTED)
  end
end

""" #= Returns true if this function has already been added to the function tree
     (or shouldn't be added, e.g. if it's builtin), otherwise false. =#"""
function isCollected(fn::M_Function)::Bool
  local collected::Bool
  @assign collected = begin
    @match P_Pointer.access(fn.status) begin
      FunctionStatus.BUILTIN => begin
        true
      end
      FunctionStatus.COLLECTED => begin
        true
      end
      _ => begin
        false
      end
    end
  end
  return collected
end

function markSimplified(fn::M_Function)
  return if P_Pointer.access(fn.status) != FunctionStatus.BUILTIN
    P_Pointer.update(fn.status, FunctionStatus.SIMPLIFIED)
  end
end

function isSimplified(fn::M_Function)::Bool
  local simplified::Bool

  @assign simplified = begin
    @match P_Pointer.access(fn.status) begin
      FunctionStatus.BUILTIN => begin
        true
      end

      FunctionStatus.SIMPLIFIED => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return simplified
end

function markEvaluated(fn::M_Function)
  return if P_Pointer.access(fn.status) != FunctionStatus.BUILTIN
    P_Pointer.update(fn.status, FunctionStatus.EVALUATED)
  end
end

function isEvaluated(fn::M_Function)::Bool
  local evaluated::Bool
  local status = P_Pointer.access(fn.status)
  evaluated = begin
    @match status begin
      FunctionStatus.BUILTIN => begin
        true
      end
      FunctionStatus.EVALUATED => begin
        true
      end
      _ => begin
        false
      end
    end
  end
  return evaluated
end

function mapCachedFuncs(inNode::InstNode, mapFn::MapFn)
  local cls_node::InstNode
  local cache::CachedData

  @assign cls_node = classScope(inNode)
  @assign cache = getFuncCache(cls_node)
  @assign cache = begin
    @match cache begin
      C_FUNCTION(__) => begin
        @assign cache.funcs = list(mapFn(fn) for fn in cache.funcs)
        cache
      end

      _ => begin
        fail()
      end
    end
  end
  return setFuncCache(cls_node, cache)
end

function getCachedFuncs(inNode::InstNode)::List{M_Function}
  local outFuncs::List{M_Function}

  local cache::CachedData

  @assign cache = getFuncCache(classScope(inNode))
  @assign outFuncs = begin
    @match cache begin
      C_FUNCTION(__) => begin
        cache.funcs
      end

      _ => begin
        fail()
      end
    end
  end
  return outFuncs
end

function instFunction3(fnNode::InstNode)::InstNode
  @assign fnNode = instantiateN1(fnNode, EMPTY_NODE())
  #=  Set up an empty function cache to signal that this function is
  =#
  #=  currently being instantiatdded, so recursive functions can be handled.
  =#
  cacheInitFunc(fnNode)
  instExpressions(fnNode)
  #@debug "Returning in instfunction3"
  return fnNode
end

function instFunction2(
  fnPath::Absyn.Path,
  fnNode::InstNode,
  info::SourceInfo,
  parent::InstNode = EMPTY_NODE(),
)::Tuple{InstNode, Bool}
  local specialBuiltin::Bool

  local def::SCode.Element = definition(fnNode)

  @assign (fnNode, specialBuiltin) = begin
    local cdef::SCode.ClassDef
    local fn::M_Function
    local cr::Absyn.ComponentRef
    local sub_fnNode::InstNode
    local funcs::List{M_Function}
    local fn_ders::List{FunctionDerivative}
    @match def begin
      SCode.CLASS(__) where {(SCodeUtil.isOperatorRecord(def))} => begin
        @assign fnNode = instFunction3(fnNode)
        @assign fnNode = instConstructor(fnPath, fnNode, info)
        (fnNode, false)
      end

      SCode.CLASS(__) where {(SCodeUtil.isRecord(def))} => begin
        @assign fnNode = instFunction3(fnNode)
        @assign fnNode = Record.instDefaultConstructor(fnPath, fnNode, info)
        (fnNode, false)
      end

      SCode.CLASS(restriction = SCode.R_OPERATOR(__), classDef = cdef && SCode.PARTS(__)) => begin
        fnNode = instFunction3(fnNode)
        fnNode = instOperatorFunctions(fnNode, info)
        (fnNode, false)
      end

      SCode.CLASS(classDef = cdef && SCode.OVERLOAD(__)) => begin
        for p in cdef.pathLst
          @assign cr = AbsynUtil.pathToCref(p)
          @assign (_, sub_fnNode, specialBuiltin) = instFunction(cr, fnNode, info)
          for f in getCachedFuncs(sub_fnNode)
            @assign fnNode = cacheAddFunc(fnNode, f, specialBuiltin)
          end
        end
        (fnNode, false)
      end

      SCode.CLASS(__) => begin
        if SCodeUtil.isOperator(def)
          checkOperatorRestrictions(fnNode)
        end
        @assign fnNode =
          setNodeType(ROOT_CLASS(parent), fnNode)
        @assign fnNode = instFunction3(fnNode)
        @assign fn = new(fnPath, fnNode)
        @assign specialBuiltin = isSpecialBuiltin(fn)
        @assign fn.derivatives =
          instDerivatives(fnNode, fn)
        @assign fnNode = cacheAddFunc(fnNode, fn, specialBuiltin)
        (fnNode, specialBuiltin)
      end
    end
  end
  return (fnNode, specialBuiltin)
end

""" #= Instantiates the given InstNode as a function. =#"""
function instFunctionNode(node::InstNode)::InstNode
  local cache::CachedData
  @assign cache = getFuncCache(node)
  @assign () = begin
    @match cache begin
      C_FUNCTION(__) => begin
        ()
      end
      _ => begin
        (node, _) =
          instFunction2(scopePath(node), node, InstNode_info(node))#info(node))
        ()
      end
    end
  end
  return node
end

function instFunctionRef(
  fn_ref::ComponentRef,
  info::SourceInfo,
)::Tuple{ComponentRef, InstNode, Bool}
  local specialBuiltin::Bool
  local fn_node::InstNode

  local cache::CachedData
  local parent::InstNode

  @assign fn_node = classScope(node(fn_ref))
  @assign cache = getFuncCache(fn_node)
  #=  Check if a cached instantiation of this function already exists. =#
  @assign (fn_node, specialBuiltin) = begin
    @match cache begin
      C_FUNCTION(__) => begin
        (fn_node, cache.specialBuiltin)
      end
      _ => begin
        @assign parent =
          if isRedeclare(node(fn_ref)) ||
             isSimple(fn_ref)
            EMPTY_NODE()
          else
            node(rest(fn_ref))
          end
        if !isComponent(parent)
          @assign parent = EMPTY_NODE()
        end
        instFunction2(toPath(fn_ref), fn_node, info, parent)
      end
    end
  end
  return (fn_ref, fn_node, specialBuiltin)
end

function instFunction(
  functionName,
  scope::InstNode,
  info::SourceInfo,
)::Tuple{ComponentRef, InstNode, Bool}
  local specialBuiltin::Bool
  local fn_node::InstNode
  local fn_ref::ComponentRef
  local cache::CachedData
  fn_ref = lookupFunction(functionName, scope, info)
  (fn_ref, fn_node, specialBuiltin) = instFunctionRef(fn_ref, info)
  return (fn_ref, fn_node, specialBuiltin)
end

function lookupFunction(
  functionName,
  scope::InstNode,
  info::SourceInfo,
)::ComponentRef
  local functionRef::ComponentRef
  local found_scope::InstNode
  local state::LookupState
  local functionPath::Absyn.Path
  local prefix::ComponentRef
  local is_class::Bool
  functionPath = AbsynUtil.crefToPath(functionName)
  #=  Make sure the name is a path.
  =#
  (functionRef, found_scope) = lookupFunctionName(functionName, scope, info)
  #=  If we found a function class we include the root in the prefix, but if we
  =#
  #=  instead found a component (i.e. a functional parameter) we don't.
  =#
  is_class = isClass(node(functionRef))
  prefix = fromNodeList(scopeList(
    found_scope,
    includeRoot = is_class,
  ))
  @assign functionRef = append(functionRef, prefix)
  return functionRef
end

function lookupFunctionSimple(functionName::String, scope::InstNode)::ComponentRef
  local functionRef::ComponentRef
  local found_scope::InstNode
  local state::LookupState
  local functionPath::Absyn.Path
  local prefix::ComponentRef
  @assign (functionRef, found_scope) =
    lookupFunctionNameSilent(Absyn.CREF_IDENT(functionName, nil), scope)
  @assign prefix =
    fromNodeList(scopeList(found_scope))
  @assign functionRef = append(functionRef, prefix)
  return functionRef
end

function new(path::Absyn.Path, node::InstNode)::M_Function
  local fn::M_Function
  local cls::Class
  local inputs::List{InstNode}
  local outputs::List{InstNode}
  local locals::List{InstNode}
  local slots::List{Slot}
  local attr::DAE.FunctionAttributes
  local status::FunctionStatusType

  @assign (inputs, outputs, locals) = collectParams(node)
  @assign attr = makeAttributes(node, inputs, outputs)
  #=  Make sure builtin functions aren't added to the function tree.
  =#
  @assign status = if isBuiltinAttr(attr)
    FunctionStatus.COLLECTED
  else
    FunctionStatus.INITIAL
  end
  @assign fn = M_FUNCTION(
    path,
    node,
    inputs,
    outputs,
    locals,
    nil,
    TYPE_UNKNOWN(),
    attr,
    nil,
    P_Pointer.create(status),
    P_Pointer.create(0),
  )
  return fn
end

function isValidParamState(cls::InstNode)::Bool
  local isValid::Bool
  @assign isValid = begin
    @match restriction(getClass(cls)) begin
      RESTRICTION_RECORD(__) => begin
        true
      end
      RESTRICTION_TYPE(__) => begin
        true
      end
      RESTRICTION_OPERATOR(__) => begin
        true
      end
      RESTRICTION_FUNCTION(__) => begin
        true
      end

      RESTRICTION_EXTERNAL_OBJECT(__) => begin
        true
      end
      _ => begin
        false
      end
    end
  end
  return isValid
end

function isValidParamType(ty::NFType)::Bool
  local isValid::Bool
  @assign isValid = begin
    @match ty begin
      TYPE_INTEGER(__) => begin
        true
      end

      TYPE_REAL(__) => begin
        true
      end

      TYPE_STRING(__) => begin
        true
      end

      TYPE_BOOLEAN(__) => begin
        true
      end

      TYPE_CLOCK(__) => begin
        true
      end

      TYPE_ENUMERATION(__) => begin
        true
      end

      TYPE_ENUMERATION_ANY(__) => begin
        true
      end

      TYPE_POLYMORPHIC(__) => begin
        true
      end

      TYPE_ARRAY(__) => begin
        isValidParamType(ty.elementType)
      end

      TYPE_COMPLEX(__) => begin
        isValidParamState(ty.cls)
      end

      TYPE_FUNCTION(__) => begin
        true
      end

      TYPE_METABOXED(__) => begin
        isValidParamType(ty.ty)
      end

      _ => begin
        false
      end
    end
  end
  return isValid
end

function checkParamTypes2(params::List{<:InstNode})
  local ty::M_Type
  return for p in params
    @assign ty = getType(p)
    if !isValidParamType(ty)
      Error.addSourceMessage(
        Error.INVALID_FUNCTION_VAR_TYPE,
        list(Type.toString(ty), name(p)),
        info(p),
      )
      fail()
    end
  end
end

""" #= Checks that all the function parameters have types which are allowed in a
     function. =#"""
function checkParamTypes(fn::M_Function)
  checkParamTypes2(fn.inputs)
  checkParamTypes2(fn.outputs)
  return checkParamTypes2(fn.locals)
end

function makeAttributes(
  @nospecialize(node::InstNode),
  @nospecialize(inputs::List{<:InstNode}),
  @nospecialize (outputs::List{<:InstNode})
)::DAE.FunctionAttributes
  local attr::DAE.FunctionAttributes

  local def::SCode.Element
  local params::Vector{InstNode}
  local res::SCode.Restriction
  local fres::SCode.FunctionRestriction
  local is_partial::Bool
  local cmts::List{SCode.Comment}
  local cmt::SCode.Comment

  @assign def = definition(node)
  @assign res = SCodeUtil.getClassRestriction(def)
  # Error.assertion(
  #   SCodeUtil.isFunctionRestriction(res),
  #   getInstanceName() + " got non-function restriction",
  #   sourceInfo(),
  # ) TODO
  @match SCode.R_FUNCTION(functionRestriction = fres) = res
  @assign is_partial = SCodeUtil.isPartial(def)
  @assign cmts = getComments(node)
  @assign cmt = mergeFunctionAnnotations(cmts)
  @assign attr = begin
    local is_impure::Bool
    local is_om_pure::Bool
    local has_out_params::Bool
    local has_unbox_args::Bool
    local nameVar::String
    local in_params::List{String}
    local out_params::List{String}
    local inline_ty::DAE.InlineType
    local builtin::DAE.FunctionBuiltin
    #=  External function.
    =#
    @matchcontinue fres begin
      SCode.FR_EXTERNAL_FUNCTION(is_impure) => begin
        @assign in_params = list(name(i) for i in inputs)
        @assign out_params = list(name(o) for o in outputs)
        @assign nameVar = SCodeUtil.isBuiltinFunction(def, in_params, out_params)
        @assign inline_ty = commentIsInlineFunc(cmt)
        @assign is_impure = is_impure || hasImpure(cmt)
        @assign has_unbox_args = hasUnboxArgsAnnotation(cmt)
        DAE.FUNCTION_ATTRIBUTES(
          inline_ty,
          hasOMPure(cmt),
          is_impure,
          is_partial,
          DAE.FUNCTION_BUILTIN(SOME(nameVar), has_unbox_args),
          DAE.FP_NON_PARALLEL(),
        )
      end

      SCode.FR_PARALLEL_FUNCTION(__) => begin
        #=  Parallel function: there are some builtin functions.
        =#
        @assign in_params = list(name(i) for i in inputs)
        @assign out_params = list(name(o) for o in outputs)
        @assign nameVar = SCodeUtil.isBuiltinFunction(def, in_params, out_params)
        @assign inline_ty = commentIsInlineFunc(cmt)
        @assign has_unbox_args = hasUnboxArgsAnnotation(cmt)
        DAE.FUNCTION_ATTRIBUTES(
          inline_ty,
          hasOMPure(cmt),
          false,
          is_partial,
          DAE.FUNCTION_BUILTIN(SOME(nameVar), has_unbox_args),
          DAE.FP_PARALLEL_FUNCTION(),
        )
      end

      SCode.FR_PARALLEL_FUNCTION(__) => begin
        #=  Parallel function: non-builtin.
        =#
        @assign inline_ty = commentIsInlineFunc(cmt)
        DAE.FUNCTION_ATTRIBUTES(
          inline_ty,
          hasOMPure(cmt),
          false,
          is_partial,
          getBuiltin(def),
          DAE.FP_PARALLEL_FUNCTION(),
        )
      end

      SCode.FR_KERNEL_FUNCTION(__) => begin
        DAE.FUNCTION_ATTRIBUTES(
          DAE.NO_INLINE(),
          true,
          false,
          is_partial,
          DAE.FUNCTION_NOT_BUILTIN(),
          DAE.FP_KERNEL_FUNCTION(),
        )
      end

      _ => begin
        #=  Kernel functions: never builtin and never inlined.
        =#
        #=  Normal function.
        =#
        #        @assign inline_ty = commentIsInlineFunc(cmt) TODO
        inline_ty = DAE.NO_INLINE() #TODO tmp
        #=  In Modelica 3.2 and before, external functions with side-effects are not marked.
        =#
        @assign is_impure =
          SCodeUtil.isRestrictionImpure(
            res,
#            Config.languageStandardAtLeast(Config.LanguageStandard.V3_3) ||
            !listEmpty(outputs),
          ) || SCodeUtil.commentHasBooleanNamedAnnotation(
            cmt,
            "__ModelicaAssociation_Impure",
          )
        DAE.FUNCTION_ATTRIBUTES(
          inline_ty,
          hasOMPure(cmt),
          is_impure,
          is_partial,
          getBuiltin(def),
          DAE.FP_NON_PARALLEL(),
        )
      end
    end
  end
  return attr
end

function commentIsInlineFunc(cmt::SCode.Comment)
  return DAE.NO_INLINE()
end

""" #= Merges the function's comments from inherited classes. =#"""
function mergeFunctionAnnotations(comments::List{<:SCode.Comment})::SCode.Comment
  local outComment::SCode.Comment

  local comment::Option{String} = NONE()
  local mod::SCode.Mod = SCode.NOMOD()
  local mod2::SCode.Mod

  for cmt in comments
    if isNone(comment)
      @assign comment = cmt.comment
    end
    @assign mod = begin
      @match cmt begin
        SCode.COMMENT(annotation_ = SOME(SCode.ANNOTATION(modification = mod2))) => begin
          SCodeUtil.mergeModifiers(mod2, mod)
        end

        _ => begin
          mod
        end
      end
    end
  end
  @assign outComment = begin
    @match mod begin
      SCode.NOMOD(__) => begin
        SCode.COMMENT(NONE(), comment)
      end

      _ => begin
        SCode.COMMENT(SOME(SCode.ANNOTATION(mod)), comment)
      end
    end
  end
  return outComment
end


function getBuiltin(def::SCode.Element)::DAE.FunctionBuiltin
  local builtin::DAE.FunctionBuiltin = if SCodeUtil.isBuiltinElement(def)
    DAE.FUNCTION_BUILTIN_PTR()
  else
    DAE.FUNCTION_NOT_BUILTIN()
  end
  return builtin
end

function hasImpure(cmt::SCode.Comment)::Bool
  local res::Bool =
    SCodeUtil.commentHasBooleanNamedAnnotation(cmt, "__ModelicaAssociation_Impure")
  return res
end

function hasOMPure(cmt::SCode.Comment)::Bool
  local res::Bool =
    !SCodeUtil.commentHasBooleanNamedAnnotation(cmt, "__OpenModelica_Impure")
  return res
end

function makeSlot(@nospecialize(componentArg::InstNode), @nospecialize(index::Int))::Slot
  local slot::Slot
  local comp::Component
  local default::Option{Expression}
  local nameVar::String
  try
    @assign comp = component(componentArg)
    @assign default = typedExp(getImplicitBinding(comp))
    @assign nameVar = name(componentArg)
    if stringGet(nameVar, 1) == 36
      if stringLength(nameVar) > 4 && substring(nameVar, 1, 4) == "in_"
        @assign nameVar = substring(nameVar, 5, stringLength(nameVar))
      end
    end
    @assign slot = SLOT(
      name(componentArg),
      SlotType.GENERIC,
      default,
      NONE(),
      index,
      SlotEvalStatus.NOT_EVALUATED,
    )
  catch e
    #    Error.assertion(false, getInstanceName() + " got invalid component", sourceInfo())
    @error "Error. Our error was $e"
    fail()
  end
  #=  Remove $in_ for OM input output arguments.
  =#
  #= /*$*/ =#
  return slot
end

function makeSlots(@nospecialize(inputs::List{<:InstNode}))::List{Slot}
  local slots::List{Slot} = nil
  local index::Int = 1
  for i in inputs
    @assign slots = _cons(makeSlot(i, index), slots)
    @assign index = index + 1
  end
  @assign slots = listReverseInPlace(slots)
  return slots
end

function paramDirection(@nospecialize(componentArg::InstNode))::DirectionType
  local direction::DirectionType
  local cty::ConnectorType.TYPE
  local io::Int
  local vis::VisibilityType
  local var::VariabilityType

  @match ATTRIBUTES(
    connectorType = cty,
    direction = direction,
    innerOuter = io,
  ) = getAttributes(component(componentArg))
  @assign vis = visibility(componentArg)
  @assign var = variability(component(componentArg))
  #=  Function components may not be connectors.
  =#
  if isFlowOrStream(cty)
    Error.addSourceMessage(
      Error.INNER_OUTER_FORMAL_PARAMETER,
      list(ConnectorType.toString(cty), name(componentArg)),
      info(componentArg),
    )
    fail()
  end
  #=  Function components may not be inner/outer.
  =#
  if io != InnerOuter.NOT_INNER_OUTER
    # Error.addSourceMessage(
    #   Error.INNER_OUTER_FORMAL_PARAMETER,
    #   list(P_Prefixes.innerOuterString(io), name(componentArg)),
    #   info(component),
    # )
    fail()
  end
  #=  Formal parameters must be public, other function variables must be protected.
  =#
  if direction != Direction.NONE
    if vis == Visibility.PROTECTED
      # Error.addSourceMessage(
      #   Error.PROTECTED_FORMAL_FUNCTION_VAR,
      #   list(name(componentArg)),
      #   info(componentArg),
      # )
      @error "Formal parameters must be public, other function variables must be protected."
      fail()
    end
  elseif vis == Visibility.PUBLIC
    # Error.addSourceMessageAsError(
    #   Error.NON_FORMAL_PUBLIC_FUNCTION_VAR,
    #   list(name(componentArg)),
    #   info(componentArg),
    # )
#    componentArg),
      # )
      @error "Non formal public funcition variable"
    fail()
  end
  return direction
end

""" #= Sorts all the function parameters as inputs, outputs and locals. =#"""
function collectParams(
  @nospecialize(node::InstNode),
  inputs::List{<:InstNode} = nil,
  outputs::List{<:InstNode} = nil,
  locals::List{<:InstNode} = nil,
)::Tuple{List{InstNode}, List{InstNode}, List{InstNode}}

  local cls::Class
  local comps::Vector{InstNode}
  local n::InstNode

  # Error.assertion(
  #   isClass(node),
  #   getInstanceName() + " got non-class node",
  #   sourceInfo(),
  # ) TODO
  @assign cls = getClass(node)
  @assign () = begin
    @match cls begin
      INSTANCED_CLASS(elements = CLASS_TREE_FLAT_TREE(components = comps)) =>
        begin
          for i = arrayLength(comps):(-1):1
            @assign n = comps[i]
            @assign () = begin
              @match paramDirection(n) begin
                Direction.INPUT => begin
                  #=  Sort the components based on their direction.
                  =#
                  @assign inputs = _cons(n, inputs)
                  ()
                end

                Direction.OUTPUT => begin
                  @assign outputs = _cons(n, outputs)
                  ()
                end

                Direction.NONE => begin
                  @assign locals = _cons(n, locals)
                  ()
                end
              end
            end
          end
          ()
        end

      EXPANDED_DERIVED(__) => begin
        @assign (inputs, outputs, locals) = collectParams(cls.baseClass)
        ()
      end

      _ => begin
        Error.assertion(
          false,
          getInstanceName() + " got non-instantiated function",
          sourceInfo(),
        )
        fail()
      end
    end
  end
  return (inputs, outputs, locals)
end

function analyseUnusedParametersExp2(
  exp::Expression,
  params::List{<:InstNode},
)::List{InstNode}

  @assign () = begin
    @match exp begin
      CREF_EXPRESSION(__) => begin
        @assign params = ListUtil.deleteMemberOnTrue(
          exp.cref,
          params,
          containsNode,
        )
        ()
      end

      _ => begin
        ()
      end
    end
  end
  return params
end

function analyseUnusedParametersExp(
  exp::Expression,
  params::List{<:InstNode},
)::List{InstNode}

  if !listEmpty(params)
    @assign params = fold(exp, analyseUnusedParametersExp2, params)
  end
  return params
end

function analyseUnusedParameters(fn::M_Function)::List{Int}
  local unusedInputs::List{Int} = nil

  local inputs::List{InstNode}
  local index::Int

  @assign inputs = foldExp(fn, analyseUnusedParametersExp, fn.inputs)
  for i in inputs
    @assign index =
      ListUtil.positionOnTrue(fn.inputs, (i) -> refEqual(node1 = i))
    @assign unusedInputs = _cons(index, unusedInputs)
  end
  return unusedInputs
end

function getBody2(node::InstNode)::List{Statement}
  local body::List{Statement}

  local cls::Class = getClass(node)
  local fn_body::Algorithm

  @assign body = begin
    @match cls begin
      INSTANCED_CLASS(
        sections = SECTIONS_SECTIONS(algorithms = fn_body <| nil()),
      ) => begin
        fn_body.statements
      end
      INSTANCED_CLASS(sections = SECTIONS_EMPTY(__)) => begin
        nil
      end
      INSTANCED_CLASS(
        sections = SECTIONS_SECTIONS(algorithms = _ <| _),
      ) => begin
        Error.assertion(
          false,
          getInstanceName() + " got function with multiple algorithm sections",
          sourceInfo(),
        )
        fail()
      end
      TYPED_DERIVED(__) => begin
        getBody2(cls.baseClass)
      end
      _ => begin
        Error.assertion(false, getInstanceName() + " got unknown function", sourceInfo())
        fail()
      end
    end
  end
  return body
end

function makeReturnType(fn::M_Function)::M_Type
  local returnType::M_Type
  local ret_tyl::List{M_Type}
  @assign ret_tyl = list(getType(o) for o in fn.outputs)
  @assign returnType = begin
    @match ret_tyl begin
      nil() => begin
        TYPE_NORETCALL()
      end

      returnType <| nil() => begin
        returnType
      end

      _ => begin
        TYPE_TUPLE(ret_tyl, NONE())
      end
    end
  end
  return returnType
end
