@UniontypeDecl Field
function name(field::Field)::String
  local name::String
  @assign name = begin
    @match field begin
      INPUT(__) => begin
        field.name
      end
      LOCAL(__) => begin
        field.name
      end
    end
  end
  return name
end

function isInput(field::Field)::Bool
  local isInput::Bool
  @assign isInput = begin
    @match field begin
      INPUT(__) => begin
        true
      end
      _ => begin
        false
      end
    end
  end
  return isInput
end

@Uniontype Field begin
  @Record LOCAL begin
    name::String
  end
  @Record INPUT begin
    name::String
  end
end

function instDefaultConstructor(
  path::Absyn.Path,
  node::InstNode,
  info::SourceInfo,
)::InstNode
  local inputs::List{InstNode}
  local locals::List{InstNode}
  local all_params::List{InstNode}
  local attr::DAE.FunctionAttributes
  local status::Pointer{FunctionStatus}
  local ctor_node::InstNode
  local out_rec::InstNode
  local out_comp::Component
  local ctor_cls::Class
  local ty_node::InstNode
  try
    @assign ctor_node = lookupLocalSimpleName(
      name(node),
      classScope(parent(node)),
    )
    @match true = referenceEq(definition(node), definition(ctor_node))
  catch
    @assign ctor_node = replaceClass(NOT_INSTANTIATED(), node)
  end
  @assign ctor_node = Inst.instantiate(ctor_node)
  Inst.instExpressions(ctor_node)
  #=  Collect the record fields.
  =#
  @assign (inputs, locals, all_params) = collectRecordParams(ctor_node)
  #=  Create the output record element, using the instance created above as both parent and type.
  =#
  @assign out_comp = UNTYPED_COMPONENT(
    ctor_node,
    listArray(nil),
    EMPTY_BINDING,
    EMPTY_BINDING,
    NFComponent.OUTPUT_ATTR,
    NONE(),
    false,
    AbsynUtil.dummyInfo,
  )
  @assign out_rec =
    fromComponent("out" + name(ctor_node), out_comp, ctor_node)
  #=  Make a record constructor class and create a node for the constructor.
  =#
  @assign ctor_cls = makeRecordConstructor(all_params, out_rec)
  @assign ctor_node = replaceClass(ctor_cls, ctor_node)
  #=  Create the constructor function and add it to the function cache.
  =#
  @assign attr = DAE.FUNCTION_ATTRIBUTES_DEFAULT
  @assign status = P_Pointer.create(FunctionStatus.INITIAL)
  cacheAddFunc(
    node,
    P_Function.FUNCTION(
      path,
      ctor_node,
      inputs,
      list(out_rec),
      locals,
      nil,
      TYPE_UNKNOWN(),
      attr,
      nil,
      status,
      P_Pointer.create(0),
    ),
    false,
  )
  return node
end

function collectRecordParams(
  recNode::InstNode,
)::Tuple{List{InstNode}, List{InstNode}, List{InstNode}}
  local allParams::List{InstNode} = nil
  local locals::List{InstNode} = nil
  local inputs::List{InstNode} = nil
  local comp::InstNode
  local comps::Array{InstNode}
  local pcomps::Array{Pointer{InstNode}}
  local tree::ClassTree
  @assign tree = classTree(getClass(recNode))
  @assign () = begin
    @match tree begin
      CLASS_TREE_FLAT_TREE(components = comps) => begin
        for i = arrayLength(comps):(-1):1
          @assign comp = comps[i]
          @assign (inputs, locals) = collectRecordParam(comp, inputs, locals)
          @assign allParams = _cons(comp, allParams)
        end
        ()
      end
      INSTANTIATED_TREE(components = pcomps) => begin
        for i = arrayLength(pcomps):(-1):1
          @assign comp = P_Pointer.access(pcomps[i])
          @assign (inputs, locals) = collectRecordParam(comp, inputs, locals)
          @assign allParams = _cons(comp, allParams)
        end
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
  return (inputs, locals, allParams)
end

function collectRecordParam(
  component::InstNode,
  inputs::List{<:InstNode},
  locals::List{<:InstNode},
)::Tuple{List{InstNode}, List{InstNode}}
  local comp::Component
  local comp_node::InstNode = resolveInner(component)
  if isProtected(comp_node)
    @assign locals = _cons(comp_node, locals)
    return (inputs, locals)
  end
  @assign comp = component(comp_node)
  if P_Component.isConst(comp) && P_Component.hasBinding(comp)
    @assign locals = _cons(comp_node, locals)
  else
    @assign inputs = _cons(comp_node, inputs)
  end
  return (inputs, locals)
end

function collectRecordFields(recNode::InstNode)::List{Field}
  local fields::List{Field}
  local tree::ClassTree
  @assign tree = classTree(getClass(recNode))
  @assign fields = foldComponents(tree, collectRecordField, nil)
  @assign fields = listReverseInPlace(fields)
  return fields
end

function collectRecordField(component::InstNode, fields::List{<:Field})::List{Field}
  local comp_node::InstNode = resolveInner(component)
  local comp::Component
  if isProtected(comp_node)
    @assign fields = _cons(P_Field.LOCAL(name(comp_node)), fields)
  else
    @assign comp = component(comp_node)
    if P_Component.isConst(comp) && P_Component.hasBinding(comp)
      @assign fields = _cons(P_Field.LOCAL(name(comp_node)), fields)
    elseif !P_Component.isOutput(comp)
      @assign fields = _cons(P_Field.INPUT(name(comp_node)), fields)
    end
  end
  return fields
end

function fieldsToDAE(fields::List{<:Field})::List{String}
  local fieldNames::List{String} = nil
  for field in fields
    @assign () = begin
      @match field begin
        INPUT(__) => begin
          @assign fieldNames = _cons(field.name, fieldNames)
          ()
        end
        _ => begin
          ()
        end
      end
    end
  end
  return fieldNames
end

function foldInputFields(
  fields::List{Field},
  args::List{T},
  func::FuncT,
  foldArg::ArgT,
) where {T, ArgT}
  local arg::T
  local rest_args::List{T} = args
  for field in fields
    @match _cons(arg, rest_args) = rest_args
    if P_Field.isInput(field)
      @assign foldArg = func(arg, foldArg)
    end
  end
  return foldArg
end
