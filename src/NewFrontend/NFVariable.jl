@Uniontype NFVariable begin
  @Record VARIABLE begin
    name::ComponentRef
    ty::NFType
    binding::Binding
    visibility::VisibilityType
    attributes::Attributes
    typeAttributes::Vector{Tuple{String, Binding}}
    comment::Option{SCode.Comment}
    info::SourceInfo
  end
end
const Variable = NFVariable

function toFlatStream(var::Variable,
  indent::String = "",
  printBindingType::Bool = false,
  s = Nothing,
)
  local first::Bool
  local b::Binding
  local var_dims::Int
  local binding_dims::Int
  s = IOStream_M.append(s, indent)
  if var.visibility == Visibility.PROTECTED
    s = IOStream_M.append(s, "protected ")
  end
  s = IOStream_M.append(s, toFlatString(var.attributes, var.ty; isTopLevel = isSimple(var.name)))
  s = IOStream_M.append(s, toFlatString(var.ty))
  s = IOStream_M.append(s, " ")
  s = IOStream_M.append(s, toFlatString(var.name))
  if !isempty(var.typeAttributes)
    s = IOStream_M.append(s, "(")
    first = true
    var_dims = dimensionCount(var.ty)
    for a in var.typeAttributes
      if first
        first = false
      else
        s = IOStream_M.append(s, ", ")
      end
      b = Util.tuple22(a)
      binding_dims =
        dimensionCount(typeOf(getBindingExp(getExp(
          b,
        ))))
      if var_dims > binding_dims
        s = append(s, "each ")
      end
      s = IOStream_M.append(s, Util.tuple21(a))
      s = IOStream_M.append(s, " = ")
      s = IOStream_M.append(s, toFlatString(b))
    end
    s = IOStream_M.append(s, ")")
  end
  if isBound(var.binding)
    s = IOStream_M.append(s, " = ")
    if printBindingType
      s = IOStream_M.append(s, "(")
      s = IOStream_M.append(s, toFlatString(getType(var.binding)))
      s = IOStream_M.append(s, ") ")
    end
    s = IOStream_M.append(s, toFlatString(var.binding))
  end
  return s
end

function toStream(
  var::Variable,
  indent::String = "",
  printBindingType::Bool = false,
  s = Nothing,
  )
  local first::Bool
  local b::Binding
  s = IOStream_M.append(s, indent)
  if var.visibility == Visibility.PROTECTED
    s = IOStream_M.append(s, "protected ")
  end
  s = IOStream_M.append(s, toString(var.attributes, var.ty))
  s = IOStream_M.append(s, toString(var.ty))
  s = IOStream_M.append(s, " ")
  s = IOStream_M.append(s, toString(var.name))
  if !isempty(var.typeAttributes)
    s = IOStream_M.append(s, "(")
    first = true
    for a in var.typeAttributes
      if first
        first = false
      else
        s = IOStream_M.append(s, ", ")
      end
      b = Util.tuple22(a)
      if isEach(b)
        s = append(s, "each ")
      end
      s = IOStream_M.append(s, Util.tuple21(a))
      s = IOStream_M.append(s, " = ")
      s = IOStream_M.append(s, toString(b))
    end
    s = IOStream_M.append(s, ")")
  end
  if isBound(var.binding)
    @assign s = IOStream_M.append(s, " = ")
    if printBindingType
      @assign s = IOStream_M.append(s, "(")
      @assign s = IOStream_M.append(s, Type.toString(getType(var.binding)))
      @assign s = IOStream_M.append(s, ") ")
    end
    @assign s = IOStream_M.append(s, toString(var.binding))
  end
  return s
end

function toString(
  var::Variable,
  indent::String = "",
  printBindingType::Bool = false,
)::String
  local str::String
  local s  = IOStream_M.create(getInstanceName(), IOStream_M.LIST())
  s = toStream(var, indent, printBindingType, s)
  str = IOStream_M.string(s)
  return str
end

function lookupTypeAttribute(name::String, var::Variable)::Binding
  local binding::Binding
  for attr in var.typeAttributes
    if Util.tuple21(attr) == name
      @assign binding = Util.tuple22(attr)
      return binding
    end
  end
  binding = EMPTY_BINDING()
  return binding
end

function isPresent(variable::Variable)::Bool
  local present::Bool =
    !isPotentiallyPresent(variable.attributes.connectorType)
  return present
end

function isDeleted(variable::Variable)::Bool
  local deleted::Bool
  local node::InstNode
  @assign node = node(variable.name)
  @assign deleted =
    isComponent(node) && P_Component.isDeleted(component(node))
  return deleted
end

function isEmptyArray(variable::Variable)::Bool
  local isEmpty::Bool = isEmptyArray(variable.ty)
  return isEmpty
end

function variability(variable::Variable)::VariabilityType
  local variability::VariabilityType = variable.attributes.variability
  return variability
end

function isStructural(variable::Variable)::Bool
  local structural::Bool =
    variable.attributes.variability <= Variability.STRUCTURAL_PARAMETER
  return structural
end

function Variable_fromCref(cref::ComponentRef)::Variable
  local variable::Variable
  local nodeVar::InstNode
  local comp::Component
  local ty::M_Type
  local binding::Binding
  local vis::VisibilityType
  local attr::Attributes
  local cmt::Option{SCode.Comment}
  local infoVar::SourceInfo
  nodeVar = node(cref)
  comp = component(nodeVar)
  ty = getSubscriptedType(cref)
  binding = getBinding(comp)
  vis = visibility(nodeVar)
  attr = getAttributes(comp)
  cmt = comment(comp)
  infoVar = InstNode_info(nodeVar)
  variable = VARIABLE(cref, ty, binding, vis, attr, nil, cmt, infoVar)
  return variable
end
