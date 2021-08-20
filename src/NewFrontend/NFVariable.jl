@UniontypeDecl NFVariable
ComponentRef = NFComponentRef
@Uniontype NFVariable begin
  @Record VARIABLE begin
    name::ComponentRef
    ty::NFType
    binding::Binding
    visibility::VisibilityType
    attributes::Attributes
    typeAttributes::List{Tuple{String, Binding}}
    comment::Option{SCode.Comment}
    info::SourceInfo
  end
end
const Variable = NFVariable

function toFlatStream(
  var::Variable,
  indent::String = "",
  printBindingType::Bool = false,
  s = Nothing,
)
  local first::Bool
  local b::Binding
  local var_dims::Int
  local binding_dims::Int
  s = append(s, indent)
  if var.visibility == Visibility.PROTECTED
    s = append(s, "protected ")
  end
  s =
    append(s, P_Component.P_Attributes.toFlatString(var.attributes, var.ty))
  s = append(s, Type.toFlatString(var.ty))
  s = append(s, " ")
  s = append(s, toFlatString(var.name))
  if !listEmpty(var.typeAttributes)
    s = append(s, "(")
    first = true
    var_dims = Type.dimensionCount(var.ty)
    for a in var.typeAttributes
      if first
        first = false
      else
        s = append(s, ", ")
      end
      b = Util.tuple22(a)
      binding_dims =
        Type.dimensionCount(typeOf(getBindingExp(getExp(
          b,
        ))))
      if var_dims > binding_dims
        s = append(s, "each ")
      end
      s = append(s, Util.tuple21(a))
      s = append(s, " = ")
      s = append(s, toFlatString(b))
    end
    s = append(s, ")")
  end
  if isBound(var.binding)
    s = append(s, " = ")
    if printBindingType
      s = append(s, "(")
      s = append(s, Type.toFlatString(getType(var.binding)))
      s = append(s, ") ")
    end
    s = append(s, toFlatString(var.binding))
  end
  return s
end

function toStream(
  var::Variable,
  indent::String = "",
  printBindingType::Bool = false,
  s = Nothin,
)
  local first::Bool
  local b::Binding
  s = append(s, indent)
  if var.visibility == Visibility.PROTECTED
    s = append(s, "protected ")
  end
  s = append(s, P_Component.P_Attributes.toString(var.attributes, var.ty))
  s = append(s, Type.toString(var.ty))
  s = append(s, " ")
  s = append(s, toString(var.name))
  if !listEmpty(var.typeAttributes)
    s = append(s, "(")
    first = true
    for a in var.typeAttributes
      if first
        first = false
      else
        s = append(s, ", ")
      end
      b = Util.tuple22(a)
      if isEach(b)
        s = append(s, "each ")
      end
      s = append(s, Util.tuple21(a))
      s = append(s, " = ")
      s = append(s, toString(b))
    end
    s = append(s, ")")
  end
  if isBound(var.binding)
    s = append(s, " = ")
    if printBindingType
      s = append(s, "(")
      s = append(s, Type.toString(getType(var.binding)))
      s = append(s, ") ")
    end
    s = append(s, toString(var.binding))
  end
  return s
end

function toString(
  var::Variable,
  indent::String = "",
  printBindingType::Bool = false,
)::String
  local str::String
  local s
  s = create(getInstanceName(), ype.LIST())
  s = toStream(var, indent, printBindingType, s)
  str = string(s)
  return str
end

function lookupTypeAttribute(name::String, var::Variable)::Binding
  local binding::Binding

  for attr in var.typeAttributes
    if Util.tuple21(attr) == name
      binding = Util.tuple22(attr)
      return binding
    end
  end
  binding = EMPTY_BINDING
  return binding
end

function isPresent(variable::Variable)::Bool
  local present::Bool =
    !ConnectorType.isPotentiallyPresent(variable.attributes.connectorType)
  return present
end

function isDeleted(variable::Variable)::Bool
  local deleted::Bool
  local node::InstNode
  node = node(variable.name)
  deleted =
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
  local node::InstNode
  local comp::Component
  local ty::M_Type
  local binding::Binding
  local vis::VisibilityType
  local attr::Attributes
  local cmt::Option{SCode.Comment}
  local info::SourceInfo
  node = node(cref)
  comp = component(node)
  ty = getSubscriptedType(cref)
  binding = getBinding(comp)
  vis = visibility(node)
  attr = getAttributes(comp)
  cmt = P_Component.comment(comp)
  info = info(node)
  variable = VARIABLE(cref, ty, binding, vis, attr, nil, cmt, info)
  return variable
end
