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
  local var_dims::Integer
  local binding_dims::Integer
  @assign s = append(s, indent)
  if var.visibility == Visibility.PROTECTED
    @assign s = append(s, "protected ")
  end
  @assign s =
    append(s, P_Component.P_Attributes.toFlatString(var.attributes, var.ty))
  @assign s = append(s, Type.toFlatString(var.ty))
  @assign s = append(s, " ")
  @assign s = append(s, toFlatString(var.name))
  if !listEmpty(var.typeAttributes)
    @assign s = append(s, "(")
    @assign first = true
    @assign var_dims = Type.dimensionCount(var.ty)
    for a in var.typeAttributes
      if first
        @assign first = false
      else
        @assign s = append(s, ", ")
      end
      @assign b = Util.tuple22(a)
      @assign binding_dims =
        Type.dimensionCount(typeOf(P_Expression.Expression.getBindingExp(getExp(
          b,
        ))))
      if var_dims > binding_dims
        @assign s = append(s, "each ")
      end
      @assign s = append(s, Util.tuple21(a))
      @assign s = append(s, " = ")
      @assign s = append(s, toFlatString(b))
    end
    @assign s = append(s, ")")
  end
  if isBound(var.binding)
    @assign s = append(s, " = ")
    if printBindingType
      @assign s = append(s, "(")
      @assign s = append(s, Type.toFlatString(getType(var.binding)))
      @assign s = append(s, ") ")
    end
    @assign s = append(s, toFlatString(var.binding))
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
  @assign s = append(s, indent)
  if var.visibility == Visibility.PROTECTED
    @assign s = append(s, "protected ")
  end
  @assign s = append(s, P_Component.P_Attributes.toString(var.attributes, var.ty))
  @assign s = append(s, Type.toString(var.ty))
  @assign s = append(s, " ")
  @assign s = append(s, toString(var.name))
  if !listEmpty(var.typeAttributes)
    @assign s = append(s, "(")
    @assign first = true
    for a in var.typeAttributes
      if first
        @assign first = false
      else
        @assign s = append(s, ", ")
      end
      @assign b = Util.tuple22(a)
      if isEach(b)
        @assign s = append(s, "each ")
      end
      @assign s = append(s, Util.tuple21(a))
      @assign s = append(s, " = ")
      @assign s = append(s, toString(b))
    end
    @assign s = append(s, ")")
  end
  if isBound(var.binding)
    @assign s = append(s, " = ")
    if printBindingType
      @assign s = append(s, "(")
      @assign s = append(s, Type.toString(getType(var.binding)))
      @assign s = append(s, ") ")
    end
    @assign s = append(s, toString(var.binding))
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
  @assign s = create(getInstanceName(), ype.LIST())
  @assign s = toStream(var, indent, printBindingType, s)
  @assign str = string(s)
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
  @assign binding = EMPTY_BINDING
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

function fromCref(cref::ComponentRef)::Variable
  local variable::Variable
  local node::InstNode
  local comp::Component
  local ty::M_Type
  local binding::Binding
  local vis::VisibilityType
  local attr::Attributes
  local cmt::Option{SCode.Comment}
  local info::SourceInfo
  @assign node = node(cref)
  @assign comp = component(node)
  @assign ty = getSubscriptedType(cref)
  @assign binding = P_Component.getBinding(comp)
  @assign vis = visibility(node)
  @assign attr = getAttributes(comp)
  @assign cmt = P_Component.comment(comp)
  @assign info = info(node)
  @assign variable = VARIABLE(cref, ty, binding, vis, attr, nil, cmt, info)
  return variable
end
