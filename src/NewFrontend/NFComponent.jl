
Restriction = NFRestriction


using MetaModelica
using ExportAll


import ..DAE
Dimension = NFDimension
M_Type = NFType
Expression = NFExpression

@Uniontype Attributes begin
  @Record ATTRIBUTES begin
    connectorType::Integer #=ConnectorType.M_Type=#
    parallelism
    variability
    direction
    innerOuter
    isFinal::Bool
    isRedeclare::Bool
    isReplaceable
  end
end

@Uniontype Component begin
  @Record DELETED_COMPONENT begin
    component::Component
  end

  @Record TYPE_ATTRIBUTE begin
    ty::M_Type
    modifier::Modifier
  end

  @Record ENUM_LITERAL_COMPONENT begin
    literal::Expression
  end

  @Record ITERATOR_COMPONENT begin
    ty::M_Type
    variability
    info::SourceInfo
  end

  @Record TYPED_COMPONENT begin
    classInst::InstNode
    ty::M_Type
    binding::Binding
    condition::Binding
    attributes
    ann::Option{Modifier} #= the annotation from SCode.Comment as a modifier =#
    comment::Option{SCode.Comment}
    info::SourceInfo
  end

  @Record UNTYPED_COMPONENT begin
    classInst::InstNode
    dimensions::Array{Dimension}
    binding::Binding
    condition::Binding
    attributes
    comment::Option{SCode.Comment}
    instantiated::Bool
    info::SourceInfo
  end

  @Record COMPONENT_DEF begin
    definition::SCode.Element
    modifier::Modifier
  end
end

const DEFAULT_ATTR =
  ATTRIBUTES(
    ConnectorType.NON_CONNECTOR,
    Parallelism.NON_PARALLEL,
    Variability.CONTINUOUS,
    Direction.NONE,
    InnerOuter.NOT_INNER_OUTER,
    false,
    false,
    NOT_REPLACEABLE,
  )
const INPUT_ATTR =
  ATTRIBUTES(
    ConnectorType.NON_CONNECTOR,
    Parallelism.NON_PARALLEL,
    Variability.CONTINUOUS,
    Direction.INPUT,
    InnerOuter.NOT_INNER_OUTER,
    false,
    false,
    NOT_REPLACEABLE,
  )
const OUTPUT_ATTR =
  ATTRIBUTES(
    ConnectorType.NON_CONNECTOR,
    Parallelism.NON_PARALLEL,
    Variability.CONTINUOUS,
    Direction.OUTPUT,
    InnerOuter.NOT_INNER_OUTER,
    false,
    false,
    NOT_REPLACEABLE,
  )
const CONSTANT_ATTR =
  ATTRIBUTES(
    ConnectorType.NON_CONNECTOR,
    Parallelism.NON_PARALLEL,
    Variability.CONSTANT,
    Direction.NONE,
    InnerOuter.NOT_INNER_OUTER,
    false,
    false,
    NOT_REPLACEABLE,
  )
const IMPL_DISCRETE_ATTR =
  ATTRIBUTES(
    ConnectorType.NON_CONNECTOR,
    Parallelism.NON_PARALLEL,
    Variability.IMPLICITLY_DISCRETE,
    Direction.NONE,
    InnerOuter.NOT_INNER_OUTER,
    false,
    false,
    NOT_REPLACEABLE,
  )

#= Forward declarations for uniontypes until Julia adds support for mutual recursion =#

@UniontypeDecl Component

function isTypeAttribute(component::Component)::Bool
  local isAttribute::Bool
  @assign isAttribute = begin
    @match component begin
      TYPE_ATTRIBUTE(__) => begin
        true
      end
      _ => begin
        false
      end
    end
  end
  return isAttribute
end

function isDeleted(component::Component)::Bool
  local isDeleted::Bool
  @assign isDeleted = begin
    local condition::Binding
    @match component begin
      TYPED_COMPONENT(condition = condition) => begin
        isBound(condition) &&
        P_Expression.Expression.isFalse(getTypedExp(condition))
      end

      DELETED_COMPONENT(__) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return isDeleted
end

function getUnitAttribute(component::Component, defaultUnit::String = "")::String
  local unitString::String

  local binding::Binding
  local unit::Expression

  @assign binding =
    lookupAttributeBinding("unit", getClass(classInstance(component)))
  if isUnbound(binding)
    @assign unitString = defaultUnit
    return unitString
  end
  @assign unit = P_Expression.Expression.getBindingExp(getExp(binding))
  @assign unitString = begin
    @match unit begin
      STRING_EXPRESSION(__) => begin
        unit.value
      end

      _ => begin
        defaultUnit
      end
    end
  end
  return unitString
end

function getFixedAttribute(component::Component)::Bool
  local fixed::Bool

  local typeAttrs::List{Modifier} = nil
  local binding::Binding

  #=  for parameters the default is fixed = true
  =#
  @assign fixed = isParameter(component) || isStructuralParameter(component)
  @assign binding =
    lookupAttributeBinding("fixed", getClass(classInstance(component)))
  #=  no fixed attribute present
  =#
  if isUnbound(binding)
    return fixed
  end
  @assign fixed =
    fixed &&
    P_Expression.Expression.isTrue(P_Expression.Expression.getBindingExp(getExp(
      binding,
    )))
  return fixed
end

function getEvaluateAnnotation(component::Component)::Bool
  local evaluate::Bool

  local cmt::SCode.Comment

  @assign evaluate = SCodeUtil.getEvaluateAnnotation(comment(component))
  return evaluate
end

function ann(component::Component)::Option{Modifier}
  local ann::Option{Modifier}

  @assign ann = begin
    @match component begin
      TYPED_COMPONENT(__) => begin
        component.ann
      end

      _ => begin
        NONE()
      end
    end
  end
  return ann
end

function comment(component::Component)::Option{SCode.Comment}
  local comment::Option{SCode.Comment}

  @assign comment = begin
    @match component begin
      COMPONENT_DEF(__) => begin
        SCodeUtil.getElementComment(component.definition)
      end

      UNTYPED_COMPONENT(__) => begin
        component.comment
      end

      TYPED_COMPONENT(__) => begin
        component.comment
      end

      _ => begin
        NONE()
      end
    end
  end
  return comment
end

function dimensionCount(@nospecialize(component::Component))::Integer
  local count::Integer
  @assign count = begin
    @match component begin
      UNTYPED_COMPONENT(__) => begin
        arrayLength(component.dimensions)
      end
      TYPED_COMPONENT(__) => begin
        listLength(arrayDims(component.ty))
      end
      _ => begin
        0
      end
    end
  end
  return count
end

function setDimensions(dims::List{<:Dimension}, component::Component)::Component
  @assign () = begin
    @match component begin
      UNTYPED_COMPONENT(__) => begin
        @assign component.dimensions = listArray(dims)
        ()
      end
      TYPED_COMPONENT(__) => begin
        @assign component.ty =
          Type.liftArrayLeftList(arrayElementType(component.ty), dims)
        ()
      end
      _ => begin
        ()
      end
    end
  end
  return component
end

function toFlatString(name::String, component::Component)::String
  local str::String

  @assign str = begin
    local def::SCode.Element
    @match component begin
      TYPED_COMPONENT(__) => begin
        P_Attributes.toFlatString(component.attributes, component.ty) +
        Type.toFlatString(component.ty) +
        " '" +
        name +
        "'" +
        toFlatString(component.binding, " = ")
      end

      TYPE_ATTRIBUTE(__) => begin
        name + P_Modifier.toFlatString(component.modifier, printName = false)
      end
    end
  end
  return str
end

function toString(name::String, component::Component)::String
  local str::String

  @assign str = begin
    local def::SCode.Element
    @match component begin
      COMPONENT_DEF(definition = def && SCode.Element.COMPONENT(__)) => begin
        SCodeDump.unparseElementStr(def)
      end

      UNTYPED_COMPONENT(__) => begin
        toString(component.attributes, TYPE_UNKNOWN()) +
        InstNode.name(component.classInst) +
        " " +
        name +
        ListUtil.toString(
          arrayList(component.dimensions),
          P_Dimension.Dimension.toString,
          "",
          "[",
          ", ",
          "]",
          false,
        ) +
        toString(component.binding, " = ")
      end

      TYPED_COMPONENT(__) => begin
        toString(component.attributes, component.ty) +
        toString(component.ty) +
        " " +
        name +
        toString(component.binding, " = ")
      end

      TYPE_ATTRIBUTE(__) => begin
        name + P_Modifier.toString(component.modifier, printName = false)
      end
    end
  end
  return str
end

function isIdentical(comp1::Component, comp2::Component)::Bool
  local identical::Bool = false
  if referenceEq(comp1, comp2)
    @assign identical = true
  else
    @assign identical = begin
      @match (comp1, comp2) begin
        (UNTYPED_COMPONENT(__), UNTYPED_COMPONENT(__)) => begin
          if !isIdentical(
            getClass(comp1.classInst),
            getClass(comp2.classInst),
          )
            return
          end
          if !isEqual(comp1.binding, comp2.binding)
            return
          end
          true
        end

        _ => begin
          true
        end
      end
    end
  end
  return identical
end

function isExternalObject(component::Component)::Bool
  local isEO::Bool
  @assign isEO = begin
    @match component begin
      UNTYPED_COMPONENT(__) => begin
        isExternalObject(getClass(component.classInst))
      end

      TYPED_COMPONENT(__) => begin
        Type.isExternalObject(component.ty)
      end

      _ => begin
        false
      end
    end
  end
  return isEO
end

function isExpandableConnector(component::Component)::Bool
  local isConnector::Bool = ConnectorType.isExpandable(connectorType(component))
  return isConnector
end

function isConnector(component::Component)::Bool
  return isConnectorType(connectorType(component))
end

function isFlow(component::Component)::Bool
  return isFlow(connectorType(component))
end

function setConnectorType(cty::ConnectorType.TYPE, component::Component)::Component
  @assign () = begin
    local attr::Attributes
    @match component begin
      UNTYPED_COMPONENT(attributes = attr) => begin
        @assign attr.connectorType = cty
        @assign component.attributes = attr
        ()
      end
      TYPED_COMPONENT(attributes = attr) => begin
        @assign attr.connectorType = cty
        @assign component.attributes = attr
        ()
      end
      _ => begin
        ()
      end
    end
  end
  return component
end

function connectorType(component::Component)::ConnectorType.TYPE
  local cty::ConnectorType.TYPE
  @assign cty = begin
    @match component begin
      UNTYPED_COMPONENT(attributes = ATTRIBUTES(connectorType = cty)) => begin
          cty
        end
      TYPED_COMPONENT(attributes = ATTRIBUTES(connectorType = cty)) => begin
        cty
      end
      DELETED_COMPONENT(__) => begin
        connectorType(component.component)
      end
      _ => begin
        NON_CONNECTOR
      end
    end
  end
  return cty
end

function isOnlyOuter(component::Component)::Bool
  local isOuter::Bool = innerOuter(component) == InnerOuter.OUTER
  return isOuter
end

function isOuter(component::Component)::Bool
  local isOuter::Bool

  local io = innerOuter(component)

  @assign isOuter = io == InnerOuter.OUTER || io == InnerOuter.INNER_OUTER
  return isOuter
end

function isInner(component::Component)::Bool
  local isInner::Bool

  local io = innerOuter(component)

  @assign isInner = io == InnerOuter.INNER || io == InnerOuter.INNER_OUTER
  return isInner
end

function innerOuter(component::Component)
  local io
  @assign io = begin
    @match component begin
      UNTYPED_COMPONENT(attributes = ATTRIBUTES(innerOuter = io)) => begin
        io
      end

      TYPED_COMPONENT(attributes = ATTRIBUTES(innerOuter = io)) => begin
        io
      end

      COMPONENT_DEF(__) => begin
        innerOuterFromSCode(SCodeUtil.prefixesInnerOuter(SCodeUtil.elementPrefixes(component.definition)))
      end

      _ => begin
        InnerOuter.NOT_INNER_OUTER
      end
    end
  end
  return io
end

function isFinal(component::Component)::Bool
  local isFinal::Bool

  @assign isFinal = begin
    @match component begin
      COMPONENT_DEF(__) => begin
        SCodeUtil.finalBool(SCodeUtil.prefixesFinal(SCodeUtil.elementPrefixes(component.definition)))
      end
      UNTYPED_COMPONENT(attributes = ATTRIBUTES(isFinal = isFinal)) => begin
        isFinal
      end
      TYPED_COMPONENT(attributes = ATTRIBUTES(isFinal = isFinal)) => begin
        isFinal
      end

      _ => begin
        false
      end
    end
  end
  return isFinal
end

function isRedeclare(component::Component)::Bool
  local isRedeclare::Bool
  @assign isRedeclare = begin
    @match component begin
      COMPONENT_DEF(__) => begin
        SCodeUtil.isElementRedeclare(component.definition)
      end
      _ => begin
        false
      end
    end
  end
  return isRedeclare
end

function isVar(component::Component)::Bool
  local isVar::Bool = variability(component) == CONTINIUOUS
  return isVar
end

function isStructuralParameter(component::Component)::Bool
  local b::Bool = variability(component) == Variability.STRUCTURAL_PARAMETER
  return b
end

function isParameter(component::Component)::Bool
  local b::Bool = variability(component) == Variability.PARAMETER
  return b
end

function isConst(component::Component)::Bool
  local isConst::Bool = variability(component) == Variability.CONSTANT
  return isConst
end

function setVariability(variability, component::Component)::Component

  @assign () = begin
    local attr::Attributes
    @match component begin
      UNTYPED_COMPONENT(attributes = attr) => begin
        @assign attr.variability = variability
        @assign component.attributes = attr
        ()
      end

      TYPED_COMPONENT(attributes = attr) => begin
        @assign attr.variability = variability
        @assign component.attributes = attr
        ()
      end

      _ => begin
        ()
      end
    end
  end
  return component
end

function variability(component::Component)
  local variability
  @assign variability = begin
    @match component begin
      TYPED_COMPONENT(attributes = ATTRIBUTES(variability = variability)) =>
        begin
          variability
        end

      UNTYPED_COMPONENT(attributes = ATTRIBUTES(variability = variability)) => begin
        variability
      end

      ITERATOR(__) => begin
        component.variability
      end

      ENUM_LITERAL(__) => begin
        Variability.CONSTANT
      end

      _ => begin
        CONTINIUOUS
      end
    end
  end
  return variability
end

function parallelism(component::Component)::Parallelism
  local parallelism::Parallelism

  @assign parallelism = begin
    @match component begin
      TYPED_COMPONENT(attributes = ATTRIBUTES(parallelism = parallelism)) => begin
        parallelism
      end

      UNTYPED_COMPONENT(attributes = ATTRIBUTES(parallelism = parallelism)) => begin
        parallelism
      end

      _ => begin
        NON_PARALLEL
      end
    end
  end
  return parallelism
end

function isOutput(component::Component)::Bool
  local isOutput::Bool = direction(component) == Direction.OUTPUT
  return isOutput
end

function makeInput(component::Component)::Component

  local attr::Attributes

  @assign () = begin
    @match component begin
      UNTYPED_COMPONENT(attributes = attr) => begin
        @assign attr.direction = Direction.INPUT
        @assign component.attributes = attr
        ()
      end

      TYPED_COMPONENT(attributes = attr) => begin
        @assign attr.direction = Direction.INPUT
        @assign component.attributes = attr
        ()
      end

      _ => begin
        ()
      end
    end
  end
  return component
end

function isInput(component::Component)::Bool
  local isInput::Bool = direction(component) == Direction.INPUT
  return isInput
end

function direction(component::Component)
  local direction::DirectionType

  @assign direction = begin
    @match component begin
      TYPED_COMPONENT(attributes = P_Attributes.ATTRIBUTES(direction = direction)) =>
        begin
          direction
        end

      UNTYPED_COMPONENT(attributes = P_Attributes.ATTRIBUTES(direction = direction)) =>
        begin
          direction
        end

      _ => begin
        Direction.NONE
      end
    end
  end
  return direction
end

function hasCondition(component::Component)::Bool
  local b::Bool

  @assign b = isBound(getCondition(component))
  return b
end

function getCondition(component::Component)::Binding
  local cond::Binding

  @assign cond = begin
    @match component begin
      UNTYPED_COMPONENT(__) => begin
        component.condition
      end

      TYPED_COMPONENT(__) => begin
        component.condition
      end

      _ => begin
        EMPTY_BINDING
      end
    end
  end
  return cond
end

function hasBinding(component::Component, parent::InstNode = EMPTY_NODE())::Bool
  local b::Bool

  local cls::Class
  local children::Array{InstNode}

  if isBound(getBinding(component))
    @assign b = true
    return b
  end
  #=  Simple case, component has normal binding equation.
  =#
  #=  Complex case, component might be a record instance where each field has
  =#
  #=  its own binding equation.
  =#
  @assign cls = getClass(classInstance(component))
  if !P_Restriction.Restriction.isRecord(restriction(cls))
    @assign b = false
    return b
  end
  #=  Not record.
  =#
  #=  Check if any child of this component is missing a binding.
  =#
  @assign children = getComponents(classTree(cls))
  for c in children
    if isComponent(c) && !hasBinding(component(c))
      @assign b = false
      return b
    end
  end
  @assign b = true
  return b
end

function setBinding(binding::Binding, component::Component)::Component

  @assign () = begin
    @match component begin
      UNTYPED_COMPONENT(__) => begin
        @assign component.binding = binding
        ()
      end

      TYPED_COMPONENT(__) => begin
        @assign component.binding = binding
        ()
      end

      TYPE_ATTRIBUTE(__) => begin
        @assign component.modifier = P_Modifier.setBinding(binding, component.modifier)
        ()
      end
    end
  end
  return component
end

""" #= Returns the component's binding. If the component does not have a binding
     and is a record instance it will try to create a binding from the
     component's children. =#"""
function getImplicitBinding(component::Component)::Binding
  local binding::Binding

  local cls_node::InstNode
  local record_exp::Expression

  @assign binding = getBinding(component)
  if isUnbound(binding)
    @assign cls_node = classInstance(component)
    if isRecord(cls_node)
      try
        @assign record_exp = makeRecordExp(cls_node)
        @assign binding = FLAT_BINDING(
          record_exp,
          P_Expression.Expression.variability(record_exp),
        )
      catch
      end
    end
  end
  return binding
end

function getBinding(component::Component)::Binding
  local b::Binding

  @assign b = begin
    @match component begin
      UNTYPED_COMPONENT(__) => begin
        component.binding
      end

      TYPED_COMPONENT(__) => begin
        component.binding
      end

      TYPE_ATTRIBUTE(__) => begin
        P_Modifier.binding(component.modifier)
      end

      _ => begin
        EMPTY_BINDING
      end
    end
  end
  return b
end

function setAttributes(attr, component::Component)::Component

  @assign () = begin
    @match component begin
      UNTYPED_COMPONENT(__) => begin
        @assign component.attributes = attr
        ()
      end

      TYPED_COMPONENT(__) => begin
        @assign component.attributes = attr
        ()
      end
    end
  end
  return component
end

function getAttributes(component::Component)
  local attr

  @assign attr = begin
    @match component begin
      UNTYPED_COMPONENT(__) => begin
        component.attributes
      end

      TYPED_COMPONENT(__) => begin
        component.attributes
      end
    end
  end
  return attr
end

function unliftType(component::Component)::Component

  @assign () = begin
    local ty::M_Type
    @match component begin
      TYPED_COMPONENT(ty = ARRAY_TYPE(elementType = ty)) => begin
        @assign component.ty = ty
        ()
      end

      ITERATOR(ty = ARRAY_TYPE(elementType = ty)) => begin
        @assign component.ty = ty
        ()
      end

      _ => begin
        ()
      end
    end
  end
  return component
end

function isTyped(component::Component)::Bool
  local isTyped::Bool

  @assign isTyped = begin
    @match component begin
      TYPED_COMPONENT(__) => begin
        true
      end

      ITERATOR(ty = TYPE_UNKNOWN(__)) => begin
        false
      end

      ITERATOR(__) => begin
        true
      end

      TYPE_ATTRIBUTE(__) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return isTyped
end

function setType(ty::M_Type, component::Component)::Component

  @assign component = begin
    @match component begin
      UNTYPED_COMPONENT(__) => begin
        TYPED_COMPONENT(
          component.classInst,
          ty,
          component.binding,
          component.condition,
          component.attributes,
          NONE(),
          component.comment,
          component.info,
        )
      end

      TYPED_COMPONENT(__) => begin
        @assign component.ty = ty
        component
      end

      ITERATOR(__) => begin
        @assign component.ty = ty
        component
      end
    end
  end
  return component
end

function getType(component::Component)::M_Type
  local ty::M_Type

  @assign ty = begin
    @match component begin
      TYPED_COMPONENT(__) => begin
        component.ty
      end

      UNTYPED_COMPONENT(__) => begin
        getType(component.classInst)
      end

      ITERATOR(__) => begin
        component.ty
      end

      TYPE_ATTRIBUTE(__) => begin
        component.ty
      end

      DELETED_COMPONENT(__) => begin
        getType(component.component)
      end

      _ => begin
        TYPE_UNKNOWN()
      end
    end
  end
  return ty
end

function mergeModifier(modifier::Modifier, component::Component)::Component
  @assign component = begin
    @match component begin
      COMPONENT_DEF(__) => begin
        @assign component.modifier = merge(modifier, component.modifier)
        component
      end
      TYPE_ATTRIBUTE(__) => begin
        TYPE_ATTRIBUTE(component.ty, merge(modifier, component.modifier))
      end
    end
  end
  return component
end

function setModifier(modifier::Modifier, component::Component)::Component

  @assign () = begin
    @match component begin
      COMPONENT_DEF(__) => begin
        @assign component.modifier = modifier
        ()
      end

      TYPE_ATTRIBUTE(__) => begin
        @assign component.modifier = modifier
        ()
      end
    end
  end
  return component
end

function getModifier(component::Component)::Modifier
  local modifier::Modifier

  @assign modifier = begin
    @match component begin
      COMPONENT_DEF(__) => begin
        component.modifier
      end

      TYPE_ATTRIBUTE(__) => begin
        component.modifier
      end

      _ => begin
        MODIFIER_NOMOD()
      end
    end
  end
  return modifier
end

function setClassInstance(classInst::InstNode, component::Component)::Component

  @assign () = begin
    @match component begin
      UNTYPED_COMPONENT(__) => begin
        @assign component.classInst = classInst
        ()
      end

      TYPED_COMPONENT(__) => begin
        @assign component.classInst = classInst
        ()
      end
    end
  end
  return component
end

function classInstance(component::Component)::InstNode
  local classInst::InstNode

  @assign classInst = begin
    @match component begin
      UNTYPED_COMPONENT(__) => begin
        component.classInst
      end

      TYPED_COMPONENT(__) => begin
        component.classInst
      end
    end
  end
  return classInst
end

""" #= This function shouldn't be used! Use InstNode.info instead, so that e.g.
     enumeration literals can be handled correctly. =#"""
function info(component::Component)::SourceInfo
  local info::SourceInfo

  @assign info = begin
    @match component begin
      COMPONENT_DEF(__) => begin
        SCodeUtil.elementInfo(component.definition)
      end

      UNTYPED_COMPONENT(__) => begin
        component.info
      end

      TYPED_COMPONENT(__) => begin
        component.info
      end

      ITERATOR(__) => begin
        component.info
      end

      TYPE_ATTRIBUTE(__) => begin
        P_Modifier.info(component.modifier)
      end

      DELETED_COMPONENT(__) => begin
        info(component.component)
      end
    end
  end
  #=  Fail for enumeration literals, InstNode.info handles that case instead.
  =#
  return info
end

function isDefinition(component::Component)::Bool
  local isDefinition::Bool

  @assign isDefinition = begin
    @match component begin
      COMPONENT_DEF(__) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return isDefinition
end

function definition(component::Component)::SCode.Element
  local definition::SCode.Element

  @match COMPONENT_DEF(definition = definition) = component
  return definition
end

function newEnum(enumType::M_Type, literalName::String, literalIndex::Integer)::Component
  local component::Component

  @assign component =
    ENUM_LITERAL(P_Expression.Expression.ENUM_LITERAL(enumType, literalName, literalIndex))
  return component
end

function new(definition::SCode.Element)::Component
  local component::Component

  @assign component = COMPONENT_DEF(definition, MODIFIER_NOMOD())
  return component
end


using MetaModelica
using ExportAll

@UniontypeDecl Attributes

function toFlatString(attr::Attributes, ty::M_Type)::String
  local str::String

  @assign str =
    P_Prefixes.unparseVariability(attr.variability, ty) +
    P_Prefixes.unparseDirection(attr.direction)
  return str
end

function toString(attr::Attributes, ty::M_Type)::String
  local str::String

  @assign str =
    (
      if attr.isRedeclare
        "redeclare "
      else
        ""
      end
    ) +
    (
      if attr.isFinal
        "final "
      else
        ""
      end
    ) +
    P_Prefixes.unparseInnerOuter(attr.innerOuter) +
    P_Prefixes.unparseReplaceable(attr.isReplaceable) +
    P_Prefixes.unparseParallelism(attr.parallelism) +
    ConnectorType.unparse(attr.connectorType) +
    P_Prefixes.unparseVariability(attr.variability, ty) +
    P_Prefixes.unparseDirection(attr.direction)
  return str
end

function toDAE(ina::Attributes, vis)::DAE.Attributes
  local outa::DAE.Attributes
  @assign outa = DAE.ATTR(
    toDAE(ina.connectorType),
    parallelismToSCode(ina.parallelism),
    variabilityToSCode(ina.variability),
    directionToAbsyn(ina.direction),
    innerOuterToAbsyn(ina.innerOuter),
    visibilityToSCode(vis),
  )
  return outa
end
