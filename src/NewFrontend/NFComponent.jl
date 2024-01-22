using MetaModelica
using ExportAll

abstract type Attributes end

"""
  Attributes is a constant struct.
  Note, can not be optimized into a mutable type, since DEFAULT attribute is used quite a lot and it would result in errors when doing comparisions by reference for attributes.
"""
struct ATTRIBUTES <: Attributes
  connectorType::Int
  parallelism::Int
  variability::Int
  direction::Int
  innerOuter::Int
  isFinal::Bool
  isRedeclare::Bool
  isReplaceable::Replaceable
  isStructuralMode::Bool
end

@Mutable_Uniontype Component begin
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
    attributes::ATTRIBUTES
    ann::Option{Modifier} #= the annotation from SCode.Comment as a modifier =#
    comment::Option{SCode.Comment}
    info::SourceInfo
  end

  @Record UNTYPED_COMPONENT begin
    classInst::InstNode
    dimensions::Vector{Dimension}
    binding::Binding
    condition::Binding
    attributes::ATTRIBUTES
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
    NOT_REPLACEABLE(),
    false,
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
    NOT_REPLACEABLE(),
    false,
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
    NOT_REPLACEABLE(),
    false,
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
    NOT_REPLACEABLE(),
    false,
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
    NOT_REPLACEABLE(),
    false,
  )

function isTypeAttribute(component::Component)::Bool
  local isAttribute::Bool
   isAttribute = begin
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
   isDeleted = begin
    local condition::Binding
    @match component begin
      TYPED_COMPONENT(condition = condition) => begin
        isBound(condition) &&
        isFalse(getTypedExp(condition))
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

   binding =
    lookupAttributeBinding("unit", getClass(classInstance(component)))
  if isUnbound(binding)
     unitString = defaultUnit
    return unitString
  end
   unit = getBindingExp(getExp(binding))
   unitString = begin
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
  #=  for parameters the default is fixed = true =#
  fixed = isParameter(component) || isStructuralParameter(component)
  binding = lookupAttributeBinding("fixed", getClass(classInstance(component)))
  #=  no fixed attribute present =#
  if isUnbound(binding)
    return fixed
  end
  fixed = fixed && isTrue(getBindingExp(getExp(binding)))
  return fixed
end

function getEvaluateAnnotation(component::Component)::Bool
  local evaluate::Bool
  local cmt::SCode.Comment
  evaluate = SCodeUtil.getEvaluateAnnotation(comment(component))
  return evaluate
end

function ann(component::Component)::Option{Modifier}
  local ann::Option{Modifier}
  ann = begin
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

   comment = begin
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

function dimensionCount(@nospecialize(component::Component))::Int
  local count::Int
   count = begin
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
   () = begin
    @match component begin
      UNTYPED_COMPONENT(__) => begin
         component.dimensions = listArray(dims)
        ()
      end
      TYPED_COMPONENT(__) => begin
         component.ty =
          liftArrayLeftList(arrayElementType(component.ty), dims)
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

   str = begin
    local def::SCode.Element
    @match component begin
      TYPED_COMPONENT(__) => begin
        toFlatString(component.attributes, component.ty) +
          toFlatString(component.ty) +
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

   str = begin
    local def::SCode.Element
    @match component begin
      COMPONENT_DEF(definition = def && SCode.COMPONENT(__)) => begin
        #TODO: SCodeDump.unparseElementStr(def)
        string(def)
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
     identical = true
  else
     identical = begin
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
   isEO = begin
    @match component begin
      UNTYPED_COMPONENT(__) => begin
        isExternalObject(getClass(component.classInst))
      end
      TYPED_COMPONENT(__) => begin
          isExternalObject(component.ty)
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
   () = begin
    local attr::Attributes
    @match component begin
      UNTYPED_COMPONENT(attributes = attr) => begin
         attr.connectorType = cty
         component.attributes = attr
        ()
      end
      TYPED_COMPONENT(attributes = attr) => begin
         attr.connectorType = cty
         component.attributes = attr
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
   cty = begin
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

   isOuter = io == InnerOuter.OUTER || io == InnerOuter.INNER_OUTER
  return isOuter
end

function isInner(component::Component)::Bool
  local isInner::Bool

  local io = innerOuter(component)

   isInner = io == InnerOuter.INNER || io == InnerOuter.INNER_OUTER
  return isInner
end

function innerOuter(component::Component)
  local io
   io = begin
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

   isFinal = begin
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
   isRedeclare = begin
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

   () = begin
    local attr::Attributes
    @match component begin
      UNTYPED_COMPONENT(attributes = attr) => begin
        @assign attr.variability = variability
        component.attributes = attr
        ()
      end

      TYPED_COMPONENT(attributes = attr) => begin
        @assign attr.variability = variability
        component.attributes = attr
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
  local v
  v = begin
    @match component begin
      TYPED_COMPONENT(attributes = ATTRIBUTES(variability = v)) =>
        begin
          v
        end

      UNTYPED_COMPONENT(attributes = ATTRIBUTES(variability = v)) => begin
        v
      end

      ITERATOR_COMPONENT(__) => begin
        component.variability
      end

      ENUM_LITERAL_COMPONENT(__) => begin
        Variability.CONSTANT
      end

      _ => begin
        Variability.CONTINUOUS
      end
    end
  end
  return v
end

function parallelism(component::Component)::Parallelism
  local parallelism::Parallelism

   parallelism = begin
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

   () = begin
    @match component begin
      UNTYPED_COMPONENT(attributes = attr) => begin
         attr.direction = Direction.INPUT
         component.attributes = attr
        ()
      end

      TYPED_COMPONENT(attributes = attr) => begin
         attr.direction = Direction.INPUT
         component.attributes = attr
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
  direction = begin
    @match component begin
      TYPED_COMPONENT(attributes = ATTRIBUTES(direction = direction)) =>
        begin
          direction
        end
      UNTYPED_COMPONENT(attributes = ATTRIBUTES(direction = direction)) =>
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

   b = isBound(getCondition(component))
  return b
end

function getCondition(component::Component)::Binding
  local cond::Binding

   cond = begin
    @match component begin
      UNTYPED_COMPONENT(__) => begin
        component.condition
      end

      TYPED_COMPONENT(__) => begin
        component.condition
      end

      _ => begin
        EMPTY_BINDING()
      end
    end
  end
  return cond
end

function hasBinding(component::Component, parent::InstNode = EMPTY_NODE())::Bool
  local b::Bool

  local cls::Class
  local children::Vector{InstNode}

  if isBound(getBinding(component))
     b = true
    return b
  end
  #=  Simple case, component has normal binding equation.
  =#
  #=  Complex case, component might be a record instance where each field has
  =#
  #=  its own binding equation.
  =#
   cls = getClass(classInstance(component))
  if !isRecord(restriction(cls))
     b = false
    return b
  end
  #=  Not record. =#
  #=  Check if any child of this component is missing a binding. =#
   children = getComponents(classTree(cls))
  for c in children
    if isComponent(c) && !hasBinding(component(c))
       b = false
      return b
    end
  end
   b = true
  return b
end

function setBinding(binding::Binding, component::Component)::Component

   () = begin
    @match component begin
      UNTYPED_COMPONENT(__) => begin
         component.binding = binding
        ()
      end

      TYPED_COMPONENT(__) => begin
         component.binding = binding
        ()
      end

      TYPE_ATTRIBUTE(__) => begin
         component.modifier = P_Modifier.setBinding(binding, component.modifier)
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

   binding = getBinding(component)
  if isUnbound(binding)
     cls_node = classInstance(component)
    if isRecord(cls_node)
      try
         record_exp = makeRecordExp(cls_node)
         binding = FLAT_BINDING(
          record_exp,
          variability(record_exp),
        )
      catch
      end
    end
  end
  return binding
end

function getBinding(component::Component)::Binding
  local b::Binding
   b = begin
    @match component begin
      UNTYPED_COMPONENT(__) => begin
        component.binding
      end
      TYPED_COMPONENT(__) => begin
        component.binding
      end
      TYPE_ATTRIBUTE(__) => begin
        binding(component.modifier)
      end
      _ => begin
        EMPTY_BINDING()
      end
    end
  end
  return b
end

function setAttributes(attr, component::Component)::Component

   () = begin
    @match component begin
      UNTYPED_COMPONENT(__) => begin
         component.attributes = attr
        ()
      end

      TYPED_COMPONENT(__) => begin
         component.attributes = attr
        ()
      end
    end
  end
  return component
end

function getAttributes(component::Component)
  local attr

   attr = begin
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

   () = begin
    local ty::M_Type
    @match component begin
      TYPED_COMPONENT(ty = TYPE_ARRAY(elementType = ty)) => begin
         component.ty = ty
        ()
      end

      ITERATOR(ty = TYPE_ARRAY(elementType = ty)) => begin
         component.ty = ty
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

   isTyped = begin    @match component begin
      TYPED_COMPONENT(__) => begin
        true
      end

      ITERATOR_COMPONENT(ty = TYPE_UNKNOWN(__)) => begin
        false
      end

      ITERATOR_COMPONENT(__) => begin
        true
      end

      TYPE_ATTRIBUTE_COMPONENT(__) => begin
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

   component = begin
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
         component.ty = ty
        component
      end

      ITERATOR(__) => begin
         component.ty = ty
        component
      end
    end
  end
  return component
end

function getType(component::Component)::M_Type
  local ty::M_Type

   ty = begin
    @match component begin
      TYPED_COMPONENT(__) => begin
        component.ty
      end

      UNTYPED_COMPONENT(__) => begin
        getType(component.classInst)
      end

      ITERATOR_COMPONENT(__) => begin
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

"""
  TODO Clean up the dbg prints here.
Note that we need to clone a new object by using @assign here...
"""
function mergeModifier(modifier::Modifier, component::Component)
  local mod
  modifiedComponent = @match component begin
    COMPONENT_DEF(__) => begin
      #strMod1 = toString(modifier, true)
      #strMod2 = toString(component.modifier, true)
      #@debug "c/mergeModifier($strMod1, $strMod2)"
      @assign component.modifier = merge(modifier, component.modifier)
      #strMod3 = toString(component.modifier, true)
      #@debug "c/mergeModifier($strMod1, $strMod2) -> $strMod3"
      component
    end
    TYPE_ATTRIBUTE(__) => begin
      #strMod1 = toString(modifier, true)
      #strMod2 = toString(component.modifier, true)
      ##@debug "t/mergeModifier($strMod1, $strMod2)"
      mod = merge(modifier, component.modifier)
      #strMod3 = toString(mod, true)
      ##@debug "t/mergeModifier($strMod1, $strMod2) -> $strMod3"
      TYPE_ATTRIBUTE(component.ty, mod)
    end
  end
  return modifiedComponent
end

function setModifier(modifier::Modifier, component::Component)::Component
  () = begin
    @match component begin
      COMPONENT_DEF(__) => begin
         component.modifier = modifier
        ()
      end

      TYPE_ATTRIBUTE(__) => begin
         component.modifier = modifier
        ()
      end
    end
  end
  return component
end

function getModifier(component::Component)::Modifier
  local modifier::Modifier
  modifier = begin
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
  () = begin
    @match component begin
      UNTYPED_COMPONENT(__) => begin
         component.classInst = classInst
        ()
      end

      TYPED_COMPONENT(__) => begin
         component.classInst = classInst
        ()
      end
    end
  end
  return component
end

function classInstance(component::Component)::InstNode
  local classInst::InstNode
  classInst = begin
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
function Component_info(component::Component)::SourceInfo
  local info::SourceInfo

   info = begin
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
        Component_info(component.component)
      end
    end
  end
  #=  Fail for enumeration literals, InstNode.info handles that case instead.
  =#
  return info
end

function isDefinition(component::Component)::Bool
  local isDefinition::Bool
  isDefinition = begin
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
  local def::SCode.Element
  def = @match component begin
    COMPONENT_DEF(def, mod) => component.definition
    TYPED_COMPONENT(__) => definition(component.classInst)
    _ => throw("Unsuported component: $(typeof(component)) in definition(component::Component)")
  end
  return def
end

function newEnum(enumType::M_Type, literalName::String, literalIndex::Int)::Component
  local component::Component
  component =
    ENUM_LITERAL_COMPONENT(ENUM_LITERAL_EXPRESSION(enumType,
                                                    literalName, literalIndex))
  return component
end

function new(definition::SCode.Element)::Component
  local component::Component
  component = COMPONENT_DEF(definition, MODIFIER_NOMOD())
  return component
end

using MetaModelica
using ExportAll

@UniontypeDecl Attributes

function toFlatString(attr::Attributes, ty::M_Type; isTopLevel = true)::String
  local str::String = ""
  if attr.isFinal
    str = str * "final "
  end
  str = str * unparseVariability(attr.variability, ty)
  if isTopLevel
    str = str * unparseDirection(attr.direction)
  end
  return str
end

function toString(attr::Attributes, ty::M_Type)::String
  local str::String
  str =
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
      unparseInnerOuter(attr.innerOuter) +
      unparseReplaceable(attr.isReplaceable) +
      unparseParallelism(attr.parallelism) +
      unparse(attr.connectorType) +
      unparseVariability(attr.variability, ty) +
      unparseDirection(attr.direction)
  return str
end

function toDAE(ina::Attributes, vis)::DAE.Attributes
  local outa::DAE.Attributes
  outa = DAE.ATTR(
    toDAE(ina.connectorType),
    parallelismToSCode(ina.parallelism),
    variabilityToSCode(ina.variability),
    directionToAbsyn(ina.direction),
    innerOuterToAbsyn(ina.innerOuter),
    visibilityToSCode(vis),
  )
  return outa
end
