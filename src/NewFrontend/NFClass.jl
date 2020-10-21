using MetaModelica.Dangerous
@Uniontype Prefixes begin
  @Record PREFIXES begin
    encapsulatedPrefix::SCode.Encapsulated
    partialPrefix::SCode.Partial
    finalPrefix::SCode.Final
    innerOuter::Absyn.InnerOuter
    replaceablePrefix::SCode.Replaceable
  end
end

@Uniontype Class begin
  @Record DAE_TYPE begin
    ty::DAE.Type
  end

  @Record TYPED_DERIVED begin
    ty::NFType
    baseClass::InstNode
    restriction::Restriction
  end

  @Record INSTANCED_BUILTIN begin
    ty::NFType
    elements::ClassTree
    restriction::Restriction
  end

  @Record INSTANCED_CLASS begin
    ty::NFType
    elements::ClassTree
    sections::Sections
    restriction::Restriction
  end

  @Record EXPANDED_DERIVED begin
    baseClass::InstNode
    modifier::Modifier
    dims::Array{Dimension}
    prefixes::Prefixes
    attributes::Attributes
    restriction::Restriction
  end

  @Record EXPANDED_CLASS begin
    elements::ClassTree
    modifier::Modifier
    prefixes::Prefixes
    restriction::Restriction
  end

  @Record PARTIAL_BUILTIN begin
    ty::NFType
    elements::ClassTree
    modifier::Modifier
    prefixes::Prefixes
    restriction::Restriction
  end
  @Record PARTIAL_CLASS begin
    elements::ClassTree
    modifier::Modifier
    prefixes
  end
  @Record NOT_INSTANTIATED begin
  end
end


const DEFAULT_PREFIXES =
  PREFIXES(
    SCode.NOT_ENCAPSULATED(),
    SCode.NOT_PARTIAL(),
    SCode.NOT_FINAL(),
    Absyn.NOT_INNER_OUTER(),
    SCode.NOT_REPLACEABLE()
  )


@UniontypeDecl Class

function toFlatString(cls::Class, clsNode::InstNode)::String
  local str::String
  local s
  @assign s = IOStream.create(getInstanceName(), IOStream.IOStreamType.LIST())
  @assign s = toFlatStream(cls, clsNode, s)
  @assign str = IOStream.string(s)
  IOStream.delete(s)
  return str
end

function toFlatStream(
  cls::Class,
  clsNode::InstNode,
  s,
)
  local name::String
  @assign name = AbsynUtil.pathString(scopePath(clsNode))
  @assign s = begin
    @match cls begin
      INSTANCED_CLASS(__) => begin
        @assign s = IOStream.append(s, P_Restriction.Restriction.toString(cls.restriction))
        @assign s = IOStream.append(s, " '")
        @assign s = IOStream.append(s, name)
        @assign s = IOStream.append(s, "'\\n")
        for comp in getComponents(cls.elements)
          @assign s = IOStream.append(s, "  ")
          @assign s = IOStream.append(s, toFlatString(comp))
          @assign s = IOStream.append(s, ";\\n")
        end
        @assign s = IOStream.append(s, "end '")
        @assign s = IOStream.append(s, name)
        @assign s = IOStream.append(s, "'")
        s
      end

      INSTANCED_BUILTIN(__) => begin
        @assign s = IOStream.append(s, "INSTANCED_BUILTIN(")
        @assign s = IOStream.append(s, name)
        @assign s = IOStream.append(s, ")")
        s
      end

      TYPED_DERIVED(__) => begin
        @assign s = IOStream.append(s, P_Restriction.Restriction.toString(cls.restriction))
        @assign s = IOStream.append(s, " '")
        @assign s = IOStream.append(s, name)
        @assign s = IOStream.append(s, "' = '")
        @assign s =
          IOStream.append(s, AbsynUtil.pathString(scopePath(cls.baseClass)))
        @assign s = IOStream.append(s, "'")
        s
      end

      _ => begin
        IOStream.append(s, "UNKNOWN_CLASS(" + name + ")")
      end
    end
  end
  return s
end

function makeRecordExp(clsNode::InstNode)::Expression
  local exp::Expression
  local cls::Class
  local ty::M_Type
  local ty_node::InstNode
  local fields::Array{InstNode}
  local args::List{Expression}
  @assign cls = getClass(clsNode)
  @match (@match TYPE_COMPLEX(complexTy = COMPLEX_RECORD(ty_node)) = ty) =
    getType(cls, clsNode)
  @assign fields = getComponents(classTree(cls))
  @assign args = list(
    getExp(P_Component.getImplicitBinding(component(f)))
    for f in fields
  )
  @assign exp = makeRecord(scopePath(ty_node), ty, args)
  return exp
end

function hasOperator(name::String, cls::Class)::Bool
  local hasOperator::Bool
  local op_node::InstNode
  local op_cls::Class
  if isOperatorRecord(restriction(cls))
    try
      @assign op_node = lookupElement(name, cls)
      @assign hasOperator = SCodeUtil.isOperator(definition(op_node))
    catch
      @assign hasOperator = false
    end
  else
    @assign hasOperator = false
  end
  return hasOperator
end

function getDerivedComments(cls::Class, cmts::List{<:SCode.Comment})::List{SCode.Comment}

  @assign cmts = begin
    @match cls begin
      EXPANDED_DERIVED(__) => begin
        getComments(cls.baseClass, cmts)
      end

      TYPED_DERIVED(__) => begin
        getComments(cls.baseClass, cmts)
      end

      _ => begin
        cmts
      end
    end
  end
  return cmts
end

function lastBaseClass(node::InstNode)::InstNode

  local cls::Class = getClass(node)

  @assign node = begin
    @match cls begin
      EXPANDED_DERIVED(__) => begin
        lastBaseClass(cls.baseClass)
      end

      TYPED_DERIVED(__) => begin
        lastBaseClass(cls.baseClass)
      end

      _ => begin
        node
      end
    end
  end
  return node
end

function isEncapsulated(cls::Class)::Bool
  local isEncapsulated::Bool

  @assign isEncapsulated = begin
    @match cls begin
      PARTIAL_CLASS(__) => begin
        SCodeUtil.encapsulatedBool(cls.prefixes.encapsulatedPrefix)
      end

      EXPANDED_CLASS(__) => begin
        SCodeUtil.encapsulatedBool(cls.prefixes.encapsulatedPrefix)
      end

      EXPANDED_DERIVED(__) => begin
        SCodeUtil.encapsulatedBool(cls.prefixes.encapsulatedPrefix)
      end

      _ => begin
        false
      end
    end
  end
  return isEncapsulated
end

function setPrefixes(prefs::Prefixes, cls::Class)::Class

  @assign () = begin
    @match cls begin
      EXPANDED_CLASS(__) => begin
        @assign cls.prefixes = prefs
        ()
      end

      EXPANDED_DERIVED(__) => begin
        @assign cls.prefixes = prefs
        ()
      end
    end
  end
  return cls
end

function getPrefixes(cls::Class)::Prefixes
  local prefs::Prefixes

  @assign prefs = begin
    @match cls begin
      PARTIAL_CLASS(__) => begin
        cls.prefixes
      end

      PARTIAL_BUILTIN(__) => begin
        cls.prefixes
      end

      EXPANDED_CLASS(__) => begin
        cls.prefixes
      end

      EXPANDED_DERIVED(__) => begin
        cls.prefixes
      end
    end
  end
  return prefs
end

function isOverdetermined(cls::Class)::Bool
  local isOverdetermined::Bool

  try
    lookupElement("equalityConstraint", cls)
    System.setHasOverconstrainedConnectors(true)
    @assign isOverdetermined = true
  catch
    @assign isOverdetermined = false
  end
  #=  set the external flag that signals the presence of expandable connectors in the model
  =#
  return isOverdetermined
end

function isExternalFunction(cls::Class)::Bool
  local isExtFunc::Bool

  @assign isExtFunc = begin
    local lang::String
    @match cls begin
      EXPANDED_DERIVED(__) => begin
        isExternalFunction(getClass(cls.baseClass))
      end

      INSTANCED_CLASS(sections = SECTIONS_EXTERNAL(language = lang)) => begin
        lang != "builtin"
      end

      TYPED_DERIVED(__) => begin
        isExternalFunction(getClass(cls.baseClass))
      end

      _ => begin
        false
      end
    end
  end
  return isExtFunc
end

function isFunction(cls::Class)::Bool
  local f::Bool = isFunction(restriction(cls))
  return f
end

function isExternalObject(cls::Class)::Bool
  return isExternalObject(restriction(cls))
end

function isExpandableConnectorClass(cls::Class)::Bool
  local isConnector::Bool =
    P_Restriction.Restriction.isExpandableConnector(restriction(cls))
  return isConnector
end

function isNonexpandableConnectorClass(cls::Class)::Bool
  local isConnector::Bool =
    P_Restriction.Restriction.isNonexpandableConnector(restriction(cls))
  return isConnector
end

function isConnectorClass(cls::Class)::Bool
  local isConnector::Bool = P_Restriction.Restriction.isConnector(restriction(cls))
  return isConnector
end

function setRestriction(res::Restriction, cls::Class)::Class

  @assign () = begin
    @match cls begin
      EXPANDED_CLASS(__) => begin
        #=  PARTIAL_BUILTIN is only used for predefined builtin types and not needed here.
        =#
        @assign cls.restriction = res
        ()
      end

      EXPANDED_DERIVED(__) => begin
        @assign cls.restriction = res
        ()
      end

      INSTANCED_CLASS(__) => begin
        @assign cls.restriction = res
        ()
      end

      INSTANCED_BUILTIN(__) => begin
        @assign cls.restriction = res
        ()
      end

      TYPED_DERIVED(__) => begin
        @assign cls.restriction = res
        ()
      end
    end
  end
  return cls
end

function restriction(cls::Class)::Restriction
  local res::Restriction

  @assign res = begin
    @match cls begin
      PARTIAL_BUILTIN(__) => begin
        cls.restriction
      end

      EXPANDED_CLASS(__) => begin
        cls.restriction
      end

      EXPANDED_DERIVED(__) => begin
        cls.restriction
      end

      INSTANCED_CLASS(__) => begin
        cls.restriction
      end

      INSTANCED_BUILTIN(__) => begin
        cls.restriction
      end

      TYPED_DERIVED(__) => begin
        cls.restriction
      end

      _ => begin
        RESTRICTION_UNKNOWN()
      end
    end
  end
  return res
end

function setType(ty::M_Type, cls::Class)::Class

  @assign () = begin
    @match cls begin
      PARTIAL_BUILTIN(__) => begin
        @assign cls.ty = ty
        ()
      end

      EXPANDED_DERIVED(__) => begin
        classApply(cls.baseClass, setType, ty)
        ()
      end

      INSTANCED_CLASS(__) => begin
        @assign cls.ty = ty
        ()
      end

      INSTANCED_BUILTIN(__) => begin
        @assign cls.ty = ty
        ()
      end

      TYPED_DERIVED(__) => begin
        @assign cls.ty = ty
        ()
      end

      _ => begin
        ()
      end
    end
  end
  return cls
end

function getType(cls::Class, clsNode::InstNode)::NFType
  local ty::NFType
  @assign ty = begin
    @match cls begin
      PARTIAL_BUILTIN(__) => begin
        cls.ty
      end

      EXPANDED_DERIVED(__) => begin
        getType(getClass(cls.baseClass), cls.baseClass)
      end

      INSTANCED_CLASS(__) => begin
        cls.ty
      end

      INSTANCED_BUILTIN(__) => begin
        begin
          @match cls.ty begin
            TYPE_POLYMORPHIC("") => begin
              TYPE_POLYMORPHIC(name(clsNode))
            end

            _ => begin
              cls.ty
            end
          end
        end
      end

      TYPED_DERIVED(__) => begin
        cls.ty
      end

      _ => begin
        TYPE_UNKNOWN()
      end
    end
  end
  return ty
end

function getTypeAttributes(cls::Class)::List{Modifier}
  local attributes::List{Modifier} = nil

  local comps::Array{InstNode}
  local mod::Modifier

  try
    @assign comps = getComponents(classTree(cls))
    for c in comps
      @assign mod = getModifier(component(c))
      if !isEmpty(mod)
        @assign attributes = _cons(mod, attributes)
      end
    end
  catch e
    @error "some $e stuff"
  end
  return attributes
end

function getAttributes(cls::Class)::Attributes
  local attr::Attributes

  @assign attr = begin
    @match cls begin
      EXPANDED_DERIVED(__) => begin
        cls.attributes
      end

      _ => begin
        NFComponent.DEFAULT_ATTR
      end
    end
  end
  return attr
end

function getDimensions(cls::Class)::List{Dimension}
  local dims::List{Dimension}

  @assign dims = begin
    @match cls begin
      INSTANCED_CLASS(__) => begin
        arrayDims(cls.ty)
      end

      INSTANCED_BUILTIN(__) => begin
        arrayDims(cls.ty)
      end

      TYPED_DERIVED(__) => begin
        arrayDims(cls.ty)
      end

      _ => begin
        nil
      end
    end
  end
  return dims
end

function hasDimensions(cls::Class)::Bool
  local hasDims::Bool

  @assign hasDims = begin
    @match cls begin
      EXPANDED_DERIVED(__) => begin
        arrayLength(cls.dims) > 0 || hasDimensions(getClass(cls.baseClass))
      end

      TYPED_DERIVED(__) => begin
        isArray(cls.ty)
      end

      _ => begin
        false
      end
    end
  end
  return hasDims
end

function isIdentical(cls1::Class, cls2::Class)::Bool
  local identical::Bool = false

  if referenceEq(cls1, cls2)
    @assign identical = true
  else
    @assign identical = begin
      @match (cls1, cls2) begin
        (EXPANDED_CLASS(__), EXPANDED_CLASS(__)) => begin
          P_Prefixes.isEqual(cls1.prefixes, cls2.prefixes) &&
          isIdentical(cls1.elements, cls2.elements)
        end

        (INSTANCED_BUILTIN(__), INSTANCED_BUILTIN(__)) => begin
          if !Type.isEqual(cls1.ty, cls2.ty)
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

function mergeModifier(modifier::Modifier, cls::Class)::Class

  @assign () = begin
    @match cls begin
      PARTIAL_CLASS(__) => begin
        @assign cls.modifier = P_Modifier.merge(modifier, cls.modifier)
        ()
      end

      EXPANDED_CLASS(__) => begin
        @assign cls.modifier = P_Modifier.merge(modifier, cls.modifier)
        ()
      end

      EXPANDED_DERIVED(__) => begin
        @assign cls.modifier = P_Modifier.merge(modifier, cls.modifier)
        ()
      end

      PARTIAL_BUILTIN(__) => begin
        @assign cls.modifier = P_Modifier.merge(modifier, cls.modifier)
        ()
      end

      _ => begin
        Error.assertion(false, getInstanceName() + " got non-modifiable class", sourceInfo())
        fail()
      end
    end
  end
  return cls
end

function setModifier(modifier::Modifier, cls::Class)::Class

  @assign () = begin
    @match cls begin
      PARTIAL_CLASS(__) => begin
        @assign cls.modifier = modifier
        ()
      end

      EXPANDED_CLASS(__) => begin
        @assign cls.modifier = modifier
        ()
      end

      EXPANDED_DERIVED(__) => begin
        @assign cls.modifier = modifier
        ()
      end

      PARTIAL_BUILTIN(__) => begin
        @assign cls.modifier = modifier
        ()
      end

      _ => begin
        Error.assertion(false, getInstanceName() + " got non-modifiable class", sourceInfo())
        fail()
      end
    end
  end
  return cls
end

function getModifier(cls::Class)::Modifier
  local modifier::Modifier

  @assign modifier = begin
    @match cls begin
      PARTIAL_CLASS(__) => begin
        cls.modifier
      end

      EXPANDED_CLASS(__) => begin
        cls.modifier
      end

      EXPANDED_DERIVED(__) => begin
        cls.modifier
      end

      PARTIAL_BUILTIN(__) => begin
        cls.modifier
      end

      _ => begin
        MODIFIER_NOMOD()
      end
    end
  end
  return modifier
end

function classTreeApply(cls::Class, func::FuncType)::Class

  @assign () = begin
    @match cls begin
      PARTIAL_CLASS(__) => begin
        @assign cls.elements = func(cls.elements)
        ()
      end

      EXPANDED_CLASS(__) => begin
        @assign cls.elements = func(cls.elements)
        ()
      end

      PARTIAL_BUILTIN(__) => begin
        @assign cls.elements = func(cls.elements)
        ()
      end

      INSTANCED_CLASS(__) => begin
        @assign cls.elements = func(cls.elements)
        ()
      end

      INSTANCED_BUILTIN(__) => begin
        @assign cls.elements = func(cls.elements)
        ()
      end

      _ => begin
        ()
      end
    end
  end
  return cls
end

function setClassTree(tree::ClassTree, cls::Class)::Class

  @assign () = begin
    @match cls begin
      PARTIAL_CLASS(__) => begin
        @assign cls.elements = tree
        ()
      end

      EXPANDED_CLASS(__) => begin
        @assign cls.elements = tree
        ()
      end

      PARTIAL_BUILTIN(__) => begin
        @assign cls.elements = tree
        ()
      end

      INSTANCED_CLASS(__) => begin
        @assign cls.elements = tree
        ()
      end

      INSTANCED_BUILTIN(__) => begin
        @assign cls.elements = tree
        ()
      end
    end
  end
  return cls
end

function classTree(cls::Class)::ClassTree
  local tree::ClassTree

  @assign tree = begin
    @match cls begin
      PARTIAL_CLASS(__) => begin
        cls.elements
      end

      PARTIAL_BUILTIN(__) => begin
        cls.elements
      end

      EXPANDED_CLASS(__) => begin
        cls.elements
      end

      EXPANDED_DERIVED(__) => begin
        classTree(getClass(cls.baseClass))
      end

      INSTANCED_CLASS(__) => begin
        cls.elements
      end

      INSTANCED_BUILTIN(__) => begin
        cls.elements
      end

      TYPED_DERIVED(__) => begin
        classTree(getClass(cls.baseClass))
      end
      _ => begin
        EMPTY_TREE()
      end
    end
  end
  @debug "Our resulting tree in class tree"
  str = LookupTree.printTreeStr(lookupTree(tree))
  @debug str
  return tree
end

function isBuiltin(cls::Class)::Bool
  local isBuiltin::Bool

  @assign isBuiltin = begin
    @match cls begin
      PARTIAL_BUILTIN(__) => begin
        true
      end

      INSTANCED_BUILTIN(__) => begin
        true
      end

      EXPANDED_DERIVED(__) => begin
        isBuiltin(getClass(cls.baseClass))
      end

      TYPED_DERIVED(__) => begin
        isBuiltin(getClass(cls.baseClass))
      end

      _ => begin
        false
      end
    end
  end
  return isBuiltin
end

function lookupAttributeValue(name::String, cls::Class)::Option{Expression}
  local value::Option{Expression} = typedExp(lookupAttributeBinding(name, cls))
  return value
end

function lookupAttributeBinding(name::String, cls::Class)::Binding
  local binding::Binding

  local attr_node::InstNode

  try
    @assign attr_node = lookupElement(name, classTree(cls))
    @assign binding = P_Component.getBinding(component(attr_node))
  catch
    @assign binding = EMPTY_BINDING
  end
  return binding
end

function nthComponent(index::Integer, cls::Class)::InstNode
  local component::InstNode

  @assign component = nthComponent(index, classTree(cls))
  return component
end

function lookupComponentIndex(name::String, cls::Class)::Integer
  local index::Integer

  @assign index = lookupComponentIndex(name, classTree(cls))
  return index
end

function lookupElement(name::String, cls::Class)::Tuple{InstNode, Bool}
  local isImport::Bool
  local node::InstNode

  @assign (node, isImport) = lookupElement(name, classTree(cls))
  return (node, isImport)
end

function setSections(sections::Sections, cls::Class)::Class
  @assign cls = begin
    @match cls begin
      INSTANCED_CLASS(__) => begin
        INSTANCED_CLASS(cls.ty, cls.elements, sections, cls.restriction)
      end
    end
  end
  return cls
end

function getSections(cls::Class)::Sections
  local sections::Sections

  @assign sections = begin
    @match cls begin
      INSTANCED_CLASS(__) => begin
        cls.sections
      end

      TYPED_DERIVED(__) => begin
        getSections(getClass(cls.baseClass))
      end

      _ => begin
        SECTIONS_EMPTY()
      end
    end
  end
  return sections
end

function initExpandedClass(cls::Class)::Class
  @assign cls = begin
    @match cls begin
      PARTIAL_CLASS(__) => begin
        EXPANDED_CLASS(
          cls.elements,
          cls.modifier,
          cls.prefixes,
          RESTRICTION_UNKNOWN(),
        )
      end
    end
  end
  return cls
end

function makeRecordConstructor(fields::List{<:InstNode}, out::InstNode)::Class
  local cls::Class

  local tree::ClassTree

  @assign tree = fromRecordConstructor(fields, out)
  @assign cls = INSTANCED_CLASS(
    TYPE_UNKNOWN(),
    tree,
    SECTIONS_EMPTY(),
    P_Restriction.Restriction.RECORD_CONSTRUCTOR(),
  )
  return cls
end

function fromEnumeration(
  literals::List{<:SCode.Enum},
  enumType::M_Type,
  prefixes::Prefixes,
  enumClass::InstNode,
)::Class
  local cls::Class

  local tree::ClassTree

  @assign tree = fromEnumeration(literals, enumType, enumClass)
  @assign cls = PARTIAL_BUILTIN(
    enumType,
    tree,
    MODIFIER_NOMOD(),
    prefixes,
    P_Restriction.Restriction.ENUMERATION(),
  )
  return cls
end

function fromSCode(
  elements::List{<:SCode.Element},
  isClassExtends::Bool,
  scope::InstNode,
  prefixes::Prefixes,
)::Class
  local cls::Class
  local tree::ClassTree
  @assign tree = fromSCode(elements, isClassExtends, scope)
  @assign cls = PARTIAL_CLASS(tree, MODIFIER_NOMOD(), prefixes)
  return cls
end

function isEqual(prefs1::Prefixes, prefs2::Prefixes)::Bool
  local isEqual::Bool = valueEq(prefs1, prefs2)
  return isEqual
end

@exportAll
