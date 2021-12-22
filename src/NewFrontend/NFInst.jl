import Absyn
import SCode
import DAE
""" 
  Instantiates a class given by its fully qualified path, with the result being a DAE.
"""
function instClassInProgram(classPath::Absyn.Path, program::SCode.Program)::Tuple{DAE.DAE_LIST, DAE.FunctionTree}
  local daeFuncs::DAE.FunctionTree
  local dae::DAE.DAE_LIST
  local top::InstNode
  local cls::InstNode
  local inst_cls::InstNode
  local name::String
  local flat_model::FlatModel
  local funcs::FunctionTree

  #=  gather here all the flags to disable expansion
  =#
  #=  and scalarization if -d=-nfScalarize is on
  =#

  #= Set scalazrize by default. =#
  FlagsUtil.set(Flags.NF_SCALARIZE, true)
  #= Should be changed using something better later =#
  if ! Flags.isSet(Flags.NF_SCALARIZE)
    FlagsUtil.set(Flags.NF_EXPAND_OPERATIONS, false)
    FlagsUtil.set(Flags.NF_EXPAND_FUNC_ARGS, false)
  end

  #=  make sure we don't expand anything
  =#
  System.setUsesCardinality(false)
  System.setHasOverconstrainedConnectors(false)
  System.setHasStreamConnectors(false)
  #=  Create a root node from the given top-level classes.
  =#
  top = makeTopNode(program)
  name = AbsynUtil.pathString(classPath)
  #=  Look up the class to instantiate and mark it as the root class.
  =#
  cls = lookupClassName(classPath, top, AbsynUtil.dummyInfo, false)
  cls = setNodeType(ROOT_CLASS(EMPTY_NODE()), cls)
  #=  Initialize the storage for automatically generated inner elements. =#
  top = setInnerOuterCache(top, C_TOP_SCOPE(NodeTree.new(), cls))
  #=  Instantiate the class. =#
  @debug "FIRST INST CALL!"
  inst_cls = instantiateN1(cls, EMPTY_NODE())
  @debug "AFTER INST CALL"
  insertGeneratedInners(inst_cls, top)
  #execStat("NFInst.instantiate(" + name + ")")
  @debug "INSTANTIATION STEP 1 DONE!"
  #=  Instantiate expressions (i.e. anything that can contains crefs, like
  =#
  #=  bindings, dimensions, etc). This is done as a separate step after
  =#
  #=  instantiation to make sure that lookup is able to find the correct nodes.
  =#
  instExpressions(inst_cls)
  #                   execStat("NFInst.instExpressions(" + name + ")")
  @debug "Inst expressions done"
  #=  Mark structural parameters.
  =#
  updateImplicitVariability(inst_cls, false #== Flags.isSet(Flags.EVAL_PARAM) ==#)
  #execStat("NFInst.updateImplicitVariability")
  #=  Type the class.
  =#
  @debug "TYPECLASS(inst_cls, name)"
  typeClass(inst_cls, name)
  @debug "AFTER type class"
  #=  Flatten the model and evaluate constants in it.
  =#
  @debug "START FLATTENING!"
  @assign flat_model = flatten(inst_cls, name)
  @debug "CONSTANT EVALUATION"
  @assign flat_model = evaluate(flat_model)
  @debug "FLATTENING DONE: flat_model"
  #= Do unit checking =#
  #TODO  @assign flat_model = UnitCheck.checkUnits(flat_model)
  #=  Apply simplifications to the model.=#
  @assign flat_model = simplifyFlatModel(flat_model)
  #=  Collect a tree of all functions that are still used in the flat model.=#
  @debug "COLLECT FUNCTIONS"
  @assign funcs = collectFunctions(flat_model, name)
  @debug "COLLECTED FUNCTIONS!"
  #=  Collect package constants that couldn't be substituted with their values =#
  #=  (e.g. because they where used with non-constant subscripts), and add them to the model. =#
  @debug "COLLECT CONSTANTS"
  @assign flat_model = collectConstants(flat_model, funcs)
  @debug "COLLECTED CONSTANTS"
  # if Flags.getConfigBool(Flags.FLAT_MODELICA)
  @debug "PRINTING FLAT MODELICA"
  #printFlatString(flat_model, FunctionTreeImpl.listValues(funcs))
  # end
  #= Scalarize array components in the flat model.=#
  @debug "Not skipping NF_SCALARIZE"
  #                  if Flags.isSet(Flags.NF_SCALARIZE)
  # @assign flat_model = scalarize(flat_model, name)
  #                  else
  # @assign flat_model.variables = ListUtil.filterOnFalse(flat_model.variables, isEmptyArray)
  #                   end
  #=  Remove empty arrays from variables =#
  @debug "VERIFYING MODEL: "
  verify(flat_model)
  #                   if Flags.isSet(Flags.NF_DUMP_FLAT)
  # print("FlatModel:\\n" + toString(flat_model) + "\\n")
  #                  end
  #=  Convert the flat model to a DAE.=#
  @debug "CONVERT TO THE DAE REPRESENTATION"
  (dae, daeFuncs) = convert(flat_model, funcs, name, InstNode_info(inst_cls))
  return (dae, daeFuncs)
end

"""
Similar to instClassInProgram but returns the flat model instead of the DAE.
Author:johti17
"""
function instClassInProgramFM(classPath::Absyn.Path, program::SCode.Program)::Tuple
  local daeFuncs::DAE.FunctionTree
  local dae::DAE.DAE_LIST
  local top::InstNode
  local cls::InstNode
  local inst_cls::InstNode
  local name::String
  local flat_model::FlatModel
  local funcs::FunctionTree

  #=  gather here all the flags to disable expansion
  =#
  #=  and scalarization if -d=-nfScalarize is on
  =#
  # if ! Flags.isSet(Flags.NF_SCALARIZE)
  #   FlagsUtil.set(Flags.NF_EXPAND_OPERATIONS, false)
  #   FlagsUtil.set(Flags.NF_EXPAND_FUNC_ARGS, false)
  # end

  #=  make sure we don't expand anything
  =#
  System.setUsesCardinality(false)
  System.setHasOverconstrainedConnectors(false)
  System.setHasStreamConnectors(false)
  #=  Create a root node from the given top-level classes.
  =#
  top = makeTopNode(program)
  name = AbsynUtil.pathString(classPath)
  #=  Look up the class to instantiate and mark it as the root class.
  =#
  cls = lookupClassName(classPath, top, AbsynUtil.dummyInfo, false)
  cls = setNodeType(ROOT_CLASS(EMPTY_NODE()), cls)
  #=  Initialize the storage for automatically generated inner elements. =#
  top = setInnerOuterCache(top, C_TOP_SCOPE(NodeTree.new(), cls))
  #=  Instantiate the class. =#
  @debug "FIRST INST CALL!"
  inst_cls = instantiateN1(cls, EMPTY_NODE())
  @debug "AFTER INST CALL"
  insertGeneratedInners(inst_cls, top)
  #execStat("NFInst.instantiate(" + name + ")")
  @debug "INSTANTIATION STEP 1 DONE!"
  #=  Instantiate expressions (i.e. anything that can contains crefs, like
  =#
  #=  bindings, dimensions, etc). This is done as a separate step after
  =#
  #=  instantiation to make sure that lookup is able to find the correct nodes.
  =#
  instExpressions(inst_cls)
  #                   execStat("NFInst.instExpressions(" + name + ")")
  @debug "Inst expressions done"
  #=  Mark structural parameters.
  =#
  updateImplicitVariability(inst_cls, false #== Flags.isSet(Flags.EVAL_PARAM) ==#)
  #execStat("NFInst.updateImplicitVariability")
  #=  Type the class.
  =#
  @debug "TYPECLASS(inst_cls, name)"
  typeClass(inst_cls, name)
  @debug "AFTER type class"
  #=  Flatten the model and evaluate constants in it.
  =#
  @debug "START FLATTENING!"
  @assign flat_model = flatten(inst_cls, name)
  @debug "CONSTANT EVALUATION"
  @assign flat_model = evaluate(flat_model)
  @debug "FLATTENING DONE: flat_model"
  #= Do unit checking =#
  #TODO  @assign flat_model = UnitCheck.checkUnits(flat_model)
  #=  Apply simplifications to the model.=#
  @assign flat_model = simplifyFlatModel(flat_model)
  #=  Collect a tree of all functions that are still used in the flat model.=#
  @debug "COLLECT FUNCTIONS"
  @assign funcs = collectFunctions(flat_model, name)
  @debug "COLLECTED FUNCTIONS!"
  #=  Collect package constants that couldn't be substituted with their values =#
  #=  (e.g. because they where used with non-constant subscripts), and add them to the model. =#
  @debug "COLLECT CONSTANTS"
  @assign flat_model = collectConstants(flat_model, funcs)
  @debug "COLLECTED CONSTANTS"
  # if Flags.getConfigBool(Flags.FLAT_MODELICA)
  @debug "PRINTING FLAT MODELICA"
  #printFlatString(flat_model, FunctionTreeImpl.listValues(funcs))
  # end
  #= Scalarize array components in the flat model.=#
  @debug "Not skipping NF_SCALARIZE"
  #                  if Flags.isSet(Flags.NF_SCALARIZE)
  # @assign flat_model = scalarize(flat_model, name)
  #                  else
  # @assign flat_model.variables = ListUtil.filterOnFalse(flat_model.variables, isEmptyArray)
  #                   end
  #=  Remove empty arrays from variables =#
  @debug "VERIFYING MODEL: "
  verify(flat_model)
  #                   if Flags.isSet(Flags.NF_DUMP_FLAT)
  # print("FlatModel:\\n" + toString(flat_model) + "\\n")
  #                  end
  return (flat_model, funcs)
end

function instantiateN1(node::InstNode, parentNode::InstNode)::InstNode
  @debug "Instantiating!!!! in Inst"
  @assign node = expand(node)
  @debug "After expansion in inst. Instantiating in class-tree "
  @assign (node, _) = instClass(node, MODIFIER_NOMOD(), DEFAULT_ATTR, true, 0, parentNode)
  return node
end

function expand(node::InstNode) ::InstNode
  @assign node = partialInstClass(node)
  @assign node = expandClass(node)
  node
end

""" #= Creates an instance node from the given list of top-level classes. =#"""
function makeTopNode(topClasses::List{<:SCode.Element}) ::InstNode
  local topNode::InstNode
  local cls_elem::SCode.Element
  local cls::Class
  local elems::ClassTree

  #=  Create a fake SCode.Element for the top scope, so we don't have to make the
  =#
  #=  definition in InstNode an Option only because of this node.
  =#
  @assign cls_elem = SCode.CLASS("<top>", SCode.defaultPrefixes, SCode.NOT_ENCAPSULATED(), SCode.NOT_PARTIAL(), SCode.R_PACKAGE(), SCode.PARTS(topClasses, nil, nil, nil, nil, nil, nil, NONE()), SCode.COMMENT(NONE(), NONE()), AbsynUtil.dummyInfo)
  #=  Make an InstNode for the top scope, to use as the parent of the top level elements.
  =#
  @assign topNode = newClass(cls_elem, EMPTY_NODE(), TOP_SCOPE())
  #=  Create a new class from the elements, and update the inst node with it.
  =#
  @assign cls = fromSCode(topClasses, false, topNode, DEFAULT_PREFIXES)
  #=  The class needs to be expanded to allow lookup in it. The top scope will
  =#
  #=  only contain classes, so we can do this instead of the whole expandClass.
  =#
  @assign cls = initExpandedClass(cls)
  #=  Set the correct for classes with builtin annotation. This
  =#
  #=  could also be done when creating InstNodes, but only top-level classes
  =#
  #=  should have this annotation anyway.
  =#
  @assign elems = classTree(cls)
  mapClasses(elems, markBuiltinTypeNodes)
  @assign cls = setClassTree(elems, cls)
  @assign topNode = updateClass(cls, topNode)
  topNode
end

function markBuiltinTypeNodes(node::InstNode) ::InstNode
  if SCodeUtil.hasBooleanNamedAnnotationInClass(definition(node), "__OpenModelica_builtin")
    @assign node = setNodeType(BUILTIN_CLASS(), node)
  end
  node
end

function partialInstClass(node::InstNode) ::InstNode
  local c::Class
  () = begin
    @match getClass(node) begin
      NOT_INSTANTIATED(__)  => begin
        c = partialInstClass2(definition(node), node)
        node = updateClass(c, node)
        ()
      end
      _  => begin
        ()
      end
    end
  end
  node
end

function partialInstClass2(definition::SCode.Element, scope::InstNode) ::Class
  local cls::Class
  local cdef::SCode.ClassDef
  local ce_cdef::SCode.ClassDef
  local ty::M_Type
  local prefs::Prefixes
#  Error.assertion(SCodeUtil.elementIsClass(definition), getInstanceName() + " got non-class element", sourceInfo())
  @match SCode.CLASS(classDef = cdef) = definition
  @assign prefs = instClassPrefixes(definition)
  @assign cls = begin
    @match cdef begin
      SCode.PARTS(__)  => begin
        fromSCode(cdef.elementLst, false, scope, prefs)
      end
      SCode.CLASS_EXTENDS(composition = ce_cdef && SCode.PARTS(__))  => begin
        if ! SCodeUtil.isElementRedeclare(definition)
          Error.addSourceMessage(Error.CLASS_EXTENDS_MISSING_REDECLARE, list(SCodeUtil.elementName(definition)), SCodeUtil.elementInfo(definition))
        end
        fromSCode(ce_cdef.elementLst, true, scope, prefs)
      end
      SCode.ENUMERATION(__)  => begin
        #=  An enumeration definition, add the literals to a new scope.
        =#
        @assign ty = makeEnumerationType(cdef.enumLst, scope)
        fromEnumeration(cdef.enumLst, ty, prefs, scope)
      end
      _  => begin
        PARTIAL_CLASS(CLASS_TREE_EMPTY_TREE(), MODIFIER_NOMOD(), prefs)
      end
    end
  end
  cls
end

function instClassPrefixes(cls::SCode.Element) ::Prefixes
  local prefixes::Prefixes

  local prefs::SCode.Prefixes

  @assign prefixes = begin
    @match cls begin
      SCode.CLASS(encapsulatedPrefix = SCode.NOT_ENCAPSULATED(__), partialPrefix = SCode.NOT_PARTIAL(__), prefixes = SCode.PREFIXES(finalPrefix = SCode.NOT_FINAL(__), innerOuter = Absyn.NOT_INNER_OUTER(__), replaceablePrefix = SCode.NOT_REPLACEABLE(__)))  => begin
        DEFAULT_PREFIXES
      end

      SCode.CLASS(prefixes = prefs)  => begin
        PREFIXES(cls.encapsulatedPrefix, cls.partialPrefix, prefs.finalPrefix, prefs.innerOuter, prefs.replaceablePrefix)
      end
    end
  end
  prefixes
end

function makeEnumerationType(literals::List{<:SCode.Enum}, scope::InstNode) ::M_Type
  local ty::M_Type

  local lits::List{String}
  local path::Absyn.Path

  @assign path = scopePath(scope)
  @assign lits = list(e.literal for e in literals)
  @assign ty = TYPE_ENUMERATION(path, lits)
  ty
end

function expandClass(node::InstNode) ::InstNode


  @assign node = begin
    @match getClass(node) begin
      PARTIAL_CLASS(__)  => begin
        expandClass2(node)
      end

      _  => begin
        node
      end
    end
  end
  node
end

function expandClass2(node::InstNode) ::InstNode


  local def::SCode.Element = definition(node)
  local cdef::SCode.ClassDef
  local info::SourceInfo

  @match SCode.CLASS(classDef = cdef, info = info) = def
  @assign node = begin
    @match cdef begin
      SCode.PARTS(__)  => begin
        expandClassParts(def, node, info)
      end

      SCode.CLASS_EXTENDS(__)  => begin
        expandClassParts(def, node, info)
      end

      SCode.DERIVED(__)  => begin
        expandClassDerived(def, cdef, node, info)
      end

      _  => begin
        #=  A short class definition, e.g. class A = B.
        =#
        Error.assertion(false, getInstanceName() + " got unknown class:\\n" + SCodeDump.unparseElementStr(def), sourceInfo())
        fail()
      end
    end
  end
  node
end

function expandClassParts(def::SCode.Element, node::InstNode, info::SourceInfo) ::InstNode


  local cls::Class
  local cls_tree::ClassTree
  local mod::Modifier
  local builtin_ext::InstNode
  local prefs::Prefixes
  local res::Restriction

  @assign cls = getClass(node)
  #=  Change the class to an empty expanded class, to avoid instantiation loops.
  =#
  @assign cls = initExpandedClass(cls)
  @assign node = updateClass(cls, node)
  @match EXPANDED_CLASS(elements = cls_tree, modifier = mod, prefixes = prefs) = cls
  @assign builtin_ext = mapFoldExtends(cls_tree, expandExtends, EMPTY_NODE())
  if name(builtin_ext) == "ExternalObject"
    @assign node = expandExternalObject(cls_tree, node)
  else
    if ! isEmpty(builtin_ext)
      checkBuiltinTypeExtends(builtin_ext, cls_tree, node)
    end
    @assign cls_tree = expand(cls_tree)
    @assign res = fromSCode(SCodeUtil.getClassRestriction(def))
    @assign cls = EXPANDED_CLASS(cls_tree, mod, prefs, res)
    @assign node = updateClass(cls, node)
  end
  node
end

function expandExtends(ext::InstNode, builtinExt::InstNode = EMPTY_NODE()) ::Tuple{InstNode, InstNode}
  local def::SCode.Element
  local base_path::Absyn.Path
  local base_nodes::List{InstNode}
  local scope::InstNode
  local base_node::InstNode
  local vis::SCode.Visibility
  local smod::SCode.Mod
  local ann::Option{SCode.Annotation}
  local info::SourceInfo
  if isEmpty(ext)
    return (ext, builtinExt)
  end
  def = definition(ext)
  () = begin
    @match def begin
      SCode.EXTENDS(base_path, vis, smod, ann, info)  => begin
        #=  Look up the base class and expand it.
        =#
        scope = parent(ext)
        #        @match (@match _cons(base_node, _) = base_nodes) = lookupBaseClassName(base_path, scope, info)
        base_nodes = lookupBaseClassName(base_path, scope, info) #Modification by me:)
        base_node = listHead(base_nodes)
        checkExtendsLoop(base_node, base_path, info)
        checkReplaceableBaseClass(base_nodes, base_path, info)
        base_node = expand(base_node)
        ext = setNodeType(BASE_CLASS(scope, def), base_node)
        #=  If the extended class is a builtin class, like Real or any type derived
        =#
        #=  from Real, then return it so we can handle it properly in expandClass.
        =#
        #=  We don't care if builtinExt is already SOME, since that's not legal and
        =#
        #=  will be caught by expandBuiltinExtends.
        =#
        if isBuiltin(base_node) || isBuiltin(getClass(base_node))
          @assign builtinExt = ext
        end
        ()
      end

      _  => begin
        ()
      end
    end
  end
  (ext, builtinExt)
end

""" #= Gives an error if a base node is in the process of being expanded itself,
             since that means we have an extends loop in the model. =#"""
function checkExtendsLoop(node::InstNode, path::Absyn.Path, info::SourceInfo)
  @assign () = begin
    @match getClass(node) begin
      EXPANDED_CLASS(elements = CLASS_TREE_PARTIAL_TREE(__))  => begin
        #=  expand begins by changing the class to an EXPANDED_CLASS, but keeps the
        =#
        #=  class tree. So finding a PARTIAL_TREE here means the class is in the
        =#
        #=  process of being expanded.
        =#
        Error.addSourceMessage(Error.EXTENDS_LOOP, list(AbsynUtil.pathString(path)), info)
        fail()
      end

      _  => begin
        ()
      end
    end
  end
end

""" #= Checks that all parts of a name used as a base class are transitively
             non-replaceable. =#"""
function checkReplaceableBaseClass(baseClasses::List{<:InstNode}, basePath::Absyn.Path, info::SourceInfo)
  local i::Int = 0
  local pos::Int
  local name::String
  local rest::List{InstNode}

  for base in baseClasses
    @assign i = i + 1
    if SCodeUtil.isElementReplaceable(definition(base))
      if listLength(baseClasses) > 1
        rest = baseClasses
        name = ""
        for j in 1:i - 1
          name = "." + name(listHead(rest)) + name
          rest = listRest(rest)
        end
        name = "<" + name(listHead(rest)) + ">" + name
        rest = listRest(rest)
        for n in rest
          name = name(n) + "." + name
        end
      else
        name = AbsynUtil.pathString(basePath)
      end
      #TODO      Error.addMultiSourceMessage(Error.REPLACEABLE_BASE_CLASS, list(name(base), name), list(InstNode_info(base), info))
      @error "Error: Class  $name in base replaceable..." 
      fail()
    end
  end
  #=  The path might contain several classes with the same name, so mark the
  =#
  #=  class in the path string to make it clear which one we mean.
  =#
end

function expandExternalObject(clsTree::ClassTree, node::InstNode) ::InstNode


  local eo_ty::ComplexType
  local c::Class

  #=  Construct the ComplexType for the external object.
  =#
  @assign eo_ty = makeExternalObjectType(clsTree, node)
  #=  Construct the Class for the external object. We use an empty class
  =#
  #=  tree here, since the constructor and destructor is embedded in the
  =#
  #=  ComplexType instead. Using an empty class tree makes sure it's not
  =#
  #=  possible to call the constructor or destructor explicitly.
  =#
  @assign c = PARTIAL_BUILTIN(TYPE_COMPLEX(node, eo_ty), NFClassTree.EMPTY_FLAT, MODIFIER_NOMOD(), NFClass.DEFAULT_PREFIXES, RESTRICTION_EXTERNAL_OBJECT())
  @assign node = updateClass(c, node)
  node
end

function checkBuiltinTypeExtends(builtinExtends::InstNode, tree::ClassTree, node::InstNode)
  #=  A class extending from a builtin type may not have other components or baseclasses.
  =#
  if componentCount(tree) > 0 || extendsCount(tree) > 1
    #Error.addSourceMessage(Error.BUILTIN_EXTENDS_INVALID_ELEMENTS, list(name(builtinExtends)), InstNode_info(node))
    @error "Extends invalid elements!"
    fail()
  end
  #=  ***TODO***: Find the invalid element and use its info to make the error
  =#
  #=              message more accurate.
  =#
end

""" #= Constructs a ComplexType for an external object, and also checks that the
             external object declaration is valid. =#"""
               function makeExternalObjectType(tree::ClassTree, node::InstNode) ::ComplexType
                 local ty::ComplexType

                 local base_path::Absyn.Path
                 local constructor::InstNode = EMPTY_NODE()
                 local destructor::InstNode = EMPTY_NODE()

                 @assign ty = begin
                   @match tree begin
                     PARTIAL_TREE(__)  => begin
                       #=  An external object may not contain components.
                       =#
                       for comp in tree.components
                         if isComponent(comp)
                           Error.addSourceMessage(Error.EXTERNAL_OBJECT_INVALID_ELEMENT, list(name(node), name(comp)), Component_info(comp))
                           fail()
                         end
                       end
                       #=  An external object may not contain extends other than the ExternalObject one.
                       =#
                       if arrayLength(tree.exts) > 1
                         for ext in tree.exts
                           if name(ext) != "ExternalObject"
                             @match CLASS_NODE(nodeType = BASE_CLASS(definition = SCode.EXTENDS(baseClassPath = base_path))) = ext
                             Error.addSourceMessage(Error.EXTERNAL_OBJECT_INVALID_ELEMENT, list(name(node), "extends " + AbsynUtil.pathString(base_path)), InstNode_info(ext))
                             fail()
                           end
                         end
                       end
                       #=  An external object must have exactly two functions called constructor and
                       =#
                       #=  destructor.
                       =#
                       for cls in tree.classes
                         @assign () = begin
                           @match name(cls) begin
                             "constructor" where (SCodeUtil.isFunction(definition(cls)))  => begin
                               @assign constructor = cls
                               ()
                             end

                             "destructor" where (SCodeUtil.isFunction(definition(cls)))  => begin
                               @assign destructor = cls
                               ()
                             end

                             _  => begin
                               #=  Found some other element => error.
                               =#
                               Error.addSourceMessage(Error.EXTERNAL_OBJECT_INVALID_ELEMENT, list(name(node), name(cls)), InstNode_info(cls))
                               fail()
                             end
                           end
                         end
                       end
                       if isEmpty(constructor)
                         Error.addSourceMessage(Error.EXTERNAL_OBJECT_MISSING_STRUCTOR, list(name(node), "constructor"), InstNode_info(node))
                         fail()
                       end
                       #=  The constructor is missing.
                       =#
                       if isEmpty(destructor)
                         Error.addSourceMessage(Error.EXTERNAL_OBJECT_MISSING_STRUCTOR, list(name(node), "destructor"), InstNode_info(node))
                         fail()
                       end
                       #=  The destructor is missing.
                       =#
                       COMPLEX_EXTERNAL_OBJECT(constructor, destructor)
                     end
                   end
                 end
                 ty
               end

function expandClassDerived(element::SCode.Element, definition::SCode.ClassDef, node::InstNode, info::SourceInfo) ::InstNode


  local ty::Absyn.TypeSpec
  local ext_node::InstNode
  local cls::Class
  local prefs::Prefixes
  local sattrs::SCode.Attributes
  local attrs::Attributes
  local dims::List{Dimension}
  local mod::Modifier
  local res::Restriction

  @match SCode.DERIVED(typeSpec = ty, attributes = sattrs) = definition
  #=  Look up the class that's being derived from and expand it.
  =#
  @match _cons(ext_node, _) = lookupBaseClassName(AbsynUtil.typeSpecPath(ty), parent(node), info)
  #=  Check that the class isn't extending itself, i.e. class A = A.
  =#
  if referenceEq(ext_node, node)
    Error.addSourceMessage(Error.RECURSIVE_SHORT_CLASS_DEFINITION, list(name(node), Dump.unparseTypeSpec(ty)), info)
    fail()
  end
  @assign ext_node = expand(ext_node)
  @assign ext_node = clone(ext_node)
  #=  Fetch the needed information from the class definition and construct a EXPANDED_DERIVED.
  =#
  @assign cls = getClass(node)
  @assign prefs = getPrefixes(cls)
  @assign attrs = instDerivedAttributes(sattrs)
  @assign dims = list(DIMENSION_RAW_DIM(d) for d in AbsynUtil.typeSpecDimensions(ty))
  @assign mod = getModifier(cls)
  @assign res = fromSCode(SCodeUtil.getClassRestriction(element))
  @assign cls = EXPANDED_DERIVED(ext_node, mod, listArray(dims), prefs, attrs, res)
  @assign node = updateClass(cls, node)
  node
end

function instDerivedAttributes(scodeAttr::SCode.Attributes) ::Attributes
  local attributes::Attributes
  local cty::ConnectorType.TYPE
  local var::VariabilityType
  local dir::DirectionType
  attributes = begin
    @match scodeAttr begin
      SCode.ATTR(connectorType = SCode.POTENTIAL(__), variability = SCode.VAR(__), direction = Absyn.BIDIR(__))  => begin
        DEFAULT_ATTR
      end
      _  => begin
        cty = fromSCode(scodeAttr.connectorType)
        var = variabilityFromSCode(scodeAttr.variability)
        dir = directionFromSCode(scodeAttr.direction)
        ATTRIBUTES(cty, Parallelism.NON_PARALLEL, var, dir, InnerOuter.NOT_INNER_OUTER, false, false, NOT_REPLACEABLE())
      end
    end
  end
  attributes
end

function instClass(node::InstNode, modifier::Modifier, attributes::Attributes = DEFAULT_ATTR, useBinding::Bool = false, instLevel::Int = 0, parent = EMPTY_NODE) ::Tuple{InstNode, Attributes}
  local cls::Class
  local outer_mod::Modifier
  @debug "INST CLASS CALLED. CALLING GETCLASS ON NODE."
  @assign cls = getClass(node)
  @debug "OUR CLASS AFTER CALLING GETCLASS"
  @assign outer_mod = getModifier(cls)
  #=  Give an error for modifiers such as (A = B), i.e. attempting to replace a =#
  #=  class without using redeclare. =#
  if hasBinding(outer_mod)
    Error.addSourceMessage(Error.MISSING_REDECLARE_IN_CLASS_MOD, list(name(node)), Binding_getInfo(binding(outer_mod)))
    fail()
  end
  @debug "CALLING INSTCLASSDEF"
  @assign (attributes, node) = instClassDef(cls, modifier, attributes, useBinding, node, parent, instLevel)
  (node, attributes)
end

function instClassDef(cls::Class, outerMod::Modifier, attributes::Attributes, useBinding::Bool, node::InstNode, parentArg::InstNode, instLevel::Int) ::Tuple{Attributes, InstNode}
  local par::InstNode
  local base_node::InstNode
  local inst_cls::Class
  local cls_tree::ClassTree
  local mod::Modifier
  local outer_mod::Modifier
  local res::Restriction
  local ty::M_Type
  local attrs::Attributes
  @debug "CALLING INSTCLASSDEF FOR CLASS"
  @assign () = begin
    @match cls begin
      EXPANDED_CLASS(restriction = res)  => begin
        #=  Skip instantiating the class tree if the class is a base class,
        =#
        #=  it has (hopefully) already been instantiated in that case.
        =#
        if isBaseClass(node)
          @assign par = parentArg
        else
          @assign (node, par) = instantiate(node, parentArg)
        end
        updateComponentType(parentArg, node)
        @assign attributes = updateClassConnectorType(res, attributes)
        @debug "The questions is the semantics here double check!!"
        inst_cls = getClass(node)
        cls_tree = inst_cls.elements
        #=  Fetch modification on the class definition (for class extends).
        =#
        @assign mod = fromElement(definition(node), nil, par)
        #=  Merge with any outer modifications.
        =#
        @assign outer_mod = merge(outerMod, cls.modifier)
        @assign mod = merge(outer_mod, mod)
        #=  Apply the modifiers of extends nodes.
        =#
        mapExtends(cls_tree, (extendsNodeX) -> modifyExtends(extendsNodeX, par))
        #=  Apply the modifiers of this scope.
        =#

        strMod = toString(mod, true)
        @debug "Merged mod EXPANDED_CLASS: $strMod"

        applyModifier(mod, cls_tree, name(node))
        #=  Apply element redeclares.
        =#
        mapRedeclareChains(cls_tree, (instLevel) -> redeclareElements(instLevel = instLevel))
        #=  Redeclare classes with redeclare modifiers. Redeclared components could
        =#
        #=  also be handled here, but since each component is only instantiated once
        =#
        #=  it's more efficient to apply the redeclare when instantiating them instead.
        =#
        redeclareClasses(cls_tree)
        #=  Instantiate the extends nodes. =#
        @debug "Double check this line. Might be ome translation error here."
        mapExtends(cls_tree, (nodeX) -> instExtends(nodeX,
                                                    attributes,
                                                    useBinding,
                                                    ExtendsVisibility.PUBLIC,
                                                    instLevel + 1))
        #=  Instantiate local components. =#
        instCp = (node) -> instComponent(
          node,
          attributes,
          MODIFIER_NOMOD(),
          useBinding,
          instLevel + 1,
          NONE()
        )
        applyLocalComponents(cls_tree, instCp)
        #=  Remove duplicate elements. =#
        cls_tree = replaceDuplicates(cls_tree)
        checkDuplicates(cls_tree)
        updateClass(setClassTree(cls_tree, inst_cls), node)
        ()
      end

      EXPANDED_DERIVED(__)  => begin
        @assign (node, par) = instantiate(node, parentArg)
        @assign node = setNodeType(DERIVED_CLASS(nodeType(node)), node)
        @match EXPANDED_DERIVED(baseClass = base_node) = getClass(node)
        #=  Merge outer modifiers and attributes.
        =#
        @assign mod = fromElement(definition(node), list(node), rootParent(node))
        @assign outer_mod = merge(outerMod, addParent(node, cls.modifier))
        @assign mod = merge(outer_mod, mod)

        strMod = toString(mod, true)
        @debug "Merged mod EXPANDED_DERIVED: $strMod"

        @assign attrs = updateClassConnectorType(cls.restriction, cls.attributes)
        @assign attributes = mergeDerivedAttributes(attrs, attributes, parentArg)
        #=  Instantiate the base class and update the nodes.
        =#
        @assign (base_node, attributes) = instClass(base_node, mod, attributes, useBinding, instLevel, par)
        @assign cls.baseClass = base_node
        @assign cls.attributes = attributes
        @assign cls.dims = arrayCopy(cls.dims)
        #=  Update the parentArg's type with the new class instance.
        =#
        @assign node = updateClass(cls, node)
        updateComponentType(parentArg, node)
        ()
      end

      PARTIAL_BUILTIN(restriction = RESTRICTION_EXTERNAL_OBJECT(__))  => begin
        @assign inst_cls = INSTANCED_BUILTIN(cls.ty, cls.elements, cls.restriction)
        @assign node = replaceClass(inst_cls, node)
        updateComponentType(parentArg, node)
        instExternalObjectStructors(cls.ty, parentArg)
        ()
      end

      PARTIAL_BUILTIN(ty = ty, restriction = res)  => begin
        @assign (node, par) = instantiate(node, parentArg)
        updateComponentType(parentArg, node)
        @assign cls_tree = classTree(getClass(node))
        @assign mod = fromElement(definition(node), list(node), parent(node))
        @assign outer_mod = merge(outerMod, addParent(node, cls.modifier))
        @assign mod = merge(outer_mod, mod)

        strMod = toString(mod, true)
        @debug "Merged mod PARTIAL_BUILTIN: $strMod"

        applyModifier(mod, cls_tree, name(node))
        @assign inst_cls = INSTANCED_BUILTIN(ty, cls_tree, res)
        @assign node = updateClass(inst_cls, node)
        ()
      end

      INSTANCED_CLASS(__)  => begin
        #=  If a class has an instance of a encapsulating class, then the encapsulating
        =#
        #=  class will have been fully instantiated to allow lookup in it. This is a
        =#
        #=  rather uncommon case hopefully, so in that case just reinstantiate the class.
        =#
        @assign node = replaceClass(NOT_INSTANTIATED(), node)
        @assign node = setNodeType(NORMAL_CLASS(), node)
        @assign node = expand(node)
        @assign node = instClass(node, outerMod, attributes, useBinding, instLevel, parent)
        updateComponentType(parent, node)
        ()
      end

      _  => begin
        Error.assertion(false, getInstanceName() + " got unknown class.", sourceInfo())
        ()
      end
    end
  end
  (attributes, node)
end

""" #= Sets the class instance of a component node. =#"""
function updateComponentType(component::InstNode, cls::InstNode) ::InstNode
  if isComponent(component)
    @assign component = componentApply(component, setClassInstance, cls)
  end
  component
end

function updateClassConnectorType(res::Restriction, attrs::Attributes) ::Attributes
  if isExpandableConnector(res)
    @assign attrs.connectorType = setExpandable(attrs.connectorType)
  elseif isConnector(res)
    @assign attrs.connectorType = setConnector(attrs.connectorType)
  end
  attrs
end

""" #= Instantiates the constructor and destructor for an ExternalObject class. =#"""
function instExternalObjectStructors(ty::M_Type, parent::InstNode)
  local constructor::InstNode
  local destructor::InstNode
  local par::InstNode

  #=  The constructor and destructor have function parameters that are instances
  =#
  #=  of the external object class, and we instantiate the structors when we
  =#
  #=  instantiate such instances. To break that loop we check that we're not
  =#
  #=  inside the external object class before instantiating the structors.
  =#
  @assign par = parent(parent(parent))
  if ! (isClass(par) && isExternalObject(getClass(par)))
    @match TYPE_COMPLEX(complexTy = COMPLEX_EXTERNAL_OBJECT(constructor, destructor)) = ty
    instFunctionNode(constructor)
    instFunctionNode(destructor)
  end
end

"""
  This function instantiates a package given a package node. If the package has
  already been instantiated, then the cached instance from the node is
  returned. Otherwise the node is fully instantiated, the instance is added to
  the node's cache, and the instantiated node is returned. 
"""
function instPackage(node::InstNode) ::InstNode
  local cache::CachedData
  local inst::InstNode
  @assign cache = getPackageCache(node)
  @assign node = begin
    @match cache begin
      C_PACKAGE(__)  => begin
        cache.instance
      end
      C_NO_CACHE(__)  => begin
        #=  Cache the package node itself first, to avoid instantiation loops if
        =#
        #=  the package uses itself somehow.
        =#
        setPackageCache(node, C_PACKAGE(node))
        #=  Instantiate the node.=#
        inst = instantiateN1(node, EMPTY_NODE()) #=Wrong function call was generated here...=#
        #=  Cache the instantiated node and instantiate expressions in it too.
        =#
        setPackageCache(node, C_PACKAGE(inst))
        instExpressions(inst)
        inst
      end
      _  => begin
        Error.assertion(false, getInstanceName() + " got invalid instance cache", sourceInfo())
        fail()
      end
    end
  end
  node
end

function modifyExtends(extendsNode::InstNode, scope::InstNode)
  local elem::SCode.Element
  local ext_mod::Modifier
  local ext_node::InstNode
  local info::SourceInfo
  local cls::Class
  local cls_tree::ClassTree

  cls = getClass(extendsNode)
  cls_tree = classTree(cls)
  #=  Create a modifier from the extends.
  =#
  @match BASE_CLASS(definition = elem) = nodeType(extendsNode)
  ext_mod = fromElement(elem, nil, scope)
  ext_mod = merge(getModifier(extendsNode), ext_mod)
  if ! isBuiltin(cls)
    #= Added by johti17 to mimic function inheritance =#
    local func = function modifyExtends2(x; scope = extendsNode) 
      modifyExtends(x, scope)
    end
    mapExtends(cls_tree, (x) -> func(x, scope = extendsNode))
    @assign () = begin
      @match elem begin
        SCode.EXTENDS(__)  => begin
          #=  TODO: Lookup the base class and merge its modifier. =#
          @match _cons(ext_node, _) = lookupBaseClassName(elem.baseClassPath, scope, elem.info)
          #=  Finding a different element than before expanding extends
          =#
          #=  (probably an inherited element) is an error.
          =#
          if ! referenceEq(definition(extendsNode), definition(ext_node))
            # Error.addMultiSourceMessage(Error.FOUND_OTHER_BASECLASS, list(AbsynUtil.pathString(elem.baseClassPath)), list(InstNode_info(extendsNode), InstNode_info(ext_node)))
            fail("Found another base class")
          end
          ()
        end

        SCode.CLASS(__)  => begin
          ()
        end
      end
    end
  end
  applyModifier(ext_mod, cls_tree, name(extendsNode))
  extendsNode
end

ExtendsVisibility = #= Enumeration =# (() -> begin
                                       PUBLIC  = 1
                                       DERIVED_PROTECTED  = 2
                                       PROTECTED  = 3
                                       ()->(PUBLIC ;DERIVED_PROTECTED ;PROTECTED )
                                       end)()

const ExtendsVisibilityType = Int
function instExtends(node::InstNode, attributes::Attributes, useBinding::Bool,
                     visibility::ExtendsVisibilityType, instLevel::Int)::InstNode
  local cls::Class
  local inst_cls::Class
  local cls_tree::ClassTree
  local vis::ExtendsVisibilityType = visibility
  @assign cls = getClass(node)
  @assign () = begin
    @match cls begin
      EXPANDED_CLASS(elements = cls_tree && CLASS_TREE_INSTANTIATED_TREE(__))  => begin
        if vis == ExtendsVisibility.PUBLIC && isProtectedBaseClass(node) || vis == ExtendsVisibility.DERIVED_PROTECTED
          @assign vis = ExtendsVisibility.PROTECTED
        end
        #=  Protect components and classes if the extends is protected, except
        =#
        #=  if they've already been protected by an extends higher up.
        =#
        if vis == ExtendsVisibility.PROTECTED && visibility != ExtendsVisibility.PROTECTED
          for c in cls_tree.classes
            P_Pointer.update(c, protectClass(P_Pointer.access(c)))
          end
          for c in cls_tree.components
            P_Pointer.update(c, protectComponent(P_Pointer.access(c)))
          end
        end
        noMod = MODIFIER_NOMOD()
        x = (nodeX) ->
          instExtends(nodeX, attributes, useBinding, vis, instLevel)
        mapExtends(cls_tree, x)
        y = (nodeX) ->
          instComponent(nodeX, attributes, noMod , useBinding, instLevel, NONE())
        applyLocalComponents(cls_tree, y)
        ()
      end

      EXPANDED_DERIVED(__)  => begin
        if vis == ExtendsVisibility.PUBLIC && isProtectedBaseClass(node)
          @assign vis = ExtendsVisibility.DERIVED_PROTECTED
        end
        @assign cls.baseClass = instExtends(cls.baseClass, attributes, useBinding, vis, instLevel)
        @assign node = updateClass(cls, node)
        ()
      end

      PARTIAL_BUILTIN(__)  => begin
        @assign inst_cls = INSTANCED_BUILTIN(cls.ty, cls.elements, cls.restriction)
        @assign node = updateClass(inst_cls, node)
        ()
      end

      _  => begin
        ()
      end
    end
  end
  node
end

""" #= Applies a modifier in the given scope, by splitting the modifier and merging
             each part with the relevant element in the scope. =#"""
               function applyModifier(modifier::Modifier, cls::ClassTree, clsName::String) ::ClassTree
                 local mods::List{Modifier}
                 local node_ptrs::List{Pointer{InstNode}}
                 local node::InstNode
                 local comp::Component
                 #=  Split the modifier into a list of submodifiers.
                 =#
                 @assign mods = toList(modifier)
                 if listEmpty(mods)
                   return cls
                 end
                 @assign () = begin
                   @match cls begin
                     CLASS_TREE_FLAT_TREE(__)  => begin
                       for mod in mods
                         try
                           @assign (node, _) = lookupElement(name(mod), cls)
                         catch e
                           #Error.addSourceMessage(Error.MISSING_MODIFIED_ELEMENT, list(name(mod), clsName), Mofifier_info(mod))
                           @error "Missing modified element!. Error was $(e)"
                           fail()
                         end
                         componentApply(node, mergeModifier, mod)
                       end
                       ()
                     end

                     _  => begin
                       for mod in mods
                         try
                           @assign node_ptrs = lookupElementsPtr(name(mod), cls)
                         catch e
                           @error "Missing modified element! "
                           Error.addSourceMessage(Error.MISSING_MODIFIED_ELEMENT, list(name(mod), clsName), Modifier_info(mod))
                           fail()
                         end
                         for node_ptr in node_ptrs
                           @assign node = resolveOuter(P_Pointer.access(node_ptr))
                           if isComponent(node)
                             componentApply(node, mergeModifier, mod)
                           else
                             if isOnlyOuter(node)
                               Error.addSourceMessage(Error.OUTER_ELEMENT_MOD, list(toString(mod, printName = false), name(mod)), info(mod))
                               fail()
                             end
                             partialInstClass(node)
                             @assign node = replaceClass(mergeModifier(mod, getClass(node)), node)
                             @assign node = clearPackageCache(node)
                             P_Pointer.update(node_ptr, node)
                           end
                         end
                       end
                       ()
                     end
                   end
                 end
                 cls
               end

function redeclareClasses(tree::ClassTree) ::ClassTree
  local cls_node::InstNode
  local redecl_node::InstNode
  local cls::Class
  local mod::Modifier
  @assign () = begin
    @match tree begin
      CLASS_TREE_INSTANTIATED_TREE(__)  => begin
        for cls_ptr in tree.classes
          @assign cls_node = P_Pointer.access(cls_ptr)
          @assign cls = getClass(resolveOuter(cls_node))
          @assign mod = getModifier(cls)
          if isRedeclare(mod)
            @match MODIFIER_REDECLARE(element = redecl_node, mod = mod) = mod
            cls_node = redeclareClass(redecl_node, cls_node, mod)
            P_Pointer.update(cls_ptr, cls_node)
          end
        end
        ()
      end
      _  => begin
        ()
      end
    end
  end
  tree
end

function redeclareElements(chain::List, instLevel::Int)
  local node::InstNode
  local node_ptr::Pointer{InstNode}

  @assign node = P_Pointer.access(listHead(chain))
  @assign node_ptr = listHead(chain)
  if isClass(node)
    for cls_ptr in listRest(chain)
      @assign node_ptr = redeclareClassElement(cls_ptr, node_ptr)
    end
    @assign node = P_Pointer.access(node_ptr)
  else
    for comp_ptr in listRest(chain)
      @assign node_ptr = redeclareComponentElement(comp_ptr, node_ptr, instLevel)
    end
    @assign node = P_Pointer.access(node_ptr)
  end
  for cls_ptr in chain
    P_Pointer.update(cls_ptr, node)
  end
end

function redeclareClassElement(redeclareCls::Pointer, replaceableCls::Pointer)::Pointer
  local outCls::Pointer{InstNode}
  local rdcl_node::InstNode
  local repl_node::InstNode
  @assign rdcl_node = P_Pointer.access(redeclareCls)
  @assign repl_node = P_Pointer.access(replaceableCls)
  @assign rdcl_node = redeclareClass(rdcl_node, repl_node, MODIFIER_NOMOD())
  @assign outCls = P_Pointer.create(rdcl_node)
  outCls
end

function redeclareComponentElement(redeclareComp::Pointer{<:InstNode}, replaceableComp::Pointer{<:InstNode}, instLevel::Int) ::Pointer{InstNode}
  local outComp::Pointer{InstNode}
  local rdcl_node::InstNode
  local repl_node::InstNode
  @assign rdcl_node = P_Pointer.access(redeclareComp)
  @assign repl_node = P_Pointer.access(replaceableComp)
  instComponent(repl_node, DEFAULT_ATTR, MODIFIER_NOMOD(), true, instLevel)
  redeclareComponent(rdcl_node, repl_node, MODIFIER_NOMOD(), MODIFIER_NOMOD(), DEFAULT_ATTR, rdcl_node, instLevel)
  @assign outComp = P_Pointer.create(rdcl_node)
  outComp
end

function redeclareClass(redeclareNode::InstNode, originalNode::InstNode, outerMod::Modifier) ::InstNode
  local redeclaredNode::InstNode
  local orig_node::InstNode
  local orig_cls::Class
  local rdcl_cls::Class
  local new_cls::Class
  local prefs::Prefixes
  local node_ty::InstNodeType
  #=  Check that the redeclare element is actually a class.
  =#
  if ! isClass(redeclareNode)
    Error.addMultiSourceMessage(Error.INVALID_REDECLARE_AS, list(typeName(originalNode), name(originalNode), typeName(redeclareNode)), list(InstNode_info(redeclareNode), InstNode_info(originalNode)))
    fail()
  end
  partialInstClass(originalNode)
  @assign orig_cls = getClass(originalNode)
  partialInstClass(redeclareNode)
  @assign rdcl_cls = getClass(redeclareNode)
  @assign prefs = mergeRedeclaredClassPrefixes(getPrefixes(orig_cls), getPrefixes(rdcl_cls), redeclareNode)
  if SCodeUtil.isClassExtends(definition(redeclareNode))
    @assign orig_node = expand(originalNode)
    @assign orig_cls = getClass(orig_node)
    @assign new_cls = begin
      @match (orig_cls, rdcl_cls) begin
        (_, PARTIAL_CLASS(__)) where (isBuiltin(orig_cls))  => begin
          #=  Class extends of a builtin type. Not very useful, but technically allowed
          =#
          #=  if the redeclaring class is empty.
          =#
          if ! SCodeUtil.isEmptyClassDef(SCodeUtil.getClassDef(definition(redeclareNode)))
            Error.addSourceMessage(Error.BUILTIN_EXTENDS_INVALID_ELEMENTS, list(name(redeclareNode)), InstNode_info(redeclareNode))
            fail()
          end
          #=  Class extends of a builtin type is only allowed if the extending class is empty,
          =#
          #=  otherwise it violates the rules of extending a builtin type.
          =#
          setPrefixes(prefs, orig_cls)
        end

        (EXPANDED_CLASS(__), PARTIAL_CLASS(__))  => begin
          #=  Class extends of a long class declaration.
          =#
          @assign node_ty = BASE_CLASS(parent(orig_node), definition(orig_node))
          @assign orig_node = setNodeType(node_ty, orig_node)
          @assign rdcl_cls.elements = setClassExtends(orig_node, rdcl_cls.elements)
          @assign rdcl_cls.modifier = merge(outerMod, rdcl_cls.modifier)
          @assign rdcl_cls.prefixes = prefs
          rdcl_cls
        end

        (EXPANDED_DERIVED(__), PARTIAL_CLASS(__))  => begin
          #=  Class extends of a short class declaration.
          =#
          @assign rdcl_cls.prefixes = prefs
          rdcl_cls
        end

        _  => begin
          Error.assertion(false, getInstanceName() + " got unknown classes", sourceInfo())
          fail()
        end
      end
    end
  else
    @assign new_cls = begin
      @match (orig_cls, rdcl_cls) begin
        (PARTIAL_BUILTIN(__), _)  => begin
          redeclareEnum(rdcl_cls, orig_cls, prefs, outerMod, redeclareNode, originalNode)
        end

        (_, PARTIAL_CLASS(__))  => begin
          @assign rdcl_cls.prefixes = prefs
          @assign rdcl_cls.modifier = merge(outerMod, rdcl_cls.modifier)
          rdcl_cls
        end

        _  => begin
          Error.assertion(false, getInstanceName() + " got unknown classes", sourceInfo())
          fail()
        end
      end
    end
  end
  @assign redeclaredNode = replaceClass(new_cls, redeclareNode)
  @assign node_ty = REDECLARED_CLASS(parent(originalNode), nodeType(originalNode))
  @assign redeclaredNode = setNodeType(node_ty, redeclaredNode)
  redeclaredNode
end

function redeclareEnum(redeclareClass::Class, originalClass::Class, prefixes::Prefixes, outerMod::Modifier, redeclareNode::InstNode, originalNode::InstNode) ::Class
  local redeclaredClass::Class = redeclareClass

  @assign redeclaredClass = begin
    local lits1::List{String}
    local lits2::List{String}
    @match (redeclaredClass, originalClass) begin
      (PARTIAL_BUILTIN(ty = TYPE_ENUMERATION(literals = lits1)), PARTIAL_BUILTIN(ty = TYPE_ENUMERATION(literals = lits2)))  => begin
        if ! (listEmpty(lits2) || ListUtil.isEqualOnTrue(lits1, lits2, stringEq))
          Error.addMultiSourceMessage(Error.REDECLARE_ENUM_NON_SUBTYPE, list(name(originalNode)), list(InstNode_info(redeclareNode), InstNode_info(originalNode)))
          fail()
        end
        @assign redeclaredClass.prefixes = prefixes
        @assign redeclaredClass.modifier = merge(outerMod, redeclaredClass.modifier)
        redeclaredClass
      end

      _  => begin
        Error.addMultiSourceMessage(Error.REDECLARE_CLASS_NON_SUBTYPE, list(toString(restriction(originalClass)), name(originalNode)), list(InstNode_info(redeclareNode), InstNode_info(originalNode)))
        fail()
      end
    end
  end
  redeclaredClass
end

function instComponent(node::InstNode, attributes::Attributes , innerMod::Modifier, useBinding::Bool, instLevel::Int, originalAttr = NONE())
  local comp::Component
  local def::SCode.Element
  local comp_node::InstNode
  local rdcl_node::InstNode
  local outer_mod::Modifier
  local cc_mod::Modifier = innerMod
  local cc_smod::SCode.Mod
  local name::String
  local parentNode::InstNode

  @assign comp_node = resolveOuter(node)
  @assign comp = component(comp_node)
  @assign parentNode = parent(comp_node)
  #=  Skip already instantiated components.
  =#
  if ! isDefinition(comp)
    checkRecursiveDefinition(classInstance(comp), comp_node, limitReached = false)
    return
  end
  #=  An already instantiated component might be due to an instantiation loop, check it.
  =#
  @match COMPONENT_DEF(definition = def, modifier = outer_mod) = comp

  if isRedeclare(outer_mod)
    checkOuterComponentMod(outer_mod, def, comp_node)
    instComponentDef(def, MODIFIER_NOMOD(), MODIFIER_NOMOD(), DEFAULT_ATTR, useBinding, comp_node, parentNode, instLevel, originalAttr, isRedeclared = true)
    @match REDECLARE(element = rdcl_node, mod = outer_mod) = outer_mod
    @assign cc_smod = SCodeUtil.getConstrainingMod(def)
    if ! SCodeUtil.isEmptyMod(cc_smod)
      @assign name = name(node)
      @assign cc_mod = create(cc_smod, name, cope.COMPONENT(name), nil, parentNode)
    end
    @assign outer_mod = merge(getModifier(rdcl_node), outer_mod)
    setModifier(outer_mod, rdcl_node)
    redeclareComponent(rdcl_node, node, MODIFIER_NOMOD(), cc_mod, attributes, node, instLevel)
  else
    instComponentDef(def, outer_mod, cc_mod, attributes, useBinding, comp_node, parentNode, instLevel, originalAttr)
  end
end

function instComponentDef(component::SCode.Element, outerMod::Modifier, innerMod::Modifier, attributes::Attributes, useBinding::Bool,
                          node::InstNode, parentNode::InstNode, instLevel::Int, originalAttr::Option{<:Attributes} = NONE(), isRedeclared::Bool = false)
  local info::SourceInfo
  local decl_mod::Modifier
  local mod::Modifier
  local cc_mod::Modifier
  local dims::List
  local ty_dims::List
  local bindingVar::Binding
  local condition::Binding
  local attr::Attributes
  local ty_attr::Attributes
  local inst_comp::Component
  local ty_node::InstNode
  local ty::Class
  local in_function::Bool
  local parent_res::Restriction
  local res::Restriction
  @match component begin
    SCode.COMPONENT(info = info)  => begin
      decl_mod = fromElement(component, nil, parentNode)
      cc_mod = instConstrainingMod(component, parentNode)
      mod = merge(decl_mod, cc_mod)
      mod = merge(mod, innerMod)
      mod = merge(outerMod, mod)
      mod = addParent(node, mod)
      str = toString(mod, true)
      checkOuterComponentMod(mod, component, node)
      dims = list(DIMENSION_RAW_DIM(d) for d in component.attributes.arrayDims)
      bindingVar = if useBinding
        binding(mod)
      else
        EMPTY_BINDING
      end
      condition = fromAbsyn(component.condition, false, list(node), parentNode, info)
      #=  Instantiate the component's attributes, and merge them with the
      =#
      #=  attributes of the component's parent (e.g. constant SomeComplexClass c).
      =#
      parent_res = restriction(getClass(parentNode))
      attr = instComponentAttributes(component.attributes, component.prefixes)
      attr = checkDeclaredComponentAttributes(attr, parent_res, node)
      attr = mergeComponentAttributes(attributes, attr, node, parent_res)
      if isSome(originalAttr)
        attr = mergeRedeclaredComponentAttributes(Util.getOption(originalAttr), attr, node)
      end
      if ! attr.isFinal && isFinal(mod)
        attr.isFinal = true
      end
      #=  Create the untyped component and update the node with it. We need the
      =#
      #=  untyped component in instClass to make sure everything is scoped
      =#
      #=  correctly during lookup, but the class node the component should have
      =#
      #=  is created by instClass. To break the circle we leave the class node
      =#
      #=  empty here, and let instClass set it for us instead.
      =#
      inst_comp = UNTYPED_COMPONENT(EMPTY_NODE(), listArray(dims), bindingVar, condition, attr, SOME(component.comment), false, info)
      updateComponent!(inst_comp, node)
      #=  Instantiate the type of the component.
      =#
      (ty_node, ty_attr) = instTypeSpec(component.typeSpec, mod, attr, useBinding && ! isBound(bindingVar), parentNode, node, info, instLevel)
      ty = getClass(ty_node)
      #=  Update the component's variability based on its type (e.g. Integer is discrete).
      =#
      ty_attr = updateComponentVariability(ty_attr, ty, ty_node)
      #=  Update the component's connector type now that we have its type.
      =#
      res = restriction(getClass(ty_node))
      ty_attr = updateComponentConnectorType(ty_attr, res, isRedeclared, node)
      if ! referenceEq(attr, ty_attr)
        componentApply(node, setAttributes, ty_attr)
      end
    end
  end
end

function instConstrainingMod(element::SCode.Element, parent::InstNode) ::Modifier
  local ccMod::Modifier
  @assign ccMod = begin
    local smod::SCode.Mod
    @match element begin
      SCode.CLASS(prefixes = SCode.PREFIXES(replaceablePrefix = SCode.REPLACEABLE(cc = SOME(SCode.CONSTRAINCLASS(modifier = smod)))))  => begin
        create(smod, element.name, cope.CLASS(element.name), nil, parent)
      end
      SCode.COMPONENT(prefixes = SCode.PREFIXES(replaceablePrefix = SCode.REPLACEABLE(cc = SOME(SCode.CONSTRAINCLASS(modifier = smod)))))  => begin
        create(smod, element.name, cope.COMPONENT(element.name), nil, parent)
      end
      _  => begin
        MODIFIER_NOMOD()
      end
    end
  end
  ccMod
end

function updateComponentConnectorType(attributes::Attributes, restriction::Restriction, isRedeclared::Bool, component::InstNode) ::Attributes
  local cty = attributes.connectorType
  if isConnectorType(cty)
    if isConnector(restriction)
      if isExpandableConnector(restriction)
        @assign cty = setPresent(cty)
      else
        @assign cty = intBitAnd(cty, intBitNot(ConnectorType.EXPANDABLE))
      end
    else
      @assign cty = intBitAnd(cty, intBitNot(intBitOr(ConnectorType.CONNECTOR, ConnectorType.EXPANDABLE)))
    end
    if ! isFlowOrStream(cty)
      @assign cty = setPotential(cty)
    end
    if cty != attributes.connectorType
      @assign attributes.connectorType = cty
    end
  elseif isFlowOrStream(cty) && ! isRedeclared
    #Error.addStrictMessage(Error.CONNECTOR_PREFIX_OUTSIDE_CONNECTOR, list(toString(cty)), Copmonent_info(component)) TODO
    @warn "CONNECTOR_PREFIX_OUTSIDE_CONNECTOR: list(toString(cty))"
    @assign attributes.connectorType = unsetFlowStream(cty)
  end
  attributes
end

function redeclareComponent(redeclareNode::InstNode, originalNode::InstNode, outerMod::Modifier, constrainingMod::Modifier, outerAttr::Attributes, redeclaredNode::InstNode, instLevel::Int)
  local orig_comp::Component
  local rdcl_comp::Component
  local new_comp::Component
  local bindingVar::Binding
  local condition::Binding
  local attr::Attributes
  local dims::Array{Dimension}
  local cmt::Option{SCode.Comment}
  local rdcl_node::InstNode
  local rdcl_type::InstNodeType

  #=  Check that the redeclare element actually is a component.
  =#
  if ! isComponent(redeclareNode)
    Error.addMultiSourceMessage(Error.INVALID_REDECLARE_AS, list(typeName(originalNode), name(originalNode), typeName(redeclareNode)), list(InstNode_info(redeclareNode), InstNode_info(originalNode)))
    fail()
  end
  @assign orig_comp = component(originalNode)
  @assign rdcl_type = REDECLARED_COMP(parent(originalNode))
  @assign rdcl_node = setNodeType(rdcl_type, redeclareNode)
  @assign rdcl_node = copyInstancePtr(originalNode, rdcl_node)
  @assign rdcl_node = updateComponent!(component(redeclareNode), rdcl_node)
  instComponent(rdcl_node, outerAttr, constrainingMod, true, instLevel, SOME(getAttributes(orig_comp)))
  @assign rdcl_comp = component(rdcl_node)
  @assign new_comp = begin
    @match (orig_comp, rdcl_comp) begin
      (UNTYPED_COMPONENT(__), UNTYPED_COMPONENT(__))  => begin
        #=  Take the binding from the outer modifier, the redeclare, or the
        =#
        #=  original component, in that order of priority.
        =#
        @assign bindingVar = binding(outerMod)
        if isUnbound(bindingVar)
          @assign bindingVar = if isBound(rdcl_comp.binding)
            rdcl_comp.binding
          else
            orig_comp.binding
          end
        end
        #=  A redeclare is not allowed to have a condition expression.
        =#
        if isBound(rdcl_comp.condition)
          Error.addSourceMessage(Error.REDECLARE_CONDITION, list(name(redeclareNode)), InstNode_info(redeclareNode))
          fail()
        end
        @assign condition = orig_comp.condition
        @assign attr = rdcl_comp.attributes
        #=  Use the dimensions of the redeclare if any, otherwise take them from the original.
        =#
        @assign dims = if arrayEmpty(rdcl_comp.dimensions)
          orig_comp.dimensions
        else
          rdcl_comp.dimensions
        end
        #=  TODO: Use comment of redeclare if available?
        =#
        @assign cmt = orig_comp.comment
        UNTYPED_COMPONENT(rdcl_comp.classInst, dims, bindingVar, condition, attr, cmt, false, rdcl_comp.info)
      end

      _  => begin
        Error.assertion(false, getInstanceName() + " got unknown components", sourceInfo())
        fail()
      end
    end
  end
  updateComponent!(new_comp, redeclaredNode)
end

""" #= Prints an error message and fails if it gets an outer component and a
             non-empty modifier. =#"""
               function checkOuterComponentMod(mod::Modifier, component::SCode.Element, node::InstNode)
                 if ! isEmpty(mod) && AbsynUtil.isOnlyOuter(SCodeUtil.prefixesInnerOuter(SCodeUtil.elementPrefixes(component)))
                   Error.addSourceMessage(Error.OUTER_ELEMENT_MOD, list(toString(mod, printName = false), name(node)), InstNode_info(node))
                   fail()
                 end
               end

"""
  This function instantiates component attributes.
  It uses ´DEFAULT_ATTR´ or some special modifier depending on the component prefix.
"""
function instComponentAttributes(compAttr::SCode.Attributes, compPrefs)::Attributes
  local attributes::Attributes
  local cty
  local par::ParallelismType
  local var::VariabilityType
  local dir::DirectionType
  local io
  local fin::Bool
  local redecl::Bool
  local repl
  attributes = begin
    @match (compAttr, compPrefs) begin
      #= Check if we want to use the default attributes=#
      (SCode.ATTR(connectorType = SCode.POTENTIAL(__),
                  parallelism = SCode.NON_PARALLEL(__),
                  variability = SCode.VAR(__),
                  direction = Absyn.BIDIR(__),
                  isField = false,
                  mode = false),
       SCode.PREFIXES(redeclarePrefix = SCode.NOT_REDECLARE(__),
                      finalPrefix = SCode.NOT_FINAL(__),
                      innerOuter = Absyn.NOT_INNER_OUTER(__),
                      replaceablePrefix = SCode.NOT_REPLACEABLE(__))
       ) => begin        
        @debug "Structural mode value: " structuralMode                        
        DEFAULT_ATTR
      end
      _  => begin
        cty = fromSCode(compAttr.connectorType)
        par = parallelismFromSCode(compAttr.parallelism)
        var = variabilityFromSCode(compAttr.variability)
        dir = directionFromSCode(compAttr.direction)
        io = innerOuterFromSCode(compPrefs.innerOuter)
        fin = SCodeUtil.finalBool(compPrefs.finalPrefix)
        redecl = SCodeUtil.redeclareBool(compPrefs.redeclarePrefix)
        repl = NOT_REPLACEABLE()
        structuralMode = compAttr.mode
        @debug "Structural mode value: " structuralMode
        ATTRIBUTES(cty, par, var, dir, io, fin, redecl, repl, structuralMode)
      end
    end
  end
  attributes
end

function mergeComponentAttributes(outerAttr::Attributes, innerAttr::Attributes, node::InstNode, parentRestriction::Restriction) ::Attributes
  local attr::Attributes

  local cty::ConnectorType.TYPE
  local par::ParallelismType
  local var::VariabilityType
  local dir::DirectionType
  local fin::Bool
  local redecl::Bool
  local repl::Replaceable

  if referenceEq(outerAttr, DEFAULT_ATTR) && innerAttr.connectorType == 0
    attr = innerAttr
  elseif referenceEq(innerAttr, DEFAULT_ATTR)
    cty = merge(outerAttr.connectorType, innerAttr.connectorType, node)
    attr = ATTRIBUTES(cty, outerAttr.parallelism,
                      outerAttr.variability, outerAttr.direction, innerAttr.innerOuter, outerAttr.isFinal,
                      innerAttr.isRedeclare, innerAttr.isReplaceable, innerAttr.isStructuralMode #=TODO: VSS Unsure if this should propagate like this=#)
  else
    cty = merge(outerAttr.connectorType, innerAttr.connectorType, node)
    par = mergeParallelism(outerAttr.parallelism, innerAttr.parallelism, node)
     var = variabilityMin(outerAttr.variability, innerAttr.variability)
    if isFunction(parentRestriction)
      dir = innerAttr.direction
    else
      dir = mergeDirection(outerAttr.direction, innerAttr.direction, node)
    end
    fin = outerAttr.isFinal || innerAttr.isFinal
    redecl = innerAttr.isRedeclare
    repl = innerAttr.isReplaceable
    attr = ATTRIBUTES(cty, par, var, dir, innerAttr.innerOuter, fin, redecl, repl,innerAttr.isStructuralMode #= TODO: VSS Unsure if this should propagate like this=#)
  end
  attr
end

function checkDeclaredComponentAttributes(attr::Attributes, parentRestriction::Restriction, component::InstNode) ::Attributes
  @assign () = begin
    @match parentRestriction begin
     RESTRICTION_CONNECTOR(__)  => begin
        #=  Components of a connector may not have prefixes 'inner' or 'outer'.
        =#
        assertNotInnerOuter(attr.innerOuter, component, parentRestriction)
        if parentRestriction.isExpandable
          assertNotFlowStream(attr.connectorType, component, parentRestriction)
          @assign attr.connectorType = intBitOr(attr.connectorType, ConnectorType.POTENTIALLY_PRESENT)
        end
        #=  Components of an expandable connector may not have the prefix 'flow'.
        =#
        #=  Mark components in expandable connectors as potentially present.
        =#
        ()
      end

      RESTRICTION_RECORD(__)  => begin
        #=  Elements of a record may not have prefixes 'input', 'output', 'inner', 'outer', 'stream', or 'flow'.
        =#
        assertNotInputOutput(attr.direction, component, parentRestriction)
        assertNotInnerOuter(attr.innerOuter, component, parentRestriction)
        assertNotFlowStream(attr.connectorType, component, parentRestriction)
        ()
      end

      _  => begin
        ()
      end
    end
  end
  attr
end

function invalidComponentPrefixError(prefix::String, node::InstNode, restriction)
  Error.addSourceMessage(Error.INVALID_COMPONENT_PREFIX, list(prefix, name(node), toString(restriction)), InstNode_info(node))
end

function assertNotInputOutput(dir, node::InstNode, restriction)
  if dir != Direction.NONE
    invalidComponentPrefixError(directionString(dir), node, restriction)
    fail()
  end
end

function assertNotInnerOuter(io, node::InstNode, restriction)
  if io != InnerOuter.NOT_INNER_OUTER
    invalidComponentPrefixError(innerOuterString(io), node, restriction)
    fail()
  end
end

function assertNotFlowStream(cty::ConnectorType.TYPE, node::InstNode, restriction)
  if ConnectorType.isFlowOrStream(cty)
    invalidComponentPrefixError(ConnectorType.toString(cty), node, restriction)
    fail()
  end
end

function mergeDerivedAttributes(outerAttr::Attributes, innerAttr::Attributes, node::InstNode) ::Attributes
  local attr::Attributes
  local cty::Int#ConnectorType.TYPE
  local par::ParallelismType
  local var::VariabilityType
  local dir::DirectionType
  local io::Int
  local fin::Bool
  local redecl::Bool
  local repl#::Replaceable type conversion work with this change?
  if referenceEq(innerAttr, DEFAULT_ATTR) && outerAttr.connectorType == 0
    attr = outerAttr
  elseif referenceEq(outerAttr, DEFAULT_ATTR) && innerAttr.connectorType == 0
    attr = innerAttr
  else
    #= The present error is here =#
    @match ATTRIBUTES(cty, par, var, dir, io, fin, redecl, repl, mo) = outerAttr
    cty = merge(cty, innerAttr.connectorType, node, true)
    var = variabilityMin(var, innerAttr.variability)
    dir = mergeDirection(dir, innerAttr.direction, node, #= allowSame = true=# true)
    attr = ATTRIBUTES(cty, par, var, dir, io, fin, redecl, repl, mo)
  end
  attr
end

function mergeRedeclaredComponentAttributes(origAttr::Attributes, redeclAttr::Attributes, node::InstNode)::Attributes
  local attr::Attributes
  local cty::ConnectorType.TYPE
  local rcty::ConnectorType.TYPE
  local cty_fs::ConnectorType.TYPE
  local rcty_fs::ConnectorType.TYPE
  local par::Parallelism
  local rpar::Parallelism
  local var::VariabilityType
  local rvar::VariabilityType
  local dir::DirectionType
  local rdir::DirectionType
  local io::InnerOuter
  local rio::InnerOuter
  local fin::Bool
  local redecl::Bool
  local repl::Replaceable

  if referenceEq(origAttr, DEFAULT_ATTR)
    @assign attr = redeclAttr
  elseif referenceEq(redeclAttr, DEFAULT_ATTR)
    @assign attr = origAttr
  else
    @match Attributes.ATTRIBUTES(cty, par, var, dir, io, _, _, _) = origAttr
    @match Attributes.ATTRIBUTES(rcty, rpar, rvar, rdir, rio, fin, redecl, repl) = redeclAttr
    @assign rcty_fs = intBitAnd(rcty, ConnectorType.FLOW_STREAM_MASK)
    @assign cty_fs = intBitAnd(cty, ConnectorType.FLOW_STREAM_MASK)
    if rcty_fs > 0
      if cty_fs > 0 && rcty_fs != cty_fs
        printRedeclarePrefixError(node, ConnectorType.toString(rcty), ConnectorType.toString(cty))
      end
    end
    if rpar != Parallelism.NON_PARALLEL
      if par != Parallelism.NON_PARALLEL && par != rpar
        printRedeclarePrefixError(node, parallelismString(rpar), parallelismString(par))
      end
      @assign par = rpar
    end
    if rvar != Variability.CONTINUOUS
      if rvar > var
        printRedeclarePrefixError(node, variabilityString(rvar), variabilityString(var))
      end
      @assign var = rvar
    end
    if rdir != Direction.NONE
      if dir != Direction.NONE && rdir != dir
        printRedeclarePrefixError(node, directionString(rdir), directionString(dir))
      end
      @assign dir = rdir
    end
    if rio != InnerOuter.NOT_INNER_OUTER
      if io != InnerOuter.NOT_INNER_OUTER && rio != io
        printRedeclarePrefixError(node, innerOuterString(rio), innerOuterString(io))
      end
      @assign io = rio
    end
    @assign attr = Attributes.ATTRIBUTES(rcty, par, var, dir, io, fin, redecl, repl)
  end
  attr
end

function mergeRedeclaredClassPrefixes(origPrefs::Prefixes, redeclPrefs::Prefixes, node::InstNode) ::Prefixes
  local prefs::Prefixes
  local enc::SCode.Encapsulated
  local par::SCode.Partial
  local fin::SCode.Final
  local io::Absyn.InnerOuter
  local rio::Absyn.InnerOuter
  local repl::SCode.Replaceable

  if referenceEq(origPrefs, DEFAULT_PREFIXES)
    @assign prefs = redeclPrefs
  else
    @match PREFIXES(innerOuter = io) = origPrefs
    @match PREFIXES(enc, par, fin, rio, repl) = redeclPrefs
    @assign io = begin
      @match (io, rio) begin
        (Absyn.NOT_INNER_OUTER(__), _)  => begin
          rio
        end

        (_, Absyn.NOT_INNER_OUTER(__))  => begin
          io
        end

        (Absyn.INNER(__), Absyn.InnerOuter.INNER(__))  => begin
          io
        end

        (Absyn.OUTER(__), Absyn.InnerOuter.OUTER(__))  => begin
          io
        end

        (Absyn.INNER_OUTER(__), Absyn.InnerOuter.INNER_OUTER(__))  => begin
          io
        end

        _  => begin
          printRedeclarePrefixError(node, innerOuterString(innerOuterFromSCode(rio)), innerOuterString(innerOuterFromSCode(io)))
          fail()
        end
      end
    end
    @assign prefs = PREFIXES(enc, par, fin, io, repl)
  end
  prefs
end

function printRedeclarePrefixError(node::InstNode, prefix1::String, prefix2::String)
  Error.addSourceMessageAndFail(Error.REDECLARE_MISMATCHED_PREFIX, list(prefix1, name(node), prefix2), InstNode_info(node))
end

function updateComponentVariability(attr::Attributes, cls::Class, clsNode::InstNode) ::Attributes
  local var::VariabilityType = attr.variability
  if referenceEq(attr, DEFAULT_ATTR) && isDiscreteClass(clsNode)
    @assign attr = IMPL_DISCRETE_ATTR
  elseif var == Variability.CONTINUOUS && isDiscreteClass(clsNode)
    @assign attr.variability = Variability.IMPLICITLY_DISCRETE
  end
  attr
end

function isDiscreteClass(clsNode::InstNode) ::Bool
  local discrete::Bool
  local base_node::InstNode
  local cls::Class
  local exts::Array{InstNode}
  base_node = lastBaseClass(clsNode)
  cls = getClass(base_node)
  discrete = begin
    @match cls begin
      EXPANDED_CLASS(restriction = RESTRICTION_TYPE(__))  => begin
        @assign exts = getExtends(cls.elements)
        if arrayLength(exts) == 1
          isDiscreteClass(exts[1])
        else
          false
        end
      end
      _  => begin
        isDiscrete(getType(cls, base_node))
      end
    end
  end
  discrete
end

function instTypeSpec(typeSpec::Absyn.TypeSpec, modifier::Modifier, attributes::Attributes, useBinding::Bool, scope::InstNode, parent::InstNode, info::SourceInfo, instLevel::Int) ::Tuple{InstNode, Attributes}
  local outAttributes::Attributes
  local node::InstNode

  @assign node = begin
    @match typeSpec begin
      Absyn.TPATH(__)  => begin
        @assign node = lookupClassName(typeSpec.path, scope, info)
        if instLevel >= 100
          checkRecursiveDefinition(node, parent, limitReached = true)
        end
        @assign node = expand(node)
        @assign (node, outAttributes) = instClass(node, modifier, attributes, useBinding, instLevel, parent)
        node
      end

      Absyn.TCOMPLEX(__)  => begin
        @error("NFInst.instTypeSpec: TCOMPLEX not implemented.\\n")
        fail()
      end
    end
  end
  (node, outAttributes)
end

""" #= Prints an error if a component causes a loop in the instance tree, for
             example because it has the same type as one of its parents. If the depth
             limit of the instance tree is reached, indicated by limitReached = true, then
             some error is always given. Otherwise an error is only given if an actual
             issue can be detected. =#"""
               function checkRecursiveDefinition(componentType::InstNode, component::InstNode, limitReached::Bool)
                 local parent::InstNode = parent(component)
                 local parent_type::InstNode

                 #=  Functions can contain instances of a parent, e.g. in equalityConstraint
                 =#
                 #=  functions, so skip this check for functions.
                 =#
                 if ! isFunction(getClass(parent))
                   while ! isEmpty(parent)
                     @assign parent_type = classScope(parent)
                     if referenceEq(definition(componentType), definition(parent_type))
                       Error.addSourceMessage(Error.RECURSIVE_DEFINITION, list(name(component), name(classScope(parent(component)))), InstNode_info(component))
                       fail()
                     end
                     @assign parent = parent(parent)
                   end
                 end

                 if limitReached
                   Error.addSourceMessage(Error.INST_RECURSION_LIMIT_REACHED, list(AbsynUtil.pathString(scopePath(component))), InstNode_info(component))
                   fail()
                 end
                 #=  If we couldn't determine the exact cause of the recursion, print a generic error.
                 =#
               end

function instDimension(dimension::Dimension, scope::InstNode, info::SourceInfo) ::Dimension


  @assign dimension = begin
    local dim::Absyn.Subscript
    local exp::Expression
    @match dimension begin
      DIMENSION_RAW_DIM(dim = dim)  => begin
        begin
          @match dim begin
            Absyn.NOSUB(__)  => begin
              DIMENSION_UNKNOWN()
            end

            Absyn.SUBSCRIPT(__)  => begin
              @assign exp = instExp(dim.subscript, scope, info)
              DIMENSION_UNTYPED(exp, false)
            end
          end
        end
      end

      _  => begin
        dimension
      end
    end
  end
  dimension
end

function instExpressions(node::InstNode, scope::InstNode = node, sections::Sections = SECTIONS_EMPTY())::Sections
  local cls::Class = getClass(node)
  local inst_cls::Class
  local local_comps::Array{InstNode}
  local exts::Array{InstNode}
  local cls_tree::ClassTree
  local res::Restriction
  local dims::Array{Dimension}
  local dim_scope::InstNode
  local info::SourceInfo
  local ty::NFType
  @assign () = begin
    @match cls begin
      EXPANDED_CLASS(elements = cls_tree, restriction = RESTRICTION_TYPE(__))  => begin
        #=  Long class declaration of a type.
        =#
        #=  Instantiate expressions in the extends nodes.
        =#
        exts = getExtends(cls_tree)
        for ext in exts
          instExpressions(ext, ext, sections)
        end
        #=  A type must extend a basic type.
        =#
        if arrayLength(exts) == 1
          @assign ty = TYPE_COMPLEX(node, ComplexType.EXTENDS_TYPE(exts[1]))
        elseif SCodeUtil.hasBooleanNamedAnnotationInClass(definition(node), "__OpenModelica_builtinType")
          @assign ty = TYPE_COMPLEX(node, ComplexType.CLASS())
        else
          Error.addSourceMessage(Error.MISSING_TYPE_BASETYPE, list(name(node)), infoInstNode_info(node))
          fail()
        end
        @assign cls_tree = flatten(cls_tree)
        @assign inst_cls = INSTANCED_CLASS(ty, cls_tree, SECTIONS_EMPTY(), cls.restriction)
        updateClass(inst_cls, node)
        ()
      end

      EXPANDED_CLASS(elements = cls_tree)  => begin
        #=  Instantiate expressions in the extends nodes.
        =#
        for ext in getExtends(cls_tree)
          @assign sections = instExpressions(ext, ext, sections)
        end
        #=  Instantiate expressions in the local components.
        =#
        applyLocalComponents(cls_tree, instComponentExpressions)
        #=  Flatten the class tree so we don't need to deal with extends anymore.
        =#
        @assign cls.elements = flatten(cls_tree)
        updateClass(cls, node)
        #=  Instantiate local equation/algorithm sections.
        =#
        @assign sections = instSections(node, scope, sections, isFunction(cls.restriction))
        @assign ty = makeComplexType(cls.restriction, node, cls)
        @assign inst_cls = INSTANCED_CLASS(ty, cls.elements, sections, cls.restriction)
        updateClass(inst_cls, node)
        instComplexType(ty)
        ()
      end

      EXPANDED_DERIVED(dims = dims)  => begin
        @assign sections = instExpressions(cls.baseClass, scope, sections)
        @assign dim_scope = parent(node)
        @assign info = InstNode_info(node)
        for i in 1:arrayLength(dims)
          @assign dims[i] = instDimension(dims[i], dim_scope, info)
        end
        if isRecord(cls.restriction)
          instRecordConstructor(node)
        end
        ()
      end

      INSTANCED_BUILTIN(elements = CLASS_TREE_FLAT_TREE(components = local_comps))  => begin
        for comp in local_comps
          instComponentExpressions(comp)
        end
        ()
      end

      INSTANCED_BUILTIN(__)  => begin
        ()
      end

      INSTANCED_CLASS(__)  => begin
        ()
      end

      _  => begin
        Error.assertion(false, getInstanceName() + " got invalid class", sourceInfo())
        fail()
      end
    end
  end
  sections
end

function makeComplexType(restriction::Restriction, node::InstNode, cls::Class)::NFType
  local ty::NFType
  local cty::ComplexType
  @assign cty = begin
    @match restriction begin
      RESTRICTION_RECORD(__)  => begin
        makeRecordComplexType(classScope(getDerivedNode(node)), cls)
      end
      _  => begin
        COMPLEX_CLASS()
      end
    end
  end
  @assign ty = TYPE_COMPLEX(node, cty)
  ty
end

function makeRecordComplexType(node::InstNode, cls::Class) ::ComplexType
  local ty::ComplexType

  local cls_node::InstNode
  local fields::List{Record.P_Field}

  @assign cls_node = if SCodeUtil.isOperatorRecord(definition(node))
    classScope(node)
  else
    classScope(getDerivedNode(node))
  end
  @assign ty = COMPLEX_RECORD(cls_node, nil)
  ty
end

function instComplexType(ty::NFType)
  @assign () = begin
    local node::InstNode
    local cache::CachedData
    @match ty begin
      TYPE_COMPLEX(complexTy = COMPLEX_RECORD(node)) where (! isModel(node))  => begin
        #=  Make sure it's really a record, and not e.g. a record inherited by a model.
        =#
        #=  TODO: This check should really be InstNode.isRecord(node), but that
        =#
        #=        causes issues with e.g. ComplexInput/ComplexOutput.
        =#
        instRecordConstructor(node)
        ()
      end
      _  => begin
        ()
      end
    end
  end
end

function instRecordConstructor(node::InstNode)
  local cache::CachedData

  @assign cache = getFuncCache(node)
  @assign () = begin
    @match cache begin
      C_FUNCTION(__)  => begin
        ()
      end
      _  => begin
        cacheInitFunc(node)
        if SCodeUtil.isOperatorRecord(definition(node))
          OperatorOverloading.instConstructor(scopePath(node, includeRoot = true), node, InstNode_info(node))
        else
          Record.instDefaultConstructor(scopePath(node, includeRoot = true), node, InstNode_info(node))
        end
        ()
      end
    end
  end
end

function instBuiltinAttribute(attribute::Modifier, node::InstNode) ::Modifier

 strMod1 = toString(attribute, true)
 @debug ">instBuiltinAttribute($strMod1)"

  @assign () = begin
    local bindingVar::Binding
    @match attribute begin
      MODIFIER_MODIFIER(binding=bindingVar)  => begin
        @assign bindingVar = addParent(node, bindingVar)
        @assign attribute.binding = instBinding(bindingVar)
        ()
      end
      MODIFIER_REDECLARE(__)  => begin
        #=  Redeclaration of builtin attributes is not allowed.
        =#
        Error.addSourceMessage(Error.INVALID_REDECLARE_IN_BASIC_TYPE, list(name(attribute)), Modifier_info(attribute))
        fail()
      end
      _  => begin
        ()
      end
    end
  end

  strMod2 = toString(attribute, true)
  @debug "<instBuiltinAttribute($strMod2)"
  attribute
end

function instComponentExpressions(componentArg::InstNode)
  local node::InstNode = resolveOuter(componentArg)
  local c::Component = component(node)
  local dims::Array{Dimension}
  @match c begin
    UNTYPED_COMPONENT(dimensions = dims, instantiated = false)  => begin
      @assign c.binding = instBinding(c.binding)
      @assign c.condition = instBinding(c.condition)
      instExpressions(c.classInst, node)
      for i in 1:arrayLength(dims)
        @inbounds dims[i] = instDimension(dims[i], parent(node), c.info)
      end
      #=  This is to avoid instantiating the same component multiple times,
      =#
      #=  which can otherwise happen with duplicate components at this stage.
      =#
      @assign c.instantiated = true
      updateComponent!(c, node)
      ()
    end

    UNTYPED_COMPONENT(__)  => begin
      ()
    end

    ENUM_LITERAL_COMPONENT(__)  => begin
      ()
    end

    TYPE_ATTRIBUTE(modifier = MODIFIER_NOMOD(__))  => begin
      ()
    end

    TYPE_ATTRIBUTE(__)  => begin
      @assign c.modifier = instBuiltinAttribute(c.modifier, componentArg)
      updateComponent!(c, node)
      ()
    end
    _  => begin
      Error.assertion(false, getInstanceName() + " got invalid component", sourceInfo())
      fail()
    end
  end
  return ()
end

function instBinding(bindingVar::Binding)::Binding
  @assign bindingVar = begin
    local bind_exp::Expression
    @match bindingVar begin
      RAW_BINDING(__)  => begin
        bind_exp = instExp(bindingVar.bindingExp, bindingVar.scope, bindingVar.info)
        bind_exp = BINDING_EXP(bind_exp, TYPE_UNKNOWN(), TYPE_UNKNOWN(), bindingVar.parents, bindingVar.isEach)
        UNTYPED_BINDING(bind_exp, false, bindingVar.scope, bindingVar.isEach, bindingVar.info)
      end
      _  => begin
        bindingVar
      end
    end
  end
  bindingVar
end

function instExpOpt(absynExp::Option{<:Absyn.Exp}, scope::InstNode, info::SourceInfo) ::Option{Expression}
  local exp::Option{Expression}
  @assign exp = begin
    local aexp::Absyn.Exp
    @match absynExp begin
      NONE()  => begin
        NONE()
      end
      SOME(aexp)  => begin
        SOME(instExp(aexp, scope, info))
      end
    end
  end
  exp
end

function instExp(absynExp::Absyn.Exp, scope::InstNode, info::SourceInfo) ::Expression
  local exp::Expression
  @assign exp = begin
    local e1::Expression
    local e2::Expression
    local e3::Expression
    local oe::Option{Expression}
    local op::Operator
    local expl::List{Expression}
    local expll::List{List{Expression}}
    @match absynExp begin
      Absyn.INTEGER(__)  => begin
        INTEGER_EXPRESSION(absynExp.value)
      end

      Absyn.REAL(__)  => begin
        REAL_EXPRESSION(stringReal(absynExp.value))
      end

      Absyn.STRING(__)  => begin
        STRING_EXPRESSION(System.unescapedString(absynExp.value))
      end

      Absyn.BOOL(__)  => begin
        BOOLEAN_EXPRESSION(absynExp.value)
      end

      Absyn.CREF(__)  => begin
        instCref(absynExp.componentRef, scope, info)
      end

      Absyn.ARRAY(__)  => begin
        @assign expl = list(instExp(e, scope, info) for e in absynExp.arrayExp)
        makeArray(TYPE_UNKNOWN(), expl)
      end

      Absyn.MATRIX(__)  => begin
        @assign expll = list(list(instExp(e, scope, info) for e in el) for el in absynExp.matrix)
        MATRIX_EXPRESSION(expll)
      end

      Absyn.RANGE(__)  => begin
        @assign e1 = instExp(absynExp.start, scope, info)
        @assign oe = instExpOpt(absynExp.step, scope, info)
        @assign e3 = instExp(absynExp.stop, scope, info)
        RANGE_EXPRESSION(TYPE_UNKNOWN(), e1, oe, e3)
      end

      Absyn.TUPLE(__)  => begin
        @assign expl = list(instExp(e, scope, info) for e in absynExp.expressions)
        TUPLE_EXPRESSION(TYPE_UNKNOWN(), expl)
      end

      Absyn.BINARY(__)  => begin
        @assign e1 = instExp(absynExp.exp1, scope, info)
        @assign e2 = instExp(absynExp.exp2, scope, info)
        @assign op = fromAbsyn(absynExp.op)
        BINARY_EXPRESSION(e1, op, e2)
      end

      Absyn.UNARY(__)  => begin
        @assign e1 = instExp(absynExp.exp, scope, info)
        @assign op = fromAbsyn(absynExp.op)
        UNARY_EXPRESSION(op, e1)
      end

      Absyn.LBINARY(__)  => begin
        @assign e1 = instExp(absynExp.exp1, scope, info)
        @assign e2 = instExp(absynExp.exp2, scope, info)
        @assign op = fromAbsyn(absynExp.op)
        LBINARY_EXPRESSION(e1, op, e2)
      end

      Absyn.LUNARY(__)  => begin
        @assign e1 = instExp(absynExp.exp, scope, info)
        @assign op = fromAbsyn(absynExp.op)
        LUNARY_EXPRESSION(op, e1)
      end

      Absyn.RELATION(__)  => begin
        @assign e1 = instExp(absynExp.exp1, scope, info)
        @assign e2 = instExp(absynExp.exp2, scope, info)
        @assign op = fromAbsyn(absynExp.op)
        RELATION_EXPRESSION(e1, op, e2)
      end

      Absyn.IFEXP(__)  => begin
        @assign e3 = instExp(absynExp.elseBranch, scope, info)
        for branch in listReverse(absynExp.elseIfBranch)
          @assign e1 = instExp(Util.tuple21(branch), scope, info)
          @assign e2 = instExp(Util.tuple22(branch), scope, info)
          @assign e3 = IF_EXPRESSION(e1, e2, e3)
        end
        @assign e1 = instExp(absynExp.ifExp, scope, info)
        @assign e2 = instExp(absynExp.trueBranch, scope, info)
        IF_EXPRESSION(e1, e2, e3)
      end

      Absyn.CALL(__)  => begin
        instantiate(absynExp.function_, absynExp.functionArgs, scope, info)
      end

      Absyn.PARTEVALFUNCTION(__)  => begin
        instPartEvalFunction(absynExp.function_, absynExp.functionArgs, scope, info)
      end

      Absyn.END(__)  => begin
        END_EXPRESSION()
      end
      _  => begin
        @error "UNKNOWN EXPRESSION!"
        fail()
      end
    end
  end
  exp
end

function instCref(absynCref::Absyn.ComponentRef, scope::InstNode, info::SourceInfo) ::Expression
  local crefExp::Expression
  local cref::ComponentRef
  local prefixed_cref::ComponentRef
  local found_scope::InstNode
  local ty::M_Type
  local comp::Component
  @assign (cref, found_scope) = begin
    @match absynCref begin
      Absyn.WILD(__)  => begin
        (COMPONENT_REF_WILD(), scope)
      end
      Absyn.ALLWILD(__)  => begin
        (COMPONENT_REF_WILD(), scope)
      end
      _  => begin
        lookupComponent(absynCref, scope, info)
      end
    end
  end
  @assign cref = instCrefSubscripts(cref, scope, info)
  @assign crefExp = begin
    @match cref begin
      COMPONENT_REF_CREF(__)  => begin
        begin
          @match cref.node begin
            COMPONENT_NODE(__)  => begin
              instCrefComponent(cref, cref.node, found_scope, info)
            end
            CLASS_NODE(__)  => begin
              if isFunction(getClass(cref.node))
                instCrefFunction(cref, info)
              else
                instCrefTypename(cref, cref.node, info)
              end
            end
            _  => begin
              Error.assertion(false, getInstanceName() + " got invalid instance node", sourceInfo())
              fail()
            end
          end
        end
      end
      _  => begin
        CREF_EXPRESSION(TYPE_UNKNOWN(), cref)
      end
    end
  end
  crefExp
end

function instCrefComponent(cref::ComponentRef, node::InstNode, scope::InstNode, info::SourceInfo) ::Expression
  local crefExp::Expression
  local comp::Component
  local prefixed_cref::ComponentRef
  @assign comp = component(node)
  @assign crefExp = begin
    @match comp begin
      ITERATOR_COMPONENT(__)  => begin
        checkUnsubscriptableCref(cref, info)
        CREF_EXPRESSION(TYPE_UNKNOWN(), makeIterator(node, comp.ty))
      end
      ENUM_LITERAL_COMPONENT(__)  => begin
        checkUnsubscriptableCref(cref, info)
        comp.literal
      end
      TYPE_ATTRIBUTE(__)  => begin
        Error.addSourceMessage(Error.LOOKUP_VARIABLE_ERROR, list(name(node), name(parent(node))), info)
        fail()
      end
      _  => begin
        @assign prefixed_cref = fromNodeList(scopeList(scope))
        @assign prefixed_cref = if isEmpty(prefixed_cref)
          cref
        else
          append(cref, prefixed_cref)
        end
        CREF_EXPRESSION(TYPE_UNKNOWN(), prefixed_cref)
      end
    end
  end
  crefExp
end

function instCrefFunction(cref::ComponentRef, info::SourceInfo) ::Expression
  local crefExp::Expression

  local fn_ref::ComponentRef

  @assign fn_ref = instFunctionRef(cref, info)
  @assign crefExp = CREF_EXPRESSION(TYPE_UNKNOWN(), fn_ref)
  crefExp
end

function instCrefTypename(@nospecialize(cref::ComponentRef), @nospecialize(node::InstNode), info::SourceInfo)x::Expression
  local crefExp::Expression
  local ty::NFType
  checkUnsubscriptableCref(cref, info)
  @assign ty = getType(node)
  @assign ty = begin
    @match ty begin
      TYPE_BOOLEAN(__)  => begin
        TYPE_ARRAY(ty, list(P_Dimension.Dimension.BOOLEAN()))
      end
      TYPE_ENUMERATION(__)  => begin
        TYPE_ARRAY(ty, list(P_Dimension.Dimension.ENUM(ty)))
      end
      _  => begin
        #Error.assertion(false, getInstanceName() + " got unknown class node " + name(node), sourceInfo())
        fail()
      end
    end
  end
  @assign crefExp = Expression.TYPENAME(ty)
  crefExp
end

function checkUnsubscriptableCref(cref::ComponentRef, info::SourceInfo)
  if hasSubscripts(cref)
    Error.addSourceMessage(Error.WRONG_NUMBER_OF_SUBSCRIPTS, list(ComponentRef.toString(cref), String(listLength(ComponentRef.getSubscripts(cref))), "0"), info)
    fail()
  end
end

function instCrefSubscripts(cref::ComponentRef, scope::InstNode, info::SourceInfo) ::ComponentRef
  @assign () = begin
    local rest_cr::ComponentRef
    @match cref begin
      COMPONENT_REF_CREF(__)  => begin
        if ! listEmpty(cref.subscripts)
          @assign cref.subscripts = list(instSubscript(s, scope, info) for s in cref.subscripts)
        end
        @assign rest_cr = instCrefSubscripts(cref.restCref, scope, info)
        if ! referenceEq(rest_cr, cref.restCref)
          @assign cref.restCref = rest_cr
        end
        ()
      end
      _  => begin
        ()
      end
    end
  end
  cref
end

function instSubscript(subscript::Subscript, scope::InstNode, info::SourceInfo)::Subscript
  local outSubscript::Subscript
  local exp::Expression
  local absynSub::Absyn.Subscript
  @match SUBSCRIPT_RAW_SUBSCRIPT(subscript = absynSub) = subscript
  outSubscript = begin
    @match absynSub begin
      Absyn.NOSUB(__)  => begin
        SUBSCRIPT_WHOLE()
      end
      Absyn.SUBSCRIPT(__)  => begin
        exp = instExp(absynSub.subscript, scope, info)
        fromExp(exp)
      end
    end
  end
  outSubscript
end

function instPartEvalFunction(func::Absyn.ComponentRef, funcArgs::Absyn.FunctionArgs, scope::InstNode, info::SourceInfo) ::Expression
  local outExp::Expression
  local fn_ref::ComponentRef
  local nargs::List{Absyn.NamedArg}
  local args::List{Expression}
  local arg_names::List{String}
  @match Absyn.FunctionArgs.FUNCTIONARGS(argNames = nargs) = funcArgs
  @assign outExp = instCref(func, scope, info)
  if ! listEmpty(nargs)
    @assign fn_ref = toCref(outExp)
    @assign args = list(instExp(arg.argValue, scope, info) for arg in nargs)
    @assign arg_names = list(arg.argName for arg in nargs)
    @assign outExp = PARTIAL_FUNCTION_APPLICATION_EXPRESSION(fn_ref, args, arg_names, TYPE_UNKNOWN())
  end
  outExp
end

function instSections(node::InstNode, scope::InstNode, sections::Sections, isFunction::Bool) ::Sections
  local el::SCode.Element = definition(node)
  local def::SCode.ClassDef
  @assign sections = begin
    @match el begin
      SCode.CLASS(classDef = SCode.PARTS(__))  => begin
        instSections2(el.classDef, scope, sections, isFunction)
      end
      SCode.CLASS(classDef = SCode.CLASS_EXTENDS(composition = def && SCode.PARTS(__)))  => begin
        instSections2(def, scope, sections, isFunction)
      end
      _  => begin
        sections
      end
    end
  end
  sections
end

function instSections2(parts::SCode.ClassDef, scope::InstNode, sections::Sections, isFunction::Bool) ::Sections
  @assign sections = begin
    local eq::List{Equation}
    local ieq::List{Equation}
    local alg::List{Algorithm}
    local ialg::List{Algorithm}
    local ext_decl::SCode.ExternalDecl
    local origin::ORIGIN_Type
    local iorigin::ORIGIN_Type
    @match (parts, sections) begin
      (_, SECTIONS_EXTERNAL(__))  => begin
        Error.addSourceMessage(Error.MULTIPLE_SECTIONS_IN_FUNCTION, list(name(scope)), info(scope))
        fail()
      end
      (SCode.PARTS(externalDecl = SOME(ext_decl)), _)  => begin
        instExternalDecl(ext_decl, scope)
      end
      (SCode.PARTS(__), _)  => begin
        @assign origin = if isFunction
          ORIGIN_FUNCTION
        else
          ORIGIN_CLASS
        end
        @assign iorigin = setFlag(origin, ORIGIN_INITIAL)
        @assign eq = instEquations(parts.normalEquationLst, scope, origin)
        @assign ieq = instEquations(parts.initialEquationLst, scope, iorigin)
        @assign alg = instAlgorithmSections(parts.normalAlgorithmLst, scope, origin)
        @assign ialg = instAlgorithmSections(parts.initialAlgorithmLst, scope, iorigin)
        join(new(eq, ieq, alg, ialg), sections)
      end
    end
  end
  sections
end

function instExternalDecl(@nospecialize(extDecl::SCode.ExternalDecl), @nospecialize(scope::InstNode)) ::Sections
  local sections::Sections
  @assign sections = begin
    local nameVar::String
    local lang::String
    local args::List{Expression}
    local ret_cref::ComponentRef
    local info::SourceInfo
    @match extDecl begin
      SCode.EXTERNALDECL(__)  => begin
        @assign info = InstNode_info(scope)
        @assign nameVar = Util.getOptionOrDefault(extDecl.funcName, name(scope))
        @assign lang = Util.getOptionOrDefault(extDecl.lang, "C")
        checkExternalDeclLanguage(lang, info)
        @assign args = list(instExp(arg, scope, info) for arg in extDecl.args)
        if isSome(extDecl.output_)
          @assign ret_cref = Lookup.lookupLocalComponent(Util.getOption(extDecl.output_), scope, info)
        else
          @assign ret_cref = COMPONENT_REF_EMPTY()
        end
        SECTIONS_EXTERNAL(nameVar, args, ret_cref, lang, extDecl.annotation_, isSome(extDecl.funcName))
      end
    end
  end
  sections
end

""" #= Checks that the language declared for an external function is valid. =#"""
function checkExternalDeclLanguage(language::String, info::SourceInfo)
  @assign () = begin
    @match language begin
      "C"  => begin
        ()
      end

      "FORTRAN 77"  => begin
        ()
      end

      "Fortran 77"  => begin
        ()
      end

      "builtin"  => begin
        ()
      end

      _  => begin
        #=  The specification also allows for C89, C99, and C11, but our code
        =#
        #=  generation only seems to support C.
        =#
        #=  Not in the specification, but used by libraries and allowed by other tools.
        =#
        Error.addSourceMessage(Error.INVALID_EXTERNAL_LANGUAGE, list(language), info)
        fail()
      end
    end
  end
end

function instEquations(scodeEql::List{<:SCode.Equation}, scope::InstNode, origin::ORIGIN_Type)::List{Equation}
  local instEql::List{Equation}
  @assign instEql = list(instEquation(eq, scope, origin) for eq in scodeEql)
  instEql
end

function instEquation(scodeEq::SCode.Equation, scope::InstNode, origin::ORIGIN_Type) ::Equation
  local instEq::Equation
  local eq::SCode.EEquation
  @match SCode.EQUATION(eEquation = eq) = scodeEq
  @assign instEq = instEEquation(eq, scope, origin)
  instEq
end

function instEEquations(scodeEql::List{<:SCode.EEquation}, scope::InstNode, origin::ORIGIN_Type) ::List{Equation}
  local instEql::List{Equation}
  @assign instEql = list(instEEquation(eq, scope, origin) for eq in scodeEql)
  instEql
end

function instEEquation(scodeEq::SCode.EEquation, scope::InstNode, origin::ORIGIN_Type) ::Equation
  local instEq::Equation
  @assign instEq = begin
    local exp1::Expression
    local exp2::Expression
    local exp3::Expression
    local oexp::Option{Expression}
    local expl::List{Expression}
    local eql::List{Equation}
    local branches::List{Equation_Branch}
    local info::SourceInfo
    local for_scope::InstNode
    local iter::InstNode
    local lhs_cr::ComponentRef
    local rhs_cr::ComponentRef
    local next_origin::ORIGIN_Type
    @match scodeEq begin
      SCode.EQ_EQUALS(info = info)  => begin
        @assign exp1 = instExp(scodeEq.expLeft, scope, info)
        @assign exp2 = instExp(scodeEq.expRight, scope, info)
        EQUATION_EQUALITY(exp1, exp2, TYPE_UNKNOWN(), makeSource(scodeEq.comment, info))
      end

      SCode.EQ_CONNECT(info = info)  => begin
        if flagSet(origin, ORIGIN_WHEN)
          Error.addSourceMessage(Error.CONNECT_IN_WHEN, list(Dump.printComponentRefStr(scodeEq.crefLeft), Dump.printComponentRefStr(scodeEq.crefRight)), info)
          fail()
        end
        @assign exp1 = instConnectorCref(scodeEq.crefLeft, scope, info)
        @assign exp2 = instConnectorCref(scodeEq.crefRight, scope, info)
        EQUATION_CONNECT(exp1, exp2, makeSource(scodeEq.comment, info))
      end

      SCode.EQ_FOR(info = info)  => begin
        @assign oexp = instExpOpt(scodeEq.range, scope, info)
        checkIteratorShadowing(scodeEq.index, scope, scodeEq.info)
        @assign (for_scope, iter) = addIteratorToScope(scodeEq.index, scope, scodeEq.info)
        @assign next_origin = setFlag(origin, ORIGIN_FOR)
        @assign eql = instEEquations(scodeEq.eEquationLst, for_scope, next_origin)
        EQUATION_FOR(iter, oexp, eql, makeSource(scodeEq.comment, info))
      end
      
      SCode.EQ_IF(info = info)  => begin
        #=  Instantiate the conditions.=#
        @assign expl = list(instExp(c, scope, info) for c in scodeEq.condition)
        #=  Instantiate each branch and pair it up with a condition.
        =#
        @assign next_origin = setFlag(origin, ORIGIN_IF)
        @assign branches = nil
        for branch in scodeEq.thenBranch
          @assign eql = instEEquations(branch, scope, next_origin)
          @match _cons(exp1, expl) = expl
          @assign branches = _cons(makeBranch(exp1, eql), branches)
        end
        #=  Instantiate the else-branch, if there is one, and make it a branch
        =#
        #=  with condition true (so we only need a simple list of branches).
        =#
        if ! listEmpty(scodeEq.elseBranch)
          @assign eql = instEEquations(scodeEq.elseBranch, scope, next_origin)
          @assign branches = _cons(makeBranch(BOOLEAN_EXPRESSION(true), eql), branches)
        end
        EQUATION_IF(listReverse(branches), makeSource(scodeEq.comment, info))
      end

      SCode.EQ_WHEN(info = info)  => begin
        if flagSet(origin, ORIGIN_WHEN)
          Error.addSourceMessageAndFail(Error.NESTED_WHEN, nil, info)
        elseif flagSet(origin, ORIGIN_INITIAL)
          Error.addSourceMessageAndFail(Error.INITIAL_WHEN, nil, info)
        end
        @assign next_origin = setFlag(origin, ORIGIN_WHEN)
        @assign exp1 = instExp(scodeEq.condition, scope, info)
        @assign eql = instEEquations(scodeEq.eEquationLst, scope, next_origin)
        @assign branches = list(makeBranch(exp1, eql))
        for branch in scodeEq.elseBranches
          @assign exp1 = instExp(Util.tuple21(branch), scope, info)
          @assign eql = instEEquations(Util.tuple22(branch), scope, next_origin)
          @assign branches = _cons(makeBranch(exp1, eql), branches)
        end
        EQUATION_WHEN(listReverse(branches), makeSource(scodeEq.comment, info))
      end

      SCode.EQ_ASSERT(info = info)  => begin
        @assign exp1 = instExp(scodeEq.condition, scope, info)
        @assign exp2 = instExp(scodeEq.message, scope, info)
        @assign exp3 = instExp(scodeEq.level, scope, info)
        EQUATION_ASSERT(exp1, exp2, exp3, makeSource(scodeEq.comment, info))
      end

      SCode.EQ_TERMINATE(info = info)  => begin
        @assign exp1 = instExp(scodeEq.message, scope, info)
        EQUATION_TERMINATE(exp1, makeSource(scodeEq.comment, info))
      end

      SCode.EQ_REINIT(info = info)  => begin
        if flagNotSet(origin, ORIGIN_WHEN)
          Error.addSourceMessage(Error.REINIT_NOT_IN_WHEN, nil, info)
          fail()
        end
        @assign exp1 = instExp(scodeEq.cref, scope, info)
        @assign exp2 = instExp(scodeEq.expReinit, scope, info)
        EQUATION_REINIT(exp1, exp2, makeSource(scodeEq.comment, info))
      end

      SCode.EQ_NORETCALL(info = info)  => begin
        @assign exp1 = instExp(scodeEq.exp, scope, info)
        EQUATION_NORETCALL(exp1, makeSource(scodeEq.comment, info))
      end

      _  => begin
        Error.assertion(false, getInstanceName() + " got unknown equation", sourceInfo())
        fail()
      end
    end
end
instEq
end

function instConnectorCref(absynCref::Absyn.ComponentRef, scope::InstNode, info::SourceInfo) ::Expression
  local outExp::Expression
  local cref::ComponentRef
  local prefix::ComponentRef
  local found_scope::InstNode
  @assign (cref, found_scope) = lookupConnector(absynCref, scope, info)
  @assign cref = instCrefSubscripts(cref, scope, info)
  @assign prefix = fromNodeList(scopeList(found_scope))
  if ! isEmpty(prefix)
    @assign cref = append(cref, prefix)
  end
  @assign outExp = CREF_EXPRESSION(TYPE_UNKNOWN(), cref)
  outExp
end

function makeSource(comment::SCode.Comment, info::SourceInfo) ::DAE.ElementSource
  local source::DAE.ElementSource
  @assign source = DAE.SOURCE(info, nil, "TODO Dummy", nil, nil, nil, list(comment))
  source
end

function addIteratorToScope(name::String, scope::InstNode, info::SourceInfo, iter_type::NFType = TYPE_UNKNOWN()) ::Tuple{InstNode, InstNode}
  local iterator::InstNode


  local iter_comp::Component

  @assign scope = openImplicitScope(scope)
  @assign iter_comp = ITERATOR_COMPONENT(iter_type, Variability.CONTINUOUS, info)
  @assign iterator = fromComponent(name, iter_comp, scope)
  @assign scope = addIterator(iterator, scope)
  (scope, iterator)
end
""" #= Gives a warning if the given iterator name is already used in an outer
             implicit scope. =#"""
               function checkIteratorShadowing(name::String, scope::InstNode, info::SourceInfo)
                 @assign () = begin
                   @match scope begin
                     IMPLICIT_SCOPE(__)  => begin
                       for iter in scope.locals
                         if name(iter) == name
                           Error.addMultiSourceMessage(Error.SHADOWED_ITERATOR, list(name), list(info(iter), info))
                           return
                         end
                       end
                       ()
                     end

                     _  => begin
                       ()
                     end
                   end
                 end
               end

" #= Inner elements can be generated automatically during instantiation if they're
                   missing, and are stored in the cache of the top scope since that's easily
                   accessible during lookup. This function copies any such inner elements into
                   the class we're instantiating, so that they are typed and flattened properly. =#"
function insertGeneratedInners(node::InstNode, topScope::InstNode)
  local inner_tree::NodeTree.Tree
  local inner_nodes::List{Tuple{String, InstNode}}
  local inner_comps::List{Pointer{InstNode}}
  local n::InstNode
  local name::String
  local str::String
  local cls::Class
  local cls_tree::ClassTree
  local base_node::InstNode
@debug "Calling insert generate inners!"
  @match C_TOP_SCOPE(addedInner = inner_tree) = getInnerOuterCache(topScope)
  #=  Empty tree => nothing more to do.
  =#
  if NodeTree.isEmpty(inner_tree)
    @debug "EMPTY NODE TREE"
    return
  end
  @assign inner_nodes = NodeTree.toList(inner_tree)
  @assign inner_comps = nil
  for e in inner_nodes
    @assign (name, n) = e
    Error.addSourceMessage(Error.MISSING_INNER_ADDED, list(typeName(n), name), info(n))
    if isComponent(n)
      instComponent(n, DEFAULT_ATTR, MODIFIER_NOMOD(), true, 0)
      try
        @match Absyn.STRING(str) = SCodeUtil.getElementNamedAnnotation(definition(classScope(n)), "missingInnerMessage")
        Error.addSourceMessage(Error.MISSING_INNER_MESSAGE, list(System.unescapedString(str)), info(n))
      catch
        @error "Error missing inners!"
        fail()
      end
      @assign inner_comps = _cons(P_Pointer.create(n), inner_comps)
    end
  end
  if ! listEmpty(inner_comps)
    @assign base_node = lastBaseClass(node)
    @assign cls = getClass(base_node)
    @assign cls_tree = appendComponentsToInstTree(inner_comps, classTree(cls))
    updateClass(setClassTree(cls_tree, cls), base_node)
  end
end

function updateImplicitVariability(node::InstNode, evalAllParams::Bool)
  local cls::Class = getClass(node)
  local cls_tree::ClassTree
  @assign () = begin
    @match cls begin
      INSTANCED_CLASS(elements = cls_tree && CLASS_TREE_FLAT_TREE(__))  => begin
        for c in cls_tree.components
          updateImplicitVariabilityComp(c, evalAllParams)
        end
        apply(cls.sections, (inWhen = false) -> updateImplicitVariabilityEq(inWhen), updateImplicitVariabilityAlg)
        ()
      end

      EXPANDED_DERIVED(__)  => begin
        for dim in cls.dims
          markStructuralParamsDim(dim)
        end
        updateImplicitVariability(cls.baseClass, evalAllParams)
        ()
      end

      INSTANCED_BUILTIN(elements = cls_tree && CLASS_TREE_FLAT_TREE(__))  => begin
        for c in cls_tree.components
          updateImplicitVariabilityComp(c, evalAllParams)
        end
        ()
      end

      _  => begin
        ()
      end
    end
  end
end

function updateImplicitVariabilityComp(co::InstNode, evalAllParams::Bool)
  local node::InstNode = resolveOuter(co)
  local c::Component = component(node)

  @assign () = begin
    local bnd::Binding
    local condition::Binding
    @match c begin
      UNTYPED_COMPONENT(binding = bnd, condition = condition)  => begin
        if isStructuralComponent(c, c.attributes, bnd, node, evalAllParams)
          markStructuralParamsComp(c, node)
        end
        #=  Parameters used in array dimensions are structural.
        =#
        for dim in c.dimensions
          markStructuralParamsDim(dim)
        end
        #=  Parameters that determine the size of a component binding are structural.
        =#
        if isBound(bnd)
          markStructuralParamsExpSize(getUntypedExp(bnd))
        end
        #=  Parameters used in a component condition are structural.
        =#
        if isBound(condition)
          markStructuralParamsExp(getUntypedExp(condition))
        end
        updateImplicitVariability(c.classInst, evalAllParams)
        ()
      end

      TYPE_ATTRIBUTE(__) where (listMember(name(co), list("fixed", "stateSelect")))  => begin
        @assign bnd = binding(c.modifier)
        if isBound(bnd)
          markStructuralParamsExp(getUntypedExp(bnd))
        end
        ()
      end

      _  => begin
        ()
      end
    end
  end
end

function isStructuralComponent(component::Component, compAttrs::Attributes, compBinding::Binding, compNode::InstNode, evalAllParams::Bool) ::Bool
  local isStructural::Bool

  local is_fixed::Bool

  if compAttrs.variability != Variability.PARAMETER
    @assign isStructural = false
  elseif evalAllParams || getEvaluateAnnotation(component)
    if ! getFixedAttribute(component)
      @assign isStructural = false
    elseif isExternalObject(component)
      @assign isStructural = false
    elseif ! hasBinding(compNode)
      if ! evalAllParams && ! Flags.getConfigBool(Flags.CHECK_MODEL)
        Error.addSourceMessage(Error.UNBOUND_PARAMETER_EVALUATE_TRUE, list(name(compNode)), info(compNode))
      end
      @assign isStructural = false
    elseif isBindingNotFixed(compBinding, requireFinal = false)
      @assign isStructural = false
    else
      @assign isStructural = true
    end
  else
    @assign isStructural = false
  end
  isStructural
end

function isBindingNotFixed(binding::Binding, requireFinal::Bool, maxDepth::Int = 4) ::Bool
  local isNotFixed::Bool

  if maxDepth == 0
    @assign isNotFixed = true
    return isNotFixed
  end
  if hasExp(binding)
    @assign isNotFixed = isExpressionNotFixed(getBindingExp(getExp(binding)), requireFinal = requireFinal, maxDepth = maxDepth)
  else
    @assign isNotFixed = true
  end
  isNotFixed
end

function isComponentBindingNotFixed(component::Component, node::InstNode, requireFinal::Bool, maxDepth::Int, isRecord::Bool = false) ::Bool
  local isNotFixed::Bool

  local binding::Binding
  local parent::InstNode

  @assign binding = getBinding(component)
  if isUnbound(binding)
    if isRecord || isRecord(node)
      @assign isNotFixed = false
    else
      @assign parent = parent(node)
      if isComponent(parent) && isRecord(parent)
        @assign isNotFixed = isComponentBindingNotFixed(component(parent), parent, requireFinal, maxDepth, true)
      else
        @assign isNotFixed = true
      end
    end
  else
    @assign isNotFixed = isBindingNotFixed(binding, requireFinal, maxDepth)
  end
  #=  TODO: Check whether the record fields have bindings or not.
  =#
  isNotFixed
end

function isExpressionNotFixed(exp::Expression; requireFinal::Bool = false, maxDepth::Int = 4) ::Bool
  local isNotFixed::Bool

  @assign isNotFixed = begin
    local nodeVar::InstNode
    local c::Component
    local var::VariabilityType
    local e::Expression
    @match exp begin
      CREF_EXPRESSION(__)  => begin
        @assign nodeVar = node(exp.cref)
        if isComponent(nodeVar)
          @assign c = component(nodeVar)
          @assign var = variability(c)
          if var <= Variability.STRUCTURAL_PARAMETER
            @assign isNotFixed = false
          elseif var == Variability.PARAMETER && (! requireFinal || isFinal(c)) && ! isExternalObject(c) && getFixedAttribute(c)
            @assign isNotFixed = isComponentBindingNotFixed(c, nodeVar, requireFinal, maxDepth - 1)
          else
            @assign isNotFixed = true
          end
        else
          @assign isNotFixed = true
        end
        isNotFixed || containsShallow(exp, (requireFinal, maxDepth) -> isExpressionNotFixed(requireFinal = requireFinal, maxDepth = maxDepth))
      end

      SIZE_EXPRESSION(__)  => begin
        if isSome(exp.dimIndex)
          @assign isNotFixed = isExpressionNotFixed(Util.getOption(exp.dimIndex), requireFinal, maxDepth)
        else
          @assign isNotFixed = false
        end
        isNotFixed
      end

      CALL_EXPRESSION(__)  => begin
        if P_Call.isImpure(exp.call) || P_Call.isExternal(exp.call)
          @assign isNotFixed = true
        else
          @assign isNotFixed = containsShallow(exp, (requireFinal, maxDepth) -> isExpressionNotFixed(requireFinal = requireFinal, maxDepth = maxDepth))
        end
        isNotFixed
      end

      _  => begin
        containsShallow(exp, (exp) -> isExpressionNotFixed(exp))
      end
    end
  end
  isNotFixed
end

function getRecordFieldBinding(comp::Component, node::InstNode) ::Binding
  local binding::Binding

  local parent::InstNode

  @assign binding = getBinding(comp)
  if isUnbound(binding)
    @assign parent = parent(node)
    if isComponent(parent) && isRecord(restriction(getClass(parent)))
      @assign binding = getRecordFieldBinding(component(parent), parent)
    end
  end
  binding
end

function markStructuralParamsDim(dimension::Dimension)
  @assign () = begin
    @match dimension begin
      DIMENSION_UNTYPED(__)  => begin
        markStructuralParamsExp(dimension.dimension)
        ()
      end

      DIMENSION_EXP(__)  => begin
        markStructuralParamsExp(dimension.exp)
        ()
      end

      _  => begin
        ()
      end
    end
  end
end

function markStructuralParamsExp(exp::Expression)
  apply(exp, markStructuralParamsExp_traverser)
end

function markStructuralParamsExp_traverser(exp::Expression)
  @assign () = begin
    local node::InstNode
    local comp::Component
    local binding::Option{Expression}
    @match exp begin
      CREF_EXPRESSION(cref = COMPONENT_REF_CREF(node = node, origin = Origin.CREF))  => begin
        if isComponent(node)
          @assign comp = component(node)
          if variability(comp) == Variability.PARAMETER
            markStructuralParamsComp(comp, node)
          end
        end
        ()
      end

      _  => begin
        ()
      end
    end
  end
end

function markStructuralParamsComp(component::Component, node::InstNode)
  local comp::Component
  local binding::Option{Expression}
  @assign comp = setVariability(Variability.STRUCTURAL_PARAMETER, component)
  updateComponent!(comp, node)
  @assign binding = untypedExp(getBinding(comp))
  if isSome(binding)
    markStructuralParamsExp(Util.getOption(binding))
  end
end

function markStructuralParamsExpSize(exp::Expression)
  apply(exp, markStructuralParamsExpSize_traverser)
end

function markStructuralParamsExpSize_traverser(exp::Expression)
  @assign () = begin
    local iters::List{Tuple{InstNode, Expression}}
    @match exp begin
      CALL_EXPRESSION(call = UNTYPED_ARRAY_CONSTRUCTOR(iters = iters))  => begin
        for iter in iters
          markStructuralParamsExp(Util.tuple22(iter))
        end
        ()
      end

      _  => begin
        ()
      end
    end
  end
end

function updateImplicitVariabilityEql(eql::List{<:Equation}, inWhen::Bool = false)
  for eq in eql
    updateImplicitVariabilityEq(eq, inWhen)
  end
end

function updateImplicitVariabilityEq(eq::Equation, inWhen::Bool = false)
  @assign () = begin
    local exp::Expression
    local eql::List{Equation}
    @match eq begin
      EQUATION_EQUALITY(__)  => begin
        if inWhen
          markImplicitWhenExp(eq.lhs)
        end
        ()
      end

      EQUATION_CONNECT(__)  => begin
        fold(eq.lhs, markStructuralParamsSubs, 0)
        fold(eq.rhs, markStructuralParamsSubs, 0)
        ()
      end

      EQUATION_FOR(__)  => begin
        updateImplicitVariabilityEql(eq.body, inWhen)
        ()
      end

      EQUATION_IF(__)  => begin
        for branch in eq.branches
          @assign () = begin
            @match branch begin
                EQUATION_BRANCH(__)  => begin
                  updateImplicitVariabilityEql(branch.body, inWhen)
                ()
              end
            end
          end
        end
        ()
      end

      EQUATION_WHEN(__)  => begin
        for branch in eq.branches
          @assign () = begin
            @match branch begin
                EQUATION_BRANCH(__)  => begin
                updateImplicitVariabilityEql(branch.body, #= inWhen =# true)
                ()
              end
            end
          end
        end
        ()
      end

      _  => begin
        ()
      end
    end
  end
end


function markStructuralParamsSub(sub::Subscript, dummy::Int = 0) ::Int
  @assign () = begin
    @match sub begin
      SUBSCRIPT_UNTYPED(__)  => begin
        markStructuralParamsExp(sub.exp)
        ()
      end
      SUBSCRIPT_INDEX(__)  => begin
        markStructuralParamsExp(sub.index)
        ()
      end
      SUBSCRIPT_SLICE(__)  => begin
        markStructuralParamsExp(sub.slice)
        ()
      end
      _  => begin
        ()
      end
    end
  end
  dummy
end

function markImplicitWhenExp(exp::Expression)
    apply(exp, markImplicitWhenExp_traverser)
end

function markImplicitWhenExp_traverser(exp::Expression)
  @assign () = begin
    local node::InstNode
    local comp::Component
    @match exp begin
      CREF_EXPRESSION(cref = COMPONENT_REF_CREF(node = node))  => begin
        if isComponent(node)
          @assign comp = component(node)
          if variability(comp) == Variability.CONTINUOUS
            @assign comp = setVariability(Variability.IMPLICITLY_DISCRETE, comp)
            updateComponent!(comp, node)
          end
        end
        ()
      end
      _  => ()
    end
  end
end
