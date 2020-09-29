module NFBuiltin

using MetaModelica
using MetaModelica.Dangerous
using ExportAll

import ..SCode
import Absyn
import ..AbsynUtil

module Elements
using MetaModelica
using ExportAll
import Absyn
import ..SCode
import ..AbsynUtil
#=  Default parts of the declarations for builtin elements and types:
=#

const ENUMTYPE_SPEC = Absyn.TPATH(Absyn.IDENT("EnumType"), NONE())::Absyn.TypeSpec
#=  StateSelect-specific elements: =#
const REAL =
  SCode.CLASS(
    "Real",
    SCode.defaultPrefixes,
    SCode.NOT_ENCAPSULATED(),
    SCode.NOT_PARTIAL(),
    SCode.R_TYPE(),
    SCode.PARTS(nil, nil, nil, nil, nil, nil, nil, NONE()),
    SCode.noComment,
    AbsynUtil.dummyInfo,
  )::SCode.Element
const INTEGER =
  SCode.CLASS(
    "Integer",
    SCode.defaultPrefixes,
    SCode.NOT_ENCAPSULATED(),
    SCode.NOT_PARTIAL(),
    SCode.R_TYPE(),
    SCode.PARTS(nil, nil, nil, nil, nil, nil, nil, NONE()),
    SCode.noComment,
    AbsynUtil.dummyInfo,
  )::SCode.Element
const BOOLEAN =
  SCode.CLASS(
    "Boolean",
    SCode.defaultPrefixes,
    SCode.NOT_ENCAPSULATED(),
    SCode.NOT_PARTIAL(),
    SCode.R_TYPE(),
    SCode.PARTS(nil, nil, nil, nil, nil, nil, nil, NONE()),
    SCode.noComment,
    AbsynUtil.dummyInfo,
  )::SCode.Element
const STRING =
  SCode.CLASS(
    "String",
    SCode.defaultPrefixes,
    SCode.NOT_ENCAPSULATED(),
    SCode.NOT_PARTIAL(),
    SCode.R_TYPE(),
    SCode.PARTS(nil, nil, nil, nil, nil, nil, nil, NONE()),
    SCode.noComment,
    AbsynUtil.dummyInfo,
  )::SCode.Element
const ENUMERATION =
  SCode.CLASS(
    "enumeration",
    SCode.defaultPrefixes,
    SCode.NOT_ENCAPSULATED(),
    SCode.NOT_PARTIAL(),
    SCode.R_TYPE(),
    SCode.PARTS(nil, nil, nil, nil, nil, nil, nil, NONE()),
    SCode.noComment,
    AbsynUtil.dummyInfo,
  )::SCode.Element
const ANY =
  SCode.CLASS(
    "polymorphic",
    SCode.defaultPrefixes,
    SCode.NOT_ENCAPSULATED(),
    SCode.NOT_PARTIAL(),
    SCode.R_TYPE(),
    SCode.PARTS(nil, nil, nil, nil, nil, nil, nil, NONE()),
    SCode.noComment,
    AbsynUtil.dummyInfo,
  )::SCode.Element
const CLOCK =
  SCode.CLASS(
    "Clock",
    SCode.defaultPrefixes,
    SCode.NOT_ENCAPSULATED(),
    SCode.NOT_PARTIAL(),
    SCode.R_PREDEFINED_CLOCK(),
    SCode.PARTS(nil, nil, nil, nil, nil, nil, nil, NONE()),
    SCode.noComment,
    AbsynUtil.dummyInfo,
  )::SCode.Element #= the Clock type =#

@exportAll()
end

import ..Main.C_FUNCTION
import ..Main.Visibility
import ..Main.VisibilityType
import ..P_Pointer
import ..Main.DEFAULT_PREFIXES
import ..Main.TYPE_POLYMORPHIC
import ..Main.CLASS_TREE_EMPTY_TREE
import ..Main.MODIFIER_NOMOD
import ..Main.RESTRICTION_TYPE
import ..Main.EMPTY_NODE_CACHE
import ..Main.EMPTY_NODE
import ..Main.BUILTIN_CLASS
import ..Main.PARTIAL_BUILTIN
import ..Main.CLASS_NODE
import ..Main.RESTRICTION_TYPE
import ..Main.EMPTY_NODE_CACHE
#= Types =#
import ..Main.TYPE_STRING
import ..Main.TYPE_ATTRIBUTE
import ..Main.TYPE_ENUMERATION
import ..Main.TYPE_INTEGER
import ..Main.TYPE_REAL
import ..Main.TYPE_BOOLEAN
#==#
import ..LookupTree
import ..InstNode

import ..Main.NORMAL_COMP
import ..Main.COMPONENT_NODE
import ..Main.STATESELECT_TYPE

import ..DuplicateTree

import ..Main.CLASS_TREE_FLAT_TREE
import ..Main.ClassTree

import ..Main.Origin
import ..Main.COMPONENT_REF_EMPTY
import ..Main.COMPONENT_REF_CREF
import ..Main.ComponentRef
import ..Main.TYPE_ENUMERATION_ANY
import ..Main.RESTRICTION_ENUMERATION
import ..Main.ENUM_LITERAL_EXPRESSION
import ..Main.Expression
import ..Main.TYPE_CLOCK
import ..Main.RESTRICTION_CLOCK
import ..Main.TYPED_COMPONENT
import ..Main.EMPTY_BINDING
import ..Main.INPUT_ATTR

const EMPTY_NODE_CACHE = listArrayLiteral(list(C_FUNCTION(nil, true, true)))::Array
#=  InstNodes for the builtin types. These have empty class trees to prevent
=#
#=  access to the attributes via dot notation (which is not needed for
=#
#=  modifiers and illegal in other cases).
=#
const POLYMORPHIC_NODE =
  CLASS_NODE(
    "polymorphic",
    Elements.ANY,
    Visibility.PUBLIC,
    P_Pointer.createImmutable(PARTIAL_BUILTIN(
      TYPE_POLYMORPHIC(""),
      CLASS_TREE_EMPTY_TREE(),
      MODIFIER_NOMOD(),
      DEFAULT_PREFIXES,
      RESTRICTION_TYPE(),
    )),
    EMPTY_NODE_CACHE,
    EMPTY_NODE(),
    BUILTIN_CLASS(),
  )::InstNode
#=  Lookup tree for Real. Generated by makeBuiltinLookupTree.
=#
const REAL_LOOKUP_TREE =
  LookupTree.NODE(
    "quantity",
     LookupTree.COMPONENT(1),
     4,
    LookupTree.NODE(
      "max",
       LookupTree.COMPONENT(5),
       3,
      LookupTree.NODE(
        "displayUnit",
         LookupTree.COMPONENT(3),
         2,
        LookupTree.EMPTY(),
         LookupTree.LEAF(
          "fixed",
           LookupTree.COMPONENT(7),
        ),
      ),
       LookupTree.NODE(
        "min",
         LookupTree.COMPONENT(4),
         2,
        LookupTree.EMPTY(),
         LookupTree.LEAF(
          "nominal",
           LookupTree.COMPONENT(8),
        ),
      ),
    ),
     LookupTree.NODE(
      "unbounded",
       LookupTree.COMPONENT(9),
       3,
      LookupTree.NODE(
        "start",
         LookupTree.COMPONENT(6),
         2,
        LookupTree.EMPTY(),
         LookupTree.LEAF(
          "stateSelect",
           LookupTree.COMPONENT(10),
        ),
      ),
       LookupTree.LEAF(
        "unit",
         LookupTree.COMPONENT(2),
      ),
    ),
  )::LookupTree.Tree



const STATESELECT_TYPE =
  TYPE_ENUMERATION(
    Absyn.IDENT("StateSelect"),
    list("never", "avoid", "default", "prefer", "always"),
  )

const REAL_CLASS_TREE =
  CLASS_TREE_FLAT_TREE(
    REAL_LOOKUP_TREE,
    listArrayLiteral(nil),
    listArrayLiteral(list(
      COMPONENT_NODE(
        "quantity",
        Visibility.PUBLIC,
        P_Pointer.createImmutable(TYPE_ATTRIBUTE(
          TYPE_STRING(),
          MODIFIER_NOMOD(),
        )),
        EMPTY_NODE(),
        NORMAL_COMP(),
      ),
      COMPONENT_NODE(
        "unit",
        Visibility.PUBLIC,
        P_Pointer.createImmutable(TYPE_ATTRIBUTE(
          TYPE_STRING(),
          MODIFIER_NOMOD(),
        )),
        EMPTY_NODE(),
        NORMAL_COMP(),
      ),
      COMPONENT_NODE(
        "displayUnit",
        Visibility.PUBLIC,
        P_Pointer.createImmutable(TYPE_ATTRIBUTE(
          TYPE_STRING(),
          MODIFIER_NOMOD(),
        )),
        EMPTY_NODE(),
        NORMAL_COMP(),
      ),
      COMPONENT_NODE(
        "min",
        Visibility.PUBLIC,
        P_Pointer.createImmutable(TYPE_ATTRIBUTE(
          TYPE_REAL(),
          MODIFIER_NOMOD(),
        )),
        EMPTY_NODE(),
        NORMAL_COMP(),
      ),
      COMPONENT_NODE(
        "max",
        Visibility.PUBLIC,
        P_Pointer.createImmutable(TYPE_ATTRIBUTE(
          TYPE_REAL(),
          MODIFIER_NOMOD(),
        )),
        EMPTY_NODE(),
        NORMAL_COMP(),
      ),
      COMPONENT_NODE(
        "start",
        Visibility.PUBLIC,
        P_Pointer.createImmutable(TYPE_ATTRIBUTE(
          TYPE_REAL(),
          MODIFIER_NOMOD(),
        )),
        EMPTY_NODE(),
        NORMAL_COMP(),
      ),
      COMPONENT_NODE(
        "fixed",
        Visibility.PUBLIC,
        P_Pointer.createImmutable(TYPE_ATTRIBUTE(
          TYPE_BOOLEAN(),
          MODIFIER_NOMOD(),
        )),
        EMPTY_NODE(),
        NORMAL_COMP(),
      ),
      COMPONENT_NODE(
        "nominal",
        Visibility.PUBLIC,
        P_Pointer.createImmutable(TYPE_ATTRIBUTE(
          TYPE_REAL(),
          MODIFIER_NOMOD(),
        )),
        EMPTY_NODE(),
        NORMAL_COMP(),
      ),
      COMPONENT_NODE(
        "unbounded",
        Visibility.PUBLIC,
        P_Pointer.createImmutable(TYPE_ATTRIBUTE(
          TYPE_BOOLEAN(),
          MODIFIER_NOMOD(),
        )),
        EMPTY_NODE(),
        NORMAL_COMP(),
      ),
      COMPONENT_NODE(
        "stateSelect",
        Visibility.PUBLIC,
        P_Pointer.createImmutable(TYPE_ATTRIBUTE(
          STATESELECT_TYPE,
          MODIFIER_NOMOD(),
        )),
        EMPTY_NODE(),
        NORMAL_COMP(),
      ),
    )),
    listArray(nil),
    DuplicateTree.EMPTY(),
  )::ClassTree
#=  TODO: #4895: This should be listArrayLiteral too, but causes compilation issues.
=#
const REAL_NODE =
  CLASS_NODE(
    "Real",
    Elements.REAL,
    Visibility.PUBLIC,
    P_Pointer.createImmutable(PARTIAL_BUILTIN(
      TYPE_REAL(),
      REAL_CLASS_TREE,
      MODIFIER_NOMOD(),
      DEFAULT_PREFIXES,
      RESTRICTION_TYPE(),
    )),
    EMPTY_NODE_CACHE,
    EMPTY_NODE(),
    BUILTIN_CLASS(),
  )::InstNode
#=  Lookup tree for Integer. Generated by makeBuiltinLookupTree.
=#
const INTEGER_LOOKUP_TREE =
  LookupTree.NODE(
    "min",
     LookupTree.COMPONENT(2),
     3,
    LookupTree.NODE(
      "max",
       LookupTree.COMPONENT(3),
       2,
      LookupTree.LEAF(
        "fixed",
         LookupTree.COMPONENT(5),
      ),
       LookupTree.EMPTY(),
    ),
     LookupTree.NODE(
      "quantity",
       LookupTree.COMPONENT(1),
       2,
      LookupTree.EMPTY(),
       LookupTree.LEAF(
        "start",
         LookupTree.COMPONENT(4),
      ),
    ),
  )::LookupTree.Tree
const INTEGER_CLASS_TREE =
  CLASS_TREE_FLAT_TREE(
    INTEGER_LOOKUP_TREE,
    listArrayLiteral(nil),
    listArrayLiteral(list(
      COMPONENT_NODE(
        "quantity",
        Visibility.PUBLIC,
        P_Pointer.createImmutable(TYPE_ATTRIBUTE(
          TYPE_STRING(),
          MODIFIER_NOMOD(),
        )),
        EMPTY_NODE(),
        NORMAL_COMP(),
      ),
      COMPONENT_NODE(
        "min",
        Visibility.PUBLIC,
        P_Pointer.createImmutable(TYPE_ATTRIBUTE(
          TYPE_INTEGER(),
          MODIFIER_NOMOD(),
        )),
        EMPTY_NODE(),
        NORMAL_COMP(),
      ),
      COMPONENT_NODE(
        "max",
        Visibility.PUBLIC,
        P_Pointer.createImmutable(TYPE_ATTRIBUTE(
          TYPE_INTEGER(),
          MODIFIER_NOMOD(),
        )),
        EMPTY_NODE(),
        NORMAL_COMP(),
      ),
      COMPONENT_NODE(
        "start",
        Visibility.PUBLIC,
        P_Pointer.createImmutable(TYPE_ATTRIBUTE(
          TYPE_INTEGER(),
          MODIFIER_NOMOD(),
        )),
        EMPTY_NODE(),
        NORMAL_COMP(),
      ),
      COMPONENT_NODE(
        "fixed",
        Visibility.PUBLIC,
        P_Pointer.createImmutable(TYPE_ATTRIBUTE(
          TYPE_BOOLEAN(),
          MODIFIER_NOMOD(),
        )),
        EMPTY_NODE(),
        NORMAL_COMP(),
      ),
    )),
    listArray(nil),
    DuplicateTree.EMPTY(),
  )::ClassTree
#=  TODO: #4895: This should be listArrayLiteral too, but causes compilation issues.
=#
const INTEGER_NODE =
  CLASS_NODE(
    "Integer",
    Elements.INTEGER,
    Visibility.PUBLIC,
    P_Pointer.createImmutable(PARTIAL_BUILTIN(
      TYPE_INTEGER(),
      INTEGER_CLASS_TREE,
      MODIFIER_NOMOD(),
      DEFAULT_PREFIXES,
      RESTRICTION_TYPE(),
    )),
    EMPTY_NODE_CACHE,
    EMPTY_NODE(),
    BUILTIN_CLASS(),
  )::InstNode
#=  Lookup tree for Boolean. Generated by makeBuiltinLookupTree. =#
const BOOLEAN_LOOKUP_TREE =
  LookupTree.NODE(
    "quantity",
     LookupTree.COMPONENT(1),
     2,
    LookupTree.LEAF(
      "fixed",
       LookupTree.COMPONENT(3),
    ),
     LookupTree.LEAF(
      "start",
       LookupTree.COMPONENT(2),
    ),
  )::LookupTree.Tree
const BOOLEAN_CLASS_TREE =
  CLASS_TREE_FLAT_TREE(
    BOOLEAN_LOOKUP_TREE,
    listArrayLiteral(nil),
    listArrayLiteral(list(
      COMPONENT_NODE(
        "quantity",
        Visibility.PUBLIC,
        P_Pointer.createImmutable(TYPE_ATTRIBUTE(
          TYPE_STRING(),
          MODIFIER_NOMOD(),
        )),
        EMPTY_NODE(),
        NORMAL_COMP(),
      ),
      COMPONENT_NODE(
        "start",
        Visibility.PUBLIC,
        P_Pointer.createImmutable(TYPE_ATTRIBUTE(
          TYPE_BOOLEAN(),
          MODIFIER_NOMOD(),
        )),
        EMPTY_NODE(),
        NORMAL_COMP(),
      ),
      COMPONENT_NODE(
        "fixed",
        Visibility.PUBLIC,
        P_Pointer.createImmutable(TYPE_ATTRIBUTE(
          TYPE_BOOLEAN(),
          MODIFIER_NOMOD(),
        )),
        EMPTY_NODE(),
        NORMAL_COMP(),
      ),
    )),
    listArray(nil),
    DuplicateTree.EMPTY(),
  )::ClassTree


#=  TODO: #4895: This should be listArrayLiteral too, but causes compilation issues.
=#
const BOOLEAN_NODE =
  CLASS_NODE(
    "Boolean",
    Elements.BOOLEAN,
    Visibility.PUBLIC,
    P_Pointer.createImmutable(PARTIAL_BUILTIN(
      TYPE_BOOLEAN(),
      BOOLEAN_CLASS_TREE,
      MODIFIER_NOMOD(),
      DEFAULT_PREFIXES,
      RESTRICTION_TYPE(),
    )),
    EMPTY_NODE_CACHE,
    EMPTY_NODE(),
    BUILTIN_CLASS(),
  )::InstNode

const BOOLEAN_CREF =
  COMPONENT_REF_CREF(
    BOOLEAN_NODE,
    nil,
    TYPE_INTEGER(),
    Origin.CREF,
    COMPONENT_REF_EMPTY(),
  )::ComponentRef
#=  Lookup tree for String. Generated by makeBuiltinLookupTree.
=#
const STRING_LOOKUP_TREE =
  LookupTree.NODE(
    "quantity",
     LookupTree.COMPONENT(1),
     2,
    LookupTree.LEAF(
      "fixed",
       LookupTree.COMPONENT(3),
    ),
     LookupTree.LEAF(
      "start",
       LookupTree.COMPONENT(2),
    ),
  )::LookupTree.Tree
const STRING_CLASS_TREE =
  CLASS_TREE_FLAT_TREE(
    STRING_LOOKUP_TREE,
    listArrayLiteral(nil),
    listArrayLiteral(list(
      COMPONENT_NODE(
        "quantity",
        Visibility.PUBLIC,
        P_Pointer.createImmutable(TYPE_ATTRIBUTE(
          TYPE_STRING(),
          MODIFIER_NOMOD(),
        )),
        EMPTY_NODE(),
        NORMAL_COMP(),
      ),
      COMPONENT_NODE(
        "start",
        Visibility.PUBLIC,
        P_Pointer.createImmutable(TYPE_ATTRIBUTE(
          TYPE_STRING(),
          MODIFIER_NOMOD(),
        )),
        EMPTY_NODE(),
        NORMAL_COMP(),
      ),
      COMPONENT_NODE(
        "fixed",
        Visibility.PUBLIC,
        P_Pointer.createImmutable(TYPE_ATTRIBUTE(
          TYPE_BOOLEAN(),
          MODIFIER_NOMOD(),
        )),
        EMPTY_NODE(),
        NORMAL_COMP(),
      ),
    )),
    listArray(nil),
    DuplicateTree.EMPTY(),
  )::ClassTree
#=  TODO: #4895: This should be listArrayLiteral too, but causes compilation issues. =#
const STRING_NODE =
  CLASS_NODE(
    "String",
    Elements.STRING,
    Visibility.PUBLIC,
    P_Pointer.createImmutable(PARTIAL_BUILTIN(
      TYPE_STRING(),
      STRING_CLASS_TREE,
      MODIFIER_NOMOD(),
      DEFAULT_PREFIXES,
      RESTRICTION_TYPE(),
    )),
    EMPTY_NODE_CACHE,
    EMPTY_NODE(),
    BUILTIN_CLASS(),
  )::InstNode
#=  Lookup tree for enumerations. Generated by makeBuiltinLookupTree.
=#
#=  NOTE: The enumeration attributes themselves are created by ClassTree.fromEnumeration,
=#
#=        so any changes to this lookup tree requires fromEnumeration to be updated too.
=#
const ENUM_LOOKUP_TREE =
  LookupTree.NODE(
    "min",
     LookupTree.COMPONENT(2),
     3,
    LookupTree.NODE(
      "max",
       LookupTree.COMPONENT(3),
       2,
      LookupTree.LEAF(
        "fixed",
         LookupTree.COMPONENT(5),
      ),
       LookupTree.EMPTY(),
    ),
     LookupTree.NODE(
      "quantity",
       LookupTree.COMPONENT(1),
       2,
      LookupTree.EMPTY(),
       LookupTree.LEAF(
        "start",
         LookupTree.COMPONENT(4),
      ),
    ),
  )::LookupTree.Tree

const ENUM_NODE =
  CLASS_NODE(
    "enumeration",
    Elements.ENUMERATION,
    Visibility.PUBLIC,
    P_Pointer.createImmutable(PARTIAL_BUILTIN(
      TYPE_ENUMERATION_ANY(),
      CLASS_TREE_EMPTY_TREE(),
      MODIFIER_NOMOD(),
      DEFAULT_PREFIXES,
      RESTRICTION_ENUMERATION(),
    )),
    EMPTY_NODE_CACHE,
    EMPTY_NODE(),
    BUILTIN_CLASS(),
  )::InstNode

const ASSERTIONLEVEL_TYPE =
  TYPE_ENUMERATION(Absyn.IDENT("AssertionLevel"),
                   list("error", "warning"))

const ASSERTIONLEVEL_ERROR =
  ENUM_LITERAL_EXPRESSION(ASSERTIONLEVEL_TYPE
                          , "error"
                          , 1)::Expression

const ASSERTIONLEVEL_WARNING =
  ENUM_LITERAL_EXPRESSION(ASSERTIONLEVEL_TYPE
                          , "error"
                          , 2)::Expression

const CLOCK_LOOKUP_TREE =
  LookupTree.NODE(
    "quantity",
     LookupTree.COMPONENT(1),
     2,
    LookupTree.LEAF(
      "fixed",
       LookupTree.COMPONENT(3),
    ),
     LookupTree.LEAF(
      "start",
       LookupTree.COMPONENT(2),
    ),
  )::LookupTree.Tree
const CLOCK_CLASS_TREE =
  CLASS_TREE_FLAT_TREE(
    CLOCK_LOOKUP_TREE,
    listArrayLiteral(nil),
    listArrayLiteral(list(
      COMPONENT_NODE(
        "quantity",
        Visibility.PUBLIC,
        P_Pointer.createImmutable(TYPE_ATTRIBUTE(
          TYPE_STRING(),
          MODIFIER_NOMOD(),
        )),
        EMPTY_NODE(),
        NORMAL_COMP(),
      ),
      COMPONENT_NODE(
        "start",
        Visibility.PUBLIC,
        P_Pointer.createImmutable(TYPE_ATTRIBUTE(
          TYPE_CLOCK(),
          MODIFIER_NOMOD(),
        )),
        EMPTY_NODE(),
        NORMAL_COMP(),
      ),
      COMPONENT_NODE(
        "fixed",
        Visibility.PUBLIC,
        P_Pointer.createImmutable(TYPE_ATTRIBUTE(
          TYPE_CLOCK(),
          MODIFIER_NOMOD(),
        )),
        EMPTY_NODE(),
        NORMAL_COMP(),
      ),
    )),
    listArray(nil),
    DuplicateTree.EMPTY(),
  )::ClassTree
#=  TODO: #4895: This should be listArrayLiteral too, but causes compilation issues.
=#
const CLOCK_NODE =
  CLASS_NODE(
    "Clock",
    Elements.CLOCK,
    Visibility.PUBLIC,
    P_Pointer.createImmutable(PARTIAL_BUILTIN(
      TYPE_CLOCK(),
      CLOCK_CLASS_TREE,
      MODIFIER_NOMOD(),
      DEFAULT_PREFIXES,
      RESTRICTION_CLOCK(),
    )),
    EMPTY_NODE_CACHE,
    EMPTY_NODE(),
    BUILTIN_CLASS(),
  )::InstNode
const CLOCK_CREF =
  COMPONENT_REF_CREF(
    CLOCK_NODE,
    nil,
    TYPE_CLOCK(),
    Origin.CREF,
    COMPONENT_REF_EMPTY(),
  )::ComponentRef
const TIME =
  COMPONENT_NODE(
    "time",
    Visibility.PUBLIC,
    P_Pointer.createImmutable(TYPED_COMPONENT(
      REAL_NODE,
      TYPE_REAL(),
      EMPTY_BINDING,
      EMPTY_BINDING,
      INPUT_ATTR,
      NONE(),
      NONE(),
      AbsynUtil.dummyInfo,
    )),
    EMPTY_NODE(),
    NORMAL_COMP(),
  )::InstNode
const TIME_CREF =
  COMPONENT_REF_CREF(
    TIME,
    nil,
    TYPE_REAL(),
    Origin.CREF,
    COMPONENT_REF_EMPTY(),
  )::ComponentRef

""" #= This function takes lists of component and class names and prints out a lookup tree.
   Useful in case any attributes needs to be added to any of the builtin type. =#"""
function makeBuiltinLookupTree(
  name::String,
  components::List{<:String},
  classes::List{<:String} = nil,
) #= Not used in the tree, only to identify the printout. =#
  local ltree::LookupTree.= LookupTree.new()
  local i::Integer
  @assign i = 1
  for comp in components
    @assign ltree = LookupTree.add(ltree, comp, LookupTree.COMPONENT(i))
    @assign i = i + 1
  end
  for cls in classes
    @assign ltree = LookupTree.add(ltree, cls, LookupTree.COMPONENT(i))
    @assign i = i + 1
  end
  print("Lookup tree for " + name + ":\\n")
  print(anyString(ltree))
  return print("\\n")
end

@exportAll()
end
