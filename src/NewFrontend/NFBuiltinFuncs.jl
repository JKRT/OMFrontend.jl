module NFBuiltinFuncs

using MetaModelica
using ExportAll

#= /*
* This file is part of OpenModelica.
*
* Copyright (c) 1998-2014, Open Source Modelica Consortium (OSMC),
* c/o Linköpings universitet, Department of Computer and Information Science,
* SE-58183 Linköping, Sweden.
*
* All rights reserved.
*
* THIS PROGRAM IS PROVIDED UNDER THE TERMS OF GPL VERSION 3 LICENSE OR
* THIS OSMC PUBLIC LICENSE (OSMC-PL) VERSION 1.2.
* ANY USE, REPRODUCTION OR DISTRIBUTION OF THIS PROGRAM CONSTITUTES
* RECIPIENT'S ACCEPTANCE OF THE OSMC PUBLIC LICENSE OR THE GPL VERSION 3,
* ACCORDING TO RECIPIENTS CHOICE.
*
* The OpenModelica software and the Open Source Modelica
* Consortium (OSMC) Public License (OSMC-PL) are obtained
* from OSMC, either from the above address,
* from the URLs: http:www.ida.liu.se/projects/OpenModelica or
* http:www.openmodelica.org, and in the OpenModelica distribution.
* GNU version 3 is obtained from: http:www.gnu.org/copyleft/gpl.html.
*
* This program is distributed WITHOUT ANY WARRANTY; without
* even the implied warranty of  MERCHANTABILITY or FITNESS
* FOR A PARTICULAR PURPOSE, EXCEPT AS EXPRESSLY SET FORTH
* IN THE BY RECIPIENT SELECTED SUBSIDIARY LICENSE CONDITIONS OF OSMC-PL.
*
* See the full OSMC Public License conditions for more details.
*
*/ =#
import ..NFClass.P_Class
import ..NFClassTree.ree
import ..NFFunction.P_Function
import ..NFFunction.P_Slot
import ..NFFunction.SlotType
import ..NFFunction.FuncType
import ..NFInstNode.P_CachedData
import ..NFInstNode.P_InstNode
import ..NFInstNode.InstNodeType
import ..NFComponent.P_Component
import ..P_NFType
P_M_Type = P_NFType
M_Type = NFType
import ..P_NFExpression
P_Expression = P_NFExpression
Expression = P_NFExpression.NFExpression
import Absyn
import ..AbsynUtil
using Absyn: Path, TypeSpec
import ..SCode
using SCode: Mod, Comment
import DAE
import ..NFBuiltin
Builtin = NFBuiltin
import ..NFBinding
import ..Pointer
import ..NFPrefixes.Visibility
import ..P_NFRestriction
P_Restriction = P_NFRestriction
Restriction = P_NFRestriction.NFRestriction
import ..P_NFComponentRef
P_ComponentRef = P_NFComponentRef
ComponentRef = P_NFComponentRef.NFComponentRef
import ..P_NFComponentRef.Origin
import ..NFModifier.P_Modifier
import ..P_NFSections
P_Sections = P_NFSections
Sections = P_NFSections.NFSections
import ..NFFunction.SlotEvalStatus
import ..NFFunction.FunctionStatus

using MetaModelica.Dangerous

const DUMMY_ELEMENT =
  SCode.CLASS(
    "DummyFunction",
    SCode.defaultPrefixes,
    SCode.Encapsulated.ENCAPSULATED(),
    SCode.Partial.NOT_PARTIAL(),
    SCode.RESTRICTION_R_FUNCTION(SCode.FunctionRestriction.FR_NORMAL_FUNCTION(false)),
    SCode.PARTS(nil, nil, nil, nil, nil, nil, nil, NONE()),
    SCode.COMMENT(NONE(), NONE()),
    AbsynUtil.dummyInfo,
  )::SCode.Element
#=  Default Integer parameter.
=#
const INT_COMPONENT =
  TYPED_COMPONENT(
    NFInstNode.EMPTY_NODE(),
    TYPE_INTEGER(),
    EMPTY_BINDING,
    EMPTY_BINDING,
    NFComponent.DEFAULT_ATTR,
    NONE(),
    NONE(),
    AbsynUtil.dummyInfo,
  )::Component
const INT_PARAM =
  COMPONENT_NODE(
    "i",
    Visibility.PUBLIC,
    P_Pointer.createImmutable(INT_COMPONENT),
    EMPTY_NODE(),
    NORMAL_COMP(),
  )::InstNode
#=  Default Real parameter.
=#
const REAL_COMPONENT =
  TYPED_COMPONENT(
    NFInstNode.EMPTY_NODE(),
    TYPE_REAL(),
    EMPTY_BINDING,
    EMPTY_BINDING,
    NFComponent.DEFAULT_ATTR,
    NONE(),
    NONE(),
    AbsynUtil.dummyInfo,
  )::Component
const REAL_PARAM =
  COMPONENT_NODE(
    "r",
    Visibility.PUBLIC,
    P_Pointer.createImmutable(REAL_COMPONENT),
    EMPTY_NODE(),
    NORMAL_COMP(),
  )::InstNode
#=  Default Boolean parameter.
=#
const BOOL_COMPONENT =
  TYPED_COMPONENT(
    NFInstNode.EMPTY_NODE(),
    TYPE_BOOLEAN(),
    EMPTY_BINDING,
    EMPTY_BINDING,
    NFComponent.DEFAULT_ATTR,
    NONE(),
    NONE(),
    AbsynUtil.dummyInfo,
  )::Component
const BOOL_PARAM =
  COMPONENT_NODE(
    "b",
    Visibility.PUBLIC,
    P_Pointer.createImmutable(BOOL_COMPONENT),
    EMPTY_NODE(),
    NORMAL_COMP(),
  )::InstNode
#=  Default String parameter.
=#
const STRING_COMPONENT =
  TYPED_COMPONENT(
    NFInstNode.EMPTY_NODE(),
    TYPE_STRING(),
    EMPTY_BINDING,
    EMPTY_BINDING,
    NFComponent.DEFAULT_ATTR,
    NONE(),
    NONE(),
    AbsynUtil.dummyInfo,
  )::Component
const STRING_PARAM =
  COMPONENT_NODE(
    "s",
    Visibility.PUBLIC,
    P_Pointer.createImmutable(STRING_COMPONENT),
    EMPTY_NODE(),
    NORMAL_COMP(),
  )::InstNode
#=  Default enumeration(:) parameter.
=#
const ENUM_COMPONENT =
  TYPED_COMPONENT(
    NFInstNode.EMPTY_NODE(),
    TYPE_ENUMERATION_ANY(),
    EMPTY_BINDING,
    EMPTY_BINDING,
    NFComponent.DEFAULT_ATTR,
    NONE(),
    NONE(),
    AbsynUtil.dummyInfo,
  )::Component
const ENUM_PARAM =
  COMPONENT_NODE(
    "e",
    Visibility.PUBLIC,
    P_Pointer.createImmutable(ENUM_COMPONENT),
    EMPTY_NODE(),
    NORMAL_COMP(),
  )::InstNode
#=  Integer(e)
=#
const EMPTY_NODE_CACHE =
  listArrayLiteral(list(
    NFInstNode.P_CachedData.NO_CACHE(),
    NFInstNode.P_CachedData.NO_CACHE(),
    NFInstNode.P_CachedData.NO_CACHE(),
  ))::Array
const INTEGER_DUMMY_NODE =
  NFInstNode.CLASS_NODE(
    "Integer",
    DUMMY_ELEMENT,
    Visibility.PUBLIC,
    P_Pointer.createImmutable(NOT_INSTANTIATED()),
    EMPTY_NODE_CACHE,
    EMPTY_NODE(),
    NORMAL_CLASS(),
  )::InstNode
const INTEGER_FUNCTION =
  P_Function.FUNCTION(
    Path.IDENT("Integer"),
    INTEGER_DUMMY_NODE,
    list(ENUM_PARAM),
    nil,
    nil,
    list(P_Slot.SLOT(
      "e",
      SlotType.POSITIONAL,
      NONE(),
      NONE(),
      1,
      SlotEvalStatus.NOT_EVALUATED,
    )),
    TYPE_INTEGER(),
    DAE.FUNCTION_ATTRIBUTES_BUILTIN,
    nil,
    P_Pointer.createImmutable(FunctionStatus.BUILTIN),
    P_Pointer.createImmutable(0),
  )::M_Function
const INTEGER_NODE =
  CLASS_NODE(
    "IntegerFunc",
    DUMMY_ELEMENT,
    Visibility.PUBLIC,
    P_Pointer.createImmutable(INSTANCED_CLASS(
      TYPE_UNKNOWN(),
      EMPTY_TREE(),
      SECTIONS_EMPTY(),
      RESTRICTION_FUNCTION(),
    )),
    listArrayLiteral(list(
      NFInstNode.C_FUNCTION(list(INTEGER_FUNCTION), true, false),
      NFInstNode.P_CachedData.NO_CACHE(),
      NFInstNode.P_CachedData.NO_CACHE(),
    )),
    EMPTY_NODE(),
    BUILTIN_CLASS(),
  )::InstNode
const INTEGER_CREF =
  CREF(
    INTEGER_NODE,
    nil,
    TYPE_INTEGER(),
    Origin.CREF,
    EMPTY(),
  )::ComponentRef
const STRING_DUMMY_NODE =
  NFInstNode.CLASS_NODE(
    "String",
    DUMMY_ELEMENT,
    Visibility.PUBLIC,
    P_Pointer.createImmutable(NOT_INSTANTIATED()),
    EMPTY_NODE_CACHE,
    EMPTY_NODE(),
    NORMAL_CLASS(),
  )::InstNode
#=  String(r, significantDigits=d, minimumLength=0, leftJustified=true)
=#
const STRING_REAL =
  P_Function.FUNCTION(
    Path.IDENT("String"),
    STRING_DUMMY_NODE,
    list(REAL_PARAM, INT_PARAM, INT_PARAM, BOOL_PARAM),
    list(STRING_PARAM),
    nil,
    list(
      P_Slot.SLOT(
        "r",
        SlotType.POSITIONAL,
        NONE(),
        NONE(),
        1,
        SlotEvalStatus.NOT_EVALUATED,
      ),
      P_Slot.SLOT(
        "significantDigits",
        SlotType.NAMED,
        SOME(INTEGER_EXPRESSION(6)),
        NONE(),
        2,
        SlotEvalStatus.NOT_EVALUATED,
      ),
      P_Slot.SLOT(
        "minimumLength",
        SlotType.NAMED,
        SOME(INTEGER_EXPRESSION(0)),
        NONE(),
        3,
        SlotEvalStatus.NOT_EVALUATED,
      ),
      P_Slot.SLOT(
        "leftJustified",
        SlotType.NAMED,
        SOME(P_Expression.BOOLEAN_EXPRESSION(true)),
        NONE(),
        4,
        SlotEvalStatus.NOT_EVALUATED,
      ),
    ),
    TYPE_STRING(),
    DAE.FUNCTION_ATTRIBUTES_BUILTIN,
    nil,
    P_Pointer.createImmutable(FunctionStatus.BUILTIN),
    P_Pointer.createImmutable(0),
  )::M_Function
#=  String(r, format=\"-0.6g\")
=#
const STRING_REAL_FORMAT =
  P_Function.FUNCTION(
    Path.IDENT("String"),
    STRING_DUMMY_NODE,
    list(REAL_PARAM, STRING_PARAM),
    list(STRING_PARAM),
    nil,
    list(
      P_Slot.SLOT(
        "r",
        SlotType.POSITIONAL,
        NONE(),
        NONE(),
        1,
        SlotEvalStatus.NOT_EVALUATED,
      ),
      P_Slot.SLOT(
        "format",
        SlotType.NAMED,
        NONE(),
        NONE(),
        2,
        SlotEvalStatus.NOT_EVALUATED,
      ),
    ),
    TYPE_STRING(),
    DAE.FUNCTION_ATTRIBUTES_BUILTIN,
    nil,
    P_Pointer.createImmutable(FunctionStatus.BUILTIN),
    P_Pointer.createImmutable(0),
  )::M_Function
#=  String(i, minimumLength=0, leftJustified=true)
=#
const STRING_INT =
  P_Function.FUNCTION(
    Path.IDENT("String"),
    STRING_DUMMY_NODE,
    list(INT_PARAM, INT_PARAM, BOOL_PARAM),
    list(STRING_PARAM),
    nil,
    list(
      P_Slot.SLOT(
        "i",
        SlotType.POSITIONAL,
        NONE(),
        NONE(),
        1,
        SlotEvalStatus.NOT_EVALUATED,
      ),
      P_Slot.SLOT(
        "minimumLength",
        SlotType.NAMED,
        SOME(INTEGER_EXPRESSION(0)),
        NONE(),
        2,
        SlotEvalStatus.NOT_EVALUATED,
      ),
      P_Slot.SLOT(
        "leftJustified",
        SlotType.NAMED,
        SOME(P_Expression.BOOLEAN_EXPRESSION(true)),
        NONE(),
        3,
        SlotEvalStatus.NOT_EVALUATED,
      ),
    ),
    TYPE_STRING(),
    DAE.FUNCTION_ATTRIBUTES_BUILTIN,
    nil,
    P_Pointer.createImmutable(FunctionStatus.BUILTIN),
    P_Pointer.createImmutable(0),
  )::M_Function
#=  String(b, minimumLength=0, leftJustified=true)
=#
const STRING_BOOL =
  P_Function.FUNCTION(
    Path.IDENT("String"),
    STRING_DUMMY_NODE,
    list(BOOL_PARAM, INT_PARAM, BOOL_PARAM),
    list(STRING_PARAM),
    nil,
    list(
      P_Slot.SLOT(
        "b",
        SlotType.POSITIONAL,
        NONE(),
        NONE(),
        1,
        SlotEvalStatus.NOT_EVALUATED,
      ),
      P_Slot.SLOT(
        "minimumLength",
        SlotType.NAMED,
        SOME(INTEGER_EXPRESSION(0)),
        NONE(),
        2,
        SlotEvalStatus.NOT_EVALUATED,
      ),
      P_Slot.SLOT(
        "leftJustified",
        SlotType.NAMED,
        SOME(P_Expression.BOOLEAN_EXPRESSION(true)),
        NONE(),
        3,
        SlotEvalStatus.NOT_EVALUATED,
      ),
    ),
    TYPE_STRING(),
    DAE.FUNCTION_ATTRIBUTES_BUILTIN,
    nil,
    P_Pointer.createImmutable(FunctionStatus.BUILTIN),
    P_Pointer.createImmutable(0),
  )::M_Function
#=  String(e, minimumLength=0, leftJustified=true)
=#
const STRING_ENUM =
  P_Function.FUNCTION(
    Path.IDENT("String"),
    STRING_DUMMY_NODE,
    list(ENUM_PARAM, INT_PARAM, BOOL_PARAM),
    list(STRING_PARAM),
    nil,
    list(
      P_Slot.SLOT(
        "e",
        SlotType.POSITIONAL,
        NONE(),
        NONE(),
        1,
        SlotEvalStatus.NOT_EVALUATED,
      ),
      P_Slot.SLOT(
        "minimumLength",
        SlotType.NAMED,
        SOME(INTEGER_EXPRESSION(0)),
        NONE(),
        2,
        SlotEvalStatus.NOT_EVALUATED,
      ),
      P_Slot.SLOT(
        "leftJustified",
        SlotType.NAMED,
        SOME(P_Expression.BOOLEAN_EXPRESSION(true)),
        NONE(),
        3,
        SlotEvalStatus.NOT_EVALUATED,
      ),
    ),
    TYPE_STRING(),
    DAE.FUNCTION_ATTRIBUTES_BUILTIN,
    nil,
    P_Pointer.createImmutable(FunctionStatus.BUILTIN),
    P_Pointer.createImmutable(0),
  )::M_Function
const STRING_NODE =
  CLASS_NODE(
    "String",
    DUMMY_ELEMENT,
    Visibility.PUBLIC,
    P_Pointer.createImmutable(PARTIAL_BUILTIN(
      TYPE_STRING(),
      EMPTY_TREE(),
      MODIFIER_NOMOD(),
      NFClass.DEFAULT_PREFIXES,
      RESTRICTION_TYPE(),
    )),
    listArrayLiteral(list(
      NFInstNode.C_FUNCTION(
        list(STRING_ENUM, STRING_INT, STRING_BOOL, STRING_REAL, STRING_REAL_FORMAT),
        true,
        true,
      ),
      NFInstNode.P_CachedData.NO_CACHE(),
      NFInstNode.P_CachedData.NO_CACHE(),
    )),
    EMPTY_NODE(),
    BUILTIN_CLASS(),
  )::InstNode
const STRING_CREF =
  CREF(
    STRING_NODE,
    nil,
    TYPE_INTEGER(),
    Origin.CREF,
    EMPTY(),
  )::ComponentRef
const ABS_REAL =
  P_Function.FUNCTION(
    Path.IDENT("abs"),
    EMPTY_NODE(),
    list(REAL_PARAM, REAL_PARAM),
    list(REAL_PARAM),
    nil,
    nil,
    TYPE_REAL(),
    DAE.FUNCTION_ATTRIBUTES_BUILTIN,
    nil,
    P_Pointer.createImmutable(FunctionStatus.BUILTIN),
    P_Pointer.createImmutable(0),
  )::M_Function
const MAX_INT =
  P_Function.FUNCTION(
    Path.IDENT("max"),
    EMPTY_NODE(),
    list(INT_PARAM, INT_PARAM),
    list(INT_PARAM),
    nil,
    nil,
    TYPE_INTEGER(),
    DAE.FUNCTION_ATTRIBUTES_BUILTIN,
    nil,
    P_Pointer.createImmutable(FunctionStatus.BUILTIN),
    P_Pointer.createImmutable(0),
  )::M_Function
const MAX_REAL =
  P_Function.FUNCTION(
    Path.IDENT("max"),
    EMPTY_NODE(),
    list(REAL_PARAM, REAL_PARAM),
    list(REAL_PARAM),
    nil,
    nil,
    TYPE_REAL(),
    DAE.FUNCTION_ATTRIBUTES_BUILTIN,
    nil,
    P_Pointer.createImmutable(FunctionStatus.BUILTIN),
    P_Pointer.createImmutable(0),
  )::M_Function
const DIV_INT =
  P_Function.FUNCTION(
    Path.IDENT("div"),
    EMPTY_NODE(),
    list(INT_PARAM, INT_PARAM),
    list(INT_PARAM),
    nil,
    nil,
    TYPE_INTEGER(),
    DAE.FUNCTION_ATTRIBUTES_BUILTIN,
    nil,
    P_Pointer.createImmutable(FunctionStatus.BUILTIN),
    P_Pointer.createImmutable(0),
  )::M_Function
const FLOOR =
  P_Function.FUNCTION(
    Path.IDENT("floor"),
    EMPTY_NODE(),
    list(REAL_PARAM),
    list(REAL_PARAM),
    nil,
    nil,
    TYPE_REAL(),
    DAE.FUNCTION_ATTRIBUTES_BUILTIN,
    nil,
    P_Pointer.createImmutable(FunctionStatus.BUILTIN),
    P_Pointer.createImmutable(0),
  )::M_Function
const INTEGER_REAL =
  P_Function.FUNCTION(
    Path.IDENT("integer"),
    EMPTY_NODE(),
    list(REAL_PARAM),
    list(INT_PARAM),
    nil,
    nil,
    TYPE_INTEGER(),
    DAE.FUNCTION_ATTRIBUTES_BUILTIN,
    nil,
    P_Pointer.createImmutable(FunctionStatus.BUILTIN),
    P_Pointer.createImmutable(0),
  )::M_Function
const INTEGER_ENUM =
  P_Function.FUNCTION(
    Path.IDENT("Integer"),
    EMPTY_NODE(),
    list(ENUM_PARAM),
    list(INT_PARAM),
    nil,
    nil,
    TYPE_INTEGER(),
    DAE.FUNCTION_ATTRIBUTES_BUILTIN,
    nil,
    P_Pointer.createImmutable(FunctionStatus.BUILTIN),
    P_Pointer.createImmutable(0),
  )::M_Function
const POSITIVE_MAX_REAL =
  P_Function.FUNCTION(
    Path.IDENT("OMCPositiveMax"),
    EMPTY_NODE(),
    list(REAL_PARAM, REAL_PARAM),
    list(REAL_PARAM),
    nil,
    nil,
    TYPE_REAL(),
    DAE.FUNCTION_ATTRIBUTES_BUILTIN,
    nil,
    P_Pointer.createImmutable(FunctionStatus.BUILTIN),
    P_Pointer.createImmutable(0),
  )::M_Function
const IN_STREAM =
  P_Function.FUNCTION(
    Path.IDENT("inStream"),
    EMPTY_NODE(),
    list(REAL_PARAM),
    list(REAL_PARAM),
    nil,
    nil,
    TYPE_REAL(),
    DAE.FUNCTION_ATTRIBUTES_BUILTIN,
    nil,
    P_Pointer.createImmutable(FunctionStatus.BUILTIN),
    P_Pointer.createImmutable(0),
  )::M_Function
const PROMOTE =
  P_Function.FUNCTION(
    Path.IDENT("promote"),
    EMPTY_NODE(),
    nil,
    nil,
    nil,
    nil,
    TYPE_UNKNOWN(),
    DAE.FUNCTION_ATTRIBUTES_BUILTIN,
    nil,
    P_Pointer.createImmutable(FunctionStatus.BUILTIN),
    P_Pointer.createImmutable(0),
  )::M_Function
const CAT =
  P_Function.FUNCTION(
    Path.IDENT("cat"),
    EMPTY_NODE(),
    nil,
    nil,
    nil,
    nil,
    TYPE_UNKNOWN(),
    DAE.FUNCTION_ATTRIBUTES_BUILTIN,
    nil,
    P_Pointer.createImmutable(FunctionStatus.BUILTIN),
    P_Pointer.createImmutable(0),
  )::M_Function
const ARRAY_FUNC =
  P_Function.FUNCTION(
    Path.IDENT("array"),
    EMPTY_NODE(),
    nil,
    nil,
    nil,
    nil,
    TYPE_UNKNOWN(),
    DAE.FUNCTION_ATTRIBUTES_BUILTIN,
    nil,
    P_Pointer.createImmutable(FunctionStatus.BUILTIN),
    P_Pointer.createImmutable(0),
  )::M_Function
const FILL_FUNC =
  P_Function.FUNCTION(
    Path.IDENT("fill"),
    EMPTY_NODE(),
    nil,
    nil,
    nil,
    nil,
    TYPE_UNKNOWN(),
    DAE.FUNCTION_ATTRIBUTES_BUILTIN,
    nil,
    P_Pointer.createImmutable(FunctionStatus.BUILTIN),
    P_Pointer.createImmutable(0),
  )::M_Function
const SMOOTH =
  P_Function.FUNCTION(
    Path.IDENT("smooth"),
    EMPTY_NODE(),
    nil,
    nil,
    nil,
    nil,
    TYPE_UNKNOWN(),
    DAE.FUNCTION_ATTRIBUTES_BUILTIN,
    nil,
    P_Pointer.createImmutable(FunctionStatus.BUILTIN),
    P_Pointer.createImmutable(0),
  )::M_Function
const CLOCK_COMPONENT =
  TYPED_COMPONENT(
    NFInstNode.EMPTY_NODE(),
    TYPE_CLOCK(),
    EMPTY_BINDING,
    EMPTY_BINDING,
    NFComponent.DEFAULT_ATTR,
    NONE(),
    NONE(),
    AbsynUtil.dummyInfo,
  )::Component
const CLOCK_PARAM =
  COMPONENT_NODE(
    "s",
    Visibility.PUBLIC,
    P_Pointer.createImmutable(CLOCK_COMPONENT),
    EMPTY_NODE(),
    NORMAL_COMP(),
  )::InstNode
const CLOCK_DUMMY_NODE =
  NFInstNode.CLASS_NODE(
    "Clock",
    DUMMY_ELEMENT,
    Visibility.PUBLIC,
    P_Pointer.createImmutable(NOT_INSTANTIATED()),
    EMPTY_NODE_CACHE,
    EMPTY_NODE(),
    NORMAL_CLASS(),
  )::InstNode
#=  Clock() - inferred clock
=#
const CLOCK_INFERED =
  P_Function.FUNCTION(
    Path.IDENT("Clock"),
    CLOCK_DUMMY_NODE,
    nil,
    list(CLOCK_PARAM),
    nil,
    nil,
    TYPE_CLOCK(),
    DAE.FUNCTION_ATTRIBUTES_BUILTIN,
    nil,
    P_Pointer.createImmutable(FunctionStatus.BUILTIN),
    P_Pointer.createImmutable(0),
  )::M_Function
#=  Clock(intervalCounter, resolution = 1) - clock with Integer interval
=#
const CLOCK_INT =
  P_Function.FUNCTION(
    Path.IDENT("Clock"),
    CLOCK_DUMMY_NODE,
    list(INT_PARAM, INT_PARAM),
    list(CLOCK_PARAM),
    nil,
    list(
      P_Slot.SLOT(
        "intervalCounter",
        SlotType.GENERIC,
        NONE(),
        NONE(),
        1,
        SlotEvalStatus.NOT_EVALUATED,
      ),
      P_Slot.SLOT(
        "resolution",
        SlotType.GENERIC,
        SOME(INTEGER_EXPRESSION(1)),
        NONE(),
        2,
        SlotEvalStatus.NOT_EVALUATED,
      ),
    ),
    TYPE_CLOCK(),
    DAE.FUNCTION_ATTRIBUTES_BUILTIN,
    nil,
    P_Pointer.createImmutable(FunctionStatus.BUILTIN),
    P_Pointer.createImmutable(0),
  )::M_Function
#=  Clock(interval) - clock with Real interval
=#
const CLOCK_REAL =
  P_Function.FUNCTION(
    Path.IDENT("Clock"),
    CLOCK_DUMMY_NODE,
    list(REAL_PARAM),
    list(CLOCK_PARAM),
    nil,
    list(P_Slot.SLOT(
      "interval",
      SlotType.GENERIC,
      NONE(),
      NONE(),
      1,
      SlotEvalStatus.NOT_EVALUATED,
    )),
    TYPE_CLOCK(),
    DAE.FUNCTION_ATTRIBUTES_BUILTIN,
    nil,
    P_Pointer.createImmutable(FunctionStatus.BUILTIN),
    P_Pointer.createImmutable(0),
  )::M_Function
#=  Clock(condition, startInterval = 0.0) - Boolean clock, triggered by zero-crossing events
=#
const CLOCK_BOOL =
  P_Function.FUNCTION(
    Path.IDENT("Clock"),
    CLOCK_DUMMY_NODE,
    list(BOOL_PARAM, REAL_PARAM),
    list(CLOCK_PARAM),
    nil,
    list(
      P_Slot.SLOT(
        "condition",
        SlotType.GENERIC,
        NONE(),
        NONE(),
        1,
        SlotEvalStatus.NOT_EVALUATED,
      ),
      P_Slot.SLOT(
        "startInterval",
        SlotType.GENERIC,
        SOME(P_Expression.REAL_EXPRESSION(0.0)),
        NONE(),
        2,
        SlotEvalStatus.NOT_EVALUATED,
      ),
    ),
    TYPE_CLOCK(),
    DAE.FUNCTION_ATTRIBUTES_BUILTIN,
    nil,
    P_Pointer.createImmutable(FunctionStatus.BUILTIN),
    P_Pointer.createImmutable(0),
  )::M_Function
#=  Clock(c, solverMethod) - Solver clock
=#
const CLOCK_SOLVER =
  P_Function.FUNCTION(
    Path.IDENT("Clock"),
    CLOCK_DUMMY_NODE,
    list(CLOCK_PARAM, STRING_PARAM),
    list(CLOCK_PARAM),
    nil,
    list(
      P_Slot.SLOT("c", SlotType.GENERIC, NONE(), NONE(), 1, SlotEvalStatus.NOT_EVALUATED),
      P_Slot.SLOT(
        "solverMethod",
        SlotType.GENERIC,
        NONE(),
        NONE(),
        2,
        SlotEvalStatus.NOT_EVALUATED,
      ),
    ),
    TYPE_CLOCK(),
    DAE.FUNCTION_ATTRIBUTES_BUILTIN,
    nil,
    P_Pointer.createImmutable(FunctionStatus.BUILTIN),
    P_Pointer.createImmutable(0),
  )::M_Function
const CLOCK_NODE =
  CLASS_NODE(
    "Clock",
    DUMMY_ELEMENT,
    Visibility.PUBLIC,
    P_Pointer.createImmutable(PARTIAL_BUILTIN(
      TYPE_CLOCK(),
      EMPTY_TREE(),
      MODIFIER_NOMOD(),
      NFClass.DEFAULT_PREFIXES,
      RESTRICTION_TYPE(),
    )),
    listArrayLiteral(list(
      NFInstNode.C_FUNCTION(
        list(CLOCK_INFERED, CLOCK_INT, CLOCK_REAL, CLOCK_BOOL, CLOCK_SOLVER),
        true,
        true,
      ),
      NFInstNode.P_CachedData.NO_CACHE(),
      NFInstNode.P_CachedData.NO_CACHE(),
    )),
    EMPTY_NODE(),
    BUILTIN_CLASS(),
  )::InstNode
const CLOCK_CREF =
  CREF(
    CLOCK_NODE,
    nil,
    TYPE_INTEGER(),
    Origin.CREF,
    EMPTY(),
  )::ComponentRef

@exportAll()
end
