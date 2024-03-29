module SCodeDump

using MetaModelica
using ExportAll
#= Forward declarations for uniontypes until Julia adds support for mutual recursion =#

@UniontypeDecl SCodeDumpOptions

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

import Absyn

import Main.AbsynUtil

import Main.SCode

import Main.Dump

import ListUtil

import Main.SCodeDumpTpl

import Main.Tpl

@Uniontype SCodeDumpOptions begin
  @Record OPTIONS begin

    stripAlgorithmSections::Bool
    stripProtectedImports::Bool
    stripProtectedClasses::Bool
    stripProtectedComponents::Bool
    stripMetaRecords::Bool #= The automatically generated records that change scope from uniontype to the package =#
    stripGraphicalAnnotations::Bool
    stripStringComments::Bool
    stripExternalDecl::Bool
    stripOutputBindings::Bool
  end
end

const defaultOptions =
  OPTIONS(false, false, false, false, true, true, false, false, false)::SCodeDumpOptions

function programStr(
  inProgram::SCode.Program,
  options::SCodeDumpOptions = defaultOptions,
)::String
  local outString::String

  @assign outString = Tpl.tplString2(SCodeDumpTpl.dumpProgram, inProgram, options)
  return outString
end

function classDefStr(cd::SCode.ClassDef, options::SCodeDumpOptions = defaultOptions)::String
  local outString::String

  @assign outString = Tpl.tplString2(SCodeDumpTpl.dumpClassDef, cd, options)
  return outString
end

function statementStr(
  stmt::SCode.Statement,
  options::SCodeDumpOptions = defaultOptions,
)::String
  local outString::String

  @assign outString = Tpl.tplString2(SCodeDumpTpl.dumpStatement, stmt, options)
  return outString
end

function equationStr(
  inEEquation::SCode.EEquation,
  options::SCodeDumpOptions = defaultOptions,
)::String
  local outString::String

  @assign outString = Tpl.tplString2(SCodeDumpTpl.dumpEEquation, inEEquation, options)
  return outString
end

""" #= Prints SCode.Mod to a string. =#"""
function printModStr(inMod::SCode.Mod, options::SCodeDumpOptions = defaultOptions)::String
  local outString::String

  @assign outString = Tpl.tplString2(SCodeDumpTpl.dumpModifier, inMod, options)
  return outString
end

""" #= Prints SCode.Comment to a string. =#"""
function printCommentAndAnnotationStr(
  inComment::SCode.Comment,
  options::SCodeDumpOptions = defaultOptions,
)::String
  local outString::String

  @assign outString = Tpl.tplString2(SCodeDumpTpl.dumpComment, inComment, options)
  return outString
end

""" #= Prints SCode.Comment.comment to a string. =#"""
function printCommentStr(
  inComment::SCode.Comment,
  options::SCodeDumpOptions = defaultOptions,
)::String
  local outString::String

  @assign outString = begin
    local comment::Option{String}
    @match inComment begin
      SCode.COMMENT(comment = comment) => begin
        Tpl.tplString2(SCodeDumpTpl.dumpCommentStr, comment, options)
      end

      _ => begin
        ""
      end
    end
  end
  return outString
end

""" #= Prints SCode.Comment.annotation to a string. =#"""
function printAnnotationStr(
  inComment::SCode.Comment,
  options::SCodeDumpOptions = defaultOptions,
)::String
  local outString::String

  @assign outString = begin
    local annotation_::Option{SCode.Annotation}
    @match (inComment, options) begin
      (SCode.COMMENT(annotation_ = annotation_), _) => begin
        Tpl.tplString2(SCodeDumpTpl.dumpAnnotationOpt, annotation_, options)
      end

      _ => begin
        ""
      end
    end
  end
  return outString
end

""" #= Prints SCode.Restriction to a string. =#"""
function restrString(inRestriction::SCode.Restriction)::String
  local outString::String

  @assign outString = begin
    @match inRestriction begin
      SCode.R_CLASS(__) => begin
        "class"
      end

      SCode.R_OPTIMIZATION(__) => begin
        "optimization"
      end

      SCode.R_MODEL(__) => begin
        "model"
      end

      SCode.R_RECORD(false) => begin
        "record"
      end

      SCode.R_RECORD(true) => begin
        "operator record"
      end

      SCode.R_BLOCK(__) => begin
        "block"
      end

      SCode.R_CONNECTOR(false) => begin
        "connector"
      end

      SCode.R_CONNECTOR(true) => begin
        "expandable connector"
      end

      SCode.R_OPERATOR(__) => begin
        "operator"
      end

      SCode.R_FUNCTION(SCode.FR_NORMAL_FUNCTION(false)) => begin
        "pure function"
      end

      SCode.R_FUNCTION(SCode.FR_NORMAL_FUNCTION(true)) => begin
        "impure function"
      end

      SCode.R_FUNCTION(SCode.FR_OPERATOR_FUNCTION(__)) => begin
        "operator function"
      end

      SCode.R_FUNCTION(SCode.FR_EXTERNAL_FUNCTION(false)) => begin
        "pure external function"
      end

      SCode.R_FUNCTION(SCode.FR_EXTERNAL_FUNCTION(true)) => begin
        "impure external function"
      end

      SCode.R_FUNCTION(SCode.FR_RECORD_CONSTRUCTOR(__)) => begin
        "record constructor"
      end

      SCode.R_FUNCTION(SCode.FR_PARALLEL_FUNCTION(__)) => begin
        "parallel function"
      end

      SCode.R_FUNCTION(SCode.FR_KERNEL_FUNCTION(__)) => begin
        "kernel function"
      end

      SCode.R_TYPE(__) => begin
        "type"
      end

      SCode.R_PACKAGE(__) => begin
        "package"
      end

      SCode.R_ENUMERATION(__) => begin
        "enumeration"
      end

      SCode.R_METARECORD(__) => begin
        "metarecord " + AbsynUtil.pathString(inRestriction.name)
      end

      SCode.R_UNIONTYPE(__) => begin
        "uniontype"
      end

      SCode.R_PREDEFINED_INTEGER(__) => begin
        "Integer"
      end

      SCode.R_PREDEFINED_REAL(__) => begin
        "Real"
      end

      SCode.R_PREDEFINED_STRING(__) => begin
        "String"
      end

      SCode.R_PREDEFINED_BOOLEAN(__) => begin
        "Boolean"
      end

      SCode.R_PREDEFINED_CLOCK(__) => begin
        "Clock"
      end

      SCode.R_PREDEFINED_ENUMERATION(__) => begin
        "enumeration"
      end
    end
  end
  #=  predefined types
  =#
  #=  BTH
  =#
  return outString
end

""" #= Translates a SCode.Restriction to a String. =#"""
function restrictionStringPP(inRestriction::SCode.Restriction)::String
  local outString::String

  @assign outString = Tpl.tplString(SCodeDumpTpl.dumpRestriction, inRestriction)
  return outString
end

const noEachStr = ""::String

""" #= Print SCode.Element to a string. =#"""
function unparseElementStr(
  inElement::SCode.Element,
  options::SCodeDumpOptions = defaultOptions,
)::String
  local outString::String

  @assign outString =
    Tpl.tplString3(SCodeDumpTpl.dumpElement, inElement, noEachStr, options)
  return outString
end

""" #= Print SCode.Element to a string. =#"""
function shortElementStr(inElement::SCode.Element)::String
  local outString::String

  @assign outString = begin
    local str::String
    local res::String
    local n::String
    local ioStr::String
    local mod::SCode.Mod
    local path::Absyn.Path
    local imp::Absyn.Import
    local io::Absyn.InnerOuter
    local rdp::SCode.Redeclare
    local rpp::SCode.Replaceable
    local pp::SCode.Partial
    @match inElement begin
      SCode.EXTENDS(baseClassPath = path, modifications = mod) => begin
        @assign str = AbsynUtil.pathString(path)
        @assign str = str + printModStr(mod, defaultOptions)
        @assign res = stringAppendList(list("extends ", str, ";"))
        res
      end

      SCode.COMPONENT(__) => begin
        @assign res = unparseElementStr(inElement, defaultOptions)
        res
      end

      SCode.CLASS(prefixes = SCode.PREFIXES(__), classDef = SCode.DERIVED(__)) => begin
        @assign res = unparseElementStr(inElement, defaultOptions)
        res
      end

      SCode.CLASS(
        name = n,
        partialPrefix = pp,
        prefixes = SCode.PREFIXES(
          innerOuter = io,
          redeclarePrefix = rdp,
          replaceablePrefix = rpp,
        ),
        classDef = SCode.CLASS_EXTENDS(__),
      ) => begin
        @assign ioStr =
          Dump.unparseInnerouterStr(io) +
          redeclareStr(rdp) +
          replaceablePrefixStr(rpp) +
          partialStr(pp)
        @assign res = stringAppendList(list(ioStr, "class extends ", n, ";"))
        res
      end

      SCode.CLASS(
        name = n,
        partialPrefix = pp,
        prefixes = SCode.PREFIXES(
          innerOuter = io,
          redeclarePrefix = rdp,
          replaceablePrefix = rpp,
        ),
        classDef = SCode.ENUMERATION(__),
      ) => begin
        @assign ioStr =
          Dump.unparseInnerouterStr(io) +
          redeclareStr(rdp) +
          replaceablePrefixStr(rpp) +
          partialStr(pp)
        @assign res = stringAppendList(list(ioStr, "class ", n, " enumeration;"))
        res
      end

      SCode.CLASS(
        name = n,
        partialPrefix = pp,
        prefixes = SCode.PREFIXES(
          innerOuter = io,
          redeclarePrefix = rdp,
          replaceablePrefix = rpp,
        ),
      ) => begin
        @assign ioStr =
          Dump.unparseInnerouterStr(io) +
          redeclareStr(rdp) +
          replaceablePrefixStr(rpp) +
          partialStr(pp)
        @assign res = stringAppendList(list(ioStr, "class ", n, ";"))
        res
      end

      SCode.IMPORT(imp = imp) => begin
        @assign str = "import " + AbsynUtil.printImportString(imp) + ";"
        str
      end
    end
  end
  return outString
end

function printEnumStr(en::SCode.Enum)::String
  local str::String

  @assign str = begin
    local s::String
    @match en begin
      SCode.ENUM(s, _) => begin
        s
      end
    end
  end
  return str
end

""" #= Print Variability to a string. =#"""
function variabilityString(inVariability::SCode.Variability)::String
  local outString::String

  @assign outString = begin
    @match inVariability begin
      SCode.VAR(__) => begin
        "VAR"
      end

      SCode.DISCRETE(__) => begin
        "DISCRETE"
      end

      SCode.PARAM(__) => begin
        "PARAM"
      end

      SCode.CONST(__) => begin
        "CONST"
      end
    end
  end
  return outString
end

""" #= Print parallelism to a string. =#"""
function parallelismString(inParallelism::SCode.Parallelism)::String
  local outString::String

  @assign outString = begin
    @match inParallelism begin
      SCode.PARGLOBAL(__) => begin
        "PARGLOBAL"
      end

      SCode.PARLOCAL(__) => begin
        "PARLOCAL"
      end

      SCode.NON_PARALLEL(__) => begin
        "NON_PARALLEL"
      end
    end
  end
  return outString
end

""" #= Print a inner outer info to a string. =#"""
function innerouterString(innerOuter::Absyn.InnerOuter)::String
  local outString::String

  @assign outString = begin
    @match innerOuter begin
      Absyn.INNER_OUTER(__) => begin
        "INNER/OUTER"
      end

      Absyn.INNER(__) => begin
        "INNER"
      end

      Absyn.OUTER(__) => begin
        "OUTER"
      end

      Absyn.NOT_INNER_OUTER(__) => begin
        ""
      end
    end
  end
  return outString
end

""" #= Print Variability to a string. =#"""
function unparseVariability(inVariability::SCode.Variability)::String
  local outString::String

  @assign outString = begin
    @match inVariability begin
      SCode.VAR(__) => begin
        ""
      end

      SCode.DISCRETE(__) => begin
        "discrete"
      end

      SCode.PARAM(__) => begin
        "parameter"
      end

      SCode.CONST(__) => begin
        "constant"
      end
    end
  end
  return outString
end

""" #= Takes a SCode.Equation rather then SCode.EEquation as equationStr does. =#"""
function equationStr2(eqns::SCode.Equation, options::SCodeDumpOptions)::String
  local s::String

  @assign s = begin
    local e::SCode.EEquation
    @match (eqns, options) begin
      (SCode.EQUATION(eEquation = e), _) => begin
        equationStr(e, options)
      end
    end
  end
  return s
end

""" #= prints SCode.Initial to a string =#"""
function printInitialStr(initial_::SCode.Initial)::String
  local str::String

  @assign str = begin
    @match initial_ begin
      SCode.INITIAL(__) => begin
        "initial"
      end

      SCode.NON_INITIAL(__) => begin
        "non initial"
      end
    end
  end
  return str
end

function connectorTypeStr(inConnectorType::SCode.ConnectorType)::String
  local str::String

  @assign str = begin
    @match inConnectorType begin
      SCode.POTENTIAL(__) => begin
        ""
      end

      SCode.FLOW(__) => begin
        "flow"
      end

      SCode.STREAM(__) => begin
        "stream"
      end
    end
  end
  return str
end

function encapsulatedStr(inEncapsulated::SCode.Encapsulated)::String
  local str::String

  @assign str = begin
    @match inEncapsulated begin
      SCode.ENCAPSULATED(__) => begin
        "encapsulated "
      end

      SCode.NOT_ENCAPSULATED(__) => begin
        ""
      end
    end
  end
  return str
end

function partialStr(inPartial::SCode.Partial)::String
  local str::String

  @assign str = begin
    @match inPartial begin
      SCode.PARTIAL(__) => begin
        "partial "
      end

      SCode.NOT_PARTIAL(__) => begin
        ""
      end
    end
  end
  return str
end

function visibilityStr(inVisibility::SCode.Visibility)::String
  local str::String

  @assign str = begin
    @match inVisibility begin
      SCode.PUBLIC(__) => begin
        "public "
      end

      SCode.PROTECTED(__) => begin
        "protected "
      end
    end
  end
  return str
end

function finalStr(inFinal::SCode.Final)::String
  local str::String

  @assign str = begin
    @match inFinal begin
      SCode.FINAL(__) => begin
        "final "
      end

      SCode.NOT_FINAL(__) => begin
        ""
      end
    end
  end
  return str
end

function eachStr(inEach::SCode.Each)::String
  local str::String

  @assign str = begin
    @match inEach begin
      SCode.EACH(__) => begin
        "each "
      end

      SCode.NOT_EACH(__) => begin
        ""
      end
    end
  end
  return str
end

function redeclareStr(inRedeclare::SCode.Redeclare)::String
  local str::String

  @assign str = begin
    @match inRedeclare begin
      SCode.REDECLARE(__) => begin
        "redeclare "
      end

      SCode.NOT_REDECLARE(__) => begin
        ""
      end
    end
  end
  return str
end

function replaceableStr(inReplaceable::SCode.Replaceable)::Tuple{String, String}
  local strConstraint::String
  local strReplaceable::String

  @assign (strReplaceable, strConstraint) = begin
    local path::Absyn.Path
    local mod::SCode.Mod
    local path_str::String
    local mod_str::String
    @match inReplaceable begin
      SCode.REPLACEABLE(SOME(SCode.CONSTRAINCLASS(
        constrainingClass = path,
        modifier = mod,
      ))) => begin
        @assign path_str = AbsynUtil.pathString(path)
        @assign mod_str = printModStr(mod, defaultOptions)
        ("replaceable ", path_str + "(" + mod_str + ")")
      end

      SCode.REPLACEABLE(NONE()) => begin
        ("replaceable ", "")
      end

      SCode.NOT_REPLACEABLE(__) => begin
        ("", "")
      end
    end
  end
  return (strReplaceable, strConstraint)
end

function replaceablePrefixStr(inReplaceable::SCode.Replaceable)::String
  local strReplaceable::String

  @assign strReplaceable = begin
    @match inReplaceable begin
      SCode.REPLACEABLE(_) => begin
        "replaceable "
      end

      SCode.NOT_REPLACEABLE(__) => begin
        ""
      end
    end
  end
  return strReplaceable
end

function replaceableConstrainClassStr(inReplaceable::SCode.Replaceable)::String
  local strReplaceable::String

  @assign (_, strReplaceable) = replaceableStr(inReplaceable)
  return strReplaceable
end

""" #= Returns prefixes as string =#"""
function prefixesStr(prefixes::SCode.Prefixes)::String
  local str::String

  @assign str = begin
    local v::SCode.Visibility
    local rd::SCode.Redeclare
    local f::SCode.Final
    local io::Absyn.InnerOuter
    local rpl::SCode.Replaceable
    local s::String
    @match prefixes begin
      SCode.PREFIXES(v, rd, f, io, rpl) => begin
        @assign s =
          visibilityStr(v) +
          redeclareStr(rd) +
          finalStr(f) +
          AbsynUtil.innerOuterStr(io) +
          replaceablePrefixStr(rpl)
        s
      end
    end
  end
  return str
end

function filterElements(
  elements::List{<:SCode.Element},
  options::SCodeDumpOptions,
)::List{SCode.Element}
  local outElements::List{SCode.Element}

  @assign outElements = ListUtil.select1(elements, filterElement, options)
  return outElements
end

function filterElement(element::SCode.Element, options::SCodeDumpOptions)::Bool
  local b::Bool

  @assign b = begin
    @match (element, options) begin
      (
        SCode.IMPORT(visibility = SCode.PROTECTED(__)),
        OPTIONS(stripProtectedImports = true),
      ) => begin
        false
      end

      (
        SCode.CLASS(prefixes = SCode.PREFIXES(visibility = SCode.PROTECTED(__))),
        OPTIONS(stripProtectedClasses = true),
      ) => begin
        false
      end

      (
        SCode.COMPONENT(prefixes = SCode.PREFIXES(visibility = SCode.PROTECTED(__))),
        OPTIONS(stripProtectedComponents = true),
      ) => begin
        false
      end

      (
        SCode.CLASS(restriction = SCode.R_METARECORD(moved = true)),
        OPTIONS(stripMetaRecords = true),
      ) => begin
        false
      end

      _ => begin
        true
      end
    end
  end
  return b
end

@exportAll()
end
