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

@UniontypeDecl InstNodeType


Expression=NFExpression
Restriction=NFRestriction
FuncType = Function
@UniontypeDecl InstNode

@Uniontype InstNodeType begin
  @Record NORMAL_CLASS begin
  end
  @Record BASE_CLASS begin
    parent::InstNode
    definition #= The extends clause definition. =#::SCode.Element
  end
  @Record DERIVED_CLASS begin
    ty::InstNodeType
  end
  @Record BUILTIN_CLASS begin
  end
  @Record TOP_SCOPE begin
  end
  @Record ROOT_CLASS begin
    parent #= The parent of the class, e.g. when instantiating a function
    in a component where the component is the parent. =#::InstNode
  end
  @Record NORMAL_COMP begin
  end
  @Record REDECLARED_COMP begin
    parent #= The parent of the replaced component =#::InstNode
  end
  @Record REDECLARED_CLASS begin
    parent::InstNode
    originalType::InstNodeType
  end
end

module NodeTree

import ..Main.InstNode

using MetaModelica
using ExportAll
Key = String
Value = InstNode
include("../Util/baseAvlTreeCode.jl")
include("../Util/baseAvlSetCode.jl")

keyCompare = (inKey1::String, inKey2::String) -> begin
  res = stringCompare(inKey1, inKey2)
  return res
end

function new()
  return EMPTY()
end

@exportAll()
end

@Uniontype CachedData begin
  @Record C_TOP_SCOPE begin
    addedInner::NodeTree.Tree
    rootClass::InstNode
  end
  @Record C_FUNCTION begin
    funcs::List{M_Function}
    typed::Bool
    specialBuiltin::Bool
  end
  @Record C_PACKAGE begin
    instance::InstNode
  end
  @Record C_NO_CACHE begin
  end
end

const NUMBER_OF_CACHES = 3::Integer

function setInnerOuterCache(in_caches::Array{<:CachedData}, in_cache::CachedData) ::Array{CachedData}
  local out_caches::Array{CachedData} = arrayUpdate(in_caches, 3, in_cache)
  out_caches
end

function getInnerOuterCache(in_caches::Array{<:CachedData}) ::CachedData
  local out_cache::CachedData = arrayGet(in_caches, 3)
  out_cache
end

function clearPackageCache(in_caches::Array{<:CachedData}) ::Array{CachedData}
  local out_caches::Array{CachedData} = arrayUpdate(in_caches, 2, C_NO_CACHE())
  out_caches
end

function setPackageCache(in_caches::Array{<:CachedData}, in_cache::CachedData) ::Array{CachedData}
  local out_caches::Array{CachedData} = arrayUpdate(in_caches, 2, in_cache)
  out_caches
end

function getPackageCache(in_caches::Array{<:CachedData}) ::CachedData
  local out_cache::CachedData = arrayGet(in_caches, 2)
  out_cache
end

function setFuncCache(in_caches::Array{<:CachedData}, in_cache::CachedData)
  #@debug "Setting func cache  with $in_caches and $in_cache"
  arrayUpdate(in_caches, 1, in_cache)
end

function getFuncCache(in_caches::Array{<:CachedData}) ::CachedData
#  @debug in_caches
  local out_cache::CachedData = arrayGet(in_caches, 1)
  out_cache
end

function addFunc(fn::M_Function, specialBuiltin::Bool, caches::Array{<:CachedData})
  local func_cache::CachedData
  @assign func_cache = getFuncCache(caches)
  @assign func_cache = begin
    @match func_cache begin
      C_NO_CACHE(__)  => begin
        C_FUNCTION(list(fn), false, specialBuiltin)
      end

      C_FUNCTION(__)  => begin
        C_FUNCTION(listAppend(func_cache.funcs, list(fn)), false, func_cache.specialBuiltin || specialBuiltin)
      end

      _  => begin
        #=  Append to end so the error messages are ordered properly.
        =#
        Error.assertion(false, getInstanceName() + ": Invalid cache for function", sourceInfo())
        fail()
      end
    end
  end
  setFuncCache(caches, func_cache)
end

function initFunc(caches::Array{<:CachedData})
  local func_cache::CachedData
  @assign func_cache = getFuncCache(caches)
  @assign func_cache = begin
    @match func_cache begin
      C_NO_CACHE(__)  => begin
        C_FUNCTION(nil, false, false)
      end
      C_FUNCTION(__)  => begin
        func_cache
      end
    end
  end
  setFuncCache(caches, func_cache)
end

function empty()::Array{CachedData}
  local cache::Array{CachedData} = arrayCreate(NUMBER_OF_CACHES, C_NO_CACHE())
  cache
end

function hasBinding(node::InstNode) ::Bool
  local hasBinding::Bool
  @assign hasBinding = begin
    @match node begin
      COMPONENT_NODE(__)  => begin
          hasBinding(P_Pointer.access(node.component)) || hasBinding(derivedParent(node))
      end
      _  => begin
        false
      end
    end
  end
  hasBinding
end

function isModel(node::InstNode) ::Bool
  local isModel::Bool
  @assign isModel = begin
    @match node begin
      CLASS_NODE(__)  => begin
        isModel(restriction(P_Pointer.access(node.cls)))
      end
      COMPONENT_NODE(__)  => begin
        isModel(P_Component.classInstance(P_Pointer.access(node.component)))
      end
      _  => begin
        false
      end
    end
  end
  isModel
end

function isRecord(@nospecialize(node::InstNode)) ::Bool
  local isRec::Bool
  @assign isRec = begin
    @match node begin
      CLASS_NODE(__)  => begin
        isRecord(restriction(P_Pointer.access(node.cls)))
      end
      COMPONENT_NODE(__)  => begin
        isRecord(classInstance(P_Pointer.access(node.component)))
      end
    end
  end
  isRec
end

function copyInstancePtr(@nospecialize(srcNode::InstNode), @nospecialize(dstNode::InstNode)) ::InstNode
  @assign () = begin
    @match (srcNode, dstNode) begin
      (COMPONENT_NODE(__), COMPONENT_NODE(__))  => begin
        @assign dstNode.component = srcNode.component
        ()
      end

      (CLASS_NODE(__), CLASS_NODE(__))  => begin
        @assign dstNode.cls = srcNode.cls
        ()
      end
    end
  end
  dstNode
end

function getComments(node::InstNode, accumCmts::List{<:SCode.Comment} = nil) ::List{SCode.Comment}
  local cmts::List{SCode.Comment}

  @assign cmts = begin
    local cmt::SCode.Comment
    local cls::Class
    @match node begin
      CLASS_NODE(definition = SCode.CLASS(cmt = cmt))  => begin
        _cons(cmt, getDerivedComments(P_Pointer.access(node.cls), accumCmts))
      end

      _  => begin
        accumCmts
      end
    end
  end
  cmts
end

function clone(node::InstNode) ::InstNode
  @assign () = begin
    local cls::Class
    @match node begin
      CLASS_NODE(__)  => begin
        @assign cls = P_Pointer.access(node.cls)
        @assign cls = classTreeApply(cls, clone)
        @assign node.cls = P_Pointer.create(cls)
        @assign node.caches = empty()
        ()
      end

      _  => begin
        ()
      end
    end
  end
  node
end

function isPartial(node::InstNode) ::Bool
  local isPartial::Bool
  @assign isPartial = begin
    @match node begin
      CLASS_NODE(__)  => begin
        SCodeUtil.isPartial(node.definition)
      end

      _  => begin
        false
      end
    end
  end
  isPartial
end

function isBuiltin(node::InstNode) ::Bool
  local isBuiltin::Bool
  @assign isBuiltin = begin
    @match node begin
      CLASS_NODE(nodeType = BUILTIN_CLASS(__))  => begin
        true
      end
      _  => begin
        false
      end
    end
  end
  isBuiltin
end

""" #= Returns the DAE type for a class, with the list of variables filled in. =#"""
function toFullDAEType(clsNode::InstNode) ::DAE.Type
  local outType::DAE.Type

  @assign outType = begin
    local cls::Class
    local vars::List{DAE.Var}
    local state::ClassInf.State
    @match clsNode begin
      CLASS_NODE(__)  => begin
        @assign cls = P_Pointer.access(clsNode.cls)
        begin
          @match cls begin
            DAE_TYPE(__)  => begin
              cls.ty
            end

            _  => begin
              @assign state = toDAE(restriction(cls), scopePath(clsNode, includeRoot = true))
              @assign vars = ConvertDAE.makeTypeVars(clsNode)
              @assign outType = DAE.Type.T_COMPLEX(state, vars, NONE())
              Pointer.update(clsNode.cls, DAE_TYPE(outType))
              outType
            end
          end
        end
      end
    end
  end
  outType
end

function stripDAETypeVars(ty::DAE.Type) ::DAE.Type
  @assign () = begin
    @match ty begin
      DAE.Type.T_COMPLEX(__)  => begin
        @assign ty.varLst = nil
        ()
      end
      _  => begin
        ()
      end
    end
  end
  ty
end

""" #= Returns the DAE type for a class, without the list of variables filled in. =#"""
function toPartialDAEType(clsNode::InstNode) ::DAE.Type
  local outType::DAE.Type

  @assign outType = begin
    local cls::Class
    local state::ClassInf.SMNode
    @match clsNode begin
      CLASS_NODE(__)  => begin
        @assign cls = P_Pointer.access(clsNode.cls)
        begin
          @match cls begin
            DAE_TYPE(__)  => begin
              stripDAETypeVars(cls.ty)
            end

            _  => begin
              @assign state = toDAE(restriction(cls), scopePath(clsNode, includeRoot = true))
              DAE.T_COMPLEX(state, nil, NONE())
            end
          end
        end
      end
    end
  end
  outType
end

function setModifier(mod::Modifier, node::InstNode) ::InstNode


  @assign () = begin
    @match node begin
      CLASS_NODE(__)  => begin
        P_Pointer.update(node.cls, setModifier(mod, P_Pointer.access(node.cls)))
        ()
      end

      COMPONENT_NODE(__)  => begin
        Pointer.update(node.component, P_Component.mergeModifier(mod, P_Pointer.access(node.component)))
        ()
      end

      _  => begin
        ()
      end
    end
  end
  node
end

function mergeModifier(mod::Modifier, node::InstNode) ::InstNode


  @assign () = begin
    @match node begin
      CLASS_NODE(__)  => begin
        Pointer.update(node.cls, mergeModifier(mod, P_Pointer.access(node.cls)))
        ()
      end

      COMPONENT_NODE(__)  => begin
        Pointer.update(node.component, P_Component.mergeModifier(mod, P_Pointer.access(node.component)))
        ()
      end

      _  => begin
        ()
      end
    end
  end
  node
end

function getModifier(node::InstNode) ::Modifier
  local mod::Modifier

  @assign mod = begin
    @match node begin
      CLASS_NODE(__)  => begin
        getModifier(P_Pointer.access(node.cls))
      end

      COMPONENT_NODE(__)  => begin
        P_Component.getModifier(P_Pointer.access(node.component))
      end

      _  => begin
        MODIFIER_NOMOD()
      end
    end
  end
  mod
end

function protectComponent(comp::InstNode) ::InstNode


  @assign () = begin
    @match comp begin
      COMPONENT_NODE(visibility = Visibility.PUBLIC)  => begin
        @assign comp.visibility = Visibility.PROTECTED
        ()
      end

      _  => begin
        ()
      end
    end
  end
  comp
end

function protectClass(cls::InstNode) ::InstNode


  @assign () = begin
    @match cls begin
      CLASS_NODE(visibility = Visibility.PUBLIC)  => begin
        @assign cls.visibility = Visibility.PROTECTED
        ()
      end

      _  => begin
        ()
      end
    end
  end
  cls
end

function isProtected(node::InstNode) ::Bool
  local isProtected::Bool

  @assign isProtected = begin
    @match node begin
      CLASS_NODE(visibility = Visibility.PROTECTED)  => begin
        true
      end

      COMPONENT_NODE(visibility = Visibility.PROTECTED)  => begin
        true
      end

      _  => begin
        false
      end
    end
  end
  isProtected
end

function visibility(node::InstNode) ::VisibilityType
  local vis::VisibilityType

  @assign vis = begin
    @match node begin
      CLASS_NODE(__)  => begin
        node.visibility
      end

      COMPONENT_NODE(__)  => begin
        node.visibility
      end

      _  => begin
        Visibility.PUBLIC
      end
    end
  end
  vis
end

function isProtectedBaseClass(node::InstNode) ::Bool
  local isProtected::Bool

  @assign isProtected = begin
    local def::SCode.Element
    @match node begin
      CLASS_NODE(nodeType = BASE_CLASS(definition = SCode.EXTENDS(visibility = SCode.PROTECTED(__))))  => begin
        true
      end

      _  => begin
        false
      end
    end
  end
  isProtected
end

function isRedeclare(node::InstNode) ::Bool
  local isRedcl::Bool = false
  @assign isRedcl = begin
    @match node begin
      CLASS_NODE(__)  => begin
        SCodeUtil.isElementRedeclare(definition(node))
      end

      COMPONENT_NODE(__)  => begin
        isRedeclare(P_Pointer.access(node.component))
      end

      _  => begin
        false
      end
    end
  end
return isRedcl
end

function toFlatStream(node::InstNode, s)
  @assign s = begin
    @match node begin
      CLASS_NODE(__)  => begin
        toFlatStream(P_Pointer.access(node.cls), node, s)
      end
      _  => begin
        IOStream.append(s, toFlatString(node))
      end
    end
  end
  s
end

function toFlatString(node::InstNode) ::String
  local name::String
  @assign name = begin
    @match node begin
      COMPONENT_NODE(__)  => begin
        P_Component.toFlatString(node.name, P_Pointer.access(node.component))
      end
      CLASS_NODE(__)  => begin
        toFlatString(P_Pointer.access(node.cls), node)
      end
      _  => begin
        name(node)
      end
    end
  end
  name
end

function toString(node::InstNode) ::String
  local name::String
  @assign name = begin
    @match node begin
      COMPONENT_NODE(__)  => begin
        toString(node.name, P_Pointer.access(node.component))
      end
      CLASS_NODE(__)  => begin
        SCodeDump.unparseElementStr(node.definition)
      end
      _  => begin
        name(node)
      end
    end
  end
  name
end

function checkIdentical(node1::InstNode, node2::InstNode)
  local n1::InstNode = resolveOuter(node1)
  local n2::InstNode = resolveOuter(node2)
  if referenceEq(n1, n2)
    return
  end
  @assign () = begin
    @matchcontinue (n1, n2) begin
      (CLASS_NODE(__), CLASS_NODE(__)) where (isIdentical(getClass(n1), getClass(n2)))  => begin
        ()
      end
      (COMPONENT_NODE(__), COMPONENT_NODE(__)) where (isIdentical(component(n1), component(n2)))  => begin
        ()
      end
      _  => begin
        Error.addMultiSourceMessage(Error.DUPLICATE_ELEMENTS_NOT_IDENTICAL, list(toString(n1), toString(n2)), list(info(n1), info(n2)))
        fail()
      end
    end
  end
end

function isSame(node1::InstNode, node2::InstNode) ::Bool
  local same::Bool = false
  local n1::InstNode = resolveOuter(node1)
  local n2::InstNode = resolveOuter(node2)
  if referenceEq(n1, n2)
    @assign same = true
    return same
  elseif stringEqual(name(n1), name(n2))
    @assign same = true
    return same
  end
  #=  TODO: This is not enough. We need a better way.
  =#
  same
end

function nameEqual(node1::InstNode, node2::InstNode) ::Bool
  local equal::Bool = name(node1) == name(node2)
  equal
end

function refCompare(node1::InstNode, node2::InstNode) ::Integer
  local res::Integer
  @assign res = begin
    @match (node1, node2) begin
      (CLASS_NODE(__), CLASS_NODE(__))  => begin
        Util.referenceCompare(P_Pointer.access(node1.cls), P_Pointer.access(node2.cls))
      end
      (COMPONENT_NODE(__), COMPONENT_NODE(__))  => begin
        Util.referenceCompare(P_Pointer.access(node1.component), P_Pointer.access(node2.component))
      end
      (CLASS_NODE(__), COMPONENT_NODE(__))  => begin
        Util.referenceCompare(P_Pointer.access(node1.cls), P_Pointer.access(node2.component))
      end
      (COMPONENT_NODE(__), CLASS_NODE(__))  => begin
        Util.referenceCompare(P_Pointer.access(node1.component), P_Pointer.access(node2.cls))
      end
    end
  end
  res
end

""" #= Returns true if two nodes references the same class or component,
                     otherwise false. =#"""
                       function refEqual(node1::InstNode, node2::InstNode) ::Bool
                         local refEqualIs::Bool
                         @assign refEqualIs = begin
                           @match (node1, node2) begin
                             (CLASS_NODE(__), CLASS_NODE(__))  => begin
                               referenceEq(P_Pointer.access(node1.cls), P_Pointer.access(node2.cls))
                             end
                             (COMPONENT_NODE(__), COMPONENT_NODE(__))  => begin
                               referenceEq(P_Pointer.access(node1.component), P_Pointer.access(node2.component))
                             end
                             _  => begin
                               false
                             end
                           end
                         end
                         #=  Other nodes like ref nodes might be equal, but we neither know nor care.
                         =#
                         refEqualIs
                       end

function addIterator(iterator::InstNode, scope::InstNode) ::InstNode


  @assign scope = begin
    @match scope begin
      IMPLICIT_SCOPE(__)  => begin
        IMPLICIT_SCOPE(scope, _cons(iterator, scope.locals))
      end
    end
  end
  scope
end

""" #= Returns the first parent of the node that's not an implicit scope, or the
                     node itself if it's not an implicit scope. =#"""
                       function explicitScope(node::InstNode) ::InstNode
                         local scope::InstNode

                         @assign scope = begin
                           @match node begin
                             IMPLICIT_SCOPE(__)  => begin
                               explicitScope(node.parentScope)
                             end

                             _  => begin
                               node
                             end
                           end
                         end
                         scope
                       end

function openImplicitScope(scope::InstNode) ::InstNode


  @assign scope = begin
    @match scope begin
      IMPLICIT_SCOPE(__)  => begin
        scope
      end

      _  => begin
        IMPLICIT_SCOPE(scope, nil)
      end
    end
  end
  scope
end

function setInnerOuterCache(node::InstNode, in_out_cache::CachedData) ::InstNode
  @assign () = begin
    @match node begin
      CLASS_NODE(__)  => begin
        setInnerOuterCache(node.caches, in_out_cache)
        ()
      end

      _  => begin
        Error.assertion(false, getInstanceName() + " got node without cache", sourceInfo())
        fail()
      end
    end
  end
  node
end

function getInnerOuterCache(inNode::InstNode) ::CachedData
  local pack_cache::CachedData
  @assign pack_cache = begin
    @match inNode begin
      CLASS_NODE(__)  => begin
        getInnerOuterCache(inNode.caches)
      end
      _  => begin
        Error.assertion(false, getInstanceName() + " got node without cache", sourceInfo())
        fail()
      end
    end
  end
  pack_cache
end

function clearPackageCache(node::InstNode) ::InstNode


  @assign () = begin
    @match node begin
      CLASS_NODE(__)  => begin
        P_CachedData.clearPackageCache(node.caches)
        ()
      end

      _  => begin
        Error.assertion(false, getInstanceName() + " got node without cache", sourceInfo())
        fail()
      end
    end
  end
  node
end

function setPackageCache(node::InstNode, in_pack_cache::CachedData) ::InstNode


  @assign () = begin
    @match node begin
      CLASS_NODE(__)  => begin
        P_CachedData.setPackageCache(node.caches, in_pack_cache)
        ()
      end

      _  => begin
        Error.assertion(false, getInstanceName() + " got node without cache", sourceInfo())
        fail()
      end
    end
  end
  node
end

function getPackageCache(inNode::InstNode) ::CachedData
  local pack_cache::CachedData

  @assign pack_cache = begin
    @match inNode begin
      CLASS_NODE(__)  => begin
        P_CachedData.getPackageCache(inNode.caches)
      end

      _  => begin
        Error.assertion(false, getInstanceName() + " got node without cache", sourceInfo())
        fail()
      end
    end
  end
  pack_cache
end

function setFuncCache(node::InstNode, in_func_cache::CachedData) ::InstNode
  @assign () = begin
    @match node begin
      CLASS_NODE(__)  => begin
        setFuncCache(node.caches, in_func_cache)
        ()
      end
      _  => begin
        #Error.assertion(false, getInstanceName() + " got node without cache", sourceInfo())
        @error "Error in set func cache"
        fail()
      end
    end
  end
  node
end

function getFuncCache(inNode::InstNode) ::CachedData
  local func_cache::CachedData
  @assign func_cache = begin
    @match inNode begin
      CLASS_NODE(__)  => begin
        getFuncCache(inNode.caches)
      end
      _  => begin
        Error.assertion(false, getInstanceName() + " got node without cache", sourceInfo())
        fail()
      end
    end
  end
  func_cache
end

function cacheAddFunc(node::InstNode, fn::M_Function, specialBuiltin::Bool) ::InstNode
  @assign () = begin
    @match node begin
      CLASS_NODE(__)  => begin
        addFunc(fn, specialBuiltin, node.caches)
        ()
      end
      _  => begin
        Error.assertion(false, getInstanceName() + " got node without cache", sourceInfo())
        fail()
      end
    end
  end
  node
end

function cacheInitFunc(node::InstNode) ::InstNode
  @assign () = begin
    @match node begin
      CLASS_NODE(__)  => begin
        initFunc(node.caches)
        ()
      end

      _  => begin
        Error.assertion(false, getInstanceName() + " got node without cache", sourceInfo())
        fail()
      end
    end
  end
  node
end

function resolveOuter(node::InstNode) ::InstNode
  local outerNode::InstNode

  @assign outerNode = begin
    @match node begin
      INNER_OUTER_NODE(__)  => begin
        node.outerNode
      end

      _  => begin
        node
      end
    end
  end
  outerNode
end

function resolveInner(node::InstNode) ::InstNode
  local innerNode::InstNode

  @assign innerNode = begin
    @match node begin
      INNER_OUTER_NODE(__)  => begin
        node.innerNode
      end

      _  => begin
        node
      end
    end
  end
  innerNode
end

function isInnerOuterNode(node::InstNode) ::Bool
  @debug "is inner outer node?"
  local isIO::Bool
  @assign isIO = begin
    @match node begin
      INNER_OUTER_NODE(__)  => begin
        true
      end
      _  => begin
        false
      end
    end
  end
  isIO
end

function isOnlyOuter(node::InstNode) ::Bool
  local isOuter::Bool

  @assign isOuter = begin
    @match node begin
      COMPONENT_NODE(__)  => begin
        isOnlyOuter(P_Pointer.access(node.component))
      end

      CLASS_NODE(__)  => begin
        AbsynUtil.isOnlyOuter(SCodeUtil.prefixesInnerOuter(SCodeUtil.elementPrefixes(node.definition)))
      end

      INNER_OUTER_NODE(__)  => begin
        isOnlyOuter(node.outerNode)
      end

      _  => begin
        false
      end
    end
  end
  isOuter
end

function isOuter(node::InstNode) ::Bool
  local isOuter::Bool

  @assign isOuter = begin
    @match node begin
      COMPONENT_NODE(__)  => begin
        P_Component.isOuter(P_Pointer.access(node.component))
      end

      CLASS_NODE(__)  => begin
        AbsynUtil.isOuter(SCodeUtil.prefixesInnerOuter(SCodeUtil.elementPrefixes(node.definition)))
      end

      INNER_OUTER_NODE(__)  => begin
        isOuter(node.outerNode)
      end

      _  => begin
        false
      end
    end
  end
  isOuter
end

function isInner(node::InstNode) ::Bool
  local isInner::Bool

  @assign isInner = begin
    @match node begin
      COMPONENT_NODE(__)  => begin
        P_Component.isInner(P_Pointer.access(node.component))
      end

      CLASS_NODE(__)  => begin
        AbsynUtil.isInner(SCodeUtil.prefixesInnerOuter(SCodeUtil.elementPrefixes(node.definition)))
      end

      INNER_OUTER_NODE(__)  => begin
        isInner(node.outerNode)
      end

      _  => begin
        false
      end
    end
  end
  isInner
end

function isOutput(node::InstNode) ::Bool
  local isOutput::Bool

  @assign isOutput = begin
    @match node begin
      COMPONENT_NODE(__)  => begin
        P_Component.isOutput(P_Pointer.access(node.component))
      end

      _  => begin
        false
      end
    end
  end
  isOutput
end

function isInput(node::InstNode) ::Bool
  local isInput::Bool

  @assign isInput = begin
    @match node begin
      COMPONENT_NODE(__)  => begin
        P_Component.isInput(P_Pointer.access(node.component))
      end

      _  => begin
        false
      end
    end
  end
  isInput
end

function scopePathClass(node::InstNode, ty::InstNodeType, includeRoot::Bool, accumPath::Absyn.Path) ::Absyn.Path
  local path::Absyn.Path

  @assign path = begin
    @match ty begin
      NORMAL_CLASS(__)  => begin
        scopePath2(classParent(node), includeRoot, Absyn.QUALIFIED(className(node), accumPath))
      end

      BASE_CLASS(__)  => begin
        scopePath2(ty.parent, includeRoot, accumPath)
      end

      DERIVED_CLASS(__)  => begin
        scopePathClass(node, ty.ty, includeRoot, accumPath)
      end

      BUILTIN_CLASS(__)  => begin
        Absyn.QUALIFIED(className(node), accumPath)
      end

      TOP_SCOPE(__)  => begin
        accumPath
      end

      ROOT_CLASS(__)  => begin
        if includeRoot
          scopePath2(classParent(node), includeRoot, Absyn.QUALIFIED(className(node), accumPath))
        else
          accumPath
        end
      end

      REDECLARED_CLASS(__)  => begin
        scopePath2(ty.parent, includeRoot, Absyn.QUALIFIED(className(node), accumPath))
      end

      _  => begin
        Error.assertion(false, getInstanceName() + " got unknown node type", sourceInfo())
        fail()
      end
    end
  end
  path
end

function scopePath2(node::InstNode, includeRoot::Bool, accumPath::Absyn.Path) ::Absyn.Path
  local path::Absyn.Path

  @assign path = begin
    @match node begin
      CLASS_NODE(__)  => begin
        scopePathClass(node, node.nodeType, includeRoot, accumPath)
      end

      COMPONENT_NODE(__)  => begin
        scopePath2(node.parent, includeRoot, Absyn.QUALIFIED(node.name, accumPath))
      end

      _  => begin
        accumPath
      end
    end
  end
  path
end

function scopePath(node::InstNode; includeRoot::Bool = false #= Whether to include the root class name or not. =#) ::Absyn.Path
  local path::Absyn.Path

  @assign path = begin
    local it::InstNodeType
    @match node begin
      CLASS_NODE(nodeType = it)  => begin
        begin
          @match it begin
            BASE_CLASS(__)  => begin
              scopePath(it.parent, includeRoot)
            end

            _  => begin
              scopePath2(node.parentScope, includeRoot, Absyn.IDENT(node.name))
            end
          end
        end
      end

      COMPONENT_NODE(__)  => begin
        scopePath2(node.parent, includeRoot, Absyn.IDENT(node.name))
      end

      IMPLICIT_SCOPE(__)  => begin
        scopePath(node.parentScope, includeRoot)
      end

      _  => begin
        Absyn.IDENT(name(node))
      end
    end
  end
  #=  For debugging.
  =#
  path
end

function scopeListClass(clsNode::InstNode, ty::InstNodeType, includeRoot::Bool, accumScopes::List{<:InstNode} = nil) ::List{InstNode}
  local scopes::List{InstNode}

  @assign scopes = begin
    @match ty begin
      NORMAL_CLASS(__)  => begin
        scopeList(parent(clsNode), includeRoot, _cons(clsNode, accumScopes))
      end

      BASE_CLASS(__)  => begin
        scopeList(ty.parent, includeRoot, accumScopes)
      end

      DERIVED_CLASS(__)  => begin
        scopeListClass(clsNode, ty.ty, includeRoot, accumScopes)
      end

      BUILTIN_CLASS(__)  => begin
        _cons(clsNode, accumScopes)
      end

      TOP_SCOPE(__)  => begin
        accumScopes
      end

      ROOT_CLASS(__)  => begin
        if includeRoot
          scopeList(parent(clsNode), includeRoot, _cons(clsNode, accumScopes))
        else
          accumScopes
        end
      end

      REDECLARED_CLASS(__)  => begin
        scopeList(ty.parent, includeRoot, _cons(getDerivedNode(clsNode), accumScopes))
      end

      _  => begin
        Error.assertion(false, getInstanceName() + " got unknown node type", sourceInfo())
        fail()
      end
    end
  end
  scopes
end

function scopeList(node::InstNode, includeRoot::Bool = false #= Whether to include the root class name or not. =#, accumScopes::List{<:InstNode} = nil) ::List{InstNode}
  scopeList(node, includeRoot = includeRoot, accumScopes = accumScopes)
end

function scopeList(node::InstNode; includeRoot::Bool = false #= Whether to include the root class name or not. =#, accumScopes::List{<:InstNode} = nil) ::List{InstNode}
  local scopes::List{InstNode}

  @assign scopes = begin
    local parent::InstNode
    @match node begin
      CLASS_NODE(__)  => begin
        scopeListClass(node, node.nodeType, includeRoot, accumScopes)
      end

      COMPONENT_NODE(parent = EMPTY_NODE(__))  => begin
        accumScopes
      end

      COMPONENT_NODE(nodeType = REDECLARED_COMP(parent = parent))  => begin
        scopeList(parent, includeRoot, _cons(node, accumScopes))
      end

      COMPONENT_NODE(__)  => begin
        scopeList(node.parent, includeRoot, _cons(node, accumScopes))
      end

      IMPLICIT_SCOPE(__)  => begin
        scopeList(node.parentScope, includeRoot, accumScopes)
      end

      _  => begin
        accumScopes
      end
    end
  end
  scopes
end

function componentApply(node::InstNode, func::FuncType, arg::ArgT)  where {ArgT}


  @assign () = begin
    @match node begin
      COMPONENT_NODE(__)  => begin
        P_Pointer.update(node.component, func(arg, P_Pointer.access(node.component)))
        ()
      end
    end
  end
  node
end

function classApply(@nospecialize(node::InstNode), @nospecialize(func::FuncType), arg::ArgT)  where {ArgT}
  @assign () = begin
    @match node begin
      CLASS_NODE(__)  => begin
        P_Pointer.update(node.cls, func(arg, P_Pointer.access(node.cls)))
        ()
      end
    end
  end
  node
end

function getType(@nospecialize(node::InstNode))::NFType
  local ty::M_Type
  @assign ty = begin
    @match node begin
      CLASS_NODE(__)  => begin
        getType(P_Pointer.access(node.cls), node)
      end

      COMPONENT_NODE(__)  => begin
        getType(P_Pointer.access(node.component))
      end
    end
  end
  ty
end

function InstNode_info(node::InstNode) ::SourceInfo
  local info::SourceInfo

  @assign info = begin
    local ty::InstNodeType
    @matchcontinue node begin
      CLASS_NODE(nodeType = ty && BASE_CLASS(__))  => begin
        SCodeUtil.elementInfo(ty.definition)
      end

      CLASS_NODE(__)  => begin
        SCodeUtil.elementInfo(node.definition)
      end

      COMPONENT_NODE(__)  => begin
        P_Component.info(P_Pointer.access(node.component))
      end

      COMPONENT_NODE(__)  => begin
        info(node.parent)
      end
      _  => begin
        AbsynUtil.dummyInfo
      end
    end
  end
  info
end

function setDefinition(definition::SCode.Element, node::InstNode) ::InstNode
  @assign () = begin
    @match node begin
      CLASS_NODE(__)  => begin
        @assign node.definition = definition
        ()
      end
    end
  end
  node
end

function definition(node::InstNode) ::SCode.Element
  local definition::SCode.Element
  @assign definition = begin
    @match node begin
      CLASS_NODE(__)  => begin
        node.definition
      end
      COMPONENT_NODE(__)  => begin
        P_Component.definition(P_Pointer.access(node.component))
      end
    end
  end
  definition
end

function setNodeType(nodeType::InstNodeType, node::InstNode) ::InstNode
  @assign () = begin
    @match node begin
      CLASS_NODE(__)  => begin
        @assign node.nodeType = nodeType
        ()
      end

      COMPONENT_NODE(__)  => begin
        @assign node.nodeType = nodeType
        ()
      end

      _  => begin
        ()
      end
    end
  end
  node
end

function nodeType(node::InstNode) ::InstNodeType
  local nodeType::InstNodeType
  @assign nodeType = begin
    @match node begin
      CLASS_NODE(__)  => begin
        node.nodeType
      end
      COMPONENT_NODE(__)  => begin
        node.nodeType
      end
    end
  end
  nodeType
end

function replaceClass(cls::Class, node::InstNode) ::InstNode
  @assign () = begin
    @match node begin
      CLASS_NODE(__)  => begin
        @assign node.cls = P_Pointer.create(cls)
        ()
      end
    end
  end
  node
end

function replaceComponent(component::Component, node::InstNode) ::InstNode
  @assign () = begin
    @match node begin
      COMPONENT_NODE(__)  => begin
        @assign node.component = P_Pointer.create(component)
        ()
      end
    end
  end
  node
end

function updateComponent!(component::Component, node::InstNode) ::InstNode
  @assign node = begin
    @match node begin
      COMPONENT_NODE(__)  => begin
        P_Pointer.update(node.component, component)
        node
      end
    end
  end
  node
end

function component(node::InstNode) ::Component
  local component::Component

  @assign component = begin
    @match node begin
      COMPONENT_NODE(__)  => begin
        P_Pointer.access(node.component)
      end
    end
  end
  component
end

function updateClass(cls::Class, node::InstNode)::InstNode
  @assign node = begin
    @match node begin
      CLASS_NODE(__)  => begin
        P_Pointer.update(node.cls, cls)
        node
      end
    end
  end
  node
end

function getDerivedNode(node::InstNode) ::InstNode
  local derived::InstNode

  @assign derived = begin
    @match node begin
      CLASS_NODE(nodeType = BASE_CLASS(parent = derived))  => begin
        getDerivedNode(derived)
      end

      _  => begin
        node
      end
    end
  end
  derived
end

function getDerivedClass(node::InstNode) ::Class
  local cls::Class

  @assign cls = begin
    @match node begin
      CLASS_NODE(__)  => begin
        getClass(getDerivedNode(node))
      end

      COMPONENT_NODE(__)  => begin
        getClass(getDerivedNode(P_Component.classInstance(P_Pointer.access(node.component))))
      end
    end
  end
  cls
end

function getClass(node::InstNode) ::Class
  local cls::Class
  @assign cls = begin
    @match node begin
      CLASS_NODE(__)  => begin
        res = P_Pointer.access(node.cls)
        res
      end
      COMPONENT_NODE(__)  => begin
        getClass(classInstance(P_Pointer.access(node.component)))
      end
    end
  end
  cls
end

""" #= Sets the parent of a node if the node lacks a parent, otherwise does nothing. =#"""
function setOrphanParent(parent::InstNode, node::InstNode) ::InstNode
  @assign () = begin
    @match node begin
      CLASS_NODE(parentScope = EMPTY_NODE(__))  => begin
        @assign node.parentScope = parent
        ()
      end
      COMPONENT_NODE(parent = EMPTY_NODE(__))  => begin
        @assign node.parent = parent
        ()
      end
      _  => begin
        ()
      end
    end
  end
  node
end

function setParent(parent::InstNode, node::InstNode) ::InstNode
  @assign () = begin
    @match node begin
      CLASS_NODE(__)  => begin
        @assign node.parentScope = parent
        ()
      end
      COMPONENT_NODE(__)  => begin
        @debug "Setting parent! for parent: $(parent.name) and node: $(node.name)"
        @debug "parent scope before $(node.parent)"
        @assign node.parent = parent
        ()
      end
      IMPLICIT_SCOPE(__)  => begin
        @assign node.parentScope = parent
        ()
      end
    end
  end
  @debug "parent scope after $(node.parent.name)"
  node
end

function topComponent(node::InstNode) ::InstNode
  local topComponent::InstNode

  @assign topComponent = begin
    @match node begin
      COMPONENT_NODE(parent = EMPTY_NODE(__))  => begin
        node
      end

      COMPONENT_NODE(__)  => begin
        topComponent(node.parent)
      end
    end
  end
  topComponent
end

function topScope(node::InstNode) ::InstNode
  local ts::InstNode
  @assign ts = begin
    @match node begin
      CLASS_NODE(nodeType = TOP_SCOPE(__))  => begin
        node
      end
      _  => begin
        topScope(parentScope(node))
      end
    end
  end
  ts
end

function classScope(node::InstNode) ::InstNode
  local scope::InstNode
  @assign scope = begin
    @match node begin
      COMPONENT_NODE(__)  => begin
        classInstance(P_Pointer.access(node.component))
      end
      _  => begin
        node
      end
    end
  end
  scope
end

""" #= Returns the parent scope of a node. In the case of a class this is simply
       the enclosing class. In the case of a component it is the enclosing class of
       the component's type. =#"""
         function parentScope(node::InstNode)::InstNode
           local scope::InstNode
           @assign scope = begin
             @match node begin
               CLASS_NODE(nodeType = DERIVED_CLASS(__))  => begin
                 parentScope(lastBaseClass(node))
               end
               CLASS_NODE(__)  => begin
                 node.parentScope
               end

               COMPONENT_NODE(__)  => begin
                 parentScope(classInstance(P_Pointer.access(node.component)))
               end

               IMPLICIT_SCOPE(__)  => begin
                 node.parentScope
               end
             end
           end
           scope
         end

function rootTypeParent(nodeType::InstNodeType, node::InstNode) ::InstNode
  local parentVar::InstNode
  @assign parentVar = begin
    @match nodeType begin
      ROOT_CLASS(__) where (! isEmpty(nodeType.parent))  => begin
        nodeType.parent
      end
      DERIVED_CLASS(__)  => begin
        rootTypeParent(nodeType.ty, node)
      end
      _  => begin
        parent(node)
      end
    end
  end
  parentVar
end

function rootParent(node::InstNode) ::InstNode
  local parent::InstNode

  @assign parent = begin
    @match node begin
      CLASS_NODE(__)  => begin
        rootTypeParent(node.nodeType, node)
      end

      _  => begin
        parent(node)
      end
    end
  end
  parent
end

function derivedParent(node::InstNode) ::InstNode
  local parent::InstNode

  @assign parent = begin
    @match node begin
      CLASS_NODE(__)  => begin
        getDerivedNode(node.parentScope)
      end

      COMPONENT_NODE(__)  => begin
        getDerivedNode(node.parent)
      end

      IMPLICIT_SCOPE(__)  => begin
        getDerivedNode(node.parentScope)
      end

      _  => begin
        EMPTY_NODE()
      end
    end
  end
  parent
end

function classParent(node::InstNode) ::InstNode
  local parent::InstNode

  @match CLASS_NODE(parentScope = parent) = node
  parent
end

function explicitParent(node::InstNode) ::InstNode
  local parentNode::InstNode = explicitScope(parent(node))
  parentNode
end

function parent(node::InstNode) ::InstNode
  local parent::InstNode
  @assign parent = begin
    @match node begin
      CLASS_NODE(__)  => begin
        node.parentScope
      end
      COMPONENT_NODE(__)  => begin
        @debug "node was: $(node.name)"
        node.parent
      end
      IMPLICIT_SCOPE(__)  => begin
        node.parentScope
      end
      _  => begin
        EMPTY_NODE()
      end
    end
  end
  parent
end

function rename(name::String, node::InstNode) ::InstNode
  @assign () = begin
    @match node begin
      CLASS_NODE(__)  => begin
        @assign node.name = name
        ()
      end

      COMPONENT_NODE(__)  => begin
        @assign node.name = name
        ()
      end
    end
  end
  node
end

""" #= Returns the type of node the given node is as a string. =#"""
function typeName(node::InstNode) ::String
  local name::String

  @assign name = begin
    @match node begin
      CLASS_NODE(__)  => begin
        "class"
      end

      COMPONENT_NODE(__)  => begin
        "component"
      end

      INNER_OUTER_NODE(__)  => begin
        typeName(node.innerNode)
      end

      REF_NODE(__)  => begin
        "ref node"
      end

      NAME_NODE(__)  => begin
        "name node"
      end

      IMPLICIT_SCOPE(__)  => begin
        "implicit scope"
      end

      EMPTY_NODE(__)  => begin
        "empty node"
      end
    end
  end
  name
end

""" #= Returns the name of a scope, which in the case of a component is the name
                     of the component's type, and for a class simply the name of the class. =#"""
                       function scopeName(node::InstNode) ::String
                         local outName::String = name(classScope(explicitScope(node)))
                         outName
                       end

function className(node::InstNode) ::String
  local name::String

  @match CLASS_NODE(name = name) = node
  name
end


function name(@nospecialize(node::InstNode))::String
  local nameVar::String
  @assign nameVar = begin
    @match node begin
      CLASS_NODE(__)  => begin
        node.name
      end
      COMPONENT_NODE(__)  => begin
        node.name
      end
      INNER_OUTER_NODE(__)  => begin
        name(node.innerNode)
      end
      REF_NODE(__)  => begin
        "REF[" + String(node.index) + "]"
      end
      NAME_NODE(__)  => begin
        node.name
      end
      IMPLICIT_SCOPE(__)  => begin
        "IMPLICIT"
      end
      EXP_NODE(__)  => begin
        "EXP(" + toString(node.exp) + ")"
      end
      EMPTY_NODE(__)  => begin
        "EMPTY"
      end
    end
  end
  #=  For bug catching, these names should never be used. =#
  nameVar
end

function isOperator(node::InstNode) ::Bool
  local op::Bool
  @assign op = begin
    @match node begin
      CLASS_NODE(__)  => begin
        SCodeUtil.isOperator(node.definition)
      end
      INNER_OUTER_NODE(__)  => begin
        isOperator(node.innerNode)
      end
      _  => begin
        false
      end
    end
  end
  op
end

""" #= @author: adrpo
                   returns true if itself or any of the parents are expandable connectors =#"""
                     function hasParentExpandableConnector(node::InstNode) ::Bool
                       local b::Bool = isExpandableConnector(node)

                       local p::InstNode

                       @assign p = node
                       while ! isEmpty(p)
                         @assign p = parent(p)
                         @assign b = boolOr(b, isExpandableConnector(p))
                         if b
                           break
                         end
                       end
                       b
                     end

function isExpandableConnector(node::InstNode) ::Bool
  local isConnector::Bool
  @assign isConnector = begin
    @match node begin
      COMPONENT_NODE(__)  => begin
        isExpandableConnector(component(node))
      end

      _  => begin
        false
      end
    end
  end
  isConnector
end

function isConnector(node::InstNode) ::Bool
  local isConnectorBool::Bool

  @assign isConnectorBool = begin
    @match node begin
      COMPONENT_NODE(__)  => begin
        isConnector(component(node))
      end

      NAME_NODE(__)  => begin
        true
      end

      _  => begin
        false
      end
    end
  end
  isConnectorBool
end

function isName(node::InstNode) ::Bool
  local isName::Bool

  @assign isName = begin
    @match node begin
      NAME_NODE(__)  => begin
        true
      end

      _  => begin
        false
      end
    end
  end
  isName
end

function isImplicit(node::InstNode) ::Bool
  local isImplicit::Bool

  @assign isImplicit = begin
    @match node begin
      IMPLICIT_SCOPE(__)  => begin
        true
      end

      _  => begin
        false
      end
    end
  end
  isImplicit
end

function isEmpty(node::InstNode) ::Bool
  local isEmpty::Bool
  @assign isEmpty = begin
    @match node begin
      EMPTY_NODE(__)  => begin
        true
      end
      _  => begin
        false
      end
    end
  end
  isEmpty
end

function isRef(node::InstNode) ::Bool
  local isRef::Bool

  @assign isRef = begin
    @match node begin
      REF_NODE(__)  => begin
        true
      end

      _  => begin
        false
      end
    end
  end
  isRef
end

function isComponent(node::InstNode) ::Bool
  local isComponent::Bool

  @assign isComponent = begin
    @match node begin
      COMPONENT_NODE(__)  => begin
        true
      end

      INNER_OUTER_NODE(__)  => begin
        isComponent(node.innerNode)
      end

      _  => begin
        false
      end
    end
  end
  isComponent
end

function isFunction(node::InstNode) ::Bool
  local isFunc::Bool

  @assign isFunc = begin
    @match node begin
      CLASS_NODE(__)  => begin
        isFunction(P_Pointer.access(node.cls))
      end

      _  => begin
        false
      end
    end
  end
  isFunc
end

function isDerivedClass(node::InstNode) ::Bool
  local isDerived::Bool

  @assign isDerived = begin
    @match node begin
      CLASS_NODE(nodeType = DERIVED_CLASS(__))  => begin
        true
      end

      _  => begin
        false
      end
    end
  end
  isDerived
end

function isUserdefinedClass(node::InstNode) ::Bool
  local isUserdefined::Bool

  @assign isUserdefined = begin
    @match node begin
      CLASS_NODE(__)  => begin
        begin
          @match node.nodeType begin
            NORMAL_CLASS(__)  => begin
              true
            end

            BASE_CLASS(__)  => begin
              true
            end

            DERIVED_CLASS(__)  => begin
              true
            end

            _  => begin
              false
            end
          end
        end
      end

      _  => begin
        false
      end
    end
  end
  isUserdefined
end

function isBaseClass(node::InstNode) ::Bool
  local isBaseClass::Bool

  @assign isBaseClass = begin
    @match node begin
      CLASS_NODE(nodeType = BASE_CLASS(__))  => begin
        true
      end

      _  => begin
        false
      end
    end
  end
  isBaseClass
end

function isClass(node::InstNode) ::Bool
  local isClass::Bool

  @assign isClass = begin
    @match node begin
      CLASS_NODE(__)  => begin
        true
      end

      INNER_OUTER_NODE(__)  => begin
        isClass(node.innerNode)
      end

      _  => begin
        false
      end
    end
  end
  isClass
end

function fromComponent(name::String, component::Component, parent::InstNode) ::InstNode
  local node::InstNode

  @assign node = COMPONENT_NODE(name, Visibility.PUBLIC, P_Pointer.create(component), parent, NORMAL_COMP())
  node
end

function newExtends(definition::SCode.Element, parent::InstNode) ::InstNode
  local node::InstNode

  local base_path::Absyn.Path
  local name::String
  local vis::SCode.Visibility

  @match SCode.Element.EXTENDS(baseClassPath = base_path, visibility = vis) = definition
  @assign name = AbsynUtil.pathLastIdent(base_path)
  @assign node = CLASS_NODE(name, definition, visibilityFromSCode(vis), P_Pointer.create(NOT_INSTANTIATED()), P_CachedData.empty(), parent, BASE_CLASS(parent, definition))
  node
end

function newComponent(definition::SCode.Element, parent::InstNode = EMPTY_NODE()) ::InstNode
  local node::InstNode

  local name::String
  local vis::SCode.Visibility

  @match SCode.COMPONENT(name = name, prefixes = SCode.PREFIXES(visibility = vis)) = definition
  @assign node = COMPONENT_NODE(name, visibilityFromSCode(vis), P_Pointer.create(new(definition)), parent, NORMAL_COMP())
  node
end

function newClass(definition::SCode.Element, parent::InstNode, nodeType::InstNodeType = NORMAL_CLASS()) ::InstNode
  local node::InstNode
  local name::String
  local vis::SCode.Visibility
  @match SCode.CLASS(name = name, prefixes = SCode.PREFIXES(visibility = vis)) = definition
  @assign node = CLASS_NODE(name, definition, visibilityFromSCode(vis), P_Pointer.create(NOT_INSTANTIATED()), empty(), parent, nodeType)
  node
end

function new(definition::SCode.Element, parent::InstNode) ::InstNode
  local node::InstNode
  @assign node = begin
    @match definition begin
      SCode.CLASS(__)  => begin
        newClass(definition, parent)
      end
      SCode.COMPONENT(__)  => begin
        newComponent(definition, parent)
      end
    end
  end
  node
end

@Uniontype InstNode begin
  @Record EMPTY_NODE begin
  end

  @Record EXP_NODE begin
    exp::Expression
  end

  @Record IMPLICIT_SCOPE begin
    parentScope::InstNode
    locals::List{InstNode}
  end

  @Record NAME_NODE begin
    name::String
  end

  @Record REF_NODE begin
    index::Integer
  end

  @Record INNER_OUTER_NODE begin
    innerNode::InstNode
    outerNode::InstNode
  end

  @Record COMPONENT_NODE begin
    name::String
    visibility
    component::Pointer{Component}
    parent #= The instance that this component is part of. =#::InstNode
    nodeType::InstNodeType
  end

  @Record CLASS_NODE begin
    name::String
    definition::SCode.Element
    visibility
    cls::Pointer
    caches::Array{CachedData}
    parentScope::InstNode
    nodeType::InstNodeType
  end

end
