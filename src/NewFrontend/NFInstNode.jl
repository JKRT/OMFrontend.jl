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

abstract type InstNode end

struct EMPTY_NODE <: InstNode
end

mutable struct EXP_NODE <: InstNode
  exp::Expression
end

mutable struct IMPLICIT_SCOPE <: InstNode
  parentScope::InstNode
  locals::Vector{InstNode}
end

mutable struct NAME_NODE{T0 <: String} <: InstNode
  name::T0
end

mutable struct REF_NODE{T <: Int} <: InstNode
  index::T
end

mutable struct INNER_OUTER_NODE <: InstNode
  innerNode::InstNode
  outerNode::InstNode
end

mutable struct COMPONENT_NODE{T0 <: String, T1 <: Integer} <: InstNode
  name::T0
  visibility::T1
  component::Pointer{Component}
  parent #= The instance that this component is part of. =#::InstNode
  nodeType::InstNodeType
end

mutable struct CLASS_NODE{T0 <: String, T1 <: Integer} <: InstNode
  name::T0
  definition::SCode.Element
  visibility::T1
  cls::Pointer{Class}
  caches::Vector{<:Any}
  parentScope::InstNode
  nodeType::InstNodeType
end

module NodeTree

import ..Main.InstNode

using MetaModelica
using ExportAll
const Key = String
const Value = InstNode
include("../Util/baseAvlTreeCode.jl")

keyCompare::Function = (inKey1::String, inKey2::String) -> begin
  res = stringCompare(inKey1, inKey2)
  return res
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

const NUMBER_OF_CACHES::Int = 3

function setInnerOuterCache(in_caches::Vector{<:CachedData}, in_cache::CachedData)
  local out_caches::Vector{CachedData} = arrayUpdate(in_caches, 3, in_cache)
  out_caches
end

function getInnerOuterCache(in_caches::Vector{<:CachedData})
  local out_cache::CachedData = arrayGet(in_caches, 3)
  out_cache
end

function clearPackageCache(in_caches::Vector{<:CachedData})
  local out_caches::Vector{CachedData} = arrayUpdate(in_caches, 2, C_NO_CACHE())
  out_caches
end

function setPackageCache(in_caches::Vector{<:CachedData}, in_cache::CachedData)
  local out_caches::Vector{CachedData} = arrayUpdate(in_caches, 2, in_cache)
  out_caches
end

function getPackageCache(in_caches::Vector{<:CachedData})
  local out_cache::CachedData = arrayGet(in_caches, 2)
  out_cache
end

function setFuncCache(in_caches::Vector{<:CachedData}, in_cache::CachedData)
  arrayUpdate(in_caches, 1, in_cache)
end

function getFuncCache(in_caches::Vector{<:CachedData})
#  #@debug in_caches
  local out_cache::CachedData = arrayGet(in_caches, 1)
  out_cache
end

function addFunc(fn::M_Function, specialBuiltin::Bool, caches::Vector{<:CachedData})
  local func_cache::CachedData
   func_cache = getFuncCache(caches)
   func_cache = begin
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

function initFunc(caches::Vector{<:CachedData})
  local func_cache::CachedData
   func_cache = getFuncCache(caches)
   func_cache = begin
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

function empty()
  local cache::Vector{CachedData} = arrayCreate(NUMBER_OF_CACHES, C_NO_CACHE())
  cache
end

function hasBinding(@nospecialize(node::InstNode))
  local hb::Bool
  hb = begin
    @match node begin
      COMPONENT_NODE(__)  => begin
        hasBinding(P_Pointer.access(node.component)) || hasBinding(derivedParent(node))
      end
      _  => begin
        false
      end
    end
  end
  hb
end

"""
  Given a node returns the section belonging to the class of the node.
"""
function getSections(node::InstNode)
  local cls = getClass(node)
  sections = @match cls begin
    INSTANCED_CLASS() => cls.sections
    TYPED_DERIVED() =>  getSections(cls.baseClass)
    _ => begin
      Error.assertion(false, getInstanceName() + " did not get an instanced class", sourceInfo());
      fail()
    end
  end
end

"""
Returns true if a node is an instance of a model.
"""
function isModel(node::InstNode)
  local r::Bool
  r = begin
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
  r
end

"""
  Returns true if the node is an instance of a record
"""
function isRecord(@nospecialize(node::InstNode))
  local isRec::Bool
   isRec = begin
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

"""
  Copies the instance pointer of the src node to the destination node.
  After applying this function this component is shared between the two nodes.
"""
function copyInstancePtr(srcNode::InstNode,
                         dstNode::InstNode)
  @match srcNode begin
    COMPONENT_NODE(__) where dstNode isa COMPONENT_NODE  => begin
      dstNode.component = srcNode.component
    end
    CLASS_NODE(__) where dstNode isa CLASS_NODE => begin
      dstNode.cls = srcNode.cls
    end
  end
  dstNode
end

function getComments(node::InstNode, accumCmts::List{<:SCode.Comment} = nil)
  local cmts::List{SCode.Comment}

   cmts = begin
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

function clone(@nospecialize(node::InstNode))
  local cls::Class
  local clonedNode::InstNode
  clonedNode = if node isa CLASS_NODE
    cls = P_Pointer.access(node.cls)
    cls = classTreeApply(cls, clone)
    #= !NB should be assign here! =#
    local nodeClassPtr = P_Pointer.create(cls)
    CLASS_NODE{String, Int}(node.name,
                            node.definition,
                            node.visibility,
                            nodeClassPtr,
                            empty(),
                            node.parentScope,
                            node.nodeType)
  end
  return clonedNode
end

function isPartial(node::InstNode)
  local isPartial::Bool
   isPartial = begin
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

function isBuiltin(node::InstNode)
  local isBuiltin::Bool
   isBuiltin = begin
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

""" Returns the DAE type for a class, with the list of variables filled in. """
function toFullDAEType(clsNode::InstNode)
  local outType::DAE.Type

   outType = begin
    local cls::Class
    local vars::List{DAE.Var}
    local state::ClassInf.State
    @match clsNode begin
      CLASS_NODE(__)  => begin
         cls = P_Pointer.access(clsNode.cls)
        begin
          @match cls begin
            DAE_TYPE(__)  => begin
              cls.ty
            end
            _  => begin
              state = toDAE(restriction(cls), scopePath(clsNode, includeRoot = true))
              vars = makeTypeVars(clsNode)
              outType = DAE.T_COMPLEX(state, vars, NONE())
              P_Pointer.update(clsNode.cls, DAE_TYPE(outType))
              outType
            end
          end
        end
      end
    end
  end
  outType
end

function stripDAETypeVars(ty::DAE.Type)
  @match ty begin
    DAE.T_COMPLEX(__)  => begin
      ty = DAE.T_COMPLEX(ty.complexClassType, ty.varLst, ty.equalityConstraint)
    end
    _  => begin
      ty
    end
  end
end

""" #= Returns the DAE type for a class, without the list of variables filled in. =#"""
function toPartialDAEType(clsNode::InstNode)
  local outType::DAE.Type

   outType = begin
    local cls::Class
    local state::ClassInf.SMNode
    @match clsNode begin
      CLASS_NODE(__)  => begin
         cls = P_Pointer.access(clsNode.cls)
        begin
          @match cls begin
            DAE_TYPE(__)  => begin
              stripDAETypeVars(cls.ty)
            end

            _  => begin
               state = toDAE(restriction(cls), scopePath(clsNode, includeRoot = true))
              DAE.T_COMPLEX(state, nil, NONE())
            end
          end
        end
      end
    end
  end
  outType
end

function setModifier(mod::Modifier, node::InstNode)


   () = begin
    @match node begin
      CLASS_NODE(__)  => begin
        P_Pointer.update(node.cls, setModifier(mod, P_Pointer.access(node.cls)))
        ()
      end

      COMPONENT_NODE(__)  => begin
        P_Pointer.update(node.component, mergeModifier(mod, P_Pointer.access(node.component)))
        ()
      end

      _  => begin
        ()
      end
    end
  end
  node
end

function mergeModifier(mod::Modifier, node::InstNode)


   () = begin
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

# function getModifier(node::InstNode)
#   local mod::Modifier

#    mod = begin
#     @match node begin
#       CLASS_NODE(__)  => begin
#         getModifier(P_Pointer.access(node.cls))
#       end

#       COMPONENT_NODE(__)  => begin
#         getModifier(P_Pointer.access(node.component))
#       end

#       _  => begin
#         MODIFIER_NOMOD()
#       end
#     end
#   end
#   mod
# end


function getModifier(node::InstNode)
  local mod
  mod = if node isa CLASS_NODE
    getModifier(P_Pointer.access(node.cls))
  elseif node isa COMPONENT_NODE
    getModifier(P_Pointer.access(node.component))
  else
    MODIFIER_NOMOD()
  end
end

function protectComponent(comp::InstNode)
   () = begin
    @match comp begin
      COMPONENT_NODE(visibility = Visibility.PUBLIC)  => begin
        comp.visibility = Visibility.PROTECTED
        ()
      end

      _  => begin
        ()
      end
    end
  end
  comp
end

function protectClass(cls::InstNode)
   () = begin
    @match cls begin
      CLASS_NODE(visibility = Visibility.PUBLIC)  => begin
        cls.visibility = Visibility.PROTECTED
        ()
      end

      _  => begin
        ()
      end
    end
  end
  cls
end

function isProtected(node::InstNode)
  local isProtected::Bool

   isProtected = begin
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

function visibility(node::InstNode)
  local vis::VisibilityType

   vis = begin
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

function isProtectedBaseClass(node::InstNode)
  local isProtected::Bool

   isProtected = begin
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

function isRedeclare(node::InstNode)
  local isRedcl::Bool = false
   isRedcl = begin
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
   s = begin
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

function toFlatString(node::InstNode)
  local name::String
   name = begin
    @match node begin
      COMPONENT_NODE(__)  => begin
        toFlatString(node.name, P_Pointer.access(node.component))
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

function toString(node::InstNode)
  local name::String
   name = begin
    @match node begin
      COMPONENT_NODE(__)  => begin
        toString(node.name, P_Pointer.access(node.component))
      end
      CLASS_NODE(__)  => begin
        #SCodeDump.unparseElementStr(node.definition)
        "CLASS_NODE: " * node.name
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
   () = begin
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

function isSame(node1::InstNode, node2::InstNode)
  local same::Bool = false
  local n1::InstNode = resolveOuter(node1)
  local n2::InstNode = resolveOuter(node2)
  if referenceEq(n1, n2)
     same = true
    return same
  elseif stringEqual(name(n1), name(n2))
     same = true
    return same
  end
  #=  TODO: This is not enough. We need a better way.
  =#
  same
end

function nameEqual(node1::InstNode, node2::InstNode)
  local equal::Bool = name(node1) == name(node2)
  equal
end

function refCompare(node1::InstNode, node2::InstNode)
  local res::Int
   res = begin
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
function refEqual(node1::InstNode, node2::InstNode)
  local refEqualIs::Bool
  refEqualIs = begin
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

function addIterator(iterator::InstNode, scope::InstNode)
  scope = begin
    @match scope begin
      IMPLICIT_SCOPE(__)  => begin
        IMPLICIT_SCOPE(scope, prepend!([iterator], scope.locals))
      end
    end
  end
  scope
end

""" #= Returns the first parent of the node that's not an implicit scope, or the
                     node itself if it's not an implicit scope. =#"""
function explicitScope(node::InstNode)
  local scope::InstNode
  scope = begin
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

function openImplicitScope(scope::InstNode)
  scope = begin
    @match scope begin
      IMPLICIT_SCOPE(__)  => begin
        scope
      end
      _  => begin
        IMPLICIT_SCOPE(scope, InstNode[])
      end
    end
  end
  scope
end

function setInnerOuterCache(node::InstNode, in_out_cache::CachedData)
   () = begin
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

function getInnerOuterCache(inNode::InstNode)
  local pack_cache::CachedData
   pack_cache = begin
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

function clearPackageCache(node::InstNode)
   () = begin
    @match node begin
      CLASS_NODE(__)  => begin
        clearPackageCache(node.caches)
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

function setPackageCache(node::InstNode, in_pack_cache::CachedData)
   () = begin
    @match node begin
      CLASS_NODE(__)  => begin
        setPackageCache(node.caches, in_pack_cache)
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

function getPackageCache(inNode::InstNode)
  local pack_cache::CachedData

   pack_cache = begin
    @match inNode begin
      CLASS_NODE(__)  => begin
        getPackageCache(inNode.caches)
      end

      _  => begin
        Error.assertion(false, getInstanceName() + " got node without cache", sourceInfo())
        fail()
      end
    end
  end
  pack_cache
end

function setFuncCache(node::InstNode, in_func_cache::CachedData)
   () = begin
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

function getFuncCache(inNode::InstNode)
  local func_cache::CachedData
   func_cache = begin
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

function cacheAddFunc(node::InstNode, fn::M_Function, specialBuiltin::Bool)
   () = begin
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

function cacheInitFunc(node::InstNode)
   () = begin
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

function resolveOuter(node::InstNode)
  local outerNode::InstNode

   outerNode = begin
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

function resolveInner(node::InstNode)
  local innerNode::InstNode

   innerNode = begin
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

function isInnerOuterNode(node::InstNode)
  #@debug "is inner outer node?"
  local isIO::Bool
   isIO = begin
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

function isOnlyOuter(node::InstNode)
  local isOuter::Bool

   isOuter = begin
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

function isOuter(node::InstNode)
  local isOuter::Bool

   isOuter = begin
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

function isInner(node::InstNode)
  local isInner::Bool

   isInner = begin
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

function isOutput(node::InstNode)
  local isOutput::Bool

   isOutput = begin
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

function isInput(node::InstNode)
  local isInput::Bool

   isInput = begin
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

function scopePathClass(node::InstNode, ty::InstNodeType, includeRoot::Bool, accumPath::Absyn.Path)
  local path::Absyn.Path

   path = begin
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

function scopePath2(node::InstNode, includeRoot::Bool, accumPath::Absyn.Path)
  local path::Absyn.Path

   path = begin
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

function scopePath(node::InstNode; includeRoot::Bool = false #= Whether to include the root class name or not. =#)
  local path::Absyn.Path

   path = begin
    local it::InstNodeType
    @match node begin
      CLASS_NODE(nodeType = it)  => begin
        begin
          @match it begin
            BASE_CLASS(__)  => begin
              scopePath(it.parent; includeRoot = includeRoot)
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

function scopeListClass(clsNode::InstNode, ty::InstNodeType, includeRoot::Bool, accumScopes::List{<:InstNode} = nil)
  local scopes::List{InstNode}

   scopes = begin
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

function scopeList(node::InstNode, includeRoot::Bool, accumScopes::List{<:InstNode} = nil)
  scopeList(node, includeRoot = includeRoot, accumScopes = accumScopes)
end

function scopeList(node::InstNode; includeRoot::Bool = false #= Whether to include the root class name or not. =#, accumScopes::List{<:InstNode} = nil)
  local scopes::List{InstNode}

   scopes = begin
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
   () = begin
    @match node begin
      COMPONENT_NODE(__)  => begin
        P_Pointer.update(node.component, func(arg, P_Pointer.access(node.component)))
        ()
      end
    end
  end
  node
end

function classApply(node::InstNode, func::FuncType, arg::ArgT)  where {ArgT}
   () = begin
    @match node begin
      CLASS_NODE(__)  => begin
        P_Pointer.update(node.cls, func(arg, P_Pointer.access(node.cls)))
        ()
      end
    end
  end
  node
end

function getType(node::InstNode)
  local ty::M_Type
   ty = begin
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

function InstNode_info(node::InstNode)
  local infoV::SourceInfo

   infoV = begin
    local ty::InstNodeType
    @matchcontinue node begin
      CLASS_NODE(nodeType = ty && BASE_CLASS(__))  => begin
        SCodeUtil.elementInfo(ty.definition)
      end

      CLASS_NODE(__)  => begin
        SCodeUtil.elementInfo(node.definition)
      end

      COMPONENT_NODE(__)  => begin
        Component_info(P_Pointer.access(node.component))
      end

      COMPONENT_NODE(__)  => begin
        InstNode_info(node.parent)
      end
      _  => begin
        AbsynUtil.dummyInfo
      end
    end
  end
  infoV
end

function setDefinition(definition::SCode.Element, node::InstNode)
   () = begin
    @match node begin
      CLASS_NODE(__)  => begin
        node.definition = definition
        ()
      end
    end
  end
  node
end

function definition(node::InstNode)
  local def::SCode.Element
  def = begin
    @match node begin
      CLASS_NODE(__)  => begin
        node.definition
      end
      COMPONENT_NODE(__)  => begin
        definition(P_Pointer.access(node.component))
      end
    end
  end
  def
end

function setNodeType(@nospecialize(nodeType::InstNodeType),
                     @nospecialize(node::InstNode))

  local newNode = if node isa COMPONENT_NODE
    COMPONENT_NODE{String, Int}(node.name,
                                node.visibility,
                                node.component,
                                node.parent,
                                nodeType)
  elseif node isa CLASS_NODE
    CLASS_NODE{String, Int}(node.name,
                            node.definition,
                            node.visibility,
                            node.cls,
                            node.caches,
                            node.parentScope,
                            nodeType)
  else
    node
  end
  newNode
end

function nodeType(node::InstNode)
  local nodeType::InstNodeType
   nodeType = begin
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

function replaceClass(cls::Class, node::InstNode)
  local replacedClass = if node isa CLASS_NODE
    local classPtr = P_Pointer.create(cls)
    CLASS_NODE{String, Int}(node.name,
                            node.definition,
                            node.visibility,
                            classPtr,
                            node.caches,
                            node.parentScope,
                            node.nodeType)
  else
    node
  end
  return replacedClass
end


function replaceComponent(component::Component, node::InstNode)
  local replacedNode =  if node isa COMPONENT_NODE
    local componentPointer = P_Pointer.create(component)
    COMPONENT_NODE{String, Int}(node.name,
                                node.visibility,
                                componentPointer,
                                node.parent,
                                node.nodeType)
  else
    node
  end
  return replacedNode
end

function updateComponent!(component::Component, node::InstNode)
   node = begin
    @match node begin
      COMPONENT_NODE(__)  => begin
        P_Pointer.update(node.component, component)
        node
      end
    end
  end
  node
end

function component(node::InstNode)
  local component::Component

   component = begin
    @match node begin
      COMPONENT_NODE(__)  => begin
        P_Pointer.access(node.component)
      end
    end
  end
  component
end

function updateClass(cls::Class, node::InstNode)
   node = begin
    @match node begin
      CLASS_NODE(__)  => begin
        P_Pointer.update(node.cls, cls)
        node
      end
    end
  end
  node
end

function getDerivedNode(node::InstNode)
  local derived::InstNode

   derived = begin
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

# function getDerivedClass(node::InstNode)
#   local cls::Class

#    cls = begin
#     @match node begin
#       CLASS_NODE(__)  => begin
#         getClass(getDerivedNode(node))
#       end

#       COMPONENT_NODE(__)  => begin
#         getClass(getDerivedNode(P_Component.classInstance(P_Pointer.access(node.component))))
#       end
#     end
#   end
#   cls
# end


function getDerivedClass(node::InstNode)
   cls =  if cls isa CLASS_NODE
     getClass(getDerivedNode(node))
   elseif cls isa COMPONENT_NODE
     getClass(getDerivedNode(P_Component.classInstance(P_Pointer.access(node.component))))
   else
     fail()
   end
  cls
end


function getClass(node::InstNode)
  cls = if node isa CLASS_NODE
    P_Pointer.access(node.cls)
  elseif  node isa COMPONENT_NODE
    getClass(classInstance(P_Pointer.access(node.component)))
  else
    fail()
  end
end

function setOrphanParent(parent::InstNode, node::CLASS_NODE)
  if node.parentScope isa EMPTY_NODE
    CLASS_NODE{String, Int}(node.name,
                            node.definition,
                            node.visibility,
                            node.cls,
                            node.caches,
                            parent,
                            node.nodeType)
  else
    node
  end
end

function setOrphanParent(parent::InstNode, node::COMPONENT_NODE)
  if node.parent isa EMPTY_NODE
    COMPONENT_NODE{String, Int}(node.name,
                                node.visibility,
                                node.component,
                                parent,
                                node.nodeType)
  else
    node
  end
end

function setParent(@nospecialize(parent::InstNode),
                    node::CLASS_NODE)
  CLASS_NODE{String, Int}(node.name,
                          node.definition,
                          node.visibility,
                          node.cls,
                          node.caches,
                          parent,
                          node.nodeType)
end

function setParent(@nospecialize(parent::InstNode),
                    node::COMPONENT_NODE)
  COMPONENT_NODE{String, Int}(node.name,
                              node.visibility,
                              node.component,
                              parent,
                              node.nodeType)
end

function setParent(@nospecialize(parent::InstNode),
                    node::IMPLICIT_SCOPE)
  IMPLICIT_SCOPE(parent, node.locals)
end

function topComponent(node::InstNode)
  local topComponent::InstNode
   topComponent = begin
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

function topScope(node::InstNode)
  local ts::InstNode
   ts = begin
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

function classScope(node::InstNode)
  local scope::InstNode
   scope = begin
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

"""
  Returns the parent scope of a node. In the case of a class this is simply
  the enclosing class. In the case of a component it is the enclosing class of
  the component's type.
"""
function parentScope(@nospecialize(node::InstNode))
  local scope::InstNode
   scope = begin
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

function rootTypeParent(nodeType::InstNodeType, node::InstNode)
  local parentVar::InstNode
   parentVar = begin
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

function rootParent(node::InstNode)
  local parent::InstNode
   parent = begin
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

function derivedParent(node::InstNode)
  local parent::InstNode
   parent = begin
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

function classParent(node::InstNode)
  local parent::InstNode
  @match CLASS_NODE(parentScope = parent) = node
  parent
end

function explicitParent(node::InstNode)
  local parentNode::InstNode = explicitScope(parent(node))
  parentNode
end

function parent(node::InstNode)
  local parent::InstNode
   parent = begin
    @match node begin
      CLASS_NODE(__)  => begin
        node.parentScope
      end
      COMPONENT_NODE(__)  => begin
        #@debug "node was: $(node.name)"
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

function rename(name::String, node::InstNode)
   () = begin
    @match node begin
      CLASS_NODE(__)  => begin
        node.name = name
        ()
      end

      COMPONENT_NODE(__)  => begin
        node.name = name
        ()
      end
    end
  end
  node
end

""" #= Returns the type of node the given node is as a string. =#"""
function typeName(node::InstNode)
  local name::String
   name = begin
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

"""
Returns the name of a scope, which in the case of a component is the name
of the component's type, and for a class simply the name of the class.
"""
function scopeName(node::InstNode)
  local outName::String = name(classScope(explicitScope(node)))
  outName
end

function className(node::InstNode)
  local name::String
  @match CLASS_NODE(name = name) = node
  name
end

function name(@nospecialize(node::InstNode))
  local nameVar::String
   nameVar = begin
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
        "REF[" + string(node.index) + "]"
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
  return nameVar
end

function isOperator(node::InstNode)
  local op::Bool
   op = begin
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

"""
 @author: adrpo
returns true if itself or any of the parents are expandable connectors
"""
function hasParentExpandableConnector(node::InstNode)
  local b::Bool = isExpandableConnector(node)
  local p::InstNode
  p = node
  while ! isEmpty(p)
    p = parent(p)
    b = boolOr(b, isExpandableConnector(p))
    if b
      break
    end
  end
  b
end

function isExpandableConnector(node::InstNode)
  local isConnector::Bool
   isConnector = begin
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

function isConnector(node::InstNode)
  local isConnectorBool::Bool

   isConnectorBool = begin
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

function isName(node::InstNode)
  local isName::Bool

   isName = begin
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

function isImplicit(node::InstNode)
  local isImplicit::Bool

   isImplicit = begin
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

function isEmpty(node::InstNode)
  local isEmpty::Bool
   isEmpty = begin
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

function isRef(node::InstNode)
  local isRef::Bool

   isRef = begin
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

function isComponent(node::InstNode)
  local isComponent::Bool

   isComponent = begin
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

function isFunction(node::InstNode)
  local isFunc::Bool

   isFunc = begin
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

function isDerivedClass(node::InstNode)
  local isDerived::Bool

   isDerived = begin
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

function isUserdefinedClass(node::InstNode)
  local isUserdefined::Bool

   isUserdefined = begin
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

function isBaseClass(node::InstNode)
  local isBaseClass::Bool

   isBaseClass = begin
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

function isClass(node::InstNode)
  local isClass::Bool

   isClass = begin
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

function fromComponent(name::String, component::Component, parent::InstNode)
  local node::InstNode

   node = COMPONENT_NODE(name, Visibility.PUBLIC, P_Pointer.create(component), parent, NORMAL_COMP())
  node
end

function newExtends(definition::SCode.Element, parent::InstNode)
  local node::InstNode

  local base_path::Absyn.Path
  local name::String
  local vis::SCode.Visibility

  @match SCode.EXTENDS(baseClassPath = base_path, visibility = vis) = definition
  name = AbsynUtil.pathLastIdent(base_path)
  node = CLASS_NODE(name, definition, visibilityFromSCode(vis), P_Pointer.create(NOT_INSTANTIATED()), #=P_CachedData.=#empty(), parent, BASE_CLASS(parent, definition))
  node
end

function newComponent(definition::SCode.Element, parent::InstNode = EMPTY_NODE())
  local node::InstNode

  local name::String
  local vis::SCode.Visibility

  @match SCode.COMPONENT(name = name, prefixes = SCode.PREFIXES(visibility = vis)) = definition
   node = COMPONENT_NODE(name, visibilityFromSCode(vis), P_Pointer.create(new(definition)), parent, NORMAL_COMP())
  node
end

function newClass(definition::SCode.Element, parent::InstNode, nodeType::InstNodeType = NORMAL_CLASS())
  local node::InstNode
  local name::String
  local vis::SCode.Visibility
  @match SCode.CLASS(name = name, prefixes = SCode.PREFIXES(visibility = vis)) = definition
   node = CLASS_NODE(name, definition, visibilityFromSCode(vis), P_Pointer.create(NOT_INSTANTIATED()), empty(), parent, nodeType)
  node
end

function new(definition::SCode.Element, parent::InstNode)
  local node::InstNode
   node = begin
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
