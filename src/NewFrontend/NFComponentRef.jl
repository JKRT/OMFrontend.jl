const Origin = (() -> begin #= Enumeration =#
  CREF = 1  #= From an Absyn cref. =#
  SCOPE = 2  #= From prefixing the cref with its scope. =#
  ITERATOR = 3  #= From an iterator. =#
  () -> (CREF; SCOPE; ITERATOR)  #= From an iterator. =#
end)()
const OriginType = Int

abstract type NFComponentRef end

const ComponentRef = NFComponentRef

struct COMPONENT_REF_WILD <: NFComponentRef end

struct COMPONENT_REF_EMPTY <: NFComponentRef end

struct COMPONENT_REF_STRING{T0 <: String,
                            T1 <: ComponentRef} <: NFComponentRef
  name::T0
  restCref::T1
end

struct COMPONENT_REF_CREF <: NFComponentRef
  node::InstNode
  subscripts::List{Subscript}
  ty::NFType #= The type of the node, without taking subscripts into account. =#
  origin::Integer
  restCref::ComponentRef
end

function isComplexArray2(cref::ComponentRef)::Bool
  local complexArray::Bool
   complexArray = begin
    @match cref begin
      COMPONENT_REF_CREF(
        ty = TYPE_ARRAY(__),
      ) where {(isArray(Type.subscript(cref.ty, cref.subscripts)))} => begin
        true
      end

      COMPONENT_REF_CREF(__) => begin
        isComplexArray2(cref.restCref)
      end

      _ => begin
        false
      end
    end
  end
  return complexArray
end

function isComplexArray(cref::ComponentRef)::Bool
  local complexArray::Bool

   complexArray = begin
    @match cref begin
      COMPONENT_REF_CREF(__) => begin
        isComplexArray2(cref.restCref)
      end

      _ => begin
        false
      end
    end
  end
  return complexArray
end

function depth(cref::ComponentRef)
  local d::Int = 0
  d = begin
    @match cref begin
      COMPONENT_REF_CREF(restCref = COMPONENT_REF_EMPTY(__)) => begin
        d + 1
      end
      COMPONENT_REF_CREF(__) => begin
        1 + depth(cref.restCref)
      end
      COMPONENT_REF_WILD(__) => begin
        0
      end
      _ => begin  #= COMPONENT_REF_EMPTY_COMPONENT_REF_CREF =#
        0
      end
    end
  end
  return d
end


"""
```
toListReverse(
  cref::COMPONENT_REF_CREF,
  accum::List{<:ComponentRef} = nil)
```
@author:johti17
"""
function toListReverse(
  cref::COMPONENT_REF_CREF,
  accum::List{<:ComponentRef} = nil)
  local tmp = cref
  while tmp isa COMPONENT_REF_CREF
    accum = Cons{ComponentRef}(tmp, accum)
    tmp = tmp.restCref
  end
  return accum
end
toListReverse(cref::ComponentRef, accum::List{<:ComponentRef} = nil) = accum

function toString(crefLst::List{ComponentRef})
  local buffer = IOBuffer()
  print("list{ComponentRef}[")
  for i in crefLst
    print(buffer, toString(i) * ",")
  end
  print("]")
end


function isFromCref(cref::ComponentRef)::Bool
  local fromCref::Bool

   fromCref = begin
    @match cref begin
      COMPONENT_REF_CREF(origin = Origin.CREF) => begin
        true
      end

      COMPONENT_REF_WILD(__) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return fromCref
end

function isDeleted(cref::ComponentRef)::Bool
  local isDeletedBool::Bool

   isDeletedBool = begin
    local node::InstNode
    @match cref begin
      COMPONENT_REF_CREF(node = node, origin = Origin.CREF) => begin
        isComponent(node) && isDeleted(component(node))
      end

      _ => begin
        false
      end
    end
  end
  return isDeletedBool
end

function evaluateSubscripts(cref::ComponentRef)::ComponentRef
  cref = begin
    local subs::List{Subscript}
    @match cref begin
      COMPONENT_REF_CREF(subscripts = nil(), origin = Origin.CREF) => begin
        COMPONENT_REF_CREF(cref.node, cref.subscripts, cref.ty, cref.origin, evaluateSubscripts(cref.restCref))
      end

      COMPONENT_REF_CREF(origin = Origin.CREF) => begin
        subs = list(eval(s) for s in cref.subscripts)
        COMPONENT_REF_CREF(cref.node, subs, cref.ty, cref.origin, evaluateSubscripts(cref.restCref))
      end

      _ => begin
        cref
      end
    end
  end
  return cref
end



function simplifySubscripts(cref::ComponentRef; trim = false)::ComponentRef
  cref = begin
    local subs::List{Subscript}
    @match cref begin
      COMPONENT_REF_CREF(subscripts = nil(), origin = Origin.CREF) => begin
        COMPONENT_REF_CREF(cref.node, cref.subscripts, cref.ty, cref.origin, simplifySubscripts(cref.restCref, trim = trim))
      end
      COMPONENT_REF_CREF(origin = Origin.CREF) => begin
        subs = list(simplifySubscript(s) for s in cref.subscripts)
        COMPONENT_REF_CREF(cref.node, subs, cref.ty, cref.origin, simplifySubscripts(cref.restCref, trim = trim))
      end
      _ => begin
        cref
      end
    end
  end
  return cref
end

""" #= Strips all subscripts from a cref. =#"""
function stripSubscriptsAll(cref::ComponentRef)::ComponentRef
  local strippedCref::ComponentRef
   strippedCref = begin
    @match cref begin
      COMPONENT_REF_CREF(__) => begin
        COMPONENT_REF_CREF(cref.node, nil, cref.ty, cref.origin, stripSubscriptsAll(cref.restCref))
      end

      _ => begin
        cref
      end
    end
  end
  return strippedCref
end

""" #= Strips the subscripts from the last name in a cref, e.g. a[2].b[3] => a[2].b =#"""
function stripSubscripts(cref::ComponentRef)::Tuple{ComponentRef, List{Subscript}}
  local subs::List{Subscript}
  local strippedCref::ComponentRef

   (strippedCref, subs) = begin
    @match cref begin
      COMPONENT_REF_CREF(__) => begin
        (COMPONENT_REF_CREF(cref.node, nil, cref.ty, cref.origin, cref.restCref), cref.subscripts)
      end

      _ => begin
        (cref, nil)
      end
    end
  end
  return (strippedCref, subs)
end

function isPackageConstant2(cref::ComponentRef)::Bool
  local isPkgConst::Bool

   isPkgConst = begin
    @match cref begin
      COMPONENT_REF_CREF(node = CLASS_NODE(__)) => begin
        isUserdefinedClass(cref.node)
      end

      COMPONENT_REF_CREF(origin = Origin.CREF) => begin
        isPackageConstant2(cref.restCref)
      end

      _ => begin
        false
      end
    end
  end
  return isPkgConst
end

function isPackageConstant(cref::ComponentRef)::Bool
  local isPkgConst::Bool

  #=  TODO: This should really be CONSTANT and not PARAMETER, but that breaks
  =#
  #=        some models since we get some redeclared parameters that look like
  =#
  #=        package constants due to redeclare issues, and which need to e.g.
  =#
  #=        be collected by Package.collectConstants.
  =#
   isPkgConst =
    nodeVariability(cref) <= Variability.PARAMETER && isPackageConstant2(cref)
  return isPkgConst
end

function scalarize(cref::ComponentRef)::List{ComponentRef}
  local crefs::List{ComponentRef}

   crefs = begin
    local dims::List{Dimension}
    local subs::List{List{Subscript}}
    @match cref begin
      COMPONENT_REF_CREF(ty = TYPE_ARRAY(__)) => begin
        dims = arrayDims(cref.ty)
        subs = scalarizeList(cref.subscripts, dims)
        subs = ListUtil.combination(subs)
        list(setSubscripts(s, cref) for s in subs)
      end

      _ => begin
        list(cref)
      end
    end
  end
  return crefs
end

function fromNodeList(nodes::List{<:InstNode})::ComponentRef
  local cref::ComponentRef = COMPONENT_REF_EMPTY()
  for n in nodes
    cref = COMPONENT_REF_CREF(n, nil, getType(n), Origin.SCOPE, cref)
  end
  return cref
end

function toPath_impl(cref::ComponentRef, accumPath::Absyn.Path)::Absyn.Path
  local path::Absyn.Path

   path = begin
    @match cref begin
      COMPONENT_REF_CREF(__) => begin
        toPath_impl(cref.restCref, Absyn.QUALIFIED(name(cref.node), accumPath))
      end

      _ => begin
        accumPath
      end
    end
  end
  return path
end

function toPath(cref::ComponentRef)::Absyn.Path
  local path::Absyn.Path

   path = begin
    @match cref begin
      COMPONENT_REF_CREF(__) => begin
        toPath_impl(cref.restCref, Absyn.IDENT(name(cref.node)))
      end
    end
  end
  return path
end

function hash(cref::ComponentRef, mod::Int)::Int
  local hash::Int = stringHashDjb2Mod(toString(cref), mod)
  return hash
end

function listToString(crs::List{<:ComponentRef})::String
  local str::String
  str = "{" + stringDelimitList(ListUtil.map(crs, toString), ",") + "}"
  return str
end

function toFlatString_impl(cref::ComponentRef, strl::List{<:String})::List{String}
  strl = begin
    local str::String
    @match cref begin
      COMPONENT_REF_CREF(__) => begin
        str =
          name(cref.node) +
          toFlatStringList(cref.subscripts)
        if isRecord(cref.ty) && !listEmpty(strl)
          strl = _cons("'" + listHead(strl), listRest(strl))
          str = str + "'"
        end
        toFlatString_impl(cref.restCref, _cons(str, strl))
      end
      COMPONENT_REF_WILD(__) => begin
        _cons("_", strl)
      end
      COMPONENT_REF_STRING(__) => begin
        toFlatString_impl(cref.restCref, _cons(cref.name, strl))
      end
      _ => begin
        strl
      end
    end
  end
  return strl
end

function toFlatString(cref::ComponentRef)::String
  local str::String
  local cr::ComponentRef
  local subs::List{Subscript}
  local strl::List{String} = nil
  (cr, subs) = stripSubscripts(cref)
  strl = toFlatString_impl(cr, strl)
  #Special case
  if !Flags.isSet(Flags.NF_SCALARIZE)
    str = stringAppendList(list(
      "'",
      stringDelimitList(strl, "."),
      "'",
      toFlatStringList(subs)
    ))
  else
    str = stringAppendList(
      list("'", stringDelimitList(strl, "."), toFlatStringList(subs), "'",))
  end
  if str  == "'time'"
    return "time"
  end
  return str
end

function toString_impl(cref::ComponentRef, strl::List{<:String})
  strl = begin
    local str::String
    @match cref begin
      COMPONENT_REF_CREF(__) => begin
         str =
          name(cref.node) + toStringList(cref.subscripts)
        toString_impl(cref.restCref, _cons(str, strl))
      end

      COMPONENT_REF_WILD(__) => begin
        _cons("_", strl)
      end

      COMPONENT_REF_STRING(__) => begin
        toString_impl(cref.restCref, _cons(cref.name, strl))
      end

      _ => begin
        strl
      end
    end
  end
  return strl
end


# WIP: John
# function toStringDelim(cref::ComponentRef, delim::String, buffer = IOBuffer())
#   @match cref begin
#     COMPONENT_REF_CREF(__) where !(cref.restCref isa  COMPONENT_REF_EMPTY)  => begin
#       print(buffer, name(cref.node), toStringDelim(cref.restCref, delim, buffer), "")
#     end
#     COMPONENT_REF_CREF(__) => begin
#       print(buffer, string(name(cref.node), toStringList(cref.subscripts)))
#     end
#     COMPONENT_REF_WILD(__) => begin
#       print(buffer, "_")
#     end
#     COMPONENT_REF_STRING(__) => begin
#       print(buffer, toStringDelim(cref.restCref, delim, buffer), delim)
#     end
#     _ => begin
#       buffer
#     end
#   end
#   return String(take!(buffer))
# end


function toString(cref::ComponentRef)
  local str = stringDelimitList(toString_impl(cref, nil), "_")
  return str
end

function toDAE_impl(
  cref::ComponentRef,
  accumCref::DAE.ComponentRef
)::DAE.ComponentRef
  local dcref::DAE.ComponentRef

   dcref = begin
    local ty::M_Type
    local dty::DAE.Type
    @match cref begin
      COMPONENT_REF_EMPTY(__) => begin
        accumCref
      end
      COMPONENT_REF_CREF(__) => begin
         ty = if isUnknown(cref.ty)
          getType(cref.node)
        else
          cref.ty
        end
         dty = toDAE(ty, makeTypeVars = false)
         dcref = DAE.CREF_QUAL(
          name(cref.node),
          dty,
          list(toDAE(s) for s in cref.subscripts),
          accumCref,
        )
        toDAE_impl(cref.restCref, dcref)
      end
    end
  end
  return dcref
end

function toDAE(cref::ComponentRef)::DAE.ComponentRef
  local dcref::DAE.ComponentRef
   dcref = begin
    @match cref begin
      COMPONENT_REF_CREF(__) => begin
         dcref = DAE.CREF_IDENT(
          name(cref.node),
          toDAE(cref.ty),
          list(toDAE(s) for s in cref.subscripts),
        )
        toDAE_impl(cref.restCref, dcref)
      end

      COMPONENT_REF_WILD(__) => begin
        DAE.WILD()
      end
      COMPONENT_REF_STRING(__) => begin
        DAE.CREF_IDENT(cref.name, DAE.T_UNKNOWN(), nil)
      end
    end
  end
  return dcref
end

function isPrefix(cref1::ComponentRef, cref2::ComponentRef)::Bool
  local isPrefix::Bool

  if referenceEq(cref1, cref2)
     isPrefix = true
    return isPrefix
  end
   isPrefix = begin
    @match (cref1, cref2) begin
      (COMPONENT_REF_CREF(__), COMPONENT_REF_CREF(__)) => begin
        if name(cref1.node) == name(cref2.node)
          isEqual(cref1.restCref, cref2.restCref)
        else
          isEqual(cref1, cref2.restCref)
        end
      end

      _ => begin
        false
      end
    end
  end
  return isPrefix
end

function isGreater(cref1::ComponentRef, cref2::ComponentRef)::Bool
  local isGreater::Bool = compare(cref1, cref2) > 0
  return isGreater
end

function isLess(cref1::ComponentRef, cref2::ComponentRef)::Bool
  local isLess::Bool = compare(cref1, cref2) < 0
  return isLess
end

function isEqual(cref1::ComponentRef, cref2::ComponentRef)::Bool
  local isEqualB::Bool = false
  if referenceEq(cref1, cref2)
    return true
  end
  isEqualB = begin
    @match (cref1, cref2) begin
      (COMPONENT_REF_CREF(__), COMPONENT_REF_CREF(__)) => begin
        name(cref1.node) == name(cref2.node) &&
        isEqualList(cref1.subscripts, cref2.subscripts) &&
        isEqual(cref1.restCref, cref2.restCref)
      end

      (COMPONENT_REF_EMPTY(__), COMPONENT_REF_EMPTY(__)) => begin
        true
      end

      (COMPONENT_REF_WILD(__), COMPONENT_REF_WILD(__)) => begin
        true
      end
      _ => begin
        false
      end
    end
  end
  return isEqualB
end

function compare(cref1::ComponentRef, cref2::ComponentRef)::Int
  local comp::Int

   comp = begin
    @match (cref1, cref2) begin
      (COMPONENT_REF_CREF(__), COMPONENT_REF_CREF(__)) => begin
        comp =
          stringCompare(name(cref1.node), name(cref2.node))
        if comp != 0
          return comp #? - John
        end
         comp =
          compareList(cref1.subscripts, cref2.subscripts)
        if comp != 0
          return comp #? - John
        end
        compare(cref1.restCref, cref2.restCref)
      end

      (COMPONENT_REF_EMPTY(__), COMPONENT_REF_EMPTY(__)) => begin
        0
      end

      (_, COMPONENT_REF_EMPTY(__)) => begin
        1
      end

      (COMPONENT_REF_EMPTY(__), _) => begin
        -1
      end
    end
  end
  return comp
end

function foldSubscripts(cref::ComponentRef, func::FuncT, arg::ArgT) where {ArgT}

   arg = begin
    @match cref begin
      COMPONENT_REF_CREF(origin = Origin.CREF) => begin
        for sub in cref.subscripts
           arg = func(sub, arg)
        end
        foldSubscripts(cref.restCref, func, arg)
      end

      _ => begin
        arg
      end
    end
  end
  return arg
end

"""  Copies subscripts from one cref to another, overwriting any subscripts on
     the destination cref.
"""
function transferSubscripts(srcCref::ComponentRef, dstCref::ComponentRef)::ComponentRef
  local cref::ComponentRef

  cref = begin
    @match (srcCref, dstCref) begin
      (COMPONENT_REF_EMPTY(__), _) => begin
        dstCref
      end

      (_, COMPONENT_REF_EMPTY(__)) => begin
        dstCref
      end

      (_, COMPONENT_REF_CREF(origin = Origin.ITERATOR)) => begin
        dstCref
      end

      (COMPONENT_REF_CREF(__), COMPONENT_REF_CREF(origin = Origin.CREF)) => begin
        local restCref = transferSubscripts(srcCref, dstCref.restCref)
        dstCref = COMPONENT_REF_CREF(dstCref.node, dstCref.subscripts, dstCref.ty, dstCref.origin, restCref)
      end

      (COMPONENT_REF_CREF(__), COMPONENT_REF_CREF(__)) where {(refEqual(srcCref.node, dstCref.node))} =>
        begin
          cref = transferSubscripts(srcCref.restCref, dstCref.restCref)
          COMPONENT_REF_CREF(dstCref.node, srcCref.subscripts, dstCref.ty, dstCref.origin, cref)
        end

      (COMPONENT_REF_CREF(__), COMPONENT_REF_CREF(__)) => begin
        transferSubscripts(srcCref.restCref, dstCref)
      end

      _ => begin
        Error.assertion(false, getInstanceName() + " failed", sourceInfo())
        fail()
      end
    end
  end
  return cref
end

""" #= Returns the subscripts of the N first parts of a cref in reverse order. =#"""
function subscriptsN(cref::ComponentRef, n::Int)::List{List{Subscript}}
  local subscripts::List{List{Subscript}} = nil
  local subs::List{Subscript}
  local rest::ComponentRef = cref
  for i = 1:n
    if isEmpty(rest)
      break
    end
    @match COMPONENT_REF_CREF(subscripts = subs, restCref = rest) = rest
    subscripts = _cons(subs, subscripts)
  end
  return subscripts
end

""" #= Returns all subscripts of a cref as a flat list in the correct order.
     Ex: a[1, 2].b[4].c[6, 3] => {1, 2, 4, 6, 3} =#"""
function subscriptsAllFlat(cref::ComponentRef)::List{Subscript}
  local subscripts::List{Subscript} = ListUtil.flattenReverse(subscriptsAll(cref))
  return subscripts
end

""" #= Returns all subscripts of a cref in reverse order.
     Ex: a[1, 2].b[4].c[6, 3] => {{6,3}, {4}, {1,2}} =#"""
function subscriptsAll(
  cref::ComponentRef,
  accumSubs::List#={<:List{<:Subscript}}=# = nil,
)::List{List{Subscript}}
  local subscripts::List{List{Subscript}}
  subscripts = begin
    @match cref begin
      COMPONENT_REF_CREF(__) => begin
        subscriptsAll(cref.restCref, _cons(cref.subscripts, accumSubs))
      end
      _ => begin
        accumSubs
      end
    end
  end
  return subscripts
end

""" Sets the subscripts of each part of a cref to the corresponding list of subscripts. """
function setSubscriptsList(
  subscripts,#::List{<:List{<:Subscript}},
  cref::ComponentRef,
)::ComponentRef
  cref = begin
    local subs::List{Subscript}
    local rest_subs::List{List{Subscript}}
    local rest_cref::ComponentRef
    @match (subscripts, cref) begin
      (subs <| rest_subs, COMPONENT_REF_CREF(__)) => begin
        rest_cref = setSubscriptsList(rest_subs, cref.restCref)
        COMPONENT_REF_CREF(cref.node, subs, cref.ty, cref.origin, rest_cref)
      end

      (nil(), _) => begin
        cref
      end
    end
  end
  return cref
end

function setSubscripts(subscripts::List{<:Subscript}, @nospecialize(cref::ComponentRef))
  local tmpCref = if cref isa COMPONENT_REF_CREF
    COMPONENT_REF_CREF(cref.node, subscripts, cref.ty, cref.origin, cref.restCref)
  else
    cref
  end
end

function getSubscripts(cref::ComponentRef)::List{Subscript}
  local subscripts::List{Subscript}
  subscripts = begin
    @match cref begin
      COMPONENT_REF_CREF(__) => begin
        cref.subscripts
      end
      _ => begin
        nil
      end
    end
  end
  return subscripts
end

function hasSubscripts(cref::ComponentRef)::Bool
  local hs::Bool
  hs = begin
    @match cref begin
      COMPONENT_REF_CREF(__) => begin
        !listEmpty(cref.subscripts) || hasSubscripts(cref.restCref)
      end

      _ => begin
        false
      end
    end
  end
  return hs
end

function applySubscripts2(subscripts::List{<:Subscript},
                          cref::ComponentRef,
                          )::Tuple{List{Subscript}, ComponentRef}
  (subscripts, cref) = begin
    local rest_cref::ComponentRef
    local cref_subs::List{Subscript}
    @match cref begin
      COMPONENT_REF_CREF(subscripts = cref_subs) => begin
         (subscripts, rest_cref) = applySubscripts2(subscripts, cref.restCref)
        if !listEmpty(subscripts)
          (cref_subs, subscripts) = mergeList(
            subscripts,
            cref_subs,
            dimensionCount(cref.ty),
          )
        end
        (subscripts, COMPONENT_REF_CREF(cref.node, cref_subs, cref.ty, cref.origin, rest_cref))
      end

      _ => begin
        (subscripts, cref)
      end
    end
  end
  return (subscripts, cref)
end

function applySubscripts(subscripts::List{<:Subscript}, cref::ComponentRef)::ComponentRef
  @match (nil, cref) = applySubscripts2(subscripts, cref)
  return cref
end

function addSubscript(subscript::Subscript, cref::ComponentRef)::ComponentRef
  local rCref = @match cref begin
    COMPONENT_REF_CREF(__) => begin
      local subs = listAppend(cref.subscripts, list(subscript))
      COMPONENT_REF_CREF(cref.node, subs, cref.ty, cref.origin, cref.rest_cref)
    end
  end
  return rCref
end

""" #= Returns the variability of the cref, with the variability of the subscripts
     taken into account. =#"""
function variability(cref::ComponentRef)::VariabilityType
  local var::VariabilityType =
    variabilityMax(nodeVariability(cref), subscriptsVariability(cref))
  return var
end

function subscriptsVariability(
  cref::ComponentRef,
  var::VariabilityType = Variability.CONSTANT,
)::VariabilityType

   () = begin
    @match cref begin
      COMPONENT_REF_CREF(origin = Origin.CREF) => begin
        for sub in cref.subscripts
          var = variabilityMax(var, variability(sub))
        end
        ()
      end

      _ => begin
        ()
      end
    end
  end
  return var
end

""" Returns the variability of the component node the cref refers to. """
function nodeVariability(cref::ComponentRef)::VariabilityType
  local var::VariabilityType
   var = begin
    @match cref begin
      COMPONENT_REF_CREF(node = COMPONENT_NODE(__)) => begin
        variability(component(cref.node))
      end
      _ => begin
        Variability.CONTINUOUS
      end
    end
  end
  return var
end

function getSubscriptedType2(restCref::ComponentRef, accumTy::NFType)::NFType
  local ty::NFType
   ty = begin
    @match restCref begin
      COMPONENT_REF_CREF(origin = Origin.CREF) => begin
         ty = liftArrayLeftList(
          accumTy,
          arrayDims(subscript(restCref.ty, restCref.subscripts)),
        )
        getSubscriptedType2(restCref.restCref, ty)
      end
      _ => begin
        accumTy
      end
    end
  end
  return ty
end

""" #= Returns the type of a cref, with the subscripts taken into account. =#"""
function getSubscriptedType(cref::ComponentRef)::NFType
  local ty::NFType
   ty = begin
    @match cref begin
      COMPONENT_REF_CREF(__) => begin
        getSubscriptedType2(cref.restCref, subscript(cref.ty, cref.subscripts))
      end
      _ => begin
        TYPE_UNKNOWN()
      end
    end
  end
  return ty
end

""" #= Returns the type of the component the given cref refers to, without taking
     subscripts into account. =#"""
function getComponentType(cref::ComponentRef)::M_Type
  local ty::M_Type

   ty = begin
    @match cref begin
      COMPONENT_REF_CREF(__) => begin
        cref.ty
      end

      _ => begin
        TYPE_UNKNOWN()
      end
    end
  end
  return ty
end

function append(cref::ComponentRef, restCref::ComponentRef)
   cref = begin
    @match cref begin
      COMPONENT_REF_CREF(__) => begin
        local restCrefTmp = append(cref.restCref, restCref)
        COMPONENT_REF_CREF(cref.node, cref.subscripts, cref.ty, cref.origin, restCrefTmp)
      end

      COMPONENT_REF_EMPTY(__) => begin
        restCref
      end
    end
  end
  return cref
end

function firstNonScope(cref::ComponentRef)::ComponentRef
  local first::ComponentRef

  local rest_cr::ComponentRef = rest(cref)

   first = begin
    @match rest_cr begin
      COMPONENT_REF_CREF(origin = Origin.SCOPE) => begin
        cref
      end

      COMPONENT_REF_EMPTY(__) => begin
        cref
      end

      _ => begin
        firstNonScope(rest_cr)
      end
    end
  end
  return first
end

function rest(cref::COMPONENT_REF_CREF)
  local restCref = cref.restCref
  return restCref
end

function firstName(cref::ComponentRef)::String
  local nameVar::String
   nameVar = begin
    @match cref begin
      COMPONENT_REF_CREF(__) => begin
        name(cref.node)
      end
      _ => begin
        ""
      end
    end
  end
  return nameVar
end

function updateNodeType(cref::ComponentRef)
  local crefRet = if cref isa COMPONENT_REF_CREF && isComponent(cref.node)
    crefTy = getType(cref.node)
    COMPONENT_REF_CREF(cref.node, cref.subscripts, crefTy, cref.origin, restCrefTmp)
  else
    cref
  end
  return crefRet
end

function nodeType(cref::ComponentRef)
  @match COMPONENT_REF_CREF(ty = ty) = cref
  return ty
end

function containsNode(cref::ComponentRef, node::InstNode)::Bool
  local res::Bool

   res = begin
    @match cref begin
      COMPONENT_REF_CREF(__) => begin
        refEqual(cref.node, node) || containsNode(cref.restCref, node)
      end

      _ => begin
        false
      end
    end
  end
  return res
end

function node(cref::ComponentRef)
  local nodeVar::InstNode
  @match COMPONENT_REF_CREF(node = nodeVar) = cref
  return nodeVar
end

function isIterator(cref::ComponentRef)::Bool
  local isIterator::Bool

   isIterator = begin
    @match cref begin
      COMPONENT_REF_CREF(origin = Origin.ITERATOR) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return isIterator
end

function isSimple(cref::ComponentRef)::Bool
  local isSimple::Bool

   isSimple = begin
    @match cref begin
      COMPONENT_REF_CREF(restCref = COMPONENT_REF_EMPTY(__)) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return isSimple
end

function isEmpty(cref::ComponentRef)::Bool
  local isEmpty::Bool

   isEmpty = begin
    @match cref begin
      COMPONENT_REF_EMPTY(__) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return isEmpty
end

function makeIterator(node::InstNode, ty::NFType)::ComponentRef
  local cref::ComponentRef = COMPONENT_REF_CREF(node, nil, ty, Origin.ITERATOR, COMPONENT_REF_EMPTY())
  return cref
end

function fromBuiltin(node::InstNode, ty::M_Type)::ComponentRef
  local cref::ComponentRef = COMPONENT_REF_CREF(node, nil, ty, Origin.SCOPE, COMPONENT_REF_EMPTY())
  return cref
end

function fromAbsynCref(
  acref::Absyn.ComponentRef,
  restCref::ComponentRef = COMPONENT_REF_EMPTY(),
)::ComponentRef
  local cref::ComponentRef
   cref = begin
    @match acref begin
      Absyn.CREF_IDENT(__) => begin
        fromAbsyn(NAME_NODE(acref.name), acref.subscripts, restCref)
      end
      Absyn.CREF_QUAL(__) => begin
        fromAbsynCref(
          acref.componentRef,
          fromAbsyn(NAME_NODE(acref.name), acref.subscripts, restCref),
        )
      end
      Absyn.CREF_FULLYQUALIFIED(__) => begin
        fromAbsynCref(acref.componentRef)
      end
      Absyn.WILD(__) => begin
        COMPONENT_REF_WILD()
      end
      Absyn.ALLWILD(__) => begin
        COMPONENT_REF_WILD()
      end
    end
  end
  return cref
end

function fromAbsyn(
  node::InstNode,
  subs::List{<:Absyn.Subscript},
  restCref::ComponentRef = COMPONENT_REF_EMPTY(),
)::ComponentRef
  local cref::ComponentRef
  local sl::List{Subscript}
  sl = list(SUBSCRIPT_RAW_SUBSCRIPT(s) for s in subs)
  cref = COMPONENT_REF_CREF(node, sl, TYPE_UNKNOWN(), Origin.CREF, restCref)
  return cref
end

function prefixScope(
  node::InstNode,
  ty::NFType,
  subs::List{<:Subscript},
  restCref::ComponentRef,
)::ComponentRef
  local cref::ComponentRef = COMPONENT_REF_CREF(node, subs, ty, Origin.SCOPE, restCref)
  return cref
end

function prefixCref(
  node::InstNode,
  ty::M_Type,
  subs::List{<:Subscript},
  restCref::ComponentRef,
  )::ComponentRef
  local cref::ComponentRef = COMPONENT_REF_CREF(node, subs, ty, Origin.CREF, restCref)
  return cref
end

function fromNode(
  node::InstNode,
  ty::NFType,
  subs::List{<:Subscript} = nil,
  origin::OriginType = Origin.CREF,
)::ComponentRef
  local cref::ComponentRef = COMPONENT_REF_CREF(node, subs, ty, origin, COMPONENT_REF_EMPTY())
  return cref
end

function nodesIncludingSplitSubs(cref::ComponentRef, accum::List = nil)
  local tmpNode
  nodes = @match cref begin
    COMPONENT_REF_CREF(__) => begin
      for s in cref.subscripts
        if isSplitIndex(s)
          @match SUBSCRIPT_SPLIT_INDEX(tmpNode) = s
          nodes = tmpNode <| nodes
        end
      end
      nodesIncludingSplitSubs(cref.restCref, cref.node <| nodes);
    end
    _ => begin
      nodes
    end
  end
end

function hasSplitSubscripts(cref::ComponentRef)
  res = @match cref begin
    COMPONENT_REF_CREF(__) => begin
      ListUtil.exist(cref.subscripts, isSplitIndex)
    end
    _ => begin
      false
    end
  end
end

function mapSubscripts(@nospecialize(cref::ComponentRef), func::Function)
  res = @match cref begin
    COMPONENT_REF_CREF(__) => begin
      if !listEmpty(cref.subscripts)
        @assign cref.subscripts = list(func(s) for s in cref.subscripts)
      end
      @assign cref.restCref = mapSubscripts(cref.restCref, func)
      cref
    end
    _ => cref
  end
  return res
end

#= New Code backported =#
function hasSplitSubscripts(cref::ComponentRef)
  local res::Bool
  res = begin
    @match cref begin
      COMPONENT_REF_CREF(origin = Origin.CREF)  => begin
        ListUtil.exist(cref.subscripts, Subscript.isSplitIndex) || hasSplitSubscripts(cref.restCref)
      end
      _  => begin
        false
      end
    end
  end
  res
end

function hasNonModelSubscripts(cref::ComponentRef) ::Bool
  local hasSubscripts::Bool
  hasSubscripts = begin
    @match cref begin
      COMPONENT_REF_CREF(__) where ((isModel(cref.node)))  => begin
        hasNonModelSubscripts(cref.restCref)
      end
      COMPONENT_REF_CREF(__)  => begin
        ! listEmpty(cref.subscripts) || hasNonModelSubscripts(cref.restCref)
      end
      _  => begin
        false
      end
    end
  end
  hasSubscripts
end

function expandSplitSubscripts(cref::ComponentRef)
  () = begin
    @match cref begin
      CREF(origin = Origin.CREF)  => begin
        cref.subscripts = expandSplitIndices(cref.subscripts, nil)
        cref.restCref = expandSplitSubscripts(cref.restCref)
        ()
      end

      _  => begin
        ()
      end
    end
  end
  cref
end
