@UniontypeDecl NFSubscript
Subscript = NFSubscript

@Uniontype NFSubscript begin
  @Record SUBSCRIPT_WHOLE begin
  end
  @Record SUBSCRIPT_EXPANDED_SLICE begin
    indices::List{Subscript}
  end
  @Record SUBSCRIPT_SLICE begin
    slice::Expression
  end
  @Record SUBSCRIPT_INDEX begin
    index::Expression
  end
  @Record SUBSCRIPT_UNTYPED begin
    exp::Expression
  end
  @Record SUBSCRIPT_RAW_SUBSCRIPT begin
    subscript::Absyn.Subscript
  end
end

function toInteger(subscript::Subscript)::Int
  local int::Int

  int = begin
    @match subscript begin
      SUBSCRIPT_INDEX(__) => begin
        toInteger(subscript.index)
      end
    end
  end
  return int
end

function toExp(subscript::Subscript)::Expression
  local exp::Expression

  exp = begin
    @match subscript begin
      UNTYPED(__) => begin
        subscript.exp
      end

      SUBSCRIPT_INDEX(__) => begin
        subscript.index
      end

      SLICE(__) => begin
        subscript.slice
      end
    end
  end
  return exp
end

function fromExp(exp::Expression)::Subscript
  local subscript::Subscript

  subscript = begin
    @match exp begin
      INTEGER_EXPRESSION(__) => begin
        SUBSCRIPT_INDEX(exp)
      end

      BOOLEAN_EXPRESSION(__) => begin
        SUBSCRIPT_INDEX(exp)
      end

      ENUM_LITERAL_EXPRESSION(__) => begin
        SUBSCRIPT_INDEX(exp)
      end

      _ => begin
        SUBSCRIPT_UNTYPED(exp)
      end
    end
  end
  return subscript
end

function isValidIndexType(ty::M_Type)::Bool
  local b::Bool = isInteger(ty) || isBoolean(ty) || isEnumeration(ty)
  return b
end

function first(dim::Dimension)::Subscript
  local sub::Subscript

  sub = begin
    @match dim begin
      P_Dimension.Dimension.INTEGER_EXPRESSION(__) => begin
        SUBSCRIPT_INDEX(INTEGER_EXPRESSION(1))
      end

      P_Dimension.Dimension.BOOLEAN(__) => begin
        SUBSCRIPT_INDEX(BOOLEAN_EXPRESSION(false))
      end

      P_Dimension.Dimension.ENUM(__) => begin
        SUBSCRIPT_INDEX(nthEnumLiteral(dim.enumType, 1))
      end
    end
  end
  return sub
end

""" #= Merges a list of subscripts with a list of 'existing' subscripts.
     This is done by e.g. subscripting existing slice and : subscripts,
     such that e.g. mergeList({1, :}, {3:5, 1:3, 4}) => {3, 1:3, 4}.
     The function will ensure that the output list contains at most as
     many subscripts as the given number of dimensions, and also returns
     the list of remaining subscripts that couldn't be added. =#"""
function mergeList(
  newSubs::List{<:Subscript},
  oldSubs::List{<:Subscript},
  dimensions::Int,
)::Tuple{List{Subscript}, List{Subscript}} #= The number of dimensions to subscript =#
  local remainingSubs::List{Subscript} #= The subscripts that didn't fit =#
  local outSubs::List{Subscript} #= The merged subscripts, at most 'dimensions' many =#

  local subs_count::Int
  local new_sub::Subscript
  local old_sub::Subscript
  local rest_old_subs::List{Subscript}
  local merged::Bool = true
  
  #=  If there aren't any existing subscripts we just add as many subscripts
  =#
  #=  from the list of new subscripts as possible.
  =#
  if listEmpty(oldSubs)
    if listLength(newSubs) <= dimensions
      outSubs = newSubs
      remainingSubs = nil
    else
      (outSubs, remainingSubs) = ListUtil.split(newSubs, dimensions)
    end
    return (outSubs, remainingSubs) #= The subscripts that didn't fit =#
  end
  subs_count = listLength(oldSubs)
  remainingSubs = newSubs
  rest_old_subs = oldSubs
  outSubs = nil
  #=  Loop over the remaining subscripts as long as they can be merged.
  =#
  while merged && !listEmpty(remainingSubs)
    @match _cons(new_sub, remainingSubs) = remainingSubs
    merged = false
    while !merged
      if listEmpty(rest_old_subs)
        remainingSubs = _cons(new_sub, remainingSubs)
        break
      else
        @match _cons(old_sub, rest_old_subs) = rest_old_subs
        (merged, outSubs) = begin
          @match old_sub begin
            SLICE(__) => begin
              #=  Loop over the old subscripts while this new subscript hasn't been
              =#
              #=  merged and there's still old subscript left.
              =#
              #=  Try to replace the old subscript with the new.
              =#
              #=  If the old subscript is a slice, subscript it with the new subscript.
              =#
              #=  The old subscript only changes if the new is an index or slice, not :.
              =#
              if !isWhole(new_sub)
                outSubs = _cons(
                  SUBSCRIPT_INDEX(applySubscript(
                    new_sub,
                    old_sub.slice,
                  )),
                  outSubs,
                )
              else
                outSubs = _cons(old_sub, outSubs)
              end
              (true, outSubs)
            end

            WHOLE(__) => begin
              (true, _cons(new_sub, outSubs))
            end

            _ => begin
              (false, _cons(old_sub, outSubs))
            end
          end
        end
      end
    end
  end
  #=  If the old subscript is :, replace it with the new subscript.
  =#
  #=  If the old subscript is a scalar index it can't be replaced.
  =#
  #=  Append any remaining old subscripts.
  =#
  for s in rest_old_subs
    outSubs = _cons(s, outSubs)
  end
  #=  Append any remaining new subscripts to the end of the list as long as
  =#
  #=  there are dimensions left to fill.
  =#
  while !listEmpty(remainingSubs) && subs_count < dimensions
    @match _cons(new_sub, remainingSubs) = remainingSubs
    outSubs = _cons(new_sub, outSubs)
    subs_count = subs_count + 1
  end
  outSubs = listReverseInPlace(outSubs)
  return (outSubs, remainingSubs) #= The subscripts that didn't fit =#
end

function variabilityList(subscripts::List{<:Subscript})::VariabilityType
  local var::VariabilityType = Variability.CONSTANT

  for s in subscripts
    var = variabilityMax(var, variability(s))
  end
  return var
end

function variability(subscript::Subscript)::VariabilityType
  local var::VariabilityType

  var = begin
    @match subscript begin
      UNTYPED(__) => begin
        variability(subscript.exp)
      end

      SUBSCRIPT_INDEX(__) => begin
        variability(subscript.index)
      end

      SLICE(__) => begin
        variability(subscript.slice)
      end

      WHOLE(__) => begin
        Variability.CONSTANT
      end
    end
  end
  return var
end

function expandList(
  subscripts::List{<:Subscript},
  dimensions::List{<:Dimension},
)::List{Subscript}
  local outSubscripts::List{Subscript} = nil

  local dim::Dimension
  local rest_dims::List{Dimension} = dimensions
  local sub::Subscript

  for s in subscripts
    @match _cons(dim, rest_dims) = rest_dims
    sub = expand(s, dim)
    outSubscripts = _cons(sub, outSubscripts)
  end
  for d in rest_dims
    sub = EXPANDED_SLICE(P_RangeIterator.RangeIterator.map(
      P_RangeIterator.RangeIterator.fromDim(d),
      makeIndex,
    ))
    outSubscripts = _cons(sub, outSubscripts)
  end
  outSubscripts = listReverse(outSubscripts)
  return outSubscripts
end

function expandSlice(subscript::Subscript)::Tuple{Subscript, Bool}
  local expanded::Bool
  local outSubscript::Subscript

  (outSubscript, expanded) = begin
    local exp::Expression
    @match subscript begin
      SLICE(__) => begin
        exp = P_ExpandExp.ExpandExp.expand(subscript.slice)
        if isArray(exp)
          outSubscript = EXPANDED_SLICE(List(
            SUBSCRIPT_INDEX(e) for e in arrayElements(exp)
          ))
          expanded = true
        else
          outSubscript = subscript
          expanded = false
        end
        (outSubscript, expanded)
      end

      _ => begin
        (subscript, false)
      end
    end
  end
  return (outSubscript, expanded)
end

function expand(subscript::Subscript, dimension::Dimension)::Tuple{Subscript, Bool}
  local expanded::Bool
  local outSubscript::Subscript

  (outSubscript, expanded) = begin
    local exp::Expression
    local iter::RangeIterator
    @match subscript begin
      SLICE(__) => begin
        expandSlice(subscript)
      end

      WHOLE(__) => begin
        iter = P_RangeIterator.RangeIterator.fromDim(dimension)
        if P_RangeIterator.RangeIterator.isValid(iter)
          outSubscript =
            EXPANDED_SLICE(P_RangeIterator.RangeIterator.map(iter, makeIndex))
          expanded = true
        else
          outSubscript = subscript
          expanded = false
        end
        (outSubscript, expanded)
      end

      _ => begin
        (subscript, true)
      end
    end
  end
  return (outSubscript, expanded)
end

function scalarizeList(
  subscripts::List{<:Subscript},
  dimensions::List{<:Dimension},
)::List{List{Subscript}}
  local outSubscripts::List{List{Subscript}} = nil

  local dim::Dimension
  local rest_dims::List{Dimension} = dimensions
  local subs::List{Subscript}

  for s in subscripts
    @match _cons(dim, rest_dims) = rest_dims
    subs = scalarize(s, dim)
    if listEmpty(subs)
      outSubscripts = nil
      return outSubscripts
    else
      outSubscripts = _cons(subs, outSubscripts)
    end
  end
  for d in rest_dims
    subs =
      P_RangeIterator.RangeIterator.map(P_RangeIterator.RangeIterator.fromDim(d), makeIndex)
    if listEmpty(subs)
      outSubscripts = nil
      return outSubscripts
    else
      outSubscripts = _cons(subs, outSubscripts)
    end
  end
  outSubscripts = listReverse(outSubscripts)
  return outSubscripts
end

function scalarize(subscript::Subscript, dimension::Dimension)::List{Subscript}
  local subscripts::List{Subscript}

  subscripts = begin
    @match subscript begin
      SUBSCRIPT_INDEX(__) => begin
        list(subscript)
      end

      SLICE(__) => begin
        List(
          SUBSCRIPT_INDEX(e)
          for
          e in
          arrayElements(P_ExpandExp.ExpandExp.expand(subscript.slice))
        )
      end

      WHOLE(__) => begin
        P_RangeIterator.RangeIterator.map(
          P_RangeIterator.RangeIterator.fromDim(dimension),
          makeIndex,
        )
      end
    end
  end
  return subscripts
end

""" #= Returns a dimension representing the size of the given subscript. =#"""
function toDimension(subscript::Subscript)::Dimension
  local dimension::Dimension

  dimension = begin
    @match subscript begin
      SUBSCRIPT_INDEX(__) => begin
        P_Dimension.Dimension.fromInteger(1)
      end

      SLICE(__) => begin
        listHead(arrayDims(typeOf(subscript.slice)))
      end

      WHOLE(__) => begin
        DIMENSION_UNKNOWN()
      end
    end
  end
  return dimension
end

function simplifySubscript(subscript::Subscript)::Subscript
  local outSubscript::Subscript
  outSubscript = begin
    @match subscript begin
      SUBSCRIPT_INDEX(__) => begin
        SUBSCRIPT_INDEX(simplify(subscript.index))
      end
      SLICE(__) => begin
        SUBSCRIPT_SLICE(simplify(subscript.slice))
      end
      _ => begin
        subscript
      end
    end
  end
  return outSubscript
end

function eval(
  subscript::Subscript,
  target::EvalTarget = P_EvalTarget.IGNORE_ERRORS(),
)::Subscript
  local outSubscript::Subscript

  outSubscript = begin
    @match subscript begin
      SUBSCRIPT_INDEX(__) => begin
        SUBSCRIPT_INDEX(Ceval.evalExp(subscript.index, target))
      end

      SLICE(__) => begin
        SLICE(Ceval.evalExp(subscript.slice, target))
      end

      _ => begin
        subscript
      end
    end
  end
  return outSubscript
end

function toFlatStringList(subscripts::List{<:Subscript})::String
  local string::String

  string = ListUtil.toString(subscripts, toFlatString, "", "[", ",", "]", false)
  return string
end

function toFlatString(subscript::Subscript)::String
  local string::String

  string = begin
    @match subscript begin
      RAW_SUBSCRIPT(__) => begin
        Dump.printSubscriptStr(subscript.subscript)
      end

      UNTYPED(__) => begin
        toFlatString(subscript.exp)
      end

      SUBSCRIPT_INDEX(__) => begin
        toFlatString(subscript.index)
      end

      SLICE(__) => begin
        toFlatString(subscript.slice)
      end

      EXPANDED_SLICE(__) => begin
        ListUtil.toString(subscript.indices, toString, "", "{", ", ", "}", false)
      end

      WHOLE(__) => begin
        ":"
      end
    end
  end
  return string
end

function toStringList(::Union{Cons{Union{}}, Nil})::String
  ""
end

function toStringList(subscripts::List{<:Subscript})::String
  local string::String

  string = ListUtil.toString(subscripts, toString, "", "[", ", ", "]", false)
  return string
end

function toString(subscript::Subscript)::String
  local string::String

  string = begin
    @match subscript begin
      RAW_SUBSCRIPT(__) => begin
        Dump.printSubscriptStr(subscript.subscript)
      end

      UNTYPED(__) => begin
        toString(subscript.exp)
      end

      SUBSCRIPT_INDEX(__) => begin
        toString(subscript.index)
      end

      SLICE(__) => begin
        toString(subscript.slice)
      end

      EXPANDED_SLICE(__) => begin
        ListUtil.toString(subscript.indices, toString, "", "{", ", ", "}", false)
      end

      WHOLE(__) => begin
        ":"
      end
    end
  end
  return string
end

function toDAEExp(subscript::Subscript)::DAE.Exp
  local daeExp::DAE.Exp

  daeExp = begin
    @match subscript begin
      SUBSCRIPT_INDEX(__) => begin
        toDAE(subscript.index)
      end

      SLICE(__) => begin
        toDAE(subscript.slice)
      end

      _ => begin
        Error.assertion(
          false,
          getInstanceName() + " failed on unknown subscript '" + toString(subscript) + "'",
          sourceInfo(),
        )
        fail()
      end
    end
  end
  return daeExp
end

function toDAE(subscript::Subscript)::DAE.Subscript
  local daeSubscript::DAE.Subscript

  daeSubscript = begin
    @match subscript begin
      SUBSCRIPT_INDEX(__) => begin
        DAE.INDEX(toDAE(subscript.index))
      end

      SLICE(__) => begin
        DAE.SLICE(toDAE(subscript.slice))
      end

      WHOLE(__) => begin
        DAE.WHOLEDIM()
      end

      _ => begin
        Error.assertion(
          false,
          getInstanceName() + " failed on unknown subscript",
          sourceInfo(),
        )
        fail()
      end
    end
  end
  return daeSubscript
end

function mapFoldExpShallow(subscript::Subscript, func::MapFunc, arg::ArgT) where {ArgT}
  local outSubscript::Subscript
  outSubscript = begin
    local exp::Expression
    @match subscript begin
      SUBSCRIPT_UNTYPED(__) => begin
        (exp, arg) = func(subscript.exp, arg)
        if referenceEq(subscript.exp, exp)
          subscript
        else
          SUBSCRIPT_UNTYPED(exp)
        end
      end
      SUBSCRIPT_INDEX(__) => begin
        (exp, arg) = func(subscript.index)
        if referenceEq(subscript.index, exp)
          subscript
        else
          SUBSCRIPT_INDEX(exp)
        end
      end
      SUBSCRIPT_SLICE(__) => begin
        (exp, arg) = func(subscript.slice, arg)
        if referenceEq(subscript.slice, exp)
          subscript
        else
          SUBSCRIPT_SLICE(exp)
        end
      end
      _ => begin
        subscript
      end
    end
  end
  return (outSubscript, arg)
end

function mapFoldExp(subscript::Subscript, func::MapFunc, arg::ArgT) where {ArgT}

  local outSubscript::Subscript

  outSubscript = begin
    local exp::Expression
    @match subscript begin
      SUBSCRIPT_UNTYPED(__) => begin
        (exp, arg) = mapFold(subscript.exp, func, arg)
        if referenceEq(subscript.exp, exp)
          subscript
        else
          SUBSCRIPT_UNTYPED(exp)
        end
      end

      SUBSCRIPT_INDEX(__) => begin
        (exp, arg) = mapFold(subscript.index, func, arg)
        if referenceEq(subscript.index, exp)
          subscript
        else
          SUBSCRIPT_INDEX(exp)
        end
      end

      SUBSCRIPT_SLICE(__) => begin
        (exp, arg) = mapFold(subscript.slice, func, arg)
        if referenceEq(subscript.slice, exp)
          subscript
        else
          SUBSCRIPT_SLICE(exp)
        end
      end

      _ => begin
        subscript
      end
    end
  end
  return (outSubscript, arg)
end

function foldExp(subscript::Subscript, func::FoldFunc, arg::ArgT) where {ArgT}
  local result::ArgT

  result = begin
    @match subscript begin
      SUBSCRIPT_UNTYPED(__) => begin
        fold(subscript.exp, func, arg)
      end

      SUBSCRIPT_INDEX(__) => begin
        fold(subscript.index, func, arg)
      end

      SUBSCRIPT_SLICE(__) => begin
        fold(subscript.slice, func, arg)
      end

      _ => begin
        arg
      end
    end
  end
  return result
end

function mapShallowExp(subscript::Subscript, func::MapFunc)::Subscript
  local outSubscript::Subscript

  outSubscript = begin
    local e1::Expression
    local e2::Expression
    @match subscript begin
      SUBSCRIPT_UNTYPED(exp = e1) => begin
        e2 = func(e1)
        if referenceEq(e1, e2)
          subscript
        else
          SUBSCRIPT_UNTYPED(e2)
        end
      end

      SUBSCRIPT_INDEX(index = e1) => begin
        e2 = func(e1)
        if referenceEq(e1, e2)
          subscript
        else
          SUBSCRIPT_INDEX(e2)
        end
      end

      SUBSCRIPT_SLICE(slice = e1) => begin
        e2 = func(e1)
        if referenceEq(e1, e2)
          subscript
        else
          SUBSCRIPT_SLICE(e2)
        end
      end

      _ => begin
        subscript
      end
    end
  end
  return outSubscript
end

function mapExp(subscript::Subscript, func::MapFunc)::Subscript
  local outSubscript::Subscript

  outSubscript = begin
    local e1::Expression
    local e2::Expression
    @match subscript begin
      SUBSCRIPT_UNTYPED(exp = e1) => begin
        e2 = map(e1, func)
        if referenceEq(e1, e2)
          subscript
        else
          SUBSCRIPT_UNTYPED(e2)
        end
      end

      SUBSCRIPT_INDEX(index = e1) => begin
        e2 = map(e1, func)
        if referenceEq(e1, e2)
          subscript
        else
          SUBSCRIPT_INDEX(e2)
        end
      end

      SUBSCRIPT_SLICE(slice = e1) => begin
        e2 = map(e1, func)
        if referenceEq(e1, e2)
          subscript
        else
          SUBSCRIPT_SLICE(e2)
        end
      end

      _ => begin
        subscript
      end
    end
  end
  return outSubscript
end

function applyExp(subscript::Subscript, func::ApplyFunc)
  return () = begin
    @match subscript begin
      UNTYPED(__) => begin
        apply(subscript.exp, func)
        ()
      end

      SUBSCRIPT_INDEX(__) => begin
        apply(subscript.index, func)
        ()
      end

      SLICE(__) => begin
        apply(subscript.slice, func)
        ()
      end

      _ => begin
        ()
      end
    end
  end
end

function listContainsExpShallow(
  subscripts::List{<:Subscript},
  func::ContainsPred,
)::Bool
  local res::Bool

  for s in subscripts
    if containsExpShallow(s, func)
      res = true
      return res
    end
  end
  res = false
  return res
end

function containsExpShallow(
  subscript::Subscript,
  func::ContainsPred,
)::Bool
  local res::Bool

  res = begin
    @match subscript begin
      UNTYPED(__) => begin
        func(subscript.exp)
      end

      SUBSCRIPT_INDEX(__) => begin
        func(subscript.index)
      end

      SLICE(__) => begin
        func(subscript.slice)
      end

      _ => begin
        false
      end
    end
  end
  return res
end

function listContainsExp(
  subscripts::List{<:Subscript},
  func::ContainsPred,
)::Bool
  local res::Bool

  for s in subscripts
    if containsExp(s, func)
      res = true
      return res
    end
  end
  res = false
  return res
end

function containsExp(subscript::Subscript, func::ContainsPred)::Bool
  local res::Bool

  res = begin
    @match subscript begin
      UNTYPED(__) => begin
        contains(subscript.exp, func)
      end

      SUBSCRIPT_INDEX(__) => begin
        contains(subscript.index, func)
      end

      SLICE(__) => begin
        contains(subscript.slice, func)
      end

      _ => begin
        false
      end
    end
  end
  return res
end

function compareList(
  subscripts1::List{<:Subscript},
  subscripts2::List{<:Subscript},
)::Int
  local comp::Int

  local s2::Subscript
  local rest_s2::List{Subscript} = subscripts2

  comp = Util.intCompare(listLength(subscripts1), listLength(subscripts2))
  if comp != 0
    return comp
  end
  for s1 in subscripts1
    @match _cons(s2, rest_s2) = rest_s2
    comp = compare(s1, s2)
    if comp != 0
      return comp
    end
  end
  comp = 0
  return comp
end

function compare(subscript1::Subscript, subscript2::Subscript)::Int
  local comp::Int

  if referenceEq(subscript1, subscript2)
    comp = 0
    return comp
  end
  comp = Util.intCompare(valueConstructor(subscript1), valueConstructor(subscript2))
  if comp != 0
    return comp
  end
  comp = begin
    local e::Expression
    @match subscript1 begin
      UNTYPED(__) => begin
        @match UNTYPED(exp = e) = subscript2
        compare(subscript1.exp, e)
      end

      SUBSCRIPT_INDEX(__) => begin
        @match SUBSCRIPT_INDEX(index = e) = subscript2
        compare(subscript1.index, e)
      end

      SLICE(__) => begin
        @match SLICE(slice = e) = subscript2
        compare(subscript1.slice, e)
      end

      WHOLE(__) => begin
        0
      end
    end
  end
  return comp
end

function isEqualList(subscripts1::List{<:Subscript}, subscripts2::List{<:Subscript})::Bool
  local isEqual::Bool

  local s2::Subscript
  local rest::List{Subscript} = subscripts2

  for s1 in subscripts1
    if listEmpty(rest)
      isEqual = false
      return isEqual
    end
    @match _cons(s2, rest) = rest
    if !isEqual(s1, s2)
      isEqual = false
      return isEqual
    end
  end
  isEqual = listEmpty(rest)
  return isEqual
end

function isEqual(subscript1::Subscript, subscript2::Subscript)::Bool
  local isEqual::Bool

  isEqual = begin
    @match (subscript1, subscript2) begin
      (RAW_SUBSCRIPT(__), RAW_SUBSCRIPT(__)) => begin
        AbsynUtil.subscriptEqual(subscript1.subscript, subscript2.subscript)
      end

      (UNTYPED(__), UNTYPED(__)) => begin
        isEqual(subscript1.exp, subscript2.exp)
      end

      (SUBSCRIPT_INDEX(__), SUBSCRIPT_INDEX(__)) => begin
        isEqual(subscript1.index, subscript2.index)
      end

      (SLICE(__), SLICE(__)) => begin
        isEqual(subscript1.slice, subscript2.slice)
      end

      (WHOLE(__), WHOLE(__)) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return isEqual
end

function isScalarLiteral(sub::Subscript)::Bool
  local isScalarLiteral::Bool
  isScalarLiteral = begin
    @match sub begin
      SUBSCRIPT_INDEX(__) => begin
        isScalarLiteral(sub.index)
      end
      _ => begin
        false
      end
    end
  end
  return isScalarLiteral
end

function isScalar(sub::Subscript)::Bool
  local isScalar::Bool
  isScalar = begin
    local ty::M_Type
    @match sub begin
      SUBSCRIPT_INDEX(__) => begin
        ty = typeOf(sub.index)
        isValidIndexType(ty)
      end

      _ => begin
        false
      end
    end
  end
  return isScalar
end

function isWhole(sub::Subscript)::Bool
  local isWhole::Bool
  isWhole = begin
    @match sub begin
      WHOLE(__) => begin
        true
      end
      _ => begin
        false
      end
    end
  end
  return isWhole
end

function isIndex(sub::Subscript)::Bool
  local isIndex::Bool
  isIndex = begin
    @match sub begin
      SUBSCRIPT_INDEX(__) => begin
        true
      end
      _ => begin
        false
      end
    end
  end
  return isIndex
end

function makeIndex(exp::Expression)::Subscript
  local subscript::Subscript
  local ty::M_Type
  ty = typeOf(exp)
  if isValidIndexType(ty)
    subscript = SUBSCRIPT_INDEX(exp)
  else
    Error.assertion(
      false,
      getInstanceName() + " got a non integer type exp to make an index sub",
      sourceInfo(),
    )
    fail()
  end
  return subscript
end
