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

function toInteger(subscript::Subscript)::Integer
  local int::Integer

  @assign int = begin
    @match subscript begin
      INDEX(__) => begin
        toInteger(subscript.index)
      end
    end
  end
  return int
end

function toExp(subscript::Subscript)::Expression
  local exp::Expression

  @assign exp = begin
    @match subscript begin
      UNTYPED(__) => begin
        subscript.exp
      end

      INDEX(__) => begin
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

  @assign subscript = begin
    @match exp begin
      INTEGER_EXPRESSION(__) => begin
        INDEX(exp)
      end

      P_Expression.BOOLEAN_EXPRESSION(__) => begin
        INDEX(exp)
      end

      ENUM_LITERAL(__) => begin
        INDEX(exp)
      end

      _ => begin
        UNTYPED(exp)
      end
    end
  end
  return subscript
end

function isValidIndexType(ty::M_Type)::Bool
  local b::Bool = isInteger(ty) || Type.isBoolean(ty) || Type.isEnumeration(ty)
  return b
end

function first(dim::Dimension)::Subscript
  local sub::Subscript

  @assign sub = begin
    @match dim begin
      P_Dimension.Dimension.INTEGER_EXPRESSION(__) => begin
        INDEX(INTEGER_EXPRESSION(1))
      end

      P_Dimension.Dimension.BOOLEAN(__) => begin
        INDEX(P_Expression.BOOLEAN_EXPRESSION(false))
      end

      P_Dimension.Dimension.ENUM(__) => begin
        INDEX(nthEnumLiteral(dim.enumType, 1))
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
  dimensions::Integer,
)::Tuple{List{Subscript}, List{Subscript}} #= The number of dimensions to subscript =#
  local remainingSubs::List{Subscript} #= The subscripts that didn't fit =#
  local outSubs::List{Subscript} #= The merged subscripts, at most 'dimensions' many =#

  local subs_count::Integer
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
      @assign outSubs = newSubs
      @assign remainingSubs = nil
    else
      @assign (outSubs, remainingSubs) = ListUtil.split(newSubs, dimensions)
    end
    return (outSubs, remainingSubs) #= The subscripts that didn't fit =#
  end
  @assign subs_count = listLength(oldSubs)
  @assign remainingSubs = newSubs
  @assign rest_old_subs = oldSubs
  @assign outSubs = nil
  #=  Loop over the remaining subscripts as long as they can be merged.
  =#
  while merged && !listEmpty(remainingSubs)
    @match _cons(new_sub, remainingSubs) = remainingSubs
    @assign merged = false
    while !merged
      if listEmpty(rest_old_subs)
        @assign remainingSubs = _cons(new_sub, remainingSubs)
        break
      else
        @match _cons(old_sub, rest_old_subs) = rest_old_subs
        @assign (merged, outSubs) = begin
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
                @assign outSubs = _cons(
                  SUBSCRIPT_INDEX(applySubscript(
                    new_sub,
                    old_sub.slice,
                  )),
                  outSubs,
                )
              else
                @assign outSubs = _cons(old_sub, outSubs)
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
    @assign outSubs = _cons(s, outSubs)
  end
  #=  Append any remaining new subscripts to the end of the list as long as
  =#
  #=  there are dimensions left to fill.
  =#
  while !listEmpty(remainingSubs) && subs_count < dimensions
    @match _cons(new_sub, remainingSubs) = remainingSubs
    @assign outSubs = _cons(new_sub, outSubs)
    @assign subs_count = subs_count + 1
  end
  @assign outSubs = listReverseInPlace(outSubs)
  return (outSubs, remainingSubs) #= The subscripts that didn't fit =#
end

function variabilityList(subscripts::List{<:Subscript})::VariabilityType
  local var::VariabilityType = Variability.CONSTANT

  for s in subscripts
    @assign var = variabilityMax(var, variability(s))
  end
  return var
end

function variability(subscript::Subscript)::VariabilityType
  local var::VariabilityType

  @assign var = begin
    @match subscript begin
      UNTYPED(__) => begin
        variability(subscript.exp)
      end

      INDEX(__) => begin
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
    @assign sub = expand(s, dim)
    @assign outSubscripts = _cons(sub, outSubscripts)
  end
  for d in rest_dims
    @assign sub = EXPANDED_SLICE(P_RangeIterator.RangeIterator.map(
      P_RangeIterator.RangeIterator.fromDim(d),
      makeIndex,
    ))
    @assign outSubscripts = _cons(sub, outSubscripts)
  end
  @assign outSubscripts = listReverse(outSubscripts)
  return outSubscripts
end

function expandSlice(subscript::Subscript)::Tuple{Subscript, Bool}
  local expanded::Bool
  local outSubscript::Subscript

  @assign (outSubscript, expanded) = begin
    local exp::Expression
    @match subscript begin
      SLICE(__) => begin
        @assign exp = P_ExpandExp.ExpandExp.expand(subscript.slice)
        if isArray(exp)
          @assign outSubscript = EXPANDED_SLICE(List(
            INDEX(e) for e in arrayElements(exp)
          ))
          @assign expanded = true
        else
          @assign outSubscript = subscript
          @assign expanded = false
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

  @assign (outSubscript, expanded) = begin
    local exp::Expression
    local iter::RangeIterator
    @match subscript begin
      SLICE(__) => begin
        expandSlice(subscript)
      end

      WHOLE(__) => begin
        @assign iter = P_RangeIterator.RangeIterator.fromDim(dimension)
        if P_RangeIterator.RangeIterator.isValid(iter)
          @assign outSubscript =
            EXPANDED_SLICE(P_RangeIterator.RangeIterator.map(iter, makeIndex))
          @assign expanded = true
        else
          @assign outSubscript = subscript
          @assign expanded = false
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
    @assign subs = scalarize(s, dim)
    if listEmpty(subs)
      @assign outSubscripts = nil
      return outSubscripts
    else
      @assign outSubscripts = _cons(subs, outSubscripts)
    end
  end
  for d in rest_dims
    @assign subs =
      P_RangeIterator.RangeIterator.map(P_RangeIterator.RangeIterator.fromDim(d), makeIndex)
    if listEmpty(subs)
      @assign outSubscripts = nil
      return outSubscripts
    else
      @assign outSubscripts = _cons(subs, outSubscripts)
    end
  end
  @assign outSubscripts = listReverse(outSubscripts)
  return outSubscripts
end

function scalarize(subscript::Subscript, dimension::Dimension)::List{Subscript}
  local subscripts::List{Subscript}

  @assign subscripts = begin
    @match subscript begin
      INDEX(__) => begin
        list(subscript)
      end

      SLICE(__) => begin
        List(
          INDEX(e)
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

  @assign dimension = begin
    @match subscript begin
      INDEX(__) => begin
        P_Dimension.Dimension.fromInteger(1)
      end

      SLICE(__) => begin
        listHead(arrayDims(typeOf(subscript.slice)))
      end

      WHOLE(__) => begin
        P_Dimension.Dimension.UNKNOWN()
      end
    end
  end
  return dimension
end

function simplify(subscript::Subscript)::Subscript
  local outSubscript::Subscript

  @assign outSubscript = begin
    @match subscript begin
      INDEX(__) => begin
        INDEX(SimplifyExp.simplify(subscript.index))
      end

      SLICE(__) => begin
        SLICE(SimplifyExp.simplify(subscript.slice))
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

  @assign outSubscript = begin
    @match subscript begin
      INDEX(__) => begin
        INDEX(Ceval.evalExp(subscript.index, target))
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

  @assign string = ListUtil.toString(subscripts, toFlatString, "", "[", ",", "]", false)
  return string
end

function toFlatString(subscript::Subscript)::String
  local string::String

  @assign string = begin
    @match subscript begin
      RAW_SUBSCRIPT(__) => begin
        Dump.printSubscriptStr(subscript.subscript)
      end

      UNTYPED(__) => begin
        toFlatString(subscript.exp)
      end

      INDEX(__) => begin
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

  @assign string = ListUtil.toString(subscripts, toString, "", "[", ", ", "]", false)
  return string
end

function toString(subscript::Subscript)::String
  local string::String

  @assign string = begin
    @match subscript begin
      RAW_SUBSCRIPT(__) => begin
        Dump.printSubscriptStr(subscript.subscript)
      end

      UNTYPED(__) => begin
        toString(subscript.exp)
      end

      INDEX(__) => begin
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

  @assign daeExp = begin
    @match subscript begin
      INDEX(__) => begin
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

function toDAE(subscript::Subscript)::DAE.P_Subscript.Subscript
  local daeSubscript::DAE.P_Subscript.Subscript

  @assign daeSubscript = begin
    @match subscript begin
      INDEX(__) => begin
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

  @assign outSubscript = begin
    local exp::Expression
    @match subscript begin
      UNTYPED(__) => begin
        @assign (exp, arg) = func(subscript.exp, arg)
        if referenceEq(subscript.exp, exp)
          subscript
        else
          UNTYPED(exp)
        end
      end

      INDEX(__) => begin
        @assign (exp, arg) = func(subscript.index, arg)
        if referenceEq(subscript.index, exp)
          subscript
        else
          INDEX(exp)
        end
      end

      SLICE(__) => begin
        @assign (exp, arg) = func(subscript.slice, arg)
        if referenceEq(subscript.slice, exp)
          subscript
        else
          SLICE(exp)
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

  @assign outSubscript = begin
    local exp::Expression
    @match subscript begin
      UNTYPED(__) => begin
        @assign (exp, arg) = mapFold(subscript.exp, func, arg)
        if referenceEq(subscript.exp, exp)
          subscript
        else
          UNTYPED(exp)
        end
      end

      INDEX(__) => begin
        @assign (exp, arg) = mapFold(subscript.index, func, arg)
        if referenceEq(subscript.index, exp)
          subscript
        else
          INDEX(exp)
        end
      end

      SLICE(__) => begin
        @assign (exp, arg) = mapFold(subscript.slice, func, arg)
        if referenceEq(subscript.slice, exp)
          subscript
        else
          SLICE(exp)
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

  @assign result = begin
    @match subscript begin
      UNTYPED(__) => begin
        fold(subscript.exp, func, arg)
      end

      INDEX(__) => begin
        fold(subscript.index, func, arg)
      end

      SLICE(__) => begin
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

  @assign outSubscript = begin
    local e1::Expression
    local e2::Expression
    @match subscript begin
      UNTYPED(exp = e1) => begin
        @assign e2 = func(e1)
        if referenceEq(e1, e2)
          subscript
        else
          UNTYPED(e2)
        end
      end

      INDEX(index = e1) => begin
        @assign e2 = func(e1)
        if referenceEq(e1, e2)
          subscript
        else
          INDEX(e2)
        end
      end

      SLICE(slice = e1) => begin
        @assign e2 = func(e1)
        if referenceEq(e1, e2)
          subscript
        else
          SLICE(e2)
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

  @assign outSubscript = begin
    local e1::Expression
    local e2::Expression
    @match subscript begin
      UNTYPED(exp = e1) => begin
        @assign e2 = map(e1, func)
        if referenceEq(e1, e2)
          subscript
        else
          UNTYPED(e2)
        end
      end

      INDEX(index = e1) => begin
        @assign e2 = map(e1, func)
        if referenceEq(e1, e2)
          subscript
        else
          INDEX(e2)
        end
      end

      SLICE(slice = e1) => begin
        @assign e2 = map(e1, func)
        if referenceEq(e1, e2)
          subscript
        else
          SLICE(e2)
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
  return @assign () = begin
    @match subscript begin
      UNTYPED(__) => begin
        apply(subscript.exp, func)
        ()
      end

      INDEX(__) => begin
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
      @assign res = true
      return res
    end
  end
  @assign res = false
  return res
end

function containsExpShallow(
  subscript::Subscript,
  func::ContainsPred,
)::Bool
  local res::Bool

  @assign res = begin
    @match subscript begin
      UNTYPED(__) => begin
        func(subscript.exp)
      end

      INDEX(__) => begin
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
      @assign res = true
      return res
    end
  end
  @assign res = false
  return res
end

function containsExp(subscript::Subscript, func::ContainsPred)::Bool
  local res::Bool

  @assign res = begin
    @match subscript begin
      UNTYPED(__) => begin
        contains(subscript.exp, func)
      end

      INDEX(__) => begin
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
)::Integer
  local comp::Integer

  local s2::Subscript
  local rest_s2::List{Subscript} = subscripts2

  @assign comp = Util.intCompare(listLength(subscripts1), listLength(subscripts2))
  if comp != 0
    return comp
  end
  for s1 in subscripts1
    @match _cons(s2, rest_s2) = rest_s2
    @assign comp = compare(s1, s2)
    if comp != 0
      return comp
    end
  end
  @assign comp = 0
  return comp
end

function compare(subscript1::Subscript, subscript2::Subscript)::Integer
  local comp::Integer

  if referenceEq(subscript1, subscript2)
    @assign comp = 0
    return comp
  end
  @assign comp = Util.intCompare(valueConstructor(subscript1), valueConstructor(subscript2))
  if comp != 0
    return comp
  end
  @assign comp = begin
    local e::Expression
    @match subscript1 begin
      UNTYPED(__) => begin
        @match UNTYPED(exp = e) = subscript2
        compare(subscript1.exp, e)
      end

      INDEX(__) => begin
        @match INDEX(index = e) = subscript2
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
      @assign isEqual = false
      return isEqual
    end
    @match _cons(s2, rest) = rest
    if !isEqual(s1, s2)
      @assign isEqual = false
      return isEqual
    end
  end
  @assign isEqual = listEmpty(rest)
  return isEqual
end

function isEqual(subscript1::Subscript, subscript2::Subscript)::Bool
  local isEqual::Bool

  @assign isEqual = begin
    @match (subscript1, subscript2) begin
      (RAW_SUBSCRIPT(__), RAW_SUBSCRIPT(__)) => begin
        AbsynUtil.subscriptEqual(subscript1.subscript, subscript2.subscript)
      end

      (UNTYPED(__), UNTYPED(__)) => begin
        isEqual(subscript1.exp, subscript2.exp)
      end

      (INDEX(__), INDEX(__)) => begin
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
  @assign isScalarLiteral = begin
    @match sub begin
      INDEX(__) => begin
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
  @assign isScalar = begin
    local ty::M_Type
    @match sub begin
      INDEX(__) => begin
        @assign ty = typeOf(sub.index)
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
  @assign isWhole = begin
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
  @assign isIndex = begin
    @match sub begin
      INDEX(__) => begin
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
  @assign ty = typeOf(exp)
  if isValidIndexType(ty)
    @assign subscript = INDEX(exp)
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
