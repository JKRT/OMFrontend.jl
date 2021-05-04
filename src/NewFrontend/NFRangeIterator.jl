@Uniontype NFRangeIterator begin
  @Record RANGEITERATOR_INVALID_RANGE begin
    exp::Expression
  end
  @Record RANGEITERATOR_ARRAY_RANGE begin
    values::List{Expression}
  end
  @Record RANGEITERATOR_REAL_RANGE begin
    start::AbstractFloat
    stepsize::AbstractFloat
    current::Integer
    steps::Integer
  end
  @Record RANGEITERATOR_INT_STEP_RANGE begin
    current::Integer
    stepsize::Integer
    last::Integer
  end
  @Record RANGEITERATOR_INT_RANGE begin
    current::Integer
    last::Integer
  end
end

const RangeIterator = NFRangeIterator

function fold(iterator::RangeIterator, func::FuncT, arg::ArgT) where {ArgT}
  local iter::RangeIterator = iterator
  local exp::Expression
  while hasNext(iter)
    (iter, exp) = next(iter)
    arg = func(exp, arg)
  end
  return arg
end

function map(iterator::RangeIterator, func::FuncT) where {T}
  local lst::List{T} = nil

  local iter::RangeIterator = iterator
  local exp::Expression

  while hasNext(iter)
    @assign (iter, exp) = next(iter)
    @assign lst = _cons(func(exp), lst)
  end
  @assign lst = listReverse(lst)
  return lst
end

function toListReverse(iterator::RangeIterator)::List{Expression}
  local expl::List{Expression} = nil

  local iter::RangeIterator = iterator
  local exp::Expression

  while hasNext(iter)
    @assign (iter, exp) = next(iter)
    @assign expl = _cons(exp, expl)
  end
  return expl
end

function toList(iterator::RangeIterator)::List{Expression}
  local expl::List{Expression} = listReverse(toListReverse(iterator))
  return expl
end

function hasNext(iterator::RangeIterator)::Bool
  local hasNext::Bool

  @assign hasNext = begin
    @match iterator begin
      RANGEITERATOR_INT_RANGE(__) => begin
        iterator.current <= iterator.last
      end

      INT_STEP_RANGE(__) => begin
        if iterator.stepsize > 0
          iterator.current <= iterator.last
        else
          iterator.current >= iterator.last
        end
      end

      REAL_RANGE(__) => begin
        iterator.current < iterator.steps
      end

      ARRAY_RANGE(__) => begin
        !listEmpty(iterator.values)
      end

      INVALID_RANGE(__) => begin
        Error.assertion(
          false,
          getInstanceName() +
          " got invalid range " +
          toString(iterator.exp),
          sourceInfo(),
        )
        fail()
      end
    end
  end
  return hasNext
end

function next(iterator::RangeIterator)::Tuple{RangeIterator, Expression}
  local nextExp::Expression

  @assign nextExp = begin
    @match iterator begin
      RANGEITERATOR_INT_RANGE(__) => begin
        @assign nextExp = INTEGER_EXPRESSION(iterator.current)
        @assign iterator.current = iterator.current + 1
        nextExp
      end

      INT_STEP_RANGE(__) => begin
        @assign nextExp = INTEGER_EXPRESSION(iterator.current)
        @assign iterator.current = iterator.current + iterator.stepsize
        nextExp
      end

      REAL_RANGE(__) => begin
        @assign nextExp = REAL_EXPRESSION(
          iterator.start + iterator.stepsize * iterator.current,
        )
        @assign iterator.current = iterator.current + 1
        nextExp
      end

      ARRAY_RANGE(__) => begin
        @assign nextExp = listHead(iterator.values)
        @assign iterator.values = listRest(iterator.values)
        nextExp
      end

      RANGEITERATOR_INVALID_RANGE(__) => begin
        Error.assertion(
          false,
          getInstanceName() +
          " got invalid range " +
          toString(iterator.exp),
          sourceInfo(),
        )
        fail()
      end
    end
  end
  return (iterator, nextExp)
end

function fromDim(dim::Dimension)::RangeIterator
  local iterator::RangeIterator

  @assign iterator = begin
    local ty::M_Type
    local expl::List{Expression}
    @match dim begin
      P_Dimension.Dimension.INTEGER_EXPRESSION(__) => begin
        RANGEITERATOR_INT_RANGE(1, dim.size)
      end

      P_Dimension.Dimension.BOOLEAN(__) => begin
        ARRAY_RANGE(list(
          BOOLEAN_EXPRESSION(false),
          BOOLEAN_EXPRESSION(true),
        ))
      end

      P_Dimension.Dimension.ENUM(enumType = ty && TYPE_ENUMERATION(__)) => begin
        ARRAY_RANGE(makeEnumLiterals(ty))
      end

      DIMENSION_EXP(__) => begin
        fromExp(dim.exp)
      end

      _ => begin
        Error.assertion(false, getInstanceName() + " got unknown dim", sourceInfo())
        fail()
      end
    end
  end
  return iterator
end

""" #= Returns a RangeIterator created from the given expression. If the
     expression isn't an expression that can be expanded into elements an
     invalid range will be returned that will trigger an assertion when used.
     The valididity of the returned iterator can be checked with isValid. =#"""
function RangeIterator_fromExp(exp::Expression)::RangeIterator
  local iterator::RangeIterator

  @assign iterator = begin
    local istart::Integer
    local istep::Integer
    local istop::Integer
    local rstart::AbstractFloat
    local rstep::AbstractFloat
    local rstop::AbstractFloat
    local ty::M_Type
    local literals::List{String}
    local path::Absyn.Path
    local values::List{Expression}
    @match exp begin
      ARRAY_EXPRESSION(__) => begin
        RANGEITERATOR_ARRAY_RANGE(exp.elements)
      end

      RANGE_EXPRESSION(
        start = INTEGER_EXPRESSION(istart),
        step = SOME(INTEGER_EXPRESSION(istep)),
        stop = INTEGER_EXPRESSION(istop),
      ) => begin
        RANGEITERATOR_INT_STEP_RANGE(istart, istep, istop)
      end

      RANGE_EXPRESSION(
        start = INTEGER_EXPRESSION(istart),
        step = NONE(),
        stop = INTEGER_EXPRESSION(istop),
      ) => begin
        RANGEITERATOR_INT_RANGE(istart, istop)
      end

      RANGE_EXPRESSION(
        start = REAL_EXPRESSION(rstart),
        step = SOME(REAL_EXPRESSION(rstep)),
        stop = REAL_EXPRESSION(rstop),
      ) => begin
        RANGEITERATOR_REAL_RANGE(rstart, rstep, 0, Util.realRangeSize(rstart, rstep, rstop))
      end

      RANGE_EXPRESSION(
        start = REAL_EXPRESSION(rstart),
        step = NONE(),
        stop = REAL_EXPRESSION(rstop),
      ) => begin
        RANGEITERATOR_REAL_RANGE(rstart, 1.0, 0, Util.realRangeSize(rstart, 1.0, rstop))
      end

      RANGE_EXPRESSION(
        start = ENUM_LITERAL_EXPRESSION(ty = ty, index = istart),
        step = NONE(),
        stop = ENUM_LITERAL_EXPRESSION(index = istop),
      ) => begin
        @match TYPE_ENUMERATION(typePath = _, literals = literals) = ty
        @assign values = nil
        if istart <= istop
          for i = 2:istart
            @assign literals = listRest(literals)
          end
          for i = istart:istop
            @assign values = _cons(
              ENUM_LITERAL_EXPRESSION(ty, listHead(literals), i),
              values,
            )
            @assign literals = listRest(literals)
          end
          @assign values = listReverse(values)
        end
        RANGEITERATOR_ARRAY_RANGE(values)
      end

      TYPENAME_EXPRESSION(
        ty = TYPE_ARRAY(elementType = ty && TYPE_ENUMERATION(literals = literals)),
      ) => begin
        #=  enumeration type based range
        =#
        @assign values = nil
        @assign istep = 0
        for l in literals
          @assign istep = istep + 1
          @assign values =
            _cons(ENUM_LITERAL_EXPRESSION(ty, l, istep), values)
        end
        RANGEITERATOR_ARRAY_RANGE(values)
      end

      _ => begin
        RANGEITERATOR_INVALID_RANGE(exp)
      end
    end
  end
  return iterator
end

function isValid(iterator::RangeIterator)::Bool
  local isValid::Bool

  @assign isValid = begin
    @match iterator begin
      INVALID_RANGE(__) => begin
        false
      end

      _ => begin
        true
      end
    end
  end
  return isValid
end
