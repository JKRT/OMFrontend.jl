abstract type NFRangeIterator end

struct RANGEITERATOR_INVALID_RANGE{T0 <: Expression} <: NFRangeIterator
  exp::T0
end

mutable struct RANGEITERATOR_ARRAY_RANGE <: NFRangeIterator
  values::List{Expression}
end

mutable struct RANGEITERATOR_REAL_RANGE{T0 <: AbstractFloat, T1 <: Integer} <: NFRangeIterator
  start::T0
  stepsize::T0
  current::T1
  steps::T1
end

mutable struct RANGEITERATOR_INT_STEP_RANGE{T0 <: Integer} <: NFRangeIterator
  current::T0
  stepsize::T0
  last::T0
end

mutable struct RANGEITERATOR_INT_RANGE{T0 <: Integer} <: NFRangeIterator
  current::T0
  last::T0
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

function map(iterator::RangeIterator, func::FuncT)
  local lst::List = nil

  local iter::RangeIterator = iterator
  local exp::Expression

  while hasNext(iter)
     (iter, exp) = next(iter)
    lst = _cons(func(exp), lst)
  end
  lst = listReverse(lst)
  return lst
end

function toListReverse(iterator::RangeIterator)::List{Expression}
  local expl::List{Expression} = nil
  local iter::RangeIterator = iterator
  local exp::Expression
  while hasNext(iter)
     (iter, exp) = next(iter)
    expl = _cons(exp, expl)
  end
  return expl
end

function toList(iterator::RangeIterator)::List{Expression}
  local expl::List{Expression} = listReverse(toListReverse(iterator))
  return expl
end

function hasNext(iterator::RangeIterator)::Bool
  local hasNext::Bool
  hasNext = begin
    @match iterator begin
      RANGEITERATOR_INT_RANGE(__) => begin
        iterator.current <= iterator.last
      end
      RANGEITERATOR_INT_STEP_RANGE(__) => begin
        if iterator.stepsize > 0
          iterator.current <= iterator.last
        else
          iterator.current >= iterator.last
        end
      end
      RANGEITERATOR_REAL_RANGE(__) => begin
        iterator.current < iterator.steps
      end
      RANGEITERATOR_ARRAY_RANGE(__) => begin
        !listEmpty(iterator.values)
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
  return hasNext
end

function next(iterator::RangeIterator)::Tuple{RangeIterator, Expression}
  local nextExp::Expression
  nextExp = begin
    @match iterator begin
      RANGEITERATOR_INT_RANGE(__) => begin
        nextExp = INTEGER_EXPRESSION(iterator.current)
        iterator.current = iterator.current + 1
        nextExp
      end

      RANGEITERATOR_INT_STEP_RANGE(__) => begin
        nextExp = INTEGER_EXPRESSION(iterator.current)
        iterator.current = iterator.current + iterator.stepsize
        nextExp
      end

      RANGEITERATOR_REAL_RANGE(__) => begin
        nextExp = REAL_EXPRESSION(
          iterator.start + iterator.stepsize * iterator.current,
        )
        iterator.current = iterator.current + 1
        nextExp
      end

      RANGEITERATOR_ARRAY_RANGE(__) => begin
        nextExp = listHead(iterator.values)
        iterator.values = listRest(iterator.values)
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
  iterator = begin
    local ty::M_Type
    local expl::List{Expression}
    @match dim begin
      DIMENSION_INTEGER(__) => begin
        RANGEITERATOR_INT_RANGE(1, dim.size)
      end

      DIMENSION_BOOLEAN(__) => begin
        ARRAY_RANGE(list(
          BOOLEAN_EXPRESSION(false),
          BOOLEAN_EXPRESSION(true),
        ))
      end

       DIMENSION_ENUM(enumType = ty && TYPE_ENUMERATION(__)) => begin
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

"""
Returns a RangeIterator created from the given expression. If the
expression isn't an expression that can be expanded into elements an
invalid range will be returned that will trigger an assertion when used.
The valididity of the returned iterator can be checked with isValid.
"""
function RangeIterator_fromExp(exp::Expression)::RangeIterator
  local iterator::RangeIterator
  @assign iterator = begin
    local istart::Int
    local istep::Int
    local istop::Int
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
        values = nil
        if istart <= istop
          for i = 2:istart
            literals = listRest(literals)
          end
          for i = istart:istop
            values = _cons(
              ENUM_LITERAL_EXPRESSION(ty, listHead(literals), i),
              values,
            )
            literals = listRest(literals)
          end
          values = listReverse(values)
        end
        RANGEITERATOR_ARRAY_RANGE(values)
      end

      TYPENAME_EXPRESSION(
        ty = TYPE_ARRAY(elementType = ty && TYPE_ENUMERATION(literals = literals)),
      ) => begin
        #=  enumeration type based range
        =#
        values = nil
        istep = 0
        for l in literals
          istep = istep + 1
          values =
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
  local valid::Bool
  valid = begin
    @match iterator begin
      RANGEITERATOR_INVALID_RANGE(__) => begin
        false
      end
      _ => begin
        true
      end
    end
  end
  return valid
end
