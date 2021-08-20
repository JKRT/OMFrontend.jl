module P_NFExpressionIterator

using MetaModelica
using ExportAll
#= Forward declarations for uniontypes until Julia adds support for mutual recursion =#

@UniontypeDecl NFExpressionIterator

import ..P_NFExpandExp
P_ExpandExp = P_NFExpandExp
ExpandExp = P_NFExpandExp.NFExpandExp
import ..NFInstNode.P_InstNode
import ..P_NFComponentRef
P_ComponentRef = P_NFComponentRef
ComponentRef = P_NFComponentRef.NFComponentRef
import ..P_NFExpressionIterator
P_ExpressionIterator = P_NFExpressionIterator
ExpressionIterator = P_NFExpressionIterator.NFExpressionIterator
import ..NFBinding.P_Binding
import ..P_NFExpression
P_Expression = P_NFExpression
Expression = P_NFExpression.NFExpression

function toList(iterator::ExpressionIterator)::List{Expression}
  local expl::List{Expression} = nil

  local iter::ExpressionIterator
  local exp::Expression

  iter = iterator
  while hasNext(iter)
    (iter, exp) = next(iter)
    expl = _cons(exp, expl)
  end
  expl = listReverse(expl)
  return expl
end

function nextOpt(
  iterator::ExpressionIterator,
)::Tuple{ExpressionIterator, Option{Expression}}
  local nextExp::Option{Expression}

  local exp::Expression

  if hasNext(iterator)
    (iterator, exp) = next(iterator)
    nextExp = SOME(exp)
  else
    nextExp = NONE()
  end
  return (iterator, nextExp)
end

function next(iterator::ExpressionIterator)::Tuple{ExpressionIterator, Expression}
  local nextExp::Expression

  (iterator, nextExp) = begin
    local rest::List{Expression}
    local arr::List{Expression}
    local next::Expression
    @match iterator begin
      ARRAY_ITERATOR(__) => begin
        @match _cons(next, rest) = iterator.slice
        if listEmpty(rest)
          (arr, rest) = nextArraySlice(iterator.array)
          iterator = ARRAY_ITERATOR(arr, rest)
        else
          #= complex assign=#@assign iterator.slice = rest
        end
        (iterator, next)
      end

      SCALAR_ITERATOR(__) => begin
        (NONE_ITERATOR(), iterator.exp)
      end

      EACH_ITERATOR(__) => begin
        (iterator, iterator.exp)
      end

      REPEAT_ITERATOR(rest, arr) => begin
        if !listEmpty(rest)
          @match _cons(next, rest) = rest
        else
          @match _cons(next, rest) = arr
        end
        (REPEAT_ITERATOR(rest, arr), next)
      end
    end
  end
  return (iterator, nextExp)
end

function hasNext(iterator::ExpressionIterator)::Bool
  local hasNext::Bool

  hasNext = begin
    @match iterator begin
      ARRAY_ITERATOR(__) => begin
        !listEmpty(iterator.slice)
      end

      SCALAR_ITERATOR(__) => begin
        true
      end

      EACH_ITERATOR(__) => begin
        true
      end

      NONE_ITERATOR(__) => begin
        false
      end

      REPEAT_ITERATOR(__) => begin
        true
      end
    end
  end
  return hasNext
end

function fromBinding(binding::Binding)::ExpressionIterator
  local iterator::ExpressionIterator

  iterator = begin
    local expl::List{Expression}
    @match binding begin
      TYPED_BINDING(eachType = NFBinding.EachType.REPEAT) => begin
        expl = arrayScalarElements(binding.bindingExp)
        if listLength(expl) == 1
          EACH_ITERATOR(listHead(expl))
        else
          REPEAT_ITERATOR(expl, expl)
        end
      end

      TYPED_BINDING(eachType = NFBinding.EachType.EACH) => begin
        EACH_ITERATOR(binding.bindingExp)
      end

      TYPED_BINDING(__) => begin
        fromExp(binding.bindingExp)
      end

      FLAT_BINDING(__) => begin
        EACH_ITERATOR(binding.bindingExp)
      end
    end
  end
  return iterator
end

function fromExpOpt(optExp::Option{<:Expression})::ExpressionIterator
  local iterator::ExpressionIterator

  iterator = begin
    local exp::Expression
    @match optExp begin
      SOME(exp) => begin
        fromExp(exp)
      end

      _ => begin
        NONE_ITERATOR()
      end
    end
  end
  return iterator
end

function fromExp(exp::Expression)::ExpressionIterator
  local iterator::ExpressionIterator

  iterator = begin
    local arr::List{Expression}
    local slice::List{Expression}
    local e::Expression
    local expanded::Bool
    @match exp begin
      ARRAY_EXPRESSION(__) => begin
        @match (ARRAY_EXPRESSION(elements = arr), expanded) =
          P_ExpandExp.ExpandExp.expand(exp)
        if !expanded
          Error.assertion(
            false,
            getInstanceName() +
            " got unexpandable expression `" +
            toString(exp) +
            "`",
            sourceInfo(),
          )
        end
        (arr, slice) = nextArraySlice(arr)
        ARRAY_ITERATOR(arr, slice)
      end

      CREF_EXPRESSION(__) => begin
        e = P_ExpandExp.ExpandExp.expandCref(exp)
        iterator = begin
          @match e begin
            ARRAY_EXPRESSION(__) => begin
              fromExp(e)
            end

            _ => begin
              SCALAR_ITERATOR(e)
            end
          end
        end
        iterator
      end

      _ => begin
        e = P_ExpandExp.ExpandExp.expand(exp)
        if referenceEq(e, exp)
          SCALAR_ITERATOR(exp)
        else
          fromExp(e)
        end
      end
    end
  end
  return iterator
end

function nextArraySlice(
  Array::List{<:Expression},
)::Tuple{List{Expression}, List{Expression}}
  local slice::List{Expression}

  local e::Expression
  local arr::List{Expression}

  if listEmpty(Array)
    slice = nil
  else
    e = listHead(Array)
    (Array, slice) = begin
      @match e begin
        ARRAY_EXPRESSION(__) => begin
          (arr, slice) = nextArraySlice(e.elements)
          if listEmpty(arr)
            Array = listRest(Array)
          else
            #= complex assign=#@assign e.elements = arr
            Array = _cons(e, listRest(Array))
          end
          (Array, slice)
        end

        _ => begin
          (nil, Array)
        end
      end
    end
  end
  return (Array, slice)
end

@Uniontype NFExpressionIterator begin
  @Record REPEAT_ITERATOR begin

    current::List{Expression}
    all::List{Expression}
  end

  @Record NONE_ITERATOR begin

  end

  @Record EACH_ITERATOR begin

    exp::Expression
  end

  @Record SCALAR_ITERATOR begin

    exp::Expression
  end

  @Record ARRAY_ITERATOR begin

    Array::List{Expression}
    slice::List{Expression}
  end
end

@exportAll()
end
