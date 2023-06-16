#=
* This file is part of OpenModelica.
*
* Copyright (c) 1998-CurrentYear, Linköping University,
* Department of Computer and Information Science,
* SE-58183 Linköping, Sweden.
*
* All rights reserved.
*
* THIS PROGRAM IS PROVIDED UNDER THE TERMS OF GPL VERSION 3
* AND THIS OSMC PUBLIC LICENSE (OSMC-PL).
* ANY USE, REPRODUCTION OR DISTRIBUTION OF THIS PROGRAM CONSTITUTES RECIPIENT'S
* ACCEPTANCE OF THE OSMC PUBLIC LICENSE.
*
* The OpenModelica software and the Open Source Modelica
* Consortium (OSMC) Public License (OSMC-PL) are obtained
* from Linköping University, either from the above address,
* from the URLs: http:www.ida.liu.se/projects/OpenModelica or
* http:www.openmodelica.org, and in the OpenModelica distribution.
* GNU version 3 is obtained from: http:www.gnu.org/copyleft/gpl.html.
*
* This program is distributed WITHOUT ANY WARRANTY; without
* even the implied warranty of  MERCHANTABILITY or FITNESS
* FOR A PARTICULAR PURPOSE, EXCEPT AS EXPRESSLY SET FORTH
* IN THE BY RECIPIENT SELECTED SUBSIDIARY LICENSE CONDITIONS
* OF OSMC-PL.
*
* See the full OSMC Public License conditions for more details.
*
=#
@Uniontype NFExpressionIterator begin
  @Record EXPRESSION_REPEAT_ITERATOR begin
    current::List{Expression}
    all::List{Expression}
  end
  @Record EXPRESSION_NONE_ITERATOR begin
  end
  @Record EXPRESSION_EACH_ITERATOR begin
    exp::Expression
  end
  @Record EXPRESSION_SCALAR_ITERATOR begin
    exp::Expression
  end
  @Record EXPRESSION_ARRAY_ITERATOR begin
    array::List{Expression}
    slice::List{Expression}
  end
end

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
      EXPRESSION_ARRAY_ITERATOR(__) => begin
        @match _cons(next, rest) = iterator.slice
        if listEmpty(rest)
          (arr, rest) = nextArraySlice(iterator.array)
          iterator = EXPRESSION_ARRAY_ITERATOR(arr, rest)
        else
          @assign iterator.slice = rest
        end
        (iterator, next)
      end

      EXPRESSION_SCALAR_ITERATOR(__) => begin
        (EXPRESSION_NONE_ITERATOR(), iterator.exp)
      end

      EXPRESSION_EACH_ITERATOR(__) => begin
        (iterator, iterator.exp)
      end

      EXPRESSION_REPEAT_ITERATOR(rest, arr) => begin
        if !listEmpty(rest)
          @match _cons(next, rest) = rest
        else
          @match _cons(next, rest) = arr
        end
        (EXPRESSION_REPEAT_ITERATOR(rest, arr), next)
      end
    end
  end
  return (iterator, nextExp)
end

function hasNext(iterator::ExpressionIterator)::Bool
  local hasNext::Bool

  @assign hasNext = begin
    @match iterator begin
      EXPRESSION_ARRAY_ITERATOR(__) => begin
        !listEmpty(iterator.slice)
      end

      EXPRESSION_SCALAR_ITERATOR(__) => begin
        true
      end

      EXPRESSION_EACH_ITERATOR(__) => begin
        true
      end

      EXPRESSION_NONE_ITERATOR(__) => begin
        false
      end

      EXPRESSION_REPEAT_ITERATOR(__) => begin
        true
      end
    end
  end
  return hasNext
end

function fromBinding(binding::Binding)::ExpressionIterator
  local iterator::ExpressionIterator

  @assign iterator = begin
    local expl::List{Expression}
    @match binding begin
      TYPED_BINDING(eachType = EachType.REPEAT) => begin
        @assign expl = arrayScalarElements(binding.bindingExp)
        if listLength(expl) == 1
          EXPRESSION_EACH_ITERATOR(listHead(expl))
        else
          EXPRESSION_REPEAT_ITERATOR(expl, expl)
        end
      end

      TYPED_BINDING(eachType = EachType.EACH) => begin
        EXPRESSION_EACH_ITERATOR(binding.bindingExp)
      end

      TYPED_BINDING(__) => begin
        fromExpToExpressionIterator(binding.bindingExp)
      end

      FLAT_BINDING(__) => begin
        EXPRESSION_EACH_ITERATOR(binding.bindingExp)
      end
    end
  end
  return iterator
end

function fromExpOpt(optExp::Option{<:Expression})::ExpressionIterator
  local iterator::ExpressionIterator
  @assign iterator = begin
    local exp::Expression
    @match optExp begin
      SOME(exp) => begin
        fromExpToExpressionIterator(exp)
      end
      _ => begin
        EXPRESSION_NONE_ITERATOR()
      end
    end
  end
  return iterator
end

function fromExpToExpressionIterator(exp::Expression)::ExpressionIterator
  local iterator::ExpressionIterator
  @assign iterator = begin
    local arr::List{Expression}
    local slice::List{Expression}
    local e::Expression
    local expanded::Bool
    @match exp begin
      ARRAY_EXPRESSION(__) => begin
        @match (ARRAY_EXPRESSION(elements = arr), expanded) = expand(exp)
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
        @assign (arr, slice) = nextArraySlice(arr)
        EXPRESSION_ARRAY_ITERATOR(arr, slice)
      end

      CREF_EXPRESSION(__) => begin
        (e, _) = expandCref(exp)
        @assign iterator = begin
          @match e begin
            ARRAY_EXPRESSION(__) => begin
              fromExpToExpressionIterator(e)
            end

            _ => begin
              EXPRESSION_SCALAR_ITERATOR(e)
            end
          end
        end
        iterator
      end

      _ => begin
        (e,_) = expand(exp)
        if referenceEq(e, exp)
          EXPRESSION_SCALAR_ITERATOR(exp)
        else
          fromExpToExpressionIterator(e)
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
    @assign slice = nil
  else
    @assign e = listHead(Array)
    @assign (Array, slice) = begin
      @match e begin
        ARRAY_EXPRESSION(__) => begin
          @assign (arr, slice) = nextArraySlice(e.elements)
          if listEmpty(arr)
            @assign Array = listRest(Array)
          else
            @assign e.elements = arr
            @assign Array = _cons(e, listRest(Array))
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
