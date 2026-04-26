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
@Mutable_Uniontype NFExpressionIterator begin
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
        @match Cons{Expression}(next, rest) = iterator.slice
        if listEmpty(rest)
          (arr, rest) = nextArraySlice(iterator.array)
          iterator = EXPRESSION_ARRAY_ITERATOR(arr, rest)
        else
          iterator.slice = rest
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
  hasNext = begin
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

function fromExpToExpressionIterator(exp::Expression)
  local iterator::ExpressionIterator
  iterator = begin
    local arr::List{Expression}
    local slice::List{Expression}
    local e::Expression
    local expanded::Bool
    @match exp begin
      ARRAY_EXPRESSION(__) => begin
        (e, expanded) = expand(exp)
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
        (arr, slice) = nextArraySlice(arrayList(e.elements))
        EXPRESSION_ARRAY_ITERATOR(arr, slice)
        #makeArrayIterator(e)
      end
      CREF_EXPRESSION(__) => begin
        (e, _) = expandCref(exp)
        iterator = begin
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
        (e, expanded) = expand(exp)
        if expanded
          if referenceEq(e, exp)
            EXPRESSION_SCALAR_ITERATOR(exp)
          else
            fromExpToExpressionIterator(e)
          end
        else
          EXPRESSION_NONE_ITERATOR()
        end
      end
    end
  end
  return iterator
end

"""
John March 2024
"""
function makeArrayIterator(exp::Expression)
  arrays = flattenArray(exp, nil)
  if listEmpty(arrays)
    iterator = EXPRESSION_ARRAY_ITERATOR(nil, 1, arrays)
  else
    iterator = EXPRESSION_ARRAY_ITERATOR(arrays, 1, listRest(arrays))
  end
end

"""
  John March 2024
"""
function flattenArray(exp::Expression, arrays::List{List})
  arrays = flattenArray_impl(exp, nil)
  arrays = listReverseInPlace(arrays)
  while ! (listEmpty(arrays) && isempty(listHead(arrays)))
    arrays = listRest(arrays)
  end
  return arrays
end

function flattenArray_impl(exp::Expression, arrays::List{List})
  if isVector(exp.ty)
    arrays = arrayElements(exp) <| arrays
  else
    for e in arrayElements(exp)
      arrays = flattenArray_impl(e, arrays)
    end
  end
  return arrays
end

"""
Fetches the next slice of arrays.
TODO: John Jan 2024. Can be optimized I think
"""
function nextArraySlice(
  arrayArg::List{<:Expression},
  )::Tuple{List{Expression}, List{Expression}}
  local slice::List{Expression}
  local e::Expression
  local arr::List{Expression}
  if listEmpty(arrayArg)
    slice = nil
  else
    e = listHead(arrayArg)
    (arrayArg, slice) = begin
      @match e begin
        ARRAY_EXPRESSION(__) => begin
          (arr, slice) = nextArraySlice(arrayList(e.elements))
          if listEmpty(arr)
            arrayArg = listRest(arrayArg)
          else
            local eElements = arr
            e = ARRAY_EXPRESSION(e.ty, listArray(eElements), e.literal)
            arrayArg = Cons{Expression}(e, listRest(arrayArg))
          end
          (arrayArg, slice)
        end

        _ => begin
          (nil, arrayArg)
        end
      end
    end
  end
  return (arrayArg, slice)
end


function toString(@nospecialize(iterator::NFExpressionIterator))
  local buffer = IOBuffer()
  println(buffer, "$(typeof(iterator)):")
  @match iterator begin
    EXPRESSION_ARRAY_ITERATOR(__) => begin
      println(buffer, toString(iterator.array))
      println(buffer, toString(iterator.slice))
    end

    EXPRESSION_SCALAR_ITERATOR(__) => begin
      println(buffer, toString(iterator.exp))
    end

    EXPRESSION_EACH_ITERATOR(__) => begin
      println(buffer, toString(iterator.exp))
    end

    EXPRESSION_NONE_ITERATOR(__) => begin
      println(buffer, "NONE_ITERATOR")
    end

    EXPRESSION_REPEAT_ITERATOR(__) => begin
      println(buffer, toString(iterator.current))
      println(buffer, toString(iterator.all))
    end
  end
  println(buffer, "end typeof(iterator):")
  return String(take!(buffer))
end
