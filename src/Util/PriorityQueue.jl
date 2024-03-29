module PriorityQueue

using MetaModelica
using ExportAll
#= Forward declarations for uniontypes until Julia adds support for mutual recursion =#

@UniontypeDecl Tree

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

import Main.SimCode

#= /* protected */ =#
#= /* TODO: Hide when RML is killed */ =#
#= /* This specific version... */ =#
Priority = Integer
Data = List
#= /* Replaceable types */ =#

function compareElement(el1::Element, el2::Element)::Bool
  local b::Bool

  local p1::Priority
  local p2::Priority

  @assign (p1, _) = el1
  @assign (p2, _) = el2
  @assign b = p1 <= p2
  return b
end

Element = Tuple
T = List
const empty = nil::T
#= /*
function isEmpty = listEmpty;
*/ =#

function isEmpty(ts::T)::Bool
  local isEmpty::Bool

  @assign isEmpty = listEmpty(ts)
  return isEmpty
end

function insert(elt::Element, ts::T)::T
  local ots::T

  @assign ots = ins(NODE(elt, 0, nil), ts)
  return ots
end

function meld(its1::T, its2::T)::T
  local ts::T

  @assign ts = begin
    local t1::Tree
    local t2::Tree
    local ts1::T
    local ts2::T
    @match (its1, its2) begin
      (ts1, nil()) => begin
        ts1
      end

      (nil(), ts2) => begin
        ts2
      end

      (t1 <| ts1, t2 <| ts2) => begin
        meld2(rank(t1) < rank(t2), rank(t2) < rank(t1), t1, ts1, t2, ts2)
      end
    end
  end
  return ts
end

function meld2(b1::Bool, b2::Bool, t1::Tree, inTs1::T, t2::Tree, inTs2::T)::T
  local ts::T

  @assign ts = begin
    local ts1::T
    local ts2::T
    @match (b1, b2, t1, inTs1, t2, inTs2) begin
      (true, _, _, ts1, _, ts2) => begin
        @assign ts = meld(ts1, _cons(t2, ts2))
        _cons(t1, ts)
      end

      (_, true, _, ts1, _, ts2) => begin
        @assign ts = meld(_cons(t1, ts1), ts2)
        _cons(t2, ts)
      end

      _ => begin
        ins(link(t1, t2), meld(inTs1, inTs2))
      end
    end
  end
  return ts
end

function findMin(inTs::T)::Element
  local elt::Element

  @assign elt = begin
    local t::Tree
    local x::Element
    local y::Element
    local ts::T
    @match inTs begin
      t <| nil() => begin
        root(t)
      end

      t <| ts => begin
        @assign x = root(t)
        @assign y = findMin(ts)
        if compareElement(x, y)
          x
        else
          y
        end
      end
    end
  end
  return elt
end

function deleteMin(ts::T)::T
  local ots::T

  local ts1::T
  local ts2::T

  @match (NODE(trees = ts1), ts2) = getMin(ts)
  @assign ots = meld(listReverse(ts1), ts2)
  return ots
end

function deleteAndReturnMin(ts::T)::Tuple{T, Element}
  local elt::Element
  local ots::T

  local ts1::T
  local ts2::T

  @match (NODE(elt = elt, trees = ts1), ts2) = getMin(ts)
  @assign ots = meld(listReverse(ts1), ts2)
  return (ots, elt)
end

function elements(ts::T)::List{Element}
  local elts::List{Element}

  @assign elts = elements2(ts, nil)
  return elts
end

function elements2(its::T, acc::List{<:Element})::List{Element}
  local elts::List{Element}

  @assign elts = begin
    local elt::Element
    local ts::T
    @match (its, acc) begin
      (nil(), _) => begin
        listReverse(acc)
      end

      (ts, _) => begin
        @assign (ts, elt) = deleteAndReturnMin(ts)
        elements2(ts, _cons(elt, acc))
      end
    end
  end
  return elts
end

#= /* TODO: Hide from user when we remove RML... */ =#
Rank = Integer

@Uniontype Tree begin
  @Record NODE begin

    elt::Element
    rank::Rank
    trees::T
  end
end

function root(tree::Tree)::Element
  local elt::Element

  @match NODE(elt = elt) = tree
  return elt
end

function rank(tree::Tree)::Rank
  local rank::Rank

  @match NODE(rank = rank) = tree
  return rank
end

function link(t1::Tree, t2::Tree)::Tree
  local t::Tree

  @assign t = begin
    local e1::Element
    local e2::Element
    local r1::Rank
    local r2::Rank
    local ts1::T
    local ts2::T
    @match (t1, t2) begin
      (NODE(e1, r1, ts1), NODE(e2, r2, ts2)) => begin
        @assign r1 = r1 + 1
        @assign r2 = r2 + 1
        @assign ts1 = _cons(t2, ts1)
        @assign ts2 = _cons(t1, ts2)
        if compareElement(root(t1), root(t2))
          NODE(e1, r1, ts1)
        else
          NODE(e2, r2, ts2)
        end
      end
    end
  end
  return t
end

function ins(t::Tree, its::T)::T
  local ots::T

  @assign ots = begin
    local t1::Tree
    local t2::Tree
    local ts::T
    @match (t, its) begin
      (_, nil()) => begin
        list(t)
      end

      (t1, t2 <| ts) => begin
        if rank(t1) < rank(t2)
          _cons(t1, _cons(t2, ts))
        else
          ins(link(t1, t2), ts)
        end
      end
    end
  end
  return ots
end

function getMin(ts::T)::Tuple{Tree, T}
  local ots::T
  local min::Tree

  @assign (min, ots) = begin
    local t::Tree
    local t1::Tree
    local t2::Tree
    local ts1::T
    local ts2::T
    local b::Bool
    @match ts begin
      t <| nil() => begin
        (t, nil)
      end

      t1 <| ts1 => begin
        @assign (t2, ts2) = getMin(ts1)
        @assign b = compareElement(root(t1), root(t2))
        (if b
          t1
        else
          t2
        end, if b
          ts1
        else
          _cons(t1, ts2)
        end)
      end
    end
  end
  return (min, ots)
end

@exportAll()
end
