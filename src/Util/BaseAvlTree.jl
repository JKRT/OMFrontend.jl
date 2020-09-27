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

module BaseAvlTree  #=TODO: Originally partial =#

using MetaModelica
using ExportAll
#= Forward declarations for uniontypes until Julia adds support for mutual recursion =#
using ..BaseAvlSet #= Modelica extend clause =#

Value = Integer

include("./baseAvlTreeCode.jl")
""" #= Balances a Tree =#"""
function balance(inTree::Tree)::Tree
  local outTree::Tree = inTree
  @assign outTree = begin
    local lh::Integer
    local rh::Integer
    local diff::Integer
    local child::Tree
    local balanced_tree::Tree
    @match outTree begin
      LEAF(__) => begin
        inTree
      end
      NODE(__) => begin
        @assign lh = height(outTree.left)
        @assign rh = height(outTree.right)
        @assign diff = lh - rh
        if diff < (-1)
          @assign balanced_tree = if calculateBalance(outTree.right) > 0
            rotateLeft(setTreeLeftRight(
              outTree,
              left = outTree.left,
              right = rotateRight(outTree.right),
            ))
          else
            rotateLeft(outTree)
          end
        elseif diff > 1
          @assign balanced_tree = if calculateBalance(outTree.left) < 0
            rotateRight(setTreeLeftRight(
              outTree,
              left = rotateLeft(outTree.left),
              right = outTree.right,
            ))
          else
            rotateRight(outTree)
          end
        elseif outTree.height != max(lh, rh) + 1
          @assign outTree.height = max(lh, rh) + 1
          @assign balanced_tree = outTree
        else
          @assign balanced_tree = outTree
        end
        balanced_tree
      end
    end
  end
  return outTree
end


@exportAll()
end
