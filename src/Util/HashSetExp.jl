module HashSetExp

using MetaModelica
using ExportAll
#= Forward declarations for uniontypes until Julia adds support for mutual recursion =#

FuncHashCref = Function
FuncCrefEqual = Function
FuncCrefStr = Function

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
#= /* Below is the instance specific code. For each hashset the user must define:

Key       - The key used to uniquely define elements in a hashset
hashFunc   - A function that maps a key to a positive integer.
keyEqual   - A comparison function between two keys, returns true if equal.
*/ =#
#= /* HashSetExp instance specific code */ =#

import Main.BaseHashSet

import DAE

import Main.Expression

import Main.ExpressionDump

Key = DAE.Exp

HashSetCrefFunctionsType = Tuple

HashSet = Tuple

""" #= 
  Returns an empty HashSet.
  Using the default bucketsize..
 =#"""
function emptyHashSet()::HashSet
  local hashSet::HashSet

  @assign hashSet = emptyHashSetSized(BaseHashSet.defaultBucketSize)
  return hashSet
end

""" #= Returns an empty HashSet.
 Using the bucketsize size =#"""
function emptyHashSetSized(size::Integer)::HashSet
  local hashSet::HashSet

  @assign hashSet = BaseHashSet.emptyHashSetWork(
    size,
    (Expression.hashExpMod, Expression.expEqual, ExpressionDump.printExpStr),
  )
  return hashSet
end

@exportAll()
end
