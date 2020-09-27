module HashTableTypeToExpType

using MetaModelica
using ExportAll
#= Forward declarations for uniontypes until Julia adds support for mutual recursion =#

FuncHashType = Function
FuncTypeEqual = Function
FuncTypeStr = Function
FuncExpTypeStr = Function

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
#= /* Below is the instance specific code. For each hashtable the user must define:

Key       - The key used to uniquely define elements in a hashtable
Value     - The data to associate with each key
hashFunc   - A function that maps a key to a positive integer.
keyEqual   - A comparison function between two keys, returns true if equal.
*/ =#
#= /* HashTable instance specific code */ =#

import DAE

import Main.BaseHashTable

import Main.Types

Key = DAE.Type

Value = DAE.Type

HashTableCrefFunctionsType = Tuple

HashTable = Tuple

function myHash(inTy::DAE.Type, hashMod::Integer)::Integer
  local hash::Integer

  local str::String
  local tt::DAE.Type
  local t::DAE.Type

  #= str := Types.printTypeStr(inTy);
  =#
  #= hash := stringHashDjb2Mod(str, hashMod);
  =#
  #= print(\"hash: \" + intString(hash) + \" for \" + str + \"\\n\");
  =#
  @assign (tt, _) = inTy
  @assign t = (tt, NONE())
  @assign hash = valueHashMod(t, hashMod)
  return hash
end

""" #= Returns an empty HashTable.
 Using the default bucketsize.. =#"""
function emptyHashTable()::HashTable
  local hashTable::HashTable

  @assign hashTable = emptyHashTableSized(BaseHashTable.biggerBucketSize)
  return hashTable
end

""" #= Returns an empty HashTable.
  Using the bucketsize size. =#"""
function emptyHashTableSized(size::Integer)::HashTable
  local hashTable::HashTable

  @assign hashTable = BaseHashTable.emptyHashTableWork(
    size,
    (myHash, Types.typesElabEquivalent, Types.printTypeStr, Types.printExpTypeStr),
  )
  return hashTable
end

@exportAll()
end
