module HashTable6

using MetaModelica
using ExportAll
#= Forward declarations for uniontypes until Julia adds support for mutual recursion =#

FuncHashCref = Function
FuncCrefEqual = Function
FuncCrefStr = Function
FuncExpStr = Function

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

import Main.BaseHashTable

import DAE

import Main.ComponentReference

import Main.ExpressionDump

import Main.Util

Key = Tuple

Value = DAE.Exp

HashTableCrefFunctionsType = Tuple

HashTable = Tuple

""" #= Calculates a hash value for Key =#"""
function hashFunc(key::Key, mod::Integer)::Integer
  local res::Integer

  local crstr::String
  local cr1::DAE.ComponentRef
  local cr2::DAE.ComponentRef

  @assign (cr1, cr2) = key
  #=  Use same factor as Djb2 hash (33)
  =#
  @assign res = intMod(
    intAbs(
      ComponentReference.hashComponentRef(cr1) +
      33 * ComponentReference.hashComponentRef(cr2),
    ),
    mod,
  )
  return res
end

function keyEqual(tpl1::Key, tpl2::Key)::Bool
  local res::Bool

  @assign res = begin
    local cr11::DAE.ComponentRef
    local cr12::DAE.ComponentRef
    local cr21::DAE.ComponentRef
    local cr22::DAE.ComponentRef
    @matchcontinue (tpl1, tpl2) begin
      ((cr11, cr12), (cr21, cr22)) => begin
        ComponentReference.crefEqualNoStringCompare(cr11, cr21) &&
        ComponentReference.crefEqualNoStringCompare(cr12, cr22)
      end
    end
  end
  return res
end

function printKey(tpl::Key)::String
  local res::String

  @assign res =
    ComponentReference.printComponentRefStr(Util.tuple21(tpl)) +
    "," +
    ComponentReference.printComponentRefStr(Util.tuple22(tpl))
  return res
end

""" #= 
  Returns an empty HashTable.
  Using the default bucketsize..
 =#"""
function emptyHashTable()::HashTable
  local hashTable::HashTable

  @assign hashTable = emptyHashTableSized(BaseHashTable.defaultBucketSize)
  return hashTable
end

""" #= 
  Returns an empty HashTable.
  Using the bucketsize size.
 =#"""
function emptyHashTableSized(size::Integer)::HashTable
  local hashTable::HashTable

  @assign hashTable = BaseHashTable.emptyHashTableWork(
    size,
    (hashFunc, keyEqual, printKey, ExpressionDump.printExpStr),
  )
  return hashTable
end

@exportAll()
end
