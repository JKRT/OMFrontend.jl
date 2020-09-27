module HashTableCrToCrEqLst

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

import Main.BackendDAE

import Main.ComponentReference

import Main.BackendDump

import ListUtil

Key = DAE.ComponentRef

Value = List

HashTableCrefFunctionsType = Tuple

HashTable = Tuple

""" #= 
  Returns an empty HashTable.
  Using the default bucketsize..
 =#"""
function emptyHashTable()::HashTable
  local hashTable::HashTable

  @assign hashTable = emptyHashTableSized(BaseHashTable.defaultBucketSize)
  return hashTable
end

""" #= Returns an empty HashTable.
 Using the bucketsize size. =#"""
function emptyHashTableSized(size::Integer)::HashTable
  local hashTable::HashTable

  @assign hashTable = BaseHashTable.emptyHashTableWork(
    size,
    (
      ComponentReference.hashComponentRefMod,
      ComponentReference.crefEqual,
      ComponentReference.printComponentRefStr,
      printTupleComponentRefEqListStr,
    ),
  )
  return hashTable
end

function printTupleComponentRefEqListStr(
  cr_eq_lst::List{<:Tuple{<:DAE.ComponentRef, BackendDAE.Equation}},
)::String
  local res::String

  @assign res = stringDelimitList(ListUtil.map(cr_eq_lst, printTupleComponentRefEqStr), ",")
  return res
end

function printTupleComponentRefEqStr(
  cr_eq::Tuple{<:DAE.ComponentRef, BackendDAE.Equation},
)::String
  local res::String

  local cr::DAE.ComponentRef
  local eq::BackendDAE.Equation

  @assign (cr, eq) = cr_eq
  @assign res =
    "{" +
    ComponentReference.printComponentRefStr(cr) +
    "," +
    BackendDump.equationString(eq) +
    "}"
  return res
end

@exportAll()
end
