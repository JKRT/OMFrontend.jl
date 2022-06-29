#= /*
* This file is part of OpenModelica.
*
* Copyright (c) 1998-CurrentYear, Open Source Modelica Consortium (OSMC),
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
module BaseHashTable

using MetaModelica
using ExportAll
#= Forward declarations for uniontypes until Julia adds support for mutual recursion =#

FuncHash = Function
FuncEq = Function
FuncKeyString = Function
FuncValString = Function

#=  Below is the instance specific code. For each hashtable the user must define:
=#
#=  Key      - The key used to uniquely define elements in a hashtable
=#
#=  Value    - The data to associate with each key
=#
#=  hashFunc - A function that maps a key to a positive integer.
=#
#=  keyEqual - A comparison function between two keys, returns true if equal.
=#

import ArrayUtil
import ListUtil
#=  Generic hashtable code below
=#
#=  adrpo: use a prime here (pick your poison):
=#
#=         3   5   7  11  13  17  19  23  29  31  37  41  43  47  53  59  61  67
=#
#=        71  73  79  83  89  97 101 103 107 109 113 127 131 137 139 149 151 157
=#
#=       163 167 173 179 181 191 193 197 199 211 223 227 229 233 239 241 251 257
=#
#=       263 269 271 277 281 283 293 307 311 313 317 331 337 347 349 353 359 367
=#
#=       373 379 383 389 397 401 409 419 421 431 433 439 443 449 457 461 463 467
=#
#=       479 487 491 499 503 509 521 523 541 547 557 563 569 571 577 587 593 599
=#
#=       601 607 613 617 619 631 641 643 647 653 659 661 673 677 683 691 701 709
=#
#=       719 727 733 739 743 751 757 761 769 773 787 797 809 811 821 823 827 829
=#
#=       839 853 857 859 863 877 881 883 887 907 911 919 929 937 941 947 953 967
=#
#=       971 977 983 991 997 1013 2053 3023 4013 4999 5051 5087 24971
=#
#=
=#
#=  You can also use Util.nextPrime if you know exactly how large the hash table
=#
#=  should be.
=#

const lowBucketSize = 257::Integer

const avgBucketSize = 2053::Integer

const bigBucketSize = 4013::Integer

const biggerBucketSize = 25343::Integer

const hugeBucketSize = 536870879::Integer #= 2^29 - 33 is prime :) =#

const defaultBucketSize = avgBucketSize::Integer

Key = Any
Value = Any
HashEntry = Tuple
HashNode = List
HashTable = Tuple
HashVector = Array
ValueArray = Tuple
FuncsTuple = Tuple

""" #= calculate the values array size based on the bucket size =#"""
function bucketToValuesSize(szBucket::Integer)::Integer
  local szArr::Integer

  @assign szArr = realInt(realMul(intReal(szBucket), 0.6))
  #=  intDiv(szBucket, 10);
  =#
  return szArr
end

function emptyHashTableWork(szBucket::Integer, fntpl::FuncsTuple)::HashTable
  local hashTable::HashTable

  local arr::Array{List{Tuple{Key, Integer}}}
  local lst::List{Option{Tuple{Key, Value}}}
  local emptyarr::Array{Option{Tuple{Key, Value}}}

  local szArr::Integer

  if szBucket < 1
    # Error.addInternalError(
    #   "Got internal hash table size " + intString(szBucket) + " <1",
    #   sourceInfo(),
    # )
    fail()
  end
  @assign arr = arrayCreate(szBucket, nil)
  @assign szArr = bucketToValuesSize(szBucket)
  @assign emptyarr = arrayCreate(szArr, NONE())
  @assign hashTable = (arr, (0, szArr, emptyarr), szBucket, fntpl)
  return hashTable
end

""" #= Add a Key-Value tuple to hashtable.
   If the Key-Value tuple already exists, the function updates the Value. =#"""
function add(entry::HashEntry, hashTable::HashTable)::HashTable
  local outHashTable::HashTable

  local hashvec::HashVector
  local varr::ValueArray
  local bsize::Integer
  local hash_idx::Integer
  local arr_idx::Integer
  local new_pos::Integer
  local fntpl::FuncsTuple
  local hashFunc::FuncHash
  local keyEqual::FuncEq
  local key::Key
  local key2::Key
  local val::Value
  local indices::HashNode

  (key, _) = entry
  #@match (hashFunc, keyEqual, _, _) = fntpl
  @match (hashvec, varr, bsize, (hashFunc, keyEqual, _, _)) = hashTable
  fntpl = hashTable[4]
  hash_idx = hashFunc(key, bsize) + 1
  indices = hashvec[hash_idx]
  for i in indices
    (key2, _) = i
    if keyEqual(key, key2)
      (_, arr_idx) = i
      valueArraySet(varr, arr_idx, entry)
      outHashTable = hashTable
      return outHashTable
    end
  end
  (varr, new_pos) = valueArrayAdd(varr, entry)
  arrayUpdate(hashvec, hash_idx, _cons((key, new_pos), indices))
  outHashTable = (hashvec, varr, bsize, fntpl)
  return outHashTable
end

""" #=
author: PA.
dump statistics on how many entries per hash value. Useful to see how hash function behaves =#"""
function dumpHashTableStatistics(hashTable::HashTable)
  return @assign _ = begin
    local hvec::HashVector
    @match hashTable begin
      (hvec, _, _, _) => begin
        print("index list lengths:\\n")
        print(stringDelimitList(list(intString(listLength(l)) for l in hvec), ","))
        print("\\n")
        print(
          "non-zero: " +
          String(sum(1 for l in hvec if !listEmpty(l))) +
          "/" +
          String(arrayLength(hvec)) +
          "\\n",
        )
        print("max element: " + String(max(listLength(l) for l in hvec)) + "\\n")
        print("total entries: " + String(sum(listLength(l) for l in hvec)) + "\\n")
        ()
      end
    end
  end
end

""" #= Add a Key-Value tuple to hashtable, without checking if it already exists.
   This function is thus more efficient than add if you already know that the
   Key-Value tuple doesn't already exist in the hashtable. =#"""
function addNoUpdCheck(entry::HashEntry, hashTable::HashTable)::HashTable
  local outHashTable::HashTable

  @assign outHashTable = begin
    local indx::Integer
    local newpos::Integer
    local n::Integer
    local bsize::Integer
    local varr::ValueArray
    local indexes::HashNode
    local hashvec::HashVector
    local v::Tuple{Key, Value}
    local key::Key
    local value::Value
    local fntpl::FuncsTuple
    local hashFunc::FuncHash
    #=  Adding when not existing previously
    =#
    @matchcontinue (entry, hashTable) begin
      (v && (key, _), (hashvec, varr, bsize, fntpl && (hashFunc, _, _, _))) => begin
        @assign indx = hashFunc(key, bsize) + 1
        @assign (varr, newpos) = valueArrayAdd(varr, v)
        @assign indexes = hashvec[indx]
        @assign hashvec = arrayUpdate(hashvec, indx, _cons((key, newpos), indexes))
        (hashvec, varr, bsize, fntpl)
      end

      _ => begin
        print("- BaseHashTable.addNoUpdCheck failed\\n")
        fail()
      end
    end
  end
  return outHashTable
end

""" #= Add a Key-Value tuple to hashtable. If the Key is already used it fails. =#"""
function addUnique(entry::HashEntry, hashTable::HashTable)::HashTable
  local outHashTable::HashTable

  local indx::Integer
  local newpos::Integer
  local bsize::Integer
  local varr::ValueArray
  local indexes::HashNode
  local hashvec::HashVector
  local key::Key
  local fntpl::FuncsTuple
  local hashFunc::FuncHash

  #=  Adding when not existing previously
  =#
  @assign (key, _) = entry
  @match (hashvec, varr, bsize, (@match (hashFunc, _, _, _) = fntpl)) = hashTable
  @shouldFail @assign _ = get(key, hashTable)
  @assign indx = hashFunc(key, bsize) + 1
  @assign (varr, newpos) = valueArrayAdd(varr, entry)
  @assign indexes = hashvec[indx]
  @assign hashvec = arrayUpdate(hashvec, indx, _cons((key, newpos), indexes))
  @assign outHashTable = (hashvec, varr, bsize, fntpl)
  return outHashTable
end

""" #= Updates an already existing value in the hashtable. Fails if the entry does
   not exist. =#"""
function update(entry::HashEntry, hashTable::HashTable)
  local varr::ValueArray
  local index::Integer
  local key::Key

  @assign (key, _) = entry
  @assign (_, varr, _, _) = hashTable
  @assign index = hasKeyIndex(key, hashTable)
  @match true = valueArrayKeyIndexExists(varr, index)
  return valueArraySet(varr, index, entry)
end

""" #= Deletes the Value associatied with Key from the HashTable.
   Note: This function does not delete from the index table, only from the
   ValueArray. This means that a lot of deletions will not make the HashTable
   more compact, it will still contain a lot of incices information. =#"""
function delete(key::Key, hashTable::HashTable)
  local indx::Integer
  local varr::ValueArray

  @assign indx = hasKeyIndex(key, hashTable)
  @assign (_, varr, _, _) = hashTable
  if !valueArrayKeyIndexExists(varr, indx)
    print("BaseHashTable.delete failed\\n")
    fail()
  end
  return valueArrayClear(varr, indx)
end

""" #= checks if the given key is in the hashTable =#"""
function hasKey(key::Key, hashTable::HashTable)::Bool
  local b::Bool

  local varr::ValueArray

  @assign (_, varr, _, _) = hashTable
  @assign b = valueArrayKeyIndexExists(varr, hasKeyIndex(key, hashTable))
  return b
end

""" #= Returns true if any of the keys are present in the hashtable. Stops and returns true upon first occurence =#"""
function anyKeyInHashTable(keys::List{<:Key}, ht::HashTable)::Bool
  local res::Bool

  for key in keys
    if hasKey(key, ht)
      @assign res = true
      return res
    end
  end
  @assign res = false
  return res
end

""" #= Returns a Value given a Key and a HashTable. =#"""
function get(key::Key, hashTable::HashTable)::Value
  local value::Value

  local i::Integer
  local varr::ValueArray

  @assign i = hasKeyIndex(key, hashTable)
  @match false = i == (-1)
  @assign (_, varr, _, _) = hashTable
  @assign (_, value) = getValueArray(varr, i)
  return value
end

""" #= help function to get and hasKey =#"""
function hasKeyIndex(key::Key, hashTable::HashTable)::Integer
  local indx::Integer

  local hashindx::Integer
  local bsize::Integer
  local indexes::HashNode
  local hashvec::HashVector
  local keyEqual::FuncEq
  local hashFunc::FuncHash

  @assign (hashvec, _, bsize, (hashFunc, keyEqual, _, _)) = hashTable
  @assign hashindx = hashFunc(key, bsize) + 1
  @assign indexes = hashvec[hashindx]
  @assign indx = hasKeyIndex2(key, indexes, keyEqual)
  return indx
end

""" #= Helper function to get =#"""
function hasKeyIndex2(key::Key, keyIndices::HashNode, keyEqual::FuncEq)::Integer
  local index::Integer #= Returns -1 on failure =#

  local key2::Key

  for keyIndex in keyIndices
    @assign (key2, index) = keyIndex
    if keyEqual(key, key2)
      return index #= Returns -1 on failure =#
    end
  end
  @assign index = -1 #= Mark the failure so we can do hasKey without matchcontinue =#
  return index #= Returns -1 on failure =#
end

function dumpHashTable(t::HashTable)
  local printKey::FuncKeyString
  local printValue::FuncValString
  local k::Key
  local v::Value

  @assign (_, _, _, (_, _, printKey, printValue)) = t
  print("HashTable:\\n")
  return for entry in hashTableList(t)
    @assign (k, v) = entry
    print("{")
    print(printKey(k))
    print(",{")
    print(printValue(v))
    print("}}\\n")
  end
end

function debugDump(ht::HashTable)
  local printKey::FuncKeyString
  local printValue::FuncValString
  local k::Key
  local v::Value
  local n::Integer
  local size::Integer
  local i::Integer
  local j::Integer
  local szBucket::Integer
  local arr::Array{Option{HashEntry}}
  local he::HashEntry
  local hashVector::Array{HashNode}

  @assign (hashVector, (n, size, arr), szBucket, (_, _, printKey, printValue)) = ht
  print("Debug HashTable:\\n")
  print("szBucket: " + intString(szBucket) + "\\n")
  print("Debug ValueArray:\\n")
  print("number of entires: " + intString(n) + "\\n")
  print("size: " + intString(size) + "\\n")
  @assign i = 0
  for entry in arr
    @assign i = i + 1
    if isSome(entry)
      @match SOME(he) = entry
      print(intString(i) + ": " + dumpTuple(he, printKey, printValue) + "\\n")
    end
  end
  print("Debug HashVector:\\n")
  @assign i = 0
  return for node in hashVector
    @assign i = i + 1
    if !listEmpty(node)
      print(intString(i) + ":")
      for n in node
        @assign (k, j) = n
        print(" {" + printKey(k) + ", " + intString(j) + "}")
      end
      print("\\n")
    end
  end
end

function dumpTuple(
  tpl::HashEntry,
  printKey::FuncKeyString,
  printValue::FuncValString,
)::String
  local str::String

  local k::Key
  local v::Value
  local sk::String
  local sv::String

  @assign (k, v) = tpl
  @assign sk = printKey(k)
  @assign sv = printValue(v)
  @assign str = stringAppendList(list("{", sk, ",{", sv, "}}"))
  return str
end

""" #= Returns the Value entries as a list of Values. =#"""
function hashTableValueList(hashTable::HashTable)::List{Value}
  local valLst::List{Value}

  @assign valLst = ListUtil.unzipSecond(hashTableList(hashTable))
  return valLst
end

""" #= Returns the Key entries as a list of Keys. =#"""
function hashTableKeyList(hashTable::HashTable)::List{Key}
  local valLst::List{Key}

  @assign valLst = ListUtil.unzipFirst(hashTableList(hashTable))
  return valLst
end

""" #= Returns the entries in the hashTable as a list of HashEntries. =#"""
function hashTableList(hashTable::HashTable)::List{HashEntry}
  local outEntries::List{HashEntry}

  local varr::ValueArray

  @assign (_, varr, _, _) = hashTable
  @assign outEntries = valueArrayList(varr)
  return outEntries
end

""" #= Returns the entries in the hashTable as a list of HashEntries, in reverse
   order. =#"""
function hashTableListReversed(hashTable::HashTable)::List{HashEntry}
  local entries::List{HashEntry}

  local varr::ValueArray

  @assign (_, varr, _, _) = hashTable
  @assign entries = valueArrayListReversed(varr)
  return entries
end

""" #= Transforms a ValueArray to a HashEntry list. =#"""
function valueArrayList(valueArray::ValueArray)::List{HashEntry}
  local outEntries::List{HashEntry}

  local arr::Array{Option{HashEntry}}

  @assign (_, _, arr) = valueArray
  @assign outEntries = ArrayUtil.fold(arr, ListUtil.consOption, nil)
  @assign outEntries = listReverse(outEntries)
  return outEntries
end

""" #= Transforms a ValueArray to a HashEntry list, in reverse order compared to
   valueArrayList. =#"""
function valueArrayListReversed(valueArray::ValueArray)::List{HashEntry}
  local entries::List{HashEntry}

  local arr::Array{Option{HashEntry}}

  @assign (_, _, arr) = valueArray
  @assign entries = ArrayUtil.fold(arr, ListUtil.consOption, nil)
  return entries
end

""" #= Returns the number of elements inserted into the table =#"""
function hashTableCurrentSize(hashTable::HashTable)::Integer
  local sz::Integer

  local va::ValueArray

  @assign (_, va, _, _) = hashTable
  @assign sz = valueArrayLength(va)
  return sz
end

""" #= Returns the number of elements in the ValueArray =#"""
function valueArrayLength(valueArray::ValueArray)::Integer
  local sz::Integer

  @assign (sz, _, _) = valueArray
  return sz
end

""" #= Adds an entry last to the ValueArray, increasing array size if no space left
   by factor 1.4 =#"""
function valueArrayAdd(valueArray::ValueArray, entry::HashEntry)::Tuple{ValueArray, Integer}
  local newpos::Integer
  local outValueArray::ValueArray

  @assign (outValueArray, newpos) = begin
    local n::Integer
    local size::Integer
    local expandsize::Integer
    local newsize::Integer
    local arr::Array{Option{HashEntry}}
    local rsize::AbstractFloat
    local rexpandsize::AbstractFloat
    @matchcontinue (valueArray, entry) begin
      ((n, size, arr), _) => begin
        if ! (n < size)
          fail() #= Have space to add array elt. =#
        end #= Have space to add array elt. =#
        @assign n = n + 1
        @assign arr = arrayUpdate(arr, n, SOME(entry))
        ((n, size, arr), n)
      end

      ((n, size, arr), _) => begin
        if n < size
          fail() #= Do NOT have space to add array elt. Expand with factor 1.4 =#
        end #= Do NOT have space to add array elt. Expand with factor 1.4 =#
        @assign rsize = intReal(size)
        @assign rexpandsize = rsize * 0.4
        @assign expandsize = realInt(rexpandsize)
        @assign expandsize = intMax(expandsize, 1)
        @assign newsize = expandsize + size
        @assign arr = ArrayUtil.expand(expandsize, arr, NONE())
        @assign n = n + 1
        @assign arr = arrayUpdate(arr, n, SOME(entry))
        ((n, newsize, arr), n)
      end

      _ => begin
        print("-HashTable.valueArrayAdd failed\\n")
        fail()
      end
    end
  end
  return (outValueArray, newpos)
end

""" #= Set the n:th variable in the ValueArray to value. =#"""
function valueArraySet(valueArray::ValueArray, pos::Integer, entry::HashEntry)::ValueArray
  local outValueArray::ValueArray

  @assign outValueArray = begin
    local arr::Array{Option{HashEntry}}
    local n::Integer
    local size::Integer
    @matchcontinue (valueArray, pos, entry) begin
      ((n, size, arr), _, _) => begin
        @match true = pos <= size
        @assign arr = arrayUpdate(arr, pos, SOME(entry))
        (n, size, arr)
      end

      ((_, size, arr), _, _) => begin
        # Error.addInternalError(
        #   "HashTable.valueArraySet(pos=" +
        #   String(pos) +
        #   ") size=" +
        #   String(size) +
        #   " arrSize=" +
        #   String(arrayLength(arr)) +
        #   " failed\\n",
        #   sourceInfo(),
        # )
        fail()
      end
    end
  end
  return outValueArray
end

""" #= Clears the n:th variable in the ValueArray (set to NONE()). =#"""
function valueArrayClear(valueArray::ValueArray, pos::Integer)
  local arr::Array{Option{HashEntry}}
  local size::Integer

  @assign (_, size, arr) = valueArray
  @match true = pos <= size
  #=  TODO: Needed? arrayUpdate checks bounds and we should more reasonably check n?
  =#
  return arrayUpdate(arr, pos, NONE())
end

""" #= Retrieve the n:th Value from ValueArray, index from 1..n. =#"""
function getValueArray(valueArray::ValueArray, pos::Integer)::Tuple{Key, Value}
  local value::Value
  local key::Key

  local arr::Array{Option{HashEntry}}
  local n::Integer

  @assign (n, _, arr) = valueArray
  @match true = pos <= n
  #=  In case the user sends in higher values and we did not clear the array properly?
  =#
  @match SOME((key, value)) = arrayGet(arr, pos)
  return (key, value)
end

""" #= Checks if the given index exists in the value array =#"""
function valueArrayKeyIndexExists(valueArray::ValueArray, pos::Integer)::Bool
  local b::Bool

  @assign b = begin
    local k::Key
    local v::Value
    local n::Integer
    local arr::Array{Option{HashEntry}}
    @match (valueArray, pos) begin
      (_, -1) => begin
        false
      end

      ((n, _, arr), _) => begin
        if pos <= n
          isSome(arr[pos])
        else
          false
        end
      end
    end
  end
  return b
end

""" #= Makes a copy of a hashtable. =#"""
function copy(inHashTable::HashTable)::HashTable
  local outCopy::HashTable

  local hv::HashVector
  local bs::Integer
  local sz::Integer
  local vs::Integer
  local ve::Integer
  local ft::FuncsTuple
  local vae::Array{Option{HashEntry}}

  @assign (hv, (vs, ve, vae), bs, ft) = inHashTable
  @assign hv = arrayCopy(hv)
  @assign vae = arrayCopy(vae)
  @assign outCopy = (hv, (vs, ve, vae), bs, ft)
  return outCopy
end

""" #= Clears the hashtable. =#"""
function clear(ht::HashTable)::HashTable

  local hv::HashVector
  local bs::Integer
  local sz::Integer
  local vs::Integer
  local ve::Integer
  local hash_idx::Integer
  local ft::FuncsTuple
  local hashFunc::FuncHash
  local key::Key
  local vae::Array{Option{HashEntry}}

  @match (hv, (vs, ve, vae), bs, (@match (hashFunc, _, _, _) = ft)) = ht
  for i = 1:vs
    @assign _ = begin
      @match arrayGet(vae, i) begin
        SOME((key, _)) => begin
          @assign hash_idx = hashFunc(key, bs) + 1
          arrayUpdate(hv, hash_idx, nil)
          arrayUpdate(vae, i, NONE())
          ()
        end

        _ => begin
          ()
        end
      end
    end
  end
  @assign ht = (hv, (0, ve, vae), bs, ft)
  return ht
end

""" #= Clears a HashTable that has not been properly stored, but was known to never delete an element (making the values sequential SOME() for as long as there are elements). NOTE: Does not handle arrays that were expanded? =#"""
function clearAssumeNoDelete(ht::HashTable)
  local hv::HashVector
  local bs::Integer
  local sz::Integer
  local vs::Integer
  local ve::Integer
  local hash_idx::Integer
  local ft::FuncsTuple
  local hashFunc::FuncHash
  local key::Key
  local vae::Array{Option{HashEntry}}
  local workaroundForBug::Bool = true #= TODO: Make it impossible to update a value by not updating n (fully mutable HT instead of this hybrid) =#
  local debug::Bool = false

  @match (hv, (vs, ve, vae), bs, (@match (hashFunc, _, _, _) = ft)) = ht
  for i = 1:ve
    @assign _ = begin
      @match arrayGet(vae, i) begin
        SOME((key, _)) => begin
          if !workaroundForBug
            @assign hash_idx = hashFunc(key, bs) + 1
            arrayUpdate(hv, hash_idx, nil)
          end
          arrayUpdate(vae, i, NONE())
          ()
        end

        _ => begin
          if !workaroundForBug
            return
          end
          ()
        end
      end
    end
  end
  if debug
    for i in vae
      if isSome(i)
        print("vae not empty\\n")
        break
      end
    end
  end
  return if workaroundForBug
    for i = 1:arrayLength(hv)
      if !listEmpty(arrayGet(hv, i))
        if debug
          print("hv not empty\\n")
        end
        arrayUpdate(hv, i, nil)
      end
    end
  end
end

@exportAll()
end
