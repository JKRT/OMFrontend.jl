module BaseHashSet

using MetaModelica
using ExportAll
#= Forward declarations for uniontypes until Julia adds support for mutual recursion =#

FuncHash = Function
FuncEq = Function
FuncKeyString = Function

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
#=  Below is the instance specific code. For each hashset the user must define:
=#
#=  Key      - The key used to uniquely define elements in a hashset
=#
#=  hashFunc - A function that maps a key to a positive integer.
=#
#=  keyEqual - A comparison function between two keys, returns true if equal.
=#

import ArrayUtil
#=  Generic hashset code below
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
#=  You can also use Util.nextPrime if you know exactly how large the hash set
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
HashSet = Tuple
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

function emptyHashSetWork(szBucket::Integer, fntpl::FuncsTuple)::HashSet
  local hashSet::HashSet

  local arr::Array{List{Tuple{Key, Integer}}}
  local lst::List{Option{Key}}
  local emptyarr::Array{Option{Key}}

  local szArr::Integer

  @assign arr = arrayCreate(szBucket, nil)
  @assign szArr = bucketToValuesSize(szBucket)
  @assign emptyarr = arrayCreate(szArr, NONE())
  @assign hashSet = (arr, (0, szArr, emptyarr), szBucket, 0, fntpl)
  return hashSet
end

""" #= 
  Add a Key to hashset.
  If the Key already exists, nothing happen.
 =#"""
function add(entry::Key, hashSet::HashSet)::HashSet
  local outHashSet::HashSet

  @assign outHashSet = begin
    local hval::Integer
    local indx::Integer
    local newpos::Integer
    local n::Integer
    local bsize::Integer
    local varr::Tuple{Integer, Integer, Array{Option{Key}}}
    local indexes::List{Tuple{Key, Integer}}
    local hashvec::Array{List{Tuple{Key, Integer}}}
    local key::Key
    local fkey::Option{Key}
    local fntpl::FuncsTuple
    local hashFunc::FuncHash
    local keystrFunc::FuncKeyString
    local s::String
    #=  Adding when not existing previously
    =#
    @match (entry, hashSet) begin
      (key, (hashvec, varr, bsize, n, fntpl && (hashFunc, _, _))) => begin
        @assign (fkey, indx) = get1(key, hashSet)
        if isSome(fkey)
          @assign varr = valueArraySetnth(varr, indx, key)
        else
          @assign indx = hashFunc(key, bsize)
          @assign newpos = valueArrayLength(varr)
          @assign varr = valueArrayAdd(varr, key)
          @assign indexes = hashvec[indx + 1]
          @assign hashvec = arrayUpdate(hashvec, indx + 1, _cons((key, newpos), indexes))
          @assign n = valueArrayLength(varr)
        end
        (hashvec, varr, bsize, n, fntpl)
      end

      (key, (_, _, bsize, _, (hashFunc, _, keystrFunc))) => begin
        print("- BaseHashSet.add failed: ")
        print("bsize: ")
        print(intString(bsize))
        print(" key: ")
        @assign s = keystrFunc(key)
        print(s + " Hash: ")
        @assign hval = hashFunc(key, bsize)
        print(intString(hval))
        print("\\n")
        fail()
      end
    end
  end
  return outHashSet
end

""" #= Add a Key to hashset, without checking if it already exists.
   This function is thus more efficient than add if you already know that the
   Key doesn't already exist in the hashset. =#"""
function addNoUpdCheck(entry::Key, hashSet::HashSet)::HashSet
  local outHashSet::HashSet

  @assign outHashSet = begin
    local hval::Integer
    local indx::Integer
    local newpos::Integer
    local n::Integer
    local n_1::Integer
    local bsize::Integer
    local indx_1::Integer
    local varr_1::Tuple{Integer, Integer, Array{Option{Key}}}
    local varr::Tuple{Integer, Integer, Array{Option{Key}}}
    local indexes::List{Tuple{Key, Integer}}
    local hashvec_1::Array{List{Tuple{Key, Integer}}}
    local hashvec::Array{List{Tuple{Key, Integer}}}
    local name_str::String
    local key::Key
    local fntpl::FuncsTuple
    local hashFunc::FuncHash
    #=  Adding when not existing previously
    =#
    @matchcontinue (entry, hashSet) begin
      (key, (hashvec, varr, bsize, _, fntpl && (hashFunc, _, _))) => begin
        @assign indx = hashFunc(key, bsize)
        @assign newpos = valueArrayLength(varr)
        @assign varr_1 = valueArrayAdd(varr, key)
        @assign indexes = hashvec[indx + 1]
        @assign hashvec_1 = arrayUpdate(hashvec, indx + 1, _cons((key, newpos), indexes))
        @assign n_1 = valueArrayLength(varr_1)
        (hashvec_1, varr_1, bsize, n_1, fntpl)
      end

      _ => begin
        print("- BaseHashSet.addNoUpdCheck failed\\n")
        fail()
      end
    end
  end
  return outHashSet
end

""" #= Add a Key to hashset. If the Key is already used it fails. =#"""
function addUnique(key::Key, hashSet::HashSet)::HashSet
  local outHashSet::HashSet

  @assign outHashSet = begin
    local hval::Integer
    local indx::Integer
    local newpos::Integer
    local n::Integer
    local n_1::Integer
    local bsize::Integer
    local indx_1::Integer
    local varr_1::Tuple{Integer, Integer, Array{Option{Key}}}
    local varr::Tuple{Integer, Integer, Array{Option{Key}}}
    local indexes::List{Tuple{Key, Integer}}
    local hashvec_1::Array{List{Tuple{Key, Integer}}}
    local hashvec::Array{List{Tuple{Key, Integer}}}
    local fntpl::FuncsTuple
    local hashFunc::FuncHash
    #=  Adding when not existing previously
    =#
    @match (key, hashSet) begin
      (
        _,
        (hashvec, varr, bsize, _, fntpl && (hashFunc, _, _)),
      ) where {(!has(key, hashSet))} => begin
        @assign indx = hashFunc(key, bsize)
        @assign newpos = valueArrayLength(varr)
        @assign varr_1 = valueArrayAdd(varr, key)
        @assign indexes = hashvec[indx + 1]
        @assign hashvec_1 = arrayUpdate(hashvec, indx + 1, _cons((key, newpos), indexes))
        @assign n_1 = valueArrayLength(varr_1)
        (hashvec_1, varr_1, bsize, n_1, fntpl)
      end
    end
  end
  return outHashSet
end

""" #= 
  delete the Key from the HashSet.
  Note: This function does not delete from the index table, only from the tuple<Integer,Integer,array<Option<Key>>>.
  This means that a lot of deletions will not make the HashSet more compact, it will still contain
  a lot of incices information.
 =#"""
function delete(key::Key, hashSet::HashSet)::HashSet
  local outHashSet::HashSet

  @assign outHashSet = begin
    local indx::Integer
    local n::Integer
    local bsize::Integer
    local indx_1::Integer
    local varr_1::Tuple{Integer, Integer, Array{Option{Key}}}
    local varr::Tuple{Integer, Integer, Array{Option{Key}}}
    local hashvec::Array{List{Tuple{Key, Integer}}}
    local fntpl::FuncsTuple
    #= /* adding when already present => Updating value */ =#
    @matchcontinue (key, hashSet) begin
      (_, (hashvec, varr, bsize, n, fntpl)) => begin
        @match (SOME(_), indx) = get1(key, hashSet)
        @assign varr_1 = valueArrayClearnth(varr, indx)
        (hashvec, varr_1, bsize, n, fntpl)
      end

      _ => begin
        print("-HashSet.delete failed\\n")
        fail()
      end
    end
  end
  return outHashSet
end

""" #= Returns true if Key is in the HashSet. =#"""
function has(key::Key, hashSet::HashSet)::Bool
  local b::Bool

  @assign b = begin
    local oKey::Option{Key}
    #=  empty set containg nothing
    =#
    @match (key, hashSet) begin
      (_, (_, (0, _, _), _, _, _)) => begin
        false
      end

      _ => begin
        @assign (oKey, _) = get1(key, hashSet)
        isSome(oKey)
      end
    end
  end
  return b
end

""" #= Returns true if all keys are in the HashSet. =#"""
function hasAll(keys::List{<:Key}, hashSet::HashSet)::Bool
  local b::Bool = true

  for key in keys
    @assign b = has(key, hashSet)
    if !b
      return b
    end
  end
  return b
end

""" #= Returns Key from the HashSet. Returns NONE() if not present =#"""
function get(key::Key, hashSet::HashSet)::Option{Key}
  local okey::Option{Key}

  @assign (okey, _) = get1(key, hashSet)
  return okey
end

""" #= help function to get =#"""
function get1(key::Key, hashSet::HashSet)::Tuple{Option{Key}, Integer}
  local indx::Integer
  local okey::Option{Key}

  @assign (okey, indx) = begin
    local hashindx::Integer
    local bsize::Integer
    local n::Integer
    local indexes::List{Tuple{Key, Integer}}
    local hashvec::Array{List{Tuple{Key, Integer}}}
    local varr::ValueArray
    local k::Option{Key}
    local keyEqual::FuncEq
    local hashFunc::FuncHash
    local b::Bool
    @match (key, hashSet) begin
      (_, (hashvec, varr, bsize, _, (hashFunc, keyEqual, _))) => begin
        @assign hashindx = hashFunc(key, bsize)
        @assign indexes = hashvec[hashindx + 1]
        @assign (indx, b) = get2(key, indexes, keyEqual)
        @assign k = if b
          valueArrayNthT(varr, indx)
        else
          NONE()
        end
        (k, indx)
      end
    end
  end
  return (okey, indx)
end

""" #= Helper function to get =#"""
function get2(
  key::Key,
  keyIndices::List{<:Tuple{<:Key, Integer}},
  keyEqual::FuncEq,
)::Tuple{Integer, Bool}
  local found::Bool = true
  local index::Integer

  local key2::Key

  for t in keyIndices
    @assign (key2, index) = t
    if keyEqual(key, key2)
      return (index, found)
    end
  end
  @assign found = false
  return (index, found)
end

""" #=  =#"""
function printHashSet(hashSet::HashSet)
  local printKey::FuncKeyString

  @assign (_, _, _, _, (_, _, printKey)) = hashSet
  return print(stringDelimitList(list(printKey(e) for e in hashSetList(hashSet)), "\\n"))
end

""" #=  =#"""
function dumpHashSet(hashSet::HashSet)
  print("HashSet:\\n")
  printHashSet(hashSet)
  return print("\\n")
end

""" #= returns the entries in the hashSet as a list of Key =#"""
function hashSetList(hashSet::HashSet)::List{Key}
  local lst::List{Key}

  @assign lst = begin
    local varr::ValueArray
    @match hashSet begin
      (_, varr, _, _, _) => begin
        valueArrayList(varr)
      end
    end
  end
  return lst
end

""" #= Transforms a ValueArray to a Key list =#"""
function valueArrayList(inValueArray::ValueArray)::List{Key}
  local outList::List{Key} = nil

  local arr::Array{Option{Key}}
  local size::Integer
  local e::Key

  @assign (size, _, arr) = inValueArray
  for i = 1:size
    if isSome(arr[i])
      @match SOME(e) = arr[i]
      @assign outList = _cons(e, outList)
    end
  end
  @assign outList = listReverse(outList)
  return outList
end

""" #= Returns the number of elements inserted into the table =#"""
function currentSize(hashSet::HashSet)::Integer
  local sz::Integer

  local va::ValueArray

  @assign (_, va, _, _, _) = hashSet
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
function valueArrayAdd(valueArray::ValueArray, entry::Key)::ValueArray
  local outValueArray::ValueArray

  @assign outValueArray = begin
    local n_1::Integer
    local n::Integer
    local size::Integer
    local expandsize::Integer
    local expandsize_1::Integer
    local newsize::Integer
    local arr_1::Array{Option{Key}}
    local arr::Array{Option{Key}}
    local arr_2::Array{Option{Key}}
    local rsize::AbstractFloat
    local rexpandsize::AbstractFloat
    @matchcontinue (valueArray, entry) begin
      ((n, size, arr), _) => begin
        if !n < size
          fail() #= Have space to add array elt. =#
        end #= Have space to add array elt. =#
        @assign n_1 = n + 1
        @assign arr_1 = arrayUpdate(arr, n + 1, SOME(entry))
        (n_1, size, arr_1)
      end

      ((n, size, arr), _) => begin
        if n < size
          fail() #= Do NOT have space to add array elt. Expand with factor 1.4 =#
        end #= Do NOT have space to add array elt. Expand with factor 1.4 =#
        @assign rsize = intReal(size)
        @assign rexpandsize = rsize * 0.4
        @assign expandsize = realInt(rexpandsize)
        @assign expandsize_1 = intMax(expandsize, 1)
        @assign newsize = expandsize_1 + size
        @assign arr_1 = ArrayUtil.expand(expandsize_1, arr, NONE())
        @assign n_1 = n + 1
        @assign arr_2 = arrayUpdate(arr_1, n + 1, SOME(entry))
        (n_1, newsize, arr_2)
      end

      _ => begin
        print("-HashSet.valueArrayAdd failed\\n")
        fail()
      end
    end
  end
  return outValueArray
end

""" #= Set the n:th variable in the ValueArray to value. =#"""
function valueArraySetnth(valueArray::ValueArray, pos::Integer, entry::Key)::ValueArray
  local outValueArray::ValueArray

  @assign outValueArray = begin
    local arr_1::Array{Option{Key}}
    local arr::Array{Option{Key}}
    local n::Integer
    local size::Integer
    @matchcontinue (valueArray, pos, entry) begin
      ((n, size, arr), _, _) => begin
        if !pos < size
          fail()
        end
        @assign arr_1 = arrayUpdate(arr, pos + 1, SOME(entry))
        (n, size, arr_1)
      end

      _ => begin
        print("-HashSet.valueArraySetnth failed\\n")
        fail()
      end
    end
  end
  return outValueArray
end

""" #= Clears the n:th variable in the ValueArray (set to NONE()). =#"""
function valueArrayClearnth(valueArray::ValueArray, pos::Integer)::ValueArray
  local outValueArray::ValueArray

  @assign outValueArray = begin
    local arr_1::Array{Option{Key}}
    local arr::Array{Option{Key}}
    local n::Integer
    local size::Integer
    @matchcontinue (valueArray, pos) begin
      ((n, size, arr), _) => begin
        if !pos < size
          fail()
        end
        @assign arr_1 = arrayUpdate(arr, pos + 1, NONE())
        (n, size, arr_1)
      end

      _ => begin
        print("-HashSet.valueArrayClearnth failed\\n")
        fail()
      end
    end
  end
  return outValueArray
end

""" #= Retrieve the n:th Value from ValueArray, index from 0..n-1. =#"""
function valueArrayNth(valueArray::ValueArray, pos::Integer)::Key
  local key::Key

  @assign key = begin
    local k::Key
    local n::Integer
    local arr::Array{Option{Key}}
    @match (valueArray, pos) begin
      ((n, _, arr), _) => begin
        if !pos <= n
          fail()
        end
        @match SOME(k) = arr[pos + 1]
        k
      end
    end
  end
  #=  should be pos<n
  =#
  return key
end

""" #= Retrieve the n:th Value from ValueArray, index from 0..n-1. =#"""
function valueArrayNthT(valueArray::ValueArray, pos::Integer)::Option{Key}
  local key::Option{Key}

  @assign key = begin
    local k::Key
    local n::Integer
    local arr::Array{Option{Key}}
    @match (valueArray, pos) begin
      ((n, _, arr), _) => begin
        if !pos <= n
          fail()
        end
        arr[pos + 1]
      end
    end
  end
  #=  should be pos<n
  =#
  return key
end

@exportAll()
end
