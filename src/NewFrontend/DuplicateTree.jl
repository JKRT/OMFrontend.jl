module DuplicateTree
using MetaModelica

import ..LookupTree
import ..InstNode

struct DUPLICATE_TREE_ENTRY{T0 <: LookupTree.Entry}
  entry::T0
  node::Option
  children::Vector{DUPLICATE_TREE_ENTRY}
  ty::Int
end

#= Singelton uniontype =#
const Entry = DUPLICATE_TREE_ENTRY
const Key = String
const Value = Entry

#= Modelica extend clause =#
include("../Util/baseAvlTreeCode.jl")

#= Use proper string compare =#
keyCompare = (inKey1::Key, inKey2::Key) -> begin
  res = stringCompare(inKey1, inKey2)
  return res
end


struct EntryTypeStruct{T0 <: Integer}
  DUPLICATE::T0
  REDECLARE::T0
  ENTRY::T0
end

const EntryType = EntryTypeStruct(#=DUPLICATE=# 1,  #=REDCLARE=# 2 , #= ENTRY =# 3)
const EntryTypeTy = Int

function newRedeclare(entry::LookupTree.Entry)
  DUPLICATE_TREE_ENTRY(entry, NONE(), Entry[], EntryType.REDECLARE)
end

function newDuplicate(kept::LookupTree.Entry, duplicate::LookupTree.Entry)
  DUPLICATE_TREE_ENTRY(kept, NONE(), Entry[newEntry(duplicate)], EntryType.DUPLICATE)
end

function newEntry(entry::LookupTree.Entry)
  DUPLICATE_TREE_ENTRY(entry, NONE(), Entry[], EntryType.ENTRY)
end

function idExistsInEntry(id::LookupTree.Entry, entry::Entry)
  local exists::Bool
    exists =
    LookupTree.isEqual(id, entry.entry) || ArrayUtil.exist(entry.children, (id) -> idExistsInEntry(id = id))
  return exists
end

end
