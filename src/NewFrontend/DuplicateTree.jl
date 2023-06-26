module DuplicateTree
using MetaModelica

@UniontypeDecl Entry
const Key = String
const Value = Entry

#= Modelica extend clause =#
include("../Util/baseAvlTreeCode.jl")

#= Use proper string compare =#
keyCompare = (inKey1::Key, inKey2::Key) -> begin
  res = stringCompare(inKey1, inKey2)
  return res
end

import ..LookupTree
import ..InstNode
struct EntryTypeStruct
  DUPLICATE::Int
  REDECLARE::Int
  ENTRY::Int
end

const EntryType = EntryTypeStruct(#=DUPLICATE=# 1,  #=REDCLARE=# 2 , #= ENTRY =# 3)
const EntryTypeTy = Int

@Uniontype Entry begin
  @Record DUPLICATE_TREE_ENTRY begin
    entry::LookupTree.Entry
    node::Option{InstNode}
    children::List{Entry}
    ty::EntryTypeTy
  end
end

function newRedeclare(entry::LookupTree.Entry)::Entry
  local redecl::Entry = DUPLICATE_TREE_ENTRY(entry, NONE(), nil, EntryType.REDECLARE)
  return redecl
end

function newDuplicate(kept::LookupTree.Entry, duplicate::LookupTree.Entry)::Entry
  local entry::Entry = DUPLICATE_TREE_ENTRY(kept, NONE(), list(newEntry(duplicate)), EntryType.DUPLICATE)
  return entry
end

function newEntry(lentry::LookupTree.Entry)::Entry
  local entry::Entry = DUPLICATE_TREE_ENTRY(lentry, NONE(), nil, EntryType.ENTRY)
  return entry
end

function idExistsInEntry(id::LookupTree.Entry, entry::Entry)::Bool
  local exists::Bool
    exists =
    LookupTree.isEqual(id, entry.entry) ||
    ListUtil.exist(entry.children, (id) -> idExistsInEntry(id = id))
  return exists
end

end
