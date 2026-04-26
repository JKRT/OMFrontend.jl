"""
  Experimental reimplementation
  of the lookup tree using the Julia dict.
"""
module JLookupTree

using ExportAll
using MetaModelica

import ..LookupTree
const Key = LookupTree.Key
const Value = LookupTree.Entry
const Tree = Dict{Key, Value}
const ConflictFunc = Function

function hasKey(t::Tree, k::Key)::Bool
  local hk = in(k, keys(t))
  return hk
end

function setValue!(t::Tree, k, v)
  if in(k, keys(t))
    @info "Conflict. Lets redo..."
    fail() #Conflict
  else
    t[k] = v
  end
end

function getIndex(t::Tree, i)
  return t[i]
end

const addConflictDefault = fail

function add(
  tree::Tree,
  inKey::Key,
  inValue::Value,
  conflictFunc::ConflictFunc = addConflictDefault,
  )::Tree
  setValue!(tree, k, v)
end

function addList(tree::Tree,
                 inValues::Cons,
                 conflictFunc::ConflictFunc = addConflictDefault)
  for (k,v) in inValues
    setValue!(tree, k, v)
  end
  return tree
end

function get(tree::Tree, key::Key)
  tree[key]
end

@exportAll
end
