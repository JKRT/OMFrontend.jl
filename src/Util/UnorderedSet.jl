module P_UnorderedSet

using MetaModelica
using ExportAll

#= Hash set with user-provided hash and equality functions.
   Follows the OpenModelica UnorderedSet API.
   Internally wraps a Dict{Any,Bool} for simplicity. =#

mutable struct UnorderedSet
  data::Dict{Any,Bool}
  hashFn::Base.Function
  eqFn::Base.Function
end

function new(hashFn::Base.Function, eqFn::Base.Function)
  return UnorderedSet(Dict{Any,Bool}(), hashFn, eqFn)
end

function new(hashFn::Base.Function, eqFn::Base.Function, _bucketCount::Integer)
  return UnorderedSet(Dict{Any,Bool}(), hashFn, eqFn)
end

function fromList(lst, hashFn::Base.Function, eqFn::Base.Function)
  s = new(hashFn, eqFn)
  for e in lst
    s.data[e] = true
  end
  return s
end

function add(elem, s::UnorderedSet)
  s.data[elem] = true
end

function addNew(elem, s::UnorderedSet)
  s.data[elem] = true
end

function addUnique(elem, s::UnorderedSet)
  s.data[elem] = true
end

function remove(elem, s::UnorderedSet)::Bool
  if haskey(s.data, elem)
    delete!(s.data, elem)
    return true
  end
  return false
end

function get(elem, s::UnorderedSet)
  if haskey(s.data, elem)
    return SOME(elem)
  end
  return nothing
end

function getOrFail(elem, s::UnorderedSet)
  if haskey(s.data, elem)
    return elem
  end
  error("UnorderedSet.getOrFail: element not found")
end

contains(elem, s::UnorderedSet) = haskey(s.data, elem)

function first(s::UnorderedSet)
  return Base.first(Base.keys(s.data))
end

function size(s::UnorderedSet)::Integer
  return length(s.data)
end

function isEmpty(s::UnorderedSet)::Bool
  return Base.isempty(s.data)
end

function toList(s::UnorderedSet)
  return list(k for k in Base.keys(s.data))
end

function toArray(s::UnorderedSet)
  return collect(Base.keys(s.data))
end

function copy(s::UnorderedSet)
  return UnorderedSet(Base.copy(s.data), s.hashFn, s.eqFn)
end

function fold(s::UnorderedSet, func, startValue)
  result = startValue
  for k in Base.keys(s.data)
    result = func(k, result)
  end
  return result
end

function apply(s::UnorderedSet, func)
  newData = Dict{Any,Bool}()
  for k in Base.keys(s.data)
    newData[func(k)] = true
  end
  s.data = newData
end

function all(s::UnorderedSet, func)::Bool
  for k in Base.keys(s.data)
    if !func(k)
      return false
    end
  end
  return true
end

function any(s::UnorderedSet, func)::Bool
  for k in Base.keys(s.data)
    if func(k)
      return true
    end
  end
  return false
end

function none(s::UnorderedSet, func)::Bool
  return !any(s, func)
end

function merge(s1::UnorderedSet, s2::UnorderedSet)
  for k in Base.keys(s2.data)
    s1.data[k] = true
  end
end

function clear(s::UnorderedSet)
  empty!(s.data)
end

function toString(s::UnorderedSet, fn, delimiter::String = "\n")
  entries = String[fn(k) for k in Base.keys(s.data)]
  return join(entries, delimiter)
end

@exportAll()
end
