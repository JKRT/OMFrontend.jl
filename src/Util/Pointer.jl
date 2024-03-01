module P_Pointer

const Pointer = Ref
using MetaModelica
using ExportAll

function access(MutableType::Pointer)
  local res = MutableType.x
  return res
end

function update(MutableType::Pointer{T}, data) where T
  MutableType.x = data
  return MutableType
end

function createImmutable(data::T) where {T}
  local st = supertype(T)
  return Pointer{Union{T, st}}(data)
end

function create(data::T) where{T}
  local st = supertype(T)
  local ptr = Pointer{st}(data)
  return ptr
end

@exportAll()
end
