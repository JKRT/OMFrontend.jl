module P_Pointer

const Pointer = Ref
using MetaModelica
using ExportAll

function access(MutableType::Pointer)
  return MutableType.x
end

function update(MutableType::Pointer{T}, data) where T
  MutableType.x = data
  return MutableType
end

function createImmutable(data::T) where T
  local st = supertype(T)
  return Pointer{st}(data)
end

function create(data::T)::Pointer where T
  local st = supertype(T)
  local ptr = Pointer{st}(data)
  return ptr
end

@exportAll()
end
