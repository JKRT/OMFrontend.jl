module LookupTree
using MetaModelica
using ExportAll

@UniontypeDecl Entry

@Uniontype Entry begin
  @Record IMPORT begin
    index::Integer
  end
  @Record COMPONENT begin
    index::Integer
  end
  @Record CLASS begin
    index::Integer
  end
end

Key = String
Value = Entry

#= Modelica extend clause =#
include("../Util/baseAvlTreeCode.jl")
include("../Util/baseAvlSetCode.jl")

keyCompare = (inKey1::String, inKey2::String) -> begin
  res = stringCompare(inKey1, inKey2)
  return res
end

keyStr = (key) -> begin
  return key
end

function isImport(entry::Entry)::Bool
  local isImport::Bool
  @assign isImport = begin
    @match entry begin
      IMPORT(__) => begin
        true
      end
      _ => begin
        false
      end
    end
  end
  return isImport
end

function new()
  return EMPTY()
end

function isEqual(entry1::Entry, entry2::Entry)::Bool
  local isEqual::Bool = index(entry1) == index(entry2)
  return isEqual
end

function index(entry::Entry)::Integer
  local index::Integer
  @assign index = begin
    @match entry begin
      CLASS(__) => begin
        entry.index
      end
      COMPONENT(__) => begin
        entry.index
      end
      IMPORT(__) => begin
        entry.index
      end
    end
  end
  return index
end

@exportAll()
end
