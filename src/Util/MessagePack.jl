module MessagePack

using MetaModelica
using ExportAll

#=  Do not modify this file: It was automatically generated from https:github.com/sjoelund/msgpack-modelica/
=#

module Pack

using MetaModelica
using ExportAll

module SimpleBuffer

using MetaModelica
using ExportAll

module SimpleBuffer

using ExternalObject #= Modelica extend clause =#

function constructor()::SimpleBuffer
  local buf::SimpleBuffer

  @error "TODO: Defined in the runtime"
  return buf
end

function destructor(buf::SimpleBuffer)
  return @error "TODO: Defined in the runtime"
end
end

function writeFile(sbuffer::SimpleBuffer, file::String)
  return @error "TODO: Defined in the runtime"
end

function position(sbuffer::SimpleBuffer)::Integer
  local position::Integer

  @error "TODO: Defined in the runtime"
  return position
end

@exportAll()
end

module Packer

using ExternalObject #= Modelica extend clause =#

function constructor(buf::SimpleBuffer.SimpleBuffer)::Packer
  local packer::Packer

  @error "TODO: Defined in the runtime"
  return packer
end

function destructor(packer::Packer)
  return @error "TODO: Defined in the runtime"
end
end

function double(packer::Packer, dbl::AbstractFloat)::Bool
  local result::Bool

  @error "TODO: Defined in the runtime"
  return result
end

function integer(packer::Packer, i::Integer)::Bool
  local result::Bool

  @error "TODO: Defined in the runtime"
  return result
end

function bool(packer::Packer, bool::Bool)::Bool
  local result::Bool

  function msgpack_pack_true(packer::Packer)::Bool
    local result::Bool

    @error "TODO: Defined in the runtime"
    return result
  end

  function msgpack_pack_false(packer::Packer)::Bool
    local result::Bool

    @error "TODO: Defined in the runtime"
    return result
  end

  @assign result = if bool
    msgpack_pack_true(packer)
  else
    msgpack_pack_false(packer)
  end
  return result
end

function sequence(packer::Packer, len::Integer)::Bool
  local result::Bool

  @error "TODO: Defined in the runtime"
  return result
end

function map(packer::Packer, len::Integer)::Bool
  local result::Bool

  @error "TODO: Defined in the runtime"
  return result
end

function string(packer::Packer, str::String)::Bool
  local result::Bool

  @error "TODO: Defined in the runtime"
  return result
end

function nil(packer::Packer)::Bool
  local result::Bool

  @error "TODO: Defined in the runtime"
  return result
end

@exportAll()
end

module Unpack

using MetaModelica
using ExportAll

module Deserializer

using ExternalObject #= Modelica extend clause =#

function constructor(file::String)::Deserializer
  local deserializer::Deserializer

  @error "TODO: Defined in the runtime"
  return deserializer
end

function destructor(deserializer::Deserializer)
  return @error "TODO: Defined in the runtime"
end
end

function next(deserializer::Deserializer, offset::Integer)::Tuple{Bool, Integer}
  local newoffset::Integer
  local success::Bool

  @error "TODO: Defined in the runtime"
  return (success, newoffset)
end

function toStream(
  deserializer::Deserializer,
  ss::Utilities.Stream.Stream,
  offset::Integer,
)::Tuple{Integer, Bool}
  local success::Bool
  local newoffset::Integer

  @error "TODO: Defined in the runtime"
  return (newoffset, success)
end

function integer(deserializer::Deserializer, offset::Integer)::Tuple{Integer, Integer, Bool}
  local success::Bool
  local newoffset::Integer
  local res::Integer

  @error "TODO: Defined in the runtime"
  return (res, newoffset, success)
end

function string(deserializer::Deserializer, offset::Integer)::Tuple{String, Integer, Bool}
  local success::Bool
  local newoffset::Integer
  local res::String

  @error "TODO: Defined in the runtime"
  return (res, newoffset, success)
end

function get_integer(deserializer::Deserializer)::Integer
  local res::Integer

  @error "TODO: Defined in the runtime"
  return res
end

@exportAll()
end

module Utilities

using MetaModelica
using ExportAll

module Stream

using MetaModelica
using ExportAll

module Stream

using ExternalObject #= Modelica extend clause =#

function constructor(file::String = "")::Stream #= Output file or \\\"\\\" for an in-memory string accessible using get() =#
  local ss::Stream

  @error "TODO: Defined in the runtime"
  return ss
end

function destructor(ss::Stream)
  return @error "TODO: Defined in the runtime"
end
end

""" #= Only works for in-memory streams =#"""
function get(ss::Stream)::String
  local str::String

  @error "TODO: Defined in the runtime"
  return str
end

function append(ss::Stream, str::String)
  return @error "TODO: Defined in the runtime"
end

@exportAll()
end

function deserializeFileToFile(
  inBinaryFile::String,
  outTextFile::String,
  separator::String = "\\n",
)
  local deserializer::Unpack.Deserializer = Unpack.Deserializer(inBinaryFile)
  local ss::Stream.Stream = Stream.Stream(outTextFile)
  local success::Bool = true
  local offset::Integer = 0

  return while success
    @assign (offset, success) = Unpack.toStream(deserializer, ss, offset)
    if success
      Stream.append(ss, separator)
    end
  end
end

@exportAll()
end

module UsersGuide

using MetaModelica
using ExportAll

module License

using MetaModelica
using ExportAll

@exportAll()
end

@exportAll()
end

@exportAll()
end
