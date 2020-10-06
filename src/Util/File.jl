module File

using MetaModelica
using ExportAll

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

module FileHandler

using ExternalObject #= Modelica extend clause =#

""" #= File constructor. =#"""
function constructor(fromID::Option{Integer} = noReference()) where {T} #= Never pass this an actual Option<Integer>. Only use File.getReference(file) or File.noReference(). Determines if we should restore from another File object or create a new File. =#
  local file::FileHandler

  ## REENABLE @debug "TODO: Defined in the runtime"
  return file
end

function destructor(file::FileHandler)
  return ## REENABLE @debug "TODO: Defined in the runtime"
end
end

Mode = (() -> begin #= Enumeration =#
  Read = 1
  Write = 2
  () -> (Read; Write)
end)()

function open(file::FileHandler, filename::String, mode::Mode = Mode.Read)
  return ## REENABLE @debug "TODO: Defined in the runtime"
end

function write(file::FileHandler, data::String)
  return ## REENABLE @debug "TODO: Defined in the runtime"
end

function writeInt(file::FileHandler, data::Integer, format::String = "%d")
  return ## REENABLE @debug "TODO: Defined in the runtime"
end

function writeReal(file::FileHandler, data::AbstractFloat, format::String = "%.15g")
  return ## REENABLE @debug "TODO: Defined in the runtime"
end

Escape = (() -> begin #= Enumeration =#
  None = 1  #= No escape string =#
  C = 2  #= Escapes C strings (minimally): \\\\n and \\\" =#
  JSON = 3  #= Escapes JSON strings (quotes and control characters) =#
  XML = 4  #= Escapes strings to XML text =#
  () -> (None; C; JSON; XML)  #= Escapes strings to XML text =#
end)()

function writeEscape(file::FileHandler, data::String, escape::Escape)
  return ## REENABLE @debug "TODO: Defined in the runtime"
end

Whence = (() -> begin #= Enumeration =#
  Set = 1  #= SEEK_SET 0=start of file =#
  Current = 2  #= SEEK_CUR 0=current byte =#
  End = 3  #= SEEK_END 0=end of file =#
  () -> (Set; Current; End)  #= SEEK_END 0=end of file =#
end)()

function seek(file::FileHandler, offset::Integer, whence::Whence = Whence.Set)::Bool
  local success::Bool

  ## REENABLE @debug "TODO: Defined in the runtime"
  return success
end

function tell(file::FileHandler)::Integer
  local pos::Integer

  ## REENABLE @debug "TODO: Defined in the runtime"
  return pos
end

function getFilename(file::Option{<:Integer})::String
  local fileName::String

  ## REENABLE @debug "TODO: Defined in the runtime"
  return fileName
end

""" #= Returns NULL (an opaque pointer; not actually Option<Integer>) =#"""
function noReference()::Option{Integer}
  local reference::Option{Integer}

  ## REENABLE @debug "TODO: Defined in the runtime"
  return reference
end

""" #= Returns an opaque pointer (not actually Option<Integer>) =#"""
function getReference(file::FileHandler)::Option{Integer}
  local reference::Option{Integer}

  ## REENABLE @debug "TODO: Defined in the runtime"
  return reference
end

function releaseReference(file::FileHandler)
  return ## REENABLE @debug "TODO: Defined in the runtime"
end

function writeSpace(file::FileHandler, n::Integer)
  return for i = 1:n
    FileHandler.write(file, " ")
  end
end

module Examples

using MetaModelica
using ExportAll

module WriteToFile #= Should be a model here. Not widley used but apparently occurs =#

const file = FileHandler()::FileHandler

open(file, "abc.txt", Mode.Write)
write(file, "def.fafaf\\n")
writeEscape(file, "xx<def.\\\\nfaf>af\\n", escape = Escape.JSON)
end

@exportAll()
end

@exportAll()
end
