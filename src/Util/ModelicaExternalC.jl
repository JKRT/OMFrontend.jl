module ModelicaExternalC

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

function Streams_print(string::String, fileName::String)
  return @error "TODO: Defined in the runtime"
end

function Streams_readLine(fileName::String, lineNumber::Integer)::Tuple{String, Bool}
  local endOfFile::Bool
  local string::String

  @error "TODO: Defined in the runtime"
  return (string, endOfFile)
end

function Streams_countLines(fileName::String)::Integer
  local numberOfLines::Integer

  @error "TODO: Defined in the runtime"
  return numberOfLines
end

function File_fullPathName(fileName::String)::String
  local outName::String

  @error "TODO: Defined in the runtime"
  return outName
end

function File_stat(name::String)::Integer
  local fileType::Integer

  @error "TODO: Defined in the runtime"
  return fileType
end

function Streams_close(fileName::String)
  return @error "TODO: Defined in the runtime"
end

function Strings_compare(string1::String, string2::String, caseSensitive::Bool)::Integer
  local result::Integer

  @error "TODO: Defined in the runtime"
  return result
end

function Strings_scanReal(
  string::String,
  startIndex::Integer,
  unsigned::Bool,
)::Tuple{Integer, AbstractFloat}
  local number::AbstractFloat
  local nextIndex::Integer

  @error "TODO: Defined in the runtime"
  return (nextIndex, number)
end

function Strings_scanInteger(
  string::String,
  startIndex::Integer,
  unsigned::Bool,
)::Tuple{Integer, Integer}
  local number::Integer
  local nextIndex::Integer

  @error "TODO: Defined in the runtime"
  return (nextIndex, number)
end

function Strings_scanString(string::String, startIndex::Integer)::Tuple{Integer, String}
  local string2::String
  local nextIndex::Integer

  @error "TODO: Defined in the runtime"
  return (nextIndex, string2)
end

function Strings_scanIdentifier(string::String, startIndex::Integer)::Tuple{Integer, String}
  local identifier::String
  local nextIndex::Integer

  @error "TODO: Defined in the runtime"
  return (nextIndex, identifier)
end

function Strings_skipWhiteSpace(string::String, startIndex::Integer(min = 1) = 1)::Integer
  local nextIndex::Integer

  @error "TODO: Defined in the runtime"
  return nextIndex
end

function Strings_hashString(string::String)::Integer
  local hash::Integer

  @error "TODO: Defined in the runtime"
  return hash
end

function ModelicaIO_readMatrixSizes(fileName::String, matrixName::String)::Integer
  local dim::Integer

  @error "TODO: Defined in the runtime"
  return dim
end

function ModelicaIO_readRealMatrix(
  fileName::String,
  matrixName::String,
  nrow::Integer,
  ncol::Integer,
  verboseRead::Bool = true,
)::AbstractFloat
  local matrix::AbstractFloat

  @error "TODO: Defined in the runtime"
  return matrix
end

@exportAll()
end
