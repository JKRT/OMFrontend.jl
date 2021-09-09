module IOStreamExt

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

function createFile(fileName::String)::Integer
  local fileID::Integer

  @error "TODO: Defined in the runtime"
  return fileID
end

function closeFile(fileID::Integer)
  return @error "TODO: Defined in the runtime"
end

function deleteFile(fileID::Integer)
  return @error "TODO: Defined in the runtime"
end

function clearFile(fileID::Integer)
  return @error "TODO: Defined in the runtime"
end

function appendFile(fileID::Integer, inString::String)
  return @error "TODO: Defined in the runtime"
end

function readFile(fileID::Integer)::String
  local outString::String

  @error "TODO: Defined in the runtime"
  return outString
end

function printFile(fileID::Integer, whereToPrint::Integer) #= stdout:1, stderr:2 =#
  return @error "TODO: Defined in the runtime"
end

function createBuffer()::Integer
  local bufferID::Integer

  @error "TODO: Defined in the runtime"
  return bufferID
end

function appendBuffer(bufferID::Integer, inString::String)
  return @error "TODO: Defined in the runtime"
end

function deleteBuffer(bufferID::Integer)
  return @error "TODO: Defined in the runtime"
end

function clearBuffer(bufferID::Integer)
  return @error "TODO: Defined in the runtime"
end

function readBuffer(bufferID::Integer)::String
  local outString::String

  @error "TODO: Defined in the runtime"
  return outString
end

function printBuffer(bufferID::Integer, whereToPrint::Integer) #= stdout:1, stderr:2 =#
  return @error "TODO: Defined in the runtime"
end

"""
New implementation
@author johti17
"""
function appendReversedList(inStringLst::List{<:String})::String
  local outString::String = ""
  for str in listReverse(inStringLst)
    outString *= str
  end
  return outString
end

function printReversedList(inStringLst::List{<:String}, whereToPrint::Integer) #= stdout:1, stderr:2 =#
  return @error "TODO: Defined in the runtime"
end

@exportAll()
end
