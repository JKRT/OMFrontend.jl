module ErrorExt

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
* THIS OSMC LICENSE (OSMC-PL) VERSION 1.2.
* ANY USE, REPRODUCTION OR DISTRIBUTION OF THIS PROGRAM CONSTITUTES
* RECIPIENT'S ACCEPTANCE OF THE OSMC LICENSE OR THE GPL VERSION 3,
* ACCORDING TO RECIPIENTS CHOICE.
*
* The OpenModelica software and the Open Source Modelica
* Consortium (OSMC) License (OSMC-PL) are obtained
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
* See the full OSMC License conditions for more details.
*
*/ =#
import ..Error.ErrorTypes

function registerModelicaFormatError()
  return @warn "TODO: Defined in the runtime"
end

function addSourceMessage(
  id::ErrorTypes.ErrorID,
  msg_type::ErrorTypes.MessageType,
  msg_severity::ErrorTypes.Severity,
  sline::Integer,
  scol::Integer,
  eline::Integer,
  ecol::Integer,
  read_only::Bool,
  filename::String,
  msg::String,
  tokens::List{<:String},
)
  @warn "TODO: Defined in the runtime"
end

function printMessagesStr(warningsAsErrors::Bool = false)::String
  local outString::String = "Placeholder error string"
  @warn "TODO: printMessagesStr not defined in the runtime"
  return outString
end

function getNumMessages()
  local num = 0
  @warn "TODO: Defined in the runtime"
  return 0
end

function getNumErrorMessages()::Integer
  local num::Integer

  @warn "TODO: Defined in the runtime"
  return num
end

function getNumWarningMessages()::Integer
  local num::Integer

  @warn "TODO: Defined in the runtime"
  return num
end

""" #= Returns all error messages and pops them from the message queue. =#"""
function getMessages()::List{ErrorTypes.TotalMessage}
  local res::List{ErrorTypes.TotalMessage}

  @warn "TODO: Defined in the runtime"
  return res
end

""" #= Returns all error messages since the last checkpoint and pops them from the message queue. =#"""
function getCheckpointMessages()::List{ErrorTypes.TotalMessage}
  local res::List{ErrorTypes.TotalMessage}

  @warn "TODO: Defined in the runtime"
  return res
end

function clearMessages()
  return @warn "TODO: Defined in the runtime"
end

""" #= Used to rollback/delete checkpoints without considering the identifier. Used to reset the error messages after a stack overflow exception. =#"""
function getNumCheckpoints()::Integer
  local n::Integer

  @warn "TODO: Defined in the runtime"
  return n
end

""" #= Used to rollback/delete checkpoints without considering the identifier. Used to reset the error messages after a stack overflow exception. =#"""
function rollbackNumCheckpoints(n::Integer)
  return @warn "TODO: Defined in the runtime"
end

""" #= Used to rollback/delete checkpoints without considering the identifier. Used to reset the error messages after a stack overflow exception. =#"""
function deleteNumCheckpoints(n::Integer)
  return @warn "TODO: Defined in the runtime"
end

""" #= sets a checkpoint for the error messages, so error messages can be rolled back (i.e. deleted) up to this point
A unique identifier for this checkpoint must be provided. It is checked when doing rollback or deletion =#"""
function setCheckpoint(id::String) #= uniqe identifier for the checkpoint (up to the programmer to guarantee uniqueness) =#
  return @warn "TODO: setCheckpoint is not defined in the runtime"
end

""" #= deletes the checkpoint at the top of the stack without
removing the error messages issued since that checkpoint.
If the checkpoint id doesn't match, the application exits with -1.
 =#"""
function delCheckpoint(id::String) #= unique identifier =#
  return @warn "TODO: Defined in the runtime"
end

function printErrorsNoWarning()::String
  local outString::String

  @warn "TODO: Defined in the runtime"
  return outString
end

""" #= rolls back error messages until the latest checkpoint,
deleting all error messages added since that point in time. A unique identifier for the checkpoint must be provided
The application will exit with return code -1 if this identifier does not match. =#"""
function rollBack(id::String) #= unique identifier =#
  return @warn "TODO: Defined in the runtime"
end

""" #= rolls back error messages until the latest checkpoint,
returning all error messages added since that point in time. A unique identifier for the checkpoint must be provided
The application will exit with return code -1 if this identifier does not match. =#"""
function popCheckPoint(id::String)::List{Integer} #= unique identifier =#
  local handles::List{Integer} #= opaque pointers; you MUST pass them back or memory is leaked =#

  @warn "TODO: Defined in the runtime"
  return handles #= opaque pointers; you MUST pass them back or memory is leaked =#
end

""" #= Pushes stored pointers back to the error stack. =#"""
function pushMessages(handles::List{<:Integer}) #= opaque pointers from popCheckPoint =#
  return @warn "TODO: Defined in the runtime"
end

""" #= Pushes stored pointers back to the error stack. =#"""
function freeMessages(handles::List{<:Integer}) #= opaque pointers from popCheckPoint =#
  return @warn "TODO: Defined in the runtime"
end

""" #= @author: adrpo
  This function checks if the specified checkpoint exists AT THE TOP OF THE STACK!.
  You can use it to rollBack/delete a checkpoint, but you're
  not sure that it exists (due to MetaModelica backtracking). =#"""
function isTopCheckpoint(id::String)::Bool #= unique identifier =#
  local isThere::Bool #= tells us if the checkpoint exists (true) or doesn't (false) =#

  @warn "TODO: Defined in the runtime"
  return isThere #= tells us if the checkpoint exists (true) or doesn't (false) =#
end

function setShowErrorMessages(inShow::Bool)
  return @warn "TODO: Defined in the runtime"
end

function moveMessagesToParentThread()
  return @warn "TODO: Defined in the runtime"
end

""" #= Makes assert() and other runtime assertions print to the error buffer =#"""
function initAssertionFunctions()
  return @warn "TODO: Defined in the runtime"
end

@exportAll()
end
