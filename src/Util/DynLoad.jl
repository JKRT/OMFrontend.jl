module DynLoad

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

import Main.Values

import Main.Error
import Main.StackOverflow

""" #= 
Executes a function given a handle (from System.lookupFunction) and a list
of values. =#"""
function executeFunction(
  handle::Integer,
  values::List{<:Values.Value},
  debug::Bool,
)::Values.Value
  local outVal::Values.Value

  function executeFunction_internal(
    handle::Integer,
    values::List{<:Values.Value},
    debug::Bool,
  )::Values.Value
    local outVal::Values.Value

    ## REENABLE @debug "TODO: Defined in the runtime"
    return outVal
  end

  StackOverflow.clearStacktraceMessages()
  #=  executeFunction returns Values.META_FAIL on stack overflow...
  =#
  @assign outVal = executeFunction_internal(handle, values, debug)
  if StackOverflow.hasStacktraceMessages()
    Error.addInternalError(
      "Stack overflow when evaluating function:\\n" +
      stringDelimitList(StackOverflow.readableStacktraceMessages(), "\\n"),
      sourceInfo(),
    )
  end
  #=  fail();  Causes seg.fault for some reason.
  =#
  return outVal
end

@exportAll()
end
