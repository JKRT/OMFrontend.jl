module StackOverflow

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

import Main.System
import Main.Testsuite

function unmangle(inSymbol::String)::String
  local outSymbol::String

  @assign outSymbol = inSymbol
  if stringLength(inSymbol) > 4
    if substring(inSymbol, 1, 4) == "omc_"
      @assign outSymbol = substring(outSymbol, 5, stringLength(outSymbol))
      @assign outSymbol = System.stringReplace(outSymbol, "__", "#")
      @assign outSymbol = System.stringReplace(outSymbol, "_", ".")
      @assign outSymbol = System.stringReplace(outSymbol, "#", "_")
    end
  end
  return outSymbol
end

function stripAddresses(inSymbol::String)::String
  local outSymbol::String

  local n::Integer
  local strs::List{String}
  local so::String
  local fun::String

  #=  regex for Linux messages
  =#
  @assign (n, strs) = System.regex(
    inSymbol,
    "^([^(]*)[(]([^+]*[^+]*)[+][^)]*[)] *[[]0x[0-9a-fA-F]*[]]",
    3,
    extended = true,
  )
  if n == 3
    @match list(_, so, fun) = strs
    @assign outSymbol = so + "(" + unmangle(fun) + ")"
  else
    @assign (n, strs) = System.regex(
      inSymbol,
      "^[0-9 ]*([A-Za-z0-9.]*) *0x[0-9a-fA-F]* ([A-Za-z0-9_]*) *[+] *[0-9]*",
      3,
      extended = true,
    )
    if n == 3
      @match list(_, so, fun) = strs
      @assign outSymbol = so + "(" + unmangle(fun) + ")"
    else
      @assign outSymbol = inSymbol
    end
  end
  #=  regex for OSX messages
  =#
  return outSymbol
end

function triggerStackOverflow()
  return ## REENABLE @debug "TODO: Defined in the runtime"
end

function generateReadableMessage(
  numFrames::Integer = 1000,
  numSkip::Integer = 4,
  delimiter::String = "\\n",
)::String
  local str::String

  StackOverflow.setStacktraceMessages(numSkip, numFrames)
  @assign str = getReadableMessage(delimiter = delimiter)
  return str
end

function getReadableMessage(delimiter::String = "\\n")::String
  local str::String

  @assign str = stringDelimitList(StackOverflow.readableStacktraceMessages(), delimiter)
  return str
end

function readableStacktraceMessages()::List{String}
  local symbols::List{String} = nil

  local prev::String = ""
  local n::Integer = 1
  local prevN::Integer = 1

  if Testsuite.isRunning()
    @assign symbols = list("[bt] [Symbols are not generated when running the test suite]")
    return symbols
  end
  for symbol in List(stripAddresses(s) for s in getStacktraceMessages())
    if prev == ""
    elseif symbol != prev
      @assign symbols = _cons("[bt] #" + String(prevN) + (
        if n != prevN
          "..." + String(n)
        else
          ""
        end
      ) + " " + prev, symbols)
      @assign n = n + 1
      @assign prevN = n
    else
      @assign n = n + 1
    end
    @assign prev = symbol
  end
  @assign symbols = _cons("[bt] #" + String(prevN) + (
    if n != prevN
      "..." + String(n)
    else
      ""
    end
  ) + " " + prev, symbols)
  @assign symbols = listReverse(symbols)
  return symbols
end

function getStacktraceMessages()::List{String}
  local symbols::List{String}

  ## REENABLE @debug "TODO: Defined in the runtime"
  return symbols
end

function setStacktraceMessages(numSkip::Integer, numFrames::Integer)
  return ## REENABLE @debug "TODO: Defined in the runtime"
end

function hasStacktraceMessages()::Bool
  local b::Bool

  ## REENABLE @debug "TODO: Defined in the runtime"
  return b
end

function clearStacktraceMessages()
  return ## REENABLE @debug "TODO: Defined in the runtime"
end

@exportAll()
end
