module SimulationResults

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

import ListUtil
import Main.ValuesUtil

function val(filename::String, varname::String, timeStamp::AbstractFloat)::AbstractFloat
  local val::AbstractFloat

  ## REENABLE @debug "TODO: Defined in the runtime"
  return val
end

function readVariables(
  filename::String,
  readParameters::Bool = true,
  openmodelicaStyle::Bool = false,
)::List{String}
  local vars::List{String}

  ## REENABLE @debug "TODO: Defined in the runtime"
  return vars
end

function readDataset(filename::String, vars::List{<:String}, dimsize::Integer)::Values.Value
  local val::Values.Value

  local rvals::List{List{AbstractFloat}}
  local vals::List{List{Values.Value}}
  local rows::List{Values.Value}

  function readDataset_work(
    filename::String,
    vars::List{<:String},
    dimsize::Integer,
  )::List{List{AbstractFloat}}
    local outMatrix::List{List{AbstractFloat}}

    ## REENABLE @debug "TODO: Defined in the runtime"
    return outMatrix
  end

  @assign rvals = readDataset_work(filename, vars, dimsize)
  @assign vals = ListUtil.mapListReverse(rvals, ValuesUtil.makeReal)
  @assign rows = ListUtil.mapReverse(vals, ValuesUtil.makeArray)
  @assign val = ValuesUtil.makeArray(rows)
  return val
end

function readSimulationResultSize(filename::String)::Integer
  local size::Integer

  ## REENABLE @debug "TODO: Defined in the runtime"
  return size
end

function close()
  return ## REENABLE @debug "TODO: Defined in the runtime"
end

function cmpSimulationResults(
  runningTestsuite::Bool,
  filename::String,
  reffilename::String,
  logfilename::String,
  refTol::AbstractFloat,
  absTol::AbstractFloat,
  vars::List{<:String},
)::List{String}
  local res::List{String}

  ## REENABLE @debug "TODO: Defined in the runtime"
  return res
end

function deltaSimulationResults(
  filename::String,
  reffilename::String,
  method::String,
  vars::List{<:String},
)::AbstractFloat
  local res::AbstractFloat

  ## REENABLE @debug "TODO: Defined in the runtime"
  return res
end

function diffSimulationResults(
  runningTestsuite::Bool,
  filename::String,
  reffilename::String,
  prefix::String,
  refTol::AbstractFloat,
  relTolDiffMaxMin::AbstractFloat,
  rangeDelta::AbstractFloat,
  vars::List{<:String},
  keepEqualResults::Bool,
)::Tuple{Bool, List{String}}
  local res::List{String}
  local success::Bool

  ## REENABLE @debug "TODO: Defined in the runtime"
  return (success, res)
end

function diffSimulationResultsHtml(
  runningTestsuite::Bool,
  filename::String,
  reffilename::String,
  refTol::AbstractFloat,
  relTolDiffMaxMin::AbstractFloat,
  rangeDelta::AbstractFloat,
  var::String,
)::String
  local html::String

  ## REENABLE @debug "TODO: Defined in the runtime"
  return html
end

function filterSimulationResults(
  inFile::String,
  outFile::String,
  vars::List{<:String},
  numberOfIntervals::Integer = 0,
  removeDescription::Bool,
)::Bool
  local result::Bool

  ## REENABLE @debug "TODO: Defined in the runtime"
  return result
end

@exportAll()
end
