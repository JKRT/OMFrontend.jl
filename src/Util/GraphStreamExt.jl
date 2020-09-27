module GraphStreamExt

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

function newStream(streamName::String, host::String, port::Integer, debug::Bool)
  return @error "TODO: Defined in the runtime"
end

function addNode(streamName::String, sourceId::String, timeId::Integer, nodeId::String)
  return @error "TODO: Defined in the runtime"
end

function addEdge(
  streamName::String,
  sourceId::String,
  timeId::Integer,
  nodeIdSource::String,
  nodeIdTarget::String,
  directed::Bool,
)
  return @error "TODO: Defined in the runtime"
end

function addNodeAttribute(
  streamName::String,
  sourceId::String,
  timeId::Integer,
  nodeId::String,
  attributeName::String,
  attributeValue::Values.Value,
)
  return @error "TODO: Defined in the runtime"
end

function changeNodeAttribute(
  streamName::String,
  sourceId::String,
  timeId::Integer,
  nodeId::String,
  attributeName::String,
  attributeValueOld::Values.Value,
  attributeValueNew::Values.Value,
)
  return @error "TODO: Defined in the runtime"
end

function addEdgeAttribute(
  streamName::String,
  sourceId::String,
  timeId::Integer,
  nodeIdSource::String,
  nodeIdTarget::String,
  attributeName::String,
  attributeValue::Values.Value,
)
  return @error "TODO: Defined in the runtime"
end

function changeEdgeAttribute(
  streamName::String,
  sourceId::String,
  timeId::Integer,
  nodeIdSource::String,
  nodeIdTarget::String,
  attributeName::String,
  attributeValueOld::Values.Value,
  attributeValueNew::Values.Value,
)
  return @error "TODO: Defined in the runtime"
end

function addGraphAttribute(
  streamName::String,
  sourceId::String,
  timeId::Integer,
  attributeName::String,
  attributeValue::Values.Value,
)
  return @error "TODO: Defined in the runtime"
end

function changeGraphAttribute(
  streamName::String,
  sourceId::String,
  timeId::Integer,
  attributeName::String,
  attributeValueOld::Values.Value,
  attributeValueNew::Values.Value,
)
  return @error "TODO: Defined in the runtime"
end

function cleanup()
  return @error "TODO: Defined in the runtime"
end

@exportAll()
end
