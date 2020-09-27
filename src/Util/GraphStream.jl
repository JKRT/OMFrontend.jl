module GraphStream

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

import Main.Autoconf
import Main.GraphStreamExt
import Main.System
import Main.Settings

function startExternalViewer(host::String, port::Integer)::Integer
  local status::Integer

  @assign status = begin
    local omhome::String
    local command::String
    local commandWin::String
    local commandLinux::String
    @matchcontinue (host, port) begin
      (_, _) => begin
        @assign omhome = Settings.getInstallationDirectoryPath()
        @assign commandWin =
          "start /b java -jar " + omhome + "/share/omc/java/org.omc.graphstream.jar"
        @assign commandLinux =
          "java -jar " + omhome + "/share/omc/java/org.omc.graphstream.jar &"
        @assign command = if "Windows_NT" == Autoconf.os
          commandWin
        else
          commandLinux
        end
        @assign status = System.systemCall(command, "")
        @match true = status == 0
        status
      end

      _ => begin
        print("GraphStream: failed to start the external viewer!\\n")
        fail()
      end
    end
  end
  return status
end

function newStream(streamName::String, host::String, port::Integer, debug::Bool)
  return GraphStreamExt.newStream(streamName, host, port, debug)
end

function addNode(streamName::String, sourceId::String, timeId::Integer, nodeId::String)
  return GraphStreamExt.addNode(streamName, sourceId, timeId, nodeId)
end

function addEdge(
  streamName::String,
  sourceId::String,
  timeId::Integer,
  nodeIdSource::String,
  nodeIdTarget::String,
  directed::Bool,
)
  return GraphStreamExt.addEdge(
    streamName,
    sourceId,
    timeId,
    nodeIdSource,
    nodeIdTarget,
    directed,
  )
end

function addNodeAttribute(
  streamName::String,
  sourceId::String,
  timeId::Integer,
  nodeId::String,
  attributeName::String,
  attributeValue::Values.Value,
)
  return GraphStreamExt.addNodeAttribute(
    streamName,
    sourceId,
    timeId,
    nodeId,
    attributeName,
    attributeValue,
  )
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
  return GraphStreamExt.changeNodeAttribute(
    streamName,
    sourceId,
    timeId,
    nodeId,
    attributeName,
    attributeValueOld,
    attributeValueNew,
  )
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
  return GraphStreamExt.addEdgeAttribute(
    streamName,
    sourceId,
    timeId,
    nodeIdSource,
    nodeIdTarget,
    attributeName,
    attributeValue,
  )
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
  return GraphStreamExt.changeEdgeAttribute(
    streamName,
    sourceId,
    timeId,
    nodeIdSource,
    nodeIdTarget,
    attributeName,
    attributeValueOld,
    attributeValueNew,
  )
end

function addGraphAttribute(
  streamName::String,
  sourceId::String,
  timeId::Integer,
  attributeName::String,
  attributeValue::Values.Value,
)
  return GraphStreamExt.addGraphAttribute(
    streamName,
    sourceId,
    timeId,
    attributeName,
    attributeValue,
  )
end

function changeGraphAttribute(
  streamName::String,
  sourceId::String,
  timeId::Integer,
  attributeName::String,
  attributeValueOld::Values.Value,
  attributeValueNew::Values.Value,
)
  return GraphStreamExt.changeGraphAttribute(
    streamName,
    sourceId,
    timeId,
    attributeName,
    attributeValueOld,
    attributeValueNew,
  )
end

function cleanup()
  return GraphStreamExt.cleanup()
end

@exportAll()
end
