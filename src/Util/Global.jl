module Global

using MetaModelica
using ExportAll
import ..System

#= /*
* This file is part of OpenModelica.
*
* Copyright (c) 1998-CurrentYear, Open Source Modelica Consortium (OSMC),
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
const recursionDepthLimit = 256::Integer
const maxFunctionFileLength = 50::Integer
#=  Thread-local roots
=#
const instOnlyForcedFunctions = 0::Integer
const simulationData = 0::Integer #= For simulations =#
const codegenTryThrowIndex = 1::Integer
const codegenFunctionList = 2::Integer
const symbolTable = 3::Integer
#=  Global roots start at index=9
=#
const instHashIndex = 9::Integer
const instNFInstCacheIndex = 10::Integer
const instNFNodeCacheIndex = 11::Integer
const builtinIndex = 12::Integer
const builtinEnvIndex = 13::Integer
const profilerTime1Index = 14::Integer
const profilerTime2Index = 15::Integer
const flagsIndex = 16::Integer
const builtinGraphIndex = 17::Integer
const rewriteRulesIndex = 18::Integer
const stackoverFlowIndex = 19::Integer
const gcProfilingIndex = 20::Integer
const inlineHashTable = 21::Integer
#=  TODO: Should be a local root?
=#
const currentInstVar = 22::Integer
const operatorOverloadingCache = 23::Integer
const optionSimCode = 24::Integer
const interactiveCache = 25::Integer
const isInStream = 26::Integer
const MM_TO_JL_HT_INDEX = 27::Integer
const packageIndexCacheIndex = 28::Integer
#=  indexes in System.tick
=#
#=  ----------------------
=#
#=  temp vars index
=#
const tmpVariableIndex = 4::Integer
#=  file seq
=#
const backendDAE_fileSequence = 20::Integer
#=  jacobian name
=#
const backendDAE_jacobianSeq = 21::Integer
#=  nodeId
=#
const fgraph_nextId = 22::Integer
#=  csevar name
=#
const backendDAE_cseIndex = 23::Integer
#=  strong component index
=#
const strongComponent_index = 24::Integer
#=  class extends
=#
const classExtends_index = 25::Integer
#=  ----------------------
=#

""" #= Called to initialize global roots (when needed) =#"""
function initialize()
  setGlobalRoot(instOnlyForcedFunctions, NONE())
  setGlobalRoot(rewriteRulesIndex, NONE())
  setGlobalRoot(stackoverFlowIndex, NONE())
  setGlobalRoot(inlineHashTable, NONE())
  setGlobalRoot(currentInstVar, NONE())
  setGlobalRoot(interactiveCache, NONE())
  setGlobalRoot(instNFInstCacheIndex, nil)
  return setGlobalRoot(instNFNodeCacheIndex, nil)
end

@exportAll()
end
