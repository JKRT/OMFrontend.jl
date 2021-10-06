#=
  Author: John Tinnerholm johti17@liu.se

Current Status:

Works when including the file in the terminal. Does not seem to work when run via tests.

=#
#using Revise For include based work
#using Revise
#= Remove before commit =#
import OMFrontend
import Absyn
import SCode
import DAE

using MetaModelica
using Test

#= Utility functions =#
function flatten(modelName::String, modelFile::String)
  p = OMFrontend.parseFile(modelFile)
  scodeProgram = OMFrontend.translateToSCode(p)
  (dae, cache) = OMFrontend.instantiateSCodeToDAE(modelName, scodeProgram)
end

function parseAndLowerToScode(modelName::String, modelFile::String)
  p = OMFrontend.parseFile(modelFile)
  scodeProgram = OMFrontend.translateToSCode(p)
end

"""
  Flattens to flat Modelica. 
  Note that, the full path specification of the file is expected.
"""
function flattenFM(model, file)
  local sp = OMFrontend.parseFile(file)
  local scode = OMFrontend.translateToSCode(sp)
  local res = OMFrontend.instantiateSCodeToFM(model, scode)
  return res
end

#= Actualy tests =#

@testset "Frontend sanitiy tests. Check if we can transform the abstract tree to SCode and that we are able to flatten without exceptions" begin
  @testset "Absyn -> SCode test" begin
    include("scodeSanityTest.jl")
  end
  
  @testset "SCode -> DAE Sanity test" begin
    include("daeTests.jl")
  end
end

@testset "Frontend Validation test. Check that the result corresponds to existing models in the original frontend " begin
    include("frontendResultTest.jl")
end
