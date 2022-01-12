#=
  Author: John Tinnerholm johti17@liu.se
=#


import Pkg

Pkg.resolve()

import Absyn
import SCode
import DAE
using MetaModelica
using Test
import OMFrontend


#= Utility functions =#
function flatten(modelName::String, modelFile::String)
  @time p = OMFrontend.parseFile(modelFile)
  @time scodeProgram = OMFrontend.translateToSCode(p)
  @time (dae, cache) = OMFrontend.instantiateSCodeToDAE(modelName, scodeProgram)
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

@testset "Frontend sanitiy tests. Check if we can transform the abstract tree to SCode and that we are able to flatten without exceptions" begin
  @testset "Absyn -> SCode test" begin
    include("scodeSanityTest.jl")
    end
  @testset "SCode -> DAE Sanity test" begin
    include("daeTests.jl")
  end
end

# #= Check that we get the correct flat Modelica=#
@testset "Frontend Validation test. Check that the result corresponds to existing models in the original frontend " begin
  include("frontendResultTest.jl")
end

# #= Test some of the MSL=#
@testset "Simple MSL test" begin
  include("mslTests.jl")
end

#= OMFrontend.jl extensions for variable structured systems =#
@testset "Basic VSS models" begin
  include("./VSS/testVSS.jl")
end
