#= Include reference models =#
include("./dynamicOverconstrainedConnectors.jl")
import ..OCC_ReferenceModels

#=
  This test set tests some basic functionality of OCC components
=#
macro test_pass_if_not_throws(modelName::String, modelFile::String)
  @test true == begin
    try
      OMFrontend.flattenModelWithMSL(modelName, modelFile)
      true
    catch e
      @error "Test of $modelName failed" e
      throw(e)
      false
    end
  end
end

@testset "Sanity test. Check that we can translate the components without exceptions are thrown" begin
  @test_pass_if_not_throws("DynamicOverconstrainedConnectors.ACPort", "./Models/DynamicOverconstrainedConnectors.mo")
  @test_pass_if_not_throws("DynamicOverconstrainedConnectors.Load", "./Models/DynamicOverconstrainedConnectors.mo")
  @test_pass_if_not_throws("DynamicOverconstrainedConnectors.Generator", "./Models/DynamicOverconstrainedConnectors.mo")
  @test_pass_if_not_throws("DynamicOverconstrainedConnectors.TransmissionLine", "./Models/DynamicOverconstrainedConnectors.mo")
  @test_pass_if_not_throws("DynamicOverconstrainedConnectors.System1", "./Models/DynamicOverconstrainedConnectors.mo")
  @test_pass_if_not_throws("DynamicOverconstrainedConnectors.System2", "./Models/DynamicOverconstrainedConnectors.mo")
  @test_pass_if_not_throws("DynamicOverconstrainedConnectors.System3", "./Models/DynamicOverconstrainedConnectors.mo")
  @test_pass_if_not_throws("DynamicOverconstrainedConnectors.System4", "./Models/DynamicOverconstrainedConnectors.mo")
end

#= Regression test. Tests the generated code against reference models =#
@testset "Test if the flat Modelica model is equal to the reference models" begin
  local modelFile = "./Models/DynamicOverconstrainedConnectors.mo"
  @test OCC_ReferenceModels.ACPort == OMFrontend.toString(OMFrontend.flattenModelWithMSL("DynamicOverconstrainedConnectors.ACPort", modelFile)[1]);
  @test OCC_ReferenceModels.Load == OMFrontend.toString(OMFrontend.flattenModelWithMSL("DynamicOverconstrainedConnectors.Load", modelFile)[1]);
  @test OCC_ReferenceModels.Generator == OMFrontend.toString(OMFrontend.flattenModelWithMSL("DynamicOverconstrainedConnectors.Generator", modelFile)[1]);
  @test OCC_ReferenceModels.TransmissionLine == OMFrontend.toString(OMFrontend.flattenModelWithMSL("DynamicOverconstrainedConnectors.TransmissionLine", modelFile)[1]);
  @test OCC_ReferenceModels.System1 == OMFrontend.toString(OMFrontend.flattenModelWithMSL("DynamicOverconstrainedConnectors.System1", modelFile)[1]);
  @test OCC_ReferenceModels.System2 == OMFrontend.toString(OMFrontend.flattenModelWithMSL("DynamicOverconstrainedConnectors.System2", modelFile)[1]);
  @test OCC_ReferenceModels.System3 == OMFrontend.toString(OMFrontend.flattenModelWithMSL("DynamicOverconstrainedConnectors.System3", modelFile)[1]);
  @test OCC_ReferenceModels.System4 == OMFrontend.toString(OMFrontend.flattenModelWithMSL("DynamicOverconstrainedConnectors.System4", modelFile)[1]);
end

#= Check the resulting flat code. =#
@testset "Check the resulting flat code" begin
end
