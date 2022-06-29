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

#= Check the resulting flat code. =#
@testset "Check the resulting flat code" begin
end
