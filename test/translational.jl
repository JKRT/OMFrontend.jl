@testset "Translational" begin
  prefix = "Modelica.Mechanics.Translational.Components"
  @test typeof(flattenModelInMSL_TST("$(prefix).Fixed")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).Mass")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).Rod")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).Spring")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).Damper")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).SpringDamper")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).ElastoGap")[1]) == OMFrontend.Main.FLAT_MODEL
  #TODO @test typeof(flattenModelInMSL_TST("$(prefix).SupportFriction")[1]) == OMFrontend.Main.FLAT_MODEL
  #TODO @test typeof(flattenModelInMSL_TST("$(prefix).Brake")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).IdealGearR2T")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).IdealRollingWheel")[1]) == OMFrontend.Main.FLAT_MODEL
end #= End Translational=#
