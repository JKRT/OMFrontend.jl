@testset "Translational" begin
  prefix = "Modelica.Mechanics.Translational.Components"
  @test typeof(flattenModelInMSL("$(prefix).Fixed")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL("$(prefix).Mass")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL("$(prefix).Rod")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL("$(prefix).Spring")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL("$(prefix).Damper")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL("$(prefix).SpringDamper")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL("$(prefix).ElastoGap")[1]) == OMFrontend.Main.FLAT_MODEL
  #TODO @test typeof(flattenModelInMSL("$(prefix).SupportFriction")[1]) == OMFrontend.Main.FLAT_MODEL
  #TODO @test typeof(flattenModelInMSL("$(prefix).Brake")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL("$(prefix).IdealGearR2T")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL("$(prefix).IdealRollingWheel")[1]) == OMFrontend.Main.FLAT_MODEL
end #= End Translational=#
