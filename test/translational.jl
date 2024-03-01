@testset "Translational" begin
  @testset "Components" begin
    prefix = "Modelica.Mechanics.Translational.Components"
    @test typeof(flattenModelInMSL_TST("$(prefix).Fixed")[1]) == OMFrontend.Main.FLAT_MODEL
    @test typeof(flattenModelInMSL_TST("$(prefix).Mass")[1]) == OMFrontend.Main.FLAT_MODEL
    @test typeof(flattenModelInMSL_TST("$(prefix).Rod")[1]) == OMFrontend.Main.FLAT_MODEL
    @test typeof(flattenModelInMSL_TST("$(prefix).Spring")[1]) == OMFrontend.Main.FLAT_MODEL
    @test typeof(flattenModelInMSL_TST("$(prefix).Damper")[1]) == OMFrontend.Main.FLAT_MODEL
    @test typeof(flattenModelInMSL_TST("$(prefix).SpringDamper")[1]) == OMFrontend.Main.FLAT_MODEL
    @test typeof(flattenModelInMSL_TST("$(prefix).ElastoGap")[1]) == OMFrontend.Main.FLAT_MODEL
    @test typeof(flattenModelInMSL_TST("$(prefix).SupportFriction")[1]) == OMFrontend.Main.FLAT_MODEL
    @test typeof(flattenModelInMSL_TST("$(prefix).Brake")[1]) == OMFrontend.Main.FLAT_MODEL
    @test typeof(flattenModelInMSL_TST("$(prefix).IdealGearR2T")[1]) == OMFrontend.Main.FLAT_MODEL
    @test typeof(flattenModelInMSL_TST("$(prefix).IdealRollingWheel")[1]) == OMFrontend.Main.FLAT_MODEL
  end
  #= All the Translational examples=#
  @testset "Examples" begin
    prefix = "Modelica.Mechanics.Translational.Examples"
    @test typeof(flattenModelInMSL_TST("$(prefix).SignConvention")[1]) == OMFrontend.Main.FLAT_MODEL
    @test typeof(flattenModelInMSL_TST("$(prefix).InitialConditions")[1]) == OMFrontend.Main.FLAT_MODEL
    @test typeof(flattenModelInMSL_TST("$(prefix).WhyArrows")[1]) == OMFrontend.Main.FLAT_MODEL
    @test typeof(flattenModelInMSL_TST("$(prefix).Accelerate")[1]) == OMFrontend.Main.FLAT_MODEL
    @test typeof(flattenModelInMSL_TST("$(prefix).Damper")[1]) == OMFrontend.Main.FLAT_MODEL
    @test typeof(flattenModelInMSL_TST("$(prefix).Oscillator")[1]) == OMFrontend.Main.FLAT_MODEL
    @test typeof(flattenModelInMSL_TST("$(prefix).Sensors")[1]) == OMFrontend.Main.FLAT_MODEL
    @test typeof(flattenModelInMSL_TST("$(prefix).Friction")[1]) == OMFrontend.Main.FLAT_MODEL
    @test typeof(flattenModelInMSL_TST("$(prefix).PreLoad")[1]) == OMFrontend.Main.FLAT_MODEL
    @test typeof(flattenModelInMSL_TST("$(prefix).ElastoGap")[1]) == OMFrontend.Main.FLAT_MODEL
    @test typeof(flattenModelInMSL_TST("$(prefix).Brake")[1]) == OMFrontend.Main.FLAT_MODEL
    @test typeof(flattenModelInMSL_TST("$(prefix).HeatLosses")[1]) == OMFrontend.Main.FLAT_MODEL
    @test typeof(flattenModelInMSL_TST("$(prefix).EddyCurrentBrake")[1]) == OMFrontend.Main.FLAT_MODEL
  end
end #= End Translational=#
