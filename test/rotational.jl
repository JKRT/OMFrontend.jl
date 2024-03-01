@testset "Rotational" begin
  @testset "Components" begin
    prefix = "Modelica.Mechanics.Rotational.Components"
    @test typeof(flattenModelInMSL_TST("$(prefix).Spring")[1]) == OMFrontend.Main.FLAT_MODEL
    @test typeof(flattenModelInMSL_TST("$(prefix).Fixed")[1]) == OMFrontend.Main.FLAT_MODEL
    @test typeof(flattenModelInMSL_TST("$(prefix).Inertia")[1]) == OMFrontend.Main.FLAT_MODEL
    @test typeof(flattenModelInMSL_TST("$(prefix).Disc")[1]) == OMFrontend.Main.FLAT_MODEL
    @test typeof(flattenModelInMSL_TST("$(prefix).Damper")[1]) == OMFrontend.Main.FLAT_MODEL
    @test typeof(flattenModelInMSL_TST("$(prefix).SpringDamper")[1]) == OMFrontend.Main.FLAT_MODEL
    @test typeof(flattenModelInMSL_TST("$(prefix).ElastoBacklash")[1]) == OMFrontend.Main.FLAT_MODEL
    @test typeof(flattenModelInMSL_TST("$(prefix).ElastoBacklash2")[1]) == OMFrontend.Main.FLAT_MODEL
    @test typeof(flattenModelInMSL_TST("$(prefix).BearingFriction")[1]) == OMFrontend.Main.FLAT_MODEL
    @test typeof(flattenModelInMSL_TST("$(prefix).Brake")[1]) == OMFrontend.Main.FLAT_MODEL
    @test typeof(flattenModelInMSL_TST("$(prefix).Clutch")[1]) == OMFrontend.Main.FLAT_MODEL
    @test typeof(flattenModelInMSL_TST("$(prefix).OneWayClutch")[1]) == OMFrontend.Main.FLAT_MODEL
    @test typeof(flattenModelInMSL_TST("$(prefix).IdealGear")[1]) == OMFrontend.Main.FLAT_MODEL
    @test typeof(flattenModelInMSL_TST("$(prefix).LossyGear")[1]) == OMFrontend.Main.FLAT_MODEL
    @test typeof(flattenModelInMSL_TST("$(prefix).IdealPlanetary")[1]) == OMFrontend.Main.FLAT_MODEL
    @test typeof(flattenModelInMSL_TST("$(prefix).Gearbox")[1]) == OMFrontend.Main.FLAT_MODEL
    @test typeof(flattenModelInMSL_TST("$(prefix).IdealGearR2T")[1]) == OMFrontend.Main.FLAT_MODEL
    @test typeof(flattenModelInMSL_TST("$(prefix).IdealRollingWheel")[1]) == OMFrontend.Main.FLAT_MODEL
  end
  #=All example models in Rotational.... =#
  @testset "Examples" begin
    prefix = "Modelica.Mechanics.Rotational.Examples"
    @test typeof(flattenModelInMSL_TST("$(prefix).First")[1]) == OMFrontend.Main.FLAT_MODEL
    @test typeof(flattenModelInMSL_TST("$(prefix).FirstGrounded")[1]) == OMFrontend.Main.FLAT_MODEL
    @test typeof(flattenModelInMSL_TST("$(prefix).Friction")[1]) == OMFrontend.Main.FLAT_MODEL
    @test typeof(flattenModelInMSL_TST("$(prefix).CoupledClutches")[1]) == OMFrontend.Main.FLAT_MODEL
    @test typeof(flattenModelInMSL_TST("$(prefix).LossyGearDemo1")[1]) == OMFrontend.Main.FLAT_MODEL
    @test typeof(flattenModelInMSL_TST("$(prefix).LossyGearDemo2")[1]) == OMFrontend.Main.FLAT_MODEL
    @test typeof(flattenModelInMSL_TST("$(prefix).LossyGearDemo3")[1]) == OMFrontend.Main.FLAT_MODEL
    @test typeof(flattenModelInMSL_TST("$(prefix).ElasticBearing")[1]) == OMFrontend.Main.FLAT_MODEL
    @test typeof(flattenModelInMSL_TST("$(prefix).Backlash")[1]) == OMFrontend.Main.FLAT_MODEL
    @test typeof(flattenModelInMSL_TST("$(prefix).RollingWheel")[1]) == OMFrontend.Main.FLAT_MODEL
    @test typeof(flattenModelInMSL_TST("$(prefix).HeatLosses")[1]) == OMFrontend.Main.FLAT_MODEL
    @test typeof(flattenModelInMSL_TST("$(prefix).SimpleGearShift")[1]) == OMFrontend.Main.FLAT_MODEL
    @test typeof(flattenModelInMSL_TST("$(prefix).EddyCurrentBrake")[1]) == OMFrontend.Main.FLAT_MODEL
    @test typeof(flattenModelInMSL_TST("$(prefix).OneWayClutch")[1]) == OMFrontend.Main.FLAT_MODEL
    @test typeof(flattenModelInMSL_TST("$(prefix).OneWayClutchDisengaged")[1]) == OMFrontend.Main.FLAT_MODEL
  end
end #= End Rotational=#
