@testset "Rotational" begin
  @testset "Components" begin
    prefix = "Modelica.Mechanics.Rotational.Components"
    @test typeof(flattenModelInMSL("$(prefix).Spring")[1]) == OMFrontend.Main.FLAT_MODEL
    @test typeof(flattenModelInMSL("$(prefix).Fixed")[1]) == OMFrontend.Main.FLAT_MODEL
    @test typeof(flattenModelInMSL("$(prefix).Inertia")[1]) == OMFrontend.Main.FLAT_MODEL
    @test typeof(flattenModelInMSL("$(prefix).Disc")[1]) == OMFrontend.Main.FLAT_MODEL
    @test typeof(flattenModelInMSL("$(prefix).Damper")[1]) == OMFrontend.Main.FLAT_MODEL
    @test typeof(flattenModelInMSL("$(prefix).SpringDamper")[1]) == OMFrontend.Main.FLAT_MODEL
    @test_skip typeof(flattenModelInMSL("$(prefix).ElastoBacklash")[1]) == OMFrontend.Main.FLAT_MODEL
    @test_skip typeof(flattenModelInMSL("$(prefix).ElastoBacklash2")[1]) == OMFrontend.Main.FLAT_MODEL
    @test_skip typeof(flattenModelInMSL("$(prefix).BearingFriction")[1]) == OMFrontend.Main.FLAT_MODEL
    @test_skip typeof(flattenModelInMSL("$(prefix).Brake")[1]) == OMFrontend.Main.FLAT_MODEL
    @test_skip typeof(flattenModelInMSL("$(prefix).Clutch")[1]) == OMFrontend.Main.FLAT_MODEL
    @test_skip typeof(flattenModelInMSL("$(prefix).OneWayClutch")[1]) == OMFrontend.Main.FLAT_MODEL
    @test typeof(flattenModelInMSL("$(prefix).IdealGear")[1]) == OMFrontend.Main.FLAT_MODEL
    @test_skip typeof(flattenModelInMSL("$(prefix).LossyGear")[1]) == OMFrontend.Main.FLAT_MODEL
    @test typeof(flattenModelInMSL("$(prefix).IdealPlanetary")[1]) == OMFrontend.Main.FLAT_MODEL
    @test_skip typeof(flattenModelInMSL("$(prefix).Gearbox")[1]) == OMFrontend.Main.FLAT_MODEL
    @test_skip typeof(flattenModelInMSL("$(prefix).IdealGearR2T")[1]) == OMFrontend.Main.FLAT_MODEL
    @test typeof(flattenModelInMSL("$(prefix).IdealRollingWheel")[1]) == OMFrontend.Main.FLAT_MODEL
  end
  @testset "Examples" begin
    prefix = "Modelica.Mechanics.Rotational.Components.Examples"
    #@test typeof(flattenModelInMSL("$(prefix).First")[1]) == OMFrontend.Main.FLAT_MODEL        
  end  
end #= End Rotational=#
