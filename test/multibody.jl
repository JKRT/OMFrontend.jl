#= Test for the multi body components =#

@testset "Testing to Flatten Modelica.Mechanics.MultiBody.Examples.Elementary..." begin
  @info "Testing to Flatten Modelica.Mechanics.MultiBody.Examples.Elementary"
  prefix = "Modelica.Mechanics.MultiBody.Examples.Elementary"
  @test typeof(flattenModelInMSL_TST("$(prefix).DoublePendulum")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).DoublePendulumInitTip")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).ForceAndTorque")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).FreeBody")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).InitSpringConstant")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).LineForceWithTwoMasses")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).Pendulum")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).PendulumWithSpringDamper")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).PointGravity")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).PointGravityWithPointMasses")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).PointGravityWithPointMasses2")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).SpringDamperSystem")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).SpringMassSystem")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).SpringWithMass")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).ThreeSprings")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).RollingWheel")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).RollingWheelSetDriving")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).RollingWheelSetPulling")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).HeatLosses")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).UserDefinedGravityField")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).Surfaces")[1]) == OMFrontend.Main.FLAT_MODEL
  #= Add more examples here! =#
end #= Modelica.Mechanics.MultiBody.Examples.Elementary=#

#=
Test some of the models in loops including the V6 Engine model
Not run every test. Takes quite a while currently...
=#
# @testset "Testing to Flatten Modelica.Mechanics.MultiBody.Examples.Loops..." begin
#   @info "Testing to Flatten Modelica.Mechanics.MultiBody.Examples.Loops"
#   prefix = "Modelica.Mechanics.MultiBody.Examples.Loops"
#   @test typeof(flattenModelInMSL_TST("$(prefix).Engine1a")[1]) == OMFrontend.Main.FLAT_MODEL
#   @test typeof(flattenModelInMSL_TST("$(prefix).Engine1b")[1]) == OMFrontend.Main.FLAT_MODEL
#   @test typeof(flattenModelInMSL_TST("$(prefix).Engine1b_analytic")[1]) == OMFrontend.Main.FLAT_MODEL
#   @test typeof(flattenModelInMSL_TST("$(prefix).EngineV6")[1]) == OMFrontend.Main.FLAT_MODEL
# end #= Modelica.Mechanics.MultiBody.Examples.Elementary=#
