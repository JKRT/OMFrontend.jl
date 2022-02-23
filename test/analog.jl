@testset "Analog.Basic" begin
  prefix = "Modelica.Electrical.Analog.Basic"
  @test typeof(flattenModelInMSL("$(prefix).Ground")[1]) == OMFrontend.Main.FLAT_MODEL
  #@test typeof(flattenModelInMSL("$(prefix).Resistor")[1]) == OMFrontend.Main.FLAT_MODEL
  # @test typeof(flattenModelInMSL("$(prefix).HeatingResistor")[1]) == OMFrontend.Main.FLAT_MODEL
  #@test typeof(flattenModelInMSL("$(prefix).Conductor")[1]) == OMFrontend.Main.FLAT_MODEL
  # @test typeof(flattenModelInMSL("$(prefix).Capacitor")[1]) == OMFrontend.Main.FLAT_MODEL
  # @test typeof(flattenModelInMSL("$(prefix).Inductor")[1]) == OMFrontend.Main.FLAT_MODEL
end #= End Translational=#
#=...=#
@testset "Analog.Sources" begin
  #prefix = "Modelica.Electrical.Analog.Sources"
  #@test typeof(flattenModelInMSL("$(prefix).SignalVoltage")[1]) == OMFrontend.Main.FLAT_MODEL
end