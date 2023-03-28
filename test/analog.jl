@testset "Analog.Basic" begin
  prefix = "Modelica.Electrical.Analog.Basic"
  @test typeof(flattenModelInMSL_TST("$(prefix).Ground")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).Resistor")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).HeatingResistor")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).Conductor")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).Capacitor")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).Inductor")[1]) == OMFrontend.Main.FLAT_MODEL
  #Note that this generates a lot of error messages but does not fail to translate...
  @test typeof(flattenModelInMSL_TST("$(prefix).SaturatingInductor")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).Transformer")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).M_Transformer")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).Gyrator")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).EMF")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).TranslationalEMF")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).VCV")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).VCC")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).CCV")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).CCC")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).OpAmp")[1]) == OMFrontend.Main.FLAT_MODEL
  @test_skip typeof(flattenModelInMSL_TST("$(prefix).OpAmpDetailed")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).VariableResistor")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).VariableConductor")[1]) == OMFrontend.Main.FLAT_MODEL
  @test_skip typeof(flattenModelInMSL_TST("$(prefix).VariableCapacitor")[1]) == OMFrontend.Main.FLAT_MODEL
end #= End Analog =#
#= Testing some sources in Analog.Sources=#
@testset "Analog.Sources" begin
  prefix = "Modelica.Electrical.Analog.Sources"
  @test typeof(flattenModelInMSL_TST("$(prefix).SignalVoltage")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).ConstantVoltage")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).SineVoltage")[1]) == OMFrontend.Main.FLAT_MODEL
  #Broken @test typeof(flattenModelInMSL_TST("$(prefix).StepVoltage")[1]) == OMFrontend.Main.FLAT_MODEL
end

@testset "Analog.Examples" begin
  prefix = "Modelica.Electrical.Analog.Examples"
  @test_skip typeof(flattenModelInMSL_TST("$(prefix).CauerLowPassAnalog")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).ChuaCircuit")[1]) == OMFrontend.Main.FLAT_MODEL
end
