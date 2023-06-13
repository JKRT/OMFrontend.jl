#= Testing a selection of basic components =#
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
  @test typeof(flattenModelInMSL_TST("$(prefix).OpAmpDetailed")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).VariableResistor")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).VariableConductor")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).VariableCapacitor")[1]) == OMFrontend.Main.FLAT_MODEL
end #= End Analog =#
#= Testing some sources in Analog.Sources=#
@testset "Analog.Sources" begin
  prefix = "Modelica.Electrical.Analog.Sources"
  @test typeof(flattenModelInMSL_TST("$(prefix).SignalVoltage")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).ConstantVoltage")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).SineVoltage")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).StepVoltage")[1]) == OMFrontend.Main.FLAT_MODEL
end

#= Test example models in analog.examples =#
@testset "Analog.Examples" begin
  prefix = "Modelica.Electrical.Analog.Examples"
  @test typeof(flattenModelInMSL_TST("$(prefix).AD_DA_conversion")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).AmplifierWithOpAmpDetailed")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).CauerLowPassAnalog")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).CauerLowPassOPV")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).CauerLowPassSC")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).CharacteristicIdealDiodes")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).CharacteristicThyristors")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).ChuaCircuit")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).CompareTransformers")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).ControlledSwitchWithArc")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).DifferenceAmplifier")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).GenerationOfFMUs")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).HeatingMOSInverter")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).HeatingNPN_OrGate")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).HeatingPNP_NORGate")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).HeatingRectifier")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).HeatingResistor")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).NandGate")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).OvervoltageProtection")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).Rectifier")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).ResonanceCircuits")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).ShowSaturatingInductor")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).ShowVariableResistor")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).SimpleTriacCircuit")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).SwitchWithArc")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).ThyristorBehaviourTest")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST("$(prefix).IdealTriacCircuit")[1]) == OMFrontend.Main.FLAT_MODEL
end
