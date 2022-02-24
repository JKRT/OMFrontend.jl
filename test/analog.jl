@testset "Analog.Basic" begin
  prefix = "Modelica.Electrical.Analog.Basic"
  @test typeof(flattenModelInMSL("$(prefix).Ground")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL("$(prefix).Resistor")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL("$(prefix).HeatingResistor")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL("$(prefix).Conductor")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL("$(prefix).Capacitor")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL("$(prefix).Inductor")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL("$(prefix).SaturatingInductor")[1]) == OMFrontend.Main.FLAT_MODEL  #Note that this generates a lot of error messages but does not fail to translate...
  @test typeof(flattenModelInMSL("$(prefix).Transformer")[1]) == OMFrontend.Main.FLAT_MODEL
  #@test typeof(flattenModelInMSL("$(prefix).M_Transformer")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL("$(prefix).Gyrator")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL("$(prefix).EMF")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL("$(prefix).TranslationalEMF")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL("$(prefix).VCV")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL("$(prefix).VCC")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL("$(prefix).CCV")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL("$(prefix).CCC")[1]) == OMFrontend.Main.FLAT_MODEL
end #= End Analog =#
#= Testing some sources in Analog.Sources=#
@testset "Analog.Sources" begin
  prefix = "Modelica.Electrical.Analog.Sources"
  @test typeof(flattenModelInMSL("$(prefix).SignalVoltage")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL("$(prefix).ConstantVoltage")[1]) == OMFrontend.Main.FLAT_MODEL
  #Broken @test typeof(flattenModelInMSL("$(prefix).StepVoltage")[1]) == OMFrontend.Main.FLAT_MODEL
end
