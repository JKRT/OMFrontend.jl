#= Testing a selection of basic components =#
@testset "Batteries.ParameterRecords" begin
  prefix = "Modelica.Electrical.Batteries.ParameterRecords"
  @test typeof(flattenModelInMSL_TST(prefix * ".ExampleData"; MSL_V = "MSL_4_0_0")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST(prefix * ".StackData"; MSL_V = "MSL_4_0_0")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST(prefix * ".CellData"; MSL_V = "MSL_4_0_0")[1]) == OMFrontend.Main.FLAT_MODEL

  prefix = "Modelica.Electrical.Batteries.ParameterRecords.TransientData"
  #@test typeof(flattenModelInMSL_TST(prefix * ".ExampleData"; MSL_V = "MSL_4_0_0")[1]) == OMFrontend.Main.FLAT_MODEL
  #@test typeof(flattenModelInMSL_TST(prefix * ".CellData"; MSL_V = "MSL_4_0_0")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL_TST(prefix * ".RCData"; MSL_V = "MSL_4_0_0")[1]) == OMFrontend.Main.FLAT_MODEL


end #= End Analog =#
