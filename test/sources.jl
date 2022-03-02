@testset "Sources" begin
  @test typeof(flattenModelInMSL("Modelica.Blocks.Sources.RealExpression")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL("Modelica.Blocks.Sources.IntegerExpression")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL("Modelica.Blocks.Sources.BooleanExpression")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL("Modelica.Blocks.Sources.Clock")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL("Modelica.Blocks.Sources.Constant")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL("Modelica.Blocks.Sources.Step")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL("Modelica.Blocks.Sources.Ramp")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL("Modelica.Blocks.Sources.Sine")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL("Modelica.Blocks.Sources.Cosine")[1]) == OMFrontend.Main.FLAT_MODEL
  @test typeof(flattenModelInMSL("Modelica.Blocks.Sources.ExpSine")[1]) == OMFrontend.Main.FLAT_MODEL
end
