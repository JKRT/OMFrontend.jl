@test typeof(flattenModelInMSL_TST("Modelica.Blocks.Math.Abs")[1]) == OMFrontend.Main.FLAT_MODEL
@test typeof(flattenModelInMSL_TST("Modelica.Blocks.Math.Sign")[1]) == OMFrontend.Main.FLAT_MODEL
@test typeof(flattenModelInMSL_TST("Modelica.Blocks.Math.Sin")[1]) == OMFrontend.Main.FLAT_MODEL
@test typeof(flattenModelInMSL_TST("Modelica.Blocks.Math.Sqrt")[1]) == OMFrontend.Main.FLAT_MODEL
