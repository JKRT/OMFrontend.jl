  @testset "Continuous" begin
    @test typeof(flattenModelInMSL_TST("Modelica.Blocks.Continuous.Integrator")[1]) == OMFrontend.Main.FLAT_MODEL
    #@test typeof(flattenModelInMSL_TST("Modelica.Blocks.Continuous.LimIntegrator")[1]) == OMFrontend.Main.FLAT_MODEL
    #@test typeof(flattenModelInMSL_TST("Modelica.Blocks.Continuous.Derivative")[1]) == OMFrontend.Main.FLAT_MODEL
    #@test typeof(flattenModelInMSL_TST("Modelica.Blocks.Continuous.FirstOrder")[1]) == OMFrontend.Main.FLAT_MODEL
    #@test typeof(flattenModelInMSL_TST("Modelica.Blocks.Continuous.SecondOrder")[1]) == OMFrontend.Main.FLAT_MODEL
    #@test typeof(flattenModelInMSL_TST("Modelica.Blocks.Continuous.PI")[1]) == OMFrontend.Main.FLAT_MODEL
    #@test typeof(flattenModelInMSL_TST("Modelica.Blocks.Continuous.PID")[1]) == OMFrontend.Main.FLAT_MODEL
    #@test typeof(flattenModelInMSL_TST("Modelica.Blocks.Continuous.LimPID")[1]) == OMFrontend.Main.FLAT_MODEL
    #@test typeof(flattenModelInMSL_TST("Modelica.Blocks.Continuous.TransferFunction")[1]) == OMFrontend.Main.FLAT_MODEL
    #@test typeof(flattenModelInMSL_TST("Modelica.Blocks.Continuous.StateSpace")[1]) == OMFrontend.Main.FLAT_MODEL
    @test typeof(flattenModelInMSL_TST("Modelica.Blocks.Continuous.Der")[1]) == OMFrontend.Main.FLAT_MODEL
    #@test typeof(flattenModelInMSL_TST("Modelica.Blocks.Continuous.LowpassButterworth")[1]) == OMFrontend.Main.FLAT_MODEL
    #@test typeof(flattenModelInMSL_TST("Modelica.Blocks.Continuous.CriticalDamping")[1]) == OMFrontend.Main.FLAT_MODEL
    #@test typeof(flattenModelInMSL_TST("Modelica.Blocks.Continuous.Filter")[1]) == OMFrontend.Main.FLAT_MODEL
  end
