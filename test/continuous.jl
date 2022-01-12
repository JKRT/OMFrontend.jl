  @testset "Continuous" begin
    @test typeof(flattenModelInMSL("Modelica.Blocks.Continuous.Integrator")[1]) == OMFrontend.Main.FLAT_MODEL
    #@test typeof(flattenModelInMSL("Modelica.Blocks.Continuous.LimIntegrator")[1]) == OMFrontend.Main.FLAT_MODEL
    #@test typeof(flattenModelInMSL("Modelica.Blocks.Continuous.Derivative")[1]) == OMFrontend.Main.FLAT_MODEL
    #@test typeof(flattenModelInMSL("Modelica.Blocks.Continuous.FirstOrder")[1]) == OMFrontend.Main.FLAT_MODEL
    #@test typeof(flattenModelInMSL("Modelica.Blocks.Continuous.SecondOrder")[1]) == OMFrontend.Main.FLAT_MODEL
    #@test typeof(flattenModelInMSL("Modelica.Blocks.Continuous.PI")[1]) == OMFrontend.Main.FLAT_MODEL
    #@test typeof(flattenModelInMSL("Modelica.Blocks.Continuous.PID")[1]) == OMFrontend.Main.FLAT_MODEL
    #@test typeof(flattenModelInMSL("Modelica.Blocks.Continuous.LimPID")[1]) == OMFrontend.Main.FLAT_MODEL
    #@test typeof(flattenModelInMSL("Modelica.Blocks.Continuous.TransferFunction")[1]) == OMFrontend.Main.FLAT_MODEL
    #@test typeof(flattenModelInMSL("Modelica.Blocks.Continuous.StateSpace")[1]) == OMFrontend.Main.FLAT_MODEL
    #@test typeof(flattenModelInMSL("Modelica.Blocks.Continuous.Der")[1]) == OMFrontend.Main.FLAT_MODEL
    #@test typeof(flattenModelInMSL("Modelica.Blocks.Continuous.LowpassButterworth")[1]) == OMFrontend.Main.FLAT_MODEL
    # @test typeof(flattenModelInMSL("Modelica.Blocks.Continuous.CriticalDamping")[1]) == OMFrontend.Main.FLAT_MODEL
    #@test typeof(flattenModelInMSL("Modelica.Blocks.Continuous.Filter")[1]) == OMFrontend.Main.FLAT_MODEL
  end
