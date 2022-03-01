@info "Testing components of the Modelica standard library"
@testset "Basic use of the MSL tests" begin
  @test begin
    try 
      initLoadMSL()
      true
    catch e
      @error "Failed loading the Modelica Standard Library:" e
      false
    end
  end
  @test true == begin
    res = OMFrontend.flattenModelWithMSL("ElectricalTest.SimpleCircuit", "./MSL_Use/SimpleCircuitMSL.mo")
    res = OMFrontend.toString(first(res))
    println(res)
    true
  end
end
