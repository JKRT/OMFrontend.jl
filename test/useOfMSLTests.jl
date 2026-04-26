@info "Testing components of the Modelica standard library"

@testset "MSL Loading tests" begin
  @test begin
    try
      OMFrontend.initLoadMSL(;MSL_Version = "MSL_3_2_3")
      true
    catch e
      @error "Failed loading the Modelica Standard Library: V3.2.3" e
      false
    end
  end
  #= Try 4.0.0 =#
  @test begin
    try
#      OMFrontend.initLoadMSL(;MSL_Version = "MSL_4_0_0")
      true
    catch e
      @error "Failed loading the Modelica Standard Library: V4.0.0:" e
      false
    end
  end

  @test true == begin
    res = OMFrontend.flattenModelWithMSL("ElectricalTest.SimpleCircuit", "./MSL_Use/SimpleCircuitMSL.mo")
    res = OMFrontend.toString(first(res))
    println(res)
    true
  end
  @test true == begin
    res = OMFrontend.flattenModelWithMSL("TransmissionLine", "./MSL_Use/TransmissionLine.mo")
    res = OMFrontend.toString(first(res))
    println(res)
    true
  end
end
