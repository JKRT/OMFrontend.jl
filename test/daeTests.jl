#= Quick sanity test for DAE. Does the Frontend compile?=#
@test try
    flatten("HelloWorld", "./Models/HelloWorld.mo")
    true
  catch e
    throw(e)
    false
  end

  @test try
    flatten("LotkaVolterra", "./Models/LotkaVolterra.mo")
    true
  catch e
    throw(e)
    false
  end

  @test try
    flatten("Robertsson", "./Models/Robertsson.mo")
    true
  catch e
    throw(e)
    false
  end

  @test try
    flatten("SimpleCircuit", "./Models/SimpleCircuit.mo")
    true
  catch e
    throw(e)
    false
  end

  @test try
    flatten("SimpleMechanicalSystem", "./Models/SimpleMechanicalSystem.mo")
    true
  catch e
    throw(e)
    false
  end

  @test try
    flatten("Influenza", "./Models/Influenza.mo")
    true
  catch e
    throw(e)
    false
  end
  
  @test try
    flatten("Casc6", "./Models/Casc6.mo")
    true
  catch e
    throw(e)
    false
  end
