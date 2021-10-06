#= This sets of test simply try to lower the input to SCode. No validation of the result.=#
@test try
  parseAndLowerToScode("HelloWorld", "./Models/HelloWorld.mo")
  true
catch e
  throw(e)
  false
end
@test try
  parseAndLowerToScode("LotkaVolterra", "./Models/LotkaVolterra.mo")
  true
catch e
  throw(e)
  false
end
@test try
  parseAndLowerToScode("Robertsson", "./Models/Robertsson.mo")
  true
catch e
  throw(e)
  false
end
@test try
  parseAndLowerToScode("SimpleCircuit", "./Models/SimpleCircuit.mo")
  true
catch e
  throw(e)
  false
end
@test try
  parseAndLowerToScode("SimpleMechanicalSystem", "./Models/SimpleMechanicalSystem.mo")
  true
catch e
  throw(e)
  false
end
@test try
  parseAndLowerToScode("Influenza", "./Models/Influenza.mo")
  true
catch e
  throw(e)
  false
end
