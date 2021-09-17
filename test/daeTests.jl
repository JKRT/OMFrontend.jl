#= Quick sanity test for DAE. Does the Frontend compile?=#
function testPassIfNoException(str1::String, str2::String)
  @test try
    flatten(str1, str2)
    true
  catch e
    false
  end
end

testPassIfNoException("HelloWorld", "./Models/HelloWorld.mo")
testPassIfNoException("LotkaVolterra", "./Models/LotkaVolterra.mo")
testPassIfNoException("Robertsson", "./Models/Robertsson.mo")
testPassIfNoException("SimpleCircuit", "./Models/SimpleCircuit.mo")
testPassIfNoException("SimpleMechanicalSystem", "./Models/SimpleMechanicalSystem.mo")
testPassIfNoException("Influenza", "./Models/Influenza.mo")
testPassIfNoException("Casc6", "./Models/Casc6.mo")
