#= Quick sanity test for DAE. Does the Frontend compile?=#
function testPassIfNoException(str1::String, str2::String)
  @test try
    flatten(str1, str2)
    true
  catch e
    @error "Failed to flatten model: $(str1) in file $(str2):" e
    throw(e)
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
testPassIfNoException("TestImport.A", "./Models/TestImport.mo")
testPassIfNoException("TestEnum", "./Models/TestEnum.mo")
testPassIfNoException("SimpleClock", "./Models/SimpleClock.mo")

