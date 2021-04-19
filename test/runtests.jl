#=
  Author: John Tinnerholm johti17@liu.se

Current Status:

Works when including the file in the terminal. Does not seem to work when run via tests.

=#

import HybridDAEParser
import Absyn
import SCode
import DAE

using Test

function flatten(modelName::String, modelFile::String)
  p = HybridDAEParser.parseFile(modelFile)
  scodeProgram = HybridDAEParser.translateToSCode(p)
  (dae, cache) = HybridDAEParser.instantiateSCodeToDAE(modelName, scodeProgram)
end

function parseAndLowerToScode(modelName::String, modelFile::String)
  p = HybridDAEParser.parseFile(modelFile)
  scodeProgram = HybridDAEParser.translateToSCode(p)
end

@testset "Absyn -> SCode test" begin
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
end

@testset "SCode -> DAE test" begin

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
  
end
