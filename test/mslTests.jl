function initLoadMSL()
  @info "Loading the MSL"
  @time OMFrontend.loadMSL()
  @info "Loaded MSL successfully"
end

function flattenModelInMSL(modelName::String)
  if !haskey(OMFrontend.LIBRARY_CACHE, "MSL")
    throw("MSL not loaded")
  end
  local libraryAsScoded = OMFrontend.LIBRARY_CACHE["MSL"]
  (FM, cache) = OMFrontend.instantiateSCodeToFM(modelName, libraryAsScoded)
end

#= Try to load the msl=#
@test begin
  try 
    initLoadMSL()
    true
  catch e
    @error "Failed loading the Modelica Standard Library:" e
    false
  end
end
  
#= Simple check that we can flatten the models without exceptions =#
@info "Testing components of the Modelica standard library"
@testset "Modelica Blocks" begin
  @info "Testing Modelica.Blocks.Continuous"
  include("continuous.jl")
  @testset "Discrete" begin
    #@test typeof(flattenModelInMSL("Modelica.Blocks.Discrete.Sampler")[1]) == OMFrontend.Main.FLAT_MODEL
  end
  @testset "Math" begin
    #@test typeof(flattenModelInMSL("Modelica.Blocks.Math.Abs")[1]) == OMFrontend.Main.FLAT_MODEL
    #@test typeof(flattenModelInMSL("Modelica.Blocks.Math.Sign")[1]) == OMFrontend.Main.FLAT_MODEL
    #@test typeof(flattenModelInMSL("Modelica.Blocks.Math.Sin")[1]) == OMFrontend.Main.FLAT_MODEL
    #@test typeof(flattenModelInMSL("Modelica.Blocks.Math.Sqrt")[1]) == OMFrontend.Main.FLAT_MODEL
  end
  @testset "Mechanics" begin
    @info "Testing Mechanics.Rotational"
    include("rotational.jl")
    @info "Testing Mechanics.Translational"
    include("translational.jl")
  end #= End Mechanics=#
  @testset "Electrics" begin
    @info "Testing Modelica.Electrical.Analog"
    include("analog.jl")
  end
end
