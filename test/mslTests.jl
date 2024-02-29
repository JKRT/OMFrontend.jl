"""
Test specific function to load the Modelica Standard Library (The MSL)
"""
function flattenModelInMSL_TST(modelName::String; MSL_V  = "MSL_3_2_3")
  if !haskey(OMFrontend.LIBRARY_CACHE, MSL_V)
    OMFrontend.initLoadMSL(MSL_Version= MSL_V)
  end
  local libraryAsScoded = OMFrontend.LIBRARY_CACHE[MSL_V]
  (FM, cache) = OMFrontend.instantiateSCodeToFM(modelName, libraryAsScoded)
end

#= Try to load the msl=#
@test begin
  try
    OMFrontend.initLoadMSL(;MSL_Version="MSL_3_2_3")
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
  include("sources.jl")
  @testset "Discrete" begin
    #@test typeof(flattenModelInMSL_TST("Modelica.Blocks.Discrete.Sampler")[1]) == OMFrontend.Main.FLAT_MODEL
  end
  @testset "Math" begin
    include("math.jl")
  end
  @testset "Mechanics" begin
    @info "Testing Mechanics.Rotational"
    include("rotational.jl")
    @info "Testing Mechanics.Translational"
    include("translational.jl")
    @info "Testing Modelica.Mechanics.MultiBody"
    include("multibody.jl")
  end #= End Mechanics=#
  @testset "Electrical" begin
    @info "Testing Modelica.Electrical.Analog"
    include("analog.jl")
    @info "Testing Modelica.Electrical.Batteries"
    #include("batteries.jl")
  end
end
