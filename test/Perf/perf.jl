#=
Test script to measure performance.
Not a part of the wider testsuite
=#
using OMFrontend
using Profile
using PProf

# Collect a profile
Profile.clear()

function flattenModelInMSL_TST(modelName::String)
  local MSL_V  = "MSL_4_0_0"
  if !haskey(OMFrontend.LIBRARY_CACHE, MSL_V)
    OMFrontend.initLoadMSL(MSL_Version= MSL_V)
  end
  local libraryAsScoded = OMFrontend.LIBRARY_CACHE["MSL_4_0_0"]
  (FM, cache) = OMFrontend.instantiateSCodeToFM(modelName, libraryAsScoded)
end

#= Precompile j i c=#
flattenModelInMSL_TST("Modelica.Mechanics.MultiBody.Examples.Loops.Engine1a");
#=
Try to flatten an engine model in the multibody library.
=#
@profile flattenModelInMSL_TST("Modelica.Mechanics.MultiBody.Examples.Loops.Engine1a");

PProf.Allocs.pprof()
