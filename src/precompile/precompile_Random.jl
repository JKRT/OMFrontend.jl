function _precompile_()
    ccall(:jl_generating_output, Cint, ()) == 1 || return nothing
    Base.precompile(Tuple{typeof(Random.shuffle!),Random.MersenneTwister,Array{Symbol,1}})
end
