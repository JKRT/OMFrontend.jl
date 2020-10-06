function _precompile_()
    ccall(:jl_generating_output, Cint, ()) == 1 || return nothing
    Base.precompile(Tuple{typeof(Base.require),Module,Symbol})
    Base.precompile(Tuple{typeof(get!),Type{Array{Function,1}},Dict{Base.PkgId,Array{Function,1}},Base.PkgId})
    Base.precompile(Tuple{typeof(joinpath),String,String,Vararg{String,N} where N})
end
