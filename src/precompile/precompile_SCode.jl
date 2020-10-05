function _precompile_()
    ccall(:jl_generating_output, Cint, ()) == 1 || return nothing
    Base.precompile(Tuple{typeof(Base.push_widen),Array{SCode.ALG_ASSIGN,1},SCode.ALG_IF})
end
