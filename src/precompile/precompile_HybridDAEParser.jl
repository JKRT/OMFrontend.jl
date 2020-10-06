function _precompile_()
    ccall(:jl_generating_output, Cint, ()) == 1 || return nothing
    Base.precompile(Tuple{typeof(HybridDAEParser.runModel),String,String})
    Base.precompile(Tuple{typeof(HybridDAEParser.translateToSCode),Absyn.PROGRAM})
end
