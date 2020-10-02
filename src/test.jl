import HybridDAEParser
#using BenchmarkTools
p = HybridDAEParser.parseFile("example.mo")
scodeProgram = HybridDAEParser.translateToSCode(p)
@info "Translation to SCode"
@info "SCode -> DAE"
(dae, cache) = HybridDAEParser.instantiateSCodeToDAE("HelloWorld", scodeProgram)
@info "After DAE Translation"
@show dae
