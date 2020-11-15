import HybridDAEParser
#using BenchmarkTools
p = HybridDAEParser.parseFile("example.mo")
scodeProgram = HybridDAEParser.translateToSCode(p)
@debug "Translation to SCode"
@debug "SCode -> DAE"
(dae, cache) = HybridDAEParser.instantiateSCodeToDAE("HelloWorld", scodeProgram)
@debug "After DAE Translation"
@show dae
