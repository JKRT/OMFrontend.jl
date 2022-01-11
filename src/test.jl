import OMFrontend
@time p = OMFrontend.parseFile("../test/Models/MWE.mo")
@time scodeProgram = OMFrontend.translateToSCode(p)
@time (dae, cache) = OMFrontend.instantiateSCodeToDAE("MWE.A", scodeProgram)

