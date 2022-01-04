@test typeof(OMFrontend.parseFile("./VSS/BreakingPendulum.mo")) == Absyn.PROGRAM
@test typeof(OMFrontend.parseFile("./VSS/SimpleSingleMode.mo")) == Absyn.PROGRAM
res = flattenFM("SimpleSingleMode", "./VSS/SimpleSingleMode.mo")
res = flattenFM("BreakingPendulum", "./VSS/BreakingPendulum.mo")
