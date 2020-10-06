using SnoopCompile
precompilestuff = @snoopi tmin=0.01 include("test.jl")
pc = SnoopCompile.parcel(precompilestuff)
SnoopCompile.write("./precompile", pc)
