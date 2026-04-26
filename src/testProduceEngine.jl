if Sys.iswindows()
  outputFile = "/Users/johti17/Desktop/TMP/"
else
  outputFile = "./"
end

fileName = "./Mechanics.mo"
modelName = "MechanicsExamples.PendulumTest"
@time res = OMFrontend.flattenModelWithMSL(modelName, fileName)
OMFrontend.writeFlatModelicaToFile(res[1], OMFrontend.cacheToFunctionList(res[2]);
                                   removeQuotes = true,
                                   fileName = string(outputFile, "PendulumOutput.mo"))

modelName = "MechanicsExamples.DoublePendulumTest"
@time res = OMFrontend.flattenModelWithMSL(modelName, fileName)
OMFrontend.writeFlatModelicaToFile(res[1], OMFrontend.cacheToFunctionList(res[2]);
                                   removeQuotes = true,
                                   fileName = string(outputFile, "DoublePendulumTest.mo"))


modelName = "MechanicsExamples.Engine1aTest"
@time res = OMFrontend.flattenModelWithMSL(modelName, fileName)
OMFrontend.writeFlatModelicaToFile(res[1], OMFrontend.cacheToFunctionList(res[2]);
                                   removeQuotes = true,
                                   fileName = string(outputFile, "Engine1aOutput.mo"))

# modelName = "MechanicsExamples.RobotTestOneAxis"
# @time res = OMFrontend.flattenModelWithMSL(modelName, fileName;MSL_Version = "MSL:4.0.0",)
# OMFrontend.writeFlatModelicaToFile(res[1], OMFrontend.cacheToFunctionList(res[2]);
#                                    removeQuotes = true,
#                                    fileName = string(outputFile, "OneAxis.mo"))

# modelName = "MechanicsExamples.RobotTest"
# @time res = OMFrontend.flattenModelWithMSL(modelName, fileName;MSL_Version = "MSL:4.0.0",)
# OMFrontend.writeFlatModelicaToFile(res[1], OMFrontend.cacheToFunctionList(res[2]);
#                                    removeQuotes = true,
#                                    fileName = string(outputFile, "RobotTest.mo"))
