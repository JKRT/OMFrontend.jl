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
@info "Write the final result to a file for testing..."
modelName = "MechanicsExamples.Engine1aTest"
@time res = OMFrontend.flattenModelWithMSL(modelName, fileName)
OMFrontend.writeFlatModelicaToFile(res[1], OMFrontend.cacheToFunctionList(res[2]);
                                   removeQuotes = true,
                                   fileName = string(outputFile, "Engine1aOutput.mo"))
