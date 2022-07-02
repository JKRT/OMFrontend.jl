using Revise
import OMFrontend
modelFile = "./Dynawo/Dynawo.mo"
modelName = "Dynawo.Examples.SMIB.SMIBStepPm" 
#modelName = "Dynawo.Electrical.Sources.Converter"
#modelName = "Modelica.Blocks.Sources.RealExpression"
#mode = OMBackend.MTK_MODE
p = OMFrontend.parseFile(modelFile);
scodeProgram = OMFrontend.translateToSCode(p);
(FM, cache) = OMFrontend.instantiateSCodeToFM(modelName, scodeProgram);
OMFrontend.toString(FM)
