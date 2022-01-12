import OMFrontend
import OM
# println(OM.toString(OMFrontend.flattenModelInMSL("Modelica.Blocks.Examples.PID_Controller")[1]))
@time p = OMFrontend.parseFile("../test/Models/PID_ControllerTotal.mo")
@time scodeProgram = OMFrontend.translateToSCode(p)
@time (dae, cache) = OMFrontend.instantiateSCodeToDAE("Modelica.Blocks.Examples.PID_Controller", scodeProgram)
