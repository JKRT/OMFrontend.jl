"
  A parser of the Hybrid DAE.
  Parses a textual top-level module description into DAE.
"
module HybridDAEParser

import Absyn
import SCode
import OpenModelicaParser
"
  Main module
"
module Main
#= We also use it at the top level =#

using MetaModelica
using ExportAll

import Absyn
import SCode
include("./Util/Pointer.jl")
import .P_Pointer
Pointer = P_Pointer.Pointer
include("./Util/Mutable.jl")
include("./Util/BaseAvlSet.jl")
include("./Util/BaseAvlTree.jl")
include("./Util/BaseHashTable.jl")
include("./Util/Gettext.jl")
include("./Util/Global.jl")
include("./Util/Flags.jl")
include("./Util/Print.jl")
include("./Util/System.jl")
include("./Util/Util.jl")
include("./AbsynUtil.jl")
include("./SCodeUtil.jl")
include("./AbsynToSCode.jl")
#=Utility for frontend=#
include("./FrontendUtil/Values.jl")
include("./FrontendUtil/ClassInf.jl")
include("./FrontendUtil/Prefix.jl")
include("./FrontendUtil/DAE.jl")

#=New Frontend=#
include("./FrontendInterfaces/NFInterfaces.jl")
include("./NewFrontend/NFType.jl")
include("./NewFrontend/NFComplexType.jl")
include("./NewFrontend/NFPrefixes.jl")
include("./NewFrontEnd/NFComponent.jl")
include("./NewFrontend/NFInstNode.jl")
include("./NewFrontend/NFSections.jl")
include("./NewFrontend/NFRecord.jl")
include("./NewFrontend/NFOperatorOverloading.jl")
include("./NewFrontend/NFCeval.jl")
include("./NewFrontend/NFTyping.jl")
include("./NewFrontend/NFInst.jl")
include("./NewFrontend/NFEquation.jl")
include("./NewFrontend/NFStatement.jl")
include("./NewFrontend/NFBinding.jl")
include("./NewFrontend/NFVariable.jl")
include("./NewFrontend/NFFlatModel.jl")
include("./NewFrontend/NFConnector.jl")
include("./NewFrontend/NFConnections.jl")
include("./NewFrontend/NFCardinalityTable.jl")
import .NFCardinalityTable
include("./NewFrontend/NFConnectionSets.jl")
include("./NewFrontend/NFFunctionDerivative.jl")
include("./NewFrontend/NFFunction.jl")
include("./NewFrontend/NFExpression.jl")
include("./NewFrontend/NFBinding.jl")
include("./NewFrontend/NFSubscript.jl")
include("./NewFrontend/NFFlatten.jl")
include("./NewFrontend/NFConvertDAE.jl")
include("./NewFrontend/NFRestriction.jl")
include("./NewFrontend/NFClass.jl")
include("./NewFrontend/NFImport.jl")
include("./NewFrontend/NFModifier.jl")
include("./NewFrontend/NFClassTree.jl")
include("./NewFrontend/NFLookup.jl")
include("./NewFrontend/NFLookupState.jl")
include("./NewFrontend/NFComponentRef.jl")
@exportAll
include("./NewFrontend/NFBuiltin.jl")
import ..NFBuiltin
include("./NewFrontend/BindingExpression.jl")
include("./NewFrontend/NFDimension.jl")
include("./NewFrontend/NFCall.jl")
include("./NewFrontend/NFOperator.jl")
include("./NewFrontend/NFTypeCheck.jl")

include("./NewFrontend/NFExpandableConnectors.jl")
end

function parseFile(file::String)::Absyn.Program
  return OpenModelicaParser.parseFile(file)
end

function translateToSCode(inProgram::Absyn.Program)::SCode.Program
  return Main.AbsynToSCode.translateAbsyn2SCode(inProgram)
end

"
  Instantiates a SCode program.
"
function instSCode(inProgram::SCode.Program)
end

"
  Instantiates and translates to DAE.
"
function instantiateSCodeToDAE(elementToInstantiate::String, inProgram::SCode.Program)
  Main.instClassInProgram(Absyn.IDENT(elementToInstantiate), inProgram)
end

function testSpin()
    p = parseFile("./src\\example.mo")
    scodeProgram = translateToSCode(p)
    @info "Translation to SCode"
    @info "SCode -> DAE"
    (dae, cache) = instantiateSCodeToDAE("HelloWorld", scodeProgram)
    @info "After DAE Translation"
end

end # module
