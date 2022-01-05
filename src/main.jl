
"""
  Main module.
  This module provides the entry to the translated code and associated tweaks and quirks. 
"""
module Main
#= We also use it at the top level =#

using MetaModelica
using ExportAll


import Absyn
import SCode
import DAE
import ListUtil
import ArrayUtil
include("./Util/Pointer.jl")
include("./Util/System.jl")
include("./Util/Corba.jl")
include("./Util/Gettext.jl")
include("./Util/Error.jl")
include("./Util/ErrorExt.jl")
import .P_Pointer
Pointer = P_Pointer.Pointer
include("./Util/Mutable.jl")
include("./Util/BaseAvlSet.jl")
include("./Util/BaseAvlTree.jl")
include("./Util/BaseHashTable.jl")
include("./Util/Global.jl")
include("./Util/Settings.jl")
include("./Util/Print.jl")
include("./Util/Util.jl")
include("./Util/StringUtil.jl")
include("./Util/Flags.jl")
include("./Util/FlagsUtil.jl")
include("./Util/IOStreamExt.jl")
include("./Util/IOStream.jl")
include("./AbsynUtil.jl")
include("./SCodeUtil.jl")
include("./AbsynToSCode.jl")
#=Utility for frontend=#
include("./FrontendUtil/Prefix.jl")


#= Disable type inference for this module =#
if isdefined(Base, :Experimental) && isdefined(Base.Experimental, Symbol("@compiler_options"))
  # @info "Setting compiler options.."
  # @info "Base.Experimental.@compiler_options compile=all optimize=3 infer=false"
  @eval Base.Experimental.@compiler_options compile=min optimize=3 infer=false
else
  throw("@compiler_options is not available.\n
         This package only works for a version of Julia with @compiler_options")
end
#= Include interfaces and aliases New Frontend=#
include("./FrontendInterfaces/NFInterfaces.jl")
include("./FrontendInterfaces/NFAlias.jl")
#= Other modules =#
include("./NewFrontend/NFType.jl")
include("./NewFrontend/NFComplexType.jl")
include("./NewFrontend/NFPrefixes.jl")
include("./NewFrontend/NFComponent.jl")
include("./NewFrontend/NFInstNode.jl")
include("./NewFrontend/NFSections.jl")
include("./NewFrontend/NFRecord.jl")
include("./NewFrontend/NFOperatorOverloading.jl")
include("./NewFrontend/NFCeval.jl")
include("./NewFrontend/NFEquation.jl")
include("./NewFrontend/NFTyping.jl")
include("./NewFrontend/NFInst.jl")
include("./NewFrontend/NFAlgorithm.jl")
include("./NewFrontend/NFStatement.jl")
include("./NewFrontend/NFBinding.jl")
include("./NewFrontend/NFVariable.jl")
include("./NewFrontend/NFFlatModel.jl")
include("./NewFrontend/NFExpression.jl")
include("./NewFrontend/NFConnector.jl")
include("./NewFrontend/NFConnections.jl")
include("./NewFrontend/NFConnection.jl")

include("./NewFrontend/NFConnectionSets.jl")

include("./NewFrontend/NFCardinalityTable.jl")

include("./NewFrontend/NFConnectEquations.jl")
import .NFCardinalityTable
include("./NewFrontend/NFFunctionDerivative.jl")

include("./NewFrontend/NFFunction.jl")

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
#import ..NFBuiltin
include("./NewFrontend/BindingExpression.jl")

include("./NewFrontend/NFDimension.jl")

include("./NewFrontend/NFBuiltinCall.jl")

include("./NewFrontend/NFCall.jl")

include("./NewFrontend/NFOperator.jl")

include("./NewFrontend/NFTypeCheck.jl")

include("./NewFrontend/NFExpandableConnectors.jl")

include("./NewFrontend/NFEvalConstants.jl")

include("./NewFrontend/NFExpandExp.jl")

#= TODO: NOT IN USE=#
#include("./NewFrontend/NFUnit.jl")
#include("./NewFrontend/NFHashTableCrToUnit.jl")
#include("./NewFrontend/NFUnitCheck.jl")
#=################# =#
include("./NewFrontend/NFSimplifyModel.jl")
include("./NewFrontend/NFSimplifyExp.jl")
include("./NewFrontend/NFPackage.jl")
#= Model verification =#
include("./NewFrontend/NFVerifyModel.jl")
include("./NewFrontend/NFInline.jl")
#=  Builtin functions =#
include("./NewFrontend/NFBuiltinFuncs.jl")
include("./NewFrontend/NFRangeIterator.jl")
end
