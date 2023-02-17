"
  An experimental Julia frontend for the Modelica language
"
module OMFrontend

using MetaModelica

import Absyn
import SCode
import OMParser


#= Cache for NFModelicaBuiltin. We only use the result once! =#
"""
Cache for NFModelicaBuiltin.
This cache is initialized when the module is loaded.
"""
const NFModelicaBuiltinCache = Dict()

"""
This cache contains various instantiated libraries for later use.
"""
const LIBRARY_CACHE = Dict()

"""
  This function "precompiles" some of the runtime Modelica libraries.
  While it results in some latency when importing OMFrontend, subsequent
  use of OMFrontend to parse and work with Modelica files is faster.
TODO:
Improving the speed of precompilation would improve the feel of this package
by a lot.
"""
function __init__()
  packagePath = dirname(realpath(Base.find_package("OMFrontend")))
  packagePath *= "/.."
  pathToTest = packagePath * "/test/Models/HelloWorld.mo"
  p = OMParser.parseFile(pathToTest, 1)
  s = Main.AbsynToSCode.translateAbsyn2SCode(p)
  Main.Global.initialize()
  # make sure we have all the flags loaded!
  #  Main.Flags.new(Flags.emptyFlags)
  builtinSCode = NFModelicaBuiltinCache["NFModelicaBuiltin"]
  program = listAppend(builtinSCode, s)
  path = Main.AbsynUtil.stringPath("HelloWorld")
  res1 = Main.instClassInProgram(path, program)
  return nothing
end

include("main.jl")

function parseFile(file::String, acceptedGram::Int64 = 1)::Absyn.Program
  return OMParser.parseFile(file, acceptedGram)
end

function translateToSCode(inProgram::Absyn.Program)::SCode.Program
  return Main.AbsynToSCode.translateAbsyn2SCode(inProgram)
end

"""
  Instantiates a SCode program.
"""
function instSCode(inProgram::SCode.Program)
end


"""
  Instantiates and translates to DAE.
  The element to instantiate should be provided in the following format:
  <component>.<component_1>.<component_2>...
"""
function instantiateSCodeToDAE(elementToInstantiate::String, inProgram::SCode.Program)
  # initialize globals
  Main.Global.initialize()
  # make sure we have all the flags loaded!
  #Main.Flags.new(Flags.emptyFlags)
  local builtinSCode = NFModelicaBuiltinCache["NFModelicaBuiltin"]
  local program = listAppend(builtinSCode, inProgram)
  local path = Main.AbsynUtil.stringPath(elementToInstantiate)
  Main.instClassInProgram(path, program)
end

"""
  Instantiates and translates to the flat model representation
  The element to instantiate should be provided in the following format:
  <component>.<component_1>.<component_2>...
"""
function instantiateSCodeToFM(elementToInstantiate::String, inProgram::SCode.Program)
  # initialize globals
  Main.Global.initialize()
  # make sure we have all the flags loaded!
#  Main.Flags.new(Flags.emptyFlags)
  local builtinSCode = NFModelicaBuiltinCache["NFModelicaBuiltin"]
  local program = listReverse(listAppend(builtinSCode, inProgram))
  local path = Main.AbsynUtil.stringPath(elementToInstantiate)
  Main.instClassInProgramFM(path, program)
end

"""
  @author: johti17
  Loads the Modelica Standard Library (MSL).
  Adds the Modelica standard library to the library cache.
"""
function loadMSL()
  if ! haskey(LIBRARY_CACHE, "MSL")
    Main.Global.initialize()
    #= Find the MSL =#
    local packagePath = dirname(realpath(Base.find_package("OMFrontend")))
    local packagePath *= "/.."
    local pathToLib = packagePath * "/lib/Modelica/msl.mo"
    local p = parseFile(pathToLib)
    #= Translate it to SCode =#
    local scodeMSL = OMFrontend.translateToSCode(p)
    LIBRARY_CACHE["MSL"] = scodeMSL
  end
end

"""
  Prints the DAE representation to a file
"""
function exportDAERepresentationToFile(fileName::String, contents::String)
  local fdesc = open(fileName, "w")
  write(fdesc, contents)
  close(fdesc)
end

"""
  ```toString(model::FlatModel)```
    Converts the flat model representation to a Julia String, the extra \\n are replaced with \n
"""
function toString(model::Main.FlatModel)
  local res = Main.toString(model)
  local res = replace(res, "\\n" => "\n")
  return res
end

"""
  Overload the Julia to string function
"""
function Base.string(model::Main.FlatModel)
  return toString(model::Main.FlatModel)
end

"""
  Converts a function tree to a string
"""
function Base.string(ft::Main.FunctionTreeImpl.LEAF)
  fLst = OMFrontend.Main.FunctionTreeImpl.toList(ft)
  local buffer = IOBuffer()
  for (_, v) in fLst
    println(buffer, OMFrontend.Main.toFlatString(v))
  end
  return replace(String(take!(buffer)), "\\n" => "\n")
end

function exportSCodeRepresentationToFile(fileName::String, contents::List{SCode.CLASS})
  local fdesc = open(fileName, "w")
  local processedContents = replace(string(contents), "," => ",\n")
  write(fdesc, processedContents)
  close(fdesc)
end
#=
  This is done during precompilation of the package
  to cache precompile versions of many methods.
=#
if ccall(:jl_generating_output, Cint, ()) == 1
  let
    #= Step one. Load the built in library =#
    @info "Precompiling builtin libraries..."
    if ! haskey(NFModelicaBuiltinCache, "NFModelicaBuiltin")
      #= Locate the external libraries =#
      packagePath = dirname(realpath(Base.find_package("OMFrontend")))
      packagePath *= "/.."
      pathToLib = packagePath * "/lib/NFModelicaBuiltin.mo"
      #= The external C stuff can be a bit flaky.. =#
      GC.enable(false)
      p = parseFile(pathToLib, 2 #== MetaModelica ==#)
      s = translateToSCode(p)
      NFModelicaBuiltinCache["NFModelicaBuiltin"] = s
      #=Enable GC again.=#
      GC.enable(true)
    end
    @info "Builtin libraries successfully precompiled!"
    @info "Initial compiler module interfaces are compiled!"
  end
end

function initLoadMSL()
  @info "Loading the MSL"
  @time loadMSL()
  @info "Loaded MSL successfully"
end

"""
  This function loads the MSL s.t it can be used for models.
"""
function flattenModelInMSL(modelName::String)
  if !haskey(LIBRARY_CACHE, "MSL")
    initLoadMSL()
  end
  local libraryAsScoded = LIBRARY_CACHE["MSL"]
  (FM, cache) = instantiateSCodeToFM(modelName, libraryAsScoded)
end

"""
  This function flattens a model with the MSL.
"""
function flattenModelWithMSL(modelName::String, fileName::String)
  if !haskey(LIBRARY_CACHE, "MSL")
    initLoadMSL()
  end
  local lib = LIBRARY_CACHE["MSL"]
  local absynProgram = parseFile(fileName)
  local sCodeProgram = translateToSCode(absynProgram)
  builtin = NFModelicaBuiltinCache["NFModelicaBuiltin"]
  program = listReverse(listAppend(builtin, sCodeProgram))
  program = listReverse(listAppend(lib, sCodeProgram))
  (FM, cache) = instantiateSCodeToFM(modelName, program)
end

end # module
