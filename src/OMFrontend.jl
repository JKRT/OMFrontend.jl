"""
  An experimental Modelica frontend written in the Julia Language
"""
module OMFrontend

using MetaModelica

import Absyn
import SCode
import OMParser
import PrecompileTools
import Distributed

#=
TODO:
  Investigate why flags have to be loaded several times.
  both in __init__() and in main.jl
=#

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
  Main.FlagsUtil.loadFlags()
  builtinSCode = NFModelicaBuiltinCache["NFModelicaBuiltin"]
  program = listAppend(builtinSCode, s)
  path = Main.AbsynUtil.stringPath("HelloWorld")
  res1 = Main.instClassInProgram(path, program)
  return nothing
end

include("main.jl")

"""
Parse a file, returns the syntax tree.
"""
function parseFile(file::String, acceptedGram::Int64 = 1)::Absyn.Program
  return OMParser.parseFile(file, acceptedGram)
end

"""
  Translate the Syntax tree to the SCode intermediate representation
"""
function translateToSCode(inProgram::Absyn.Program)::SCode.Program
  return Main.AbsynToSCode.translateAbsyn2SCode(inProgram)
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
function instantiateSCodeToFM(elementToInstantiate::String,
                              inProgram::SCode.Program; scalarize = true)
  # initialize globals
  Main.Global.initialize()
  # make sure we have all the flags loaded!
  #  Main.Flags.new(Flags.emptyFlags)
  Main.FlagsUtil.set(Main.Flags.NF_SCALARIZE, scalarize)
  local builtinSCode = NFModelicaBuiltinCache["NFModelicaBuiltin"]
  local program = listReverse(listAppend(builtinSCode, inProgram))
  local path = Main.AbsynUtil.stringPath(elementToInstantiate)
  (flat_model, funcs, inst_cls) = Main.instClassInProgramFM(path, program)
  return (flat_model, funcs)
end

"""
```
  exportDAERepresentationToFile(fileName::String, contents::String)
```
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
function Base.string(ft::Main.FunctionTreeImpl.Tree)
  fLst = OMFrontend.Main.FunctionTreeImpl.toList(ft)
  local buffer = IOBuffer()
  for (_, v) in fLst
    println(buffer, OMFrontend.Main.toFlatString(v))
  end
  return replace(String(take!(buffer)), "\\n" => "\n")
end

function toFlatModelica(fm, fLst; printBindingTypes = false)
  return replace(Main.toFlatString(fm, fLst, printBindingTypes), "\\n" => "\n")
end

function writeFlatModelicaToFile(fm, fLst; printBindingtypes = false, fileName)
  local fmStr = toFlatModelica(fm, fLst; printBindingTypes = printBindingtypes)
  write(fileName, fmStr)
  close(fileName)
end

"""
  Converts the function cache represented as a tree where the path is the key into a list of functions
"""
function cacheToFunctionList(cache)
  fLst = OMFrontend.Main.FunctionTreeImpl.toList(cache)
  fv = map(fLst) do kv
    last(kv)
  end
  arrayList(fv)
end

"""
  Dumps the SCode representation of a Modelica model to a file.
"""
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

function initLoadMSL(;MSL_Version = "MSL:3.2.3")
  # For printing
  @info "Loading MSL\n\t Version: $(MSL_Version)"
  MSL_Version = replace(MSL_Version, "." => "_")
  MSL_Version = replace(MSL_Version, ":" => "_")
  @time loadMSL(MSL_Version = MSL_Version)
  @info "MSL successfully Loaded"
end

"""
`function flattenModelWithMSL(modelName::String, fileName::String; MSL_Version = "MSL:3.2.3")`

Returns the flat representation of a modelica model along with the functions used and define by the model.
See the keyword argument for specifying MSL version.
Valid versions are 3.2.3 and 4.0.0.
"""
function flattenModelWithMSL(modelName::String,
                             fileName::String; MSL_Version = "MSL:3.2.3")
  if !haskey(LIBRARY_CACHE, MSL_Version)
    initLoadMSL(MSL_Version = MSL_Version)
  end
  MSL_Version = replace(MSL_Version, "." => "_")
  MSL_Version = replace(MSL_Version, ":" => "_")
  local lib = LIBRARY_CACHE[MSL_Version]
  local absynProgram = parseFile(fileName)
  local sCodeProgram = translateToSCode(absynProgram)
  #= Add builtin function to the program (model) and instantiate it =#
  builtin = NFModelicaBuiltinCache["NFModelicaBuiltin"]
  #program = listReverse(listAppend(sCodeProgram, builtin))
  program = listAppend(sCodeProgram, lib)
  println("Attempting to instantiate..." * modelName)
  (FM, cache) = instantiateSCodeToFM(modelName, program)
end

"""
`flattenModel(modelName::String, fileName::String)`

Returns the flat representation of a modelica model along with the functions used and define by the model.
"""
function flattenModel(modelName::String, fileName::String)
  local absynProgram = parseFile(fileName)
  local sCodeProgram = translateToSCode(absynProgram)
  (FM, cache) = instantiateSCodeToFM(modelName, sCodeProgram)
end


"""
  @author: johti17
  Loads the Modelica Standard Library (MSL).
  Adds the Modelica standard library to the library cache.
Currently 3.2.3 is the default version.

Available versions are:
4.0.0
3.2.3
"""
function loadMSL(; MSL_Version)
  MSL_Version = replace(MSL_Version, "." => "_")
  MSL_Version = replace(MSL_Version, ":" => "_")
  if ! haskey(LIBRARY_CACHE, MSL_Version)
    #= Initialize various global variables =#
    Main.Global.initialize()
    #= Find the MSL =#
    try
      local packagePath = dirname(realpath(Base.find_package("OMFrontend")))
      local packagePath *= "/.."
      local pathToLib = packagePath * string("/lib/Modelica/", MSL_Version, ".mo")
      local p = parseFile(pathToLib)
      #= Translate it to SCode =#
      local scodeMSL = OMFrontend.translateToSCode(p)
      global LIBRARY_CACHE[MSL_Version] = scodeMSL
    catch e
      @info "Failed loading the Modelica Standard Library. Valid versions are 3.2.3 and 4.0.0"
      @info "Continue instantiating the model until the next error."
    end
  end
end

Base.show(io::IO, ::MIME"text/plain", fm::OMFrontend.Main.FLAT_MODEL) = begin
  print(io, "Flat Model:\n", string(fm))
end

Base.show(io::IO, ::MIME"text/plain", t::Tuple{OMFrontend.Main.FLAT_MODEL, OMFrontend.Main.FunctionTreeImpl.EMPTY}) = begin
  print(io, "Flat Model:\n", string(first(t)))
  print(io, "\n(No Functions)\n")
end

Base.show(io::IO, ::MIME"text/plain", t::Tuple{OMFrontend.Main.FLAT_MODEL, OMFrontend.Main.FunctionTreeImpl.LEAF}) = begin
  print(io, "Flat Model:\n", string(first(t)))
  print(io, "\nFunctions:\n", string(last(t)))
end

Base.show(io::IO, ::MIME"text/plain", t::Tuple{OMFrontend.Main.FLAT_MODEL, OMFrontend.Main.FunctionTreeImpl.NODE}) = begin
  print(io, "Flat Model:\n", string(first(t)))
  print(io, "\nFunctions:\n", string(last(t)))
end

include("precompilation.jl")

end #OMFrontend
