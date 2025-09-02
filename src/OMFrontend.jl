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

#= This file defines additional utility macros.. =#
include("util.jl")

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
  This cache contains libraries for later use.
"""
const LIBRARY_CACHE = Dict{String, SCode.Program}()

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
  s = Frontend.AbsynToSCode.translateAbsyn2SCode(p)
  Frontend.Global.initialize()
  # make sure we have all the flags loaded!
  Frontend.FlagsUtil.loadFlags()
  builtinSCode = NFModelicaBuiltinCache["NFModelicaBuiltin"]
  program = listAppend(builtinSCode, s)
  path = Frontend.AbsynUtil.stringPath("HelloWorld")
  res1 = Frontend.instClassInProgram(path, program)
  return nothing
end

include("main.jl")
#=Internal modules=#
import .Frontend.Flags
import .Frontend.FlagsUtil


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
  return Frontend.AbsynToSCode.translateAbsyn2SCode(inProgram)
end

"""
  Instantiates and translates to DAE.
  The element to instantiate should be provided in the following format:
  <component>.<component_1>.<component_2>...
"""
function instantiateSCodeToDAE(elementToInstantiate::String, inProgram::SCode.Program)
  # initialize globals
  Frontend.Global.initialize()
  # make sure we have all the flags loaded!
  #Frontend.Flags.new(Flags.emptyFlags)
  local builtinSCode = NFModelicaBuiltinCache["NFModelicaBuiltin"]
  local program = listAppend(builtinSCode, inProgram)
  local path = Frontend.AbsynUtil.stringPath(elementToInstantiate)
  Frontend.instClassInProgram(path, program)
end

"""
  Instantiates and translates to the flat model representation
  The element to instantiate should be provided in the following format:
  <component>.<component_1>.<component_2>...
"""
function instantiateSCodeToFM(elementToInstantiate::String,
                              inProgram::SCode.Program; scalarize = true)
  # initialize globals
  Frontend.Global.initialize()
  # make sure we have all the flags loaded!
  #  Frontend.Flags.new(Flags.emptyFlags)
  Frontend.FlagsUtil.set(Frontend.Flags.NF_SCALARIZE, scalarize)
  local builtinSCode = NFModelicaBuiltinCache["NFModelicaBuiltin"]
  local program = listReverse(listAppend(builtinSCode, inProgram))
  local path = Frontend.AbsynUtil.stringPath(elementToInstantiate)
  (flat_model, funcs, inst_cls) = Frontend.instClassInProgramFM(path, program)
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
function toString(model::Frontend.FlatModel)
  local res = Frontend.toString(model)
  local res = replace(res, "\\n" => "\n")
  return res
end

function toString(fmFuncs::Tuple)
  local model = first(fmFuncs)
  local funcs = last(fmFuncs)
  local res = Frontend.toString(model)
  res *= string(funcs)
  res = replace(res, "\\n" => "\n")
  return res
end

"""
  Overload the Julia to string function
"""
function Base.string(model::Frontend.FlatModel)
  return toString(model::Frontend.FlatModel)
end

"""
  Converts a function tree to a string
"""
function Base.string(ft::Frontend.FunctionTreeImpl.Tree)
  local fLst = OMFrontend.Frontend.FunctionTreeImpl.toList(ft)
  local buffer = IOBuffer()
  for (_, v) in fLst
    println(buffer, OMFrontend.Frontend.toFlatString(v))
  end
  return replace(String(take!(buffer)), "\\n" => "\n")
end

function toFlatModelica(fm, fLst::List; printBindingTypes = false)
  return replace(Frontend.toFlatString(fm, fLst, printBindingTypes), "\\n" => "\n")
end

function toFlatModelica(fm, fLst::Frontend.FunctionTreeImpl.NODE; printBindingTypes = false)
  return replace(Frontend.toFlatString(fm, cacheToFunctionList(fLst), printBindingTypes), "\\n" => "\n")
end

function toFlatModelica(flatModelicaAndFunctionTree::Tuple;
                        printBindingTypes = false)
  local fLst = cacheToFunctionList(last(flatModelicaAndFunctionTree))
  local fm = first(flatModelicaAndFunctionTree)
  return replace(Frontend.toFlatString(fm, fLst, printBindingTypes), "\\n" => "\n")
end

function writeFlatModelicaToFile(fm, fLst;
                                 printBindingtypes = false,
                                 fileName,
                                 removeQuotes::Bool)
  local fmStr = toFlatModelica(fm,
                               fLst;
                               printBindingTypes = printBindingtypes,)
  fmStr = if removeQuotes
    removeQuotesFromFlatModelica(fmStr)
  else
    fmStr
  end
  f = write(fileName, fmStr)
  #close(f)
end

"""
  Converts the function cache represented as a tree where the path is the key into a list of functions
"""
function cacheToFunctionList(cache)
  fLst = OMFrontend.Frontend.FunctionTreeImpl.toList(cache)
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

function initLoadMSL(;MSL_Version = "MSL:3.2.3")
  # For printing
  @info "Loading MSL\n\t Version: $(MSL_Version)"
  MSL_Version = replace(MSL_Version, "." => "_")
  MSL_Version = replace(MSL_Version, ":" => "_")
  @time return loadMSL(MSL_Version = MSL_Version)
  @info "MSL successfully Loaded"
end

"""
`function flattenModelWithMSL(modelName::String, fileName::String; MSL_Version = "MSL:3.2.3")`

Returns the flat representation of a modelica model along with the functions used and define by the model.
See the keyword argument for specifying MSL version.
Valid versions are 3.2.3 and 4.0.0.
"""
function flattenModelWithMSL(modelName::String,
                             fileName::String;
                             MSL_Version = "MSL:3.2.3",
                             scalarize = true)
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
  #println("Attempting to instantiate..." * modelName)
  (FM, cache) = instantiateSCodeToFM(modelName, program; scalarize = scalarize)
end

"""
`flattenModel(modelName::String, fileName::String)`

Returns the flat representation of a modelica model along with the functions used and define by the model.
"""
function flattenModel(modelName::String, fileName::String; scalarize = true)
  local absynProgram = parseFile(fileName)
  local sCodeProgram = translateToSCode(absynProgram)
  (FM, cache) = instantiateSCodeToFM(modelName, sCodeProgram; scalarize = scalarize)
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
    Frontend.Global.initialize()
    #= Find the MSL =#
    try
      @info "Loading MSL.."
      local packagePath = dirname(realpath(Base.find_package("OMFrontend")))
      local packagePath *= "/.."
      local pathToLib = packagePath * string("/lib/Modelica/", MSL_Version, ".mo")
      @info "Initial parsing of the MSL..."
      @time local p = parseFile(pathToLib)
      #= Translate it to SCode =#
      local scodeMSL = OMFrontend.translateToSCode(p)
      global LIBRARY_CACHE[MSL_Version] = scodeMSL
      return scodeMSL
    catch e
      @info "Failed loading the Modelica Standard Library. Valid versions are 3.2.3 and 4.0.0"
      @info "Continue instantiating the model until the next error."
    end
end
end


"""
```
enableDumpDebug()
```
Enable staged dumping of the flat model between different compiler phases.
NOTE this will generate files on your local drive if enabled.

To disable see ```disableDumpDebug()```
"""
function enableDumpDebug()
  status = FlagsUtil.enableDebug(Flags.NF_DUMP_FLAT)
  @info "Enabled Flags.NF_DUMP_FLAT. Old status was $(status)"
end

"""
```
disableDumpDebug()
```
Disables staged dumping of the flat  model between different compiler phases.

"""
function disableDumpDebug()
  status = FlagsUtil.disableDebug(Flags.NF_DUMP_FLAT)
  @info "Disabled Flags.NF_DUMP_FLAT. Old status was $(status)"
end

Base.show(io::IO, ::MIME"text/plain", fm::OMFrontend.Frontend.FLAT_MODEL) = begin
  print(io, "Flat Model:\n", string(fm))
end

Base.show(io::IO, ::MIME"text/plain", t::Tuple{OMFrontend.Frontend.FLAT_MODEL, OMFrontend.Frontend.FunctionTreeImpl.EMPTY}) = begin
  print(io, "Flat Model:\n", string(first(t)))
  print(io, "\n(No Functions)\n")
end

Base.show(io::IO, ::MIME"text/plain", t::Tuple{OMFrontend.Frontend.FLAT_MODEL, OMFrontend.Frontend.FunctionTreeImpl.LEAF}) = begin
  print(io, "Flat Model:\n", string(first(t)))
  print(io, "\nFunctions:\n", string(last(t)))
end

Base.show(io::IO, ::MIME"text/plain", t::Tuple{OMFrontend.Frontend.FLAT_MODEL, OMFrontend.Frontend.FunctionTreeImpl.NODE}) = begin
  print(io, "Flat Model:\n", string(first(t)))
  print(io, "\nFunctions:\n", string(last(t)))
end

"""
```
removeQuotesFromFlatModelica(flatModelicaStr::String)
```
This function postprocesses a flat modelica model represented as a string.
It does so by removing quoted variables and expressions where possible.
This function should be used on models that has ascii characters only.
This can be useful if you wish to remove redundant clutter from flat models.

  NOTE: Not exhaustively tested for all models.
"""
function removeQuotesFromFlatModelica(fmStr::String)

  local specialSymbols = ["'+'", "'*'", "'/'", "'-'", "'constructor'"]

  function shouldBeQuoted(matchedString)
    local reg = r"\["
    local reg2 = r"\*|\+|-"
    contains(matchedString.match, reg) || contains(matchedString.match, reg2)
  end

  local buffer::IOBuffer = IOBuffer()
  if ! isascii(fmStr)
    @info "The model contains characters not in the ascii character encoding format.\nThe string was not modified."
    return fmStr
  end
  local strs = split(fmStr, "\n")
  for str in strs
    local matchedStr::Option{RegexMatch}
    local replaced = false
    local mstr = str
    if (contains(mstr, "'"))
      local reg = r"'[^']*'"
      matchedStrings = eachmatch(reg,  mstr)

      for matchedString in matchedStrings
        local underscoresReplaced = replace(matchedString.match, "." => "_")
        if ! (contains(matchedString.match, r"\[")  || contains(matchedString.match, r"\+|\*|\-"))
          strWithQuotesAndUnderscoresReplaced = replace(underscoresReplaced, "'" => "")
          mstr = replace(mstr, matchedString.match => strWithQuotesAndUnderscoresReplaced)
        else
          #= Bad code...=#
          for ss in specialSymbols
            mstr = replace(mstr, ss => replace(ss, "'"=> ""))
          end
        end
      end
      println(buffer, mstr)
    else
      println(buffer, mstr)
    end
  end

  return String(take!(buffer))
end

include("precompilation.jl")

end #OMFrontend
