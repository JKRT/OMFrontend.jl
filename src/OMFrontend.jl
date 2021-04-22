"
  An experimental Julia frontend for the Modelica language
"
module OMFrontend

import Absyn
import SCode
import OpenModelicaParser

include("main.jl")

function parseFile(file::String, acceptedGram::Int64 = 1)::Absyn.Program
  return OpenModelicaParser.parseFile(file, acceptedGram)
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
function instantiateSCodeToDAE(@nospecialize(elementToInstantiate::String), @nospecialize (inProgram::SCode.Program))
  # initialize globals
  Main.Global.initialize()
  # make sure we have all the flags loaded!
  # Main.Flags.new(Flags.emptyFlags)
  @debug "Parsing buildin stuff"
  path = realpath(realpath(Base.find_package("OMFrontend") * "./../../"))
  path = path * "/lib/NFModelicaBuiltin.mo"
  GC.enable(false) #=This C stuff can be a bit flaky..=#
  p = parseFile(path, 2 #== MetaModelica ==#)
  @debug "SCode translation"
  s = HybridDAEParser.translateToSCode(p)
  p = Main.listAppend(s, inProgram)
  GC.enable(true)
  Main.instClassInProgram(Absyn.IDENT(elementToInstantiate), p)
end

function testSpin()
    p = parseFile("./src\\example.mo")
    scodeProgram = translateToSCode(p)
    @debug "Translation to SCode"
    @debug "SCode -> DAE"
    (dae, cache) = instantiateSCodeToDAE("HelloWorld", scodeProgram)
    @debug "After DAE Translation"
  return dae
end

function testSpinDAEExport()
  p = parseFile("example.mo")
  scodeProgram = translateToSCode(p)
  @debug "Translation to SCode"
  @debug "SCode -> DAE"
  (dae, cache) = instantiateSCodeToDAE("HelloWorld", scodeProgram)
  @debug "Exporting to file"
  exportDAERepresentationToFile("testDAE.jl", "$dae")
  @debug "DAE Exported"
end

function exportDAERepresentationToFile(fileName::String, contents::String)
  local fdesc = open(fileName, "w")
  write(fdesc, contents)
  close(fdesc)
end

end # module
