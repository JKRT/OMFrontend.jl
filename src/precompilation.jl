using PrecompileTools
#=
Some routine to cache more native code for this package.
Not meant to be used by users.
The functionality of this file is called during the precompilation phase.
=#

function workload()
  @info "Precompiling builtin libraries..."
  @info "NOTE: This version of OMFrontend only supports Julia versions greater than 1.10"
  if ! haskey(NFModelicaBuiltinCache, "NFModelicaBuiltin")
    @info "Locating external libraries.."
    packagePath = dirname(realpath(Base.find_package("OMFrontend")))
    packagePath *= "/.."
    pathToLib = packagePath * "/lib/NFModelicaBuiltin.mo"
    #= The external C stuff can be a bit flaky.. =#
    GC.enable(false)
    p = parseFile(pathToLib, 2 #= MetaModelica =#)
    @info "Translating builtin library to SCode.."
    s = translateToSCode(Absyn.PROGRAM(nil, Absyn.TOP()))
    s = translateToSCode(p)
    @info "SCode translation done. Saving the library in the cache."
    NFModelicaBuiltinCache["NFModelicaBuiltin"] = s
    @info "Builtin Library Loaded!"

    #@info "initialize cache" #Not in use at the moment...
    #Frontend.MemoryUtil.initialize(400)

    #=Enable GC again.=#
    GC.enable(true)
  end
  @info "Builtin libraries successfully precompiled!"
  @info "Initial compiler module interfaces are compiled!"
  #= Make sure that we load the builtin scode =#
  packagePath = dirname(realpath(Base.find_package("OMFrontend")))
  packagePath *= "/.."
  pathToLib = packagePath * "/lib/NFModelicaBuiltin.mo"
  #= The external C stuff can be a bit flaky.. =#
  GC.enable(false)
  p = OMParser.parseFile(pathToLib, 2 #= MetaModelica=#)
  builtinSCode = Frontend.AbsynToSCode.translateAbsyn2SCode(p)
  GC.enable(true)
  #= End preamble =#
  #= Load the Modelica Standard Library =#
  @info "Loading Version 4.0.0 of the Modelica Standard Library"
  local MSL_V_STARTUP  = "MSL_4_0_0"
  initLoadMSL(MSL_Version= MSL_V_STARTUP)
  #=
  Instantiate the HelloWorld module
  This will precompile a significant part of the frontend.
  =#
  packagePath = dirname(realpath(Base.find_package("OMFrontend")))
  packagePath *= "/.."
  pathToTest = packagePath * "/test/Models/HelloWorld.mo"
  p = OMParser.parseFile(pathToTest, 1)
  s = Frontend.AbsynToSCode.translateAbsyn2SCode(p)
  @info "Compiling core modules. This might take awhile.."
  Frontend.Global.initialize()
  # make sure we have all the flags loaded!
  Frontend.FlagsUtil.loadFlags()
  program = listAppend(builtinSCode, s)
  path = Frontend.AbsynUtil.stringPath("HelloWorld")
  @info "Timings concerning compiling core modules for instantiation:"
  @time res1 = Frontend.instClassInProgram(path, program)
  #=
  For other developers reading this.
  Comment out below if you are using revise and want to have faster feedback when changing different datatypes.
  -John 2023-06-22
  =#
  @info "Testing to load MSL models"
  function flattenModelInMSL_TST(modelName::String; MSL_V)
    if !haskey(OMFrontend.LIBRARY_CACHE, MSL_V)
      OMFrontend.initLoadMSL(MSL_Version= MSL_V)
    end
    local libraryAsScoded = OMFrontend.LIBRARY_CACHE[MSL_V]
    (FM, cache) = OMFrontend.instantiateSCodeToFM(modelName, libraryAsScoded)
  end
  precompile_prefix = "Modelica.Electrical.Analog.Examples"
  precompile_model_names = [
    "IdealTriacCircuit",
    "NandGate",
    "AmplifierWithOpAmpDetailed",
    "SimpleTriacCircuit"
  ]
  for p in precompile_model_names
    @info "Translating: $(p)"
    @time flattenModelInMSL_TST(string(precompile_prefix, ".", p); MSL_V = "MSL_4_0_0")
  end
  precompile_libraryAsScode = OMFrontend.LIBRARY_CACHE["MSL_4_0_0"]
  precompile_prefix = "Modelica.Mechanics.Rotational.Examples"
  precompile_model_names = [
    "RollingWheel",
    "OneWayClutch",
    "SimpleGearShift"
  ]
  @info "Time spent precompiling MSL models in: $(precompile_prefix)"
  for p in precompile_model_names
    @info "Translating: $(string(precompile_prefix, ".", p))"
    @time flattenModelInMSL_TST(string(precompile_prefix, ".", p); MSL_V = "MSL_4_0_0")
  end

  precompile_prefix = "Modelica.Mechanics.MultiBody.Examples.Elementary"
  @info "Checking MSL Examples in $(precompile_prefix)"
  precompile_model_names = [
    "DoublePendulum",
    "DoublePendulumInitTip",
    "ForceAndTorque",
    "FreeBody"
  ]
  for p in precompile_model_names
    @info "Translating: $(string(precompile_prefix, ".", p))"
    @time flattenModelInMSL_TST(string(precompile_prefix, ".", p); MSL_V = "MSL_4_0_0")
  end

  precompile_prefix = "Modelica.Mechanics.MultiBody.Examples.Loops"
  @info "Checking MSL Examples in $(precompile_prefix)"
  precompile_model_names = [
    "Engine1a",
    "Engine1b",
    "Engine1b_analytic"
  ]
  for p in precompile_model_names
    @info "Translating: $(string(precompile_prefix, ".", p))"
    @time flattenModelInMSL_TST(string(precompile_prefix, ".", p); MSL_V = "MSL_4_0_0")
  end
  @info "Core compiler modules are successfully precompiled!"
  @info "Compiler modules are successfully precompiled!"
  #@info "Initializing initial memory buffers..."
end

PrecompileTools.@compile_workload begin
  workload()
end

# @recompile_invalidations begin
#   workload()
# end
