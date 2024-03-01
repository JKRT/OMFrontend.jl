#=
  Some routine to cache more naitve code for this package.
  Not meant to be used by users.
=#
PrecompileTools.@compile_workload begin
  #= Make sure that we load the bultin scode=#
  packagePath = dirname(realpath(Base.find_package("OMFrontend")))
  packagePath *= "/.."
  pathToLib = packagePath * "/lib/NFModelicaBuiltin.mo"
  #= The external C stuff can be a bit flaky.. =#
  GC.enable(false)
  p = OMParser.parseFile(pathToLib, 2 #== MetaModelica ==#)
  builtinSCode = Main.AbsynToSCode.translateAbsyn2SCode(p)
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
  s = Main.AbsynToSCode.translateAbsyn2SCode(p)
  @info "Compiling core modules. This might take awhile.."
  Main.Global.initialize()
  # make sure we have all the flags loaded!
  Main.FlagsUtil.loadFlags()
  program = listAppend(builtinSCode, s)
  path = Main.AbsynUtil.stringPath("HelloWorld")
  @info "Timings concerning compiling core modules for instantiation:"
  @time res1 = Main.instClassInProgram(path, program)
  #=
  For other developers reading this.
  Comment out below if you are using revise and want to have faster feedback when changing different datatypes.
  -John 2023-06-22
  =#
  # @info "Testing to load  a large MSL model"
  # function flattenModelInMSL_TST(modelName::String; MSL_V)
  #   if !haskey(OMFrontend.LIBRARY_CACHE, MSL_V)
  #     OMFrontend.initLoadMSL(MSL_Version= MSL_V)
  #   end
  #   local libraryAsScoded = OMFrontend.LIBRARY_CACHE[MSL_V]
  #   (FM, cache) = OMFrontend.instantiateSCodeToFM(modelName, libraryAsScoded)
  # end
  # precompile_prefix = "Modelica.Electrical.Analog.Examples"
  # precompile_model_names = [
  #   "IdealTriacCircuit",
  #   "NandGate",
  #   "AmplifierWithOpAmpDetailed",
  #   "SimpleTriacCircuit"
  # ]
  # for p in precompile_model_names
  #   @info "Translating:" p
  #   @time flattenModelInMSL_TST(string(precompile_prefix, ".", p); MSL_V = "MSL_4_0_0")
  # end
  # precompile_libraryAsScode = OMFrontend.LIBRARY_CACHE["MSL_4_0_0"]
  # precompile_prefix = "Modelica.Mechanics.Rotational.Examples"
  # precompile_model_names = [
  #   "RollingWheel",
  #   "OneWayClutch",
  #   "SimpleGearShift"
  # ]
  # @info "Time spent precompiing MSL models in: $(precompile_prefix)"
  # for p in precompile_model_names
  #   @info "Translating:" string(precompile_prefix, ".", p)
  #   @time flattenModelInMSL_TST(string(precompile_prefix, ".", p); MSL_V = "MSL_4_0_0")
  # end
  # @info "Core compiler modules are successfully precompiled!"
  # @info "Compiler modules are successfully precompiled!"
end
