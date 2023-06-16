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
  @info "Testing to load  a large MSL model"
  local precompile_libraryAsScode = OMFrontend.LIBRARY_CACHE["MSL_4_0_0"]
  local precompile_prefix = "Modelica.Mechanics.MultiBody.Examples.Loops"
  @info "Time spent precompiing MSL models in: $(precompile_prefix)"
  local precompile_model_name = "$(precompile_prefix).Engine1a"
  @info "Testing $(precompile_model_name)"
  @time (FM, cache) = OMFrontend.instantiateSCodeToFM(precompile_model_name, precompile_libraryAsScode)
  @info "Core compiler modules are successfully precompiled!"
  @info "Compiler modules are successfully precompiled!"
end
