#=
  Driven by PackageCompiler at sysimage build time.

  Loading OMFrontend already triggers a substantial workload via __init__
  (parses NFModelicaBuiltin, instantiates HelloWorld) and via the
  PrecompileTools @compile_workload registered in src/precompilation.jl.
  We additionally exercise the public entry points so they are traced
  with concrete input types and baked into the resulting sysimage.
=#

using OMFrontend

let
    pkgRoot     = dirname(dirname(realpath(Base.find_package("OMFrontend"))))
    helloWorld  = joinpath(pkgRoot, "test", "Models", "HelloWorld.mo")

    program = OMFrontend.parseFile(helloWorld, 1)
    scode   = OMFrontend.translateToSCode(program)
    OMFrontend.instantiateSCodeToDAE("HelloWorld", scode)
end
