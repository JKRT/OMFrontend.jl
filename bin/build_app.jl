#=
  Build a self-contained OMFrontend executable via PackageCompiler.create_app.

  The output is a directory tree containing `bin/OMFrontend(.exe)` plus the
  shared libraries it needs at runtime. The binary's entry point is
  `OMFrontend.julia_main` (defined in src/cli.jl).

  Output directory:
    OMFRONTEND_APP_DIR  -- defaults to <repo-root>/OMFrontend-app

  Usage:
    julia --project bin/build_app.jl

  PackageCompiler is resolved through the stacked default environment, so it
  does not need to be listed in OMFrontend's Project.toml. Install once with:
    julia -e 'import Pkg; Pkg.add("PackageCompiler")'

  Note on bundled data: the OMFrontend `__init__` reads `lib/NFModelicaBuiltin.mo`
  and `test/Models/HelloWorld.mo` via `Base.find_package("OMFrontend")`.
  PackageCompiler bundles each package's source tree under
  `share/julia/packages/<pkg>/<uuid>` inside the app, so `find_package` resolves
  to that location at runtime and the relative `lib/...` and `test/...` paths
  still work. We package the same MSL libraries that ship in the repo.
=#

using PackageCompiler

const REPO_ROOT = abspath(joinpath(@__DIR__, ".."))
const APP_DIR   = get(ENV, "OMFRONTEND_APP_DIR", joinpath(REPO_ROOT, "OMFrontend-app"))
const PROJECT   = REPO_ROOT
const PRECOMP   = abspath(joinpath(@__DIR__, "precompile_workload.jl"))

@info "Building OMFrontend app" project = PROJECT app_dir = APP_DIR precompile_execution = PRECOMP

# `force = true` lets us re-run without first removing the previous build.
create_app(
    PROJECT,
    APP_DIR;
    executables                = ["OMFrontend" => "julia_main"],
    precompile_execution_file  = PRECOMP,
    force                      = true,
    include_lazy_artifacts     = true,
)

binSuffix = Sys.iswindows() ? ".exe" : ""
binary    = joinpath(APP_DIR, "bin", "OMFrontend" * binSuffix)

@info "App written" path = APP_DIR binary = binary
if isfile(binary)
    @info "Binary present" size_mb = round(filesize(binary) / 1024 / 1024, digits = 1)
end
