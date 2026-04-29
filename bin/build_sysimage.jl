#=
  Build a sysimage that bakes OMFrontend (and its precompile workload) into
  native code.

  Output path defaults to <repo-root>/OMFrontend-sysimage.<platform-ext>,
  where the extension is so / dylib / dll. Both the path and the extension
  can be overridden through environment variables:

    OMFRONTEND_SYSIMAGE_PATH  full output path (overrides everything)
    OMFRONTEND_SYSIMAGE_EXT   extension only, used when PATH is unset

  Usage:
    julia --project bin/build_sysimage.jl

  PackageCompiler is resolved through the stacked default environment
  (the @v#.# entry on LOAD_PATH), so it does not need to be listed in
  OMFrontend's Project.toml. Install it once with:
    julia -e 'import Pkg; Pkg.add("PackageCompiler")'
=#

using PackageCompiler

const REPO_ROOT     = abspath(joinpath(@__DIR__, ".."))
const DEFAULT_EXT   = Sys.iswindows() ? "dll" : Sys.isapple() ? "dylib" : "so"
const EXT           = get(ENV, "OMFRONTEND_SYSIMAGE_EXT", DEFAULT_EXT)
const SYSIMAGE_PATH = get(ENV, "OMFRONTEND_SYSIMAGE_PATH",
                          joinpath(REPO_ROOT, "OMFrontend-sysimage." * EXT))
const PRECOMPILE    = abspath(joinpath(@__DIR__, "precompile_workload.jl"))

@info "Building sysimage" sysimage_path = SYSIMAGE_PATH precompile_execution = PRECOMPILE

create_sysimage(
    ["OMFrontend"];
    sysimage_path           = SYSIMAGE_PATH,
    precompile_execution_file = PRECOMPILE,
)

@info "Sysimage written" path = SYSIMAGE_PATH size_mb = round(filesize(SYSIMAGE_PATH) / 1024 / 1024, digits = 1)
