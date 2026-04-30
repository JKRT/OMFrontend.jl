#=
  Build a trimmed AOT binary of OMFrontend using juliac (Julia 1.12+).

  juliac is experimental: --trim=safe requires a fully type-inferable program;
  --trim=unsafe is permissive and ships even with inference holes (failures may
  surface at runtime as MethodError). Start with unsafe to see how close
  OMFrontend is, then attempt safe and treat the inference report as a TODO list.

  Output:
    OMFRONTEND_TRIM_DIR  output directory (default <repo-root>/OMFrontend-trim)
    OMFRONTEND_TRIM_MODE no | unsafe | safe        (default unsafe)
    OMFRONTEND_TRIM_LOG  capture juliac stdout/stderr to a log file (default
                         <out-dir>/build.log)

  Usage:
    julia --project=. bin/build_trim.jl
=#

const REPO_ROOT = abspath(joinpath(@__DIR__, ".."))
const ENTRY     = abspath(joinpath(@__DIR__, "trim_entry.jl"))

const TRIM_MODE = get(ENV, "OMFRONTEND_TRIM_MODE", "unsafe")
const TRIM_DIR  = abspath(get(ENV, "OMFRONTEND_TRIM_DIR",
                              joinpath(REPO_ROOT, "OMFrontend-trim")))
const OUT_BIN   = joinpath(TRIM_DIR, Sys.iswindows() ? "omfrontend.exe" : "omfrontend")
const LOG_FILE  = abspath(get(ENV, "OMFRONTEND_TRIM_LOG", joinpath(TRIM_DIR, "build.log")))

const JULIAC = abspath(joinpath(Sys.BINDIR, "..", "share", "julia", "juliac", "juliac.jl"))

isfile(JULIAC) || error("juliac.jl not found at $(JULIAC); requires Julia 1.12+.")
isfile(ENTRY)  || error("entry file not found at $(ENTRY)")
mkpath(TRIM_DIR)

if TRIM_MODE ∉ ("no", "unsafe", "safe")
    error("OMFRONTEND_TRIM_MODE must be one of: no, unsafe, safe (got $(TRIM_MODE))")
end

@info "Building trimmed OMFrontend" mode=TRIM_MODE entry=ENTRY out=OUT_BIN log=LOG_FILE

# juliac flags. --experimental is mandatory in 1.12. --compile-ccallable also
# emits the @ccallable C entry so the binary can be called via dlopen if needed.
cmd = `$(Base.julia_cmd()) --project=$(REPO_ROOT) $(JULIAC)
       --experimental
       --trim=$(TRIM_MODE)
       --compile-ccallable
       --output-exe $(OUT_BIN)
       $(ENTRY)`

open(LOG_FILE, "w") do io
    println(io, "# juliac command")
    println(io, cmd)
    println(io)
end

# Tee through the log so the user sees progress and can inspect after.
try
    run(pipeline(cmd, stdout = LOG_FILE, stderr = LOG_FILE; append = true))
catch e
    @error "juliac failed; see log" log=LOG_FILE error=e
    exit(1)
end

if isfile(OUT_BIN)
    @info "Trimmed binary built" path=OUT_BIN size_mb=round(filesize(OUT_BIN)/1024/1024, digits=1)
else
    @error "juliac reported success but output binary is missing" expected=OUT_BIN
    exit(1)
end
