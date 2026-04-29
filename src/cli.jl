#= /*
* This file is part of OpenModelica.
*
* Copyright (c) 1998-CurrentYear, Open Source Modelica Consortium (OSMC),
* c/o Linköpings universitet, Department of Computer and Information Science,
* SE-58183 Linköping, Sweden.
*
* All rights reserved.
*
* THIS PROGRAM IS PROVIDED UNDER THE TERMS OF AGPL VERSION 3 LICENSE OR
* THIS OSMC PUBLIC LICENSE (OSMC-PL) VERSION 1.8.
* ANY USE, REPRODUCTION OR DISTRIBUTION OF THIS PROGRAM CONSTITUTES
* RECIPIENT'S ACCEPTANCE OF THE OSMC PUBLIC LICENSE OR THE GNU AGPL
* VERSION 3, ACCORDING TO RECIPIENTS CHOICE.
*
* The OpenModelica software and the OSMC (Open Source Modelica Consortium)
* Public License (OSMC-PL) are obtained from OSMC, either from the above
* address, from the URLs:
* http://www.openmodelica.org or
* https://github.com/OpenModelica/ or
* http://www.ida.liu.se/projects/OpenModelica,
* and in the OpenModelica distribution.
*
* GNU AGPL version 3 is obtained from:
* https://www.gnu.org/licenses/licenses.html#GPL
*
* This program is distributed WITHOUT ANY WARRANTY; without
* even the implied warranty of MERCHANTABILITY or FITNESS
* FOR A PARTICULAR PURPOSE, EXCEPT AS EXPRESSLY SET FORTH
* IN THE BY RECIPIENT SELECTED SUBSIDIARY LICENSE CONDITIONS OF OSMC-PL.
*
* See the full OSMC Public License conditions for more details.
*
*/ =#

#=
  Command-line interface for OMFrontend.

  Wired up as `OMFrontend.julia_main`, this module is the entry point for a
  self-contained binary built with `PackageCompiler.create_app`. The CLI
  surfaces the most-used translation pipeline steps from src/OMFrontend.jl:
  parse a file, translate to SCode, flatten a model (optionally against the
  Modelica Standard Library), and emit the result as flat Modelica.

  No external argument-parsing dependency is used. The hand-rolled scanner
  in `_parseOpts` keeps the closure of OMFrontend's deps unchanged.
=#

const CLI_PROGRAM = "OMFrontend"

const CLI_HELP = """
Usage: $(CLI_PROGRAM) <command> [options]

Commands:
  flatten <ModelName>   Flatten a Modelica model to flat Modelica.
  parse   <PATH>        Parse a .mo file and report success.
  scode   <PATH>        Parse a .mo file and translate to SCode.
  version               Print the OMFrontend version.
  help [<command>]      Show help for a command (or this overview).

Global options:
  -h, --help            Show this help text.
  -V, --version         Print the OMFrontend version.

Run `$(CLI_PROGRAM) help <command>` for command-specific options.
"""

const CLI_HELP_FLATTEN = """
Usage: $(CLI_PROGRAM) flatten <ModelName> [options]

Flatten <ModelName> to flat Modelica and write the result to stdout (or to
--out). The model definition is taken from --file, or from the Modelica
Standard Library when --msl is given.

Options:
  --file PATH        Modelica source file containing the model.
  --msl  VERSION     MSL version to load (e.g. 3.2.3, 4.0.0). When set, the
                     model is looked up inside the MSL after loading.
  --out  PATH        Write the flat Modelica string to PATH instead of stdout.
  --no-scalarize     Skip array scalarization (kept as array equations).
  --keep-quotes      Keep quoted Modelica identifiers in the output.

At least one of --file or --msl must be given.
"""

const CLI_HELP_PARSE = """
Usage: $(CLI_PROGRAM) parse <PATH> [options]

Parse PATH as Modelica (or MetaModelica with --grammar 2) and report success.

Options:
  --grammar N        Grammar to use. 1 = Modelica (default), 2 = MetaModelica.
  --out PATH         Write a textual dump of the syntax tree to PATH.
"""

const CLI_HELP_SCODE = """
Usage: $(CLI_PROGRAM) scode <PATH> [options]

Parse PATH and translate it to SCode.

Options:
  --out PATH         Write the SCode dump to PATH instead of stdout.
"""

#=
  Lightweight option scanner. Returns (positional, options) where options is
  Dict{String, Any} keyed by the flag/option name as it appears on the command
  line (e.g. "--file"). Flags map to `true`, valued options to their string
  value. Throws ArgumentError on missing values or unknown flags.
=#
function _parseOpts(args::AbstractVector{<:AbstractString},
                    flags::AbstractSet{String},
                    valued::AbstractSet{String})
    positional = String[]
    options    = Dict{String, Any}()
    i = 1
    while i <= length(args)
        a = args[i]
        if a in flags
            options[a] = true
        elseif a in valued
            i += 1
            i <= length(args) || throw(ArgumentError("Missing value for $(a)"))
            options[a] = args[i]
        elseif startswith(a, "--") || (startswith(a, "-") && length(a) > 1 && !isdigit(a[2]))
            throw(ArgumentError("Unknown option: $(a)"))
        else
            push!(positional, a)
        end
        i += 1
    end
    return (positional, options)
end

function _wantsHelp(opts::AbstractDict)::Bool
    return get(opts, "--help", false) === true || get(opts, "-h", false) === true
end

function _cmd_flatten(args::AbstractVector{<:AbstractString})::Cint
    flags  = Set(["--no-scalarize", "--keep-quotes", "-h", "--help"])
    valued = Set(["--file", "--msl", "--out"])
    pos, opts = _parseOpts(args, flags, valued)

    if _wantsHelp(opts)
        print(CLI_HELP_FLATTEN)
        return 0
    end

    if length(pos) != 1
        println(stderr, "flatten: expected exactly one model name (got $(length(pos)))")
        print(stderr, CLI_HELP_FLATTEN)
        return 2
    end
    modelName = pos[1]

    file       = get(opts, "--file", nothing)
    mslVersion = get(opts, "--msl",  nothing)
    outPath    = get(opts, "--out",  nothing)
    scalarize  = !haskey(opts, "--no-scalarize")
    keepQuotes = haskey(opts, "--keep-quotes")

    if file === nothing && mslVersion === nothing
        println(stderr, "flatten: at least one of --file or --msl is required")
        return 2
    end

    fmAndCache = if mslVersion !== nothing
        flattenModelWithMSL(modelName; MSL_Version = mslVersion, scalarize = scalarize)
    else
        flattenModel(modelName, file; scalarize = scalarize)
    end

    rendered = toFlatModelica(fmAndCache; printBindingTypes = false)
    if !keepQuotes
        rendered = removeQuotesFromFlatModelica(rendered)
    end

    if outPath === nothing
        print(rendered)
        endswith(rendered, "\n") || println()
    else
        write(outPath, rendered)
        println(stderr, "Wrote flat Modelica to $(outPath)")
    end
    return 0
end

function _cmd_parse(args::AbstractVector{<:AbstractString})::Cint
    flags  = Set(["-h", "--help"])
    valued = Set(["--grammar", "--out"])
    pos, opts = _parseOpts(args, flags, valued)

    if _wantsHelp(opts)
        print(CLI_HELP_PARSE)
        return 0
    end

    if length(pos) != 1
        println(stderr, "parse: expected exactly one path (got $(length(pos)))")
        print(stderr, CLI_HELP_PARSE)
        return 2
    end
    path    = pos[1]
    grammar = parse(Int, get(opts, "--grammar", "1"))
    outPath = get(opts, "--out", nothing)

    program = parseFile(path, Int64(grammar))
    msg = "Parsed $(path) successfully (grammar = $(grammar))."
    if outPath === nothing
        println(msg)
    else
        write(outPath, string(program))
        println(stderr, msg, " Dump written to $(outPath).")
    end
    return 0
end

function _cmd_scode(args::AbstractVector{<:AbstractString})::Cint
    flags  = Set(["-h", "--help"])
    valued = Set(["--out"])
    pos, opts = _parseOpts(args, flags, valued)

    if _wantsHelp(opts)
        print(CLI_HELP_SCODE)
        return 0
    end

    if length(pos) != 1
        println(stderr, "scode: expected exactly one path (got $(length(pos)))")
        print(stderr, CLI_HELP_SCODE)
        return 2
    end
    path    = pos[1]
    outPath = get(opts, "--out", nothing)

    program = parseFile(path)
    scode   = translateToSCode(program)

    if outPath === nothing
        println(string(scode))
    else
        exportSCodeRepresentationToFile(outPath, scode)
        println(stderr, "Wrote SCode to $(outPath)")
    end
    return 0
end

function _versionString()::String
    try
        project = joinpath(dirname(dirname(realpath(Base.find_package("OMFrontend")))),
                           "Project.toml")
        for line in eachline(project)
            m = match(r"^version\s*=\s*\"([^\"]+)\"", line)
            m === nothing && continue
            return m.captures[1]
        end
    catch
    end
    return "unknown"
end

function _cmd_help(args::AbstractVector{<:AbstractString})::Cint
    if isempty(args)
        print(CLI_HELP)
        return 0
    end
    cmd = args[1]
    text = if cmd == "flatten"
        CLI_HELP_FLATTEN
    elseif cmd == "parse"
        CLI_HELP_PARSE
    elseif cmd == "scode"
        CLI_HELP_SCODE
    elseif cmd in ("help", "version")
        CLI_HELP
    else
        println(stderr, "help: unknown command \"$(cmd)\"")
        print(stderr, CLI_HELP)
        return 2
    end
    print(text)
    return 0
end

function _runCLI(args::AbstractVector{<:AbstractString})::Cint
    if isempty(args)
        print(CLI_HELP)
        return 0
    end

    cmd  = args[1]
    rest = @view args[2:end]

    if cmd in ("-h", "--help", "help")
        return _cmd_help(rest)
    elseif cmd in ("-V", "--version", "version")
        println("$(CLI_PROGRAM) $(_versionString())")
        return 0
    elseif cmd == "flatten"
        return _cmd_flatten(rest)
    elseif cmd == "parse"
        return _cmd_parse(rest)
    elseif cmd == "scode"
        return _cmd_scode(rest)
    else
        println(stderr, "Unknown command: $(cmd)")
        print(stderr, CLI_HELP)
        return 2
    end
end

"""
    julia_main() :: Cint

Entry point for an OMFrontend binary built with `PackageCompiler.create_app`.
Reads command-line arguments from `Base.ARGS` and returns a process exit
code (0 on success, 1 on uncaught error, 2 on usage error).
"""
function julia_main()::Cint
    try
        return _runCLI(Base.ARGS)
    catch e
        println(stderr, "OMFrontend CLI error: ", sprint(showerror, e))
        return 1
    end
end
