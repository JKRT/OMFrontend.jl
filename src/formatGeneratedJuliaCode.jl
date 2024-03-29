#=
Checks if it is possible to parse a file using Meta.Parse
if so attempts to parse the same file using CSTParser.

If  the above suceeds formats the files using YASStyle.
=#
import CSTParser
using JuliaFormatter
@debug length(ARGS)
if length(ARGS) < 1
  println("Specify a path to a folder to format\n")
  exit(1)
end

PATH = ARGS[1]

@debug "Testing parsing"
#= Test and see if generated files follow Julia syntax =#
for f in filter(x -> endswith(x, "jl"), readdir(PATH))
  local fullPath = abspath("$PATH")
  local pathToParse = "$(fullPath)/$(f)"
  fileContents = read(pathToParse, String)
  println(abspath("$f"))
  CSTP_SUCEED = false
  try
    Meta.parse(fileContents)
  catch error
    @error "Error parsing: $f"
  end
end

@debug "Formatting output\n"

for f in filter(x -> endswith(x, "jl"), readdir(PATH))
  local fullPath = abspath("$PATH")
  local pathToParse = "$(fullPath)/$(f)"
  fileContents = read(pathToParse, String)
  println(abspath("$f"))
  CSTP_SUCEED = false
  try
    CSTParser.parse(fileContents, true)
    style = YASStyle(),
    format_file(
      pathToParse,
      indent = 2,
      verbose = false,
      always_for_in = false,
      whitespace_typedefs = true,
      whitespace_ops_in_indices = true,
      remove_extra_newlines = true,
      import_to_using = false,
      pipe_to_function_call = false,
      short_to_long_function_def = false,
      always_use_return = true,
    )
  catch error
    @debug "Error parsing: $f"
  end
end

exit(0)
