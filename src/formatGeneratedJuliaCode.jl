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
