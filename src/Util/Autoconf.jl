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

module Autoconf

using MetaModelica
using ExportAll

const haveBStatic = true::Bool
const bstatic = if haveBStatic
  "-Wl,-Bstatic"
else
  ""
end::String
const bdynamic = if haveBStatic
  "-Wl,-Bdynamic"
else
  ""
end::String
const configureCommandLine =
  "Configured 2020-09-12 23:22:36 using arguments:  '--disable-option-checking' '--prefix=/home/johti17/OpenModelica/build' 'CC=clang-9' 'CXX=clang++-9' '--without-omc' '--with-ombuilddir=/home/johti17/OpenModelica/build' '--cache-file=/dev/null' '--srcdir=.'"::String
const os = "linux"::String
const make = "make"::String
const exeExt = ""::String
const dllExt = ".so"::String
const ldflags_runtime =
  " -Wl,--no-as-needed -Wl,--disable-new-dtags -lOpenModelicaRuntimeC  -llapack -lblas   -lm -lomcgc -lpthread -rdynamic"::String
const ldflags_runtime_sim =
  " -Wl,--no-as-needed -Wl,--disable-new-dtags -lSimulationRuntimeC  -llapack -lblas   -lm -lomcgc -lpthread -rdynamic -Wl,--no-undefined"::String
const ldflags_runtime_fmu =
  " -Wl,--no-as-needed -Wl,--disable-new-dtags  -llapack -lblas   -lm -lpthread -rdynamic -Wl,--no-undefined"::String
const platform = "Unix"::String
const pathDelimiter = "/"::String
const groupDelimiter = ":"::String
const corbaLibs = ""::String
const hwloc = if 0 == 1
  "-lhwloc"
else
  ""
end::String
const systemLibs =
  list(
    "-lomcruntime",
    "-lexpat",
    "-lsqlite3",
    "-llpsolve55",
    corbaLibs,
    "-lomcgc",
    hwloc,
  )::List
const triple = "x86_64-linux-gnu"::String

@exportAll()
end
