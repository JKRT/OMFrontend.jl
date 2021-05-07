[![Github Action CI](https://github.com/JKRT/OMFrontend.jl/workflows/CI/badge.svg)](https://github.com/JKRT/OMFrontend.jl/actions)
# OMFrontend.jl
Experimental implementation of NF. That is a Modelica frontend in Julia

## Developer instructions 
  - Go to ImmutableList.jl 
  - Dev it using the Julia package manager
  - Go to MetaModelica.jl 
  - Dev it using the Julia package manager
  - Go to Absyn.jl

Using same procedure as above for:
  - ArrayUtil.jl 
  - ListUtil.jl
Once this is done develop SCode and last but not least the DAE.
## Core modules
	- Develop the OpenModelicaParser.jl
	- Develop OMFrontend.jl
	- Develop OMBackend.jl
	- Develop the OM package 

Since this is currently work in progress expect some warnings:) 

Make sure that all submodules are on master

```
git submodule foreach "git checkout master && git pull --recursive"
```
