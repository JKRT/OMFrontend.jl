[![Github Action CI](https://github.com/JKRT/OMFrontend.jl/workflows/CI/badge.svg)](https://github.com/JKRT/OMFrontend.jl/actions)[![License: OSMC-PL](https://img.shields.io/badge/license-OSMC--PL-lightgrey.svg)](OSMC-License.txt)
# OMFrontend.jl
Experimental implementation of NF. That is a Modelica frontend in Julia.
For the entire suite of software components that make up this compiler see [OpenModelica.jl](https://github.com/JKRT/OM.jl)

## Notes regarding the first public release
This should be considered an alpha.
This means that changes may happen quickly, and that they might be breaking.

## Example use

Assuming you have the following Modelica model:
```
model HelloWorld
  Real x(start = 1, fixed = true);
  parameter Real a = 1;
equation
  der(x) = - a * x;
end HelloWorld;
```

You can use the API in the following way (Assuming the file is in the active directory):

```
# Note that we use using. This is done in order to allow the correct overloading of various pretty printing printouts
using OMFrontend
modelFile = "HelloWorld.mo"
modelName = "HelloWorld"
p = OMFrontend.parseFile(modelFile)
scodeProgram = OMFrontend.translateToSCode(p);
(FM, cache) = OMFrontend.instantiateSCodeToFM(modelName, scodeProgram);
OMFrontend.toString(FM)
```

This should result in the following string representation of FlatModelica:

```
"class HelloWorld\n  Real x(fixed = true, start = 1.0);\n  parameter Real a = 1.0;\nequation\n  der(x) = -a * x;\nend HelloWorld;\n"
```

Note: In this case the flat Modelica code is almost indentical to the original model.

As a modeler it might also be the case you are interesting in some more advanced models
or you wish to construct models using libraries such as the Modelica Standard Library.
For instance, say you are interested in `AD_DA_conversion` in `Modelica.Electrical.Analog.Examples`
To instantiate this model we can define the function:
```julia
function flattenModelInMSL_TST(modelName::String)
  local MSL_V  = "MSL.4.0.0"
  if !haskey(OMFrontend.LIBRARY_CACHE, MSL_V)
    OMFrontend.initLoadMSL(MSL_Version= MSL_V)
  end
  local libraryAsScoded = OMFrontend.LIBRARY_CACHE["MSL.4.0.0"]
  (FM, cache) = OMFrontend.instantiateSCodeToFM(modelName, libraryAsScoded)
end

```
To flatten this model execute:
```
(FM, cache) = flattenModelInMSL_TST("Modelica.Electrical.Analog.Examples.AD_DA_conversion");
```
Note the `;` the representation use pointers in order to share the data.
Hence, if the above is executed without it there may be looping output.
This can be resolved by using `using OMFrontend` before you start using the package.

Similar to the code above it can be exported to a string by:

```
str = OMFrontend.string(FM)
println(str)
```

### Representation
This particular model uses the FlatModelica datastructure.
An alternative is to use the DAE datastructure defined by [DAE.jl](https://github.com/JKRT/DAE.jl)

### Other API functions
There is also an API defined for using libraries, however, as of this writing the documentations for these procedures
are not yet finalized and is subject to change. If you are a developer please see `OMFrontend.jl/src/OMFrontend.jl` for the other methods.

### Issues/Questions/Contributing
It should be noted that this package is a component of [OM.jl](https://github.com/JKRT/OM.jl) hence the API and the functionality for OMFrontend.jl is somewhat low level.


For any questions or ideas on how we could collaborate on something please see my LiU email at my page [LiU-page](https://liu.se/en/employee/johti17)
