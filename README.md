[![Github Action CI](https://github.com/JKRT/OMFrontend.jl/workflows/CI/badge.svg)](https://github.com/JKRT/OMFrontend.jl/actions/workflows/ci.yml)
[![Documentation](https://img.shields.io/badge/docs-stable-blue.svg)](https://JKRT.github.io/OMFrontend.jl/)
[![License: OSMC-PL](https://img.shields.io/badge/license-OSMC--PL-lightgrey.svg)](OSMC-License.txt)

# OMFrontend.jl

An experimental implementation of the OpenModelica New Frontend (NF) in Julia.
The package parses Modelica source, lowers it to SCode, and instantiates it
into FlatModelica or DAE form.

For the full compiler suite that builds on top of this package, see
[OpenModelica.jl](https://github.com/JKRT/OM.jl). The API exposed here is
deliberately low-level; for higher-level use cases, prefer OM.jl.

## Release Status

This package is under active development. Interfaces may change without
notice, and breaking changes should be expected between releases.

## Example Use

Given the following Modelica model in `HelloWorld.mo`:

```modelica
model HelloWorld
  Real x(start = 1, fixed = true);
  parameter Real a = 1;
equation
  der(x) = -a * x;
end HelloWorld;
```

The shortest path from source to a flat model is `flattenModel`:

```julia
using OMFrontend
(FM, functions) = OMFrontend.flattenModel("HelloWorld", "HelloWorld.mo")
println(OMFrontend.toString(FM))
```

The output is the flat representation of the model:

```
class HelloWorld
  Real x(fixed = true, start = 1.0);
  parameter Real a = 1.0;
equation
  der(x) = -a * x;
end HelloWorld;
```

If finer control is needed, the same result can be produced step by step:

```julia
absynProgram  = OMFrontend.parseFile("HelloWorld.mo")
scodeProgram  = OMFrontend.translateToSCode(absynProgram)
(FM, funcs)   = OMFrontend.instantiateSCodeToFM("HelloWorld", scodeProgram)
```

To obtain the DAE form instead of FlatModelica, call `instantiateSCodeToDAE`
on the SCode program returned by `translateToSCode`. See
[DAE.jl](https://github.com/JKRT/DAE.jl) for the data structure.

### Flattening Models from the Modelica Standard Library

`flattenModelWithMSL` loads MSL on demand (cached after the first call) and
flattens a model from it directly:

```julia
(FM, funcs) = OMFrontend.flattenModelWithMSL(
  "Modelica.Electrical.Analog.Examples.AD_DA_conversion";
  MSL_Version = "MSL:4.0.0",
)
```

Supported versions are `"MSL:3.2.3"` and `"MSL:4.0.0"`. Pass
`forceReload = true` to drop the cached library and reparse from disk.

### Combining a User Model with Libraries

Custom libraries can be loaded once and then reused:

```julia
libKey = OMFrontend.loadLibrary("/path/to/MyLib.mo")

(FM, funcs) = OMFrontend.flattenModelWithLibraries(
  "MyPackage.Examples.Demo",
  "MyModel.mo";
  libraries = [libKey],
  MSL = true,
  MSL_Version = "MSL:4.0.0",
)
```

`loadPackageDirectory(path)` is the equivalent loader for libraries organised
as a `package.mo` directory tree.

### Inspecting and Exporting Results

A flat model and its function tree can be rendered as a string or written to
disk:

```julia
str = OMFrontend.toFlatModelica((FM, funcs))
OMFrontend.exportDAERepresentationToFile("HelloWorld.flat.mo", str)
```

For debugging the lowering pipeline, `OMFrontend.enableDumpDebug()` writes the
flat model after each compiler phase to the working directory.
`OMFrontend.disableDumpDebug()` turns it off again.

### Issues, Questions, and Contributing

OMFrontend.jl is a component of [OM.jl](https://github.com/JKRT/OM.jl), and
the API reflects that low-level role.

For questions or collaboration ideas, contact details are available on my
[LiU page](https://liu.se/en/employee/johti17).
