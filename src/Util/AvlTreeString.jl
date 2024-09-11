module AvlTreeString

using MetaModelica
using ExportAll

import .Frontend.BaseAvlTree
using BaseAvlTree #= Modelica extend clause =#

Key = String
Value = Integer

@exportAll()
end
