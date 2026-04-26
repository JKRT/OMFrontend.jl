module AvlTreeStringString

using MetaModelica
using ExportAll

import .Frontend.BaseAvlTree
using BaseAvlTree #= Modelica extend clause =#

Key = String
Value = String

@exportAll()
end
