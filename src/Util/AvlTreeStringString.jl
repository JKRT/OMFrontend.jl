module AvlTreeStringString

using MetaModelica
using ExportAll

import .Main.BaseAvlTree
using BaseAvlTree #= Modelica extend clause =#

Key = String
Value = String

@exportAll()
end
