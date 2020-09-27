module AvlTreeCRToInt

using MetaModelica
using ExportAll

import .Main.BaseAvlTree
import DAE

import .Main.ComponentReference

using BaseAvlTree #= Modelica extend clause =#

Key = DAE.ComponentRef
Value = Integer

@exportAll()
end
