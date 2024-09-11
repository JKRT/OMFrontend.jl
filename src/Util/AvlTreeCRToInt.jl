module AvlTreeCRToInt

using MetaModelica
using ExportAll

import .Frontend.BaseAvlTree
import DAE

import .Frontend.ComponentReference

using BaseAvlTree #= Modelica extend clause =#

Key = DAE.ComponentRef
Value = Integer

@exportAll()
end
