module InstUtil

import ..Frontend.Expression
import ..Frontend.FlatModel
import ..Frontend.map

using ..Frontend
using MetaModelica

"""
```
restoreMissingArrayVariables!(flatModel::FlatModel)
```
  Some variables are never defined in the flat model.
  They can appear everywhere as expressions,
  the easiest case is where these variables occur as
  lhs = rhs, but they may also appear in binary expressions etc.
    The easiest way to deal with the is to create one variable
    for each cref of an array type where the parent cref is not a record.

  Each of these variables can be assumed to have been scalarized previously.
  Hence, we know the size and the component of the particular vector.
  For each such array variable appearing in an equation
  we create the variable and an additional equation.
If the NF_SCALARIZE flag is set to false the function will do nothing.
"""
function restoreMissingArrayVariables!(flatModel::FlatModel)
  local arrayVariableSet = Dict{String, ComponentRef}()
  if Flags.isSet(Flags.NF_SCALARIZE)
    function collectArrayVariable2(exp::Expression)
      local outExp = @match exp begin
        CREF_EXPRESSION(ty, cref) where {ty isa TYPE_ARRAY} =>  begin
          local parentCref = cref.restCref
          local parentIsRecord = isRecord(getComponentType(parentCref))
          if !(parentIsRecord)
            arrayVariableSet[toFlatString(cref)] = cref
          end
          #@info "before expansion" toString(exp)
          (expArr, _) = expand(exp)
          #@info "after expansion" toString(expArr)
          expArr
        end
        _ => begin
          exp
        end
      end
      return outExp
    end

    function collectArrayVariable(e::Expression)
      exp = map(e, collectArrayVariable2)
      return exp
    end
    #=
    Traverse all expressions in the array equations to find component references of type array
    =#
    mapExpList!(flatModel.initialEquations, collectArrayVariable)
    mapExpList!(flatModel.equations, collectArrayVariable)
    newVars = VARIABLE[]
    for v in values(arrayVariableSet)
      push!(newVars, Variable_fromCrefNoBinding(v))
    end
    #append!(flatModel.variables, newVars)
   end
end
end #= InstUtil =#
