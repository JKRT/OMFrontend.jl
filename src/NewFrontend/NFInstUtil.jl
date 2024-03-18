module InstUtil

import ..Main.Expression
import ..Main.FlatModel
import ..Main.mapExp

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
If the NF_SCALARIZE is not set the function will do nothing.
"""
function restoreMissingArrayVariables!(flatModel::FlatModel)
  if false #Flags.isSet(Flags.NF_SCALARIZE)
    function collectArrayVariable(exp::Expression)

    end

    #= Get all Equations of the ARRAY_EQUALITY type =#
     local arrayEqs = filter((x) -> x isa EQUATION_ARRAY_EQUALITY, flat_model.equations)
    #= Debugging info =#
    str = ""
    for eq in arrayEqs
      str *= string(toFlatString(eq), "\n")
    end
    write("allArrayEq.log", replace(str, "\\n" => "\n"))
    #=
    Traverse all expressions in the array equations to find component references of type array
    =#
    for aeq in arrayEqs
      mapExp(aeq, collectArrayVariable)
    end


  #   arrayEqsExpanded = Base.map(x -> mapExp(x, (y) -> Base.first(expand(y))), arrayEqs)
  #   str= Base.reduce(*, Base.map(x->toFlatString(x) * "\n", arrayEqsExpanded))
  #   write("allArrayEqExpanded.log", replace(str, "\\n" => "\n"))
  #   local arrVars = Variable[]
  #   for eq in arrayEqs
  #     @assert eq.lhs isa CREF_EXPRESSION
  #     push!(arrVars, Variable_fromCrefNoBinding(eq.lhs.cref))
  #   end
  #   arrVarsStr = Base.map(x->toFlatString(x) * "\n", arrVars)
  #   str = Base.reduce(*, arrVarsStr)
  #   write("variablesToAdd.log", str)
  #   @assign flat_model.variables = vcat(flat_model.variables, arrVars)
  #   #filter!((x) -> !(x isa EQUATION_ARRAY_EQUALITY), flat_model.equations)
  #   #@assign flat_model.equations = vcat(flat_model.equations, arrayEqsExpanded)
  #   dumpFlatModel(flat_model, name * "afterAddingArrayVarsBack")
   end
end



end #= InstUtil =#
