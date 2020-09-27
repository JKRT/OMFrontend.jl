module P_NFVerifyModel

using MetaModelica
using ExportAll
#= Forward declarations for uniontypes until Julia adds support for mutual recursion =#

@UniontypeDecl NFVerifyModel

import ..NFFlatModel
FlatModel = NFFlatModel
import ..P_NFExpandExp
P_ExpandExp = P_NFExpandExp
ExpandExp = P_NFExpandExp.NFExpandExp
import ..P_NFExpression
P_Expression = P_NFExpression
Expression = P_NFExpression.NFExpression
import ..P_NFEquation
P_Equation = P_NFEquation
Equation = P_NFEquation.NFEquation
import ..P_NFComponentRef
P_ComponentRef = P_NFComponentRef
ComponentRef = P_NFComponentRef.NFComponentRef
import ..ExecStat.execStat
import ..ElementSource
import DAE
import ..ErrorTypes
import ..Error
import ListUtil

function verify(flatModel::FlatModel)
  for eq in flatModel.equations
    verifyEquation(eq)
  end
  return execStat(getInstanceName())
end

function expandCrefSet(crefs::List{<:ComponentRef})::List{ComponentRef}
  local outCrefs::List{ComponentRef} = nil

  local exp::Expression
  local expl::List{Expression}

  for cref in crefs
    @assign exp = P_Expression.Expression.fromCref(cref)
    @assign exp = P_ExpandExp.ExpandExp.expandCref(exp)
    if P_Expression.Expression.isArray(exp)
      @assign expl = P_Expression.Expression.arrayElements(exp)
      @assign outCrefs =
        listAppend(List(P_Expression.Expression.toCref(e) for e in expl), outCrefs)
    else
      @assign outCrefs = _cons(cref, outCrefs)
    end
  end
  @assign outCrefs = ListUtil.sort(outCrefs, isGreater)
  @assign outCrefs = ListUtil.sortedUnique(outCrefs, isEqual)
  return outCrefs
end

function checkCrefSetEquality(
  crefs1::List{<:ComponentRef},
  crefs2::List{<:ComponentRef},
  errMsg::ErrorTypes.Message,
  source::DAE.ElementSource,
)
  #=  Assume the user isn't mixing different ways of subscripting array
  =#
  #=  varibles in the different branches, and just check the sets as is.
  =#
  if ListUtil.isEqualOnTrue(crefs1, crefs2, isEqual)
    return
  end
  #=  If the sets didn't match, expand arrays into scalars and try again.
  =#
  if ListUtil.isEqualOnTrue(
    expandCrefSet(crefs1),
    expandCrefSet(crefs2),
    isEqual,
  )
    return
  end
  #=  Couldn't get the sets to match, print an error and fail.
  =#
  Error.addSourceMessage(errMsg, nil, ElementSource.getInfo(source))
  return fail()
end

""" #= Checks that the left-hand sides of the given if-equation branches consists
     of the same set of crefs, and adds that set to the given set of crefs. =#"""
function whenEquationIfCrefs(
  branches::List{<:Equation},
  source::DAE.ElementSource,
  crefs::List{<:ComponentRef},
)::List{ComponentRef}

  local crefs1::List{ComponentRef}
  local crefs2::List{ComponentRef}
  local rest_branches::List{P_Equation.Equation}
  local body::List{Equation}

  @match _cons(P_Equation.Equation.BRANCH(body = body), rest_branches) = branches
  @assign crefs1 = whenEquationBranchCrefs(body)
  for branch in rest_branches
    @match P_Equation.Equation.BRANCH(body = body) = branch
    @assign crefs2 = whenEquationBranchCrefs(body)
    checkCrefSetEquality(crefs1, crefs2, Error.WHEN_IF_VARIABLE_MISMATCH, source)
  end
  #=  All the branches must have the same set of crefs on the lhs.
  =#
  @assign crefs = listAppend(crefs1, crefs)
  return crefs
end

function whenEquationEqualityCrefs(
  lhsExp::Expression,
  crefs::List{<:ComponentRef},
)::List{ComponentRef}

  @assign crefs = begin
    @match lhsExp begin
      CREF_EXPRESSION(__) => begin
        _cons(lhsExp.cref, crefs)
      end

      TUPLE_EXPRESSION(__) => begin
        ListUtil.fold(lhsExp.elements, whenEquationEqualityCrefs, crefs)
      end
    end
  end
  return crefs
end

""" #= Helper function to verifyWhenEquation, returns the set of crefs that the
     given list of equations contains on the lhs. =#"""
function whenEquationBranchCrefs(eql::List{<:Equation})::List{ComponentRef}
  local crefs::List{ComponentRef} = nil

  for eq in eql
    @assign crefs = begin
      @match eq begin
        EQUATION_EQUALITY(__) => begin
          whenEquationEqualityCrefs(eq.lhs, crefs)
        end

        EQUATION_CREF_EQUALITY(__) => begin
          _cons(eq.lhs, crefs)
        end

        EQUATION_ARRAY_EQUALITY(__) => begin
          whenEquationEqualityCrefs(eq.lhs, crefs)
        end

        EQUATION_REINIT(__) => begin
          whenEquationEqualityCrefs(eq.cref, crefs)
        end

        EQUATION_IF(__) => begin
          whenEquationIfCrefs(eq.branches, eq.source, crefs)
        end

        _ => begin
          crefs
        end
      end
    end
  end
  @assign crefs = ListUtil.sort(crefs, isGreater)
  @assign crefs = ListUtil.sortedUnique(crefs, isEqual)
  return crefs
end

""" #= Checks that each branch in a when-equation has the same set of crefs on the lhs. =#"""
function verifyWhenEquation(
  branches::List{<:Equation},
  source::DAE.ElementSource,
)
  local crefs1::List{ComponentRef}
  local crefs2::List{ComponentRef}
  local rest_branches::List{P_Equation.Equation}
  local body::List{Equation}

  #=  Only when-equation with more than one branch needs to be checked.
  =#
  if ListUtil.hasOneElement(branches)
    return
  end
  @match _cons(P_Equation.Equation.BRANCH(body = body), rest_branches) = branches
  @assign crefs1 = whenEquationBranchCrefs(body)
  return for branch in rest_branches
    @match P_Equation.Equation.BRANCH(body = body) = branch
    @assign crefs2 = whenEquationBranchCrefs(body)
    checkCrefSetEquality(
      crefs1,
      crefs2,
      Error.DIFFERENT_VARIABLES_SOLVED_IN_ELSEWHEN,
      source,
    )
  end
end

function verifyEquation(eq::Equation)
  return @assign () = begin
    @match eq begin
      P_Equation.Equation.WHEN(__) => begin
        verifyWhenEquation(eq.branches, eq.source)
        ()
      end

      _ => begin
        ()
      end
    end
  end
end

@Uniontype NFVerifyModel begin end

@exportAll()
end
