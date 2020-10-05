module AbsynUtil

using MetaModelica
using ExportAll
#= Forward declarations for uniontypes until Julia adds support for mutual recursion =#

FuncTplToTpl = Function

FuncType = Function

FuncType = Function

FuncTplToTpl = Function

FuncType = Function

FuncType = Function

FuncType = Function

FuncType = Function

FuncType = Function

FuncType = Function

FuncType = Function

FuncType = Function

FuncType = Function

FuncType = Function

FuncType = Function

FuncType = Function

FuncType = Function

FuncType = Function

FuncType = Function

FuncType = Function

FuncType = Function

FuncType = Function

FuncType = Function

FuncType = Function

ModFunc = Function

ModFunc = Function

MapFunc = Function

FuncType = Function

FuncType = Function

FuncType = Function

FuncType = Function

FuncType = Function

FuncType = Function

FuncType = Function

FuncT = Function

FuncT = Function

FuncT = Function

FuncType = Function

FuncType = Function

FuncType = Function

FuncType = Function

FuncType = Function

FuncType = Function

#= /*
* This file is part of OpenModelica.
*
* Copyright (c) 1998-CurrentYear, Open Source Modelica Consortium (OSMC),
* c/o Linköpings universitet, Department of Computer and Information Science,
* SE-58183 Linköping, Sweden.
*
* All rights reserved.
*
* THIS Absyn.PROGRAM IS PROVIDED UNDER THE TERMS OF GPL VERSION 3 LICENSE OR
* THIS OSMC PUBLIC LICENSE (OSMC-PL) VERSION 1.2.
* ANY USE, REPRODUCTION OR DISTRIBUTION OF THIS Absyn.PROGRAM CONSTITUTES
* RECIPIENT'S ACCEPTANCE OF THE OSMC PUBLIC LICENSE OR THE GPL VERSION 3,
* ACCORDING TO RECIPIENTS CHOICE.
*
* The OpenModelica software and the Open Source Modelica
* Consortium (OSMC) Public License (OSMC-PL) are obtained
* from OSMC, either from the above address,
* from the URLs: http:www.ida.liu.se/projects/OpenModelica or
* http:www.openmodelica.org, and in the OpenModelica distribution.
* GNU version 3 is obtained from: http:www.gnu.org/copyleft/gpl.html.
*
* This program is distributed WITHOUT ANY WARRANTY; without
* even the implied warranty of  MERCHANTABILITY or FITNESS
* FOR A PARTICULAR PURPOSE, EXCEPT AS EXPRESSLY SET FORTH
* IN THE BY RECIPIENT SELECTED SUBSIDIARY LICENSE CONDITIONS OF OSMC-PL.
*
* See the full OSMC Public License conditions for more details.
*
*/ =#

import Absyn

import ListUtil

import ..Main.Util

const dummyParts = Absyn.PARTS(nil, nil, nil, nil, NONE())::Absyn.ClassDef

const dummyInfo = Absyn.SOURCEINFO("", false, 0, 0, 0, 0, 0.0)::Absyn.Info

const dummyProgram = Absyn.PROGRAM(nil, Absyn.TOP())::Absyn.Program
TypeA = Any
Type_a = Any
Argument = Any
Arg = Any
#=  stefan
=#

""" #= Traverses all subequations of an equation.
   Takes a function and an extra argument passed through the traversal =#"""
function traverseEquation(
  inEquation::Absyn.Equation,
  inFunc::FuncTplToTpl,
  inTypeA::TypeA,
)::Tuple{Absyn.Equation, TypeA}
  local outTpl::Tuple{Absyn.Equation, TypeA}

  @assign outTpl = begin
    local arg::TypeA
    local arg_1::TypeA
    local arg_2::TypeA
    local arg_3::TypeA
    local arg_4::TypeA
    local eq::Absyn.Equation
    local eq_1::Absyn.Equation
    local rel::FuncTplToTpl
    local e::Absyn.Exp
    local e_1::Absyn.Exp
    local eqilst::List{Absyn.EquationItem}
    local eqilst1::List{Absyn.EquationItem}
    local eqilst2::List{Absyn.EquationItem}
    local eqilst_1::List{Absyn.EquationItem}
    local eqilst1_1::List{Absyn.EquationItem}
    local eqilst2_1::List{Absyn.EquationItem}
    local eeqitlst::List{Tuple{Absyn.Exp, List{Absyn.EquationItem}}}
    local eeqitlst_1::List{Tuple{Absyn.Exp, List{Absyn.EquationItem}}}
    local fis::Absyn.ForIterators
    local ei::Absyn.EquationItem
    local fis_1::Absyn.ForIterators
    local ei_1::Absyn.EquationItem
    @matchcontinue (inEquation, inFunc, inTypeA) begin
      (eq && Absyn.EQ_IF(e, eqilst1, eeqitlst, eqilst2), rel, arg) => begin
        @assign (eqilst1_1, arg_1) = traverseEquationItemList(eqilst1, rel, arg)
        @assign (eeqitlst_1, arg_2) = traverseExpEqItemTupleList(eeqitlst, rel, arg_1)
        @assign (eqilst2_1, arg_3) = traverseEquationItemList(eqilst2, rel, arg_2)
        @match (Absyn.EQ_IF(), arg_4) = rel((eq, arg_3))
        (Absyn.EQ_IF(e, eqilst1_1, eeqitlst_1, eqilst2_1), arg_4)
      end

      (eq && Absyn.EQ_FOR(_, eqilst), rel, arg) => begin
        @assign (eqilst_1, arg_1) = traverseEquationItemList(eqilst, rel, arg)
        @match (Absyn.EQ_FOR(fis_1, _), arg_2) = rel((eq, arg_1))
        (Absyn.EQ_FOR(fis_1, eqilst_1), arg_2)
      end

      (eq && Absyn.EQ_WHEN_E(_, eqilst, eeqitlst), rel, arg) => begin
        @assign (eqilst_1, arg_1) = traverseEquationItemList(eqilst, rel, arg)
        @assign (eeqitlst_1, arg_2) = traverseExpEqItemTupleList(eeqitlst, rel, arg_1)
        @match (Absyn.EQ_WHEN_E(e_1, _, _), arg_3) = rel((eq, arg_2))
        (Absyn.EQ_WHEN_E(e_1, eqilst_1, eeqitlst_1), arg_3)
      end

      (eq && Absyn.EQ_FAILURE(ei), rel, arg) => begin
        @assign (ei_1, arg_1) = traverseEquationItem(ei, rel, arg)
        @match (Absyn.EQ_FAILURE(), arg_2) = rel((eq, arg_1))
        (Absyn.EQ_FAILURE(ei_1), arg_2)
      end

      (eq, rel, arg) => begin
        @assign (eq_1, arg_1) = rel((eq, arg))
        (eq_1, arg_1)
      end
    end
  end
  return outTpl
end

#=  stefan
=#

""" #= Traverses the equation inside an equationitem =#"""
function traverseEquationItem(
  inEquationItem::Absyn.EquationItem,
  inFunc::FuncTplToTpl,
  inTypeA::TypeA,
)::Tuple{Absyn.EquationItem, TypeA}
  local outTpl::Tuple{Absyn.EquationItem, TypeA}

  @assign outTpl = begin
    local ei::Absyn.EquationItem
    local rel::FuncTplToTpl
    local arg::TypeA
    local arg_1::TypeA
    local eq::Absyn.Equation
    local eq_1::Absyn.Equation
    local oc::Option{Absyn.Comment}
    local info::Info
    @matchcontinue (inEquationItem, inFunc, inTypeA) begin
      (Absyn.EQUATIONITEM(eq, oc, info), rel, arg) => begin
        @assign (eq_1, arg_1) = traverseEquation(eq, rel, arg)
        (Absyn.EQUATIONITEM(eq_1, oc, info), arg_1)
      end

      (ei, _, arg) => begin
        (ei, arg)
      end
    end
  end
  return outTpl
end

#=  stefan
=#

""" #= calls traverseEquationItem on every element of the given list =#"""
function traverseEquationItemList(
  inEquationItemList::List{<:Absyn.EquationItem},
  inFunc::FuncTplToTpl,
  inTypeA::TypeA,
)::Tuple{List{Absyn.EquationItem}, TypeA}
  local outTpl::Tuple{List{Absyn.EquationItem}, TypeA}

  local arg2::TypeA = inTypeA

  @assign outTpl = (
    List(
      begin
        local ei::Absyn.EquationItem
        local ei_1::Absyn.EquationItem
        @match el begin
          ei => begin
            @assign (ei_1, arg2) = traverseEquationItem(ei, inFunc, arg2)
            ei_1
          end
        end
      end for el in inEquationItemList
    ),
    arg2,
  )
  return outTpl
end

#=  stefan
=#

""" #= traverses a list of Absyn.Exp * Absyn.EquationItem list tuples
  mostly used for else-if blocks =#"""
function traverseExpEqItemTupleList(
  inList::List{<:Tuple{<:Absyn.Exp, List{<:Absyn.EquationItem}}},
  inFunc::FuncTplToTpl,
  inTypeA::TypeA,
)::Tuple{List{Tuple{Absyn.Exp, List{Absyn.EquationItem}}}, TypeA}
  local outTpl::Tuple{List{Tuple{Absyn.Exp, List{Absyn.EquationItem}}}, TypeA}

  local arg2::TypeA = inTypeA

  @assign outTpl = (
    List(
      begin
        local e::Absyn.Exp
        local eilst::List{Absyn.EquationItem}
        local eilst_1::List{Absyn.EquationItem}
        @match el begin
          (e, eilst) => begin
            @assign (eilst_1, arg2) = traverseEquationItemList(eilst, inFunc, arg2)
            (e, eilst_1)
          end
        end
      end for el in inList
    ),
    arg2,
  )
  return outTpl
end

#=  stefan
=#

""" #= Traverses all subalgorithms of an algorithm
  Takes a function and an extra argument passed through the traversal =#"""
function traverseAlgorithm(
  inAlgorithm::Absyn.Algorithm,
  inFunc::FuncTplToTpl,
  inTypeA::TypeA,
)::Tuple{Absyn.Algorithm, TypeA}
  local outTpl::Tuple{Absyn.Algorithm, TypeA}

  @assign outTpl = begin
    local arg::TypeA
    local arg_1::TypeA
    local arg1_1::TypeA
    local arg2_1::TypeA
    local arg3_1::TypeA
    local alg::Absyn.Algorithm
    local alg_1::Absyn.Algorithm
    local alg1_1::Absyn.Algorithm
    local alg2_1::Absyn.Algorithm
    local alg3_1::Absyn.Algorithm
    local ailst::List{Absyn.AlgorithmItem}
    local ailst1::List{Absyn.AlgorithmItem}
    local ailst2::List{Absyn.AlgorithmItem}
    local ailst_1::List{Absyn.AlgorithmItem}
    local ailst1_1::List{Absyn.AlgorithmItem}
    local ailst2_1::List{Absyn.AlgorithmItem}
    local eaitlst::List{Tuple{Absyn.Exp, List{Absyn.AlgorithmItem}}}
    local eaitlst_1::List{Tuple{Absyn.Exp, List{Absyn.AlgorithmItem}}}
    local rel::FuncTplToTpl
    local ai::Absyn.AlgorithmItem
    local ai_1::Absyn.AlgorithmItem
    local e::Absyn.Exp
    local e_1::Absyn.Exp
    local fis::Absyn.ForIterators
    local fis_1::Absyn.ForIterators
    @matchcontinue (inAlgorithm, inFunc, inTypeA) begin
      (alg && Absyn.ALG_IF(_, ailst1, eaitlst, ailst2), rel, arg) => begin
        @assign (ailst1_1, arg1_1) = traverseAlgorithmItemList(ailst1, rel, arg)
        @assign (eaitlst_1, arg2_1) = traverseExpAlgItemTupleList(eaitlst, rel, arg1_1)
        @assign (ailst2_1, arg3_1) = traverseAlgorithmItemList(ailst2, rel, arg2_1)
        @match (Absyn.ALG_IF(e_1, _, _, _), arg_1) = rel((alg, arg3_1))
        (Absyn.ALG_IF(e_1, ailst1_1, eaitlst_1, ailst2_1), arg_1)
      end

      (alg && Absyn.ALG_FOR(_, ailst), rel, arg) => begin
        @assign (ailst_1, arg1_1) = traverseAlgorithmItemList(ailst, rel, arg)
        @match (Absyn.ALG_FOR(fis_1, _), arg_1) = rel((alg, arg1_1))
        (Absyn.ALG_FOR(fis_1, ailst_1), arg_1)
      end

      (alg && Absyn.ALG_PARFOR(_, ailst), rel, arg) => begin
        @assign (ailst_1, arg1_1) = traverseAlgorithmItemList(ailst, rel, arg)
        @match (Absyn.ALG_PARFOR(fis_1, _), arg_1) = rel((alg, arg1_1))
        (Absyn.ALG_PARFOR(fis_1, ailst_1), arg_1)
      end

      (alg && Absyn.ALG_WHILE(_, ailst), rel, arg) => begin
        @assign (ailst_1, arg1_1) = traverseAlgorithmItemList(ailst, rel, arg)
        @match (Absyn.ALG_WHILE(e_1, _), arg_1) = rel((alg, arg1_1))
        (Absyn.ALG_WHILE(e_1, ailst_1), arg_1)
      end

      (alg && Absyn.ALG_WHEN_A(_, ailst, eaitlst), rel, arg) => begin
        @assign (ailst_1, arg1_1) = traverseAlgorithmItemList(ailst, rel, arg)
        @assign (eaitlst_1, arg2_1) = traverseExpAlgItemTupleList(eaitlst, rel, arg1_1)
        @match (Absyn.ALG_WHEN_A(e_1, _, _), arg_1) = rel((alg, arg2_1))
        (Absyn.ALG_WHEN_A(e_1, ailst_1, eaitlst_1), arg_1)
      end

      (alg, rel, arg) => begin
        @assign (alg_1, arg_1) = rel((alg, arg))
        (alg_1, arg_1)
      end
    end
  end
  return outTpl
end

#=  stefan
=#

""" #= traverses the Absyn.Algorithm contained in an Absyn.AlgorithmItem, if any
  see traverseAlgorithm =#"""
function traverseAlgorithmItem(
  inAlgorithmItem::Absyn.AlgorithmItem,
  inFunc::FuncTplToTpl,
  inTypeA::TypeA,
)::Tuple{Absyn.AlgorithmItem, TypeA}
  local outTpl::Tuple{Absyn.AlgorithmItem, TypeA}

  @assign outTpl = begin
    local rel::FuncTplToTpl
    local arg::TypeA
    local arg_1::TypeA
    local alg::Absyn.Algorithm
    local alg_1::Absyn.Algorithm
    local oc::Option{Absyn.Comment}
    local ai::Absyn.AlgorithmItem
    local info::Info
    @matchcontinue (inAlgorithmItem, inFunc, inTypeA) begin
      (Absyn.ALGORITHMITEM(alg, oc, info), rel, arg) => begin
        @assign (alg_1, arg_1) = traverseAlgorithm(alg, rel, arg)
        (Absyn.ALGORITHMITEM(alg_1, oc, info), arg_1)
      end

      (ai, _, arg) => begin
        (ai, arg)
      end
    end
  end
  return outTpl
end

#=  stefan
=#

""" #= calls traverseAlgorithmItem on each item in a list of AlgorithmItems =#"""
function traverseAlgorithmItemList(
  inAlgorithmItemList::List{<:Absyn.AlgorithmItem},
  inFunc::FuncTplToTpl,
  inTypeA::TypeA,
)::Tuple{List{Absyn.AlgorithmItem}, TypeA}
  local outTpl::Tuple{List{Absyn.AlgorithmItem}, TypeA}

  @assign outTpl = begin
    local rel::FuncTplToTpl
    local arg::TypeA
    local arg_1::TypeA
    local arg_2::TypeA
    local ai::Absyn.AlgorithmItem
    local ai_1::Absyn.AlgorithmItem
    local cdr::List{Absyn.AlgorithmItem}
    local cdr_1::List{Absyn.AlgorithmItem}
    @match (inAlgorithmItemList, inFunc, inTypeA) begin
      (nil(), _, arg) => begin
        (nil, arg)
      end

      (ai <| cdr, rel, arg) => begin
        @assign (ai_1, arg_1) = traverseAlgorithmItem(ai, rel, arg)
        @assign (cdr_1, arg_2) = traverseAlgorithmItemList(cdr, rel, arg_1)
        (_cons(ai_1, cdr_1), arg_2)
      end
    end
  end
  return outTpl
end

#=  stefan
=#

""" #= traverses a list of Absyn.Exp * Absyn.AlgorithmItem list tuples
  mostly used for else-if blocks =#"""
function traverseExpAlgItemTupleList(
  inList::List{<:Tuple{<:Absyn.Exp, List{<:Absyn.AlgorithmItem}}},
  inFunc::FuncTplToTpl,
  inTypeA::TypeA,
)::Tuple{List{Tuple{Absyn.Exp, List{Absyn.AlgorithmItem}}}, TypeA}
  local outTpl::Tuple{List{Tuple{Absyn.Exp, List{Absyn.AlgorithmItem}}}, TypeA}

  @assign outTpl = begin
    local rel::FuncTplToTpl
    local arg::TypeA
    local arg_1::TypeA
    local arg_2::TypeA
    local cdr::List{Tuple{Absyn.Exp, List{Absyn.AlgorithmItem}}}
    local cdr_1::List{Tuple{Absyn.Exp, List{Absyn.AlgorithmItem}}}
    local e::Absyn.Exp
    local ailst::List{Absyn.AlgorithmItem}
    local ailst_1::List{Absyn.AlgorithmItem}
    @match (inList, inFunc, inTypeA) begin
      (nil(), _, arg) => begin
        (nil, arg)
      end

      ((e, ailst) <| cdr, rel, arg) => begin
        @assign (ailst_1, arg_1) = traverseAlgorithmItemList(ailst, rel, arg)
        @assign (cdr_1, arg_2) = traverseExpAlgItemTupleList(cdr, rel, arg_1)
        (_cons((e, ailst_1), cdr_1), arg_2)
      end
    end
  end
  return outTpl
end

""" #=  Traverses all subexpressions of an Absyn.Exp expression.
  Takes a function and an extra argument passed through the traversal.
  NOTE:This function was copied from Expression.traverseExpression. =#"""
function traverseExp(inExp::Absyn.Exp, inFunc::FuncType, inArg::Type_a)::Tuple{Absyn.Exp, Type_a}
  local outArg::Type_a
  local outExp::Absyn.Exp

  @assign (outExp, outArg) = traverseExpBidir(inExp, dummyTraverseExp, inFunc, inArg)
  return (outExp, outArg)
end

""" #=  Traverses all subexpressions of an Absyn.Exp expression.
  Takes a function and an extra argument passed through the traversal. =#"""
function traverseExpTopDown(inExp::Absyn.Exp, inFunc::FuncType, inArg::Type_a)::Tuple{Absyn.Exp, Type_a}
  local outArg::Type_a
  local outExp::Absyn.Exp

  @assign (outExp, outArg) = traverseExpBidir(inExp, inFunc, dummyTraverseExp, inArg)
  return (outExp, outArg)
end

""" #= calls traverseExp on each element in the given list =#"""
function traverseExpList(
  inExpList::List{<:Absyn.Exp},
  inFunc::FuncTplToTpl,
  inArg::Type_a,
)::Tuple{List{Absyn.Exp}, Type_a}
  local outArg::Type_a
  local outExpList::List{Absyn.Exp}

  @assign (outExpList, outArg) =
    traverseExpListBidir(inExpList, dummyTraverseExp, inFunc, inArg)
  return (outExpList, outArg)
end

""" #= Traverses a list of expressions, calling traverseExpBidir on each
  expression. =#"""
function traverseExpListBidir(
  inExpl::List{<:Absyn.Exp},
  enterFunc::FuncType,
  exitFunc::FuncType,
  inArg::Argument,
)::Tuple{List{Absyn.Exp}, Argument}
  local outArg::Argument
  local outExpl::List{Absyn.Exp}

  @assign (outExpl, outArg) =
    ListUtil.map2FoldCheckReferenceEq(inExpl, traverseExpBidir, enterFunc, exitFunc, inArg)
  return (outExpl, outArg)
end

""" #= This function takes an expression and a tuple with an enter function, an exit
  function, and an extra argument. For each expression it encounters it calls
  the enter function with the expression and the extra argument. It then
  traverses all subexpressions in the expression and calls traverseExpBidir on
  them with the updated argument. Finally it calls the exit function, again with
  the updated argument. This means that this function is bidirectional, and can
  be used to emulate both top-down and bottom-up traversal. =#"""
function traverseExpBidir(
  inExp::Absyn.Exp,
  enterFunc::FuncType,
  exitFunc::FuncType,
  inArg::Argument,
)::Tuple{Absyn.Exp, Argument}
  local arg::Argument
  local e::Absyn.Exp

  @assign (e, arg) = enterFunc(inExp, inArg)
  @assign (e, arg) = traverseExpBidirSubExps(e, enterFunc, exitFunc, arg)
  @assign (e, arg) = exitFunc(e, arg)
  return (e, arg)
end

""" #= Same as traverseExpBidir, but with an optional expression. Calls
  traverseExpBidir if the option is SOME(), or just returns the input if it's
  NONE() =#"""
function traverseExpOptBidir(
  inExp::Option{<:Absyn.Exp},
  enterFunc::FuncType,
  exitFunc::FuncType,
  inArg::Argument,
)::Tuple{Option{Absyn.Exp}, Argument}
  local arg::Argument
  local outExp::Option{Absyn.Exp}

  @assign (outExp, arg) = begin
    local e1::Absyn.Exp
    local e2::Absyn.Exp
    local tup::Tuple{FuncType, FuncType, Argument}
    @match (inExp, enterFunc, exitFunc, inArg) begin
      (SOME(e1), _, _, _) => begin
        @assign (e2, arg) = traverseExpBidir(e1, enterFunc, exitFunc, inArg)
        (if referenceEq(e1, e2)
          inExp
        else
          SOME(e2)
        end, arg)
      end

      _ => begin
        (inExp, inArg)
      end
    end
  end
  return (outExp, arg)
end

""" #= Helper function to traverseExpBidir. Traverses the subexpressions of an
  expression and calls traverseExpBidir on them. =#"""
function traverseExpBidirSubExps(
  inExp::Absyn.Exp,
  enterFunc::FuncType,
  exitFunc::FuncType,
  inArg::Argument,
)::Tuple{Absyn.Exp, Argument}
  local arg::Argument
  local e::Absyn.Exp

  @assign (e, arg) = begin
    local e1::Absyn.Exp
    local e1m::Absyn.Exp
    local e2::Absyn.Exp
    local e2m::Absyn.Exp
    local e3::Absyn.Exp
    local e3m::Absyn.Exp
    local oe1::Option{Absyn.Exp}
    local oe1m::Option{Absyn.Exp}
    local tup::Tuple{FuncType, FuncType, Argument}
    local op::Absyn.Operator
    local cref::Absyn.ComponentRef
    local crefm::Absyn.ComponentRef
    local else_ifs1::List{Tuple{Absyn.Exp, Absyn.Exp}}
    local else_ifs2::List{Tuple{Absyn.Exp, Absyn.Exp}}
    local expl1::List{Absyn.Exp}
    local expl2::List{Absyn.Exp}
    local mat_expl::List{List{Absyn.Exp}}
    local fargs1::Absyn.FunctionArgs
    local fargs2::Absyn.FunctionArgs
    local error_msg::String
    local id::Absyn.Ident
    local enterName::Absyn.Ident
    local exitName::Absyn.Ident
    local match_ty::Absyn.MatchType
    local match_decls::List{Absyn.ElementItem}
    local match_cases::List{Absyn.Case}
    local cmt::Option{String}
    @match (inExp, enterFunc, exitFunc, inArg) begin
      (Absyn.INTEGER(__), _, _, _) => begin
        (inExp, inArg)
      end

      (Absyn.REAL(__), _, _, _) => begin
        (inExp, inArg)
      end

      (Absyn.STRING(__), _, _, _) => begin
        (inExp, inArg)
      end

      (Absyn.BOOL(__), _, _, _) => begin
        (inExp, inArg)
      end

      (Absyn.CREF(componentRef = cref), _, _, arg) => begin
        @assign (crefm, arg) = traverseExpBidirCref(cref, enterFunc, exitFunc, arg)
        (if referenceEq(cref, crefm)
          inExp
        else
          Absyn.CREF(crefm)
        end, arg)
      end

      (Absyn.BINARY(exp1 = e1, op = op, exp2 = e2), _, _, arg) => begin
        @assign (e1m, arg) = traverseExpBidir(e1, enterFunc, exitFunc, arg)
        @assign (e2m, arg) = traverseExpBidir(e2, enterFunc, exitFunc, arg)
        (if referenceEq(e1, e1m) && referenceEq(e2, e2m)
          inExp
        else
          Absyn.BINARY(e1m, op, e2m)
        end, arg)
      end

      (Absyn.UNARY(op = op, exp = e1), _, _, arg) => begin
        @assign (e1m, arg) = traverseExpBidir(e1, enterFunc, exitFunc, arg)
        (if referenceEq(e1, e1m)
          inExp
        else
          Absyn.UNARY(op, e1m)
        end, arg)
      end

      (Absyn.LBINARY(exp1 = e1, op = op, exp2 = e2), _, _, arg) => begin
        @assign (e1m, arg) = traverseExpBidir(e1, enterFunc, exitFunc, arg)
        @assign (e2m, arg) = traverseExpBidir(e2, enterFunc, exitFunc, arg)
        (if referenceEq(e1, e1m) && referenceEq(e2, e2m)
          inExp
        else
          Absyn.LBINARY(e1m, op, e2m)
        end, arg)
      end

      (Absyn.LUNARY(op = op, exp = e1), _, _, arg) => begin
        @assign (e1m, arg) = traverseExpBidir(e1, enterFunc, exitFunc, arg)
        (if referenceEq(e1, e1m)
          inExp
        else
          Absyn.LUNARY(op, e1m)
        end, arg)
      end

      (Absyn.RELATION(exp1 = e1, op = op, exp2 = e2), _, _, arg) => begin
        @assign (e1m, arg) = traverseExpBidir(e1, enterFunc, exitFunc, arg)
        @assign (e2m, arg) = traverseExpBidir(e2, enterFunc, exitFunc, arg)
        (if referenceEq(e1, e1m) && referenceEq(e2, e2m)
          inExp
        else
          Absyn.RELATION(e1m, op, e2m)
        end, arg)
      end

      (
        Absyn.IFEXP(ifExp = e1, trueBranch = e2, elseBranch = e3, elseIfBranch = else_ifs1),
        _,
        _,
        arg,
      ) => begin
        @assign (e1m, arg) = traverseExpBidir(e1, enterFunc, exitFunc, arg)
        @assign (e2m, arg) = traverseExpBidir(e2, enterFunc, exitFunc, arg)
        @assign (e3m, arg) = traverseExpBidir(e3, enterFunc, exitFunc, arg)
        @assign (else_ifs2, arg) = ListUtil.map2FoldCheckReferenceEq(
          else_ifs1,
          traverseExpBidirElseIf,
          enterFunc,
          exitFunc,
          arg,
        )
        (
          if referenceEq(e1, e1m) &&
             referenceEq(e2, e2m) &&
             referenceEq(e3, e3m) &&
             referenceEq(else_ifs1, else_ifs2)
            inExp
          else
            Absyn.IFEXP(e1m, e2m, e3m, else_ifs2)
          end,
          arg,
        )
      end

      (Absyn.CALL(function_ = cref, functionArgs = fargs1), _, _, arg) => begin
        @assign (fargs2, arg) =
          traverseExpBidirFunctionArgs(fargs1, enterFunc, exitFunc, arg)
        (if referenceEq(fargs1, fargs2)
          inExp
        else
          Absyn.CALL(cref, fargs2)
        end, arg)
      end

      (Absyn.PARTEVALFUNCTION(function_ = cref, functionArgs = fargs1), _, _, arg) => begin
        @assign (fargs2, arg) =
          traverseExpBidirFunctionArgs(fargs1, enterFunc, exitFunc, arg)
        (if referenceEq(fargs1, fargs2)
          inExp
        else
          Absyn.PARTEVALFUNCTION(cref, fargs2)
        end, arg)
      end

      (Absyn.ARRAY(arrayExp = expl1), _, _, arg) => begin
        @assign (expl2, arg) = traverseExpListBidir(expl1, enterFunc, exitFunc, arg)
        (if referenceEq(expl1, expl2)
          inExp
        else
          Absyn.ARRAY(expl2)
        end, arg)
      end

      (Absyn.MATRIX(matrix = mat_expl), _, _, arg) => begin
        @assign (mat_expl, arg) = ListUtil.map2FoldCheckReferenceEq(
          mat_expl,
          traverseExpListBidir,
          enterFunc,
          exitFunc,
          arg,
        )
        (Absyn.MATRIX(mat_expl), arg)
      end

      (Absyn.RANGE(start = e1, step = oe1, stop = e2), _, _, arg) => begin
        @assign (e1m, arg) = traverseExpBidir(e1, enterFunc, exitFunc, arg)
        @assign (oe1m, arg) = traverseExpOptBidir(oe1, enterFunc, exitFunc, arg)
        @assign (e2m, arg) = traverseExpBidir(e2, enterFunc, exitFunc, arg)
        (if referenceEq(e1, e1m) && referenceEq(e2, e2m) && referenceEq(oe1, oe1m)
          inExp
        else
          Absyn.RANGE(e1m, oe1m, e2m)
        end, arg)
      end

      (Absyn.END(__), _, _, _) => begin
        (inExp, inArg)
      end

      (Absyn.TUPLE(expressions = expl1), _, _, arg) => begin
        @assign (expl2, arg) = traverseExpListBidir(expl1, enterFunc, exitFunc, arg)
        (if referenceEq(expl1, expl2)
          inExp
        else
          Absyn.TUPLE(expl2)
        end, arg)
      end

      (Absyn.AS(id = id, exp = e1), _, _, arg) => begin
        @assign (e1m, arg) = traverseExpBidir(e1, enterFunc, exitFunc, arg)
        (if referenceEq(e1, e1m)
          inExp
        else
          Absyn.AS(id, e1m)
        end, arg)
      end

      (Absyn.CONS(head = e1, rest = e2), _, _, arg) => begin
        @assign (e1m, arg) = traverseExpBidir(e1, enterFunc, exitFunc, arg)
        @assign (e2m, arg) = traverseExpBidir(e2, enterFunc, exitFunc, arg)
        (if referenceEq(e1, e1m) && referenceEq(e2, e2m)
          inExp
        else
          Absyn.CONS(e1m, e2m)
        end, arg)
      end

      (
        Absyn.MATCHEXP(
          matchTy = match_ty,
          inputExp = e1,
          localDecls = match_decls,
          cases = match_cases,
          comment = cmt,
        ),
        _,
        _,
        arg,
      ) => begin
        @assign (e1, arg) = traverseExpBidir(e1, enterFunc, exitFunc, arg)
        @assign (match_cases, arg) = ListUtil.map2FoldCheckReferenceEq(
          match_cases,
          traverseMatchCase,
          enterFunc,
          exitFunc,
          arg,
        )
        (Absyn.MATCHEXP(match_ty, e1, match_decls, match_cases, cmt), arg)
      end

      (Absyn.LIST(exps = expl1), _, _, arg) => begin
        @assign (expl2, arg) = traverseExpListBidir(expl1, enterFunc, exitFunc, arg)
        (if referenceEq(expl1, expl2)
          inExp
        else
          Absyn.LIST(expl2)
        end, arg)
      end

      (Absyn.CODE(__), _, _, _) => begin
        (inExp, inArg)
      end

      (Absyn.DOT(__), _, _, arg) => begin
        @assign (e1, arg) = traverseExpBidir(inExp.exp, enterFunc, exitFunc, arg)
        @assign (e2, arg) = traverseExpBidir(inExp.index, enterFunc, exitFunc, arg)
        (if referenceEq(inExp.exp, e1) && referenceEq(inExp.index, e2)
          inExp
        else
          Absyn.DOT(e1, e2)
        end, arg)
      end

      _ => begin
        @assign (_, _, enterName) = System.dladdr(enterFunc)
        @assign (_, _, exitName) = System.dladdr(exitFunc)
        @assign error_msg =
          "in traverseExpBidirSubExps(" +
          enterName +
          ", " +
          exitName +
          ") - Unknown expression: "
        @assign error_msg = error_msg + Dump.printExpStr(inExp)
        Error.addMessage(Error.INTERNAL_ERROR, list(error_msg))
        fail()
      end
    end
  end
  return (e, arg)
end

""" #= Helper function to traverseExpBidirSubExps. Traverses any expressions in a
  component reference (i.e. in it's subscripts). =#"""
function traverseExpBidirCref(
  inCref::Absyn.ComponentRef,
  enterFunc::FuncType,
  exitFunc::FuncType,
  inArg::Argument,
)::Tuple{Absyn.ComponentRef, Argument}
  local arg::Argument
  local outCref::Absyn.ComponentRef

  @assign (outCref, arg) = begin
    local name::Absyn.Ident
    local cr1::Absyn.ComponentRef
    local cr2::Absyn.ComponentRef
    local subs1::List{Absyn.Subscript}
    local subs2::List{Absyn.Subscript}
    local tup::Tuple{FuncType, FuncType, Argument}
    @match (inCref, enterFunc, exitFunc, inArg) begin
      (Absyn.CREF_FULLYQUALIFIED(componentRef = cr1), _, _, arg) => begin
        @assign (cr2, arg) = traverseExpBidirCref(cr1, enterFunc, exitFunc, arg)
        (if referenceEq(cr1, cr2)
          inCref
        else
          crefMakeFullyQualified(cr2)
        end, arg)
      end

      (Absyn.CREF_QUAL(name = name, subscripts = subs1, componentRef = cr1), _, _, arg) => begin
        @assign (subs2, arg) = ListUtil.map2FoldCheckReferenceEq(
          subs1,
          traverseExpBidirSubs,
          enterFunc,
          exitFunc,
          arg,
        )
        @assign (cr2, arg) = traverseExpBidirCref(cr1, enterFunc, exitFunc, arg)
        (if referenceEq(cr1, cr2) && referenceEq(subs1, subs2)
          inCref
        else
          Absyn.CREF_QUAL(name, subs2, cr2)
        end, arg)
      end

      (Absyn.CREF_IDENT(name = name, subscripts = subs1), _, _, arg) => begin
        @assign (subs2, arg) = ListUtil.map2FoldCheckReferenceEq(
          subs1,
          traverseExpBidirSubs,
          enterFunc,
          exitFunc,
          arg,
        )
        (if referenceEq(subs1, subs2)
          inCref
        else
          Absyn.CREF_IDENT(name, subs2)
        end, arg)
      end

      (Absyn.ALLWILD(__), _, _, _) => begin
        (inCref, inArg)
      end

      (Absyn.WILD(__), _, _, _) => begin
        (inCref, inArg)
      end
    end
  end
  return (outCref, arg)
end

""" #= Helper function to traverseExpBidirCref. Traverses expressions in a
  subscript. =#"""
function traverseExpBidirSubs(
  inSubscript::Absyn.Subscript,
  enterFunc::FuncType,
  exitFunc::FuncType,
  inArg::Argument,
)::Tuple{Absyn.Subscript, Argument}
  local arg::Argument
  local outSubscript::Absyn.Subscript

  @assign (outSubscript, arg) = begin
    local e1::Absyn.Exp
    local e2::Absyn.Exp
    @match (inSubscript, enterFunc, exitFunc, inArg) begin
      (Absyn.SUBSCRIPT(subscript = e1), _, _, arg) => begin
        @assign (e2, arg) = traverseExpBidir(e1, enterFunc, exitFunc, inArg)
        (if referenceEq(e1, e2)
          inSubscript
        else
          Absyn.SUBSCRIPT(e2)
        end, arg)
      end

      (Absyn.NOSUB(__), _, _, _) => begin
        (inSubscript, inArg)
      end
    end
  end
  return (outSubscript, arg)
end

""" #= Helper function to traverseExpBidirSubExps. Traverses the expressions in an
  elseif branch. =#"""
function traverseExpBidirElseIf(
  inElseIf::Tuple{<:Absyn.Exp, Absyn.Exp},
  enterFunc::FuncType,
  exitFunc::FuncType,
  inArg::Argument,
)::Tuple{Tuple{Absyn.Exp, Absyn.Exp}, Argument}
  local arg::Argument
  local outElseIf::Tuple{Absyn.Exp, Absyn.Exp}

  local e1::Absyn.Exp
  local e2::Absyn.Exp
  local tup::Tuple{FuncType, FuncType, Argument}

  @assign (e1, e2) = inElseIf
  @assign (e1, arg) = traverseExpBidir(e1, enterFunc, exitFunc, inArg)
  @assign (e2, arg) = traverseExpBidir(e2, enterFunc, exitFunc, arg)
  @assign outElseIf = (e1, e2)
  return (outElseIf, arg)
end

""" #= Helper function to traverseExpBidirSubExps. Traverses the expressions in a
  list of function argument. =#"""
function traverseExpBidirFunctionArgs(
  inArgs::Absyn.FunctionArgs,
  enterFunc::FuncType,
  exitFunc::FuncType,
  inArg::Argument,
)::Tuple{Absyn.FunctionArgs, Argument}
  local outArg::Argument
  local outArgs::Absyn.FunctionArgs

  @assign (outArgs, outArg) = begin
    local e1::Absyn.Exp
    local e2::Absyn.Exp
    local expl1::List{Absyn.Exp}
    local expl2::List{Absyn.Exp}
    local named_args1::List{Absyn.NamedArg}
    local named_args2::List{Absyn.NamedArg}
    local iters1::Absyn.ForIterators
    local iters2::Absyn.ForIterators
    local arg::Argument
    local iterType::ReductionIterType
    @match (inArgs, enterFunc, exitFunc, inArg) begin
      (Absyn.FUNCTIONARGS(args = expl1, argNames = named_args1), _, _, arg) => begin
        @assign (expl2, arg) = traverseExpListBidir(expl1, enterFunc, exitFunc, arg)
        @assign (named_args2, arg) = ListUtil.map2FoldCheckReferenceEq(
          named_args1,
          traverseExpBidirNamedArg,
          enterFunc,
          exitFunc,
          arg,
        )
        (if referenceEq(expl1, expl2) && referenceEq(named_args1, named_args2)
          inArgs
        else
          Absyn.FUNCTIONARGS(expl2, named_args2)
        end, arg)
      end

      (Absyn.FOR_ITER_FARG(e1, iterType, iters1), _, _, arg) => begin
        @assign (e2, arg) = traverseExpBidir(e1, enterFunc, exitFunc, arg)
        @assign (iters2, arg) = ListUtil.map2FoldCheckReferenceEq(
          iters1,
          traverseExpBidirIterator,
          enterFunc,
          exitFunc,
          arg,
        )
        (if referenceEq(e1, e2) && referenceEq(iters1, iters2)
          inArgs
        else
          Absyn.FOR_ITER_FARG(e2, iterType, iters2)
        end, arg)
      end
    end
  end
  return (outArgs, outArg)
end

""" #= Helper function to traverseExpBidirFunctionArgs. Traverses the expressions in
  a named function argument. =#"""
function traverseExpBidirNamedArg(
  inArg::Absyn.NamedArg,
  enterFunc::FuncType,
  exitFunc::FuncType,
  inExtra::Argument,
)::Tuple{Absyn.NamedArg, Argument}
  local outExtra::Argument
  local outArg::Absyn.NamedArg

  local name::Absyn.Ident
  local value1::Absyn.Exp
  local value2::Absyn.Exp

  @match Absyn.NAMEDARG(name, value1) = inArg
  @assign (value2, outExtra) = traverseExpBidir(value1, enterFunc, exitFunc, inExtra)
  @assign outArg = if referenceEq(value1, value2)
    inArg
  else
    Absyn.NAMEDARG(name, value2)
  end
  return (outArg, outExtra)
end

""" #= Helper function to traverseExpBidirFunctionArgs. Traverses the expressions in
  an iterator. =#"""
function traverseExpBidirIterator(
  inIterator::Absyn.ForIterator,
  enterFunc::FuncType,
  exitFunc::FuncType,
  inArg::Argument,
)::Tuple{Absyn.ForIterator, Argument}
  local outArg::Argument
  local outIterator::Absyn.ForIterator

  local name::Absyn.Ident
  local guardExp1::Option{Absyn.Exp}
  local guardExp2::Option{Absyn.Exp}
  local range1::Option{Absyn.Exp}
  local range2::Option{Absyn.Exp}

  @match Absyn.ITERATOR(name = name, guardExp = guardExp1, range = range1) = inIterator
  @assign (guardExp2, outArg) = traverseExpOptBidir(guardExp1, enterFunc, exitFunc, inArg)
  @assign (range2, outArg) = traverseExpOptBidir(range1, enterFunc, exitFunc, outArg)
  @assign outIterator = if referenceEq(guardExp1, guardExp2) && referenceEq(range1, range2)
    inIterator
  else
    Absyn.ITERATOR(name, guardExp2, range2)
  end
  return (outIterator, outArg)
end

function traverseMatchCase(
  inMatchCase::Absyn.Case,
  enterFunc::FuncType,
  exitFunc::FuncType,
  inArg::Argument,
)::Tuple{Absyn.Case, Argument}
  local outArg::Argument
  local outMatchCase::Absyn.Case

  @assign (outMatchCase, outArg) = begin
    local arg::Argument
    local pattern::Absyn.Exp
    local result::Absyn.Exp
    local info::Info
    local resultInfo::Info
    local pinfo::Info
    local ldecls::List{Absyn.ElementItem}
    local cp::Absyn.ClassPart
    local cmt::Option{String}
    local patternGuard::Option{Absyn.Exp}
    @match (inMatchCase, enterFunc, exitFunc, inArg) begin
      (
        Absyn.CASE(pattern, patternGuard, pinfo, ldecls, cp, result, resultInfo, cmt, info),
        _,
        _,
        arg,
      ) => begin
        @assign (pattern, arg) = traverseExpBidir(pattern, enterFunc, exitFunc, arg)
        @assign (patternGuard, arg) =
          traverseExpOptBidir(patternGuard, enterFunc, exitFunc, arg)
        @assign (cp, arg) = traverseClassPartBidir(cp, enterFunc, exitFunc, arg)
        @assign (result, arg) = traverseExpBidir(result, enterFunc, exitFunc, arg)
        (Absyn.CASE(pattern, patternGuard, pinfo, ldecls, cp, result, resultInfo, cmt, info), arg)
      end

      (
        Absyn.ELSE(
          localDecls = ldecls,
          classPart = cp,
          result = result,
          resultInfo = resultInfo,
          comment = cmt,
          info = info,
        ),
        _,
        _,
        arg,
      ) => begin
        @assign (cp, arg) = traverseClassPartBidir(cp, enterFunc, exitFunc, arg)
        @assign (result, arg) = traverseExpBidir(result, enterFunc, exitFunc, arg)
        (Absyn.ELSE(ldecls, cp, result, resultInfo, cmt, info), arg)
      end
    end
  end
  return (outMatchCase, outArg)
end

function traverseClassPartBidir(
  cp::Absyn.ClassPart,
  enterFunc::FuncType,
  exitFunc::FuncType,
  inArg::Argument,
)::Tuple{Absyn.ClassPart, Argument}
  local outArg::Argument
  local outCp::Absyn.ClassPart

  @assign (outCp, outArg) = begin
    local algs::List{Absyn.AlgorithmItem}
    local eqs::List{Absyn.EquationItem}
    local arg::Argument
    @match (cp, enterFunc, exitFunc, inArg) begin
      (Absyn.ALGORITHMS(algs), _, _, arg) => begin
        @assign (algs, arg) = ListUtil.map2FoldCheckReferenceEq(
          algs,
          traverseAlgorithmItemBidir,
          enterFunc,
          exitFunc,
          arg,
        )
        (Absyn.ALGORITHMS(algs), arg)
      end

      (Absyn.EQUATIONS(eqs), _, _, arg) => begin
        @assign (eqs, arg) = ListUtil.map2FoldCheckReferenceEq(
          eqs,
          traverseEquationItemBidir,
          enterFunc,
          exitFunc,
          arg,
        )
        (Absyn.EQUATIONS(eqs), arg)
      end
    end
  end
  return (outCp, outArg)
end

function traverseEquationItemListBidir(
  inEquationItems::List{<:Absyn.EquationItem},
  enterFunc::FuncType,
  exitFunc::FuncType,
  inArg::Argument,
)::Tuple{List{Absyn.EquationItem}, Argument}
  local outArg::Argument
  local outEquationItems::List{Absyn.EquationItem}

  @assign (outEquationItems, outArg) = ListUtil.map2FoldCheckReferenceEq(
    inEquationItems,
    traverseEquationItemBidir,
    enterFunc,
    exitFunc,
    inArg,
  )
  return (outEquationItems, outArg)
end

function traverseAlgorithmItemListBidir(
  inAlgs::List{<:Absyn.AlgorithmItem},
  enterFunc::FuncType,
  exitFunc::FuncType,
  inArg::Argument,
)::Tuple{List{Absyn.AlgorithmItem}, Argument}
  local outArg::Argument
  local outAlgs::List{Absyn.AlgorithmItem}

  @assign (outAlgs, outArg) = ListUtil.map2FoldCheckReferenceEq(
    inAlgs,
    traverseAlgorithmItemBidir,
    enterFunc,
    exitFunc,
    inArg,
  )
  return (outAlgs, outArg)
end

function traverseAlgorithmItemBidir(
  inAlgorithmItem::Absyn.AlgorithmItem,
  enterFunc::FuncType,
  exitFunc::FuncType,
  inArg::Argument,
)::Tuple{Absyn.AlgorithmItem, Argument}
  local outArg::Argument
  local outAlgorithmItem::Absyn.AlgorithmItem

  @assign (outAlgorithmItem, outArg) = begin
    local arg::Argument
    local alg::Absyn.Algorithm
    local cmt::Option{Absyn.Comment}
    local info::Info
    @match (inAlgorithmItem, enterFunc, exitFunc, inArg) begin
      (Absyn.ALGORITHMITEM(algorithm_ = alg, comment = cmt, info = info), _, _, arg) => begin
        @assign (alg, arg) = traverseAlgorithmBidir(alg, enterFunc, exitFunc, arg)
        (Absyn.ALGORITHMITEM(alg, cmt, info), arg)
      end

      (Absyn.ALGORITHMITEMCOMMENT(__), _, _, _) => begin
        (inAlgorithmItem, inArg)
      end
    end
  end
  return (outAlgorithmItem, outArg)
end

function traverseEquationItemBidir(
  inEquationItem::Absyn.EquationItem,
  enterFunc::FuncType,
  exitFunc::FuncType,
  inArg::Argument,
)::Tuple{Absyn.EquationItem, Argument}
  local outArg::Argument
  local outEquationItem::Absyn.EquationItem

  @assign (outEquationItem, outArg) = begin
    local arg::Argument
    local eq::Absyn.Equation
    local cmt::Option{Absyn.Comment}
    local info::Info
    @match (inEquationItem, enterFunc, exitFunc, inArg) begin
      (Absyn.EQUATIONITEM(equation_ = eq, comment = cmt, info = info), _, _, arg) => begin
        @assign (eq, arg) = traverseEquationBidir(eq, enterFunc, exitFunc, arg)
        (Absyn.EQUATIONITEM(eq, cmt, info), arg)
      end
    end
  end
  return (outEquationItem, outArg)
end

function traverseEquationBidir(
  inEquation::Absyn.Equation,
  enterFunc::FuncType,
  exitFunc::FuncType,
  inArg::Argument,
)::Tuple{Absyn.Equation, Argument}
  local outArg::Argument
  local outEquation::Absyn.Equation

  @assign (outEquation, outArg) = begin
    local arg::Argument
    local e1::Absyn.Exp
    local e2::Absyn.Exp
    local eqil1::List{Absyn.EquationItem}
    local eqil2::List{Absyn.EquationItem}
    local else_branch::List{Tuple{Absyn.Exp, List{Absyn.EquationItem}}}
    local cref1::Absyn.ComponentRef
    local cref2::Absyn.ComponentRef
    local iters::Absyn.ForIterators
    local func_args::Absyn.FunctionArgs
    local eq::Absyn.EquationItem
    @match (inEquation, enterFunc, exitFunc, inArg) begin
      (
        Absyn.EQ_IF(
          ifExp = e1,
          equationTrueItems = eqil1,
          elseIfBranches = else_branch,
          equationElseItems = eqil2,
        ),
        _,
        _,
        arg,
      ) => begin
        @assign (e1, arg) = traverseExpBidir(e1, enterFunc, exitFunc, arg)
        @assign (eqil1, arg) =
          traverseEquationItemListBidir(eqil1, enterFunc, exitFunc, arg)
        @assign (else_branch, arg) = ListUtil.map2FoldCheckReferenceEq(
          else_branch,
          traverseEquationBidirElse,
          enterFunc,
          exitFunc,
          arg,
        )
        @assign (eqil2, arg) =
          traverseEquationItemListBidir(eqil2, enterFunc, exitFunc, arg)
        (Absyn.EQ_IF(e1, eqil1, else_branch, eqil2), arg)
      end

      (Absyn.EQ_EQUALS(leftSide = e1, rightSide = e2), _, _, arg) => begin
        @assign (e1, arg) = traverseExpBidir(e1, enterFunc, exitFunc, arg)
        @assign (e2, arg) = traverseExpBidir(e2, enterFunc, exitFunc, arg)
        (Absyn.EQ_EQUALS(e1, e2), arg)
      end

      (Absyn.EQ_PDE(leftSide = e1, rightSide = e2, domain = cref1), _, _, arg) => begin
        @assign (e1, arg) = traverseExpBidir(e1, enterFunc, exitFunc, arg)
        @assign (e2, arg) = traverseExpBidir(e2, enterFunc, exitFunc, arg)
        @assign cref1 = traverseExpBidirCref(cref1, enterFunc, exitFunc, arg)
        (Absyn.EQ_PDE(e1, e2, cref1), arg)
      end

      (Absyn.EQ_CONNECT(connector1 = cref1, connector2 = cref2), _, _, arg) => begin
        @assign (cref1, arg) = traverseExpBidirCref(cref1, enterFunc, exitFunc, arg)
        @assign (cref2, arg) = traverseExpBidirCref(cref2, enterFunc, exitFunc, arg)
        (Absyn.EQ_CONNECT(cref1, cref2), arg)
      end

      (Absyn.EQ_FOR(iterators = iters, forEquations = eqil1), _, _, arg) => begin
        @assign (iters, arg) = ListUtil.map2FoldCheckReferenceEq(
          iters,
          traverseExpBidirIterator,
          enterFunc,
          exitFunc,
          arg,
        )
        @assign (eqil1, arg) =
          traverseEquationItemListBidir(eqil1, enterFunc, exitFunc, arg)
        (Absyn.EQ_FOR(iters, eqil1), arg)
      end

      (
        Absyn.EQ_WHEN_E(whenExp = e1, whenEquations = eqil1, elseWhenEquations = else_branch),
        _,
        _,
        arg,
      ) => begin
        @assign (e1, arg) = traverseExpBidir(e1, enterFunc, exitFunc, arg)
        @assign (eqil1, arg) =
          traverseEquationItemListBidir(eqil1, enterFunc, exitFunc, arg)
        @assign (else_branch, arg) = ListUtil.map2FoldCheckReferenceEq(
          else_branch,
          traverseEquationBidirElse,
          enterFunc,
          exitFunc,
          arg,
        )
        (Absyn.EQ_WHEN_E(e1, eqil1, else_branch), arg)
      end

      (Absyn.EQ_NORETCALL(functionName = cref1, functionArgs = func_args), _, _, arg) => begin
        @assign (cref1, arg) = traverseExpBidirCref(cref1, enterFunc, exitFunc, arg)
        @assign (func_args, arg) =
          traverseExpBidirFunctionArgs(func_args, enterFunc, exitFunc, arg)
        (Absyn.EQ_NORETCALL(cref1, func_args), arg)
      end

      (Absyn.EQ_FAILURE(equ = eq), _, _, arg) => begin
        @assign (eq, arg) = traverseEquationItemBidir(eq, enterFunc, exitFunc, arg)
        (Absyn.EQ_FAILURE(eq), arg)
      end
    end
  end
  return (outEquation, outArg)
end

function traverseEquationBidirElse(
  inElse::Tuple{<:Absyn.Exp, List{<:Absyn.EquationItem}},
  enterFunc::FuncType,
  exitFunc::FuncType,
  inArg::Argument,
)::Tuple{Tuple{Absyn.Exp, List{Absyn.EquationItem}}, Argument}
  local arg::Argument
  local outElse::Tuple{Absyn.Exp, List{Absyn.EquationItem}}

  local e::Absyn.Exp
  local eqil::List{Absyn.EquationItem}

  @assign (e, eqil) = inElse
  @assign (e, arg) = traverseExpBidir(e, enterFunc, exitFunc, inArg)
  @assign (eqil, arg) = traverseEquationItemListBidir(eqil, enterFunc, exitFunc, arg)
  @assign outElse = (e, eqil)
  return (outElse, arg)
end

function traverseAlgorithmBidirElse(
  inElse::Tuple{<:Absyn.Exp, List{<:Absyn.AlgorithmItem}},
  enterFunc::FuncType,
  exitFunc::FuncType,
  inArg::Argument,
)::Tuple{Tuple{Absyn.Exp, List{Absyn.AlgorithmItem}}, Argument}
  local arg::Argument
  local outElse::Tuple{Absyn.Exp, List{Absyn.AlgorithmItem}}

  local e::Absyn.Exp
  local algs::List{Absyn.AlgorithmItem}

  @assign (e, algs) = inElse
  @assign (e, arg) = traverseExpBidir(e, enterFunc, exitFunc, inArg)
  @assign (algs, arg) = traverseAlgorithmItemListBidir(algs, enterFunc, exitFunc, arg)
  @assign outElse = (e, algs)
  return (outElse, arg)
end

function traverseAlgorithmBidir(
  inAlg::Absyn.Algorithm,
  enterFunc::FuncType,
  exitFunc::FuncType,
  inArg::Argument,
)::Tuple{Absyn.Algorithm, Argument}
  local outArg::Argument
  local outAlg::Absyn.Algorithm

  @assign (outAlg, outArg) = begin
    local arg::Argument
    local e1::Absyn.Exp
    local e2::Absyn.Exp
    local algs1::List{Absyn.AlgorithmItem}
    local algs2::List{Absyn.AlgorithmItem}
    local else_branch::List{Tuple{Absyn.Exp, List{Absyn.AlgorithmItem}}}
    local cref1::Absyn.ComponentRef
    local cref2::Absyn.ComponentRef
    local iters::Absyn.ForIterators
    local func_args::Absyn.FunctionArgs
    local alg::Absyn.AlgorithmItem
    @match (inAlg, enterFunc, exitFunc, inArg) begin
      (Absyn.ALG_ASSIGN(e1, e2), _, _, arg) => begin
        @assign (e1, arg) = traverseExpBidir(e1, enterFunc, exitFunc, arg)
        @assign (e2, arg) = traverseExpBidir(e2, enterFunc, exitFunc, arg)
        (Absyn.ALG_ASSIGN(e1, e2), arg)
      end

      (Absyn.ALG_IF(e1, algs1, else_branch, algs2), _, _, arg) => begin
        @assign (e1, arg) = traverseExpBidir(e1, enterFunc, exitFunc, arg)
        @assign (algs1, arg) =
          traverseAlgorithmItemListBidir(algs1, enterFunc, exitFunc, arg)
        @assign (else_branch, arg) = ListUtil.map2FoldCheckReferenceEq(
          else_branch,
          traverseAlgorithmBidirElse,
          enterFunc,
          exitFunc,
          arg,
        )
        @assign (algs2, arg) =
          traverseAlgorithmItemListBidir(algs2, enterFunc, exitFunc, arg)
        (Absyn.ALG_IF(e1, algs1, else_branch, algs2), arg)
      end

      (Absyn.ALG_FOR(iters, algs1), _, _, arg) => begin
        @assign (iters, arg) = ListUtil.map2FoldCheckReferenceEq(
          iters,
          traverseExpBidirIterator,
          enterFunc,
          exitFunc,
          arg,
        )
        @assign (algs1, arg) =
          traverseAlgorithmItemListBidir(algs1, enterFunc, exitFunc, arg)
        (Absyn.ALG_FOR(iters, algs1), arg)
      end

      (Absyn.ALG_PARFOR(iters, algs1), _, _, arg) => begin
        @assign (iters, arg) = ListUtil.map2FoldCheckReferenceEq(
          iters,
          traverseExpBidirIterator,
          enterFunc,
          exitFunc,
          arg,
        )
        @assign (algs1, arg) =
          traverseAlgorithmItemListBidir(algs1, enterFunc, exitFunc, arg)
        (Absyn.ALG_PARFOR(iters, algs1), arg)
      end

      (Absyn.ALG_WHILE(e1, algs1), _, _, arg) => begin
        @assign (e1, arg) = traverseExpBidir(e1, enterFunc, exitFunc, arg)
        @assign (algs1, arg) =
          traverseAlgorithmItemListBidir(algs1, enterFunc, exitFunc, arg)
        (Absyn.ALG_WHILE(e1, algs1), arg)
      end

      (Absyn.ALG_WHEN_A(e1, algs1, else_branch), _, _, arg) => begin
        @assign (e1, arg) = traverseExpBidir(e1, enterFunc, exitFunc, arg)
        @assign (algs1, arg) =
          traverseAlgorithmItemListBidir(algs1, enterFunc, exitFunc, arg)
        @assign (else_branch, arg) = ListUtil.map2FoldCheckReferenceEq(
          else_branch,
          traverseAlgorithmBidirElse,
          enterFunc,
          exitFunc,
          arg,
        )
        (Absyn.ALG_WHEN_A(e1, algs1, else_branch), arg)
      end

      (Absyn.ALG_NORETCALL(cref1, func_args), _, _, arg) => begin
        @assign (cref1, arg) = traverseExpBidirCref(cref1, enterFunc, exitFunc, arg)
        @assign (func_args, arg) =
          traverseExpBidirFunctionArgs(func_args, enterFunc, exitFunc, arg)
        (Absyn.ALG_NORETCALL(cref1, func_args), arg)
      end

      (Absyn.ALG_RETURN(__), _, _, arg) => begin
        (inAlg, arg)
      end

      (Absyn.ALG_BREAK(__), _, _, arg) => begin
        (inAlg, arg)
      end

      (Absyn.ALG_CONTINUE(__), _, _, arg) => begin
        (inAlg, arg)
      end

      (Absyn.ALG_FAILURE(algs1), _, _, arg) => begin
        @assign (algs1, arg) =
          traverseAlgorithmItemListBidir(algs1, enterFunc, exitFunc, arg)
        (Absyn.ALG_FAILURE(algs1), arg)
      end

      (Absyn.ALG_TRY(algs1, algs2), _, _, arg) => begin
        @assign (algs1, arg) =
          traverseAlgorithmItemListBidir(algs1, enterFunc, exitFunc, arg)
        @assign (algs2, arg) =
          traverseAlgorithmItemListBidir(algs2, enterFunc, exitFunc, arg)
        (Absyn.ALG_TRY(algs1, algs2), arg)
      end
    end
  end
  return (outAlg, outArg)
end

function makeIdentPathFromString(s::String)::Absyn.Path
  local p::Absyn.Path

  @assign p = Absyn.IDENT(s)
  return p
end

function makeQualifiedPathFromStrings(s1::String, s2::String)::Absyn.Path
  local p::Absyn.Path

  @assign p = Absyn.QUALIFIED(s1, Absyn.IDENT(s2))
  return p
end

""" #= returns the class name of a Absyn.Class as a Absyn.Path =#"""
function className(cl::Absyn.Class)::Absyn.Path
  local name::Absyn.Path

  local id::String

  @match Absyn.CLASS(name = id) = cl
  @assign name = Absyn.IDENT(id)
  return name
end

function isClassNamed(inName::String, inClass::Absyn.Class)::Bool
  local outIsNamed::Bool

  @assign outIsNamed = begin
    @match inClass begin
      Absyn.CLASS(__) => begin
        inName == inClass.name
      end

      _ => begin
        false
      end
    end
  end
  return outIsNamed
end

""" #= The Absyn.ElementSpec type contains the name of the element, and this function
   extracts this name. =#"""
function elementSpecName(inElementSpec::Absyn.ElementSpec)::Absyn.Ident
  local outIdent::Absyn.Ident

  @assign outIdent = begin
    local n::Absyn.Ident
    @match inElementSpec begin
      Absyn.CLASSDEF(class_ = Absyn.CLASS(name = n)) => begin
        n
      end

      Absyn.COMPONENTS(components = Absyn.COMPONENTITEM(component = Absyn.COMPONENT(name = n)) <| nil()) =>
        begin
          n
        end
    end
  end
  return outIdent
end

function isClassdef(inElement::Absyn.Element)::Bool
  local b::Bool

  @assign b = begin
    @match inElement begin
      Absyn.ELEMENT(specification = Absyn.CLASSDEF(__)) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return b
end

function elementSpecificationIsClassDef(es::Absyn.ElementSpec)::Bool
  local b::Bool

  @assign b = begin
    @match es begin
      Absyn.CLASSDEF(__) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return b
end

""" #= This function takes a Absyn.Import and prints it as a flat-string. =#"""
function printImportString(imp::Absyn.Import)::String
  local ostring::String
  @assign ostring = begin
    local path::Absyn.Path
    local name::String
    @match imp begin
      Absyn.NAMED_IMPORT(name, _) => begin
        name
      end

      Absyn.QUAL_IMPORT(path) => begin
        @assign name = pathString(path)
        name
      end

      Absyn.UNQUAL_IMPORT(path) => begin
        @assign name = pathString(path)
        name
      end
    end
  end
  return ostring
end

""" #= returns the string of an expression if it is a string constant. =#"""
function expString(exp::Absyn.Exp)::String
  local str::String

  @match Absyn.STRING(str) = exp
  return str
end

""" #= returns the componentRef of an expression if matches. =#"""
function expCref(exp::Absyn.Exp)::Absyn.ComponentRef
  local cr::Absyn.ComponentRef

  @match Absyn.CREF(cr) = exp
  return cr
end

""" #= returns the componentRef of an expression if matches. =#"""
function crefExp(cr::Absyn.ComponentRef)::Absyn.Exp
  local exp::Absyn.Exp

  @assign exp = Absyn.CREF(cr)
  return exp
end

function expComponentRefStr(aexp::Absyn.Exp)::String
  local outString::String

  @assign outString = printComponentRefStr(expCref(aexp))
  return outString
end

function printComponentRefStr(cr::Absyn.ComponentRef)::String
  local ostring::String

  @assign ostring = begin
    local s1::String
    local s2::String
    local child::Absyn.ComponentRef
    @match cr begin
      Absyn.CREF_IDENT(s1, _) => begin
        s1
      end

      Absyn.CREF_QUAL(s1, _, child) => begin
        @assign s2 = printComponentRefStr(child)
        @assign s1 = s1 + "." + s2
        s1
      end

      Absyn.CREF_FULLYQUALIFIED(child) => begin
        @assign s2 = printComponentRefStr(child)
        @assign s1 = "." + s2
        s1
      end

      Absyn.ALLWILD(__) => begin
        "__"
      end

      Absyn.WILD(__) => begin
        "_"
      end
    end
  end
  return ostring
end

""" #= Returns true if two paths are equal. =#"""
function pathEqual(inPath1::Absyn.Path, inPath2::Absyn.Path)::Bool
  local outBoolean::Bool

  @assign outBoolean = begin
    local id1::String
    local id2::String
    local res::Bool
    local path1::Absyn.Path
    local path2::Absyn.Path
    #=  fully qual vs. path
    =#
    @match (inPath1, inPath2) begin
      (Absyn.FULLYQUALIFIED(path1), path2) => begin
        pathEqual(path1, path2)
      end

      (path1, Absyn.FULLYQUALIFIED(path2)) => begin
        pathEqual(path1, path2)
      end

      (Absyn.IDENT(id1), Absyn.IDENT(id2)) => begin
        stringEq(id1, id2)
      end

      (Absyn.QUALIFIED(id1, path1), Absyn.QUALIFIED(id2, path2)) => begin
        @assign res = if stringEq(id1, id2)
          pathEqual(path1, path2)
        else
          false
        end
        res
      end

      _ => begin
        false
      end
    end
  end
  #=  path vs. fully qual
  =#
  #=  ident vs. ident
  =#
  #=  qual ident vs. qual ident
  =#
  #=  other return false
  =#
  return outBoolean
end

""" #= Author BZ 2009-01
   Check whether two type specs are equal or not. =#"""
function typeSpecEqual(a::Absyn.TypeSpec, b::Absyn.TypeSpec)::Bool
  local ob::Bool

  @assign ob = begin
    local p1::Absyn.Path
    local p2::Absyn.Path
    local oad1::Option{Absyn.ArrayDim}
    local oad2::Option{Absyn.ArrayDim}
    local lst1::List{Absyn.TypeSpec}
    local lst2::List{Absyn.TypeSpec}
    local i1::Absyn.Ident
    local i2::Absyn.Ident
    local pos1::Integer
    local pos2::Integer
    #=  first try full equality
    =#
    @matchcontinue (a, b) begin
      (Absyn.TPATH(p1, oad1), Absyn.TPATH(p2, oad2)) => begin
        @match true = pathEqual(p1, p2)
        @match true = optArrayDimEqual(oad1, oad2)
        true
      end

      (Absyn.TCOMPLEX(p1, lst1, oad1), Absyn.TCOMPLEX(p2, lst2, oad2)) => begin
        @match true = pathEqual(p1, p2)
        @match true = ListUtil.isEqualOnTrue(lst1, lst2, typeSpecEqual)
        @match true = optArrayDimEqual(oad1, oad2)
        true
      end

      _ => begin
        false
      end
    end
  end
  return ob
end

""" #= Author BZ
   helper function for typeSpecEqual =#"""
function optArrayDimEqual(oad1::Option{<:Absyn.ArrayDim}, oad2::Option{<:Absyn.ArrayDim})::Bool
  local b::Bool

  @assign b = begin
    local ad1::List{Absyn.Subscript}
    local ad2::List{Absyn.Subscript}
    @matchcontinue (oad1, oad2) begin
      (SOME(ad1), SOME(ad2)) => begin
        @match true = ListUtil.isEqualOnTrue(ad1, ad2, subscriptEqual)
        true
      end

      (NONE(), NONE()) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return b
end

""" #= This function simply converts a Absyn.Path to a string. =#"""
function typeSpecPathString(tp::Absyn.TypeSpec)::String
  local s::String

  @assign s = begin
    local p::Absyn.Path
    @match tp begin
      Absyn.TCOMPLEX(path = p) => begin
        pathString(p)
      end

      Absyn.TPATH(path = p) => begin
        pathString(p)
      end
    end
  end
  return s
end

""" #= Converts a Absyn.TypeSpec to Absyn.Path =#"""
function typeSpecPath(tp::Absyn.TypeSpec)::Absyn.Path
  local op::Absyn.Path

  @assign op = begin
    local p::Absyn.Path
    @match tp begin
      Absyn.TCOMPLEX(path = p) => begin
        p
      end

      Absyn.TPATH(path = p) => begin
        p
      end
    end
  end
  return op
end

""" #= Returns the dimensions of a Absyn.TypeSpec. =#"""
function typeSpecDimensions(inTypeSpec::Absyn.TypeSpec)::Absyn.ArrayDim
  local outDimensions::Absyn.ArrayDim

  @assign outDimensions = begin
    local dim::Absyn.ArrayDim
    @match inTypeSpec begin
      Absyn.TPATH(arrayDim = SOME(dim)) => begin
        dim
      end

      Absyn.TCOMPLEX(arrayDim = SOME(dim)) => begin
        dim
      end

      _ => begin
        nil
      end
    end
  end
  return outDimensions
end

""" #= This function simply converts a Absyn.Path to a string. =#"""
function pathString(
  path::Absyn.Path,
  delimiter::String = ".",
  usefq::Bool = true,
  reverse::Bool = false,
)::String
  local s::String
  local p1::Absyn.Path
  local p2::Absyn.Path
  local count::Integer = 0
  local len::Integer = 0
  local dlen::Integer = stringLength(delimiter)
  local b::Bool

  #=  First, calculate the length of the string to be generated
  =#
  @assign p1 = if usefq
    path
  else
    makeNotFullyQualified(path)
  end
  _ = begin
    @match p1 begin
      Absyn.IDENT(__) => begin
        #=  Do not allocate memory if we're just going to copy the only identifier
        =#
        @assign s = p1.name
        return s
        ()
      end
      _ => begin
        ()
      end
    end
  end
  @assign p2 = p1
  @assign b = true
  while b
    @assign (p2, len, count, b) = begin
      @match p2 begin
        Absyn.IDENT(__) => begin
          (p2, len + 1, count + stringLength(p2.name), false)
        end

        Absyn.QUALIFIED(__) => begin
          (p2.path, len + 1, count + stringLength(p2.name), true)
        end

        Absyn.FULLYQUALIFIED(__) => begin
          (p2.path, len + 1, count, true)
        end
      end
    end
  end
  @assign s = pathStringWork(p1, (len - 1) * dlen + count, delimiter, dlen, reverse)
  return s
end

function pathStringWork(
  inPath::Absyn.Path,
  len::Integer,
  delimiter::String,
  dlen::Integer,
  reverse::Bool,
)::String
  local s::String = ""

  local p::Absyn.Path = inPath
  local b::Bool = true
  local count::Integer = 0
  #=  Allocate a string of the exact required length
  =#
  local sb::System.StringAllocator = System.StringAllocator(len)

  #=  Fill the string
  =#
  while b
    @assign (p, count, b) = begin
      @match p begin
        Absyn.IDENT(__) => begin
          System.stringAllocatorStringCopy(sb, p.name, if reverse
              len - count - stringLength(p.name)
            else
              count
            end)
          (p, count + stringLength(p.name), false)
        end

        Absyn.QUALIFIED(__) => begin
          System.stringAllocatorStringCopy(
            sb,
            p.name,
            if reverse
              len - count - dlen - stringLength(p.name)
            else
              count
            end,
          )
          System.stringAllocatorStringCopy(sb, delimiter, if reverse
              len - count - dlen
            else
              count + stringLength(p.name)
            end)
          (p.path, count + stringLength(p.name) + dlen, true)
        end

        Absyn.FULLYQUALIFIED(__) => begin
          System.stringAllocatorStringCopy(sb, delimiter, if reverse
            len - count - dlen
          else
            count
          end)
          (p.path, count + dlen, true)
        end
      end
    end
  end
  #=  Return the string
  =#
  @assign s = System.stringAllocatorResult(sb, s)
  return s
end

@ExtendedFunction pathStringNoQual pathString(usefq = false)

function pathStringDefault(path::Absyn.Path)::String
  local s::String = pathString(path)
  return s
end

function classNameCompare(c1::Absyn.Class, c2::Absyn.Class)::Integer
  local o::Integer

  @assign o = stringCompare(c1.name, c2.name)
  return o
end

function classNameGreater(c1::Absyn.Class, c2::Absyn.Class)::Bool
  local b::Bool

  @assign b = stringCompare(c1.name, c2.name) > 0
  return b
end

function pathCompare(ip1::Absyn.Path, ip2::Absyn.Path)::Integer
  local o::Integer

  @assign o = begin
    local p1::Absyn.Path
    local p2::Absyn.Path
    local i1::String
    local i2::String
    @match (ip1, ip2) begin
      (Absyn.FULLYQUALIFIED(p1), Absyn.FULLYQUALIFIED(p2)) => begin
        pathCompare(p1, p2)
      end

      (Absyn.FULLYQUALIFIED(__), _) => begin
        1
      end

      (_, Absyn.FULLYQUALIFIED(__)) => begin
        -1
      end

      (Absyn.QUALIFIED(i1, p1), Absyn.QUALIFIED(i2, p2)) => begin
        @assign o = stringCompare(i1, i2)
        @assign o = if o == 0
          pathCompare(p1, p2)
        else
          o
        end
        o
      end

      (Absyn.QUALIFIED(__), _) => begin
        1
      end

      (_, Absyn.QUALIFIED(__)) => begin
        -1
      end

      (Absyn.IDENT(i1), Absyn.IDENT(i2)) => begin
        stringCompare(i1, i2)
      end
    end
  end
  return o
end

function pathCompareNoQual(ip1::Absyn.Path, ip2::Absyn.Path)::Integer
  local o::Integer

  @assign o = begin
    local p1::Absyn.Path
    local p2::Absyn.Path
    local i1::String
    local i2::String
    @match (ip1, ip2) begin
      (Absyn.FULLYQUALIFIED(p1), p2) => begin
        pathCompareNoQual(p1, p2)
      end

      (p1, Absyn.FULLYQUALIFIED(p2)) => begin
        pathCompareNoQual(p1, p2)
      end

      (Absyn.QUALIFIED(i1, p1), Absyn.QUALIFIED(i2, p2)) => begin
        @assign o = stringCompare(i1, i2)
        @assign o = if o == 0
          pathCompare(p1, p2)
        else
          o
        end
        o
      end

      (Absyn.QUALIFIED(__), _) => begin
        1
      end

      (_, Absyn.QUALIFIED(__)) => begin
        -1
      end

      (Absyn.IDENT(i1), Absyn.IDENT(i2)) => begin
        stringCompare(i1, i2)
      end
    end
  end
  return o
end

""" #= Hashes a path. =#"""
function pathHashMod(path::Absyn.Path, mod::Integer)::Integer
  local hash::Integer

  #=  hash := valueHashMod(path,mod);
  =#
  #=  print(pathString(path) + \" => \" + intString(hash) + \"\\n\");
  =#
  #=  hash := stringHashDjb2Mod(pathString(path),mod);
  =#
  #=  TODO: stringHashDjb2 is missing a default value for the seed; add this once we bootstrapped omc so we can use that function instead of our own hack
  =#
  @assign hash = intAbs(intMod(pathHashModWork(path, 5381), mod))
  return hash
end

""" #= Hashes a path. =#"""
function pathHashModWork(path::Absyn.Path, acc::Integer)::Integer
  local hash::Integer

  @assign hash = begin
    local p::Absyn.Path
    local s::String
    local i::Integer
    local i2::Integer
    @match (path, acc) begin
      (Absyn.FULLYQUALIFIED(p), _) => begin
        pathHashModWork(p, acc * 31 + 46)
      end

      (Absyn.QUALIFIED(s, p), _) => begin
        @assign i = stringHashDjb2(s)
        @assign i2 = acc * 31 + 46
        pathHashModWork(p, i2 * 31 + i)
      end

      (Absyn.IDENT(s), _) => begin
        @assign i = stringHashDjb2(s)
        @assign i2 = acc * 31 + 46
        i2 * 31 + i
      end
    end
  end
  #= /* '.' */ =#
  return hash
end

""" #= Returns a path converted to string or an empty string if nothing exist =#"""
function optPathString(inPathOption::Option{<:Absyn.Path})::String
  local outString::String

  @assign outString = begin
    local str::Absyn.Ident
    local p::Absyn.Path
    @match inPathOption begin
      NONE() => begin
        ""
      end

      SOME(p) => begin
        @assign str = pathString(p)
        str
      end
    end
  end
  return outString
end

""" #=  Changes a path to string. Uses the input string as separator.
  If the separtor exists in the string then it is doubled (sep _ then
  a_b changes to a__b) before delimiting
  (Replaces dots with that separator). And also unquotes each ident.
 =#"""
function pathStringUnquoteReplaceDot(inPath::Absyn.Path, repStr::String)::String
  local outString::String

  local strlst::List{String}
  local rep_rep::String

  @assign rep_rep = repStr + repStr
  @assign strlst = pathToStringList(inPath)
  @assign strlst = ListUtil.map2(strlst, System.stringReplace, repStr, rep_rep)
  @assign strlst = ListUtil.map(strlst, System.unquoteIdentifier)
  @assign outString = stringDelimitList(strlst, repStr)
  return outString
end

""" #= Converts a string into a qualified path. =#"""
function stringPath(str::String)::Absyn.Path
  local qualifiedPath::Absyn.Path

  local paths::List{String}

  @assign paths = Util.stringSplitAtChar(str, ".")
  @assign qualifiedPath = stringListPath(paths)
  return qualifiedPath
end

""" #= Converts a list of strings into a qualified path. =#"""
function stringListPath(paths::List{<:String})::Absyn.Path
  local qualifiedPath::Absyn.Path

  @assign qualifiedPath = begin
    local str::String
    local rest_str::List{String}
    local p::Absyn.Path
    @matchcontinue paths begin
      nil() => begin
        fail()
      end

      str <| nil() => begin
        Absyn.IDENT(str)
      end

      str <| rest_str => begin
        @assign p = stringListPath(rest_str)
        Absyn.QUALIFIED(str, p)
      end
    end
  end
  return qualifiedPath
end

""" #= Converts a list of strings into a qualified path, in reverse order.
   Ex: {'a', 'b', 'c'} => c.b.a =#"""
function stringListPathReversed(inStrings::List{<:String})::Absyn.Path
  local outPath::Absyn.Path

  local id::String
  local rest_str::List{String}
  local path::Absyn.Path

  @match _cons(id, rest_str) = inStrings
  @assign path = Absyn.IDENT(id)
  @assign outPath = stringListPathReversed2(rest_str, path)
  return outPath
end

function stringListPathReversed2(inStrings::List{<:String}, inAccumPath::Absyn.Path)::Absyn.Path
  local outPath::Absyn.Path

  @assign outPath = begin
    local id::String
    local rest_str::List{String}
    local path::Absyn.Path
    @match (inStrings, inAccumPath) begin
      (nil(), _) => begin
        inAccumPath
      end

      (id <| rest_str, _) => begin
        @assign path = Absyn.QUALIFIED(id, inAccumPath)
        stringListPathReversed2(rest_str, path)
      end
    end
  end
  return outPath
end

""" #= Returns the two last idents of a path =#"""
function pathTwoLastIdents(inPath::Absyn.Path)::Absyn.Path
  local outTwoLast::Absyn.Path

  @assign outTwoLast = begin
    local p::Absyn.Path
    @match inPath begin
      Absyn.QUALIFIED(path = Absyn.IDENT(__)) => begin
        inPath
      end

      Absyn.QUALIFIED(path = p) => begin
        pathTwoLastIdents(p)
      end

      Absyn.FULLYQUALIFIED(path = p) => begin
        pathTwoLastIdents(p)
      end
    end
  end
  return outTwoLast
end

""" #= Returns the last ident (after last dot) in a path =#"""
function pathLastIdent(inPath::Absyn.Path)::String
  local outIdent::String

  @assign outIdent = begin
    local id::Absyn.Ident
    local p::Absyn.Path
    @match inPath begin
      Absyn.QUALIFIED(path = p) => begin
        pathLastIdent(p)
      end

      Absyn.IDENT(name = id) => begin
        id
      end

      Absyn.FULLYQUALIFIED(path = p) => begin
        pathLastIdent(p)
      end
    end
  end
  return outIdent
end

""" #= Returns the last ident (after last dot) in a path =#"""
function pathLast(path::Absyn.Path)::Absyn.Path

  @assign path = begin
    local p::Absyn.Path
    @match path begin
      Absyn.QUALIFIED(path = p) => begin
        pathLast(p)
      end

      Absyn.IDENT(__) => begin
        path
      end

      Absyn.FULLYQUALIFIED(path = p) => begin
        pathLast(p)
      end
    end
  end
  return path
end

""" #= Returns the first ident (before first dot) in a path =#"""
function pathFirstIdent(inPath::Absyn.Path)::Absyn.Ident
  local outIdent::Absyn.Ident

  @assign outIdent = begin
    local n::Absyn.Ident
    local p::Absyn.Path
    @match inPath begin
      Absyn.FULLYQUALIFIED(path = p) => begin
        pathFirstIdent(p)
      end

      Absyn.QUALIFIED(name = n) => begin
        n
      end

      Absyn.IDENT(name = n) => begin
        n
      end
    end
  end
  return outIdent
end

function pathFirstPath(inPath::Absyn.Path)::Absyn.Path
  local outPath::Absyn.Path

  @assign outPath = begin
    local n::Absyn.Ident
    @match inPath begin
      Absyn.IDENT(__) => begin
        inPath
      end

      Absyn.QUALIFIED(name = n) => begin
        Absyn.IDENT(n)
      end

      Absyn.FULLYQUALIFIED(path = outPath) => begin
        pathFirstPath(outPath)
      end
    end
  end
  return outPath
end

function pathSecondIdent(inPath::Absyn.Path)::Absyn.Ident
  local outIdent::Absyn.Ident

  @assign outIdent = begin
    local n::Absyn.Ident
    local p::Absyn.Path
    @match inPath begin
      Absyn.QUALIFIED(path = Absyn.QUALIFIED(name = n)) => begin
        n
      end

      Absyn.QUALIFIED(path = Absyn.IDENT(name = n)) => begin
        n
      end

      Absyn.FULLYQUALIFIED(path = p) => begin
        pathSecondIdent(p)
      end
    end
  end
  return outIdent
end

function pathRest(inPath::Absyn.Path)::Absyn.Path
  local outPath::Absyn.Path

  @assign outPath = begin
    @match inPath begin
      Absyn.QUALIFIED(path = outPath) => begin
        outPath
      end

      Absyn.FULLYQUALIFIED(path = outPath) => begin
        pathRest(outPath)
      end
    end
  end
  return outPath
end

""" #= strips the same prefix paths and returns the stripped path. e.g pathStripSamePrefix(P.M.A, P.M.B) => A =#"""
function pathStripSamePrefix(inPath1::Absyn.Path, inPath2::Absyn.Path)::Absyn.Path
  local outPath::Absyn.Path

  @assign outPath = begin
    local ident1::Absyn.Ident
    local ident2::Absyn.Ident
    local path1::Absyn.Path
    local path2::Absyn.Path
    @matchcontinue (inPath1, inPath2) begin
      (_, _) => begin
        @assign ident1 = pathFirstIdent(inPath1)
        @assign ident2 = pathFirstIdent(inPath2)
        @match true = stringEq(ident1, ident2)
        @assign path1 = stripFirst(inPath1)
        @assign path2 = stripFirst(inPath2)
        pathStripSamePrefix(path1, path2)
      end

      _ => begin
        inPath1
      end
    end
  end
  return outPath
end

""" #= Returns the prefix of a path, i.e. this.is.a.path => this.is.a =#"""
function pathPrefix(path::Absyn.Path)::Absyn.Path
  local prefix::Absyn.Path

  @assign prefix = begin
    local p::Absyn.Path
    local n::Absyn.Ident
    @matchcontinue path begin
      Absyn.FULLYQUALIFIED(path = p) => begin
        pathPrefix(p)
      end

      Absyn.QUALIFIED(name = n, path = Absyn.IDENT(__)) => begin
        Absyn.IDENT(n)
      end

      Absyn.QUALIFIED(name = n, path = p) => begin
        @assign p = pathPrefix(p)
        Absyn.QUALIFIED(n, p)
      end
    end
  end
  return prefix
end

""" #= Prefixes a path with an identifier. =#"""
function prefixPath(prefix::Absyn.Ident, path::Absyn.Path)::Absyn.Path
  local outPath::Absyn.Path

  @assign outPath = Absyn.QUALIFIED(prefix, path)
  return outPath
end

""" #= Prefixes an optional path with an identifier. =#"""
function prefixOptPath(prefix::Absyn.Ident, optPath::Option{<:Absyn.Path})::Option{Absyn.Path}
  local outPath::Option{Absyn.Path}

  @assign outPath = begin
    local path::Absyn.Path
    @match (prefix, optPath) begin
      (_, NONE()) => begin
        SOME(Absyn.IDENT(prefix))
      end

      (_, SOME(path)) => begin
        SOME(Absyn.QUALIFIED(prefix, path))
      end
    end
  end
  return outPath
end

""" #= Adds a suffix to a path. Ex:
     suffixPath(a.b.c, 'd') => a.b.c.d =#"""
function suffixPath(inPath::Absyn.Path, inSuffix::Absyn.Ident)::Absyn.Path
  local outPath::Absyn.Path

  @assign outPath = begin
    local name::Absyn.Ident
    local path::Absyn.Path
    @match (inPath, inSuffix) begin
      (Absyn.IDENT(name), _) => begin
        Absyn.QUALIFIED(name, Absyn.IDENT(inSuffix))
      end

      (Absyn.QUALIFIED(name, path), _) => begin
        @assign path = suffixPath(path, inSuffix)
        Absyn.QUALIFIED(name, path)
      end

      (Absyn.FULLYQUALIFIED(path), _) => begin
        @assign path = suffixPath(path, inSuffix)
        Absyn.FULLYQUALIFIED(path)
      end
    end
  end
  return outPath
end

""" #= returns true if suffix_path is a suffix of path =#"""
function pathSuffixOf(suffix_path::Absyn.Path, path::Absyn.Path)::Bool
  local res::Bool

  @assign res = begin
    local p::Absyn.Path
    @matchcontinue (suffix_path, path) begin
      (_, _) => begin
        @match true = pathEqual(suffix_path, path)
        true
      end

      (_, Absyn.FULLYQUALIFIED(path = p)) => begin
        pathSuffixOf(suffix_path, p)
      end

      (_, Absyn.QUALIFIED(path = p)) => begin
        pathSuffixOf(suffix_path, p)
      end

      _ => begin
        false
      end
    end
  end
  return res
end

""" #= returns true if suffix_path is a suffix of path =#"""
function pathSuffixOfr(path::Absyn.Path, suffix_path::Absyn.Path)::Bool
  local res::Bool

  @assign res = pathSuffixOf(suffix_path, path)
  return res
end

function pathToStringList(path::Absyn.Path)::List{String}
  local outPaths::List{String}

  @assign outPaths = listReverse(pathToStringListWork(path, nil))
  return outPaths
end

function pathToStringListWork(path::Absyn.Path, acc::List{<:String})::List{String}
  local outPaths::List{String}

  @assign outPaths = begin
    local n::String
    local p::Absyn.Path
    local strings::List{String}
    @match (path, acc) begin
      (Absyn.IDENT(name = n), _) => begin
        _cons(n, acc)
      end

      (Absyn.FULLYQUALIFIED(path = p), _) => begin
        pathToStringListWork(p, acc)
      end

      (Absyn.QUALIFIED(name = n, path = p), _) => begin
        pathToStringListWork(p, _cons(n, acc))
      end
    end
  end
  return outPaths
end

""" #=
  Replaces the first part of a path with a replacement path:
  (a.b.c, d.e) => d.e.b.c
  (a, b.c.d) => b.c.d
 =#"""
function pathReplaceFirstIdent(path::Absyn.Path, replPath::Absyn.Path)::Absyn.Path
  local outPath::Absyn.Path

  @assign outPath = begin
    local p::Absyn.Path
    #=  Should not be possible to replace FQ paths
    =#
    @match (path, replPath) begin
      (Absyn.QUALIFIED(path = p), _) => begin
        joinPaths(replPath, p)
      end

      (Absyn.IDENT(__), _) => begin
        replPath
      end
    end
  end
  return outPath
end

""" #= Function for appending subscripts at end of last ident =#"""
function addSubscriptsLast(icr::Absyn.ComponentRef, i::List{<:Absyn.Subscript})::Absyn.ComponentRef
  local ocr::Absyn.ComponentRef

  @assign ocr = begin
    local subs::List{Absyn.Subscript}
    local id::String
    local cr::Absyn.ComponentRef
    @match (icr, i) begin
      (Absyn.CREF_IDENT(id, subs), _) => begin
        Absyn.CREF_IDENT(id, listAppend(subs, i))
      end

      (Absyn.CREF_QUAL(id, subs, cr), _) => begin
        @assign cr = addSubscriptsLast(cr, i)
        Absyn.CREF_QUAL(id, subs, cr)
      end

      (Absyn.CREF_FULLYQUALIFIED(cr), _) => begin
        @assign cr = addSubscriptsLast(cr, i)
        crefMakeFullyQualified(cr)
      end
    end
  end
  return ocr
end

""" #=
  Replaces the first part of a cref with a replacement path:
  (a[4].b.c[3], d.e) => d.e[4].b.c[3]
  (a[3], b.c.d) => b.c.d[3]
 =#"""
function crefReplaceFirstIdent(icref::Absyn.ComponentRef, replPath::Absyn.Path)::Absyn.ComponentRef
  local outCref::Absyn.ComponentRef

  @assign outCref = begin
    local subs::List{Absyn.Subscript}
    local cr::Absyn.ComponentRef
    local cref::Absyn.ComponentRef
    @match (icref, replPath) begin
      (Absyn.CREF_FULLYQUALIFIED(componentRef = cr), _) => begin
        @assign cr = crefReplaceFirstIdent(cr, replPath)
        crefMakeFullyQualified(cr)
      end

      (Absyn.CREF_QUAL(componentRef = cr, subscripts = subs), _) => begin
        @assign cref = pathToCref(replPath)
        @assign cref = addSubscriptsLast(cref, subs)
        joinCrefs(cref, cr)
      end

      (Absyn.CREF_IDENT(subscripts = subs), _) => begin
        @assign cref = pathToCref(replPath)
        @assign cref = addSubscriptsLast(cref, subs)
        cref
      end
    end
  end
  return outCref
end

""" #= Returns true if prefixPath is a prefix of path, false otherwise. =#"""
function pathPrefixOf(prefixPath::Absyn.Path, path::Absyn.Path)::Bool
  local isPrefix::Bool

  @assign isPrefix = begin
    local p::Absyn.Path
    local p2::Absyn.Path
    local id::String
    local id2::String
    @matchcontinue (prefixPath, path) begin
      (Absyn.FULLYQUALIFIED(p), p2) => begin
        pathPrefixOf(p, p2)
      end

      (p, Absyn.FULLYQUALIFIED(p2)) => begin
        pathPrefixOf(p, p2)
      end

      (Absyn.IDENT(id), Absyn.IDENT(id2)) => begin
        stringEq(id, id2)
      end

      (Absyn.IDENT(id), Absyn.QUALIFIED(name = id2)) => begin
        stringEq(id, id2)
      end

      (Absyn.QUALIFIED(id, p), Absyn.QUALIFIED(id2, p2)) => begin
        @match true = stringEq(id, id2)
        @match true = pathPrefixOf(p, p2)
        true
      end

      _ => begin
        false
      end
    end
  end
  return isPrefix
end

""" #= Alternative names: crefIsPrefixOf, isPrefixOf, prefixOf
  Author: DH 2010-03

  Returns true if prefixCr is a prefix of cr, i.e., false otherwise.
  Subscripts are Absyn.NOT checked. =#"""
function crefPrefixOf(prefixCr::Absyn.ComponentRef, cr::Absyn.ComponentRef)::Bool
  local out::Bool

  @assign out = begin
    @matchcontinue (prefixCr, cr) begin
      (_, _) => begin
        @match true = crefEqualNoSubs(prefixCr, cr)
        true
      end

      (_, _) => begin
        crefPrefixOf(prefixCr, crefStripLast(cr))
      end

      _ => begin
        false
      end
    end
  end
  return out
end

""" #= removes the prefix_path from path, and returns the rest of path =#"""
function removePrefix(prefix_path::Absyn.Path, path::Absyn.Path)::Absyn.Path
  local newPath::Absyn.Path

  @assign newPath = begin
    local p::Absyn.Path
    local p2::Absyn.Path
    local id1::Absyn.Ident
    local id2::Absyn.Ident
    #=  fullyqual path
    =#
    @match (prefix_path, path) begin
      (p, Absyn.FULLYQUALIFIED(p2)) => begin
        removePrefix(p, p2)
      end

      (Absyn.QUALIFIED(name = id1, path = p), Absyn.QUALIFIED(name = id2, path = p2)) => begin
        @match true = stringEq(id1, id2)
        removePrefix(p, p2)
      end

      (Absyn.IDENT(id1), Absyn.QUALIFIED(name = id2, path = p2)) => begin
        @match true = stringEq(id1, id2)
        p2
      end
    end
  end
  #=  qual
  =#
  #=  ids
  =#
  return newPath
end

""" #= Tries to remove a given prefix from a path with removePrefix. If it fails it
  removes the first identifier in the prefix and tries again, until it either
  succeeds or reaches the end of the prefix. Ex:
    removePartialPrefix(A.B.C, B.C.D.E) => D.E
   =#"""
function removePartialPrefix(inPrefix::Absyn.Path, inPath::Absyn.Path)::Absyn.Path
  local outPath::Absyn.Path

  @assign outPath = begin
    local p::Absyn.Path
    @matchcontinue (inPrefix, inPath) begin
      (_, _) => begin
        @assign p = removePrefix(inPrefix, inPath)
        p
      end

      (Absyn.QUALIFIED(path = p), _) => begin
        @assign p = removePrefix(p, inPath)
        p
      end

      (Absyn.FULLYQUALIFIED(path = p), _) => begin
        @assign p = removePartialPrefix(p, inPath)
        p
      end

      _ => begin
        inPath
      end
    end
  end
  return outPath
end

""" #=
  function: crefRemovePrefix
  Alternative names: removePrefix
  Author: DH 2010-03

  If prefixCr is a prefix of cr, removes prefixCr from cr and returns the remaining reference,
  otherwise fails. Subscripts are Absyn.NOT checked.
 =#"""
function crefRemovePrefix(prefixCr::Absyn.ComponentRef, cr::Absyn.ComponentRef)::Absyn.ComponentRef
  local out::Absyn.ComponentRef

  @assign out = begin
    local prefixIdent::Absyn.Ident
    local ident::Absyn.Ident
    local prefixRestCr::Absyn.ComponentRef
    local restCr::Absyn.ComponentRef
    #=  fqual
    =#
    @match (prefixCr, cr) begin
      (
        Absyn.CREF_FULLYQUALIFIED(componentRef = prefixRestCr),
        Absyn.CREF_FULLYQUALIFIED(componentRef = restCr),
      ) => begin
        crefRemovePrefix(prefixRestCr, restCr)
      end

      (
        Absyn.CREF_QUAL(name = prefixIdent, componentRef = prefixRestCr),
        Absyn.CREF_QUAL(name = ident, componentRef = restCr),
      ) => begin
        @match true = stringEq(prefixIdent, ident)
        crefRemovePrefix(prefixRestCr, restCr)
      end

      (Absyn.CREF_IDENT(name = prefixIdent), Absyn.CREF_QUAL(name = ident, componentRef = restCr)) =>
        begin
          @match true = stringEq(prefixIdent, ident)
          restCr
        end

      (Absyn.CREF_IDENT(name = prefixIdent), Absyn.CREF_IDENT(name = ident)) => begin
        @match true = stringEq(prefixIdent, ident)
        Absyn.CREF_IDENT("", nil)
      end
    end
  end
  #=  qual
  =#
  #=  id vs. qual
  =#
  #=  id vs. id
  =#
  return out
end

""" #= Author BZ,
   checks if one Absyn.IDENT(..) is contained in path. =#"""
function pathContains(fullPath::Absyn.Path, pathId::Absyn.Path)::Bool
  local b::Bool

  @assign b = begin
    local str1::String
    local str2::String
    local qp::Absyn.Path
    local b1::Bool
    local b2::Bool
    @match (fullPath, pathId) begin
      (Absyn.IDENT(str1), Absyn.IDENT(str2)) => begin
        stringEq(str1, str2)
      end

      (Absyn.QUALIFIED(str1, qp), Absyn.IDENT(str2)) => begin
        @assign b1 = stringEq(str1, str2)
        @assign b2 = pathContains(qp, pathId)
        @assign b1 = boolOr(b1, b2)
        b1
      end

      (Absyn.FULLYQUALIFIED(qp), _) => begin
        pathContains(qp, pathId)
      end
    end
  end
  return b
end

""" #= Author OT,
   checks if Absyn.Path contains the given string. =#"""
function pathContainsString(p1::Absyn.Path, str::String)::Bool
  local b::Bool

  @assign b = begin
    local str1::String
    local searchStr::String
    local qp::Absyn.Path
    local b1::Bool
    local b2::Bool
    local b3::Bool
    @match (p1, str) begin
      (Absyn.IDENT(str1), searchStr) => begin
        @assign b1 = System.stringFind(str1, searchStr) != (-1)
        b1
      end

      (Absyn.QUALIFIED(str1, qp), searchStr) => begin
        @assign b1 = System.stringFind(str1, searchStr) != (-1)
        @assign b2 = pathContainsString(qp, searchStr)
        @assign b3 = boolOr(b1, b2)
        b3
      end

      (Absyn.FULLYQUALIFIED(qp), searchStr) => begin
        pathContainsString(qp, searchStr)
      end
    end
  end
  return b
end

""" #= This function checks if subPath is contained in path.
   If it is the complete path is returned. Otherwise the function fails.
   For example,
     pathContainedIn( C.D, A.B.C) => A.B.C.D
     pathContainedIn(C.D, A.B.C.D) => A.B.C.D
     pathContainedIn(A.B.C.D, A.B.C.D) => A.B.C.D
     pathContainedIn(B.C,A.B) => A.B.C =#"""
function pathContainedIn(subPath::Absyn.Path, path::Absyn.Path)::Absyn.Path
  local completePath::Absyn.Path

  @assign completePath = begin
    local ident::Absyn.Ident
    local newPath::Absyn.Path
    local newSubPath::Absyn.Path
    #=  A suffix, e.g. C.D in A.B.C.D
    =#
    @matchcontinue (subPath, path) begin
      (_, _) => begin
        @match true = pathSuffixOf(subPath, path)
        path
      end

      (_, _) => begin
        @assign ident = pathLastIdent(path)
        @assign newPath = stripLast(path)
        @assign newPath = pathContainedIn(subPath, newPath)
        joinPaths(newPath, Absyn.IDENT(ident))
      end

      _ => begin
        @assign ident = pathLastIdent(subPath)
        @assign newSubPath = stripLast(subPath)
        @assign newSubPath = pathContainedIn(newSubPath, path)
        joinPaths(newSubPath, Absyn.IDENT(ident))
      end
    end
  end
  #=  strip last ident of path and recursively check if suffix.
  =#
  #=  strip last ident of subpath and recursively check if suffix.
  =#
  return completePath
end

""" #= Author BZ 2009-08
   Function for getting ComponentRefs out from Subscripts =#"""
function getCrefsFromSubs(
  isubs::List{<:Absyn.Subscript},
  includeSubs::Bool,
  includeFunctions::Bool,
)::List{Absyn.ComponentRef} #= note that if you say includeSubs = false then you won't get the functions from array subscripts =#
  local crefs::List{Absyn.ComponentRef}

  @assign crefs = begin
    local crefs1::List{Absyn.ComponentRef}
    local exp::Absyn.Exp
    local subs::List{Absyn.Subscript}
    @match (isubs, includeSubs, includeFunctions) begin
      (nil(), _, _) => begin
        nil
      end

      (Absyn.NOSUB(__) <| subs, _, _) => begin
        getCrefsFromSubs(subs, includeSubs, includeFunctions)
      end

      (Absyn.SUBSCRIPT(exp) <| subs, _, _) => begin
        @assign crefs1 = getCrefsFromSubs(subs, includeSubs, includeFunctions)
        @assign crefs = getCrefFromExp(exp, includeSubs, includeFunctions)
        listAppend(crefs, crefs1)
      end
    end
  end
  return crefs
end

""" #= Returns a flattened list of the
   component references in an expression =#"""
function getCrefFromExp(
  inExp::Absyn.Exp,
  includeSubs::Bool,
  includeFunctions::Bool,
)::List{Absyn.ComponentRef} #= note that if you say includeSubs = false then you won't get the functions from array subscripts =#
  local outComponentRefLst::List{Absyn.ComponentRef}

  @assign outComponentRefLst = begin
    local cr::Absyn.ComponentRef
    local l1::List{Absyn.ComponentRef}
    local l2::List{Absyn.ComponentRef}
    local res::List{Absyn.ComponentRef}
    local e1::ComponentCondition
    local e2::ComponentCondition
    local e3::ComponentCondition
    local op::Absyn.Operator
    local e4::List{Tuple{ComponentCondition, ComponentCondition}}
    local farg::Absyn.FunctionArgs
    local expl::List{ComponentCondition}
    local expll::List{List{ComponentCondition}}
    local subs::List{Absyn.Subscript}
    local lstres1::List{List{Absyn.ComponentRef}}
    local crefll::List{List{Absyn.ComponentRef}}
    @match (inExp, includeSubs, includeFunctions) begin
      (INTEGER(__), _, _) => begin
        nil
      end

      (REAL(__), _, _) => begin
        nil
      end

      (Absyn.STRING(__), _, _) => begin
        nil
      end

      (BOOL(__), _, _) => begin
        nil
      end

      (Absyn.CREF(componentRef = Absyn.ALLWILD(__)), _, _) => begin
        nil
      end

      (Absyn.CREF(componentRef = Absyn.WILD(__)), _, _) => begin
        nil
      end

      (Absyn.CREF(componentRef = cr), false, _) => begin
        list(cr)
      end

      (Absyn.CREF(componentRef = cr), true, _) => begin
        @assign subs = getSubsFromCref(cr, includeSubs, includeFunctions)
        @assign l1 = getCrefsFromSubs(subs, includeSubs, includeFunctions)
        _cons(cr, l1)
      end

      (Absyn.BINARY(exp1 = e1, exp2 = e2), _, _) => begin
        @assign l1 = getCrefFromExp(e1, includeSubs, includeFunctions)
        @assign l2 = getCrefFromExp(e2, includeSubs, includeFunctions)
        @assign res = listAppend(l1, l2)
        res
      end

      (Absyn.UNARY(exp = e1), _, _) => begin
        @assign res = getCrefFromExp(e1, includeSubs, includeFunctions)
        res
      end

      (Absyn.LBINARY(exp1 = e1, exp2 = e2), _, _) => begin
        @assign l1 = getCrefFromExp(e1, includeSubs, includeFunctions)
        @assign l2 = getCrefFromExp(e2, includeSubs, includeFunctions)
        @assign res = listAppend(l1, l2)
        res
      end

      (Absyn.LUNARY(exp = e1), _, _) => begin
        @assign res = getCrefFromExp(e1, includeSubs, includeFunctions)
        res
      end

      (Absyn.RELATION(exp1 = e1, exp2 = e2), _, _) => begin
        @assign l1 = getCrefFromExp(e1, includeSubs, includeFunctions)
        @assign l2 = getCrefFromExp(e2, includeSubs, includeFunctions)
        @assign res = listAppend(l1, l2)
        res
      end

      (Absyn.IFEXP(ifExp = e1, trueBranch = e2, elseBranch = e3), _, _) => begin
        ListUtil.flatten(list(
          getCrefFromExp(e1, includeSubs, includeFunctions),
          getCrefFromExp(e2, includeSubs, includeFunctions),
          getCrefFromExp(e3, includeSubs, includeFunctions),
        ))
      end

      (Absyn.CALL(function_ = cr, functionArgs = farg), _, _) => begin
        @assign res = getCrefFromFarg(farg, includeSubs, includeFunctions)
        @assign res = if includeFunctions
          _cons(cr, res)
        else
          res
        end
        res
      end

      (PARTEVALFUNCTION(function_ = cr, functionArgs = farg), _, _) => begin
        @assign res = getCrefFromFarg(farg, includeSubs, includeFunctions)
        @assign res = if includeFunctions
          _cons(cr, res)
        else
          res
        end
        res
      end

      (ARRAY(arrayExp = expl), _, _) => begin
        @assign lstres1 = ListUtil.map2(expl, getCrefFromExp, includeSubs, includeFunctions)
        @assign res = ListUtil.flatten(lstres1)
        res
      end

      (MATRIX(matrix = expll), _, _) => begin
        @assign res = ListUtil.flatten(ListUtil.flatten(ListUtil.map2List(
          expll,
          getCrefFromExp,
          includeSubs,
          includeFunctions,
        )))
        res
      end

      (RANGE(start = e1, step = SOME(e3), stop = e2), _, _) => begin
        @assign l1 = getCrefFromExp(e1, includeSubs, includeFunctions)
        @assign l2 = getCrefFromExp(e2, includeSubs, includeFunctions)
        @assign l2 = listAppend(l1, l2)
        @assign l1 = getCrefFromExp(e3, includeSubs, includeFunctions)
        @assign res = listAppend(l1, l2)
        res
      end

      (RANGE(start = e1, step = NONE(), stop = e2), _, _) => begin
        @assign l1 = getCrefFromExp(e1, includeSubs, includeFunctions)
        @assign l2 = getCrefFromExp(e2, includeSubs, includeFunctions)
        @assign res = listAppend(l1, l2)
        res
      end

      (END(__), _, _) => begin
        nil
      end

      (Absyn.TUPLE(expressions = expl), _, _) => begin
        @assign crefll = ListUtil.map2(expl, getCrefFromExp, includeSubs, includeFunctions)
        @assign res = ListUtil.flatten(crefll)
        res
      end

      (CODE(__), _, _) => begin
        nil
      end

      (AS(exp = e1), _, _) => begin
        getCrefFromExp(e1, includeSubs, includeFunctions)
      end

      (CONS(e1, e2), _, _) => begin
        @assign l1 = getCrefFromExp(e1, includeSubs, includeFunctions)
        @assign l2 = getCrefFromExp(e2, includeSubs, includeFunctions)
        @assign res = listAppend(l1, l2)
        res
      end

      (LIST(expl), _, _) => begin
        @assign crefll = ListUtil.map2(expl, getCrefFromExp, includeSubs, includeFunctions)
        @assign res = ListUtil.flatten(crefll)
        res
      end

      (MATCHEXP(__), _, _) => begin
        fail()
      end

      (DOT(__), _, _) => begin
        getCrefFromExp(inExp.exp, includeSubs, includeFunctions)
      end

      _ => begin
        Error.addInternalError(
          getInstanceName() + " failed " + Dump.printExpStr(inExp),
          sourceInfo(),
        )
        fail()
      end
    end
  end
  #=  TODO: Handle else if-branches.
  =#
  #=  inExp.index is only allowed to contain names to index the function call; not crefs that are evaluated in any way
  =#
  return outComponentRefLst
end

""" #= Returns the flattened list of all component references
  present in a list of function arguments. =#"""
function getCrefFromFarg(
  inFunctionArgs::Absyn.FunctionArgs,
  includeSubs::Bool,
  includeFunctions::Bool,
)::List{Absyn.ComponentRef} #= note that if you say includeSubs = false then you won't get the functions from array subscripts =#
  local outComponentRefLst::List{Absyn.ComponentRef}

  @assign outComponentRefLst = begin
    local l1::List{List{Absyn.ComponentRef}}
    local l2::List{List{Absyn.ComponentRef}}
    local fl1::List{Absyn.ComponentRef}
    local fl2::List{Absyn.ComponentRef}
    local fl3::List{Absyn.ComponentRef}
    local res::List{Absyn.ComponentRef}
    local expl::List{ComponentCondition}
    local nargl::List{Absyn.NamedArg}
    local iterators::Absyn.ForIterators
    local exp::Absyn.Exp
    @match (inFunctionArgs, includeSubs, includeFunctions) begin
      (Absyn.FUNCTIONARGS(args = expl, argNames = nargl), _, _) => begin
        @assign l1 = ListUtil.map2(expl, getCrefFromExp, includeSubs, includeFunctions)
        @assign fl1 = ListUtil.flatten(l1)
        @assign l2 = ListUtil.map2(nargl, getCrefFromNarg, includeSubs, includeFunctions)
        @assign fl2 = ListUtil.flatten(l2)
        @assign res = listAppend(fl1, fl2)
        res
      end

      (Absyn.FOR_ITER_FARG(exp, _, iterators), _, _) => begin
        @assign l1 = ListUtil.map2Option(
          ListUtil.map(iterators, iteratorRange),
          getCrefFromExp,
          includeSubs,
          includeFunctions,
        )
        @assign l2 = ListUtil.map2Option(
          ListUtil.map(iterators, iteratorGuard),
          getCrefFromExp,
          includeSubs,
          includeFunctions,
        )
        @assign fl1 = ListUtil.flatten(l1)
        @assign fl2 = ListUtil.flatten(l2)
        @assign fl3 = getCrefFromExp(exp, includeSubs, includeFunctions)
        @assign res = listAppend(fl1, listAppend(fl2, fl3))
        res
      end
    end
  end
  return outComponentRefLst
end

function iteratorName(iterator::Absyn.ForIterator)::String
  local name::String

  @match Absyn.ITERATOR(name = name) = iterator
  return name
end

function iteratorRange(iterator::Absyn.ForIterator)::Option{Absyn.Exp}
  local range::Option{Absyn.Exp}

  @match Absyn.ITERATOR(range = range) = iterator
  return range
end

function iteratorGuard(iterator::Absyn.ForIterator)::Option{Absyn.Exp}
  local guardExp::Option{Absyn.Exp}

  @match Absyn.ITERATOR(guardExp = guardExp) = iterator
  return guardExp
end

#=  stefan
=#

""" #= returns the names from a list of NamedArgs as a string list =#"""
function getNamedFuncArgNamesAndValues(
  inNamedArgList::List{<:Absyn.NamedArg},
)::Tuple{List{String}, List{Absyn.Exp}}
  local outExpList::List{Absyn.Exp}
  local outStringList::List{String}

  @assign (outStringList, outExpList) = begin
    local cdr::List{Absyn.NamedArg}
    local s::String
    local e::Absyn.Exp
    local slst::List{String}
    local elst::List{Absyn.Exp}
    @match inNamedArgList begin
      nil() => begin
        (nil, nil)
      end

      Absyn.NAMEDARG(argName = s, argValue = e) <| cdr => begin
        @assign (slst, elst) = getNamedFuncArgNamesAndValues(cdr)
        (_cons(s, slst), _cons(e, elst))
      end
    end
  end
  return (outStringList, outExpList)
end

""" #= Returns the flattened list of all component references
  present in a list of named function arguments. =#"""
function getCrefFromNarg(
  inNamedArg::Absyn.NamedArg,
  includeSubs::Bool,
  includeFunctions::Bool,
)::List{Absyn.ComponentRef} #= note that if you say includeSubs = false then you won't get the functions from array subscripts =#
  local outComponentRefLst::List{Absyn.ComponentRef}

  @assign outComponentRefLst = begin
    local res::List{Absyn.ComponentRef}
    local exp::ComponentCondition
    @match (inNamedArg, includeSubs, includeFunctions) begin
      (Absyn.NAMEDARG(argValue = exp), _, _) => begin
        @assign res = getCrefFromExp(exp, includeSubs, includeFunctions)
        res
      end
    end
  end
  return outComponentRefLst
end

""" #= This function joins two paths =#"""
function joinPaths(inPath1::Absyn.Path, inPath2::Absyn.Path)::Absyn.Path
  local outPath::Absyn.Path

  @assign outPath = begin
    local str::Absyn.Ident
    local p2::Absyn.Path
    local p_1::Absyn.Path
    local p::Absyn.Path
    @match (inPath1, inPath2) begin
      (Absyn.IDENT(name = str), p2) => begin
        Absyn.QUALIFIED(str, p2)
      end

      (Absyn.QUALIFIED(name = str, path = p), p2) => begin
        @assign p_1 = joinPaths(p, p2)
        Absyn.QUALIFIED(str, p_1)
      end

      (Absyn.FULLYQUALIFIED(p), p2) => begin
        joinPaths(p, p2)
      end

      (p, Absyn.FULLYQUALIFIED(p2)) => begin
        joinPaths(p, p2)
      end
    end
  end
  return outPath
end

""" #= This function joins two paths when the first one might be NONE =#"""
function joinPathsOpt(inPath1::Option{<:Absyn.Path}, inPath2::Absyn.Path)::Absyn.Path
  local outPath::Absyn.Path

  @assign outPath = begin
    local p::Absyn.Path
    @match (inPath1, inPath2) begin
      (NONE(), _) => begin
        inPath2
      end

      (SOME(p), _) => begin
        joinPaths(p, inPath2)
      end
    end
  end
  return outPath
end

function joinPathsOptSuffix(inPath1::Absyn.Path, inPath2::Option{<:Absyn.Path})::Absyn.Path
  local outPath::Absyn.Path

  @assign outPath = begin
    local p::Absyn.Path
    @match (inPath1, inPath2) begin
      (_, SOME(p)) => begin
        joinPaths(inPath1, p)
      end

      _ => begin
        inPath1
      end
    end
  end
  return outPath
end

""" #= This function selects the second path when the first one
  is NONE() otherwise it will select the first one. =#"""
function selectPathsOpt(inPath1::Option{<:Absyn.Path}, inPath2::Absyn.Path)::Absyn.Path
  local outPath::Absyn.Path

  @assign outPath = begin
    local p::Absyn.Path
    @match (inPath1, inPath2) begin
      (NONE(), p) => begin
        p
      end

      (SOME(p), _) => begin
        p
      end
    end
  end
  return outPath
end

""" #= author Lucian
  This function joins a path list =#"""
function pathAppendList(inPathLst::List{<:Absyn.Path})::Absyn.Path
  local outPath::Absyn.Path

  @assign outPath = begin
    local path::Absyn.Path
    local res_path::Absyn.Path
    local first::Absyn.Path
    local rest::List{Absyn.Path}
    @match inPathLst begin
      nil() => begin
        Absyn.IDENT("")
      end

      path <| nil() => begin
        path
      end

      first <| rest => begin
        @assign path = pathAppendList(rest)
        @assign res_path = joinPaths(first, path)
        res_path
      end
    end
  end
  return outPath
end

""" #= Returns the path given as argument to
  the function minus the last ident. =#"""
function stripLast(inPath::Absyn.Path)::Absyn.Path
  local outPath::Absyn.Path

  @assign outPath = begin
    local str::Absyn.Ident
    local p::Absyn.Path
    @match inPath begin
      Absyn.QUALIFIED(name = str, path = Absyn.IDENT(__)) => begin
        Absyn.IDENT(str)
      end

      Absyn.QUALIFIED(name = str, path = p) => begin
        @assign p = stripLast(p)
        Absyn.QUALIFIED(str, p)
      end

      Absyn.FULLYQUALIFIED(p) => begin
        @assign p = stripLast(p)
        Absyn.FULLYQUALIFIED(p)
      end
    end
  end
  return outPath
end

function stripLastOpt(inPath::Absyn.Path)::Option{Absyn.Path}
  local outPath::Option{Absyn.Path}

  @assign outPath = begin
    local p::Absyn.Path
    @match inPath begin
      Absyn.IDENT(__) => begin
        NONE()
      end

      _ => begin
        @assign p = stripLast(inPath)
        SOME(p)
      end
    end
  end
  return outPath
end

""" #= Returns the path given as argument to
  the function minus the last ident. =#"""
function crefStripLast(inCref::Absyn.ComponentRef)::Absyn.ComponentRef
  local outCref::Absyn.ComponentRef

  @assign outCref = begin
    local str::Absyn.Ident
    local c_1::Absyn.ComponentRef
    local c::Absyn.ComponentRef
    local subs::List{Absyn.Subscript}
    @match inCref begin
      Absyn.CREF_IDENT(__) => begin
        fail()
      end

      Absyn.CREF_QUAL(name = str, subscripts = subs, componentRef = Absyn.CREF_IDENT(__)) => begin
        Absyn.CREF_IDENT(str, subs)
      end

      Absyn.CREF_QUAL(name = str, subscripts = subs, componentRef = c) => begin
        @assign c_1 = crefStripLast(c)
        Absyn.CREF_QUAL(str, subs, c_1)
      end

      Absyn.CREF_FULLYQUALIFIED(componentRef = c) => begin
        @assign c_1 = crefStripLast(c)
        crefMakeFullyQualified(c_1)
      end
    end
  end
  return outCref
end

""" #=
Author BZ 2008-04
Function for splitting Absynpath into two parts,
qualified part, and ident part (all_but_last, last);
 =#"""
function splitQualAndIdentPath(inPath::Absyn.Path)::Tuple{Absyn.Path, Absyn.Path}
  local outPath2::Absyn.Path
  local outPath1::Absyn.Path

  @assign (outPath1, outPath2) = begin
    local qPath::Absyn.Path
    local curPath::Absyn.Path
    local identPath::Absyn.Path
    local s1::String
    local s2::String
    @match inPath begin
      Absyn.QUALIFIED(name = s1, path = Absyn.IDENT(name = s2)) => begin
        (Absyn.IDENT(s1), Absyn.IDENT(s2))
      end

      Absyn.QUALIFIED(name = s1, path = qPath) => begin
        @assign (curPath, identPath) = splitQualAndIdentPath(qPath)
        (Absyn.QUALIFIED(s1, curPath), identPath)
      end

      Absyn.FULLYQUALIFIED(qPath) => begin
        @assign (curPath, identPath) = splitQualAndIdentPath(qPath)
        (curPath, identPath)
      end
    end
  end
  return (outPath1, outPath2)
end

""" #= Returns the path given as argument
  to the function minus the first ident. =#"""
function stripFirst(inPath::Absyn.Path)::Absyn.Path
  local outPath::Absyn.Path

  @assign outPath = begin
    local p::Absyn.Path
    @match inPath begin
      Absyn.QUALIFIED(path = p) => begin
        p
      end

      Absyn.FULLYQUALIFIED(p) => begin
        stripFirst(p)
      end
    end
  end
  return outPath
end

""" #= This function converts a Absyn.ComponentRef to a Absyn.Path, if possible.
  If the component reference contains subscripts, it will silently fail. =#"""
function crefToPath(inComponentRef::Absyn.ComponentRef)::Absyn.Path
  local outPath::Absyn.Path

  @assign outPath = begin
    local i::Absyn.Ident
    local p::Absyn.Path
    local c::Absyn.ComponentRef
    @match inComponentRef begin
      Absyn.CREF_IDENT(name = i, subscripts = nil()) => begin
        Absyn.IDENT(i)
      end

      Absyn.CREF_QUAL(name = i, subscripts = nil(), componentRef = c) => begin
        @assign p = crefToPath(c)
        Absyn.QUALIFIED(i, p)
      end

      Absyn.CREF_FULLYQUALIFIED(componentRef = c) => begin
        @assign p = crefToPath(c)
        Absyn.FULLYQUALIFIED(p)
      end
    end
  end
  return outPath
end

""" #= This function converts a Absyn.ElementSpec to a Absyn.Path, if possible.
  If the Absyn.ElementSpec is not EXTENDS, it will silently fail. =#"""
function elementSpecToPath(inElementSpec::Absyn.ElementSpec)::Absyn.Path
  local outPath::Absyn.Path

  @assign outPath = begin
    local p::Absyn.Path
    @match inElementSpec begin
      EXTENDS(path = p) => begin
        p
      end
    end
  end
  return outPath
end

""" #= Converts a Absyn.ComponentRef to a Absyn.Path, ignoring any subscripts. =#"""
function crefToPathIgnoreSubs(inComponentRef::Absyn.ComponentRef)::Absyn.Path
  local outPath::Absyn.Path

  @assign outPath = begin
    local i::Absyn.Ident
    local p::Absyn.Path
    local c::Absyn.ComponentRef
    @match inComponentRef begin
      Absyn.CREF_IDENT(name = i) => begin
        Absyn.IDENT(i)
      end

      Absyn.CREF_QUAL(name = i, componentRef = c) => begin
        @assign p = crefToPathIgnoreSubs(c)
        Absyn.QUALIFIED(i, p)
      end

      Absyn.CREF_FULLYQUALIFIED(componentRef = c) => begin
        @assign p = crefToPathIgnoreSubs(c)
        Absyn.FULLYQUALIFIED(p)
      end
    end
  end
  return outPath
end

""" #= This function converts a Absyn.Path to a Absyn.ComponentRef. =#"""
function pathToCref(inPath::Absyn.Path)::Absyn.ComponentRef
  local outComponentRef::Absyn.ComponentRef

  @assign outComponentRef = begin
    local i::Absyn.Ident
    local c::Absyn.ComponentRef
    local p::Absyn.Path
    @match inPath begin
      Absyn.IDENT(name = i) => begin
        Absyn.CREF_IDENT(i, nil)
      end

      Absyn.QUALIFIED(name = i, path = p) => begin
        @assign c = pathToCref(p)
        Absyn.CREF_QUAL(i, nil, c)
      end

      Absyn.FULLYQUALIFIED(p) => begin
        @assign c = pathToCref(p)
        crefMakeFullyQualified(c)
      end
    end
  end
  return outComponentRef
end

""" #= This function converts a Absyn.Path to a Absyn.ComponentRef, and applies the given
  subscripts to the last identifier. =#"""
function pathToCrefWithSubs(inPath::Absyn.Path, inSubs::List{<:Absyn.Subscript})::Absyn.ComponentRef
  local outComponentRef::Absyn.ComponentRef

  @assign outComponentRef = begin
    local i::Absyn.Ident
    local c::Absyn.ComponentRef
    local p::Absyn.Path
    @match (inPath, inSubs) begin
      (Absyn.IDENT(name = i), _) => begin
        Absyn.CREF_IDENT(i, inSubs)
      end

      (Absyn.QUALIFIED(name = i, path = p), _) => begin
        @assign c = pathToCrefWithSubs(p, inSubs)
        Absyn.CREF_QUAL(i, nil, c)
      end

      (Absyn.FULLYQUALIFIED(p), _) => begin
        @assign c = pathToCrefWithSubs(p, inSubs)
        crefMakeFullyQualified(c)
      end
    end
  end
  return outComponentRef
end

""" #= Returns the last identifier in a component reference. =#"""
function crefLastIdent(inComponentRef::Absyn.ComponentRef)::Absyn.Ident
  local outIdent::Absyn.Ident

  @assign outIdent = begin
    local cref::Absyn.ComponentRef
    local id::Absyn.Ident
    @match inComponentRef begin
      Absyn.CREF_IDENT(name = id) => begin
        id
      end

      Absyn.CREF_QUAL(componentRef = cref) => begin
        crefLastIdent(cref)
      end

      Absyn.CREF_FULLYQUALIFIED(componentRef = cref) => begin
        crefLastIdent(cref)
      end
    end
  end
  return outIdent
end

""" #= Returns the basename of the component reference, but fails if it encounters
  any subscripts. =#"""
function crefFirstIdentNoSubs(inCref::Absyn.ComponentRef)::Absyn.Ident
  local outIdent::Absyn.Ident

  @assign outIdent = begin
    local id::Absyn.Ident
    local cr::Absyn.ComponentRef
    @match inCref begin
      Absyn.CREF_IDENT(name = id, subscripts = nil()) => begin
        id
      end

      Absyn.CREF_QUAL(name = id, subscripts = nil()) => begin
        id
      end

      Absyn.CREF_FULLYQUALIFIED(componentRef = cr) => begin
        crefFirstIdentNoSubs(cr)
      end
    end
  end
  return outIdent
end

""" #= Returns true if the component reference is a simple identifier, otherwise false. =#"""
function crefIsIdent(inComponentRef::Absyn.ComponentRef)::Bool
  local outIsIdent::Bool

  @assign outIsIdent = begin
    @match inComponentRef begin
      Absyn.CREF_IDENT(__) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return outIsIdent
end

""" #= Returns true if the component reference is a qualified identifier, otherwise false. =#"""
function crefIsQual(inComponentRef::Absyn.ComponentRef)::Bool
  local outIsQual::Bool

  @assign outIsQual = begin
    @match inComponentRef begin
      Absyn.CREF_QUAL(__) => begin
        true
      end

      Absyn.CREF_FULLYQUALIFIED(__) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return outIsQual
end

""" #= Return the last subscripts of an Absyn.ComponentRef =#"""
function crefLastSubs(inComponentRef::Absyn.ComponentRef)::List{Absyn.Subscript}
  local outSubscriptLst::List{Absyn.Subscript}

  @assign outSubscriptLst = begin
    local id::Absyn.Ident
    local subs::List{Absyn.Subscript}
    local res::List{Absyn.Subscript}
    local cr::Absyn.ComponentRef
    @match inComponentRef begin
      Absyn.CREF_IDENT(subscripts = subs) => begin
        subs
      end

      Absyn.CREF_QUAL(componentRef = cr) => begin
        @assign res = crefLastSubs(cr)
        res
      end

      Absyn.CREF_FULLYQUALIFIED(componentRef = cr) => begin
        @assign res = crefLastSubs(cr)
        res
      end
    end
  end
  return outSubscriptLst
end

function crefSetLastSubs(
  inCref::Absyn.ComponentRef,
  inSubscripts::List{<:Absyn.Subscript},
)::Absyn.ComponentRef
  local outCref::Absyn.ComponentRef = inCref

  @assign outCref = begin
    @match outCref begin
      Absyn.CREF_IDENT(__) => begin
        @assign outCref.subscripts = inSubscripts
        outCref
      end

      Absyn.CREF_QUAL(__) => begin
        @assign outCref.componentRef = crefSetLastSubs(outCref.componentRef, inSubscripts)
        outCref
      end

      Absyn.CREF_FULLYQUALIFIED(__) => begin
        @assign outCref.componentRef = crefSetLastSubs(outCref.componentRef, inSubscripts)
        outCref
      end
    end
  end
  return outCref
end

""" #= This function finds if a cref has subscripts =#"""
function crefHasSubscripts(cref::Absyn.ComponentRef)::Bool
  local hasSubscripts::Bool

  @assign hasSubscripts = begin
    @match cref begin
      Absyn.CREF_IDENT(__) => begin
        !listEmpty(cref.subscripts)
      end

      Absyn.CREF_QUAL(subscripts = nil()) => begin
        crefHasSubscripts(cref.componentRef)
      end

      Absyn.CREF_FULLYQUALIFIED(__) => begin
        crefHasSubscripts(cref.componentRef)
      end

      Absyn.WILD(__) => begin
        false
      end

      Absyn.ALLWILD(__) => begin
        false
      end

      _ => begin
        true
      end
    end
  end
  return hasSubscripts
end

""" #=
Author: BZ, 2009-09
 Extract subscripts of crefs. =#"""
function getSubsFromCref(
  cr::Absyn.ComponentRef,
  includeSubs::Bool,
  includeFunctions::Bool,
)::List{Absyn.Subscript} #= note that if you say includeSubs = false then you won't get the functions from array subscripts =#
  local subscripts::List{Absyn.Subscript}

  @assign subscripts = begin
    local subs2::List{Absyn.Subscript}
    local child::Absyn.ComponentRef
    @match (cr, includeSubs, includeFunctions) begin
      (Absyn.CREF_IDENT(_, subs2), _, _) => begin
        subs2
      end

      (Absyn.CREF_QUAL(_, subs2, child), _, _) => begin
        @assign subscripts = getSubsFromCref(child, includeSubs, includeFunctions)
        @assign subscripts = ListUtil.unionOnTrue(subscripts, subs2, subscriptEqual)
        subscripts
      end

      (Absyn.CREF_FULLYQUALIFIED(child), _, _) => begin
        @assign subscripts = getSubsFromCref(child, includeSubs, includeFunctions)
        subscripts
      end
    end
  end
  return subscripts
end

#=  stefan
=#

""" #= Gets the last ident in a Absyn.ComponentRef =#"""
function crefGetLastIdent(inComponentRef::Absyn.ComponentRef)::Absyn.ComponentRef
  local outComponentRef::Absyn.ComponentRef

  @assign outComponentRef = begin
    local cref::Absyn.ComponentRef
    local cref_1::Absyn.ComponentRef
    local id::Absyn.Ident
    local subs::List{Absyn.Subscript}
    @match inComponentRef begin
      Absyn.CREF_IDENT(id, subs) => begin
        Absyn.CREF_IDENT(id, subs)
      end

      Absyn.CREF_QUAL(_, _, cref) => begin
        @assign cref_1 = crefGetLastIdent(cref)
        cref_1
      end

      Absyn.CREF_FULLYQUALIFIED(cref) => begin
        @assign cref_1 = crefGetLastIdent(cref)
        cref_1
      end
    end
  end
  return outComponentRef
end

""" #= Strips the last subscripts of a Absyn.ComponentRef =#"""
function crefStripLastSubs(inComponentRef::Absyn.ComponentRef)::Absyn.ComponentRef
  local outComponentRef::Absyn.ComponentRef

  @assign outComponentRef = begin
    local id::Absyn.Ident
    local subs::List{Absyn.Subscript}
    local s::List{Absyn.Subscript}
    local cr_1::Absyn.ComponentRef
    local cr::Absyn.ComponentRef
    @match inComponentRef begin
      Absyn.CREF_IDENT(name = id) => begin
        Absyn.CREF_IDENT(id, nil)
      end

      Absyn.CREF_QUAL(name = id, subscripts = s, componentRef = cr) => begin
        @assign cr_1 = crefStripLastSubs(cr)
        Absyn.CREF_QUAL(id, s, cr_1)
      end

      Absyn.CREF_FULLYQUALIFIED(componentRef = cr) => begin
        @assign cr_1 = crefStripLastSubs(cr)
        crefMakeFullyQualified(cr_1)
      end
    end
  end
  return outComponentRef
end

""" #= This function joins two ComponentRefs. =#"""
function joinCrefs(
  inComponentRef1::Absyn.ComponentRef,
  inComponentRef2::Absyn.ComponentRef,
)::Absyn.ComponentRef
  local outComponentRef::Absyn.ComponentRef

  @assign outComponentRef = begin
    local id::Absyn.Ident
    local sub::List{Absyn.Subscript}
    local cr2::Absyn.ComponentRef
    local cr_1::Absyn.ComponentRef
    local cr::Absyn.ComponentRef
    @match (inComponentRef1, inComponentRef2) begin
      (Absyn.CREF_IDENT(name = id, subscripts = sub), cr2) => begin
        @shouldFail @match Absyn.CREF_FULLYQUALIFIED() = cr2
        Absyn.CREF_QUAL(id, sub, cr2)
      end

      (Absyn.CREF_QUAL(name = id, subscripts = sub, componentRef = cr), cr2) => begin
        @assign cr_1 = joinCrefs(cr, cr2)
        Absyn.CREF_QUAL(id, sub, cr_1)
      end

      (Absyn.CREF_FULLYQUALIFIED(componentRef = cr), cr2) => begin
        @assign cr_1 = joinCrefs(cr, cr2)
        crefMakeFullyQualified(cr_1)
      end
    end
  end
  return outComponentRef
end

""" #= Returns first ident from a Absyn.ComponentRef =#"""
function crefFirstIdent(inCref::Absyn.ComponentRef)::Absyn.Ident
  local outIdent::Absyn.Ident

  @assign outIdent = begin
    @match inCref begin
      Absyn.CREF_IDENT(__) => begin
        inCref.name
      end

      Absyn.CREF_QUAL(__) => begin
        inCref.name
      end

      Absyn.CREF_FULLYQUALIFIED(__) => begin
        crefFirstIdent(inCref.componentRef)
      end
    end
  end
  return outIdent
end

function crefSecondIdent(cref::Absyn.ComponentRef)::Absyn.Ident
  local ident::Absyn.Ident

  @assign ident = begin
    @match cref begin
      Absyn.CREF_QUAL(__) => begin
        crefFirstIdent(cref.componentRef)
      end

      Absyn.CREF_FULLYQUALIFIED(__) => begin
        crefSecondIdent(cref.componentRef)
      end
    end
  end
  return ident
end

""" #= Returns the first part of a cref. =#"""
function crefFirstCref(inCref::Absyn.ComponentRef)::Absyn.ComponentRef
  local outCref::Absyn.ComponentRef

  @assign outCref = begin
    @match inCref begin
      Absyn.CREF_QUAL(__) => begin
        Absyn.CREF_IDENT(inCref.name, inCref.subscripts)
      end

      Absyn.CREF_FULLYQUALIFIED(__) => begin
        crefFirstCref(inCref.componentRef)
      end

      _ => begin
        inCref
      end
    end
  end
  return outCref
end

""" #= Strip the first ident from a Absyn.ComponentRef =#"""
function crefStripFirst(inComponentRef::Absyn.ComponentRef)::Absyn.ComponentRef
  local outComponentRef::Absyn.ComponentRef

  @assign outComponentRef = begin
    local cr::Absyn.ComponentRef
    @match inComponentRef begin
      Absyn.CREF_QUAL(componentRef = cr) => begin
        cr
      end

      Absyn.CREF_FULLYQUALIFIED(componentRef = cr) => begin
        crefStripFirst(cr)
      end
    end
  end
  return outComponentRef
end

function crefIsFullyQualified(inCref::Absyn.ComponentRef)::Bool
  local outIsFullyQualified::Bool

  @assign outIsFullyQualified = begin
    @match inCref begin
      Absyn.CREF_FULLYQUALIFIED(__) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return outIsFullyQualified
end

""" #= Makes a component reference fully qualified unless it already is. =#"""
function crefMakeFullyQualified(inComponentRef::Absyn.ComponentRef)::Absyn.ComponentRef
  local outComponentRef::Absyn.ComponentRef

  @assign outComponentRef = begin
    @match inComponentRef begin
      Absyn.CREF_FULLYQUALIFIED(__) => begin
        inComponentRef
      end

      _ => begin
        Absyn.CREF_FULLYQUALIFIED(inComponentRef)
      end
    end
  end
  return outComponentRef
end

""" #= Maps a class restriction to the corresponding string for printing =#"""
function restrString(inRestriction::Absyn.Restriction)::String
  local outString::String

  @assign outString = begin
    @match inRestriction begin
      Absyn.R_CLASS(__) => begin
        "Absyn.CLASS"
      end

      Absyn.R_OPTIMIZATION(__) => begin
        "OPTIMIZATION"
      end

      Absyn.R_MODEL(__) => begin
        "MODEL"
      end

      Absyn.R_RECORD(__) => begin
        "RECORD"
      end

      Absyn.R_BLOCK(__) => begin
        "BLOCK"
      end

      Absyn.R_CONNECTOR(__) => begin
        "CONNECTOR"
      end

      Absyn.R_EXP_CONNECTOR(__) => begin
        "EXPANDABLE CONNECTOR"
      end

      Absyn.R_TYPE(__) => begin
        "TYPE"
      end

      Absyn.R_PACKAGE(__) => begin
        "PACKAGE"
      end

      Absyn.R_FUNCTION(Absyn.FR_NORMAL_FUNCTION(Absyn.PURE(__))) => begin
        "PURE FUNCTION"
      end

      Absyn.R_FUNCTION(Absyn.FR_NORMAL_FUNCTION(Absyn.IMPURE(__))) => begin
        "IMPURE FUNCTION"
      end

      Absyn.R_FUNCTION(Absyn.FR_NORMAL_FUNCTION(Absyn.NO_PURITY(__))) => begin
        "FUNCTION"
      end

      Absyn.R_FUNCTION(Absyn.FR_OPERATOR_FUNCTION(__)) => begin
        "OPERATOR FUNCTION"
      end

      Absyn.R_PREDEFINED_INTEGER(__) => begin
        "PREDEFINED_INT"
      end

      Absyn.R_PREDEFINED_REAL(__) => begin
        "PREDEFINED_REAL"
      end

      Absyn.R_PREDEFINED_STRING(__) => begin
        "PREDEFINED_STRING"
      end

      Absyn.R_PREDEFINED_BOOLEAN(__) => begin
        "PREDEFINED_BOOL"
      end

      Absyn.R_PREDEFINED_CLOCK(__) => begin
        "PREDEFINED_CLOCK"
      end

      Absyn.R_UNIONTYPE(__) => begin
        "UNIONTYPE"
      end

      _ => begin
        "* Unknown restriction *"
      end
    end
  end
  #=  BTH
  =#
  #= /* MetaModelica restriction */ =#
  return outString
end

""" #= Returns the path (=name) of the last class in a program =#"""
function lastClassname(inProgram::Absyn.Program)::Absyn.Path
  local outPath::Absyn.Path

  local lst::List{Absyn.Class}
  local id::Absyn.Ident

  @match Absyn.PROGRAM(classes = lst) = inProgram
  @match Absyn.CLASS(name = id) = ListUtil.last(lst)
  @assign outPath = Absyn.IDENT(id)
  return outPath
end

""" #= Retrieves the filename where the class is stored. =#"""
function classFilename(inClass::Absyn.Class)::String
  local outFilename::String

  @match Absyn.CLASS(info = SOURCEINFO(fileName = outFilename)) = inClass
  return outFilename
end

""" #= Sets the filename where the class is stored. =#"""
function setClassFilename(inClass::Absyn.Class, fileName::String)::Absyn.Class
  local outClass::Absyn.Class

  @assign outClass = begin
    local info::SourceInfo
    local cl::Absyn.Class
    @match inClass begin
      cl && Absyn.CLASS(info = info && SOURCEINFO(__)) => begin
        @assign info.fileName = fileName
        @assign cl.info = info
        cl
      end
    end
  end
  return outClass
end

""" #= author: BZ
  Sets the name of the class =#"""
function setClassName(inClass::Absyn.Class, newName::String)::Absyn.Class
  local outClass::Absyn.Class = inClass

  @assign outClass = begin
    @match outClass begin
      Absyn.CLASS(__) => begin
        @assign outClass.name = newName
        outClass
      end
    end
  end
  return outClass
end

function setClassBody(inClass::Absyn.Class, inBody::Absyn.ClassDef)::Absyn.Class
  local outClass::Absyn.Class = inClass

  @assign outClass = begin
    @match outClass begin
      Absyn.CLASS(__) => begin
        @assign outClass.body = inBody
        outClass
      end
    end
  end
  return outClass
end

""" #=  Checks if the name of a Absyn.ComponentRef is
 equal to the name of another Absyn.ComponentRef, including subscripts.
 See also crefEqualNoSubs. =#"""
function crefEqual(iCr1::Absyn.ComponentRef, iCr2::Absyn.ComponentRef)::Bool
  local outBoolean::Bool

  @assign outBoolean = begin
    local id::Absyn.Ident
    local id2::Absyn.Ident
    local ss1::List{Absyn.Subscript}
    local ss2::List{Absyn.Subscript}
    local cr1::Absyn.ComponentRef
    local cr2::Absyn.ComponentRef
    @matchcontinue (iCr1, iCr2) begin
      (Absyn.CREF_IDENT(name = id, subscripts = ss1), Absyn.CREF_IDENT(name = id2, subscripts = ss2)) => begin
        @match true = stringEq(id, id2)
        @match true = subscriptsEqual(ss1, ss2)
        true
      end

      (
        Absyn.CREF_QUAL(name = id, subscripts = ss1, componentRef = cr1),
        Absyn.CREF_QUAL(name = id2, subscripts = ss2, componentRef = cr2),
      ) => begin
        @match true = stringEq(id, id2)
        @match true = subscriptsEqual(ss1, ss2)
        @match true = crefEqual(cr1, cr2)
        true
      end

      (Absyn.CREF_FULLYQUALIFIED(componentRef = cr1), Absyn.CREF_FULLYQUALIFIED(componentRef = cr2)) => begin
        crefEqual(cr1, cr2)
      end

      _ => begin
        false
      end
    end
  end
  return outBoolean
end

""" #= @author: adrpo
   a.b, a -> true
   b.c, a -> false =#"""
function crefFirstEqual(iCr1::Absyn.ComponentRef, iCr2::Absyn.ComponentRef)::Bool
  local outBoolean::Bool

  @assign outBoolean = stringEq(crefFirstIdent(iCr1), crefFirstIdent(iCr2))
  return outBoolean
end

function subscriptEqual(inSubscript1::Absyn.Subscript, inSubscript2::Absyn.Subscript)::Bool
  local outIsEqual::Bool

  @assign outIsEqual = begin
    local e1::Absyn.Exp
    local e2::Absyn.Exp
    @match (inSubscript1, inSubscript2) begin
      (Absyn.NOSUB(__), Absyn.NOSUB(__)) => begin
        true
      end

      (Absyn.SUBSCRIPT(e1), Absyn.SUBSCRIPT(e2)) => begin
        expEqual(e1, e2)
      end

      _ => begin
        false
      end
    end
  end
  return outIsEqual
end

""" #= Checks if two subscript lists are equal. =#"""
function subscriptsEqual(inSubList1::List{<:Absyn.Subscript}, inSubList2::List{<:Absyn.Subscript})::Bool
  local outIsEqual::Bool

  @assign outIsEqual = ListUtil.isEqualOnTrue(inSubList1, inSubList2, subscriptEqual)
  return outIsEqual
end

""" #= Checks if the name of a Absyn.ComponentRef is equal to the name
   of another Absyn.ComponentRef without checking subscripts.
   See also crefEqual. =#"""
function crefEqualNoSubs(cr1::Absyn.ComponentRef, cr2::Absyn.ComponentRef)::Bool
  local outBoolean::Bool

  @assign outBoolean = begin
    local rest1::Absyn.ComponentRef
    local rest2::Absyn.ComponentRef
    local id::Absyn.Ident
    local id2::Absyn.Ident
    @matchcontinue (cr1, cr2) begin
      (Absyn.CREF_IDENT(name = id), Absyn.CREF_IDENT(name = id2)) => begin
        @match true = stringEq(id, id2)
        true
      end

      (
        Absyn.CREF_QUAL(name = id, componentRef = rest1),
        Absyn.CREF_QUAL(name = id2, componentRef = rest2),
      ) => begin
        @match true = stringEq(id, id2)
        @match true = crefEqualNoSubs(rest1, rest2)
        true
      end

      (
        Absyn.CREF_FULLYQUALIFIED(componentRef = rest1),
        Absyn.CREF_FULLYQUALIFIED(componentRef = rest2),
      ) => begin
        crefEqualNoSubs(rest1, rest2)
      end

      _ => begin
        false
      end
    end
  end
  return outBoolean
end

""" #= checks if the provided parameter is a package or not =#"""
function isPackageRestriction(inRestriction::Absyn.Restriction)::Bool
  local outIsPackage::Bool

  @assign outIsPackage = begin
    @match inRestriction begin
      R_PACKAGE(__) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return outIsPackage
end

""" #= checks if restriction is a function or not =#"""
function isFunctionRestriction(inRestriction::Absyn.Restriction)::Bool
  local outIsFunction::Bool

  @assign outIsFunction = begin
    @match inRestriction begin
      R_FUNCTION(__) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return outIsFunction
end

""" #= Returns true if two expressions are equal =#"""
function expEqual(exp1::Absyn.Exp, exp2::Absyn.Exp)::Bool
  local equal::Bool

  @assign equal = begin
    local b::Bool
    local x::Absyn.Exp
    local y::Absyn.Exp
    local i::Integer
    local r::String
    #=  real vs. integer
    =#
    @matchcontinue (exp1, exp2) begin
      (INTEGER(i), REAL(r)) => begin
        @assign b = realEq(intReal(i), System.stringReal(r))
        b
      end

      (REAL(r), INTEGER(i)) => begin
        @assign b = realEq(intReal(i), System.stringReal(r))
        b
      end

      (x, y) => begin
        valueEq(x, y)
      end
    end
  end
  #=  anything else, exact match!
  =#
  return equal
end

""" #= Returns true if two each attributes are equal =#"""
function eachEqual(each1::Absyn.Each, each2::Absyn.Each)::Bool
  local equal::Bool

  @assign equal = begin
    @match (each1, each2) begin
      (Absyn.NON_EACH(__), Absyn.NON_EACH(__)) => begin
        true
      end

      (Absyn.EACH(__), Absyn.EACH(__)) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return equal
end

""" #= Returns true if two Absyn.FunctionArgs are equal =#"""
function functionArgsEqual(args1::Absyn.FunctionArgs, args2::Absyn.FunctionArgs)::Bool
  local equal::Bool

  @assign equal = begin
    local expl1::List{Absyn.Exp}
    local expl2::List{Absyn.Exp}
    @match (args1, args2) begin
      (Absyn.FUNCTIONARGS(args = expl1), Absyn.FUNCTIONARGS(args = expl2)) => begin
        ListUtil.isEqualOnTrue(expl1, expl2, expEqual)
      end

      _ => begin
        false
      end
    end
  end
  return equal
end

""" #= author: adrpo
  gets the name of the class. =#"""
function getClassName(inClass::Absyn.Class)::String
  local outName::String

  @match Absyn.CLASS(name = outName) = inClass
  return outName
end

IteratorIndexedCref = Tuple

""" #= Find all crefs in an expression which are subscripted with the given
   iterator, and return a list of cref-Integer tuples, where the cref is the
   index of the subscript. =#"""
function findIteratorIndexedCrefs(
  inExp::Absyn.Exp,
  inIterator::String,
  inCrefs::List{<:IteratorIndexedCref} = nil,
)::List{IteratorIndexedCref}
  local outCrefs::List{IteratorIndexedCref}

  @assign (_, outCrefs) = traverseExp(
    inExp,
    (inIterator) -> findIteratorIndexedCrefs_traverser(inIterator = inIterator),
    nil,
  )
  @assign outCrefs = ListUtil.fold(
    outCrefs,
    (iteratorIndexedCrefsEqual) ->
      ListUtil.unionEltOnTrue(inCompFunc = iteratorIndexedCrefsEqual),
    inCrefs,
  )
  return outCrefs
end

""" #= Traversal function used by deduceReductionIterationRange. Used to find crefs
   which are subscripted by a given iterator. =#"""
function findIteratorIndexedCrefs_traverser(
  inExp::Absyn.Exp,
  inCrefs::List{<:IteratorIndexedCref},
  inIterator::String,
)::Tuple{Absyn.Exp, List{IteratorIndexedCref}}
  local outCrefs::List{IteratorIndexedCref}
  local outExp::Absyn.Exp = inExp

  @assign outCrefs = begin
    local cref::Absyn.ComponentRef
    @match inExp begin
      Absyn.CREF(componentRef = cref) => begin
        getIteratorIndexedCrefs(cref, inIterator, inCrefs)
      end

      _ => begin
        inCrefs
      end
    end
  end
  return (outExp, outCrefs)
end

""" #= Checks whether two cref-index pairs are equal. =#"""
function iteratorIndexedCrefsEqual(
  inCref1::IteratorIndexedCref,
  inCref2::IteratorIndexedCref,
)::Bool
  local outEqual::Bool

  local cr1::Absyn.ComponentRef
  local cr2::Absyn.ComponentRef
  local idx1::Integer
  local idx2::Integer

  @assign (cr1, idx1) = inCref1
  @assign (cr2, idx2) = inCref2
  @assign outEqual = idx1 == idx2 && crefEqual(cr1, cr2)
  return outEqual
end

""" #= Checks if the given component reference is subscripted by the given iterator.
   Only cases where a subscript consists of only the iterator is considered.
   If so it adds a cref-index pair to the list, where the cref is the subscripted
   cref without subscripts, and the index is the subscripted dimension. E.g. for
   iterator i:
     a[i] => (a, 1), b[1, i] => (b, 2), c[i+1] => (), d[2].e[i] => (d[2].e, 1) =#"""
function getIteratorIndexedCrefs(
  inCref::Absyn.ComponentRef,
  inIterator::String,
  inCrefs::List{<:IteratorIndexedCref},
)::List{IteratorIndexedCref}
  local outCrefs::List{IteratorIndexedCref} = inCrefs

  local crefs::List{Tuple{Absyn.ComponentRef, Integer}}

  @assign outCrefs = begin
    local subs::List{Absyn.Subscript}
    local idx::Integer
    local name::String
    local id::String
    local cref::Absyn.ComponentRef
    @match inCref begin
      Absyn.CREF_IDENT(name = id, subscripts = subs) => begin
        #=  For each subscript, check if the subscript consists of only the
        =#
        #=  iterator we're looking for.
        =#
        @assign idx = 1
        for sub in subs
          @assign _ = begin
            @match sub begin
              Absyn.SUBSCRIPT(
                subscript = Absyn.CREF(
                  componentRef = Absyn.CREF_IDENT(name = name, subscripts = nil()),
                ),
              ) => begin
                if name == inIterator
                  @assign outCrefs = _cons((Absyn.CREF_IDENT(id, nil), idx), outCrefs)
                end
                ()
              end

              _ => begin
                ()
              end
            end
          end
          @assign idx = idx + 1
        end
        outCrefs
      end

      Absyn.CREF_QUAL(name = id, subscripts = subs, componentRef = cref) => begin
        @assign crefs = getIteratorIndexedCrefs(cref, inIterator, nil)
        #=  Append the prefix from the qualified cref to any matches, and add
        =#
        #=  them to the result list.
        =#
        for cr in crefs
          @assign (cref, idx) = cr
          @assign outCrefs = _cons((Absyn.CREF_QUAL(id, subs, cref), idx), outCrefs)
        end
        getIteratorIndexedCrefs(Absyn.CREF_IDENT(id, subs), inIterator, outCrefs)
      end

      Absyn.CREF_FULLYQUALIFIED(componentRef = cref) => begin
        @assign crefs = getIteratorIndexedCrefs(cref, inIterator, nil)
        #=  Make any matches fully qualified, and add them to the result list.
        =#
        for cr in crefs
          @assign (cref, idx) = cr
          @assign outCrefs = _cons((Absyn.CREF_FULLYQUALIFIED(cref), idx), outCrefs)
        end
        outCrefs
      end

      _ => begin
        inCrefs
      end
    end
  end
  return outCrefs
end

function pathReplaceIdent(path::Absyn.Path, last::String)::Absyn.Path
  local out::Absyn.Path

  @assign out = begin
    local p::Absyn.Path
    local n::String
    local s::String
    @match (path, last) begin
      (Absyn.FULLYQUALIFIED(p), s) => begin
        @assign p = pathReplaceIdent(p, s)
        Absyn.FULLYQUALIFIED(p)
      end

      (Absyn.QUALIFIED(n, p), s) => begin
        @assign p = pathReplaceIdent(p, s)
        Absyn.QUALIFIED(n, p)
      end

      (Absyn.IDENT(__), s) => begin
        Absyn.IDENT(s)
      end
    end
  end
  return out
end

function getFileNameFromInfo(inInfo::SourceInfo)::String
  local inFileName::String

  @match SOURCEINFO(fileName = inFileName) = inInfo
  return inFileName
end

""" #= @author: adrpo
  this function returns true if the given Absyn.InnerOuter
  is one of Absyn.INNER_OUTER() or Absyn.OUTER() =#"""
function isOuter(io::Absyn.InnerOuter)::Bool
  local isItAnOuter::Bool

  @assign isItAnOuter = begin
    @match io begin
      Absyn.INNER_OUTER(__) => begin
        true
      end

      Absyn.OUTER(__) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return isItAnOuter
end

""" #= @author: adrpo
  this function returns true if the given Absyn.InnerOuter
  is one of Absyn.INNER_OUTER() or Absyn.INNER() =#"""
function isInner(io::Absyn.InnerOuter)::Bool
  local isItAnInner::Bool

  @assign isItAnInner = begin
    @match io begin
      Absyn.INNER_OUTER(__) => begin
        true
      end

      Absyn.INNER(__) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return isItAnInner
end

""" #= Returns true if the Absyn.InnerOuter is Absyn.INNER, false otherwise. =#"""
function isOnlyInner(inIO::Absyn.InnerOuter)::Bool
  local outOnlyInner::Bool

  @assign outOnlyInner = begin
    @match inIO begin
      Absyn.INNER(__) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return outOnlyInner
end

""" #= Returns true if the Absyn.InnerOuter is Absyn.OUTER, false otherwise. =#"""
function isOnlyOuter(inIO::Absyn.InnerOuter)::Bool
  local outOnlyOuter::Bool

  @assign outOnlyOuter = begin
    @match inIO begin
      Absyn.OUTER(__) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return outOnlyOuter
end

function isInnerOuter(inIO::Absyn.InnerOuter)::Bool
  local outIsInnerOuter::Bool

  @assign outIsInnerOuter = begin
    @match inIO begin
      Absyn.INNER_OUTER(__) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return outIsInnerOuter
end

function isNotInnerOuter(inIO::Absyn.InnerOuter)::Bool
  local outIsNotInnerOuter::Bool

  @assign outIsNotInnerOuter = begin
    @match inIO begin
      Absyn.NOT_INNER_OUTER(__) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return outIsNotInnerOuter
end

""" #= Returns true if two Absyn.InnerOuter's are equal =#"""
function innerOuterEqual(io1::Absyn.InnerOuter, io2::Absyn.InnerOuter)::Bool
  local res::Bool

  @assign res = begin
    @match (io1, io2) begin
      (Absyn.INNER(__), Absyn.INNER(__)) => begin
        true
      end

      (Absyn.OUTER(__), Absyn.OUTER(__)) => begin
        true
      end

      (Absyn.INNER_OUTER(__), Absyn.INNER_OUTER(__)) => begin
        true
      end

      (Absyn.NOT_INNER_OUTER(__), Absyn.NOT_INNER_OUTER(__)) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return res
end

""" #= Makes a path fully qualified unless it already is. =#"""
function makeFullyQualified(inPath::Absyn.Path)::Absyn.Path
  local outPath::Absyn.Path

  @assign outPath = begin
    @match inPath begin
      Absyn.FULLYQUALIFIED(__) => begin
        inPath
      end

      _ => begin
        Absyn.FULLYQUALIFIED(inPath)
      end
    end
  end
  return outPath
end

""" #= Makes a path not fully qualified unless it already is. =#"""
function makeNotFullyQualified(inPath::Absyn.Path)::Absyn.Path
  local outPath::Absyn.Path

  @assign outPath = begin
    local path::Absyn.Path
    @match inPath begin
      Absyn.FULLYQUALIFIED(path) => begin
        path
      end

      _ => begin
        inPath
      end
    end
  end
  return outPath
end

""" #= Compares two import elements.  =#"""
function importEqual(im1::Absyn.Import, im2::Absyn.Import)::Bool
  local outBoolean::Bool

  @assign outBoolean = begin
    local id::Absyn.Ident
    local id2::Absyn.Ident
    local p1::Absyn.Path
    local p2::Absyn.Path
    @matchcontinue (im1, im2) begin
      (Absyn.NAMED_IMPORT(name = id, path = p1), Absyn.NAMED_IMPORT(name = id2, path = p2)) => begin
        @match true = stringEq(id, id2)
        @match true = pathEqual(p1, p2)
        true
      end

      (Absyn.QUAL_IMPORT(path = p1), Absyn.QUAL_IMPORT(path = p2)) => begin
        @match true = pathEqual(p1, p2)
        true
      end

      (Absyn.UNQUAL_IMPORT(path = p1), Absyn.UNQUAL_IMPORT(path = p2)) => begin
        @match true = pathEqual(p1, p2)
        true
      end

      _ => begin
        false
      end
    end
  end
  return outBoolean
end

""" #= Transforms an if-expression to canonical form (without else-if branches) =#"""
function canonIfExp(inExp::Absyn.Exp)::Absyn.Exp
  local outExp::Absyn.Exp

  @assign outExp = begin
    local cond::Absyn.Exp
    local tb::Absyn.Exp
    local eb::Absyn.Exp
    local ei_cond::Absyn.Exp
    local ei_tb::Absyn.Exp
    local e::Absyn.Exp
    local eib::List{Tuple{Absyn.Exp, Absyn.Exp}}
    @match inExp begin
      Absyn.IFEXP(elseIfBranch = nil()) => begin
        inExp
      end

      Absyn.IFEXP(
        ifExp = cond,
        trueBranch = tb,
        elseBranch = eb,
        elseIfBranch = (ei_cond, ei_tb) <| eib,
      ) => begin
        @assign e = canonIfExp(Absyn.IFEXP(ei_cond, ei_tb, eb, eib))
        Absyn.IFEXP(cond, tb, e, nil)
      end
    end
  end
  return outExp
end

""" #= @author: adrpo
  This function checks if a modification only contains literal expressions =#"""
function onlyLiteralsInAnnotationMod(inMod::List{<:Absyn.ElementArg})::Bool
  local onlyLiterals::Bool

  @assign onlyLiterals = begin
    local dive::List{Absyn.ElementArg}
    local rest::List{Absyn.ElementArg}
    local eqMod::Absyn.EqMod
    local b1::Bool
    local b2::Bool
    local b3::Bool
    local b::Bool
    @matchcontinue inMod begin
      nil() => begin
        true
      end

      Absyn.MODIFICATION(path = Absyn.IDENT(name = "interaction")) <| rest => begin
        @assign b = onlyLiteralsInAnnotationMod(rest)
        b
      end

      Absyn.MODIFICATION(modification = SOME(Absyn.CLASSMOD(dive, eqMod))) <| rest => begin
        @assign b1 = onlyLiteralsInEqMod(eqMod)
        @assign b2 = onlyLiteralsInAnnotationMod(dive)
        @assign b3 = onlyLiteralsInAnnotationMod(rest)
        @assign b = boolAnd(b1, boolAnd(b2, b3))
        b
      end

      _ <| rest => begin
        @assign b = onlyLiteralsInAnnotationMod(rest)
        b
      end

      _ => begin
        false
      end
    end
  end
  #=  skip \"interaction\" annotation!
  =#
  #=  search inside, some(exp)
  =#
  #=  failed above, return false
  =#
  return onlyLiterals
end

""" #= @author: adrpo
  This function checks if an optional expression only contains literal expressions =#"""
function onlyLiteralsInEqMod(eqMod::Absyn.EqMod)::Bool
  local onlyLiterals::Bool

  @assign onlyLiterals = begin
    local exp::Absyn.Exp
    local lst::List{Absyn.Exp}
    local b::Bool
    @match eqMod begin
      Absyn.NOMOD(__) => begin
        true
      end

      Absyn.EQMOD(exp = exp) => begin
        @match (_, _cons(lst, nil)) = traverseExpBidir(
          exp,
          onlyLiteralsInExpEnter,
          onlyLiteralsInExpExit,
          _cons(nil, nil),
        )
        @assign b = listEmpty(lst)
        b
      end
    end
  end
  #=  search inside, some(exp)
  =#
  return onlyLiterals
end

""" #= @author: adrpo
 Visitor function for checking if Absyn.Exp contains only literals, NO CREFS!
 It returns an empty list if it doesn't contain any crefs! =#"""
function onlyLiteralsInExpEnter(
  inExp::Absyn.Exp,
  inLst::List{<:List{<:Absyn.Exp}},
)::Tuple{Absyn.Exp, List{List{Absyn.Exp}}}
  local outLst::List{List{Absyn.Exp}}
  local outExp::Absyn.Exp

  @assign (outExp, outLst) = begin
    local b::Bool
    local e::Absyn.Exp
    local cr::Absyn.ComponentRef
    local lst::List{Absyn.Exp}
    local rest::List{List{Absyn.Exp}}
    local name::String
    local fargs::Absyn.FunctionArgs
    #=  first handle all graphic enumerations!
    =#
    #=  FillPattern.*, Smooth.*, TextAlignment.*, etc!
    =#
    @match (inExp, inLst) begin
      (e && Absyn.CREF(Absyn.CREF_QUAL(name = name)), lst <| rest) => begin
        @assign b = listMember(
          name,
          list(
            "LinePattern",
            "Arrow",
            "FillPattern",
            "BorderPattern",
            "TextStyle",
            "Smooth",
            "TextAlignment",
          ),
        )
        @assign lst = ListUtil.consOnTrue(!b, e, lst)
        (inExp, _cons(lst, rest))
      end

      (Absyn.CREF(__), lst <| rest) => begin
        (inExp, _cons(_cons(inExp, lst), rest))
      end

      _ => begin
        (inExp, inLst)
      end
    end
  end
  #=  crefs, add to list
  =#
  #=  anything else, return the same!
  =#
  return (outExp, outLst)
end

""" #= @author: adrpo
 Visitor function for checking if Absyn.Exp contains only literals, NO CREFS!
 It returns an empty list if it doesn't contain any crefs! =#"""
function onlyLiteralsInExpExit(
  inExp::Absyn.Exp,
  inLst::List{<:List{<:Absyn.Exp}},
)::Tuple{Absyn.Exp, List{List{Absyn.Exp}}}
  local outLst::List{List{Absyn.Exp}}
  local outExp::Absyn.Exp

  @assign (outExp, outLst) = begin
    local lst::List{List{Absyn.Exp}}
    #=  first handle DynamicSelect; pop the stack (ignore any crefs inside DynamicSelect)
    =#
    @match (inExp, inLst) begin
      (Absyn.CALL(function_ = Absyn.CREF_IDENT(name = "DynamicSelect")), lst) => begin
        (inExp, lst)
      end

      _ => begin
        (inExp, inLst)
      end
    end
  end
  #=  anything else, return the same!
  =#
  return (outExp, outLst)
end

function makeCons(e1::Absyn.Exp, e2::Absyn.Exp)::Absyn.Exp
  local e::Absyn.Exp

  @assign e = CONS(e1, e2)
  return e
end

function crefIdent(cr::Absyn.ComponentRef)::String
  local str::String

  @match Absyn.CREF_IDENT(str, nil) = cr
  return str
end

function unqotePathIdents(inPath::Absyn.Path)::Absyn.Path
  local path::Absyn.Path

  @assign path =
    stringListPath(ListUtil.map(pathToStringList(inPath), System.unquoteIdentifier))
  return path
end

""" #= If the given component reference is fully qualified this function removes the
  fully qualified qualifier, otherwise does nothing. =#"""
function unqualifyCref(inCref::Absyn.ComponentRef)::Absyn.ComponentRef
  local outCref::Absyn.ComponentRef

  @assign outCref = begin
    local cref::Absyn.ComponentRef
    @match inCref begin
      Absyn.CREF_FULLYQUALIFIED(componentRef = cref) => begin
        cref
      end

      _ => begin
        inCref
      end
    end
  end
  return outCref
end

function pathIsFullyQualified(inPath::Absyn.Path)::Bool
  local outIsQualified::Bool

  @assign outIsQualified = begin
    @match inPath begin
      Absyn.FULLYQUALIFIED(__) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return outIsQualified
end

function pathIsIdent(inPath::Absyn.Path)::Bool
  local outIsIdent::Bool

  @assign outIsIdent = begin
    @match inPath begin
      Absyn.IDENT(__) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return outIsIdent
end

function pathIsQual(inPath::Absyn.Path)::Bool
  local outIsQual::Bool

  @assign outIsQual = begin
    @match inPath begin
      Absyn.QUALIFIED(__) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return outIsQual
end

function withinEqual(within1::Absyn.Within, within2::Absyn.Within)::Bool
  local b::Bool

  @assign b = begin
    local p1::Absyn.Path
    local p2::Absyn.Path
    @match (within1, within2) begin
      (Absyn.TOP(__), Absyn.TOP(__)) => begin
        true
      end

      (Absyn.WITHIN(p1), Absyn.WITHIN(p2)) => begin
        pathEqual(p1, p2)
      end

      _ => begin
        false
      end
    end
  end
  return b
end

function withinString(w1::Absyn.Within)::String
  local str::String

  @assign str = begin
    local p1::Absyn.Path
    @match w1 begin
      Absyn.TOP(__) => begin
        "within ;"
      end

      Absyn.WITHIN(p1) => begin
        "within " + pathString(p1)
        +";"
      end
    end
  end
  return str
end

function joinWithinPath(within_::Absyn.Within, path::Absyn.Path)::Absyn.Path
  local outPath::Absyn.Path

  @assign outPath = begin
    local path1::Absyn.Path
    @match (within_, path) begin
      (Absyn.TOP(__), _) => begin
        path
      end

      (Absyn.WITHIN(path1), _) => begin
        joinPaths(path1, path)
      end
    end
  end
  return outPath
end

function innerOuterStr(io::Absyn.InnerOuter)::String
  local str::String

  @assign str = begin
    @match io begin
      Absyn.INNER_OUTER(__) => begin
        "inner outer "
      end

      Absyn.INNER(__) => begin
        "inner "
      end

      Absyn.OUTER(__) => begin
        "outer "
      end

      Absyn.NOT_INNER_OUTER(__) => begin
        ""
      end
    end
  end
  return str
end

function subscriptExpOpt(inSub::Absyn.Subscript)::Option{Absyn.Exp}
  local outExpOpt::Option{Absyn.Exp}

  @assign outExpOpt = begin
    local e::Absyn.Exp
    @match inSub begin
      Absyn.SUBSCRIPT(subscript = e) => begin
        SOME(e)
      end

      Absyn.NOSUB(__) => begin
        NONE()
      end
    end
  end
  return outExpOpt
end

function crefInsertSubscriptLstLst(
  inExp::Absyn.Exp,
  inLst::List{<:List{<:Absyn.Subscript}},
)::Tuple{Absyn.Exp, List{List{Absyn.Subscript}}}
  local outLst::List{List{Absyn.Subscript}}
  local outExp::Absyn.Exp

  @assign (outExp, outLst) = begin
    local cref::Absyn.ComponentRef
    local cref2::Absyn.ComponentRef
    local subs::List{List{Absyn.Subscript}}
    local e::Absyn.Exp
    @matchcontinue (inExp, inLst) begin
      (Absyn.CREF(componentRef = cref), subs) => begin
        @assign cref2 = crefInsertSubscriptLstLst2(cref, subs)
        (Absyn.CREF(cref2), subs)
      end

      _ => begin
        (inExp, inLst)
      end
    end
  end
  return (outExp, outLst)
end

""" #= Helper function to crefInsertSubscriptLstLst =#"""
function crefInsertSubscriptLstLst2(
  inCref::Absyn.ComponentRef,
  inSubs::List{<:List{<:Absyn.Subscript}},
)::Absyn.ComponentRef
  local outCref::Absyn.ComponentRef

  @assign outCref = begin
    local cref::Absyn.ComponentRef
    local cref2::Absyn.ComponentRef
    local n::Absyn.Ident
    local subs::List{List{Absyn.Subscript}}
    local s::List{Absyn.Subscript}
    @matchcontinue (inCref, inSubs) begin
      (cref, nil()) => begin
        cref
      end

      (Absyn.CREF_IDENT(name = n), s <| nil()) => begin
        Absyn.CREF_IDENT(n, s)
      end

      (Absyn.CREF_QUAL(name = n, componentRef = cref), s <| subs) => begin
        @assign cref2 = crefInsertSubscriptLstLst2(cref, subs)
        Absyn.CREF_QUAL(n, s, cref2)
      end

      (Absyn.CREF_FULLYQUALIFIED(componentRef = cref), subs) => begin
        @assign cref2 = crefInsertSubscriptLstLst2(cref, subs)
        crefMakeFullyQualified(cref2)
      end
    end
  end
  return outCref
end

function isCref(exp::Absyn.Exp)::Bool
  local b::Bool

  @assign b = begin
    @match exp begin
      Absyn.CREF(__) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return b
end

function isTuple(exp::Absyn.Exp)::Bool
  local b::Bool

  @assign b = begin
    @match exp begin
      Absyn.TUPLE(__) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return b
end

""" #= @author: johti
   Returns true if all fields are crefs =#"""
function allFieldsAreCrefs(expLst::List{<:Absyn.Exp})::Bool
  local b::Bool

  @assign b = ListUtil.mapAllValueBool(expLst, complexIsCref, true)
  return b
end

""" #=  @author: johti
    Returns true if everything contained
    in the tuple or a cons cell is a constant reference. =#"""
function complexIsCref(inExp::Absyn.Exp)::Bool
  local b::Bool

  @assign b = begin
    @match inExp begin
      Absyn.TUPLE(__) => begin
        allFieldsAreCrefs(inExp.expressions)
      end

      CONS(__) => begin
        complexIsCref(inExp.head) && complexIsCref(inExp.rest)
      end

      _ => begin
        isCref(inExp)
      end
    end
  end
  return b
end

function isDerCref(exp::Absyn.Exp)::Bool
  local b::Bool

  @assign b = begin
    @match exp begin
      Absyn.CALL(Absyn.CREF_IDENT("der", nil()), Absyn.FUNCTIONARGS(Absyn.CREF(__) <| nil(), nil())) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return b
end

function isDerCrefFail(exp::Absyn.Exp)
  return @match Absyn.CALL(Absyn.CREF_IDENT("der", nil), Absyn.FUNCTIONARGS(list(Absyn.CREF()), nil)) = exp
end

""" #= author: adrpo
  returns all the expressions from array dimension as a list
  also returns if we have unknown dimensions in the array dimension =#"""
function getExpsFromArrayDim(inAd::Absyn.ArrayDim)::Tuple{Bool, List{Absyn.Exp}}
  local outExps::List{Absyn.Exp}
  local hasUnknownDimensions::Bool

  @assign (hasUnknownDimensions, outExps) = getExpsFromArrayDim_tail(inAd, nil)
  return (hasUnknownDimensions, outExps)
end

""" #= author: adrpo
  returns all the expressions from array dimension as a list
  also returns if we have unknown dimensions in the array dimension =#"""
function getExpsFromArrayDimOpt(inAdO::Option{<:Absyn.ArrayDim})::Tuple{Bool, List{Absyn.Exp}}
  local outExps::List{Absyn.Exp}
  local hasUnknownDimensions::Bool

  @assign (hasUnknownDimensions, outExps) = begin
    local ad::Absyn.ArrayDim
    @match inAdO begin
      NONE() => begin
        (false, nil)
      end

      SOME(ad) => begin
        @assign (hasUnknownDimensions, outExps) = getExpsFromArrayDim_tail(ad, nil)
        (hasUnknownDimensions, outExps)
      end
    end
  end
  return (hasUnknownDimensions, outExps)
end

""" #= author: adrpo
  returns all the expressions from array dimension as a list
  also returns if we have unknown dimensions in the array dimension =#"""
function getExpsFromArrayDim_tail(
  inAd::Absyn.ArrayDim,
  inAccumulator::List{<:Absyn.Exp},
)::Tuple{Bool, List{Absyn.Exp}}
  local outExps::List{Absyn.Exp}
  local hasUnknownDimensions::Bool

  @assign (hasUnknownDimensions, outExps) = begin
    local rest::List{Absyn.Subscript}
    local e::Absyn.Exp
    local exps::List{Absyn.Exp}
    local acc::List{Absyn.Exp}
    local b::Bool
    #=  handle empty list
    =#
    @match (inAd, inAccumulator) begin
      (nil(), acc) => begin
        (false, listReverse(acc))
      end

      (Absyn.SUBSCRIPT(e) <| rest, acc) => begin
        @assign (b, exps) = getExpsFromArrayDim_tail(rest, _cons(e, acc))
        (b, exps)
      end

      (Absyn.NOSUB(__) <| rest, acc) => begin
        @assign (_, exps) = getExpsFromArrayDim_tail(rest, acc)
        (true, exps)
      end
    end
  end
  #=  handle Absyn.SUBSCRIPT
  =#
  #=  handle Absyn.NOSUB
  =#
  return (hasUnknownDimensions, outExps)
end

""" #= @author: adrpo
 returns true if the given direction is input or output =#"""
function isInputOrOutput(direction::Absyn.Direction)::Bool
  local isIorO::Bool #= input or output only =#

  @assign isIorO = begin
    @match direction begin
      Absyn.INPUT(__) => begin
        true
      end

      Absyn.INPUTOUTPUT(__) => begin
        true
      end

      Absyn.INPUT_OUTPUT(__) => begin
        true
      end

      Absyn.BIDIR(__) => begin
        false
      end
    end
  end
  return isIorO #= input or output only =#
end

function isInput(inDirection::Absyn.Direction)::Bool
  local outIsInput::Bool

  @assign outIsInput = begin
    @match inDirection begin
      Absyn.INPUT(__) => begin
        true
      end

      Absyn.INPUT_OUTPUT(__) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return outIsInput
end

function isOutput(inDirection::Absyn.Direction)::Bool
  local outIsOutput::Bool

  @assign outIsOutput = begin
    @match inDirection begin
      Absyn.OUTPUT(__) => begin
        true
      end

      Absyn.INPUT_OUTPUT(__) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return outIsOutput
end

function directionEqual(inDirection1::Absyn.Direction, inDirection2::Absyn.Direction)::Bool
  local outEqual::Bool

  @assign outEqual = begin
    @match (inDirection1, inDirection2) begin
      (Absyn.BIDIR(__), Absyn.BIDIR(__)) => begin
        true
      end

      (Absyn.INPUT(__), Absyn.INPUT(__)) => begin
        true
      end

      (Absyn.OUTPUT(__), Absyn.OUTPUT(__)) => begin
        true
      end

      (Absyn.INPUT_OUTPUT(__), Absyn.INPUT_OUTPUT(__)) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return outEqual
end

function isFieldEqual(isField1::Absyn.IsField, isField2::Absyn.IsField)::Bool
  local outEqual::Bool

  @assign outEqual = begin
    @match (isField1, isField2) begin
      (Absyn.NONFIELD(__), Absyn.NONFIELD(__)) => begin
        true
      end

      (Absyn.FIELD(__), Absyn.FIELD(__)) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return outEqual
end

function pathLt(path1::Absyn.Path, path2::Absyn.Path)::Bool
  local lt::Bool

  @assign lt = stringCompare(pathString(path1), pathString(path2)) < 0
  return lt
end

function pathGe(path1::Absyn.Path, path2::Absyn.Path)::Bool
  local ge::Bool

  @assign ge = !pathLt(path1, path2)
  return ge
end

""" #= Strips out long class definitions =#"""
function getShortClass(cl::Absyn.Class)::Absyn.Class
  local o::Absyn.Class

  @assign o = begin
    local name::Absyn.Ident
    local pa::Bool
    local fi::Bool
    local en::Bool
    local re::Absyn.Restriction
    local body::Absyn.ClassDef
    local info::Info
    @match cl begin
      Absyn.CLASS(body = Absyn.PARTS(__)) => begin
        fail()
      end

      Absyn.CLASS(body = Absyn.CLASS_EXTENDS(__)) => begin
        fail()
      end

      Absyn.CLASS(name, pa, fi, en, re, body, info) => begin
        @assign body = stripClassDefComment(body)
        Absyn.CLASS(name, pa, fi, en, re, body, info)
      end
    end
  end
  return o
end

""" #= Strips out class definition comments. =#"""
function stripClassDefComment(cl::Absyn.ClassDef)::Absyn.ClassDef
  local o::Absyn.ClassDef

  @assign o = begin
    local enumLiterals::EnumDef
    local typeSpec::Absyn.TypeSpec
    local attributes::ElementAttributes
    local arguments::List{Absyn.ElementArg}
    local functionNames::List{Absyn.Path}
    local functionName::Absyn.Path
    local vars::List{Absyn.Ident}
    local typeVars::List{String}
    local baseClassName::Absyn.Ident
    local modifications::List{Absyn.ElementArg}
    local parts::List{Absyn.ClassPart}
    local classAttrs::List{Absyn.NamedArg}
    local ann::List{Absyn.Annotation}
    @match cl begin
      Absyn.PARTS(typeVars, classAttrs, parts, ann, _) => begin
        Absyn.PARTS(typeVars, classAttrs, parts, ann, NONE())
      end

      Absyn.CLASS_EXTENDS(baseClassName, modifications, _, parts, ann) => begin
        Absyn.CLASS_EXTENDS(baseClassName, modifications, NONE(), parts, ann)
      end

      DERIVED(typeSpec, attributes, arguments, _) => begin
        DERIVED(typeSpec, attributes, arguments, NONE())
      end

      ENUMERATION(enumLiterals, _) => begin
        ENUMERATION(enumLiterals, NONE())
      end

      OVERLOAD(functionNames, _) => begin
        OVERLOAD(functionNames, NONE())
      end

      PDER(functionName, vars, _) => begin
        PDER(functionName, vars, NONE())
      end

      _ => begin
        cl
      end
    end
  end
  return o
end

""" #= Strips out the parts of a function definition that are not needed for the interface =#"""
function getFunctionInterface(cl::Absyn.Class)::Absyn.Class
  local o::Absyn.Class

  @assign o = begin
    local name::Absyn.Ident
    local partialPrefix::Bool
    local finalPrefix::Bool
    local encapsulatedPrefix::Bool
    local info::Info
    local typeVars::List{String}
    local classParts::List{Absyn.ClassPart}
    local elts::List{Absyn.ElementItem}
    local funcRest::FunctionRestriction
    local classAttr::List{Absyn.NamedArg}
    @match cl begin
      Absyn.CLASS(
        name,
        partialPrefix,
        finalPrefix,
        encapsulatedPrefix,
        R_FUNCTION(funcRest),
        Absyn.PARTS(typeVars, classAttr, classParts, _, _),
        info,
      ) => begin
        @match (@match _cons(_, _) = elts) =
          ListUtil.fold(listReverse(classParts), getFunctionInterfaceParts, nil)
        Absyn.CLASS(
          name,
          partialPrefix,
          finalPrefix,
          encapsulatedPrefix,
          R_FUNCTION(funcRest),
          Absyn.PARTS(typeVars, classAttr, _cons(PUBLIC(elts), nil), nil, NONE()),
          info,
        )
      end
    end
  end
  return o
end

function getFunctionInterfaceParts(
  part::Absyn.ClassPart,
  elts::List{<:Absyn.ElementItem},
)::List{Absyn.ElementItem}
  local oelts::List{Absyn.ElementItem}

  @assign oelts = begin
    local elts1::List{Absyn.ElementItem}
    local elts2::List{Absyn.ElementItem}
    @match (part, elts) begin
      (PUBLIC(elts1), elts2) => begin
        @assign elts1 = ListUtil.filterOnTrue(elts1, filterAnnotationItem)
        listAppend(elts1, elts2)
      end

      _ => begin
        elts
      end
    end
  end
  return oelts
end

function filterAnnotationItem(elt::Absyn.ElementItem)::Bool
  local outB::Bool

  @assign outB = begin
    @match elt begin
      Absyn.ELEMENTITEM(__) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return outB
end

""" #= Filter outs the nested classes from the class if any. =#"""
function filterNestedClasses(cl::Absyn.Class)::Absyn.Class
  local o::Absyn.Class

  @assign o = begin
    local name::Absyn.Ident
    local partialPrefix::Bool
    local finalPrefix::Bool
    local encapsulatedPrefix::Bool
    local restriction::Absyn.Restriction
    local typeVars::List{String}
    local classAttrs::List{Absyn.NamedArg}
    local classParts::List{Absyn.ClassPart}
    local annotations::List{Absyn.Annotation}
    local comment::Option{String}
    local info::Info
    @match cl begin
      Absyn.CLASS(
        name,
        partialPrefix,
        finalPrefix,
        encapsulatedPrefix,
        restriction,
        Absyn.PARTS(typeVars, classAttrs, classParts, annotations, comment),
        info,
      ) => begin
        @match (@match _cons(_, _) = classParts) =
          ListUtil.fold(listReverse(classParts), filterNestedClassesParts, nil)
        Absyn.CLASS(
          name,
          partialPrefix,
          finalPrefix,
          encapsulatedPrefix,
          restriction,
          Absyn.PARTS(typeVars, classAttrs, classParts, annotations, comment),
          info,
        )
      end

      _ => begin
        cl
      end
    end
  end
  return o
end

""" #= Helper function for filterNestedClasses =#"""
function filterNestedClassesParts(
  classPart::Absyn.ClassPart,
  inClassParts::List{<:Absyn.ClassPart},
)::List{Absyn.ClassPart}
  local outClassPart::List{Absyn.ClassPart}

  @assign outClassPart = begin
    local classParts::List{Absyn.ClassPart}
    local elts::List{Absyn.ElementItem}
    @match (classPart, inClassParts) begin
      (PUBLIC(elts), classParts) => begin
        @assign classPart.contents = ListUtil.filterOnFalse(elts, isElementItemClass)
        _cons(classPart, classParts)
      end

      (PROTECTED(elts), classParts) => begin
        @assign classPart.contents = ListUtil.filterOnFalse(elts, isElementItemClass)
        _cons(classPart, classParts)
      end

      _ => begin
        _cons(classPart, inClassParts)
      end
    end
  end
  return outClassPart
end

""" #= @author: adrpo
   returns the EXTERNAL form parts if there is any.
   if there is none, it fails! =#"""
function getExternalDecl(inCls::Absyn.Class)::Absyn.ClassPart
  local outExternal::Absyn.ClassPart

  local cp::Absyn.ClassPart
  local class_parts::List{Absyn.ClassPart}

  @match Absyn.CLASS(body = Absyn.PARTS(classParts = class_parts)) = inCls
  @assign outExternal = ListUtil.find(class_parts, isExternalPart)
  return outExternal
end

function isExternalPart(inClassPart::Absyn.ClassPart)::Bool
  local outFound::Bool

  @assign outFound = begin
    @match inClassPart begin
      EXTERNAL(__) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return outFound
end

function isParts(cl::Absyn.ClassDef)::Bool
  local b::Bool

  @assign b = begin
    @match cl begin
      Absyn.PARTS(__) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return b
end

""" #= Makes a class into an Absyn.ElementItem =#"""
function makeClassElement(cl::Absyn.Class)::Absyn.ElementItem
  local el::Absyn.ElementItem

  local info::Info
  local fp::Bool

  @match Absyn.CLASS(finalPrefix = fp, info = info) = cl
  @assign el =
    Absyn.ELEMENTITEM(Absyn.ELEMENT(fp, NONE(), Absyn.NOT_INNER_OUTER(), Absyn.CLASSDEF(false, cl), info, NONE()))
  return el
end

function componentName(c::Absyn.ComponentItem)::String
  local name::String

  @match Absyn.COMPONENTITEM(component = Absyn.COMPONENT(name = name)) = c
  return name
end

function pathSetLastIdent(inPath::Absyn.Path, inLastIdent::Absyn.Path)::Absyn.Path
  local outPath::Absyn.Path

  @assign outPath = begin
    local p::Absyn.Path
    local n::String
    @match (inPath, inLastIdent) begin
      (Absyn.IDENT(__), _) => begin
        inLastIdent
      end

      (Absyn.QUALIFIED(n, p), _) => begin
        @assign p = pathSetLastIdent(p, inLastIdent)
        Absyn.QUALIFIED(n, p)
      end

      (Absyn.FULLYQUALIFIED(p), _) => begin
        @assign p = pathSetLastIdent(p, inLastIdent)
        Absyn.FULLYQUALIFIED(p)
      end
    end
  end
  return outPath
end

""" #= @author:
  returns true if expression contains initial() =#"""
function expContainsInitial(inExp::Absyn.Exp)::Bool
  local hasInitial::Bool

  @assign hasInitial = begin
    local b::Bool
    @matchcontinue inExp begin
      _ => begin
        @assign (_, b) = traverseExp(inExp, isInitialTraverseHelper, false)
        b
      end

      _ => begin
        false
      end
    end
  end
  return hasInitial
end

""" #= @author:
  returns true if expression is initial() =#"""
function isInitialTraverseHelper(inExp::Absyn.Exp, inBool::Bool)::Tuple{Absyn.Exp, Bool}
  local outBool::Bool
  local outExp::Absyn.Exp

  @assign (outExp, outBool) = begin
    local e::Absyn.Exp
    local b::Bool
    #=  make sure we don't have not initial()
    =#
    @match (inExp, inBool) begin
      (Absyn.UNARY(Absyn.NOT(__), _), _) => begin
        (inExp, inBool)
      end

      (e, _) => begin
        @assign b = isInitial(e)
        (e, b)
      end

      _ => begin
        (inExp, inBool)
      end
    end
  end
  #=  we have initial
  =#
  return (outExp, outBool)
end

""" #= @author:
  returns true if expression is initial() =#"""
function isInitial(inExp::Absyn.Exp)::Bool
  local hasReinit::Bool

  @assign hasReinit = begin
    @match inExp begin
      Absyn.CALL(function_ = Absyn.CREF_IDENT("initial", _)) => begin
        true
      end

      Absyn.CALL(function_ = Absyn.CREF_FULLYQUALIFIED(Absyn.CREF_IDENT("initial", _))) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return hasReinit
end

""" #= Return the path of the given import. =#"""
function importPath(inImport::Absyn.Import)::Absyn.Path
  local outPath::Absyn.Path

  @assign outPath = begin
    local path::Absyn.Path
    @match inImport begin
      Absyn.NAMED_IMPORT(path = path) => begin
        path
      end

      Absyn.QUAL_IMPORT(path = path) => begin
        path
      end

      Absyn.UNQUAL_IMPORT(path = path) => begin
        path
      end

      Absyn.GROUP_IMPORT(prefix = path) => begin
        path
      end
    end
  end
  return outPath
end

""" #= Returns the import name of a named or qualified import. =#"""
function importName(inImport::Absyn.Import)::Absyn.Ident
  local outName::Absyn.Ident

  @assign outName = begin
    local name::Absyn.Ident
    local path::Absyn.Path
    #=  Named import has a given name, 'import D = A.B.C' => D.
    =#
    @match inImport begin
      Absyn.NAMED_IMPORT(name = name) => begin
        name
      end

      Absyn.QUAL_IMPORT(path = path) => begin
        pathLastIdent(path)
      end
    end
  end
  #=  Qualified import uses the last identifier, 'import A.B.C' => C.
  =#
  return outName
end

""" #=  This function takes an old annotation as first argument and a new
   annotation as  second argument and merges the two.
   Absyn.Annotation \\\"parts\\\" that exist in both the old and the new annotation
   will be changed according to the new definition. For instance,
   merge_annotations(annotation(x=1,y=2),annotation(x=3))
   => annotation(x=3,y=2) =#"""
function mergeAnnotations(inAnnotation1::Absyn.Annotation, inAnnotation2::Absyn.Annotation)::Absyn.Annotation
  local outAnnotation::Absyn.Annotation

  @assign outAnnotation = begin
    local oldmods::List{Absyn.ElementArg}
    local newmods::List{Absyn.ElementArg}
    local a::Absyn.Annotation
    @match (inAnnotation1, inAnnotation2) begin
      (Absyn.ANNOTATION(elementArgs = nil()), a) => begin
        a
      end

      (Absyn.ANNOTATION(elementArgs = oldmods), Absyn.ANNOTATION(elementArgs = newmods)) => begin
        Absyn.ANNOTATION(mergeAnnotations2(oldmods, newmods))
      end
    end
  end
  return outAnnotation
end

function mergeAnnotations2(
  oldmods::List{<:Absyn.ElementArg},
  newmods::List{<:Absyn.ElementArg},
)::List{Absyn.ElementArg}
  local res::List{Absyn.ElementArg} = listReverse(oldmods)

  local mods::List{Absyn.ElementArg}
  local b::Bool
  local p::Absyn.Path
  local mod1::Absyn.ElementArg
  local mod2::Absyn.ElementArg

  for mod in newmods
    @match Absyn.MODIFICATION(path = p) = mod
    try
      @assign mod2 = ListUtil.find(res, (p) -> isModificationOfPath(path = p))
      @assign mod1 = subModsInSameOrder(mod2, mod)
      @match (res, true) =
        ListUtil.replaceOnTrue(mod1, res, (p) -> isModificationOfPath(path = p))
    catch
      @assign res = _cons(mod, res)
    end
  end
  @assign res = listReverse(res)
  return res
end

""" #= Merges an annotation into a Absyn.Comment option. =#"""
function mergeCommentAnnotation(
  inAnnotation::Absyn.Annotation,
  inComment::Option{<:Absyn.Comment},
)::Option{Absyn.Comment}
  local outComment::Option{Absyn.Comment}

  @assign outComment = begin
    local ann::Absyn.Annotation
    local cmt::Option{String}
    #=  No comment, create a new one.
    =#
    @match inComment begin
      NONE() => begin
        SOME(Absyn.COMMENT(SOME(inAnnotation), NONE()))
      end

      SOME(Absyn.COMMENT(annotation_ = NONE(), comment = cmt)) => begin
        SOME(Absyn.COMMENT(SOME(inAnnotation), cmt))
      end

      SOME(Absyn.COMMENT(annotation_ = SOME(ann), comment = cmt)) => begin
        SOME(Absyn.COMMENT(SOME(mergeAnnotations(ann, inAnnotation)), cmt))
      end
    end
  end
  #=  A comment without annotation, insert the annotation.
  =#
  #=  A comment with annotation, merge the annotations.
  =#
  return outComment
end

""" #= returns true or false if the given path is in the list of modifications =#"""
function isModificationOfPath(mod::Absyn.ElementArg, path::Absyn.Path)::Bool
  local yes::Bool

  @assign yes = begin
    local id1::String
    local id2::String
    @match (mod, path) begin
      (Absyn.MODIFICATION(path = Absyn.IDENT(name = id1)), Absyn.IDENT(name = id2)) => begin
        id1 == id2
      end

      _ => begin
        false
      end
    end
  end
  return yes
end

function subModsInSameOrder(oldmod::Absyn.ElementArg, newmod::Absyn.ElementArg)::Absyn.ElementArg
  local mod::Absyn.ElementArg

  @assign mod = begin
    local args1::List{Absyn.ElementArg}
    local args2::List{Absyn.ElementArg}
    local res::List{Absyn.ElementArg}
    local arg2::Absyn.ElementArg
    local eq1::Absyn.EqMod
    local eq2::Absyn.EqMod
    local p::Absyn.Path
    #=  mod1 or mod2 has no submods
    =#
    @match (oldmod, newmod) begin
      (_, Absyn.MODIFICATION(modification = NONE())) => begin
        newmod
      end

      (Absyn.MODIFICATION(modification = NONE()), _) => begin
        newmod
      end

      (
        Absyn.MODIFICATION(modification = SOME(Absyn.CLASSMOD(args1, _))),
        arg2 && Absyn.MODIFICATION(modification = SOME(Absyn.CLASSMOD(args2, eq2))),
      ) => begin
        #=  mod1
        =#
        #=  Delete all items from args2 that are not in args1
        =#
        @assign res = nil
        for arg1 in args1
          @match Absyn.MODIFICATION(path = p) = arg1
          if ListUtil.exist(args2, (p) -> isModificationOfPath(path = p))
            @assign res = _cons(arg1, res)
          end
        end
        @assign res = listReverse(res)
        #=  Merge the annotations
        =#
        @assign res = mergeAnnotations2(res, args2)
        @assign arg2.modification = SOME(Absyn.CLASSMOD(res, eq2))
        arg2
      end
    end
  end
  return mod
end

function annotationToElementArgs(ann::Absyn.Annotation)::List{Absyn.ElementArg}
  local args::List{Absyn.ElementArg}

  @match Absyn.ANNOTATION(args) = ann
  return args
end

function pathToTypeSpec(inPath::Absyn.Path)::Absyn.TypeSpec
  local outTypeSpec::Absyn.TypeSpec

  @assign outTypeSpec = Absyn.TPATH(inPath, NONE())
  return outTypeSpec
end

function typeSpecString(inTs::Absyn.TypeSpec)::String
  local outStr::String

  @assign outStr = Dump.unparseTypeSpec(inTs)
  return outStr
end

function crefString(inCr::Absyn.ComponentRef)::String
  local outStr::String

  @assign outStr = Dump.printComponentRefStr(inCr)
  return outStr
end

function typeSpecStringNoQualNoDims(inTs::Absyn.TypeSpec)::String
  local outStr::String

  @assign outStr = begin
    local str::Absyn.Ident
    local s::Absyn.Ident
    local str1::Absyn.Ident
    local str2::Absyn.Ident
    local str3::Absyn.Ident
    local path::Absyn.Path
    local adim::Option{List{Absyn.Subscript}}
    local typeSpecLst::List{Absyn.TypeSpec}
    @match inTs begin
      Absyn.TPATH(path = path) => begin
        @assign str = pathString(makeNotFullyQualified(path))
        str
      end

      Absyn.TCOMPLEX(path = path, typeSpecs = typeSpecLst) => begin
        @assign str1 = pathString(makeNotFullyQualified(path))
        @assign str2 = typeSpecStringNoQualNoDimsLst(typeSpecLst)
        @assign str = stringAppendList(list(str1, "<", str2, ">"))
        str
      end
    end
  end
  return outStr
end

function typeSpecStringNoQualNoDimsLst(inTypeSpecLst::List{<:Absyn.TypeSpec})::String
  local outString::String

  @assign outString =
    ListUtil.toString(inTypeSpecLst, typeSpecStringNoQualNoDims, "", "", ", ", "", false)
  return outString
end

function crefStringIgnoreSubs(inCr::Absyn.ComponentRef)::String
  local outStr::String

  local p::Absyn.Path

  @assign p = crefToPathIgnoreSubs(inCr)
  @assign outStr = pathString(makeNotFullyQualified(p))
  return outStr
end

function importString(inImp::Absyn.Import)::String
  local outStr::String

  @assign outStr = Dump.unparseImportStr(inImp)
  return outStr
end

""" #= @author: adrpo
 full Ref -> string
 cref/path full qualified, type dims, subscripts in crefs =#"""
function refString(inRef::Ref)::String
  local outStr::String

  @assign outStr = begin
    local cr::Absyn.ComponentRef
    local ts::Absyn.TypeSpec
    local im::Absyn.Import
    @match inRef begin
      RCR(cr) => begin
        crefString(cr)
      end

      RTS(ts) => begin
        typeSpecString(ts)
      end

      RIM(im) => begin
        importString(im)
      end
    end
  end
  return outStr
end

""" #= @author: adrpo
 brief Ref -> string
 no cref/path full qualified, no type dims, no subscripts in crefs =#"""
function refStringBrief(inRef::Ref)::String
  local outStr::String

  @assign outStr = begin
    local cr::Absyn.ComponentRef
    local ts::Absyn.TypeSpec
    local im::Absyn.Import
    @match inRef begin
      RCR(cr) => begin
        crefStringIgnoreSubs(cr)
      end

      RTS(ts) => begin
        typeSpecStringNoQualNoDims(ts)
      end

      RIM(im) => begin
        importString(im)
      end
    end
  end
  return outStr
end

function getArrayDimOptAsList(inArrayDim::Option{<:Absyn.ArrayDim})::Absyn.ArrayDim
  local outArrayDim::Absyn.ArrayDim

  @assign outArrayDim = begin
    local ad::Absyn.ArrayDim
    @match inArrayDim begin
      SOME(ad) => begin
        ad
      end

      _ => begin
        nil
      end
    end
  end
  return outArrayDim
end

""" #= Removes a variable from a variable list =#"""
function removeCrefFromCrefs(
  inAbsynComponentRefLst::List{<:Absyn.ComponentRef},
  inComponentRef::Absyn.ComponentRef,
)::List{Absyn.ComponentRef}
  local outAbsynComponentRefLst::List{Absyn.ComponentRef}

  @assign outAbsynComponentRefLst = begin
    local n1::String
    local n2::String
    local rest_1::List{Absyn.ComponentRef}
    local rest::List{Absyn.ComponentRef}
    local cr1::Absyn.ComponentRef
    local cr2::Absyn.ComponentRef
    @matchcontinue (inAbsynComponentRefLst, inComponentRef) begin
      (nil(), _) => begin
        nil
      end

      (cr1 <| rest, cr2) => begin
        @match Absyn.CREF_IDENT(name = n1, subscripts = nil) = cr1
        @match Absyn.CREF_IDENT(name = n2, subscripts = nil) = cr2
        @match true = stringEq(n1, n2)
        @assign rest_1 = removeCrefFromCrefs(rest, cr2)
        rest_1
      end

      (cr1 <| rest, cr2) => begin
        @match Absyn.CREF_QUAL(name = n1) = cr1
        @match Absyn.CREF_IDENT(name = n2) = cr2
        @match true = stringEq(n1, n2)
        @assign rest_1 = removeCrefFromCrefs(rest, cr2)
        rest_1
      end

      (cr1 <| rest, cr2) => begin
        @assign rest_1 = removeCrefFromCrefs(rest, cr2)
        _cons(cr1, rest_1)
      end
    end
  end
  #=  If modifier like on comp like: T t(x=t.y) => t.y must be removed
  =#
  return outAbsynComponentRefLst
end

""" #= Retrieve e.g. the documentation annotation as a string from the class passed as argument. =#"""
function getNamedAnnotationInClass(inClass::Absyn.Class, id::Absyn.Path, f::ModFunc)::Option{TypeA}
  local outString::Option{TypeA}

  @assign outString = begin
    local str::TypeA
    local res::TypeA
    local parts::List{Absyn.ClassPart}
    local annlst::List{Absyn.ElementArg}
    local ann::List{Absyn.Annotation}
    @matchcontinue (inClass, id, f) begin
      (Absyn.CLASS(body = Absyn.PARTS(ann = ann)), _, _) => begin
        @assign annlst = ListUtil.flatten(ListUtil.map(ann, annotationToElementArgs))
        @match SOME(str) = getNamedAnnotationStr(annlst, id, f)
        SOME(str)
      end

      (Absyn.CLASS(body = Absyn.CLASS_EXTENDS(ann = ann)), _, _) => begin
        @assign annlst = ListUtil.flatten(ListUtil.map(ann, annotationToElementArgs))
        @match SOME(str) = getNamedAnnotationStr(annlst, id, f)
        SOME(str)
      end

      (Absyn.CLASS(body = DERIVED(comment = SOME(Absyn.COMMENT(SOME(Absyn.ANNOTATION(annlst)), _)))), _, _) => begin
        @match SOME(res) = getNamedAnnotationStr(annlst, id, f)
        SOME(res)
      end

      (
        Absyn.CLASS(body = ENUMERATION(comment = SOME(Absyn.COMMENT(SOME(Absyn.ANNOTATION(annlst)), _)))),
        _,
        _,
      ) => begin
        @match SOME(res) = getNamedAnnotationStr(annlst, id, f)
        SOME(res)
      end

      (
        Absyn.CLASS(body = OVERLOAD(comment = SOME(Absyn.COMMENT(SOME(Absyn.ANNOTATION(annlst)), _)))),
        _,
        _,
      ) => begin
        @match SOME(res) = getNamedAnnotationStr(annlst, id, f)
        SOME(res)
      end

      _ => begin
        NONE()
      end
    end
  end
  return outString
end

""" #= Helper function to getNamedAnnotationInElementitemlist. =#"""
function getNamedAnnotationStr(
  inAbsynElementArgLst::List{<:Absyn.ElementArg},
  id::Absyn.Path,
  f::ModFunc,
)::Option{TypeA}
  local outString::Option{TypeA}

  @assign outString = begin
    local str::TypeA
    local ann::Absyn.ElementArg
    local mod::Option{Absyn.Modification}
    local xs::List{Absyn.ElementArg}
    local id1::Absyn.Ident
    local id2::Absyn.Ident
    local rest::Absyn.Path
    @matchcontinue (inAbsynElementArgLst, id, f) begin
      (Absyn.MODIFICATION(path = Absyn.IDENT(name = id1), modification = mod) <| _, Absyn.IDENT(id2), _) =>
        begin
          @match true = stringEq(id1, id2)
          @assign str = f(mod)
          SOME(str)
        end

      (
        Absyn.MODIFICATION(
          path = Absyn.IDENT(name = id1),
          modification = SOME(Absyn.CLASSMOD(elementArgLst = xs)),
        ) <| _,
        Absyn.QUALIFIED(name = id2, path = rest),
        _,
      ) => begin
        @match true = stringEq(id1, id2)
        getNamedAnnotationStr(xs, rest, f)
      end

      (_ <| xs, _, _) => begin
        getNamedAnnotationStr(xs, id, f)
      end
    end
  end
  return outString
end

""" #= This function splits each part of a cref into CREF_IDENTs and applies the
   given function to each part. If the given cref is a qualified cref then the
   map function is expected to also return Absyn.CREF_IDENT, so that the split cref
   can be reconstructed. Otherwise the map function is free to return whatever
   it wants. =#"""
function mapCrefParts(inCref::Absyn.ComponentRef, inMapFunc::MapFunc)::Absyn.ComponentRef
  local outCref::Absyn.ComponentRef

  @assign outCref = begin
    local name::Absyn.Ident
    local subs::List{Absyn.Subscript}
    local rest_cref::Absyn.ComponentRef
    local cref::Absyn.ComponentRef
    @match (inCref, inMapFunc) begin
      (Absyn.CREF_QUAL(name, subs, rest_cref), _) => begin
        @assign cref = Absyn.CREF_IDENT(name, subs)
        @match Absyn.CREF_IDENT(name, subs) = inMapFunc(cref)
        @assign rest_cref = mapCrefParts(rest_cref, inMapFunc)
        Absyn.CREF_QUAL(name, subs, rest_cref)
      end

      (Absyn.CREF_FULLYQUALIFIED(cref), _) => begin
        @assign cref = mapCrefParts(cref, inMapFunc)
        Absyn.CREF_FULLYQUALIFIED(cref)
      end

      _ => begin
        @assign cref = inMapFunc(inCref)
        cref
      end
    end
  end
  return outCref
end

function opEqual(op1::Absyn.Operator, op2::Absyn.Operator)::Bool
  local isEqual::Bool

  @assign isEqual = valueEq(op1, op2)
  return isEqual
end

function opIsElementWise(op::Absyn.Operator)::Bool
  local isElementWise::Bool

  @assign isElementWise = begin
    @match op begin
      Absyn.ADD_EW(__) => begin
        true
      end

      Absyn.SUB_EW(__) => begin
        true
      end

      Absyn.MUL_EW(__) => begin
        true
      end

      Absyn.DIV_EW(__) => begin
        true
      end

      Absyn.POW_EW(__) => begin
        true
      end

      Absyn.UPLUS_EW(__) => begin
        true
      end

      Absyn.UMINUS_EW(__) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return isElementWise
end

function dummyTraverseExp(inExp::Absyn.Exp, inArg::Arg)::Tuple{Absyn.Exp, Arg}
  local outArg::Arg
  local outExp::Absyn.Exp

  @assign outExp = inExp
  @assign outArg = inArg
  return (outExp, outArg)
end

""" #= retrives defineunit definitions in elements =#"""
function getDefineUnitsInElements(elts::List{<:Absyn.ElementItem})::List{Absyn.Element}
  local outElts::List{Absyn.Element}

  @assign outElts = begin
    local e::Absyn.Element
    local rest::List{Absyn.ElementItem}
    @matchcontinue elts begin
      nil() => begin
        nil
      end

      Absyn.ELEMENTITEM(e && Absyn.DEFINEUNIT(__)) <| rest => begin
        @assign outElts = getDefineUnitsInElements(rest)
        _cons(e, outElts)
      end

      _ <| rest => begin
        getDefineUnitsInElements(rest)
      end
    end
  end
  return outElts
end

""" #= Returns the public and protected elements in a class. =#"""
function getElementItemsInClass(inClass::Absyn.Class)::List{Absyn.ElementItem}
  local outElements::List{Absyn.ElementItem}

  @assign outElements = begin
    local parts::List{Absyn.ClassPart}
    @match inClass begin
      Absyn.CLASS(body = Absyn.PARTS(classParts = parts)) => begin
        ListUtil.mapFlat(parts, getElementItemsInClassPart)
      end

      Absyn.CLASS(body = Absyn.CLASS_EXTENDS(parts = parts)) => begin
        ListUtil.mapFlat(parts, getElementItemsInClassPart)
      end

      _ => begin
        nil
      end
    end
  end
  return outElements
end

""" #= Returns the public and protected elements in a class part. =#"""
function getElementItemsInClassPart(inClassPart::Absyn.ClassPart)::List{Absyn.ElementItem}
  local outElements::List{Absyn.ElementItem}

  @assign outElements = begin
    local elts::List{Absyn.ElementItem}
    @match inClassPart begin
      PUBLIC(contents = elts) => begin
        elts
      end

      PROTECTED(contents = elts) => begin
        elts
      end

      _ => begin
        nil
      end
    end
  end
  return outElements
end

function makePublicClassPartFromElementItems(elementItems::List{<:Absyn.ElementItem})::Absyn.ClassPart
  local classParts::Absyn.ClassPart

  @assign classParts = PUBLIC(elementItems)
  return classParts
end

function makePublicClassPartFromElementItem(ei::Absyn.ElementItem)::Absyn.ClassPart
  local classParts::Absyn.ClassPart

  @assign classParts = PUBLIC(list(ei))
  return classParts
end

function traverseClassComponents(inClass::Absyn.Class, inFunc::FuncType, inArg::ArgT) where {ArgT}
  local outArg::ArgT
  local outClass::Absyn.Class = inClass

  @assign outClass = begin
    local body::Absyn.ClassDef
    @match outClass begin
      Absyn.CLASS(__) => begin
        @assign (body, outArg) = traverseClassDef(
          outClass.body,
          (inFunc) -> traverseClassPartComponents(inFunc = inFunc),
          inArg,
        )
        if !referenceEq(body, outClass.body)
          @assign outClass.body = body
        end
        outClass
      end
    end
  end
  return (outClass, outArg)
end

function traverseListGeneric(inList::List{T}, inFunc::FuncType, inArg::ArgT) where {T, ArgT}
  local outContinue::Bool = true
  local outArg::ArgT = inArg
  local outList::List{T} = nil

  local eq::Bool
  local changed::Bool = false
  local e::T
  local new_e::T
  local rest_e::List{T} = inList

  while !listEmpty(rest_e)
    @match _cons(e, rest_e) = rest_e
    @assign (new_e, outArg, outContinue) = inFunc(e, outArg)
    @assign eq = referenceEq(new_e, e)
    @assign outList = _cons(if eq
      e
    else
      new_e
    end, outList)
    @assign changed = changed || !eq
    if !outContinue
      break
    end
  end
  if changed
    @assign outList = ListUtil.append_reverse(outList, rest_e)
  else
    @assign outList = inList
  end
  return (outList, outArg, outContinue)
end

function traverseClassPartComponents(
  inClassPart::Absyn.ClassPart,
  inFunc::FuncType,
  inArg::ArgT,
) where {ArgT}
  local outContinue::Bool = true
  local outArg::ArgT = inArg
  local outClassPart::Absyn.ClassPart = inClassPart

  @assign _ = begin
    local items::List{Absyn.ElementItem}
    @match outClassPart begin
      PUBLIC(__) => begin
        @assign (items, outArg, outContinue) = traverseListGeneric(
          outClassPart.contents,
          (inFunc) -> traverseElementItemComponents(inFunc = inFunc),
          inArg,
        )
        @assign outClassPart.contents = items
        ()
      end

      PROTECTED(__) => begin
        @assign (items, outArg, outContinue) = traverseListGeneric(
          outClassPart.contents,
          (inFunc) -> traverseElementItemComponents(inFunc = inFunc),
          inArg,
        )
        @assign outClassPart.contents = items
        ()
      end

      _ => begin
        ()
      end
    end
  end
  return (outClassPart, outArg, outContinue)
end

function traverseElementItemComponents(
  inItem::Absyn.ElementItem,
  inFunc::FuncType,
  inArg::ArgT,
) where {ArgT}
  local outContinue::Bool
  local outArg::ArgT
  local outItem::Absyn.ElementItem

  @assign (outItem, outArg, outContinue) = begin
    local elem::Absyn.Element
    @match inItem begin
      Absyn.ELEMENTITEM(__) => begin
        @assign (elem, outArg, outContinue) =
          traverseElementComponents(inItem.element, inFunc, inArg)
        @assign outItem = if referenceEq(elem, inItem.element)
          inItem
        else
          Absyn.ELEMENTITEM(elem)
        end
        (outItem, outArg, outContinue)
      end

      _ => begin
        (inItem, inArg, true)
      end
    end
  end
  return (outItem, outArg, outContinue)
end

function traverseElementComponents(
  inElement::Absyn.Element,
  inFunc::FuncType,
  inArg::ArgT,
) where {ArgT}
  local outContinue::Bool
  local outArg::ArgT
  local outElement::Absyn.Element = inElement

  @assign (outElement, outArg, outContinue) = begin
    local spec::Absyn.ElementSpec
    @match outElement begin
      Absyn.ELEMENT(__) => begin
        @assign (spec, outArg, outContinue) =
          traverseElementSpecComponents(outElement.specification, inFunc, inArg)
        if !referenceEq(spec, outElement.specification)
          @assign outElement.specification = spec
        end
        (outElement, outArg, outContinue)
      end

      _ => begin
        (inElement, inArg, true)
      end
    end
  end
  return (outElement, outArg, outContinue)
end

function traverseElementSpecComponents(
  inSpec::Absyn.ElementSpec,
  inFunc::FuncType,
  inArg::ArgT,
) where {ArgT}
  local outContinue::Bool
  local outArg::ArgT
  local outSpec::Absyn.ElementSpec = inSpec

  @assign (outSpec, outArg, outContinue) = begin
    local cls::Absyn.Class
    local comps::List{Absyn.ComponentItem}
    @match outSpec begin
      Absyn.COMPONENTS(__) => begin
        @assign (comps, outArg, outContinue) = inFunc(outSpec.components, inArg)
        if !referenceEq(comps, outSpec.components)
          @assign outSpec.components = comps
        end
        (outSpec, outArg, outContinue)
      end

      _ => begin
        (inSpec, inArg, true)
      end
    end
  end
  return (outSpec, outArg, outContinue)
end

function traverseClassDef(inClassDef::Absyn.ClassDef, inFunc::FuncType, inArg::ArgT) where {ArgT}
  local outContinue::Bool = true
  local outArg::ArgT = inArg
  local outClassDef::Absyn.ClassDef = inClassDef

  @assign _ = begin
    local parts::List{Absyn.ClassPart}
    @match outClassDef begin
      Absyn.PARTS(__) => begin
        @assign (parts, outArg, outContinue) =
          traverseListGeneric(outClassDef.classParts, inFunc, inArg)
        @assign outClassDef.classParts = parts
        ()
      end

      Absyn.CLASS_EXTENDS(__) => begin
        @assign (parts, outArg, outContinue) =
          traverseListGeneric(outClassDef.parts, inFunc, inArg)
        @assign outClassDef.parts = parts
        ()
      end

      _ => begin
        ()
      end
    end
  end
  return (outClassDef, outArg, outContinue)
end

function isEmptyMod(inMod::Absyn.Modification)::Bool
  local outIsEmpty::Bool

  @assign outIsEmpty = begin
    @match inMod begin
      Absyn.CLASSMOD(nil(), Absyn.NOMOD(__)) => begin
        true
      end

      Absyn.CLASSMOD(nil(), Absyn.EQMOD(exp = Absyn.TUPLE(expressions = nil()))) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return outIsEmpty
end

function isEmptySubMod(inSubMod::Absyn.ElementArg)::Bool
  local outIsEmpty::Bool

  @assign outIsEmpty = begin
    local mod::Absyn.Modification
    @match inSubMod begin
      Absyn.MODIFICATION(modification = NONE()) => begin
        true
      end

      Absyn.MODIFICATION(modification = SOME(mod)) => begin
        isEmptyMod(mod)
      end
    end
  end
  return outIsEmpty
end

function elementArgName(inArg::Absyn.ElementArg)::Absyn.Path
  local outName::Absyn.Path

  @assign outName = begin
    local e::Absyn.ElementSpec
    @match inArg begin
      Absyn.MODIFICATION(path = outName) => begin
        outName
      end

      Absyn.REDECLARATION(elementSpec = e) => begin
        makeIdentPathFromString(elementSpecName(e))
      end
    end
  end
  return outName
end

function elementArgEqualName(inArg1::Absyn.ElementArg, inArg2::Absyn.ElementArg)::Bool
  local outEqual::Bool

  local name1::Absyn.Path
  local name2::Absyn.Path

  @assign outEqual = begin
    @match (inArg1, inArg2) begin
      (Absyn.MODIFICATION(path = name1), Absyn.MODIFICATION(path = name2)) => begin
        pathEqual(name1, name2)
      end

      _ => begin
        false
      end
    end
  end
  return outEqual
end

""" #= Creates a Msg based on a boolean value. =#"""
function optMsg(inShowMessage::Bool, inInfo::SourceInfo)::Msg
  local outMsg::Msg

  @assign outMsg = if inShowMessage
    Absyn.MSG(inInfo)
  else
    Absyn.NO_MSG()
  end
  return outMsg
end

function makeSubscript(inExp::Absyn.Exp)::Absyn.Subscript
  local outSubscript::Absyn.Subscript

  @assign outSubscript = Absyn.SUBSCRIPT(inExp)
  return outSubscript
end

""" #= Splits a cref into parts. =#"""
function crefExplode(
  inCref::Absyn.ComponentRef,
  inAccum::List{<:Absyn.ComponentRef} = nil,
)::List{Absyn.ComponentRef}
  local outCrefParts::List{Absyn.ComponentRef}

  @assign outCrefParts = begin
    @match inCref begin
      Absyn.CREF_QUAL(__) => begin
        crefExplode(inCref.componentRef, _cons(crefFirstCref(inCref), inAccum))
      end

      Absyn.CREF_FULLYQUALIFIED(__) => begin
        crefExplode(inCref.componentRef, inAccum)
      end

      _ => begin
        listReverse(_cons(inCref, inAccum))
      end
    end
  end
  return outCrefParts
end

""" #= Calls the given function on each subexpression (non-recursively) of the given
   expression, sending in the extra argument to each call. =#"""
function traverseExpShallow(inExp::Absyn.Exp, inArg::ArgT, inFunc::FuncT) where {ArgT}
  local outExp::Absyn.Exp = inExp

  @assign _ = begin
    local e1::Absyn.Exp
    local e2::Absyn.Exp
    @match outExp begin
      Absyn.BINARY(__) => begin
        @assign outExp.exp1 = inFunc(outExp.exp1, inArg)
        @assign outExp.exp2 = inFunc(outExp.exp2, inArg)
        ()
      end

      Absyn.UNARY(__) => begin
        @assign outExp.exp = inFunc(outExp.exp, inArg)
        ()
      end

      Absyn.LBINARY(__) => begin
        @assign outExp.exp1 = inFunc(outExp.exp1, inArg)
        @assign outExp.exp2 = inFunc(outExp.exp2, inArg)
        ()
      end

      Absyn.LUNARY(__) => begin
        @assign outExp.exp = inFunc(outExp.exp, inArg)
        ()
      end

      Absyn.RELATION(__) => begin
        @assign outExp.exp1 = inFunc(outExp.exp1, inArg)
        @assign outExp.exp2 = inFunc(outExp.exp2, inArg)
        ()
      end

      Absyn.IFEXP(__) => begin
        @assign outExp.ifExp = inFunc(outExp.ifExp, inArg)
        @assign outExp.trueBranch = inFunc(outExp.trueBranch, inArg)
        @assign outExp.elseBranch = inFunc(outExp.elseBranch, inArg)
        @assign outExp.elseIfBranch = List(
          (inFunc(Util.tuple21(e), inArg), inFunc(Util.tuple22(e), inArg))
          for e in outExp.elseIfBranch
        )
        ()
      end

      Absyn.CALL(__) => begin
        @assign outExp.functionArgs =
          traverseExpShallowFuncArgs(outExp.functionArgs, inArg, inFunc)
        ()
      end

      PARTEVALFUNCTION(__) => begin
        @assign outExp.functionArgs =
          traverseExpShallowFuncArgs(outExp.functionArgs, inArg, inFunc)
        ()
      end

      ARRAY(__) => begin
        @assign outExp.arrayExp = List(inFunc(e, inArg) for e in outExp.arrayExp)
        ()
      end

      MATRIX(__) => begin
        @assign outExp.matrix =
          List(List(inFunc(e, inArg) for e in lst) for lst in outExp.matrix)
        ()
      end

      RANGE(__) => begin
        @assign outExp.start = inFunc(outExp.start, inArg)
        @assign outExp.step = Util.applyOption1(outExp.step, inFunc, inArg)
        @assign outExp.stop = inFunc(outExp.stop, inArg)
        ()
      end

      Absyn.TUPLE(__) => begin
        @assign outExp.expressions = List(inFunc(e, inArg) for e in outExp.expressions)
        ()
      end

      AS(__) => begin
        @assign outExp.exp = inFunc(outExp.exp, inArg)
        ()
      end

      CONS(__) => begin
        @assign outExp.head = inFunc(outExp.head, inArg)
        @assign outExp.rest = inFunc(outExp.rest, inArg)
        ()
      end

      LIST(__) => begin
        @assign outExp.exps = List(inFunc(e, inArg) for e in outExp.exps)
        ()
      end

      DOT(__) => begin
        @assign outExp.exp = inFunc(outExp.exp, inArg)
        @assign outExp.index = inFunc(outExp.index, inArg)
        ()
      end

      _ => begin
        ()
      end
    end
  end
  return outExp
end

function traverseExpShallowFuncArgs(
  inArgs::Absyn.FunctionArgs,
  inArg::ArgT,
  inFunc::FuncT,
) where {ArgT}
  local outArgs::Absyn.FunctionArgs = inArgs

  @assign outArgs = begin
    @match outArgs begin
      Absyn.FUNCTIONARGS(__) => begin
        @assign outArgs.args = List(inFunc(arg, inArg) for arg in outArgs.args)
        outArgs
      end

      Absyn.FOR_ITER_FARG(__) => begin
        @assign outArgs.exp = inFunc(outArgs.exp, inArg)
        @assign outArgs.iterators = List(
          traverseExpShallowIterator(it, inArg, inFunc) for it in outArgs.iterators
        )
        outArgs
      end
    end
  end
  return outArgs
end

function traverseExpShallowIterator(
  inIterator::Absyn.ForIterator,
  inArg::ArgT,
  inFunc::FuncT,
) where {ArgT}
  local outIterator::Absyn.ForIterator

  local name::String
  local guard_exp::Option{Absyn.Exp}
  local range_exp::Option{Absyn.Exp}

  @match Absyn.ITERATOR(name, guard_exp, range_exp) = inIterator
  @assign guard_exp = Util.applyOption1(guard_exp, inFunc, inArg)
  @assign range_exp = Util.applyOption1(range_exp, inFunc, inArg)
  @assign outIterator = Absyn.ITERATOR(name, guard_exp, range_exp)
  return outIterator
end

function isElementItemClass(inElement::Absyn.ElementItem)::Bool
  local outIsClass::Bool

  @assign outIsClass = begin
    @match inElement begin
      Absyn.ELEMENTITEM(element = Absyn.ELEMENT(specification = Absyn.CLASSDEF(__))) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return outIsClass
end

function isElementItem(inElement::Absyn.ElementItem)::Bool
  local outIsClass::Bool

  @assign outIsClass = begin
    @match inElement begin
      Absyn.ELEMENTITEM(__) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return outIsClass
end

function isAlgorithmItem(inAlg::Absyn.AlgorithmItem)::Bool
  local outIsClass::Bool

  @assign outIsClass = begin
    @match inAlg begin
      Absyn.ALGORITHMITEM(__) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return outIsClass
end

function isElementItemClassNamed(inName::String, inElement::Absyn.ElementItem)::Bool
  local outIsNamed::Bool

  @assign outIsNamed = begin
    local name::String
    @match inElement begin
      Absyn.ELEMENTITEM(element = Absyn.ELEMENT(specification = Absyn.CLASSDEF(class_ = Absyn.CLASS(name = name)))) => begin
        name == inName
      end

      _ => begin
        false
      end
    end
  end
  return outIsNamed
end

function isEmptyClassPart(inClassPart::Absyn.ClassPart)::Bool
  local outIsEmpty::Bool

  @assign outIsEmpty = begin
    @match inClassPart begin
      PUBLIC(contents = nil()) => begin
        true
      end

      PROTECTED(contents = nil()) => begin
        true
      end

      CONSTRAINTS(contents = nil()) => begin
        true
      end

      Absyn.EQUATIONS(contents = nil()) => begin
        true
      end

      INITIALEQUATIONS(contents = nil()) => begin
        true
      end

      Absyn.ALGORITHMS(contents = nil()) => begin
        true
      end

      INITIALALGORITHMS(contents = nil()) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return outIsEmpty
end

""" #= For use with traverseExp =#"""
function isInvariantExpNoTraverse(e::Absyn.Exp, b::Bool)::Tuple{Absyn.Exp, Bool}

  if !b
    return (e, b)
  end
  @assign b = begin
    @match e begin
      INTEGER(__) => begin
        true
      end

      REAL(__) => begin
        true
      end

      Absyn.STRING(__) => begin
        true
      end

      BOOL(__) => begin
        true
      end

      Absyn.BINARY(__) => begin
        true
      end

      Absyn.UNARY(__) => begin
        true
      end

      Absyn.LBINARY(__) => begin
        true
      end

      Absyn.LUNARY(__) => begin
        true
      end

      Absyn.RELATION(__) => begin
        true
      end

      Absyn.IFEXP(__) => begin
        true
      end

      Absyn.CALL(function_ = Absyn.CREF_FULLYQUALIFIED(__)) => begin
        true
      end

      PARTEVALFUNCTION(function_ = Absyn.CREF_FULLYQUALIFIED(__)) => begin
        true
      end

      ARRAY(__) => begin
        true
      end

      MATRIX(__) => begin
        true
      end

      RANGE(__) => begin
        true
      end

      CONS(__) => begin
        true
      end

      LIST(__) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  #=  case Absyn.CREF(Absyn.CREF_FULLYQUALIFIED()) then true;
  =#
  return (e, b)
end

""" #= Returns the number of parts a path consists of, e.g. A.B.C gives 3. =#"""
function pathPartCount(path::Absyn.Path, partsAccum::Integer = 0)::Integer
  local parts::Integer

  @assign parts = begin
    @match path begin
      Absyn.Path.Absyn.IDENT(__) => begin
        partsAccum + 1
      end

      Absyn.Path.Absyn.QUALIFIED(__) => begin
        pathPartCount(path.path, partsAccum + 1)
      end

      Absyn.Path.Absyn.FULLYQUALIFIED(__) => begin
        pathPartCount(path.path, partsAccum)
      end
    end
  end
  return parts
end

function getAnnotationsFromConstraintClass(inCC::Option{<:Absyn.ConstrainClass})::List{Absyn.ElementArg}
  local outElArgLst::List{Absyn.ElementArg}

  @assign outElArgLst = begin
    local elementArgs::List{Absyn.ElementArg}
    @match inCC begin
      SOME(Absyn.CONSTRAINCLASS(
        comment = SOME(Absyn.COMMENT(annotation_ = SOME(Absyn.ANNOTATION(elementArgs)))),
      )) => begin
        elementArgs
      end

      _ => begin
        nil
      end
    end
  end
  return outElArgLst
end

function getAnnotationsFromItems(
  inComponentItems::List{<:Absyn.ComponentItem},
  ccAnnotations::List{<:Absyn.ElementArg},
)::List{List{Absyn.ElementArg}}
  local outLst::List{List{Absyn.ElementArg}} = nil

  local annotations::List{Absyn.ElementArg}
  local res::List{String}
  local str::String

  for comp in listReverse(inComponentItems)
    @assign annotations = begin
      @match comp begin
        Absyn.COMPONENTITEM(
          comment = SOME(Absyn.COMMENT(annotation_ = SOME(Absyn.ANNOTATION(annotations)))),
        ) => begin
          listAppend(annotations, ccAnnotations)
        end

        _ => begin
          ccAnnotations
        end
      end
    end
    @assign outLst = _cons(annotations, outLst)
  end
  return outLst
end

""" #=  This function strips out the `graphics\\' modification from an Absyn.ElementArg
   list and return two lists, one with the other modifications and the
   second with the `graphics\\' modification =#"""
function stripGraphicsAndInteractionModification(
  inAbsynElementArgLst::List{<:Absyn.ElementArg},
)::Tuple{List{Absyn.ElementArg}, List{Absyn.ElementArg}}
  local outAbsynElementArgLst2::List{Absyn.ElementArg}
  local outAbsynElementArgLst1::List{Absyn.ElementArg}

  @assign (outAbsynElementArgLst1, outAbsynElementArgLst2) = begin
    local mod::Absyn.ElementArg
    local rest::List{Absyn.ElementArg}
    local l1::List{Absyn.ElementArg}
    local l2::List{Absyn.ElementArg}
    #=  handle empty
    =#
    @matchcontinue inAbsynElementArgLst begin
      nil() => begin
        (nil, nil)
      end

      Absyn.MODIFICATION(path = Absyn.IDENT(name = "interaction")) <| rest => begin
        @assign (l1, l2) = stripGraphicsAndInteractionModification(rest)
        (l1, l2)
      end

      Absyn.MODIFICATION(modification = NONE(), path = Absyn.IDENT(name = "graphics")) <|
      rest => begin
        @assign (l1, l2) = stripGraphicsAndInteractionModification(rest)
        (l1, l2)
      end

      mod &&
        Absyn.MODIFICATION(modification = SOME(_), path = Absyn.IDENT(name = "graphics")) <| rest => begin
        @assign (l1, l2) = stripGraphicsAndInteractionModification(rest)
        (l1, _cons(mod, l2))
      end

      mod && Absyn.MODIFICATION(__) <| rest => begin
        @assign (l1, l2) = stripGraphicsAndInteractionModification(rest)
        (_cons(mod, l1), l2)
      end
    end
  end
  #=  adrpo: remove interaction annotations as we don't handle them currently
  =#
  #=  adrpo: remove empty annotations, to handle bad Dymola annotations, for example: Diagram(graphics)
  =#
  #=  add graphics to the second tuple
  =#
  #=  collect in the first tuple
  =#
  return (outAbsynElementArgLst1, outAbsynElementArgLst2)
end

""" #=  This function traverses all classes of a program and applies a function
   to each class. The function takes the Absyn.Class, Absyn.Path option
   and an additional argument and returns an updated class and the
   additional values. The Absyn.Path option contains the path to the class
   that is traversed.
   inputs:  (Absyn.Program,
               Absyn.Path option,
               ((Absyn.Class  Absyn.Path option  \\'a) => (Absyn.Class  Absyn.Path option  \\'a)),  /* rel-ation to apply */
            \\'a, /* extra value passed to re-lation */
            bool) /* true = traverse protected elements */
   outputs: (Absyn.Program   Absyn.Path option  \\'a) =#"""
function traverseClasses(
  inProgram::Absyn.Program,
  inPath::Option{<:Absyn.Path},
  inFunc::FuncType,
  inArg::Type_a,
  inVisitProtected::Bool,
)::Tuple{Absyn.Program, Option{Absyn.Path}, Type_a}
  local outTpl::Tuple{Absyn.Program, Option{Absyn.Path}, Type_a}

  @assign outTpl = begin
    local classes::List{Absyn.Class}
    local pa_1::Option{Absyn.Path}
    local pa::Option{Absyn.Path}
    local args_1::Type_a
    local args::Type_a
    local within_::Absyn.Within
    local visitor::FuncType
    local traverse_prot::Bool
    local p::Absyn.Program
    @match (inProgram, inPath, inFunc, inArg, inVisitProtected) begin
      (p && Absyn.PROGRAM(__), pa, visitor, args, traverse_prot) => begin
        @assign (classes, pa_1, args_1) =
          traverseClasses2(p.classes, pa, visitor, args, traverse_prot)
        @assign p.classes = classes
        (p, pa_1, args_1)
      end
    end
  end
  return outTpl
end

""" #=  Helperfunction to traverseClasses. =#"""
function traverseClasses2(
  inClasses::List{<:Absyn.Class},
  inPath::Option{<:Absyn.Path},
  inFunc::FuncType,
  inArg::Type_a,
  inVisitProtected::Bool,
)::Tuple{List{Absyn.Class}, Option{Absyn.Path}, Type_a} #= visit protected elements =#
  local outTpl::Tuple{List{Absyn.Class}, Option{Absyn.Path}, Type_a}

  @assign outTpl = begin
    local pa::Option{Absyn.Path}
    local pa_1::Option{Absyn.Path}
    local pa_2::Option{Absyn.Path}
    local pa_3::Option{Absyn.Path}
    local visitor::FuncType
    local args::Type_a
    local args_1::Type_a
    local args_2::Type_a
    local args_3::Type_a
    local class_1::Absyn.Class
    local class_2::Absyn.Class
    local class_::Absyn.Class
    local classes_1::List{Absyn.Class}
    local classes::List{Absyn.Class}
    local traverse_prot::Bool
    @matchcontinue (inClasses, inPath, inFunc, inArg, inVisitProtected) begin
      (nil(), pa, _, args, _) => begin
        (nil, pa, args)
      end

      (class_ <| classes, pa, visitor, args, traverse_prot) => begin
        @assign (class_1, _, args_1) = visitor((class_, pa, args))
        @assign (class_2, _, args_2) =
          traverseInnerClass(class_1, pa, visitor, args_1, traverse_prot)
        @assign (classes_1, pa_3, args_3) =
          traverseClasses2(classes, pa, visitor, args_2, traverse_prot)
        (_cons(class_2, classes_1), pa_3, args_3)
      end

      (class_ <| classes, pa, visitor, args, traverse_prot) => begin
        @assign (class_2, _, args_2) =
          traverseInnerClass(class_, pa, visitor, args, traverse_prot)
        @match true = classHasLocalClasses(class_2)
        @assign (classes_1, pa_3, args_3) =
          traverseClasses2(classes, pa, visitor, args_2, traverse_prot)
        (_cons(class_2, classes_1), pa_3, args_3)
      end

      (_ <| classes, pa, visitor, args, traverse_prot) => begin
        @assign (classes_1, pa_3, args_3) =
          traverseClasses2(classes, pa, visitor, args, traverse_prot)
        (classes_1, pa_3, args_3)
      end

      (class_ <| _, _, _, _, _) => begin
        print("-traverse_classes2 failed on class:")
        print(AbsynUtil.pathString(AbsynUtil.className(class_)))
        print("\\n")
        fail()
      end
    end
  end
  #= /* Visitor failed, but class contains inner classes after traversal, i.e. those inner classes didn't fail, and thus
     the class must be included also */ =#
  #= /* Visitor failed, remove class */ =#
  return outTpl
end

""" #= Returns true if class contains a local class =#"""
function classHasLocalClasses(cl::Absyn.Class)::Bool
  local res::Bool

  @assign res = begin
    local parts::List{Absyn.ClassPart}
    #=  A class with parts.
    =#
    @match cl begin
      Absyn.CLASS(body = Absyn.PARTS(classParts = parts)) => begin
        @assign res = partsHasLocalClass(parts)
        res
      end

      Absyn.CLASS(body = Absyn.CLASS_EXTENDS(parts = parts)) => begin
        @assign res = partsHasLocalClass(parts)
        res
      end
    end
  end
  #=  An extended class with parts: model extends M end M;
  =#
  return res
end

""" #= Help function to classHasLocalClass =#"""
function partsHasLocalClass(inParts::List{<:Absyn.ClassPart})::Bool
  local res::Bool

  @assign res = begin
    local elts::List{Absyn.ElementItem}
    local parts::List{Absyn.ClassPart}
    @match inParts begin
      Absyn.PUBLIC(elts) <| _ => begin
        eltsHasLocalClass(elts)
      end

      Absyn.PROTECTED(elts) <| _ => begin
        eltsHasLocalClass(elts)
      end

      _ <| parts => begin
        partsHasLocalClass(parts)
      end

      _ => begin
        false
      end
    end
  end
  return res
end

""" #= help function to partsHasLocalClass =#"""
function eltsHasLocalClass(inElts::List{<:Absyn.ElementItem})::Bool
  local res::Bool

  @assign res = begin
    local elts::List{Absyn.ElementItem}
    @match inElts begin
      Absyn.ELEMENTITEM(Absyn.ELEMENT(specification = Absyn.CLASSDEF(__))) <| _ => begin
        true
      end

      _ <| elts => begin
        eltsHasLocalClass(elts)
      end

      nil() => begin
        false
      end
    end
  end
  return res
end

""" #= Returns true if class contains a local class with a supplied restriction =#"""
function classHasLocalClassOfType(cl::Absyn.Class, inRestriction::Absyn.Restriction)::Bool
  local res::Bool

  @assign res = begin
    local parts::List{Absyn.ClassPart}
    @matchcontinue cl begin
      Absyn.CLASS(body = Absyn.PARTS(classParts = parts)) => begin
        partsHasLocalClassOfType(parts, inRestriction)
      end

      Absyn.CLASS(body = Absyn.CLASS_EXTENDS(parts = parts)) => begin
        partsHasLocalClassOfType(parts, inRestriction)
      end

      _ => begin
        false
      end
    end
  end
  return res
end

""" #= Help function to classHasLocalClassOfType =#"""
function partsHasLocalClassOfType(
  inParts::List{<:Absyn.ClassPart},
  inRestriction::Absyn.Restriction,
)::Bool
  local res::Bool

  @assign res = begin
    local elts::List{Absyn.ElementItem}
    local parts::List{Absyn.ClassPart}
    @matchcontinue inParts begin
      Absyn.PUBLIC(contents = elts) <| _ => begin
        eltsHasLocalClassOfType(elts, inRestriction)
      end

      Absyn.PROTECTED(contents = elts) <| _ => begin
        eltsHasLocalClassOfType(elts, inRestriction)
      end

      _ <| parts => begin
        partsHasLocalClassOfType(parts, inRestriction)
      end

      nil() => begin
        fail()
      end
    end
  end
  return res
end

""" #= Returns true if there exists a local class which is restricted to the supplied restriction.
Otherwise fails =#"""
function eltsHasLocalClassOfType(
  inElts::List{<:Absyn.ElementItem},
  inRestriction::Absyn.Restriction,
)::Bool
  local res::Bool

  @assign res = begin
    local elts::List{Absyn.ElementItem}
    local restriction::Absyn.Restriction
    @matchcontinue inElts begin
      Absyn.ELEMENTITEM(Absyn.ELEMENT(
        specification = Absyn.CLASSDEF(class_ = Absyn.CLASS(restriction = restriction)),
      )) <| _ => begin
        @assign res = valueEq(restriction, inRestriction)
        if res == false
          fail()
        end
        #= Go to next
        =#
        res
      end

      _ <| elts => begin
        eltsHasLocalClassOfType(elts, inRestriction)
      end

      nil() => begin
        fail()
      end
    end
  end
  return res
end

""" #=  Helperfunction to traverseClasses2. This function traverses all inner classes of a class. =#"""
function traverseInnerClass(
  inClass::Absyn.Class,
  inPath::Option{<:Absyn.Path},
  inFunc::FuncType,
  inArg::Type_a,
  inVisitProtected::Bool,
)::Tuple{Absyn.Class, Option{Absyn.Path}, Type_a} #= if true, traverse protected elts =#
  local outTpl::Tuple{Absyn.Class, Option{Absyn.Path}, Type_a}

  @assign outTpl = begin
    local tmp_pa::Absyn.Path
    local pa::Absyn.Path
    local parts_1::List{Absyn.ClassPart}
    local parts::List{Absyn.ClassPart}
    local pa_1::Option{Absyn.Path}
    local args_1::Type_a
    local args::Type_a
    local name::String
    local bcname::String
    local p::Bool
    local f::Bool
    local e::Bool
    local visit_prot::Bool
    local r::Absyn.Restriction
    local str_opt::Option{String}
    local file_info::SourceInfo
    local visitor::FuncType
    local cl::Absyn.Class
    local modif::List{Absyn.ElementArg}
    local typeVars::List{String}
    local classAttrs::List{Absyn.NamedArg}
    local cmt::Absyn.Comment
    local ann::List{Absyn.Annotation}
    #= /* a class with parts */ =#
    @matchcontinue (inClass, inPath, inFunc, inArg, inVisitProtected) begin
      (
        Absyn.CLASS(
          name,
          p,
          f,
          e,
          r,
          Absyn.PARTS(typeVars, classAttrs, parts, ann, str_opt),
          file_info,
        ),
        SOME(pa),
        visitor,
        args,
        visit_prot,
      ) => begin
        @assign tmp_pa = AbsynUtil.joinPaths(pa, Absyn.IDENT(name))
        @assign (parts_1, pa_1, args_1) =
          traverseInnerClassParts(parts, SOME(tmp_pa), visitor, args, visit_prot)
        (
          Absyn.CLASS(
            name,
            p,
            f,
            e,
            r,
            Absyn.PARTS(typeVars, classAttrs, parts_1, ann, str_opt),
            file_info,
          ),
          pa_1,
          args_1,
        )
      end

      (
        Absyn.CLASS(
          name = name,
          partialPrefix = p,
          finalPrefix = f,
          encapsulatedPrefix = e,
          restriction = r,
          body = Absyn.PARTS(
            typeVars = typeVars,
            classAttrs = classAttrs,
            classParts = parts,
            ann = ann,
            comment = str_opt,
          ),
          info = file_info,
        ),
        NONE(),
        visitor,
        args,
        visit_prot,
      ) => begin
        @assign (parts_1, pa_1, args_1) = traverseInnerClassParts(
          parts,
          SOME(Absyn.IDENT(name)),
          visitor,
          args,
          visit_prot,
        )
        (
          Absyn.CLASS(
            name,
            p,
            f,
            e,
            r,
            Absyn.PARTS(typeVars, classAttrs, parts_1, ann, str_opt),
            file_info,
          ),
          pa_1,
          args_1,
        )
      end

      (
        Absyn.CLASS(
          name = name,
          partialPrefix = p,
          finalPrefix = f,
          encapsulatedPrefix = e,
          restriction = r,
          body = Absyn.PARTS(
            typeVars = typeVars,
            classAttrs = classAttrs,
            classParts = parts,
            ann = ann,
            comment = str_opt,
          ),
          info = file_info,
        ),
        pa_1,
        visitor,
        args,
        visit_prot,
      ) => begin
        @assign (parts_1, pa_1, args_1) =
          traverseInnerClassParts(parts, pa_1, visitor, args, visit_prot)
        (
          Absyn.CLASS(
            name,
            p,
            f,
            e,
            r,
            Absyn.PARTS(typeVars, classAttrs, parts_1, ann, str_opt),
            file_info,
          ),
          pa_1,
          args_1,
        )
      end

      (
        Absyn.CLASS(
          name = name,
          partialPrefix = p,
          finalPrefix = f,
          encapsulatedPrefix = e,
          restriction = r,
          body = Absyn.CLASS_EXTENDS(
            baseClassName = bcname,
            comment = str_opt,
            modifications = modif,
            parts = parts,
            ann = ann,
          ),
          info = file_info,
        ),
        SOME(pa),
        visitor,
        args,
        visit_prot,
      ) => begin
        @assign tmp_pa = AbsynUtil.joinPaths(pa, Absyn.IDENT(name))
        @assign (parts_1, pa_1, args_1) =
          traverseInnerClassParts(parts, SOME(tmp_pa), visitor, args, visit_prot)
        (
          Absyn.CLASS(
            name,
            p,
            f,
            e,
            r,
            Absyn.CLASS_EXTENDS(bcname, modif, str_opt, parts_1, ann),
            file_info,
          ),
          pa_1,
          args_1,
        )
      end

      (
        Absyn.CLASS(
          name = name,
          partialPrefix = p,
          finalPrefix = f,
          encapsulatedPrefix = e,
          restriction = r,
          body = Absyn.CLASS_EXTENDS(
            baseClassName = bcname,
            comment = str_opt,
            modifications = modif,
            parts = parts,
            ann = ann,
          ),
          info = file_info,
        ),
        NONE(),
        visitor,
        args,
        visit_prot,
      ) => begin
        @assign (parts_1, pa_1, args_1) = traverseInnerClassParts(
          parts,
          SOME(Absyn.IDENT(name)),
          visitor,
          args,
          visit_prot,
        )
        (
          Absyn.CLASS(
            name,
            p,
            f,
            e,
            r,
            Absyn.CLASS_EXTENDS(bcname, modif, str_opt, parts_1, ann),
            file_info,
          ),
          pa_1,
          args_1,
        )
      end

      (
        Absyn.CLASS(
          name = name,
          partialPrefix = p,
          finalPrefix = f,
          encapsulatedPrefix = e,
          restriction = r,
          body = Absyn.CLASS_EXTENDS(
            baseClassName = bcname,
            comment = str_opt,
            modifications = modif,
            parts = parts,
            ann = ann,
          ),
          info = file_info,
        ),
        pa_1,
        visitor,
        args,
        visit_prot,
      ) => begin
        @assign (parts_1, pa_1, args_1) =
          traverseInnerClassParts(parts, pa_1, visitor, args, visit_prot)
        (
          Absyn.CLASS(
            name,
            p,
            f,
            e,
            r,
            Absyn.CLASS_EXTENDS(bcname, modif, str_opt, parts_1, ann),
            file_info,
          ),
          pa_1,
          args_1,
        )
      end

      (cl, pa_1, _, args, _) => begin
        (cl, pa_1, args)
      end
    end
  end
  #= /* adrpo: handle also an extended class with parts: model extends M end M; */ =#
  #= /* otherwise */ =#
  return outTpl
end

""" #= Helper function to traverseInnerClass =#"""
function traverseInnerClassParts(
  inClassParts::List{<:Absyn.ClassPart},
  inPath::Option{<:Absyn.Path},
  inFunc::FuncType,
  inArg::Type_a,
  inVisitProtected::Bool,
)::Tuple{List{Absyn.ClassPart}, Option{Absyn.Path}, Type_a} #= visist protected elts =#
  local outTpl::Tuple{List{Absyn.ClassPart}, Option{Absyn.Path}, Type_a}

  @assign outTpl = begin
    local pa::Option{Absyn.Path}
    local pa_1::Option{Absyn.Path}
    local pa_2::Option{Absyn.Path}
    local args::Type_a
    local args_1::Type_a
    local args_2::Type_a
    local elts_1::List{Absyn.ElementItem}
    local elts::List{Absyn.ElementItem}
    local parts_1::List{Absyn.ClassPart}
    local parts::List{Absyn.ClassPart}
    local visitor::FuncType
    local visit_prot::Bool
    local part::Absyn.ClassPart
    @matchcontinue (inClassParts, inPath, inFunc, inArg, inVisitProtected) begin
      (nil(), pa, _, args, _) => begin
        (nil, pa, args)
      end

      (Absyn.PUBLIC(contents = elts) <| parts, pa, visitor, args, visit_prot) => begin
        @assign (elts_1, _, args_1) =
          traverseInnerClassElements(elts, pa, visitor, args, visit_prot)
        @assign (parts_1, pa_2, args_2) =
          traverseInnerClassParts(parts, pa, visitor, args_1, visit_prot)
        (_cons(Absyn.PUBLIC(elts_1), parts_1), pa_2, args_2)
      end

      (Absyn.PROTECTED(contents = elts) <| parts, pa, visitor, args, true) => begin
        @assign (elts_1, _, args_1) =
          traverseInnerClassElements(elts, pa, visitor, args, true)
        @assign (parts_1, pa_2, args_2) =
          traverseInnerClassParts(parts, pa, visitor, args_1, true)
        (_cons(Absyn.PROTECTED(elts_1), parts_1), pa_2, args_2)
      end

      (part <| parts, pa, visitor, args, true) => begin
        @assign (parts_1, pa_1, args_1) =
          traverseInnerClassParts(parts, pa, visitor, args, true)
        (_cons(part, parts_1), pa_1, args_1)
      end
    end
  end
  return outTpl
end

""" #= Helper function to traverseInnerClassParts =#"""
function traverseInnerClassElements(
  inElements::List{<:Absyn.ElementItem},
  inPath::Option{<:Absyn.Path},
  inFuncType::FuncType,
  inArg::Type_a,
  inVisitProtected::Bool,
)::Tuple{List{Absyn.ElementItem}, Option{Absyn.Path}, Type_a} #= visit protected elts =#
  local outTpl::Tuple{List{Absyn.ElementItem}, Option{Absyn.Path}, Type_a}

  @assign outTpl = begin
    local pa::Option{Absyn.Path}
    local pa_1::Option{Absyn.Path}
    local pa_2::Option{Absyn.Path}
    local args::Type_a
    local args_1::Type_a
    local args_2::Type_a
    local elt_spec_1::Absyn.ElementSpec
    local elt_spec::Absyn.ElementSpec
    local elts_1::List{Absyn.ElementItem}
    local elts::List{Absyn.ElementItem}
    local f::Bool
    local visit_prot::Bool
    local r::Option{Absyn.RedeclareKeywords}
    local io::Absyn.InnerOuter
    local info::SourceInfo
    local constr::Option{Absyn.ConstrainClass}
    local visitor::FuncType
    local elt::Absyn.ElementItem
    local repl::Bool
    local cl::Absyn.Class
    @matchcontinue (inElements, inPath, inFuncType, inArg, inVisitProtected) begin
      (nil(), pa, _, args, _) => begin
        (nil, pa, args)
      end

      (
        Absyn.ELEMENTITEM(
          element = Absyn.ELEMENT(
            finalPrefix = f,
            redeclareKeywords = r,
            innerOuter = io,
            specification = elt_spec,
            info = info,
            constrainClass = constr,
          ),
        ) <| elts,
        pa,
        visitor,
        args,
        visit_prot,
      ) => begin
        @assign (elt_spec_1, _, args_1) =
          traverseInnerClassElementspec(elt_spec, pa, visitor, args, visit_prot)
        @assign (elts_1, pa_2, args_2) =
          traverseInnerClassElements(elts, pa, visitor, args_1, visit_prot)
        (
          _cons(
            Absyn.ELEMENTITEM(Absyn.ELEMENT(f, r, io, elt_spec_1, info, constr)),
            elts_1,
          ),
          pa_2,
          args_2,
        )
      end

      (
        Absyn.ELEMENTITEM(
          element = Absyn.ELEMENT(
            finalPrefix = f,
            redeclareKeywords = r,
            innerOuter = io,
            specification = Absyn.CLASSDEF(repl, cl),
            info = info,
            constrainClass = constr,
          ),
        ) <| elts,
        pa,
        visitor,
        args,
        visit_prot,
      ) => begin
        @assign (cl, _, args_1) = traverseInnerClass(cl, pa, visitor, args, visit_prot)
        @match true = classHasLocalClasses(cl)
        @assign (elts_1, pa_2, args_2) =
          traverseInnerClassElements(elts, pa, visitor, args_1, visit_prot)
        (
          _cons(
            Absyn.ELEMENTITEM(Absyn.ELEMENT(
              f,
              r,
              io,
              Absyn.CLASSDEF(repl, cl),
              info,
              constr,
            )),
            elts_1,
          ),
          pa_2,
          args_2,
        )
      end

      (
        Absyn.ELEMENTITEM(element = Absyn.ELEMENT(__)) <| elts,
        pa,
        visitor,
        args,
        visit_prot,
      ) => begin
        @assign (elts_1, pa_2, args_2) =
          traverseInnerClassElements(elts, pa, visitor, args, visit_prot)
        (elts_1, pa_2, args_2)
      end

      (elt <| elts, pa, visitor, args, visit_prot) => begin
        @assign (elts_1, pa_1, args_1) =
          traverseInnerClassElements(elts, pa, visitor, args, visit_prot)
        (_cons(elt, elts_1), pa_1, args_1)
      end
    end
  end
  #= /* Visitor failed in elementspec, but inner classes succeeded, include class */ =#
  #= /* Visitor failed in elementspec, remove class */ =#
  return outTpl
end

""" #=  Helperfunction to traverseInnerClassElements =#"""
function traverseInnerClassElementspec(
  inElementSpec::Absyn.ElementSpec,
  inPath::Option{<:Absyn.Path},
  inFuncType::FuncType,
  inArg::Type_a,
  inVisitProtected::Bool,
)::Tuple{Absyn.ElementSpec, Option{Absyn.Path}, Type_a} #= visit protected elts =#
  local outTpl::Tuple{Absyn.ElementSpec, Option{Absyn.Path}, Type_a}

  @assign outTpl = begin
    local class_1::Absyn.Class
    local class_2::Absyn.Class
    local class_::Absyn.Class
    local pa_1::Option{Absyn.Path}
    local pa_2::Option{Absyn.Path}
    local pa::Option{Absyn.Path}
    local args_1::Type_a
    local args_2::Type_a
    local args::Type_a
    local repl::Bool
    local visit_prot::Bool
    local visitor::FuncType
    local elt_spec::Absyn.ElementSpec
    @match (inElementSpec, inPath, inFuncType, inArg, inVisitProtected) begin
      (Absyn.CLASSDEF(replaceable_ = repl, class_ = class_), pa, visitor, args, visit_prot) => begin
        @assign (class_1, _, args_1) = visitor((class_, pa, args))
        @assign (class_2, pa_2, args_2) =
          traverseInnerClass(class_1, pa, visitor, args_1, visit_prot)
        (Absyn.CLASSDEF(repl, class_2), pa_2, args_2)
      end

      (elt_spec && Absyn.EXTENDS(__), pa, _, args, _) => begin
        (elt_spec, pa, args)
      end

      (elt_spec && Absyn.IMPORT(__), pa, _, args, _) => begin
        (elt_spec, pa, args)
      end

      (elt_spec && Absyn.COMPONENTS(__), pa, _, args, _) => begin
        (elt_spec, pa, args)
      end
    end
  end
  return outTpl
end

""" #= @auhtor: johti
 Get the typespec path in an Absyn.ElementItem if it has one =#"""
function getTypeSpecFromElementItemOpt(
  inElementItem::Absyn.ElementItem,
)::Option{Absyn.TypeSpec}
  local outTypeSpec::Option{Absyn.TypeSpec}

  @assign outTypeSpec = begin
    local typeSpec::Absyn.TypeSpec
    local specification::Absyn.ElementSpec
    @matchcontinue inElementItem begin
      Absyn.ELEMENTITEM(__) => begin
        begin
          @match inElementItem.element begin
            Absyn.ELEMENT(specification = specification) => begin
              begin
                @match specification begin
                  Absyn.COMPONENTS(typeSpec = typeSpec) => begin
                    SOME(typeSpec)
                  end
                end
              end
            end
          end
        end
      end

      _ => begin
        NONE()
      end
    end
  end
  return outTypeSpec
end

""" #= @auhtor: johti
     Get a Absyn.ComponentItem from an Absyn.ElementItem if it has one =#"""
function getElementSpecificationFromElementItemOpt(
  inElementItem::Absyn.ElementItem,
)::Option{Absyn.ElementSpec}
  local outSpec::Option{Absyn.ElementSpec}

  @assign outSpec = begin
    local specification::Absyn.ElementSpec
    local element::Absyn.Element
    @matchcontinue inElementItem begin
      Absyn.ELEMENTITEM(element = element) => begin
        begin
          @match element begin
            Absyn.ELEMENT(specification = specification) => begin
              SOME(specification)
            end
          end
        end
      end

      _ => begin
        NONE()
      end
    end
  end
  return outSpec
end

""" #= @auhtor: johti
 Get the componentItems from a given elemSpec otherwise returns an empty list =#"""
function getComponentItemsFromElementSpec(
  elemSpec::Absyn.ElementSpec,
)::List{Absyn.ComponentItem}
  local componentItems::List{Absyn.ComponentItem}

  @assign componentItems = begin
    local components::List{Absyn.ComponentItem}
    local importTmp::Absyn.Import
    @match elemSpec begin
      Absyn.COMPONENTS(components = components) => begin
        components
      end

      _ => begin
        nil
      end
    end
  end
  #= TODO do not fail in silence
  =#
  return componentItems
end

""" #= @author: johti
 Get the componentItems from a given elementItem =#"""
function getComponentItemsFromElementItem(
  inElementItem::Absyn.ElementItem,
)::List{Absyn.ComponentItem}
  local componentItems::List{Absyn.ComponentItem}

  @assign componentItems = begin
    local elementSpec::Absyn.ElementSpec
    @match getElementSpecificationFromElementItemOpt(inElementItem) begin
      SOME(elementSpec) => begin
        getComponentItemsFromElementSpec(elementSpec)
      end

      _ => begin
        nil
      end
    end
  end
  #= TODO do not fail in silence
  =#
  return componentItems
end

""" #= @author johti
  Get the direction if one exists otherwise returns Absyn.BIDIR() =#"""
function getDirection(elementItem::Absyn.ElementItem)::Absyn.Direction
  local oDirection::Absyn.Direction

  @assign oDirection = begin
    local element::Absyn.Element
    @matchcontinue elementItem begin
      Absyn.ELEMENTITEM(element = element) => begin
        begin
          local specification::Absyn.ElementSpec
          @match element begin
            Absyn.ELEMENT(specification = specification) => begin
              begin
                local attributes::ElementAttributes
                @match specification begin
                  Absyn.COMPONENTS(attributes = attributes) => begin
                    begin
                      local direction::Absyn.Direction
                      @match attributes begin
                        Absyn.ATTR(direction = direction) => begin
                          direction
                        end
                      end
                    end
                  end
                end
              end
            end
          end
        end
      end

      _ => begin
        Absyn.BIDIR()
      end
    end
  end
  return oDirection
end

function isNamedPathIdent(path::Absyn.Path, name::String)::Bool
  local res::Bool

  @assign res = begin
    @match path begin
      Absyn.IDENT(__) => begin
        path.name == name
      end

      _ => begin
        false
      end
    end
  end
  return res
end

""" #= @author johti17: Returns true if the class restriction is uniontype =#"""
function isUniontype(cls::Absyn.Class)::Bool
  local b::Bool

  @assign b = begin
    @match cls.restriction begin
      Absyn.R_UNIONTYPE(__) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return b
end

""" #= Returns true if the class restriction is a record =#"""
function classRestrictionisRecord(cls::Absyn.Class)::Bool
  local restrictionIsRecord::Bool = false

  @assign restrictionIsRecord = begin
    @match cls begin
      Absyn.CLASS(__) => begin
        restrictionIsRecord(cls.restriction)
      end

      _ => begin
        false
      end
    end
  end
  return restrictionIsRecord
end

""" #= Returns true if the class restriction is not a record =#"""
function classRestrictionisNotRecord(cls::Absyn.Class)::Bool
  local restrictionIsNotRecord::Bool = false

  @assign restrictionIsNotRecord = !classRestrictionisRecord(cls)
  return restrictionIsNotRecord
end

""" #= @Author johti17: Returns true if the given restriction is a record =#"""
function restrictionIsRecord(restriction::Absyn.Restriction)::Bool
  local isRecord::Bool

  @assign isRecord = begin
    @match restriction begin
      Absyn.R_RECORD(__) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return isRecord
end

""" #= @Author johti17:
    Returns all first level parts in a given class which are not records. =#"""
function splitRecordsAndOtherElements(
  cls::Absyn.Class,
)::Tuple{List{Absyn.ClassPart}, List{Absyn.ClassPart}}
  local nonRecordParts::List{Absyn.ClassPart} = nil
  local recordParts::List{Absyn.ClassPart} = nil

  local elementItems::List{Absyn.ElementItem} = nil
  local nonClassDefs::List{Absyn.ElementItem}
  local elementSpecs::List{Absyn.ElementSpec} = nil
  local classes::List{Absyn.Class} = nil

  @assign elementItems = getElementItemsInClass(cls)
  @assign elementSpecs = Util.listOfOptionToList(ListUtil.map(
    elementItems,
    getElementSpecificationFromElementItemOpt,
  ))
  @assign classes =
    Util.listOfOptionToList(ListUtil.map(elementSpecs, getClassFromElementSpecOpt))
  #= /*Filter out the remaining components to be consed to nonRecordParts*/ =#
  @assign nonClassDefs = ListUtil.map(
    ListUtil.filterOnTrue(elementSpecs, elementSpecsIscomponentsOrImports),
    makeElementItemFromElementSpec,
  )
  @assign (recordParts, nonRecordParts) = (
    list(makePublicClassPartFromElementItems(ListUtil.map(
      ListUtil.filterOnTrue(classes, classRestrictionisRecord),
      makeClassElement,
    ))),
    list(makePublicClassPartFromElementItems(ListUtil.map(
      ListUtil.filterOnTrue(classes, classRestrictionisNotRecord),
      makeClassElement,
    ))),
  )
  #= /* Place element items first */ =#
  @assign nonRecordParts =
    _cons(makePublicClassPartFromElementItems(nonClassDefs), nonRecordParts)
  return (recordParts, nonRecordParts)
end

function makeElementItemFromElementSpec(spec::Absyn.ElementSpec)::Absyn.ElementItem
  local ei::Absyn.ElementItem

  @assign ei = Absyn.ELEMENTITEM(Absyn.ELEMENT(
    false,
    NONE(),
    Absyn.NOT_INNER_OUTER(),
    spec,
    dummyInfo,
    NONE(),
  ))
  return ei
end

""" #= Returns the class from an element spec iff the elementspec is a Absyn.CLASSDEF.
 Otherwise returns NONE. =#"""
function getClassFromElementSpecOpt(es::Absyn.ElementSpec)::Option{Absyn.Class}
  local outClass::Option{Absyn.Class}

  @assign outClass = begin
    @match es begin
      Absyn.CLASSDEF(__) => begin
        SOME(es.class_)
      end

      _ => begin
        NONE()
      end
    end
  end
  return outClass
end

""" #= @Author johti17:
Returns true if those local classes are functions.
N.B!: Only checks the first level of nesting. =#"""
function classHasLocalClassesThatAreFunctions(cls::Absyn.Class)::Bool
  local b::Bool

  local function_restriction::Absyn.Restriction =
    Absyn.R_FUNCTION(Absyn.FR_NORMAL_FUNCTION(Absyn.NO_PURITY()))

  @assign b =
    classHasLocalClassOfType(cls, function_restriction) ||
    classHasLocalClassOfType(
      cls,
      Absyn.R_FUNCTION(Absyn.FR_NORMAL_FUNCTION(Absyn.PURE())),
    ) ||
    classHasLocalClassOfType(
      cls,
      Absyn.R_FUNCTION(Absyn.FR_NORMAL_FUNCTION(Absyn.IMPURE())),
    ) ||
    classHasLocalClassOfType(cls, Absyn.R_FUNCTION(FR_OPERATOR_FUNCTION())) ||
    classHasLocalClassOfType(cls, Absyn.R_FUNCTION(FR_KERNEL_FUNCTION())) ||
    classHasLocalClassOfType(cls, Absyn.R_FUNCTION(FR_PARALLEL_FUNCTION()))
  return b
end

""" #= @Author johti17:
 Returns true if any of those local classes are a uniontype
N.B!: Only checks the first level of nesting. =#"""
function classHasLocalClassesThatAreUniontypes(cls::Absyn.Class)::Bool
  local b::Bool

  local uniontype_restriction::Absyn.Restriction = Absyn.R_UNIONTYPE()

  @assign b = classHasLocalClassOfType(cls, uniontype_restriction)
  return b
end

function elementSpecsIscomponentsOrImports(es::Absyn.ElementSpec)::Bool
  local b::Bool

  @assign b = elementSpecIsImport(es) || elementSpecIsComponents(es)
  return b
end

function elementSpecIsImport(es::Absyn.ElementSpec)::Bool
  local isImport::Bool

  @assign isImport = begin
    @match es begin
      Absyn.IMPORT(__) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return isImport
end

function elementSpecIsComponents(es::Absyn.ElementSpec)::Bool
  local isComponents::Bool

  @assign isComponents = begin
    @match es begin
      Absyn.COMPONENTS(__) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return isComponents
end

@exportAll()
end
