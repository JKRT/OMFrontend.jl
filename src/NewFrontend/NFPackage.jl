module NFPackage

using MetaModelica
using ExportAll

#= /*
* This file is part of OpenModelica.
*
* Copyright (c) 1998-2014, Open Source Modelica Consortium (OSMC),
* c/o Linköpings universitet, Department of Computer and Information Science,
* SE-58183 Linköping, Sweden.
*
* All rights reserved.
*
* THIS PROGRAM IS PROVIDED UNDER THE TERMS OF GPL VERSION 3 LICENSE OR
* THIS OSMC PUBLIC LICENSE (OSMC-PL) VERSION 1.2.
* ANY USE, REPRODUCTION OR DISTRIBUTION OF THIS PROGRAM CONSTITUTES
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
import ..NFFlatModel
FlatModel = NFFlatModel
import ..NFFlatten.FunctionTree

import ..ExecStat.execStat
import ..P_NFComponentRef
P_ComponentRef = P_NFComponentRef
ComponentRef = P_NFComponentRef.NFComponentRef
import ..P_NFExpression
P_Expression = P_NFExpression
Expression = P_NFExpression.NFExpression
import ..NFBinding.P_Binding
import ..P_NFEquation
P_Equation = P_NFEquation
Equation = P_NFEquation.NFEquation
import ..P_NFStatement
P_Statement = P_NFStatement
Statement = P_NFStatement.NFStatement
import ListUtil
import ..NFComponent.P_Component
import ..NFInstNode.P_InstNode
import ..NFTyping
Typing = NFTyping
import ..NFCeval
Ceval = NFCeval
import ..NFFunction.P_Function
import ..NFClass.P_Class
import ..P_NFSections
P_Sections = P_NFSections
Sections = P_NFSections.NFSections
import ..NFClassTree
= NFClassTree
ClassTree = NFClassTree.ClassTree
import ..NFTyping.ExpOrigin
import ..P_NFVariable
P_Variable = P_NFVariable
Variable = P_NFVariable.NFVariable
import ..P_NFAlgorithm
P_Algorithm = P_NFAlgorithm
Algorithm = P_NFAlgorithm.NFAlgorithm

Constants = ConstantsSetImpl.Tree

module ConstantsSetImpl

using MetaModelica
using ExportAll

import ..BaseAvlSet
import ..P_NFComponentRef
P_ComponentRef = P_NFComponentRef
ComponentRef = P_NFComponentRef.NFComponentRef
using BaseAvlSet #= Modelica extend clause =#

Key = ComponentRef

@exportAll()
end

function collectConstants(flatModel::FlatModel, functions::FunctionTree)::FlatModel

  local vars::List{Variable} = nil
  local binding::Binding
  local constants::Constants

  @assign constants = Constants.new()
  @assign constants =
    ListUtil.fold(flatModel.variables, collectVariableConstants, constants)
  @assign constants =
    P_Equation.Equation.foldExpList(flatModel.equations, collectExpConstants, constants)
  @assign constants = P_Equation.Equation.foldExpList(
    flatModel.initialEquations,
    collectExpConstants,
    constants,
  )
  @assign constants =
    P_Algorithm.Algorithm.foldExpList(flatModel.algorithms, collectExpConstants, constants)
  @assign constants = P_Algorithm.Algorithm.foldExpList(
    flatModel.initialAlgorithms,
    collectExpConstants,
    constants,
  )
  @assign constants = FunctionTree.fold(functions, collectFuncConstants, constants)
  @assign vars =
    listReverse(P_Variable.Variable.fromCref(c) for c in Constants.listKeys(constants))
  @assign flatModel.variables = listAppend(vars, flatModel.variables)
  execStat(getInstanceName())
  return flatModel
end

function replaceConstants(
  flatModel::FlatModel,
  functions::FunctionTree,
)::Tuple{FlatModel, FunctionTree}

  @assign flatModel.variables =
    List(replaceVariableConstants(c) for c in flatModel.variables)
  @assign flatModel.equations =
    P_Equation.Equation.mapExpList(flatModel.equations, replaceExpConstants)
  @assign flatModel.initialEquations =
    P_Equation.Equation.mapExpList(flatModel.initialEquations, replaceExpConstants)
  @assign flatModel.algorithms =
    P_Algorithm.Algorithm.mapExpList(flatModel.algorithms, replaceExpConstants)
  @assign flatModel.initialAlgorithms =
    P_Algorithm.Algorithm.mapExpList(flatModel.initialAlgorithms, replaceExpConstants)
  @assign functions = FunctionTree.map(functions, replaceFuncConstants)
  execStat(getInstanceName())
  return (flatModel, functions)
end

function collectVariableConstants(var::Variable, constants::Constants)::Constants

  @assign constants = collectBindingConstants(var.binding, constants)
  #=  TODO: The component's attributes (i.e. start, etc) might also contain
  =#
  #=        package constants.
  =#
  return constants
end

function collectBindingConstants(binding::Binding, constants::Constants)::Constants

  if isExplicitlyBound(binding)
    @assign constants = collectExpConstants(getTypedExp(binding), constants)
  end
  return constants
end

function collectExpConstants(exp::Expression, constants::Constants)::Constants

  @assign constants =
    P_Expression.Expression.fold(exp, collectExpConstants_traverser, constants)
  return constants
end

function collectExpConstants_traverser(exp::Expression, constants::Constants)::Constants

  local cref::ComponentRef

  @assign () = begin
    @match exp begin
      CREF_EXPRESSION(cref = cref && CREF(__)) =>
        begin
          if isPackageConstant(cref)
            Typing.typeComponentBinding(cref.node, ExpOrigin.CLASS)
            @assign constants = Constants.add(
              constants,
              stripSubscriptsAll(cref),
            )
            @assign constants = collectBindingConstants(
              P_Component.getBinding(component(node(
                cref,
              ))),
              constants,
            )
          end
          #=  Add the constant to the set.
          =#
          #=  Collect constants from the constant's binding.
          =#
          ()
        end

      _ => begin
        ()
      end
    end
  end
  return constants
end

function collectFuncConstants(
  name::Absyn.Path,
  func::M_Function,
  constants::Constants,
)::Constants

  local cls::Class
  local comps::Array{InstNode}
  local sections::Sections

  @assign cls = getClass(func.node)
  @assign () = begin
    @match cls begin
      INSTANCED_CLASS(
        elements = FLAT_TREE(components = comps),
        sections = sections,
      ) => begin
        for c in comps
          @assign constants = collectBindingConstants(
            P_Component.getBinding(component(c)),
            constants,
          )
        end
        @assign () = begin
          @match sections begin
            P_Sections.Sections.SECTIONS(__) => begin
              @assign constants = P_Algorithm.Algorithm.foldExpList(
                sections.algorithms,
                collectExpConstants,
                constants,
              )
              ()
            end

            P_Sections.Sections.EXTERNAL(__) => begin
              for arg in sections.args
                @assign constants = collectExpConstants(arg, constants)
              end
              ()
            end

            _ => begin
              ()
            end
          end
        end
        ()
      end

      _ => begin
        ()
      end
    end
  end
  return constants
end

function replaceVariableConstants(var::Variable)::Variable

  local cref::ComponentRef
  local binding::Binding

  @assign binding = replaceBindingConstants(var.binding)
  if !referenceEq(binding, var.binding)
    @assign var.binding = binding
  end
  return var
end

function replaceBindingConstants(binding::Binding)::Binding

  @assign () = begin
    @match binding begin
      TYPED_BINDING(__) => begin
        @assign binding.bindingExp = replaceExpConstants(binding.bindingExp)
        ()
      end

      _ => begin
        ()
      end
    end
  end
  return binding
end

function replaceExpConstants(exp::Expression)::Expression

  @assign exp = P_Expression.Expression.map(exp, replaceExpConstants_traverser)
  return exp
end

function replaceExpConstants_traverser(exp::Expression)::Expression

  local cref::ComponentRef

  @assign exp = begin
    @match exp begin
      CREF_EXPRESSION(cref = cref && CREF(__)) =>
        begin
          if isPackageConstant(cref)
            Ceval.evalExp(exp, Ceval.P_EvalTarget.IGNORE_ERRORS())
          else
            exp
          end
        end

      _ => begin
        exp
      end
    end
  end
  return exp
end

function replaceFuncConstants(name::Absyn.Path, func::M_Function)::M_Function

  local cls::Class
  local comps::Array{InstNode}
  local sections::Sections
  local comp::Component
  local binding::Binding
  local eval_binding::Binding

  @assign cls = getClass(func.node)
  @assign () = begin
    @match cls begin
      INSTANCED_CLASS(
        elements = FLAT_TREE(components = comps),
        sections = sections,
      ) => begin
        for c in comps
          @assign comp = component(c)
          @assign binding = P_Component.getBinding(comp)
          @assign eval_binding = replaceBindingConstants(binding)
          if !referenceEq(binding, eval_binding)
            @assign comp = P_Component.setBinding(eval_binding, comp)
            updateComponent(comp, c)
          end
        end
        @assign () = begin
          @match sections begin
            P_Sections.Sections.SECTIONS(__) => begin
              @assign sections.algorithms = List(
                P_Algorithm.Algorithm.mapExp(a, replaceExpConstants)
                for a in sections.algorithms
              )
              @assign cls.sections = sections
              updateClass(cls, func.node)
              ()
            end

            P_Sections.Sections.EXTERNAL(__) => begin
              @assign sections.args =
                List(replaceExpConstants(arg) for arg in sections.args)
              @assign cls.sections = sections
              updateClass(cls, func.node)
              ()
            end

            _ => begin
              ()
            end
          end
        end
        ()
      end
    end
  end
  return func
end

@exportAll()
end
