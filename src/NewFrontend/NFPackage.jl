
module ConstantsSetImpl
import ..NFComponentRef
using MetaModelica
using ExportAll
ComponentRef = NFComponentRef
#= Modelica extend clause =#
const Key = ComponentRef
const Value = Int
include("../Util/baseAvlTreeCode.jl")
@exportAll()
end

Constants = ConstantsSetImpl.Tree

function collectConstants(flatModel::FlatModel, functions::FunctionTree)::FlatModel

  local vars::List{Variable} = nil
  local binding::Binding
  local constants::Constants

  @assign constants = ConstantsSetImpl.new()
  @assign constants =
    ListUtil.fold(flatModel.variables, collectVariableConstants, constants)
  @assign constants =
    foldExpList(flatModel.equations, collectExpConstants, constants)
  @assign constants = foldExpList(
    flatModel.initialEquations,
    collectExpConstants,
    constants,
  )
  @assign constants =
    foldExpList(flatModel.algorithms, collectExpConstants, constants)
  @assign constants = foldExpList(
    flatModel.initialAlgorithms,
    collectExpConstants,
    constants,
  )
  @assign constants = FunctionTreeImpl.fold(functions, collectFuncConstants, constants)
  @assign vars =
    listReverse(list(Variable_fromCref(c) for c in ConstantsSetImpl.listKeys(constants)))
  @assign flatModel.variables = listAppend(vars, flatModel.variables)
#  execStat(getInstanceName()) TODO
  return flatModel
end

function replaceConstants(
  flatModel::FlatModel,
  functions::FunctionTree,
)::Tuple{FlatModel, FunctionTree}

  @assign flatModel.variables =
    list(replaceVariableConstants(c) for c in flatModel.variables)
  @assign flatModel.equations =
    mapExpList(flatModel.equations, replaceExpConstants)
  @assign flatModel.initialEquations =
    mapExpList(flatModel.initialEquations, replaceExpConstants)
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
    fold(exp, collectExpConstants_traverser, constants)
  return constants
end

function collectExpConstants_traverser(@nospecialize(exp::Expression), @nospecialize(constants::Constants))::Constants
  local cref::ComponentRef
  @assign () = begin
    @match exp begin
      CREF_EXPRESSION(cref = cref && CREF_EXPRESSION(__)) =>
        begin
          if isPackageConstant(cref)
            typeComponentBinding(cref.node, ORIGIN_CLASS)
            @assign constants = Constants.add(
              constants,
              stripSubscriptsAll(cref),
            )
            @assign constants = collectBindingConstants(
              getBinding(component(node(
                cref,
              ))),
              constants,
            )
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

function collectFuncConstants(
  name::Absyn.Path,
  func::M_Function,
  constants::Constants,
)::Constants

  local cls::Class
  local comps::Vector{InstNode}
  local sections::Sections

  @assign cls = getClass(func.node)
  @assign () = begin
    @match cls begin
      INSTANCED_CLASS(
        elements = CLASS_TREE_FLAT_TREE(components = comps),
        sections = sections,
      ) => begin
        for c in comps
          @assign constants = collectBindingConstants(
            getBinding(component(c)),
            constants,
          )
        end
        @assign () = begin
          @match sections begin
            P_Sections.Sections.SECTIONS(__) => begin
              @assign constants = foldExpList(
                sections.algorithms,
                collectExpConstants,
                constants,
              )
              ()
            end

            SECTIONS_EXTERNAL(__) => begin
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

  @assign exp = map(exp, replaceExpConstants_traverser)
  return exp
end

function replaceExpConstants_traverser(exp::Expression)::Expression

  local cref::ComponentRef

  @assign exp = begin
    @match exp begin
      CREF_EXPRESSION(cref = cref && CREF(__)) =>
        begin
          if isPackageConstant(cref)
            Ceval.evalExp(exp, Ceval.EVALTARGET_IGNORE_ERRORS())
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
  local comps::Vector{InstNode}
  local sections::Sections
  local comp::Component
  local binding::Binding
  local eval_binding::Binding

  @assign cls = getClass(func.node)
  @assign () = begin
    @match cls begin
      INSTANCED_CLASS(
        elements = CLASS_TREE_FLAT_TREE(components = comps),
        sections = sections,
      ) => begin
        for c in comps
          @assign comp = component(c)
          @assign binding = getBinding(comp)
          @assign eval_binding = replaceBindingConstants(binding)
          if !referenceEq(binding, eval_binding)
            @assign comp = P_Component.setBinding(eval_binding, comp)
            updateComponent!(comp, c)
          end
        end
        @assign () = begin
          @match sections begin
            P_Sections.Sections.SECTIONS(__) => begin
              @assign sections.algorithms = list(
                P_Algorithm.Algorithm.mapExp(a, replaceExpConstants)
                for a in sections.algorithms
              )
              @assign cls.sections = sections
              updateClass(cls, func.node)
              ()
            end

            SECTIONS_EXTERNAL(__) => begin
              @assign sections.args =
                list(replaceExpConstants(arg) for arg in sections.args)
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
