@UniontypeDecl Modifier

module ModTable
import ..Modifier
using MetaModelica
using ExportAll
using ..Main

const Key = String
const Value = Modifier
#= Modelica extend clause =#
include("../Util/baseAvlTreeCode.jl")


keyCompare = (inKey1::String, inKey2::String) -> begin
  res = stringCompare(inKey1, inKey2)
  return res
end

@exportAll()
end

abstract type Modifier end

struct MODIFIER_NOMOD <: Modifier end

mutable struct MODIFIER_REDECLARE{  T0 <: SCode.Final,
                                    T1 <: SCode.Each,
                                    T2 <: InstNode,
                                    T3 <: Modifier} <: Modifier
  finalPrefix::T0
  eachPrefix::T1
  element::T2
  mod::T3
end

mutable struct MODIFIER_MODIFIER{ T0 <: String,
                                  T1 <: SCode.Final,
                                  T2 <: SCode.Each,
                                  T4 <: ModTable.Tree,
                                  T5 <: SOURCEINFO} <: Modifier
  name::T0
  finalPrefix::T1
  eachPrefix::T2
  binding::Binding
  subModifiers::T4
  info::T5
end


#= Structure that represents where a modifier comes from. =#
@Uniontype ModifierScope begin
  @Record SCOPE_EXTENDS begin
    path::Absyn.Path
  end
  @Record SCOPE_CLASS begin
    name::String
  end
  @Record SCOPE_COMPONENT begin
    name::String
  end
end

const EMPTY_MOD = MODIFIER_NOMOD()::MODIFIER_NOMOD

function toString(scope::ModifierScope)::String
  local string::String
  @assign string = begin
    @match scope begin
      COMPONENT(__) => begin
        "component " + scope.name
      end

      CLASS(__) => begin
        "class " + scope.name
      end

      EXTENDS(__) => begin
        "extends " + AbsynUtil.pathString(scope.path)
      end
    end
  end
  return string
end

function name(scope::ModifierScope)::String
  local name::String
  @assign name = begin
    @match scope begin
      COMPONENT(__) => begin
        scope.name
      end
      CLASS(__) => begin
        scope.name
      end
      EXTENDS(__) => begin
        AbsynUtil.pathString(scope.path)
      end
    end
  end
  return name
end

function fromElement(element::SCode.Element)::ModifierScope
  local scope::ModifierScope
  @assign scope = begin
    @match element begin
      SCode.Element.COMPONENT(__) => begin
        COMPONENT(element.name)
      end
      SCode.CLASS(__) => begin
        CLASS(element.name)
      end
      SCode.Element.EXTENDS(__) => begin
        EXTENDS(element.baseClassPath)
      end
    end
  end
  return scope
end


function toFlatString(mod::Modifier, printName::Bool = true)::String
  local string::String
  @assign string = begin
    local submods::List{Modifier}
    local subs_str::String
    local binding_str::String
    local binding_sep::String
    @match mod begin
      MODIFIER_MODIFIER(__) => begin
        @assign submods = ModTable.listValues(mod.subModifiers)
        if !listEmpty(submods)
          @assign subs_str =
            "(" + stringDelimitList(list(toFlatString(s) for s in submods), ", ") + ")"
          @assign binding_sep = " = "
        else
          @assign subs_str = ""
          @assign binding_sep = if printName
            " = "
          else
            "= "
          end
        end
        @assign binding_str = toFlatString(mod.binding, binding_sep)
        if printName
          mod.name + subs_str + binding_str
        else
          subs_str + binding_str
        end
      end

      _ => begin
        ""
      end
    end
  end
  return string
end

function toString(mod::Modifier, printName::Bool = true)::String
  local string::String
  @assign string = begin
    local submods::List{Modifier}
    local subs_str::String
    local binding_str::String
    local binding_sep::String
    @match mod begin
      MODIFIER_MODIFIER(__) => begin
        @assign submods = ModTable.listValues(mod.subModifiers)
        if !listEmpty(submods)
          @assign subs_str =
            "(" + stringDelimitList(list(toString(s) for s in submods), ", ") + ")"
          @assign binding_sep = " = "
        else
          @assign subs_str = ""
          @assign binding_sep = if printName
            " = "
          else
            "= "
          end
        end
        @assign binding_str = toString(mod.binding, binding_sep)
        if printName
          mod.name + subs_str + binding_str
        else
          subs_str + binding_str
        end
      end

      MODIFIER_REDECLARE(__) => begin
        toString(mod.element)
      end

      _ => begin
        ""
      end
    end
  end
  return string
end

function map(mod::Modifier, func::FuncT)::Modifier
  @match mod begin
    MODIFIER_MODIFIER(__) => begin
      mod.subModifiers = ModTable.map(mod.subModifiers, func)
      ()
      end
    _ => begin
      ()
    end
  end
  return mod
end

function isFinal(mod::Modifier)::Bool
  local isFinal::Bool

  @assign isFinal = begin
    @match mod begin
      MODIFIER_MODIFIER(finalPrefix = SCode.FINAL(__)) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return isFinal
end

function isEach(mod::Modifier)::Bool
  local isEach::Bool
  isEach = begin
    @match mod begin
      MODIFIER_MODIFIER(eachPrefix = SCode.EACH(__)) => begin
        true
      end
      _ => begin
        false
      end
    end
  end
  return isEach
end

function toList(mod::Modifier)
  local modList::List{Modifier}
  modList = begin
    @match mod begin
      MODIFIER_MODIFIER(__) => begin
        ModTable.listValues(mod.subModifiers)
      end
      _ => begin
        nil
      end
    end
  end
  return modList
end

function isRedeclare(mod::Modifier)::Bool
  local isRedeclare::Bool

  @assign isRedeclare = begin
    @match mod begin
      MODIFIER_REDECLARE(__) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return isRedeclare
end

function isEmpty(mod::Modifier)::Bool
  local isEmpty::Bool

  @assign isEmpty = begin
    @match mod begin
      MODIFIER_NOMOD(__) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return isEmpty
end

function merge(outerMod::Modifier, innerMod::Modifier, name::String = "")::Modifier
  local mergedMod::Modifier

  mergedMod = begin
    local submods::ModTable.Tree
    local binding::Binding
    #=  One of the modifiers is NOMOD, return the other. =#
    @match (outerMod, innerMod) begin
      (MODIFIER_NOMOD(__), _) => begin
        innerMod
      end

      (_, MODIFIER_NOMOD(__)) => begin
        outerMod
      end

      (MODIFIER_MODIFIER(__), MODIFIER_MODIFIER(__)) => begin
        #=  Two modifiers, merge bindings and submodifiers. =#
        checkFinalOverride(innerMod.finalPrefix, outerMod, innerMod.info)
        binding = if isBound(outerMod.binding)
          outerMod.binding
        else
          innerMod.binding
        end
        submods =
          ModTable.join(innerMod.subModifiers, outerMod.subModifiers, merge)
        MODIFIER_MODIFIER(
          outerMod.name,
          outerMod.finalPrefix,
          outerMod.eachPrefix,
          binding,
          submods,
          outerMod.info,
        )
      end

      (MODIFIER_REDECLARE(__), MODIFIER_MODIFIER(__)) => begin
        mod = merge(outerMod.mod, innerMod)
        MODIFIER_REDECLARE(outerMod.finalPrefix, outerMod.eachPrefix, outerMod.element, mod)
      end

      (MODIFIER_MODIFIER(__), MODIFIER_REDECLARE(__)) => begin
        mod = merge(outerMod, innerMod.mod)
        MODIFIER_REDECLARE(innerMod.finalPrefix, innerMod.eachPrefix, innerMod.element, mod)
      end

      (MODIFIER_REDECLARE(__), _) => begin
        outerMod
      end

      (_, MODIFIER_REDECLARE(__)) => begin
        innerMod
      end

      _ => begin
        Error.addMessage(Error.INTERNAL_ERROR, list("Mod.mergeMod failed on unknown modifier."))
        fail()
      end
    end
  end
  return mergedMod
end

function setBinding(binding::Binding, modifier::Modifier)::Modifier
  @match modifier begin
    MODIFIER_MODIFIER(__) => begin
      modifier.binding = binding
      ()
    end
  end
  return modifier
end

function binding(modifier::Modifier)::Binding
  local binding::Binding
  @assign binding = begin
    @match modifier begin
      MODIFIER_MODIFIER(__) => begin
        modifier.binding
      end
      _ => begin
        EMPTY_BINDING()
      end
    end
  end
  return binding
end

function hasBinding(modifier::Modifier)::Bool
  local b::Bool
  b = begin
    @match modifier begin
      MODIFIER_MODIFIER(__) => begin
        isBound(modifier.binding)
      end
      _ => begin
        false
      end
    end
  end
  return b
end

function Modifier_info(modifier::Modifier)::SourceInfo
  local info::SourceInfo

  @assign info = begin
    @match modifier begin
      MODIFIER_MODIFIER(__) => begin
        modifier.info
      end

      MODIFIER_REDECLARE(__) => begin
        info(modifier.element)
      end

      _ => begin
        AbsynUtil.dummyInfo
      end
    end
  end
  return info
end

function name(modifier::Modifier)::String
  local nameStr::String
  nameStr = begin
    @match modifier begin
      MODIFIER_MODIFIER(__) => begin
        modifier.name
      end
      MODIFIER_REDECLARE(__) => begin
        name(modifier.element)
      end
    end
  end
  return nameStr
end

function lookupModifier(modName::String, modifier::Modifier)::Modifier
  local subMod::Modifier
  subMod = begin
    @matchcontinue modifier begin
      MODIFIER_MODIFIER(__) => begin
        ModTable.get(modifier.subModifiers, modName)
      end
      _ => begin
        EMPTY_MOD
      end
    end
  end
  return subMod
end

function addParent_work(name::String, parentNode::InstNode, mod::Modifier)::Modifier
  local outMod::Modifier
  outMod = begin
    local binding::Binding
    @match mod begin
      MODIFIER_MODIFIER(binding = binding) => begin
        mod.binding = addParent(parentNode, binding)
        if !isEach(binding)
          map(mod, (x,y) -> addParent_work(x, parentNode, y))
        else
          mod
        end
      end

      _ => begin
        mod
      end
    end
  end
  return outMod
end

function addParent(parentNode::InstNode, mod::Modifier)::Modifier
  local outMod::Modifier
  outMod = begin
    local binding::Binding
    @match mod begin
      MODIFIER_MODIFIER(binding = binding) => begin
        mod.binding = addParent(parentNode, binding)
        map(mod, (x, y) -> addParent_work(x, parentNode, y))
      end
      _ => begin
        mod
      end
    end
  end
  return outMod
end

"""  This function makes modifiers applied to final elements final, e.g. for
     'final Real x(start = 1.0)' it will mark '(start = 1.0)' as final. This is
     done so that we only need to check for final violations while merging
     modifiers.
"""
function patchElementModFinal(
  prefixes::SCode.Prefixes,
  info::SourceInfo,
  mod::SCode.Mod,
)::SCode.Mod

  if SCodeUtil.finalBool(SCodeUtil.prefixesFinal(prefixes))
    mod = begin
      @match mod begin
        SCode.MOD(__) => begin
          @assign mod.finalPrefix = SCode.FINAL()
          mod
        end

        SCode.REDECL(__) => begin
          @assign mod.finalPrefix = SCode.FINAL()
          mod
        end

        _ => begin
          SCode.MOD(SCode.Final.FINAL(), SCode.NOT_EACH(), nil, NONE(), info)
        end
      end
    end
  end
  return mod
end

function fromElement(
  element::SCode.Element,
  parents::List{<:InstNode},
  scope::InstNode,
  )
  local mod::Modifier
  mod = begin
    local def::SCode.ClassDef
    local smod::SCode.Mod
    @match element begin
      SCode.EXTENDS(__) => begin
        create(
          element.modifications,
          "",
          SCOPE_EXTENDS(element.baseClassPath),
          parents,
          scope,
        )
      end

      SCode.COMPONENT(__) => begin
        smod =
          patchElementModFinal(element.prefixes, element.info, element.modifications)
        create(smod, element.name, SCOPE_COMPONENT(element.name), parents, scope)
      end

      SCode.CLASS(classDef = def && SCode.DERIVED(__)) => begin
        create(
          def.modifications,
          element.name,
          SCOPE_CLASS(element.name),
          parents,
          scope,
        )
      end

      SCode.CLASS(classDef = def && SCode.CLASS_EXTENDS(__)) => begin
        create(
          def.modifications,
          element.name,
          SCOPE_CLASS(element.name),
          parents,
          scope,
        )
      end

      _ => begin
        MODIFIER_NOMOD()
      end
    end
  end
  return mod
end

function stripSCodeMod(elem::SCode.Element)::Tuple{SCode.Element, SCode.Mod}
  local mod::SCode.Mod

  @assign mod = begin
    local cdef::SCode.ClassDef
    @match elem begin
      SCode.CLASS(classDef = cdef && SCode.ClassDef.DERIVED(modifications = mod)) => begin
        if !SCodeUtil.isEmptyMod(mod)
          @assign cdef.modifications = SCode.NOMOD()
          @assign elem.classDef = cdef
        end
        mod
      end

      SCode.Element.COMPONENT(modifications = mod) => begin
        if !SCodeUtil.isEmptyMod(mod)
          @assign elem.modifications = SCode.NOMOD()
        end
        mod
      end

      _ => begin
        SCode.NOMOD()
      end
    end
  end
  return (elem, mod)
end

function create(mod::SCode.Mod,
                name::String,
                modScope::ModifierScope,
                parents::List{<:InstNode},
                scope::InstNode)
  local newMod::Modifier
  newMod = begin
    local submod_lst::List{Tuple{String, Modifier}}
    local submod_table::ModTable.Tree
    local binding::Binding
    local elem::SCode.Element
    local smod::SCode.Mod
    local is_each::Bool
    local node::InstNode
    local pars::List{InstNode}
    @match mod begin
      SCode.NOMOD(__) => begin
        MODIFIER_NOMOD()
      end
      SCode.MOD(__) => begin
        is_each = SCodeUtil.eachBool(mod.eachPrefix)
        binding =
          fromAbsyn(mod.binding, is_each, parents, scope, mod.info)
        pars = if is_each
          nil
        else
          parents
        end
        submod_lst =
          list(tuple(m.ident, createSubMod(m, modScope, pars, scope)) for m in mod.subModLst)
        submod_table = ModTable.fromList(
          submod_lst,
          (modScope, nil) -> mergeLocal(scope = modScope, prefix = nil),
        )
        MODIFIER_MODIFIER(name, mod.finalPrefix, mod.eachPrefix, binding, submod_table, mod.info)
      end

      SCode.REDECL(element = elem) => begin
        node = new(elem, scope)
        if isClass(node)
          partialInstClass(node)
        end
        MODIFIER_REDECLARE(mod.finalPrefix, mod.eachPrefix, node, MODIFIER_NOMOD())
      end
    end
  end
  return newMod
end

""" #= Merges two modifiers in the same scope, i.e. like a(x(y = 1), x(z = 2)).
     This is allowed as long as the two modifiers doesn't modify the same
     element, otherwise it's an error. =#"""
function mergeLocal(
  mod1::Modifier,
  mod2::Modifier,
  name::String = "",
  scope::ModifierScope = nothing,
  prefix::List{<:String} = nil,
)::Modifier
  local mod::Modifier
  local comp_name::String
  @assign mod = begin
    @match (mod1, mod2) begin
      (MODIFIER_MODIFIER(__), MODIFIER_MODIFIER(binding = UNBOUND(__))) => begin
        #=  The second modifier has no binding, use the binding from the first.
        =#
        lst = mod1.name <| prefix
        #@debug "Value of $lst"
        @assign mod1.subModifiers = ModTable.join(
          mod1.subModifiers,
          mod2.subModifiers,
          (x, scope, lst) ->
            mergeLocal(x, scope, lst))
        mod1
      end
      (MODIFIER_MODIFIER(binding = UNBOUND(__)), MODIFIER_MODIFIER(__)) => begin
        #=  The first modifier has no binding, use the binding from the second.
        =#
        lst = mod1.name <| prefix
        #@debug "Value of $lst"
        @assign mod2.subModifiers = ModTable.join(
          mod2.subModifiers,
          mod1.subModifiers,
          (x) ->
            mergeLocal(x, scope, lst))
        mod2
      end

      _ => begin
        #=  Both modifiers modify the same element, give duplicate modification error.
        =#
        comp_name =
          stringDelimitList(listReverse(_cons(P_Modifier.name(mod1), prefix)), ".")
        Error.addMultiSourceMessage(
          Error.DUPLICATE_MODIFICATIONS,
          list(comp_name, P_ModifierScope.toString(scope)),
          list(P_Modifier.info(mod1), P_Modifier.info(mod2)),
        )
        fail()
      end
    end
  end
  return mod
end

"""
Checks that a modifier is not trying to override a final modifier.
In that case it prints an error and fails, otherwise it does nothing.
"""
function checkFinalOverride(
  innerFinal::SCode.Final,
  outerMod::Modifier,
  innerInfo::SourceInfo,
  )
  _ = begin
    @match innerFinal begin
      SCode.FINAL(__) => begin
        Error.addMultiSourceMessage(
          Error.FINAL_COMPONENT_OVERRIDE,
          list(name(outerMod), toString(outerMod, false)),
          list(Modifier_info(outerMod), innerInfo),
        )
        fail()
      end
      _ => begin
        ()
      end
    end
  end
end

function createSubMod(subMod::SCode.SubMod,
                      modScope::ModifierScope,
                      parents::List{<:InstNode},
                      scope::InstNode)
  local mod::Modifier = create(subMod.mod, subMod.ident, modScope, parents, scope)
  return mod
end
