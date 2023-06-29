module FunctionTreeImpl
  using MetaModelica
  using ExportAll
  import ..Absyn
  import ..Main.M_Function
  import ..Main.toString
  import ..Main.toFlatString
  import ..Main.AbsynUtil
  const Key = Absyn.Path
  const Value = M_Function
  include("../Util/baseAvlTreeCode.jl")
  addConflictDefault = addConflictKeep
  keyStr = (k) -> begin
    return AbsynUtil.pathString(k)
  end

  valueStr = (vs) -> begin
    return toFlatString(vs)
  end

#= John 2023-04-03:
Default(Julia) string compare does not give the right result.
MetaModelica string comp is used instead.
=#
  keyCompare = (inKey1::Key, inKey2::Key) -> begin
    return stringCompare(keyStr(inKey1), keyStr(inKey2))
  end

end #= FunctionTreeImpl =#
FunctionTree = FunctionTreeImpl.Tree
const NFFunctionTree = FunctionTreeImpl

import .FunctionTree

function flatten(classInst::InstNode, name::String; prefix = COMPONENT_REF_EMPTY())
  local flatModel::FlatModel
  local sections::Sections
  local vars::Vector{Variable}
  local eql::Vector{Equation}
  local ieql::Vector{Equation}
  local alg::Vector{Algorithm}
  local ialg::Vector{Algorithm}
  local structuralSubmodels::List{FLAT_MODEL} = nil
  local cmt::Option{SCode.Comment}
  sections = SECTIONS_EMPTY()
  @debug "CALLING TOP LEVEL FLATTEN"
  cmt = SCodeUtil.getElementComment(definition(classInst))
  (vars, sections, structuralSubmodels) = flattenClass(
    getClass(classInst),
    prefix,
    Visibility.PUBLIC,
    NONE(),
    Variable[],
    sections,
    structuralSubmodels,
  )
  flatModel = begin
    @match sections begin
      SECTIONS(__) => begin
        eql = sections.equations
        ieql = sections.initialEquations
        alg = sections.algorithms #Was reverse
        ialg = sections.initialAlgorithms #Was reverse
        FLAT_MODEL(name,
                   vars,
                   eql,
                   ieql,
                   alg,
                   ialg,
                   structuralSubmodels,
                   NONE(),
                   nil,
                   nil,
                   Bool[],
                   cmt)
      end
      _ => begin
        FLAT_MODEL(name,
                   vars,
                   Equation[],
                   Equation[],
                   Algorithm[],
                   Algorithm[],
                   nil,
                   NONE(),
                   nil,
                   nil,
                   Bool[],
                   cmt)
      end
    end
  end
#  execStat(getInstanceName() + "(" + name + ")")
#  flatModel = resolveConnections(flatModel, name)
  return flatModel
end

function collectFunctions(flatModel::FlatModel, name::String)::FunctionTree
  local funcs::FunctionTree
  funcs = FunctionTreeImpl.new()
  funcs = ArrayUtil.fold(flatModel.variables, collectComponentFuncs, funcs)
  funcs = ArrayUtil.fold(flatModel.equations, collectEquationFuncs, funcs)
  funcs = ArrayUtil.fold(flatModel.initialEquations, collectEquationFuncs, funcs)
  funcs = ArrayUtil.fold(flatModel.algorithms, collectAlgorithmFuncs, funcs)
  funcs = ArrayUtil.fold(flatModel.initialAlgorithms, collectAlgorithmFuncs, funcs)
  return funcs
end

function flattenClass(
  cls::Class,
  prefix::ComponentRef,
  visibility::VisibilityType,
  binding::Option{<:Binding},
  vars::Vector{Variable},
  sections::Sections,
  #= Extension. Models that are a structural part of a flat model and should not be merged.=#
  structuralSubModels::List{FLAT_MODEL},
  #= End extension =#
  )
  @debug "CALLING flattenClass"
  local comps::Vector{InstNode}
  local bindings::List{Binding}
  local b::Binding
  () = begin
    @match cls begin
      INSTANCED_CLASS(elements = CLASS_TREE_FLAT_TREE(components = comps)) =>
        begin
          if isSome(binding)
            @match SOME(b) = binding
            if isBound(b)
              b = flattenBinding(b, rest(prefix))
              bindings = getRecordBindings(b, comps)
              Error.assertion(
                listLength(bindings) == arrayLength(comps),
                getInstanceName() +
                " got record binding with wrong number of elements for " +
                toString(prefix),
                sourceInfo(),
              )
              for c in comps
                (vars, sections) = flattenComponent(
                  c,
                  prefix,
                  visibility,
                  SOME(listHead(bindings)),
                  vars,
                  sections,
                  structuralSubModels
                )
                bindings = listRest(bindings)
              end
            else
              for c in comps
                (vars, sections, structuralSubModels) =
                  flattenComponent(c, prefix, visibility, binding,
                                   vars, sections, structuralSubModels)
              end
            end
          else
            for c in comps
              (vars, sections, structuralSubModels) =
                flattenComponent(c, prefix, visibility, NONE(),
                                 vars, sections, structuralSubModels)
            end
          end
          sections = flattenSections(cls.sections, prefix, sections)
          ()
        end

      TYPED_DERIVED(__) => begin
        @assign (vars, sections) = flattenClass(
          getClass(cls.baseClass),
          prefix,
          visibility,
          binding,
          vars,
          sections,
          structuralSubModels
        )
        ()
      end

      INSTANCED_BUILTIN(__) => begin
        ()
      end

      _ => begin
        Error.assertion(
          false,
          getInstanceName() +
          " got non-instantiated component " +
          toString(prefix) +
          "\\n",
          sourceInfo(),
        )
        ()
      end
    end
  end
  return (vars, sections, structuralSubModels)
end

function flattenComponent(
  inComponent::InstNode,
  prefix::ComponentRef,
  visibility::VisibilityType,
  outerBinding::Option{<:Binding},
  vars::Vector{Variable},
  sections::Sections,
  #= Passed from the top level class. =#
  structuralSubModels::List{FLAT_MODEL}
)
  @debug "FLATTEN COMPONENT: " * toString(inComponent)
  local comp_node::InstNode
  local c::Component
  local ty::M_Type
  local condition::Binding
  local cls::Class
  local vis::VisibilityType
  #=  Remove components that are only outer. =#
  if isOnlyOuter(inComponent)
    return (vars, sections, structuralSubModels)
  end
  comp_node = resolveOuter(inComponent)
  c = component(comp_node)
  @match c begin
    TYPED_COMPONENT(condition = condition, ty = ty) => begin
      #=  Delete the component if it has a condition that's false. =#
      if isDeletedComponent(condition, prefix)
        deleteComponent(inComponent)
        return (vars, sections, structuralSubModels)
      end
      cls = getClass(c.classInst)
      vis = if isProtected(inComponent)
        Visibility.PROTECTED
      else
        visibility
      end
      if isComplexComponent(ty)
        #= A complex component such as a model or a class =#
        if c.attributes.isStructuralMode == true
          @debug "FLATTEN A COMPLEX STRUCTURAL COMPONENT"
          #=
            Since this component is structural we do not flatten it.
            Instead we create a new FlatModel and add it to the list.
            If this component in turn has structural subcomponents these are added
            to the list of FlatModels for this component using recursion.
          =#
          local prefixOfComponent = fromNode(comp_node, ty)
          structuralSubModels =
            flatten(comp_node #= TODO: Or do we need some instantation specific stuff=#,
                    name(comp_node), prefix = prefixOfComponent) <| structuralSubModels
        else
          @debug "FLATTEN A COMPLEX COMPONENT WITH ATTRIBUTES: " c.attributes
          (vars, sections) = flattenComplexComponent(
            comp_node,
            c,
            cls,
            ty,
            vis,
            outerBinding,
            prefix,
            vars,
            sections,
          )
        end
      else
        @debug "Flatten a simple component"
        (vars, sections) = flattenSimpleComponent(
          comp_node,
          c,
          vis,
          outerBinding,
          getTypeAttributes(cls),
          prefix,
          vars,
          sections,
        )
      end
      ()
    end
    DELETED_COMPONENT(__) => begin
      @debug "Component deleted"
      ()
    end
    _ => begin
      #Error.assertion(false, getInstanceName() + " got unknown component", sourceInfo())
      @error "Got unknown component!"
      fail()
    end
  end
  return (vars, sections, structuralSubModels)
end

function isDeletedComponent(condition::Binding, prefix::ComponentRef)::Bool
  local isDeleted::Bool

  local exp::Expression
  local cond::Binding

  if isBound(condition)
    cond = condition
    exp = getTypedExp(cond)
    exp = evalExp(exp, EVALTARGET_CONDITION(Binding_getInfo(cond)))
    exp = stripBindingInfo(exp)
    if arrayAllEqual(exp)
      @assign exp = arrayFirstScalar(exp)
    end
    @assign isDeleted = begin
      @match exp begin
        BOOLEAN_EXPRESSION(__) => begin
          !exp.value
        end

        _ => begin
          #=  TODO: Flattening the condition works as intended here, but we can't yet
          =#
          #=        delete components inside array instances in a reliable way since
          =#
          #=        the components share the same node. I.e. we can't delete a[1].x
          =#
          #=        while keeping a[2].x. So for now we skip flattening the condition,
          =#
          #=        so that we get an error message in that case instead (because then
          =#
          #=        the expression will be an array instead of a scalar boolean).
          =#
          #= cond := flattenBinding(condition, prefix);
          =#
          #=  Hack to make arrays work when all elements have the same value.
          =#
          Error.addSourceMessage(
            Error.CONDITIONAL_EXP_WITHOUT_VALUE,
            list(toString(exp)),
            Binding_getInfo(cond),
          )
          fail()
        end
      end
    end
  else
    @assign isDeleted = false
  end
  return isDeleted
end

""" Recursively marks components as deleted. """
function deleteComponent(compNode::InstNode)
  local comp::Component

  #=  @adrpo: don't delete the inner/outer node, it doesn't work!
  =#
  if isInnerOuterNode(compNode)
    return
  end
  @assign comp = component(compNode)
  updateComponent!(DELETED_COMPONENT(comp), compNode)
  return deleteClassComponents(classInstance(comp))
end

function deleteClassComponents(clsNode::InstNode)
  local cls::Class = getClass(clsNode)
  local comps::Vector{InstNode}

  return @assign () = begin
    @match cls begin
      INSTANCED_CLASS(
        elements = CLASS_TREE_FLAT_TREE(components = comps),
      ) where {(!isType(cls.restriction))} => begin
        for c in comps
          deleteComponent(c)
        end
        ()
      end

      TYPED_DERIVED(__) => begin
        deleteClassComponents(cls.baseClass)
        ()
      end

      _ => begin
        ()
      end
    end
  end
end

function isComplexComponent(ty::NFType)::Bool
  local isComplex::Bool
  @assign isComplex = begin
    @match ty begin
      TYPE_COMPLEX(complexTy = COMPLEX_EXTERNAL_OBJECT(__)) => begin
        false
      end
      TYPE_COMPLEX(__) => begin
        true
      end
      TYPE_ARRAY(__) => begin
        isComplexComponent(ty.elementType)
      end

      _ => begin
        false
      end
    end
  end
  return isComplex
end

function flattenSimpleComponent(
  n::InstNode,
  comp::Component,
  visibility::VisibilityType,
  outerBinding::Option{<:Binding},
  typeAttrs::List{<:Modifier},
  prefix::ComponentRef,
  vars::Vector{Variable},
  sections::Sections,
)

  local comp_node::InstNode = n
  local name::ComponentRef
  local binding::Binding
  local ty
  local cmt::Option{SCode.Comment}
  local info::SourceInfo
  local comp_attr::Attributes
  local vis::VisibilityType
  local eq::Equation
  local ty_attrs::Vector{Tuple{String, Binding}}
  local var::VariabilityType
  local unfix::Bool
  ty = comp.ty
  binding = comp.binding
  comp_attr = comp.attributes
  cmt = comp.comment
  info = comp.info
  var = comp_attr.variability
  if isSome(outerBinding)
    @match SOME(binding) = outerBinding
    unfix = isUnbound(binding) && var == Variability.PARAMETER
  else
    binding = flattenBinding(binding, prefix)
    unfix = false
  end
  #=  If the component is an array component with a binding and at least discrete variability,
  =#
  #=  move the binding into an equation. This avoids having to scalarize the binding.
  =#
  # if !Flags.isSet(Flags.NF_API)
  #   if isArray(ty) && isBound(binding) && var >= Variability.DISCRETE
  #     @assign name = prefixCref(comp_node, ty, nil, prefix)
  #     @assign eq = EQUATION_ARRAY_EQUALITY(
  #       CREF_EXPRESSION(ty, name),
  #       getTypedExp(binding),
  #       ty,
  #       ElementSource.createElementSource(info),
  #     )
  #     @assign sections = prependEquation(eq, sections)
  #     @assign binding = EMPTY_BINDING
  #   end
  # end
  name = prefixScope(comp_node, ty, nil, prefix)
  ty_attrs = [flattenTypeAttribute(m, name) for m in typeAttrs]
  #=  Set fixed = true for parameters that are part of a record instance whose
  =#
  #=  binding couldn't be split and was moved to an initial equation.
  =#
  if unfix
    ty_attrs = ArrayUtil.removeOnTrue("fixed", isTypeAttributeNamed, ty_attrs)
    ty_attrs = push!(
      ty_attrs,
      (
        "fixed",
        FLAT_BINDING(
          BOOLEAN_EXPRESSION(false),
          Variability.CONSTANT,
        )
      )
    )
  end
  vars = push!(
    vars,
    VARIABLE(
      name,
      ty,
      binding,
      visibility,
      comp_attr,
      ty_attrs,
      cmt,
      info,
    ),
  )
  return (vars, sections)
end

function flattenTypeAttribute(attr::Modifier, prefix::ComponentRef)::Tuple{String, Binding}
  local outAttr::Tuple{String, Binding}
  local bnd::Binding
  bnd = flattenBinding(binding(attr), prefix, true)
  outAttr = (name(attr), bnd)
  return outAttr
end

function isTypeAttributeNamed(name::String, attr::Tuple{<:String, Binding})::Bool
  local isNamed::Bool
  local attr_name::String
  (attr_name, _) = attr
  isNamed = name == attr_name
  return isNamed
end

function getRecordBindings(binding::Binding, comps::Vector{<:InstNode})::List{Binding}
  local recordBindings::List{Binding} = nil
  local binding_exp::Expression
  local var::VariabilityType
  binding_exp = getTypedExp(binding)
  var = variability(binding)
  #=  Convert the expressions in the record expression into bindings.
  =#
  @assign recordBindings = begin
    @match binding_exp begin
RECORD_EXPRESSION(__) => begin
        list(if isEmpty(e)
          EMPTY_BINDING
        else
          FLAT_BINDING(e, var)
        end for e in binding_exp.elements)
      end

      _ => begin
        #=  The binding for a record field might be Expression.EMPTY if it comes
        =#
        #=  from an evaluated function call where it wasn't assigned a value.
        =#
        Error.assertion(
          false,
          getInstanceName() +
          " got non-record binding " +
          toString(binding_exp),
          sourceInfo(),
        )
        fail()
      end
    end
  end
  return recordBindings
end

function flattenComplexComponent(
  node::InstNode,
  comp::Component,
  cls::Class,
  ty::NFType,
  visibility::VisibilityType,
  outerBinding::Option{<:Binding},
  prefix::ComponentRef,
  vars::Vector{Variable},
  sections::Sections,
)::Tuple{Vector{Variable}, Sections}
  @debug "FLATTEN COMPLEX COMPONENT: " * toString(node)
  local dims::List{Dimension}
  local name::ComponentRef
  local binding::Binding
  local opt_binding::Option{Binding}
  local binding_exp::Expression
  local eq::Equation
  local bindings::List{Expression}
  local comp_var::VariabilityType
  local binding_var::VariabilityType
  local structuralSubModels::List{FLAT_MODEL}
  dims = arrayDims(ty)
  binding = if isSome(outerBinding)
    Util.getOption(outerBinding)
  else
    getBinding(comp)
  end
  #=  Create an equation if there's a binding on a complex component.
  =#
  if isExplicitlyBound(binding)
    binding = flattenBinding(binding, prefix)
    binding_exp = getTypedExp(binding)
    binding_var = variability(binding)
    comp_var = variability(comp)
    if comp_var <= Variability.STRUCTURAL_PARAMETER ||
       binding_var <= Variability.STRUCTURAL_PARAMETER
      binding_exp =
        stripBindingInfo(evalExp(binding_exp))
    elseif binding_var == Variability.PARAMETER && isFinal(comp)
        binding_exp =
          stripBindingInfo(evalExp(binding_exp))
    else
      binding_exp = simplify(binding_exp)
    end
    binding_exp = splitRecordCref(binding_exp)
    if !isRecordOrRecordArray(binding_exp)
      name = prefixCref(node, ty, nil, prefix)
      eq = EQUATION_EQUALITY(
        CREF_EXPRESSION(ty, name),
        binding_exp,
        ty,
        ElementSource.createElementSource(InstNode_info(node)),
      )
      sections = prependEquation(
        eq,
        sections,
        comp_var <= Variability.PARAMETER,
      )
      opt_binding = SOME(EMPTY_BINDING)
    else
      binding = setTypedExp(binding_exp, binding)
      opt_binding = SOME(binding)
    end
  else
    opt_binding = NONE()
  end
  #=  TODO: This will probably not work so well if the binding is an array that
  =#
  #=        contains record non-literals. In that case we should probably
  =#
  #=        create an equation for each non-literal in the array, and pass the
  =#
  #=        rest on as usual.
  =#
  name = prefixScope(node, ty, nil, prefix)
  #=  Flatten the class directly if the component is a scalar, otherwise scalarize it.
  =#
  if listEmpty(dims)
    (vars, sections) = flattenClass(cls, name, visibility, opt_binding, vars, sections, nil)
  else
    (vars, sections) = flattenArray(cls, dims, name, visibility, opt_binding, vars, sections, nil)
  end
  return (vars, sections)
end

function flattenArray(
  cls::Class,
  dimensions::List{<:Dimension},
  prefix::ComponentRef,
  visibility::VisibilityType,
  binding::Option{<:Binding},
  vars::Vector{Variable},
  sections::Sections,
  subscripts::List{<:Subscript} = nil,
)::Tuple{Vector{Variable}, Sections}
  local dim::Dimension
  local rest_dims::List{Dimension}
  local sub_pre::ComponentRef
  local range_iter::RangeIterator
  local sub_exp::Expression
  local subs::List{Subscript}
  local vrs::Vector{Variable}
  local sects::Sections
  #=  if we don't scalarize flatten the class and vectorize it
  =#
  if !Flags.isSet(Flags.NF_SCALARIZE)
    @assign (vrs, sects) = flattenClass(
      cls,
      prefix,
      visibility,
      binding,
      nil,
      P_Sections.Sections.SECTIONS(nil, nil, nil, nil),
    )
    for v in vrs
      @assign v.ty = liftArrayLeftList(v.ty, dimensions)
      @assign vars = _cons(v, vars)
    end
    @assign () = begin
      @match sects begin
        P_Sections.Sections.SECTIONS(__) => begin
          #=  add dimensions to the types
          =#
          #=  vectorize equations
          =#
          for eqn in listReverse(sects.equations)
            @assign sections = P_Sections.Sections.prependEquation(
              vectorizeEquation(eqn, dimensions, prefix),
              sections,
            )
          end
          for eqn in listReverse(sects.initialEquations)
            @assign sections = P_Sections.Sections.prependEquation(
              vectorizeEquation(eqn, dimensions, prefix),
              sections,
              true,
            )
          end
          for alg in listReverse(sects.algorithms)
            @assign sections = P_Sections.Sections.prependAlgorithm(
              vectorizeAlgorithm(alg, dimensions, prefix),
              sections,
            )
          end
          for alg in listReverse(sects.initialAlgorithms)
            @assign sections = P_Sections.Sections.prependAlgorithm(
              vectorizeAlgorithm(alg, dimensions, prefix),
              sections,
              true,
            )
          end
          ()
        end
      end
    end
    return (vars, sections)
  end
  if listEmpty(dimensions)
    subs = listReverse(subscripts)
    sub_pre = setSubscripts(subs, prefix)
    (vars, sections) = flattenClass(
      cls,
      sub_pre,
      visibility,
      subscriptBindingOpt(subs, binding),
      vars,
      sections,
      nil,
    )
  else
    @match _cons(dim, rest_dims) = dimensions
    range_iter = fromDim(dim)
    while hasNext(range_iter)
      (range_iter, sub_exp) = next(range_iter)
      (vars, sections) = flattenArray(
        cls,
        rest_dims,
        prefix,
        visibility,
        binding,
        vars,
        sections,
        _cons(SUBSCRIPT_INDEX(sub_exp), subscripts),
      )
    end
  end
  return (vars, sections)
end

function vectorizeEquation(
  eqn::Equation,
  dimensions::List,
  prefix::ComponentRef,
)::Equation
  local veqn::Equation
  @assign veqn = begin
    local prefix_node::InstNode
    local iter::InstNode
    local stop::Int
    local range::Expression
    @match eqn begin
      EQUATION_EQUALITY(
        lhs = CREF_EXPRESSION(__),
        rhs = CREF_EXPRESSION(__),
      ) => begin
        EQUATION_ARRAY_EQUALITY(
          eqn.lhs,
          eqn.rhs,
          liftArrayLeftList(eqn.ty, dimensions),
          eqn.source,
        )
      end
      _ => begin
        #=  convert simple equality of crefs to array equality
        =#
        #=  wrap general equation into for loop
        =#
        @assign iter = begin
          @match node(prefix) begin
            prefix_node && COMPONENT_NODE(__) => begin
              COMPONENT_NODE(
                "i",
                prefix_node.visibility,
                P_Pointer.create(ITERATOR(
                  TYPE_INTEGER(),
                  Variability.IMPLICITLY_DISCRETE,
                  P_Component.info(Pointer.access(prefix_node.component)),
                )),
                prefix_node.parent,
                NORMAL_COMP(),
              )
            end
          end
        end
        @match list(P_Dimension.Dimension.INTEGER_EXPRESSION(size = stop)) = dimensions
        @assign range = RANGE_EXPRESSION(
          TYPE_ARRAY(TYPE_INTEGER(), dimensions),
          INTEGER_EXPRESSION(1),
          NONE(),
          INTEGER_EXPRESSION(stop),
        )
        @assign veqn = mapExp(
          eqn,
          (
            x = prefix,
            y = SUBSCRIPT_INDEX(CREF_EXPRESSION(TYPE_INTEGER(), makeIterator(iter, TYPE_INTEGER()))),
          )
          -> addIterator(x, y)),
        EQUATION_FOR(
          iter,
          SOME(range),
          list(veqn),
          source(eqn),
        )
      end
    end
  end
  return veqn
end

function vectorizeAlgorithm(
  alg::Algorithm,
  dimensions::List{<:Dimension},
  prefix::ComponentRef
)::Algorithm
  local valg::Algorithm
  @assign valg = begin
    local prefix_node::InstNode
    local iter::InstNode
    local stop::Int
    local range::Expression
    local body::Vector{Statement}
    @match alg begin
      ALGORITHM(
        statements = ALG_ASSIGNMENT(
          lhs = CREF_EXPRESSION(__),
          rhs = CREF_EXPRESSION(__),
        ) <| nil(),
      ) => begin
        alg
      end

      _ => begin
        #=  let simple assignment as is
        =#
        #=  wrap general algorithm into for loop
        =#
        @assign iter = begin
          @match node(prefix) begin
            prefix_node && COMPONENT_NODE(__) => begin
              COMPONENT_NODE(
                "i",
                prefix_node.visibility,
                P_Pointer.create(ITERATOR_COMPONENT(
                  TYPE_INTEGER(),
                  Variability.IMPLICITLY_DISCRETE,
                  P_Component.info(P_Pointer.access(prefix_node.component)),
                )),
                prefix_node.parent,
                NORMAL_COMP(),
              )
            end
          end
        end
        @match list(P_Dimension.Dimension.INTEGER_EXPRESSION(size = stop)) = dimensions
        @assign range = RANGE_EXPRESSION(
          TYPE_ARRAY(TYPE_INTEGER(), dimensions),
          INTEGER_EXPRESSION(1),
          NONE(),
          INTEGER_EXPRESSION(stop),
        )
        @debug "Manually check this error. It has to do with higher order functions in the translation"
        @assign body = mapExpList(
          alg.statements,
          (x) -> addIterator(
            x,
            subscript = SUBSCRIPT_INDEX(CREF_EXPRESSION(
              TYPE_INTEGER(),
              makeIterator(iter, TYPE_INTEGER()),
            ))))
        ALGORITHM(
          list(P_Statement.Statement.FOR(iter, SOME(range), body, alg.source)),
          alg.source,
        )
      end
    end
  end
  return valg
end

function addIterator(
  exp::Expression,
  prefix::ComponentRef,
  subscript::Subscript,
)::Expression

  @assign exp = map(
    exp,
    (prefix, subscript) -> addIterator_traverse(prefix = prefix, subscript = subscript),
  )
  return exp
end

function addIterator_traverse(
  exp::Expression,
  prefix::ComponentRef,
  subscript::Subscript,
)::Expression

  local restString::String
  local prefixString::String = toString(prefix)
  local prefixLength::Int = stringLength(prefixString)

  @assign exp = begin
    local restCref::ComponentRef
    @match exp begin
      CREF_EXPRESSION(
        cref = CREF(restCref = restCref),
      ) => begin
        @assign restString = toString(restCref)
        if prefixLength <= stringLength(restString) &&
           prefixString == substring(restString, 1, prefixLength)
          @assign exp.cref =
            applySubscripts(list(subscript), exp.cref)
        end
        exp
      end

      _ => begin
        exp
      end
    end
  end
  return exp
end

function subscriptBindingOpt(
  subscripts::List{<:Subscript},
  binding::Option{<:Binding},
)::Option{Binding}

  local b::Binding
  local exp::Expression
  local ty::M_Type

  if isSome(binding)
    @match SOME(b) = binding
    @assign binding = begin
      @match b begin
        TYPED_BINDING(bindingExp = exp, bindingType = ty) => begin
          @assign b.bindingExp = applySubscripts(subscripts, exp)
          @assign b.bindingType = arrayElementType(ty)
          SOME(b)
        end

        FLAT_BINDING(bindingExp = exp) => begin
          @assign b.bindingExp = applySubscripts(subscripts, exp)
          SOME(b)
        end

        _ => begin
          binding
        end
      end
    end
  end
  return binding
end


function flattenBinding(
  binding::Binding,
  prefix::ComponentRef
)::Binding
  flattenBinding(binding, prefix, false)
end

function flattenBinding(
  binding::Binding,
  prefix::ComponentRef,
  isTypeAttribute::Bool
)::Binding
  @assign binding = begin
    local subs::List{Subscript}
    local accum_subs::List{Subscript}
    local binding_level::Int
    local bind_exp::Expression
    local pars::List{InstNode}
    local par::InstNode
    @match binding begin
      UNBOUND(__) => begin
        binding
      end
      TYPED_BINDING(__) => begin
        if binding.isFlattened
          return binding
        end
        @assign binding.bindingExp =
          flattenBindingExp(binding.bindingExp, prefix, isTypeAttribute)
        @assign binding.isFlattened = true
        binding
      end

      CEVAL_BINDING(__) => begin
        EMPTY_BINDING
      end

      FLAT_BINDING(__) => begin
        binding
      end

      INVALID_BINDING(__) => begin
        #=  CEVAL_BINDINGs are temporary bindings generated by the constant
        =#
        #=  evaluation and no longer needed after flattening.
        =#
        Error.addTotalMessages(binding.errors)
        fail()
      end
      _ => begin
        #Error.assertion(false, getInstanceName() + " got untyped binding.", sourceInfo())
        @error "Untyped binding!"
        fail()
      end
    end
  end
  return binding
end

function flattenBindingExp(
  exp::Expression,
  prefix::ComponentRef,
  isTypeAttribute::Bool = false
)::Expression
  local outExp::Expression
  local subs::List{Subscript}
  local accum_subs::List{Subscript}
  local binding_level::Int
  local parents::List{InstNode}
  local pre::ComponentRef
  local cr_node::InstNode
  local par::InstNode
  outExp = begin
    @match exp begin
      BINDING_EXP(exp = outExp) => begin
        parents = listRest(exp.parents)
        if !exp.isEach
          if isTypeAttribute && !isempty(parents)
            parents = listRest(parents)
          end
          if !isempty(parents)
            outExp = flattenBindingExp2(outExp, prefix, parents)
          end
        end
        flattenExp(outExp, prefix)
      end
      _ => begin
        exp
      end
    end
  end
  return outExp
end

function flattenBindingExp2(
  @nospecialize(exp::Expression),
  @nospecialize(prefix::ComponentRef),
  parents::List{InstNode},
)::Expression
  local outExp::Expression = exp
  local binding_level::Int = 0
  local subs::List{Subscript}
  local pre::ComponentRef = prefix
  local pre_node::InstNode
  local par::InstNode
  par = listHead(parents)
  if isComponent(par) && !isEmpty(pre)
    pre_node = node(pre)
    while !refEqual(pre_node, par)
      pre = rest(pre)
      if isEmpty(pre)
        return outExp
      end
      pre_node = node(pre)
    end
  end
  for parent in parents
    binding_level = binding_level + dimensionCount(getType(parent))
  end
  if binding_level > 0
    #subs = listAppend(listReverse(s) for s in subscriptsAll(pre)) modifed as per below
    local subsT = list(listReverse(s) for s in subscriptsAll(pre) if !(s isa Nil) )
    subs = list(Base.collect(Iterators.flatten(subsT))...)
    #= End of modification=#
    binding_level = min(binding_level, listLength(subs))
    #subs = ListUtil.firstN_reverse(subs, binding_level)
    subs = ListUtil.firstN(subs, binding_level)
    outExp = applySubscripts(subs, exp)
  end
  #=
  TODO: Optimize this, making a list of all subscripts in the prefix when
  only a few are needed is unnecessary.
  =#
  return outExp
end

function flattenExp(exp::Expression, prefix::ComponentRef)::Expression
  @assign exp =
    map(exp, (x) -> flattenExp_traverse(x,prefix))
  return exp
end

function flattenExp_traverse(exp::Expression, prefix::ComponentRef)::Expression
  exp = begin
    @match exp begin
      CREF_EXPRESSION(__) => begin
        @assign exp.cref = transferSubscripts(prefix, exp.cref)
        exp
      end

      BINDING_EXP(__) => begin
        flattenBindingExp(exp, prefix)
      end

      _ => begin
        exp
      end
    end
    return exp
  end
end

function flattenSections(
  sections::Sections,
  prefix::ComponentRef,
  accumSections::Sections,
)::Sections
  () = begin
    local eq::Vector{Equation}
    local ieq::Vector{Equation}
    local alg::Vector{Algorithm}
    local ialg::Vector{Algorithm}
    @match sections begin
      SECTIONS(__) => begin
        eq = flattenEquations(sections.equations, prefix)
        ieq = flattenEquations(sections.initialEquations, prefix)
        alg = flattenAlgorithms(sections.algorithms, prefix)
        ialg = flattenAlgorithms(sections.initialAlgorithms, prefix)
        accumSections =
          prepend(eq, ieq, alg, ialg, accumSections)
        ()
      end
      _ => begin
        ()
      end
    end
  end
  return accumSections
end

function flattenEquations(eql::Vector{Equation}, prefix::ComponentRef)
  local equations::Vector{Equation} = Equation[]
  for eq in eql
    equations = flattenEquation(eq, prefix, equations)
  end
  return equations
end

function flattenEquation(
  @nospecialize(eq::Equation),
  @nospecialize(prefix::ComponentRef),
  inEquations::Vector{Equation},
)
  equations = begin
    local e1::Expression
    local e2::Expression
    local e3::Expression
    local eql::Vector{Equation}
    @match eq begin
      EQUATION_EQUALITY(__) => begin
        e1 = flattenExp(eq.lhs, prefix)
        e2 = flattenExp(eq.rhs, prefix)
        push!(inEquations, EQUATION_EQUALITY(e1, e2, eq.ty, eq.source))
      end
      EQUATION_FOR(__) => begin
        #Currently we unroll everything by default -John Tinnerholm 2021-05-04
        eql = if Flags.isSet(Flags.NF_SCALARIZE)
           unrollForLoop(eq, prefix, inEquations)
        else
          splitForLoop(eq, prefix, equations)
        end
        eql
      end
      EQUATION_CONNECT(__) => begin
        e1 = flattenExp(eq.lhs, prefix)
        e2 = flattenExp(eq.rhs, prefix)
        push!(inEquations, EQUATION_CONNECT(e1, e2, eq.source))
      end
      EQUATION_IF(__) => begin
        flattenIfEquation(eq, prefix, inEquations)
      end
      EQUATION_WHEN(__) => begin
        @assign eq.branches = Equation_Branch[flattenEqBranch(b, prefix) for b in eq.branches]
        push!(inEquations, eq)
      end
      EQUATION_ASSERT(__) => begin
        e1 = flattenExp(eq.condition, prefix)
        e2 = flattenExp(eq.message, prefix)
        e3 = flattenExp(eq.level, prefix)
        push!(inEquations, EQUATION_ASSERT(e1, e2, e3, eq.source))
      end
      EQUATION_TERMINATE(__) => begin
        e1 = flattenExp(eq.message, prefix)
        push!(inEquations, EQUATION_TERMINATE(e1, eq.source))
      end
      EQUATION_REINIT(__) => begin
        e1 = flattenExp(eq.cref, prefix)
        e2 = flattenExp(eq.reinitExp, prefix)
        push!(inEquations, EQUATION_REINIT(e1, e2, eq.source))
      end
      EQUATION_NORETCALL(__) => begin
        e1 = flattenExp(eq.exp, prefix)
        push!(inEquations, EQUATION_NORETCALL(e1, eq.source))
      end
      _ => begin
        push!(inEquations, eq)
      end
    end
  end
  return equations
end

function flattenIfEquation(
  @nospecialize(eq::Equation),
  prefix::ComponentRef,
  equations::Vector{Equation},
)
  local branch::Equation_Branch
  local branches::Vector{Equation_Branch}
  local bl::Vector{Equation_Branch} = Equation_Branch[]
  local cond::Expression
  local eql::Vector{Equation} = Equation[]
  local var::VariabilityType
  local has_connect::Bool
  local src::DAE.ElementSource
  local info::SourceInfo
  local target::EvalTarget
  @match EQUATION_IF(branches = branches, source = src) = eq
  has_connect = contains(eq, isConnectEq)
  #=  Print errors for unbound constants/parameters if the if-equation contains
  =#
  #=  connects, since we must select a branch in that case.
  =#
  target = if has_connect
    EVALTARGET_GENERIC(Equation_info(eq))
  else
    EVALTARGET_IGNORE_ERRORS()
  end
  while !isempty(branches)
    @match [branch, branches...] = branches
    bl = begin
      @match branch begin
        EQUATION_BRANCH(cond, var, eql) => begin
          #=  Flatten the condition and body of the branch. =#
          cond = flattenExp(cond, prefix)
          eql = flattenEquations(eql, prefix)
          #=  Evaluate structural conditions. =#
          if var <= Variability.STRUCTURAL_PARAMETER
            cond = evalExp(cond, target)
            if !isBoolean(cond) && has_connect
              Error.addInternalError(
                "Failed to evaluate branch condition in if equation containing connect equations: `" +
                toString(cond) +
                "`",
                info(eq),
              )
              fail()
            end
          end
          #=  Conditions in an if-equation that contains connects must be possible to evaluate. =#
          if isTrue(cond)           #=  The condition is true and the branch will thus always be selected =#
            #=  if reached, so we can discard the remaining branches.          =#
            branches = Equation_Branch[]
            if isempty(bl) #= If we haven't collected any other branches yet, replace the if-equation with this branch.=#
              equations = vcat(eql, equations)
            else
              bl = push!(bl, makeBranch(cond, eql, var))
            end
          elseif !isFalse(cond)
            push!(bl, makeBranch(cond, eql, var))
          end
          #=  Otherwise, append this branch. =#
          #=  Only add the branch to the list of branches if the condition is not =#
          #=  literal false, otherwise just drop it since it will never trigger. =#
          bl
        end
          EQUATION_INVALID_BRANCH(
          branch = EQUATION_BRANCH(
            condition = cond,
            conditionVar = var,
          ),
        ) where {(has_connect)} => begin
          #=  An invalid branch must have a false condition, anything else is an error.
          =#
          if var <= Variability.STRUCTURAL_PARAMETER
            cond = evalExp(cond, target)
          end
          if !isFalse(cond)
            triggerErrors(branch)
          end
          bl
        end
        _ => begin
          push!(bl, branch)
        end
      end
    end
  end
  #=
  Add the flattened if-equation to the list of equations if there are any
  branches still remaining.
  =#
  if !isempty(bl)
    equations = push!(equations, EQUATION_IF(bl, src))
    return equations
  end
  return equations
end

function isConnectEq(eq::Equation)::Bool
  local isConnect::Bool

  @assign isConnect = begin
    local fn::M_Function
    @match eq begin
      EQUATION_CONNECT(__) => begin
        true
      end

      EQUATION_NORETCALL(
        exp = CALL_EXPRESSION(call = TYPED_CALL(fn = fn)),
      ) => begin
        AbsynUtil.pathFirstIdent(name(fn)) == "Connections"
      end

      _ => begin
        false
      end
    end
  end
  return isConnect
end

function flattenEqBranch(
  @nospecialize(branch::Equation_Branch),
  @nospecialize(prefix::ComponentRef),
)::Equation_Branch
  local exp::Expression
  local eql::Vector{Equation}
  local var::VariabilityType
  @match EQUATION_BRANCH(exp, var, eql) = branch
  exp = flattenExp(exp, prefix)
  eql = flattenEquations(eql, prefix)
  branch = makeBranch(exp, eql, var)
  return branch
end

"""
 Unrolls an equational for-loop.
"""
function unrollForLoop(
  forLoop::EQUATION_FOR,
  prefix::ComponentRef,
  equations::Vector{Equation},
)
  local iter::InstNode
  local body::Vector{Equation}
  local unrolled_body::Vector{Equation}
  local range::Expression
  local range_iter::RangeIterator
  local val::Expression
  @match EQUATION_FOR(iterator = iter, range = SOME(range), body = body) =
    forLoop
  #=
  Unroll the loop by replacing the iterator with each of its values
  in the for loop body.
  =#
  range = flattenExp(range, prefix)
  range =
    evalExp(range, EVALTARGET_RANGE(Equation_info(forLoop)))
  range_iter = RangeIterator_fromExp(range)
  while hasNext(range_iter)
    (range_iter, val) = next(range_iter)
    unrolled_body = mapExpList(
      body,
      (expArg) -> replaceIterator(expArg, iter, val),
    )
    unrolled_body = flattenEquations(unrolled_body, prefix)
    equations = vcat(unrolled_body, equations)
  end
  return equations
end

function splitForLoop(
  forLoop::Equation,
  prefix::ComponentRef,
  equations::List{<:Equation},
)::Vector{Equation}

  local iter::InstNode
  local range::Option{Expression}
  local body::Vector{Equation}
  local connects::Vector{Equation}
  local non_connects::Vector{Equation}
  local src::DAE.ElementSource

  @match EQUATION_FOR(iter, range, body, src) = forLoop
  @assign (connects, non_connects) = splitForLoop2(body)
  if !listEmpty(connects)
    @assign equations =
      unrollForLoop(EQUATION_FOR(iter, range, connects, src), prefix, equations)
  end
  if !listEmpty(non_connects)
    @assign equations =
      _cons(EQUATION_FOR(iter, range, non_connects, src), equations)
  end
  return equations
end

function splitForLoop2(forBody::List{<:Equation})::Tuple{Vector{Equation}, Vector{Equation}}
  local nonConnects::Vector{Equation} = nil
  local connects::Vector{Equation} = nil

  local conns::Vector{Equation}
  local nconns::Vector{Equation}

  for eq in forBody
    @assign () = begin
      @match eq begin
        EQUATION_CONNECT(__) => begin
          @assign connects = _cons(eq, connects)
          ()
        end

        EQUATION_FOR(__) => begin
          @assign (conns, nconns) = splitForLoop2(eq.body)
          if !listEmpty(conns)
            @assign connects = _cons(
              EQUATION_FOR(eq.iterator, eq.range, conns, eq.source),
              connects,
            )
          end
          if !listEmpty(nconns)
            @assign nonConnects = _cons(
              EQUATION_FOR(eq.iterator, eq.range, nconns, eq.source),
              nonConnects,
            )
          end
          ()
        end

        _ => begin
          @assign nonConnects = _cons(eq, nonConnects)
          ()
        end
      end
    end
  end
  return (connects, nonConnects)
end

function flattenAlgorithms(
  algorithms::Vector{Algorithm},
  prefix::ComponentRef)
  local outAlgorithms::Vector{Algorithm} = Algorithm[]
  for alg in algorithms
    @assign alg.statements = mapExpList(
      alg.statements,
      (x) -> flattenExp(x, prefix),
    )
    #=
      CheckModel relies on the ElementSource to know whether a certain algorithm comes from
      an array component, otherwise is will miscount the number of equations.
    =#
    if hasSubscripts(prefix)
      @assign alg.source = addElementSourceArrayPrefix(alg.source, prefix)
    end
    outAlgorithms = prepend!([alg], outAlgorithms)
  end
  return outAlgorithms
end

function addElementSourceArrayPrefix(
  source::DAE.ElementSource,
  prefix::ComponentRef,
)::DAE.ElementSource

  local comp_pre::DAE.ComponentPrefix

  #=  It seems the backend doesn't really care about the ComponentPrefix, and
  =#
  #=  creating a proper prefix here could be rather expensive. So we just create
  =#
  #=  a dummy prefix here with one subscript to keep CheckModel happy.
  =#
  @assign comp_pre = DAE.ComponentPrefix.PRE(
    firstName(prefix),
    nil,
    list(DAE.SUBSCRIPT_INDEX(DAE.Exp.ICONST(-1))),
    DAE.ComponentPrefix.NOCOMPPRE(),
    ClassInf.State.UNKNOWN(Absyn.IDENT("?")),
    AbsynUtil.dummyInfo,
  )
  @assign source = ElementSource.addElementSourceInstanceOpt(source, comp_pre)
  return source
end

""" Generates the connect equations and adds them to the equation list """
function resolveConnections(flatModel::FlatModel, name::String)::FlatModel
  local conns::Connections
  local conn_eql::Vector{Equation}
  local csets::ConnectionSets.Sets
  local csets_array::Vector{List{Connector}}
  local ctable::CardinalityTable.Table
  local broken::BrokenEdges = nil
  #=  get the connections from the model =#
  (flatModel, conns) = collect(flatModel)
  #=  Elaborate expandable connectors.=#
  (flatModel, conns) = elaborate(flatModel, conns)
  #=  handle overconstrained connections =#
  #=  - build the graph =#
  #=  - evaluate the Connections.* operators =#
  #=  - generate the equations to replace the broken connects =#
  #=  - return the broken connects + the equations =#
  if System.getHasOverconstrainedConnectors()
    (flatModel, broken, _) = handleOverconstrainedConnections(flatModel, conns, name)
  end
  #=  add the broken connections  =#
  conns = addBroken(broken, conns)
  #=  build the sets, check the broken connects =#
  csets = ConnectionSets.fromConnections(conns)
  (csets_array, _) = ConnectionSets.extractSets(csets)
  #=  generate the equations =#
  conn_eql = generateEquations(csets_array) #=In NFConnectEquations=#
  #=  append the equalityConstraint call equations for the broken connects =#
  if System.getHasOverconstrainedConnectors()
    conn_eql = vcat(conn_eql, listArray(ListUtil.flatten(ListUtil.map(broken, Util.tuple33))))
  end
  #=  add the equations to the flat model=#
  @assign flatModel.equations = vcat(conn_eql, flatModel.equations)
  @assign flatModel.variables = [v for v in flatModel.variables if isPresent(v)]
  ctable = CardinalityTable.fromConnections(conns)
  #=  Evaluate any connection operators if they're used. =#
  if System.getHasStreamConnectors() || System.getUsesCardinality()
    flatModel = evaluateConnectionOperators(flatModel, csets, csets_array, ctable)
  end
#  execStat(getInstanceName() + "(" + name + ")")
  return flatModel
end

function evaluateConnectionOperators(
  flatModel::FlatModel,
  sets::ConnectionSets.Sets,
  setsArray::Vector{<:List{<:Connector}},
  ctable::CardinalityTable.Table,
)::FlatModel
  @assign flatModel.variables =
    Variable[evaluateBindingConnOp(c, sets, setsArray, ctable)
             for c in flatModel.variables]
  @assign flatModel.equations =
    evaluateEquationsConnOp(flatModel.equations, sets, setsArray, ctable)
  @assign flatModel.initialEquations =
    evaluateEquationsConnOp(flatModel.initialEquations, sets, setsArray, ctable)
  #=  TODO: Implement evaluation for algorithm sections. =#
  return flatModel
end

function evaluateBindingConnOp(
  var::Variable,
  sets#=::ConnectionSets.Sets=#,
  setsArray::Vector{<:List{<:Connector}},
  ctable::CardinalityTable.Table,
)::Variable
  local binding::Binding
  local exp::Expression
  local eval_exp::Expression
  () = begin
    @match var begin
      VARIABLE(
        binding = binding && TYPED_BINDING(bindingExp = exp),
      ) => begin
        eval_exp = evaluateOperators(exp, sets, setsArray, ctable)
        if !referenceEq(exp, eval_exp)
          @assign binding.bindingExp = eval_exp
          @assign var.binding = binding
        end
        ()
      end

      _ => begin
        ()
      end
    end
  end
  return var
end

function evaluateEquationsConnOp(
  equations::Vector{Equation},
  sets#=::ConnectionSets.Sets=#,
  setsArray::Vector{<:List{<:Connector}},
  ctable::CardinalityTable.Table,
)
  equations = [
    mapExp(
      eq,
      (expArg) -> evaluateOperators(
        expArg,
        sets,
        setsArray,
        ctable,
      ),
    ) for eq in equations
  ]
  return equations
end

function collectComponentFuncs(var::Variable, funcs::FunctionTree)::FunctionTree
  () = begin
    @match var begin
      VARIABLE(__) => begin
        funcs = collectTypeFuncs(var.ty, funcs)
        funcs = collectBindingFuncs(var.binding, funcs)
        for attr in var.typeAttributes
          funcs = collectBindingFuncs(Util.tuple22(attr), funcs)
        end
        ()
      end
    end
  end
  return funcs
end

function collectBindingFuncs(binding::Binding, funcs::FunctionTree)::FunctionTree
  if isExplicitlyBound(binding)
    @assign funcs = collectExpFuncs(getTypedExp(binding), funcs)
  end
  return funcs
end

function collectTypeFuncs(ty::NFType, funcs::FunctionTree)::FunctionTree
  @assign () = begin
    local con::InstNode
    local de::InstNode
    local fn::M_Function
    @match ty begin
      TYPE_ARRAY(__) => begin
        funcs = foldExpList(
          ty.dimensions,
          collectExpFuncs_traverse,
          funcs,
        )
        funcs = collectTypeFuncs(ty.elementType, funcs)
        ()
      end

      TYPE_FUNCTION(fn = fn) => begin
        funcs = flattenFunction(fn, funcs)
        ()
      end

      TYPE_COMPLEX(
        complexTy = COMPLEX_EXTERNAL_OBJECT(constructor = con, destructor = de),
      ) => begin
        #=  Collect external object structors.
        =#
        @assign funcs = collectStructor(con, funcs)
        @assign funcs = collectStructor(de, funcs)
        ()
      end

      TYPE_COMPLEX(complexTy = COMPLEX_RECORD(constructor = con)) => begin
        #=  Collect record constructors.
        =#
        @assign funcs = collectStructor(con, funcs)
        ()
      end
      _ => begin
        ()
      end
    end
  end
  return funcs
end

function collectStructor(node::InstNode, funcs::FunctionTree)::FunctionTree
  local cache::CachedData
  local fn::List{M_Function}
  @assign cache = getFuncCache(node)
  @assign () = begin
    @match cache begin
      C_FUNCTION(__) => begin
        for fn in cache.funcs
          @assign funcs = flattenFunction(fn, funcs)
        end
        ()
      end

      _ => begin
        ()
      end
    end
  end
  return funcs
end

function collectEquationFuncs(@nospecialize(eq::Equation),
                              @nospecialize(funcs::FunctionTree))
  () = begin
    @match eq begin
      EQUATION_EQUALITY(__) => begin
        funcs = collectExpFuncs(eq.lhs, funcs)
        funcs = collectExpFuncs(eq.rhs, funcs)
        funcs = collectTypeFuncs(eq.ty, funcs)
        ()
      end
      EQUATION_ARRAY_EQUALITY(__) => begin
        #=  Lhs is always a cref, no need to check it. =#
        funcs = collectExpFuncs(eq.rhs, funcs)
        funcs = collectTypeFuncs(eq.ty, funcs)
        ()
      end
      EQUATION_FOR(__) => begin
        #=
          For equations are always unrolled, so functions in the range doesn't
          matter since they are always evaluated.
        =#
        funcs = ArrayUtil.fold(eq.body, collectEquationFuncs, funcs)
        ()
      end
      EQUATION_IF(__) => begin
        funcs = ArrayUtil.fold(eq.branches, collectEqBranchFuncs, funcs)
        ()
      end
      EQUATION_WHEN(__) => begin
        funcs = ArrayUtil.fold(eq.branches, collectEqBranchFuncs, funcs)
        ()
      end
      EQUATION_ASSERT(__) => begin
        funcs = collectExpFuncs(eq.condition, funcs)
        funcs = collectExpFuncs(eq.message, funcs)
        funcs = collectExpFuncs(eq.level, funcs)
        ()
      end
      EQUATION_TERMINATE(__) => begin
        funcs = collectExpFuncs(eq.message, funcs)
        ()
      end
      EQUATION_REINIT(__) => begin
        funcs = collectExpFuncs(eq.reinitExp, funcs)
        ()
      end
      EQUATION_NORETCALL(__) => begin
        funcs = collectExpFuncs(eq.exp, funcs)
        ()
      end
      _ => begin
        ()
      end
    end
  end
  return funcs
end

function collectEqBranchFuncs(
  @nospecialize(branch::Equation_Branch),
  @nospecialize(funcs::FunctionTree),
  )::FunctionTree
  () = begin
    @match branch begin
      EQUATION_BRANCH(__) => begin
        funcs = collectExpFuncs(branch.condition, funcs)
        funcs = ArrayUtil.fold(branch.body, collectEquationFuncs, funcs)
        ()
      end
      _ => begin
        ()
      end
    end
  end
  return funcs
end

function collectAlgorithmFuncs(alg::Algorithm, funcs::FunctionTree)::FunctionTree
  funcs = ArrayUtil.fold(alg.statements, collectStatementFuncs, funcs)
  return funcs
end

function collectStatementFuncs(@nospecialize(stmt::Statement), funcs::FunctionTree)::FunctionTree
  () = begin
    @match stmt begin
      ALG_ASSIGNMENT(__) => begin
        funcs = collectExpFuncs(stmt.lhs, funcs)
        funcs = collectExpFuncs(stmt.rhs, funcs)
        funcs = collectTypeFuncs(stmt.ty, funcs)
        ()
      end

      ALG_FOR(__) => begin
        funcs = ArrayUtil.fold(stmt.body, collectStatementFuncs, funcs)
        funcs = collectExpFuncs(Util.getOption(stmt.range), funcs)
        ()
      end

      ALG_IF(__) => begin
        funcs = ArrayUtil.fold(stmt.branches, collectStmtBranchFuncs, funcs)
        ()
      end

      ALG_WHEN(__) => begin
        funcs = ArrayUtil.fold(stmt.branches, collectStmtBranchFuncs, funcs)
        ()
      end

      ALG_ASSERT(__) => begin
        funcs = collectExpFuncs(stmt.condition, funcs)
        funcs = collectExpFuncs(stmt.message, funcs)
        funcs = collectExpFuncs(stmt.level, funcs)
        ()
      end

      ALG_TERMINATE(__) => begin
        funcs = collectExpFuncs(stmt.message, funcs)
        ()
      end

      ALG_NORETCALL(__) => begin
        funcs = collectExpFuncs(stmt.exp, funcs)
        ()
      end

      ALG_WHILE(__) => begin
        funcs = collectExpFuncs(stmt.condition, funcs)
        funcs = ArrayUtil.fold(stmt.body, collectStatementFuncs, funcs)
        ()
      end

      _ => begin
        ()
      end
    end
  end
  return funcs
end

function collectStmtBranchFuncs(
  branch::Tuple{Expression, Vector{Statement}},
  funcs::FunctionTree,
)::FunctionTree
  funcs = collectExpFuncs(Util.tuple21(branch), funcs)
  funcs = ArrayUtil.fold(Util.tuple22(branch), collectStatementFuncs, funcs)
  return funcs
end

function collectExpFuncs(exp::Expression, funcs::FunctionTree)::FunctionTree
  funcs = fold(exp, collectExpFuncs_traverse, funcs)
  return funcs
end

function collectExpFuncs_traverse(exp::Expression, funcs::FunctionTree)::FunctionTree
  @assign () = begin
    local fn::M_Function
    @match exp begin
      CALL_EXPRESSION(__) => begin
        funcs = flattenFunction(typedFunction(exp.call), funcs)
        ()
      end
      CREF_EXPRESSION(__) => begin
        funcs = collectTypeFuncs(exp.ty, funcs)
        ()
      end
      RECORD_EXPRESSION(__) => begin
        funcs = collectTypeFuncs(exp.ty, funcs)
        ()
      end
      PARTIAL_FUNCTION_APPLICATION_EXPRESSION(__) => begin
        for f in P_Function.getRefCache(exp.fn)
          funcs = flattenFunction(f, funcs)
        end
        ()
      end
      _ => begin
        ()
      end
    end
  end
  return funcs
end

function flattenFunction(func::M_Function, funcs::FunctionTree)::FunctionTree
  local fn::M_Function = func
  if !isCollected(fn)
    fn = evaluateFunction(fn)
    simplifyFunction(fn)
    collect(fn)
    if !isPartial(fn.node)
      funcs = FunctionTreeImpl.add(funcs, name(fn), fn)
      funcs = collectClassFunctions(fn.node, funcs)
      for fn_der in fn.derivatives
        for der_fn in getCachedFuncs(fn_der.derivativeFn)
          funcs = flattenFunction(der_fn, funcs)
        end
      end
    end
  end
  return funcs
end

function collectClassFunctions(clsNode::InstNode, funcs::FunctionTree)::FunctionTree
  local cls::Class
  local cls_tree::ClassTree
  local sections::Sections
  local comp::Component
  local binding::Binding
  @assign cls = getClass(clsNode)
  @assign () = begin
    @match cls begin
      INSTANCED_CLASS(
        elements = cls_tree && CLASS_TREE_FLAT_TREE(__),
        sections = sections,
      ) => begin
        for c in cls_tree.components
          @assign comp = component(c)
          @assign funcs = collectTypeFuncs(getType(comp), funcs)
          @assign binding = getBinding(comp)
          if isExplicitlyBound(binding)
            @assign funcs = collectExpFuncs(getTypedExp(binding), funcs)
          end
        end
        () = begin
          @match sections begin
            SECTIONS(__) => begin
              funcs = ArrayUtil.fold(sections.algorithms, collectAlgorithmFuncs, funcs)
              ()
            end

            _ => begin
              ()
            end
          end
        end
        ()
      end

      TYPED_DERIVED(__) => begin
        @assign funcs = collectClassFunctions(cls.baseClass, funcs)
        ()
      end

      _ => begin
        ()
      end
    end
  end
  return funcs
end
