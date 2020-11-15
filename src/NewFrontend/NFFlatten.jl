Equation = NFEquation
Statement = NFStatement
FlatModel = NFFlatModel
Algorithm = NFAlgorithm
CardinalityTable = NFCardinalityTable
ComponentRef = NFComponentRef
Dimension = NFDimension
ExpressionIterator = NFExpressionIterator
Expression = NFExpression
Connection = NFConnection
Connector = NFConnector
ComplexType = NFComplexType

module FunctionTreeImpl
using MetaModelica
using ExportAll
import ..Absyn
import ..Main.M_Function
Key = Absyn.Path
Value = M_Function
include("../Util/baseAvlTreeCode.jl")
include("../Util/baseAvlSetCode.jl")


addConflictDefault = addConflictKeep

function new()
  return EMPTY()
end

@exportAll()
end
FunctionTree = FunctionTreeImpl.Tree
const NFFunctionTree = FunctionTreeImpl

import .FunctionTree
function flatten(classInst::InstNode, name::String)::FlatModel
  local flatModel::FlatModel
  local sections::Sections
  local vars::List{Variable}
  local eql::List{Equation}
  local ieql::List{Equation}
  local alg::List{Algorithm}
  local ialg::List{Algorithm}
  local cmt::Option{SCode.Comment}
  @assign sections = SECTIONS_EMPTY()
  @assign cmt = SCodeUtil.getElementComment(definition(classInst))
  @assign (vars, sections) = flattenClass(
    getClass(classInst),
    COMPONENT_REF_EMPTY(),
    Visibility.PUBLIC,
    NONE(),
    nil,
    sections,
  )
  @assign vars = listReverseInPlace(vars)
  @assign flatModel = begin
    @match sections begin
      SECTIONS(__) => begin
        @assign eql = listReverseInPlace(sections.equations)
        @assign ieql = listReverseInPlace(sections.initialEquations)
        @assign alg = listReverseInPlace(sections.algorithms)
        @assign ialg = listReverseInPlace(sections.initialAlgorithms)
        FLAT_MODEL(name, vars, eql, ieql, alg, ialg, cmt)
      end

      _ => begin
        FLAT_MODEL(name, vars, nil, nil, nil, nil, cmt)
      end
    end
  end
#  execStat(getInstanceName() + "(" + name + ")")
  @assign flatModel = resolveConnections(flatModel, name)
  return flatModel
end

function collectFunctions(flatModel::FlatModel, name::String)::FunctionTree
  local funcs::FunctionTree
  @assign funcs = FunctionTreeImpl.new()
  @assign funcs = ListUtil.fold(flatModel.variables, collectComponentFuncs, funcs)
  @assign funcs = ListUtil.fold(flatModel.equations, collectEquationFuncs, funcs)
  @assign funcs = ListUtil.fold(flatModel.initialEquations, collectEquationFuncs, funcs)
  @assign funcs = ListUtil.fold(flatModel.algorithms, collectAlgorithmFuncs, funcs)
  @assign funcs = ListUtil.fold(flatModel.initialAlgorithms, collectAlgorithmFuncs, funcs)
#  execStat(getInstanceName() + "(" + name + ")")
  return funcs
end

function flattenClass(
  cls::Class,
  prefix::ComponentRef,
  visibility::VisibilityType,
  binding::Option{<:Binding},
  vars::List{<:Variable},
  sections::Sections,
)::Tuple{List{Variable}, Sections}

  local comps::Array{InstNode}
  local bindings::List{Binding}
  local b::Binding

  #=  print(\">\" + stringAppendList(List.fill(\"  \", ComponentRef.depth(prefix)-1)) + ComponentRef.toString(prefix) + \"\\n\");
  =#
  @assign () = begin
    @match cls begin
      INSTANCED_CLASS(elements = CLASS_TREE_FLAT_TREE(components = comps)) =>
        begin
          if isSome(binding)
            @match SOME(b) = binding
            if isBound(b)
              @assign b = flattenBinding(b, rest(prefix))
              @assign bindings = getRecordBindings(b, comps)
              Error.assertion(
                listLength(bindings) == arrayLength(comps),
                getInstanceName() +
                " got record binding with wrong number of elements for " +
                toString(prefix),
                sourceInfo(),
              )
              for c in comps
                @assign (vars, sections) = flattenComponent(
                  c,
                  prefix,
                  visibility,
                  SOME(listHead(bindings)),
                  vars,
                  sections,
                )
                @assign bindings = listRest(bindings)
              end
            else
              for c in comps
                @assign (vars, sections) =
                  flattenComponent(c, prefix, visibility, binding, vars, sections)
              end
            end
          else
            for c in comps
              @assign (vars, sections) =
                flattenComponent(c, prefix, visibility, NONE(), vars, sections)
            end
          end
          @assign sections = flattenSections(cls.sections, prefix, sections)
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
  #=  print(\"<\" + stringAppendList(List.fill(\"  \", ComponentRef.depth(prefix)-1)) + ComponentRef.toString(prefix) + \"\\n\");
  =#
  return (vars, sections)
end

function flattenComponent(
  inComponent::InstNode,
  prefix::ComponentRef,
  visibility::VisibilityType,
  outerBinding::Option{<:Binding},
  vars::List{<:Variable},
  sections::Sections,
)::Tuple{List{Variable}, Sections}

  local comp_node::InstNode
  local c::Component
  local ty::M_Type
  local condition::Binding
  local cls::Class
  local vis::VisibilityType

  #=  Remove components that are only outer.
  =#
  if isOnlyOuter(inComponent)
    return (vars, sections)
  end
  @assign comp_node = resolveOuter(inComponent)
  @assign c = component(comp_node)
  @assign () = begin
    @match c begin
      TYPED_COMPONENT(condition = condition, ty = ty) => begin
        #=  Delete the component if it has a condition that's false.
        =#
        if isDeletedComponent(condition, prefix)
          deleteComponent(inComponent)
          return
        end
        @assign cls = getClass(c.classInst)
        @assign vis = if isProtected(inComponent)
          Visibility.PROTECTED
        else
          visibility
        end
        if isComplexComponent(ty)
          @assign (vars, sections) = flattenComplexComponent(
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
        else
          @assign (vars, sections) = flattenSimpleComponent(
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
        ()
      end

      _ => begin
        #Error.assertion(false, getInstanceName() + " got unknown component", sourceInfo())
        @error "Got unknown component!"
        fail()
      end
    end
  end
  #=  print(\"<-\" + stringAppendList(List.fill(\"  \", ComponentRef.depth(prefix))) + ComponentRef.toString(prefix) + \".\" + InstNode.name(component) + \"\\n\");
  =#
  return (vars, sections)
end

function isDeletedComponent(condition::Binding, prefix::ComponentRef)::Bool
  local isDeleted::Bool

  local exp::Expression
  local cond::Binding

  if isBound(condition)
    @assign cond = condition
    @assign exp = getTypedExp(cond)
    @assign exp = Ceval.evalExp(exp, Ceval.P_EvalTarget.CONDITION(Binding_getInfo(cond)))
    @assign exp = stripBindingInfo(exp)
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

""" #= Recursively marks components as deleted. =#"""
function deleteComponent(compNode::InstNode)
  local comp::Component

  #=  @adrpo: don't delete the inner/outer node, it doesn't work!
  =#
  if isInnerOuterNode(compNode)
    return
  end
  @assign comp = component(compNode)
  updateComponent!(P_Component.DELETED_COMPONENT(comp), compNode)
  return deleteClassComponents(P_Component.classInstance(comp))
end

function deleteClassComponents(clsNode::InstNode)
  local cls::Class = getClass(clsNode)
  local comps::Array{InstNode}

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
      TYPE_COMPLEX(complexTy = EXTERNAL_OBJECT(__)) => begin
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
  vars::List{<:Variable},
  sections::Sections,
)::Tuple{List{Variable}, Sections}

  local comp_node::InstNode = n
  local name::ComponentRef
  local binding::Binding
  local ty
  local cmt::Option{SCode.Comment}
  local info::SourceInfo
  local comp_attr::Attributes
  local vis::VisibilityType
  local eq::Equation
  local ty_attrs::List{Tuple{String, Binding}}
  local var::VariabilityType
  local unfix::Bool
  ty = comp.ty
  binding = comp.binding
  comp_attr = comp.attributes
  cmt = comp.comment
  info = comp.info
  @assign var = comp_attr.variability
  if isSome(outerBinding)
    @match SOME(binding) = outerBinding
    @assign unfix = isUnbound(binding) && var == Variability.PARAMETER
  else
    binding = flattenBinding(binding, prefix)
    @assign unfix = false
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
  #       ElementSource_createElementSource(info),
  #     )
  #     @assign sections = prependEquation(eq, sections)
  #     @assign binding = EMPTY_BINDING
  #   end
  # end
  @assign name = prefixScope(comp_node, ty, nil, prefix)
  @assign ty_attrs = list(flattenTypeAttribute(m, name) for m in typeAttrs)
  #=  Set fixed = true for parameters that are part of a record instance whose
  =#
  #=  binding couldn't be split and was moved to an initial equation.
  =#
  if unfix
    @assign ty_attrs = ListUtil.removeOnTrue("fixed", isTypeAttributeNamed, ty_attrs)
    @assign ty_attrs = _cons(
      (
        "fixed",
        FLAT_BINDING(
          BOOLEAN_EXPRESSION(false),
          Variability.CONSTANT,
        ),
      ),
      ty_attrs,
    )
  end
  @assign vars = _cons(
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
    vars,
  )
  return (vars, sections)
end

function flattenTypeAttribute(attr::Modifier, prefix::ComponentRef)::Tuple{String, Binding}
  local outAttr::Tuple{String, Binding}
  local bnd::Binding
  @assign bnd = flattenBinding(binding(attr), prefix, true)
  @assign outAttr = (name(attr), bnd)
  return outAttr
end

function isTypeAttributeNamed(name::String, attr::Tuple{<:String, Binding})::Bool
  local isNamed::Bool

  local attr_name::String

  @assign (attr_name, _) = attr
  @assign isNamed = name == attr_name
  return isNamed
end

function getRecordBindings(binding::Binding, comps::Array{<:InstNode})::List{Binding}
  local recordBindings::List{Binding} = nil

  local binding_exp::Expression
  local var::VariabilityType

  @assign binding_exp = getTypedExp(binding)
  @assign var = variability(binding)
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
  ty::M_Type,
  visibility::VisibilityType,
  outerBinding::Option{<:Binding},
  prefix::ComponentRef,
  vars::List{<:Variable},
  sections::Sections,
)::Tuple{List{Variable}, Sections}

  local dims::List{Dimension}
  local name::ComponentRef
  local binding::Binding
  local opt_binding::Option{Binding}
  local binding_exp::Expression
  local eq::Equation
  local bindings::List{Expression}
  local comp_var::VariabilityType
  local binding_var::VariabilityType

  @assign dims = arrayDims(ty)
  @assign binding = if isSome(outerBinding)
    Util.getOption(outerBinding)
  else
    getBinding(comp)
  end
  #=  Create an equation if there's a binding on a complex component.
  =#
  if isExplicitlyBound(binding)
    @assign binding = flattenBinding(binding, prefix)
    @assign binding_exp = getTypedExp(binding)
    @assign binding_var = variability(binding)
    @assign comp_var = variability(comp)
    if comp_var <= Variability.STRUCTURAL_PARAMETER ||
       binding_var <= Variability.STRUCTURAL_PARAMETER
      @assign binding_exp =
        stripBindingInfo(Ceval.evalExp(binding_exp))
    elseif binding_var == Variability.PARAMETER && isFinal(comp)
      try
        @assign binding_exp =
          stripBindingInfo(Ceval.evalExp(binding_exp))
      catch e
        @error  "e"
      end
    else
      @assign binding_exp = simplify(binding_exp)
    end
    @assign binding_exp = splitRecordCref(binding_exp)
    if !isRecordOrRecordArray(binding_exp)
      @assign name = prefixCref(node, ty, nil, prefix)
      @assign eq = EQUATION_EQUALITY(
        CREF_EXPRESSION(ty, name),
        binding_exp,
        ty,
        ElementSource_createElementSource(info(node)),
      )
      @assign sections = P_Sections.Sections.prependEquation(
        eq,
        sections,
        isInitial = comp_var <= Variability.PARAMETER,
      )
      @assign opt_binding = SOME(EMPTY_BINDING)
    else
      @assign binding = setTypedExp(binding_exp, binding)
      @assign opt_binding = SOME(binding)
    end
  else
    @assign opt_binding = NONE()
  end
  #=  TODO: This will probably not work so well if the binding is an array that
  =#
  #=        contains record non-literals. In that case we should probably
  =#
  #=        create an equation for each non-literal in the array, and pass the
  =#
  #=        rest on as usual.
  =#
  @assign name = prefixScope(node, ty, nil, prefix)
  #=  Flatten the class directly if the component is a scalar, otherwise scalarize it.
  =#
  if listEmpty(dims)
    @assign (vars, sections) =
      flattenClass(cls, name, visibility, opt_binding, vars, sections)
  else
    @assign (vars, sections) =
      flattenArray(cls, dims, name, visibility, opt_binding, vars, sections)
  end
  return (vars, sections)
end

function flattenArray(
  cls::Class,
  dimensions::List{<:Dimension},
  prefix::ComponentRef,
  visibility::VisibilityType,
  binding::Option{<:Binding},
  vars::List{<:Variable},
  sections::Sections,
  subscripts::List{<:Subscript} = nil,
)::Tuple{List{Variable}, Sections}

  local dim::Dimension
  local rest_dims::List{Dimension}
  local sub_pre::ComponentRef
  local range_iter::RangeIterator
  local sub_exp::Expression
  local subs::List{Subscript}
  local vrs::List{Variable}
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
    @assign subs = listReverse(subscripts)
    @assign sub_pre = setSubscripts(subs, prefix)
    @assign (vars, sections) = flattenClass(
      cls,
      sub_pre,
      visibility,
      subscriptBindingOpt(subs, binding),
      vars,
      sections,
    )
  else
    @match _cons(dim, rest_dims) = dimensions
    @assign range_iter = P_RangeIterator.RangeIterator.fromDim(dim)
    while P_RangeIterator.RangeIterator.hasNext(range_iter)
      @assign (range_iter, sub_exp) = P_RangeIterator.RangeIterator.next(range_iter)
      @assign (vars, sections) = flattenArray(
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
    local stop::Integer
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
    local stop::Integer
    local range::Expression
    local body::List{Statement}
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
                P_Pointer.create(P_Component.ITERATOR(
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
  local prefixLength::Integer = stringLength(prefixString)

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
    local binding_level::Integer
    local bind_exp::Expression
    local pars::List{InstNode}
    local par::InstNode
    @match binding begin
      UNBOUND(__) => begin
        binding
      end
      TYPED_BINDING(__) => begin
        if binding.isFlattened
          return
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
  local binding_level::Integer
  local parents::List{InstNode}
  local pre::ComponentRef
  local cr_node::InstNode
  local par::InstNode
  @assign outExp = begin
    @match exp begin
      BINDING_EXP(exp = outExp) => begin
        @assign parents = listRest(exp.parents)
        if !exp.isEach
          if isTypeAttribute && !listEmpty(parents)
            @assign parents = listRest(parents)
          end
          if !listEmpty(parents)
            @assign outExp = flattenBindingExp2(outExp, prefix, parents)
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
  exp::Expression,
  prefix::ComponentRef,
  parents::List{<:InstNode},
)::Expression
  local outExp::Expression = exp

  local binding_level::Integer = 0
  local subs::List{Subscript}
  local pre::ComponentRef = prefix
  local pre_node::InstNode
  local par::InstNode

  @assign par = listHead(parents)
  if isComponent(par) && !isEmpty(pre)
    @assign pre_node = node(pre)
    while !refEqual(pre_node, par)
      @assign pre = rest(pre)
      if isEmpty(pre)
        return outExp
      end
      @assign pre_node = node(pre)
    end
  end
  for parent in parents
    @assign binding_level = binding_level + Type.dimensionCount(getType(parent))
  end
  if binding_level > 0
    @assign subs =
      listAppend(listReverse(s) for s in subscriptsAll(pre))
    @assign binding_level = min(binding_level, listLength(subs))
    @assign subs = ListUtil.firstN_reverse(subs, binding_level)
    @assign outExp = applySubscripts(subs, exp)
  end
  #=  TODO: Optimize this, making a list of all subscripts in the prefix when
  =#
  #=        only a few are needed is unnecessary.
  =#
  return outExp
end

function flattenExp(exp::Expression, prefix::ComponentRef)::Expression
  @assign exp =
    map(exp, (x) -> flattenExp_traverse(x,prefix))
  return exp
end

function flattenExp_traverse(exp::Expression, prefix::ComponentRef)::Expression

  @assign exp = begin
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
  end
  return exp
end

function flattenSections(
  sections::Sections,
  prefix::ComponentRef,
  accumSections::Sections,
)::Sections

  @assign () = begin
    local eq::List{Equation}
    local ieq::List{Equation}
    local alg::List{Algorithm}
    local ialg::List{Algorithm}
    @match sections begin
      SECTIONS(__) => begin
        @assign eq = flattenEquations(sections.equations, prefix)
        @assign ieq = flattenEquations(sections.initialEquations, prefix)
        @assign alg = flattenAlgorithms(sections.algorithms, prefix)
        @assign ialg = flattenAlgorithms(sections.initialAlgorithms, prefix)
        @assign accumSections =
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

function flattenEquations(eql::List{<:Equation}, prefix::ComponentRef)::List{Equation}
  local equations::List{Equation} = nil

  for eq in eql
    @assign equations = flattenEquation(eq, prefix, equations)
  end
  return equations
end

function flattenEquation(
  eq::Equation,
  prefix::ComponentRef,
  equations::List{<:Equation},
)::List{Equation}

  @assign equations = begin
    local e1::Expression
    local e2::Expression
    local e3::Expression
    local eql::List{Equation}
    @match eq begin
      EQUATION_EQUALITY(__) => begin
        @assign e1 = flattenExp(eq.lhs, prefix)
        @assign e2 = flattenExp(eq.rhs, prefix)
        _cons(EQUATION_EQUALITY(e1, e2, eq.ty, eq.source), equations)
      end

      EQUATION_FOR(__) => begin
        if Flags.isSet(Flags.NF_SCALARIZE)
          @assign eql = unrollForLoop(eq, prefix, equations)
        else
          @assign eql = splitForLoop(eq, prefix, equations)
        end
        eql
      end

      EQUATION_CONNECT(__) => begin
        @assign e1 = flattenExp(eq.lhs, prefix)
        @assign e2 = flattenExp(eq.rhs, prefix)
        _cons(EQUATION_CONNECT(e1, e2, eq.source), equations)
      end

      EQUATION_IF(__) => begin
        flattenIfEquation(eq, prefix, equations)
      end

      EQUATION_WHEN(__) => begin
        @assign eq.branches = list(flattenEqBranch(b, prefix) for b in eq.branches)
        _cons(eq, equations)
      end

      EQUATION_ASSERT(__) => begin
        @assign e1 = flattenExp(eq.condition, prefix)
        @assign e2 = flattenExp(eq.message, prefix)
        @assign e3 = flattenExp(eq.level, prefix)
        _cons(EQUATION_ASSERT(e1, e2, e3, eq.source), equations)
      end

      EQUATION_TERMINATE(__) => begin
        @assign e1 = flattenExp(eq.message, prefix)
        _cons(EQUATION_TERMINATE(e1, eq.source), equations)
      end

      EQUATION_REINIT(__) => begin
        @assign e1 = flattenExp(eq.cref, prefix)
        @assign e2 = flattenExp(eq.reinitExp, prefix)
        _cons(EQUATION_REINIT(e1, e2, eq.source), equations)
      end

      EQUATION_NORETCALL(__) => begin
        @assign e1 = flattenExp(eq.exp, prefix)
        _cons(EQUATION_NORETCALL(e1, eq.source), equations)
      end
      _ => begin
        _cons(eq, equations)
      end
    end
  end
  return equations
end

function flattenIfEquation(
  eq::Equation,
  prefix::ComponentRef,
  equations::List{<:Equation},
)::List{Equation}

  local branch::Equation_Branch
  local branches::List{Equation_Branch}
  local bl::List{Equation_Branch} = nil
  local cond::Expression
  local eql::List{Equation}
  local var::VariabilityType
  local has_connect::Bool
  local src::DAE.ElementSource
  local info::SourceInfo
  local target::EvalTarget

  @match EQUATION_IF(branches = branches, source = src) = eq
  @assign has_connect = contains(eq, isConnectEq)
  #=  Print errors for unbound constants/parameters if the if-equation contains
  =#
  #=  connects, since we must select a branch in that case.
  =#
  @assign target = if has_connect
    GENERIC(info(eq))
  else
    IGNORE_ERRORS()
  end
  while !listEmpty(branches)
    @match _cons(branch, branches) = branches
    @assign bl = begin
      @match branch begin
        EQUATION_BRANCH(cond, var, eql) => begin
          #=  Flatten the condition and body of the branch.
          =#
          @assign cond = flattenExp(cond, prefix)
          @assign eql = flattenEquations(eql, prefix)
          #=  Evaluate structural conditions.
          =#
          if var <= Variability.STRUCTURAL_PARAMETER
            @assign cond = evalExp(cond, target)
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
          #=  Conditions in an if-equation that contains connects must be possible to evaluate.
          =#
          if isTrue(cond)
            @assign branches = nil
            if listEmpty(bl)
              @assign equations = listAppend(eql, equations)
            else
              @assign bl = _cons(
                  makeBranch(cond, listReverseInPlace(eql), var),
                bl,
              )
            end
          elseif !isFalse(cond)
            @assign bl = _cons(
                makeBranch(cond, listReverseInPlace(eql), var),
              bl,
            )
          end
          #=  The condition is true and the branch will thus always be selected
          =#
          #=  if reached, so we can discard the remaining branches.
          =#
          #=  If we haven't collected any other branches yet, replace the if-equation with this branch.
          =#
          #=  Otherwise, append this branch.
          =#
          #=  Only add the branch to the list of branches if the condition is not
          =#
          #=  literal false, otherwise just drop it since it will never trigger.
          =#
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
            @assign cond = evalExp(cond, target)
          end
          if !isFalse(cond)
            triggerErrors(branch)
          end
          bl
        end

        _ => begin
          _cons(branch, bl)
        end
      end
    end
  end
  #=  Add the flattened if-equation to the list of equations if there are any
  =#
  #=  branches still remaining.
  =#
  if !listEmpty(bl)
    @assign equations =
      _cons(EQUATION_IF(listReverseInPlace(bl), src), equations)
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
  branch::Equation_Branch,
  prefix::ComponentRef,
)::Equation_Branch

  local exp::Expression
  local eql::List{Equation}
  local var::VariabilityType

  @match EQUATION_BRANCH(exp, var, eql) = branch
  @assign exp = flattenExp(exp, prefix)
  @assign eql = flattenEquations(eql, prefix)
  @assign branch = makeBranch(exp, listReverseInPlace(eql), var)
  return branch
end

function unrollForLoop(
  forLoop::Equation,
  prefix::ComponentRef,
  equations::List{<:Equation},
)::List{Equation}

  local iter::InstNode
  local body::List{Equation}
  local unrolled_body::List{Equation}
  local range::Expression
  local range_iter::RangeIterator
  local val::Expression

  @match EQUATION_FOR(iterator = iter, range = SOME(range), body = body) =
    forLoop
  #=  Unroll the loop by replacing the iterator with each of its values in the for loop body.
  =#
  @assign range = flattenExp(range, prefix)
  @assign range =
    Ceval.evalExp(range, Ceval.P_EvalTarget.RANGE(P_Equation.Equation.info(forLoop)))
  @assign range_iter = P_RangeIterator.RangeIterator.fromExp(range)
  while P_RangeIterator.RangeIterator.hasNext(range_iter)
    @assign (range_iter, val) = P_RangeIterator.RangeIterator.next(range_iter)
    @assign unrolled_body = mapExpList(
      body,
      (iter, val) ->
        replaceIterator(iterator = iter, iteratorValue = val),
    )
    @assign unrolled_body = flattenEquations(unrolled_body, prefix)
    @assign equations = listAppend(unrolled_body, equations)
  end
  return equations
end

function splitForLoop(
  forLoop::Equation,
  prefix::ComponentRef,
  equations::List{<:Equation},
)::List{Equation}

  local iter::InstNode
  local range::Option{Expression}
  local body::List{Equation}
  local connects::List{Equation}
  local non_connects::List{Equation}
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

function splitForLoop2(forBody::List{<:Equation})::Tuple{List{Equation}, List{Equation}}
  local nonConnects::List{Equation} = nil
  local connects::List{Equation} = nil

  local conns::List{Equation}
  local nconns::List{Equation}

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
  algorithms::List{<:Algorithm},
  prefix::ComponentRef,
)::List{Algorithm}
  local outAlgorithms::List{Algorithm} = nil

  for alg in algorithms
    @assign alg.statements = P_Statement.Statement.mapExpList(
      alg.statements,
      (prefix) -> flattenExp(prefix = prefix),
    )
    if hasSubscripts(prefix)
      @assign alg.source = addElementSourceArrayPrefix(alg.source, prefix)
    end
    @assign outAlgorithms = _cons(alg, outAlgorithms)
  end
  #=  CheckModel relies on the ElementSource to know whether a certain algorithm comes from
  =#
  #=  an array component, otherwise is will miscount the number of equations.
  =#
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

""" #= Generates the connect equations and adds them to the equation list =#"""
function resolveConnections(flatModel::FlatModel, name::String)::FlatModel
  local conns::Connections
  local conn_eql::List{Equation}
  local csets::ConnectionSets.Sets
  local csets_array::Array{List{Connector}}
  local ctable::CardinalityTable.Table
  local broken::BrokenEdges = nil
  #=  get the connections from the model
  =#
  @assign (flatModel, conns) = collect(flatModel)
  return flatModel
  #= Let's ignore the stuff below for now=#

  #=  Elaborate expandable connectors.
  =#
  @assign (flatModel, conns) = elaborate(flatModel, conns)
  #=  handle overconstrained connections
  =#
  #=  - build the graph
  =#
  #=  - evaluate the Connections.* operators
  =#
  #=  - generate the equations to replace the broken connects
  =#
  #=  - return the broken connects + the equations
  =#

  # if System.getHasOverconstrainedConnectors()
  #   @assign (flatModel, broken) =
  #     NFOCConnectionGraph.handleOverconstrainedConnections(flatModel, conns, name)
  # TODO
  # end
  #=  add the broken connections
  =#
  @assign conns = P_Connections.Connections.addBroken(broken, conns)
  #=  build the sets, check the broken connects
  =#
  @assign csets = ConnectionSets.fromConnections(conns)
  @assign csets_array = ConnectionSets.extractSets(csets)
  #=  generate the equations
  =#
  @assign conn_eql = ConnectEquations.generateEquations(csets_array)
  #=  append the equalityConstraint call equations for the broken connects
  =#
  if System.getHasOverconstrainedConnectors()
    @assign conn_eql =
      listAppend(conn_eql, ListUtil.flatten(ListUtil.map(broken, Util.tuple33)))
  end
  #=  add the equations to the flat model
  =#
  @assign flatModel.equations = listAppend(conn_eql, flatModel.equations)
  @assign flatModel.variables =
    list(v for v in flatModel.variables if P_Variable.Variable.isPresent(v))
  @assign ctable = CardinalityTable.fromConnections(conns)
  #=  Evaluate any connection operators if they're used.
  =#
  if System.getHasStreamConnectors() || System.getUsesCardinality()
    @assign flatModel = evaluateConnectionOperators(flatModel, csets, csets_array, ctable)
  end
  execStat(getInstanceName() + "(" + name + ")")
  return flatModel
end

function evaluateConnectionOperators(
  flatModel::FlatModel,
  sets#=::ConnectionSets.Sets=#,
  setsArray::Array{<:List{<:Connector}},
  ctable::CardinalityTable.Table,
)::FlatModel

  @assign flatModel.variables =
    list(evaluateBindingConnOp(c, sets, setsArray, ctable) for c in flatModel.variables)
  @assign flatModel.equations =
    evaluateEquationsConnOp(flatModel.equations, sets, setsArray, ctable)
  @assign flatModel.initialEquations =
    evaluateEquationsConnOp(flatModel.initialEquations, sets, setsArray, ctable)
  #=  TODO: Implement evaluation for algorithm sections.
  =#
  return flatModel
end

function evaluateBindingConnOp(
  var::Variable,
  sets#=::ConnectionSets.Sets=#,
  setsArray::Array{<:List{<:Connector}},
  ctable::CardinalityTable.Table,
)::Variable

  local binding::Binding
  local exp::Expression
  local eval_exp::Expression

  @assign () = begin
    @match var begin
      VARIABLE(
        binding = binding && TYPED_BINDING(bindingExp = exp),
      ) => begin
        @assign eval_exp = ConnectEquations.evaluateOperators(exp, sets, setsArray, ctable)
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
  equations::List{<:Equation},
  sets#=::ConnectionSets.Sets=#,
  setsArray::Array{<:List{<:Connector}},
  ctable::CardinalityTable.Table,
)::List{Equation}

  @assign equations = List(
    P_Equation.Equation.mapExp(
      eq,
      (sets, setsArray, ctable) -> ConnectEquations.evaluateOperators(
        sets = sets,
        setsArray = setsArray,
        ctable = ctable,
      ),
    ) for eq in equations
  )
  return equations
end

function collectComponentFuncs(var::Variable, funcs::FunctionTree)::FunctionTree

  @assign () = begin
    @match var begin
      VARIABLE(__) => begin
        @assign funcs = collectTypeFuncs(var.ty, funcs)
        @assign funcs = collectBindingFuncs(var.binding, funcs)
        for attr in var.typeAttributes
          @assign funcs = collectBindingFuncs(Util.tuple22(attr), funcs)
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
        @assign funcs = P_Dimension.Dimension.foldExpList(
          ty.dimensions,
          collectExpFuncs_traverse,
          funcs,
        )
        @assign funcs = collectTypeFuncs(ty.elementType, funcs)
        ()
      end

      TYPE_FUNCTION(fn = fn) => begin
        @assign funcs = flattenFunction(fn, funcs)
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

function collectEquationFuncs(eq::Equation, funcs::FunctionTree)::FunctionTree

  @assign () = begin
    @match eq begin
      EQUATION_EQUALITY(__) => begin
        @assign funcs = collectExpFuncs(eq.lhs, funcs)
        @assign funcs = collectExpFuncs(eq.rhs, funcs)
        @assign funcs = collectTypeFuncs(eq.ty, funcs)
        ()
      end

      EQUATION_ARRAY_EQUALITY(__) => begin
        #=  Lhs is always a cref, no need to check it.
        =#
        @assign funcs = collectExpFuncs(eq.rhs, funcs)
        @assign funcs = collectTypeFuncs(eq.ty, funcs)
        ()
      end

      EQUATION_FOR(__) => begin
        #=  For equations are always unrolled, so functions in the range doesn't
        =#
        #=  matter since they are always evaluated.
        =#
        @assign funcs = ListUtil.fold(eq.body, collectEquationFuncs, funcs)
        ()
      end

      EQUATION_IF(__) => begin
        @assign funcs = ListUtil.fold(eq.branches, collectEqBranchFuncs, funcs)
        ()
      end

      EQUATION_WHEN(__) => begin
        @assign funcs = ListUtil.fold(eq.branches, collectEqBranchFuncs, funcs)
        ()
      end

      EQUATION_ASSERT(__) => begin
        @assign funcs = collectExpFuncs(eq.condition, funcs)
        @assign funcs = collectExpFuncs(eq.message, funcs)
        @assign funcs = collectExpFuncs(eq.level, funcs)
        ()
      end

      EQUATION_TERMINATE(__) => begin
        @assign funcs = collectExpFuncs(eq.message, funcs)
        ()
      end

      EQUATION_REINIT(__) => begin
        @assign funcs = collectExpFuncs(eq.reinitExp, funcs)
        ()
      end

      EQUATION_NORETCALL(__) => begin
        @assign funcs = collectExpFuncs(eq.exp, funcs)
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
  branch::Equation_Branch,
  funcs::FunctionTree,
)::FunctionTree
  @assign () = begin
    @match branch begin
      EQUATION_BRANCH(__) => begin
        @assign funcs = collectExpFuncs(branch.condition, funcs)
        @assign funcs = ListUtil.fold(branch.body, collectEquationFuncs, funcs)
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
  @assign funcs = ListUtil.fold(alg.statements, collectStatementFuncs, funcs)
  return funcs
end

function collectStatementFuncs(stmt::Statement, funcs::FunctionTree)::FunctionTree
  @assign () = begin
    @match stmt begin
      P_Statement.Statement.ASSIGNMENT(__) => begin
        @assign funcs = collectExpFuncs(stmt.lhs, funcs)
        @assign funcs = collectExpFuncs(stmt.rhs, funcs)
        @assign funcs = collectTypeFuncs(stmt.ty, funcs)
        ()
      end

      P_Statement.Statement.FOR(__) => begin
        @assign funcs = ListUtil.fold(stmt.body, collectStatementFuncs, funcs)
        @assign funcs = collectExpFuncs(Util.getOption(stmt.range), funcs)
        ()
      end

      P_Statement.Statement.IF(__) => begin
        @assign funcs = ListUtil.fold(stmt.branches, collectStmtBranchFuncs, funcs)
        ()
      end

      P_Statement.Statement.WHEN(__) => begin
        @assign funcs = ListUtil.fold(stmt.branches, collectStmtBranchFuncs, funcs)
        ()
      end

      P_Statement.Statement.ASSERT(__) => begin
        @assign funcs = collectExpFuncs(stmt.condition, funcs)
        @assign funcs = collectExpFuncs(stmt.message, funcs)
        @assign funcs = collectExpFuncs(stmt.level, funcs)
        ()
      end

      P_Statement.Statement.TERMINATE(__) => begin
        @assign funcs = collectExpFuncs(stmt.message, funcs)
        ()
      end

      P_Statement.Statement.NORETCALL(__) => begin
        @assign funcs = collectExpFuncs(stmt.exp, funcs)
        ()
      end

      P_Statement.Statement.WHILE(__) => begin
        @assign funcs = collectExpFuncs(stmt.condition, funcs)
        @assign funcs = ListUtil.fold(stmt.body, collectStatementFuncs, funcs)
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
  branch::Tuple{<:Expression, List{<:Statement}},
  funcs::FunctionTree,
)::FunctionTree

  @assign funcs = collectExpFuncs(Util.tuple21(branch), funcs)
  @assign funcs = ListUtil.fold(Util.tuple22(branch), collectStatementFuncs, funcs)
  return funcs
end

function collectExpFuncs(exp::Expression, funcs::FunctionTree)::FunctionTree
  @assign funcs = fold(exp, collectExpFuncs_traverse, funcs)
  return funcs
end

function collectExpFuncs_traverse(exp::Expression, funcs::FunctionTree)::FunctionTree

  @assign () = begin
    local fn::M_Function
    @match exp begin
      CALL_EXPRESSION(__) => begin
        @assign funcs = flattenFunction(typedFunction(exp.call), funcs)
        ()
      end

      CREF_EXPRESSION(__) => begin
        @assign funcs = collectTypeFuncs(exp.ty, funcs)
        ()
      end

RECORD_EXPRESSION(__) => begin
        @assign funcs = collectTypeFuncs(exp.ty, funcs)
        ()
      end

      PARTIAL_FUNCTION_APPLICATION_EXPRESSION(__) => begin
        for f in P_Function.getRefCache(exp.fn)
          @assign funcs = flattenFunction(f, funcs)
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

function flattenFunction(@nospecialize(func::M_Function), @nospecialize(funcs::FunctionTree))::FunctionTree

  local fn::M_Function = func

  if !isCollected(fn)
    @assign fn = EvalConstants.evaluateFunction(fn)
    SimplifyModel.simplifyFunction(fn)
    P_Function.collect(fn)
    if !isPartial(fn.node)
      @assign funcs = FunctionTree.add(funcs, P_Function.name(fn), fn)
      @assign funcs = collectClassFunctions(fn.node, funcs)
      for fn_der in fn.derivatives
        for der_fn in getCachedFuncs(fn_der.derivativeFn)
          @assign funcs = flattenFunction(der_fn, funcs)
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
        @assign () = begin
          @match sections begin
            P_Sections.Sections.SECTIONS(__) => begin
              @assign funcs =
                ListUtil.fold(sections.algorithms, collectAlgorithmFuncs, funcs)
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
