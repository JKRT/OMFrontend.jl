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
Dimension = NFDimension
Equation = NFEquation
Expression = NFExpression
Statement = NFStatement
Operator = NFOperator
Connector = NFConnector
Connection = NFConnection
ComponentRef = NFComponentRef
Subscript = NFSubscript
ComplexType = NFComplexType
Restriction = NFRestriction

@UniontypeDecl TypingError
function isError(error::TypingError)::Bool
  local isError::Bool
  @assign isError = begin
    @match error begin
      NO_ERROR(__) => begin
        false
      end
      _ => begin
        true
      end
    end
  end
  return isError
end

@Uniontype TypingError begin
  @Record OUT_OF_BOUNDS begin
    upperBound::Integer
  end
  @Record NO_ERROR begin
  end
end

ORIGIN_Type = Integer
M_Type = Integer
#=  Flag values: =#
const ORIGIN_CLASS = 0
#=  In class.=#
const ORIGIN_FUNCTION = intBitLShift(1, 0)::M_Type
#=  In function.=#
const ORIGIN_ALGORITHM = intBitLShift(1, 1)::M_Type
#=  In algorithm section.=#
const ORIGIN_EQUATION = intBitLShift(1, 2)::M_Type
#=  In equation section.=#
const ORIGIN_INITIAL = intBitLShift(1, 3)::M_Type
#=  In initial section.=#
const ORIGIN_LHS = intBitLShift(1, 4)::M_Type
#=  On left hand side of equality/assignment.=#
const ORIGIN_RHS = intBitLShift(1, 5)::M_Type
#=  On right hand side of equality/assignment.=#
const ORIGIN_WHEN = intBitLShift(1, 6)::M_Type
#=  In when equation/statement.=#
const ORIGIN_CLOCKED = intBitLShift(1, 7)::M_Type
#=  Part of a clocked when equation.=#
const ORIGIN_FOR = intBitLShift(1, 8)::M_Type
#=  In a for loop.=#
const ORIGIN_IF = intBitLShift(1, 9)::M_Type
#=  In an if equation/statement.=#
const ORIGIN_WHILE = intBitLShift(1, 10)::M_Type
#=  In a while loop.=#
const ORIGIN_NONEXPANDABLE = intBitLShift(1, 11)::M_Type
#=  In non-parameter if/for.=#
const ORIGIN_ITERATION_RANGE = intBitLShift(1, 12)::M_Type
#=  In range used for iteration.=#
const ORIGIN_DIMENSION = intBitLShift(1, 13)::M_Type
#=  In dimension.=#
const ORIGIN_BINDING = intBitLShift(1, 14)::M_Type
#=  In binding.=#
const ORIGIN_CONDITION = intBitLShift(1, 15)::M_Type
#=  In conditional expression.=#
const ORIGIN_SUBSCRIPT = intBitLShift(1, 16)::M_Type
#=  In subscript.=#
const ORIGIN_SUBEXPRESSION = intBitLShift(1, 17)::M_Type
#=  Part of a larger expression.=#
const ORIGIN_CONNECT = intBitLShift(1, 18)::M_Type
#=  Part of connect argument.=#
const ORIGIN_NOEVENT = intBitLShift(1, 19)::M_Type
#=  Part of noEvent argument.=#
const ORIGIN_ASSERT = intBitLShift(1, 20)::M_Type

const ORIGIN_EQ_SUBEXPRESSION = intBitOr(ORIGIN_EQUATION, ORIGIN_SUBEXPRESSION)::M_Type
const ORIGIN_VALIDNAME_SCOPE = intBitOr(ORIGIN_ITERATION_RANGE, ORIGIN_DIMENSION)::M_Type
const ORIGIN_DISCRETE_SCOPE = intBitOr(ORIGIN_WHEN, intBitOr(ORIGIN_INITIAL, ORIGIN_FUNCTION))::M_Type

""" #= Returns true if the given origin indicates the expression is alone on
     either side of an equality/assignment. =#"""
function isSingleExpression(origin::M_Type)::Bool
  local isSingle::Bool = origin < ITERATION_RANGE - 1
  return isSingle
end

function setFlag(origin, flag)
  local newOrigin
  @assign newOrigin = intBitOr(origin, flag)
  return newOrigin
end

function flagSet(origin::M_Type, flag::M_Type)::Bool
  local set::Bool
  @assign set = intBitAnd(origin, flag) > 0
  return set
end

function flagNotSet(origin::M_Type, flag::M_Type)::Bool
  local notSet::Bool
  @assign notSet = intBitAnd(origin, flag) == 0
  return notSet
end

function typeClass(cls::InstNode, name::String)
  typeClassType(cls, EMPTY_BINDING, ORIGIN_CLASS, cls)
  typeComponents(cls, ORIGIN_CLASS)
#  execStat("NFtypeComponents(" + name + ")")
  typeBindings(cls, cls, ORIGIN_CLASS)
#  execStat("NFTyping.typeBindings(" + name + ")")
  typeClassSections(cls, ORIGIN_CLASS)
  #execStat("NFTyping.typeClassSections(" + name + ")")
  return
end

function typeComponents(cls::InstNode, origin::ORIGIN_Type)
  local c::Class = getClass(cls)
  local c2::Class
  local cls_tree::ClassTree
  local ext_node::InstNode
  local con::InstNode
  local de::InstNode

  return @assign () = begin
    @match c begin
      INSTANCED_CLASS(restriction = RESTRICTION_TYPE(__)) => begin
        ()
      end

      INSTANCED_CLASS(elements = cls_tree && CLASS_TREE_FLAT_TREE(__)) => begin
        for c in cls_tree.components
          typeComponent(c, origin)
        end
        @assign () = begin
          @match c.ty begin
            TYPE_COMPLEX(complexTy = COMPLEX_RECORD(constructor = con)) => begin
              typeStructor(con)
              ()
            end

            _ => begin
              ()
            end
          end
        end
        ()
      end

      TYPED_DERIVED(ty = ARRAY_TYPE(__)) => begin
        #=  For derived types with dimensions we keep them as they are, because we
        =#
        #=  need to preserve the dimensions.
        =#
        typeComponents(c.baseClass, origin)
        ()
      end

      TYPED_DERIVED(__) => begin
        #=  Derived types without dimensions can be collapsed.
        =#
        typeComponents(c.baseClass, origin)
        @assign c2 = getClass(c.baseClass)
        @assign c2 = setRestriction(c.restriction, c2)
        updateClass(c2, cls)
        ()
      end

      INSTANCED_BUILTIN(
        ty = TYPE_COMPLEX(
          complexTy = COMPLEX_EXTERNAL_OBJECT(constructor = con, destructor = de),
        ),
      ) => begin
        typeStructor(con)
        typeStructor(de)
        ()
      end

      INSTANCED_BUILTIN(__) => begin
        ()
      end

      _ => begin
        Error.assertion(
          false,
          getInstanceName() + " got uninstantiated class " + name(cls),
          sourceInfo(),
        )
        fail()
      end
    end
  end
end

function typeStructor(node::InstNode)
  local cache::CachedData
  local fnl::List{M_Function}

  @assign cache = getFuncCache(node)
  return @assign () = begin
    @match cache begin
      C_FUNCTION(funcs = fnl, typed = false) => begin
        @assign fnl = List(P_Function.typeFunction(fn) for fn in fnl)
        @assign fnl = List(
          patchOperatorRecordConstructorBinding(fn) for fn in fnl
        )
        setFuncCache(
          node,
          C_FUNCTION(fnl, true, cache.specialBuiltin),
        )
        ()
      end

      _ => begin
        ()
      end
    end
  end
end

function typeClassType(
  clsNode::InstNode,
  componentBinding::Binding,
  origin::ORIGIN_Type,
  instanceNode::InstNode,
)::NFType
  local ty::NFType

  local cls::Class
  local ty_cls::Class
  local node::InstNode
  local ty_node::InstNode
  local fn::M_Function
  local is_expandable::Bool

  @assign cls = getClass(clsNode)
  @assign ty = begin
    @match cls begin
      INSTANCED_CLASS(
        restriction = RESTRICTION_CONNECTOR(isExpandable = is_expandable),
      ) => begin
        @assign ty = TYPE_COMPLEX(clsNode, makeConnectorType(cls.elements, is_expandable))
        @assign cls.ty = ty
        updateClass(cls, clsNode)
        ty
      end

      INSTANCED_CLASS(
        ty = TYPE_COMPLEX(
          cls = ty_node,
          complexTy = COMPLEX_RECORD(constructor = node),
        ),
      ) => begin
        @assign ty = TYPE_COMPLEX(ty_node, makeRecordType(node))
        @assign cls.ty = ty
        updateClass(cls, clsNode)
        ty
      end

      INSTANCED_CLASS(
        ty = TYPE_COMPLEX(complexTy = COMPLEX_EXTENDS_TYPE(node)),
      ) => begin
        #=  A long class declaration of a type extending from a type has the type of the base class.
        =#
        @assign ty = typeClassType(node, componentBinding, origin, instanceNode)
        @assign cls.ty = ty
        updateClass(cls, clsNode)
        ty
      end

      INSTANCED_CLASS(
        restriction = RESTRICTION_FUNCTION(__),
      ) where {(isComponent(instanceNode))} => begin
        #=  A component of function type, i.e. a functional input parameter.
        =#
        @match _cons(fn, _) = P_Function.typeNodeCache(clsNode)
        if !P_Function.isPartial(fn)
          Error.addSourceMessage(
            Error.META_FUNCTION_NO_PARTIAL_PREFIX,
            list(AbsynUtil.pathString(P_Function.name(fn))),
            info(instanceNode),
          )
          fail()
        end
        @assign ty = TYPE_FUNCTION(fn, FunctionType.FUNCTIONAL_PARAMETER)
        @assign cls.ty = ty
        updateClass(cls, clsNode)
        ty
      end

      INSTANCED_CLASS(__) => begin
        cls.ty
      end

      EXPANDED_DERIVED(__) => begin
        typeDimensions(cls.dims, clsNode, componentBinding, origin, info(clsNode))
        @assign ty = typeClassType(cls.baseClass, componentBinding, origin, instanceNode)
        @assign ty = Type.liftArrayLeftList(ty, arrayList(cls.dims))
        @assign ty_cls = TYPED_DERIVED(ty, cls.baseClass, cls.restriction)
        updateClass(ty_cls, clsNode)
        ty
      end

      INSTANCED_BUILTIN(__) => begin
        cls.ty
      end

      TYPED_DERIVED(__) => begin
        cls.ty
      end

      _ => begin
        Error.assertion(
          false,
          getInstanceName() + " got noninstantiated class " + name(clsNode),
          sourceInfo(),
        )
        fail()
      end
    end
  end
  return ty
end

function makeConnectorType(ctree::ClassTree, isExpandable::Bool)::ComplexType
  local connectorTy::ComplexType

  local pots::List{InstNode} = nil
  local flows::List{InstNode} = nil
  local streams::List{InstNode} = nil
  local exps::List{InstNode} = nil
  local cty::ConnectorType.TYPE

  if isExpandable
    for c in enumerateComponents(ctree)
      @assign cty = connectorType(component(c))
      if intBitAnd(cty, ConnectorType.EXPANDABLE) > 0
        @assign exps = _cons(c, exps)
      else
        @assign pots = _cons(c, pots)
      end
    end
    @assign connectorTy = ComplexType.EXPANDABLE_CONNECTOR(pots, exps)
  else
    for c in enumerateComponents(ctree)
      @assign cty = P_Component.connectorType(component(c))
      if intBitAnd(cty, ConnectorType.FLOW) > 0
        @assign flows = _cons(c, flows)
      elseif intBitAnd(cty, ConnectorType.STREAM) > 0
        @assign streams = _cons(c, streams)
      elseif intBitAnd(cty, ConnectorType.POTENTIAL) > 0
        @assign pots = _cons(c, pots)
      else
        Error.addInternalError(
          "Invalid connector type on component " + name(c),
          info(c),
        )
        fail()
      end
    end
    @assign connectorTy = ComplexType.CONNECTOR(pots, flows, streams)
    if !listEmpty(streams)
      System.setHasStreamConnectors(true)
    end
  end
  return connectorTy
end

function makeRecordType(constructor::InstNode)::ComplexType
  local recordTy::ComplexType

  local cache::CachedData
  local fn::M_Function
  local fields::List{Record.P_Field}

  @assign cache = getFuncCache(constructor)
  @assign recordTy = begin
    @match cache begin
      C_FUNCTION(funcs = fn <| _) => begin
        @assign fields = Record.collectRecordFields(fn.node)
        COMPLEX_RECORD(constructor, fields)
      end

      _ => begin
        Error.assertion(
          false,
          getInstanceName() + " got record type without constructor",
          sourceInfo(),
        )
        fail()
      end
    end
  end
  return recordTy
end

function typeComponent(inComponent::InstNode, origin::ORIGIN_Type)::NFType
  local ty::NFType
  local node::InstNode = resolveOuter(inComponent)
  local c::Component = component(node)

  @assign ty = begin
    @match c begin
      UNTYPED_COMPONENT(__) => begin
        #=  An untyped component, type it.
        =#
        #=  Type the component's dimensions.
        =#
        typeDimensions(c.dimensions, node, c.binding, origin, c.info)
        #=  Construct the type of the component and update the node with it.
        =#
        @assign ty = typeClassType(c.classInst, c.binding, origin, inComponent)
        @assign ty = liftArrayLeftList(ty, arrayList(c.dimensions))
        updateComponent(setType(ty, c), node)
        #=  Check that flow/stream variables are Real.
        =#
        checkComponentStreamAttribute(c.attributes.connectorType, ty, inComponent)
        #=  Type the component's children.
        =#
        typeComponents(c.classInst, origin)
        ty
      end
      TYPED_COMPONENT(__) => begin
        c.ty
      end

      ITERATOR_COMPONENT(__) => begin
        c.ty
      end

      ENUM_LITERAL_COMPONENT(literal = ENUM_LITERAL_EXPRESSION(ty = ty)) =>
        begin
          ty
        end

      _ => begin
        #=  A component that has already been typed, skip it.
        =#
        #=  Any other type of component shouldn't show up here.
        =#
        Error.assertion(
          false,
          getInstanceName() +
          " got noninstantiated component " +
          name(component),
          sourceInfo(),
        )
        fail()
      end
    end
  end
  return ty
end

function checkComponentStreamAttribute(
  cty::Integer,
  ty::NFType,
  component::InstNode,
)
  local ety::NFType

  return if isFlowOrStream(cty)
    @assign ety = arrayElementType(ty)
    if !(isReal(ety) || isComplex(ety))
      Error.addSourceMessageAndFail(
        Error.NON_REAL_FLOW_OR_STREAM,
        list(ConnectorType.toString(cty), name(component)),
        info(component),
      )
    end
  end
end

function checkConnectorType(node::InstNode)::Bool
  local isConnector::Bool

  local dnode::InstNode = getDerivedNode(node)

  if isEmpty(dnode) || isInnerOuterNode(dnode)
    @assign isConnector = false
  else
    @assign isConnector =
      isConnectorClass(getClass(dnode)) ||
      checkConnectorType(parent(dnode))
  end
  return isConnector
end

function typeIterator(
  iterator::InstNode,
  range::Expression,
  origin::ORIGIN_Type,
  structural::Bool,
)::Tuple{Expression, M_Type, Variability} #= If the iteration range must be a parameter expression or not. =#
  local var::VariabilityType
  local ty::M_Type
  local outRange::Expression

  local c::Component = component(iterator)
  local exp::Expression
  local info::SourceInfo

  @assign (outRange, ty, var) = begin
    @match c begin
      P_Component.ITERATOR(info = info) => begin
        @assign (exp, ty, var) =
          typeExp(range, ORIGIN_setFlag(origin, ORIGIN_ITERATION_RANGE), info)
        #=  If the iteration range is structural, it must be a parameter expression.
        =#
        if structural && var > Variability.PARAMETER
          Error.addSourceMessageAndFail(
            Error.NON_PARAMETER_ITERATOR_RANGE,
            list(toString(exp)),
            info,
          )
        end
        #=  The iteration range must be a vector expression.
        =#
        if !Type.isVector(ty)
          Error.addSourceMessageAndFail(
            Error.FOR_EXPRESSION_ERROR,
            list(toString(exp), Type.toString(ty)),
            info,
          )
        end
        #=  The type of the iterator is the element type of the range expression.
        =#
        @assign c = P_Component.ITERATOR(arrayElementType(ty), var, info)
        updateComponent(c, iterator)
        (exp, ty, var)
      end

      _ => begin
        Error.assertion(
          false,
          getInstanceName() + " got non-iterator " + name(iterator),
          sourceInfo(),
        )
        fail()
      end
    end
  end
  return (outRange, ty, var)
end

function typeDimensions(
  dimensions::Array{<:Dimension},
  component::InstNode,
  binding::Binding,
  origin::ORIGIN_Type,
  info::SourceInfo,
)::Array{Dimension}

  for i = 1:arrayLength(dimensions)
    typeDimension(dimensions, i, component, binding, origin, info)
  end
  return dimensions
end

function typeDimension(
  dimensions::Array{<:Dimension},
  index::Integer,
  component::InstNode,
  binding::Binding,
  origin::ORIGIN_Type,
  info::SourceInfo,
)::Dimension
  local dimension::Dimension = dimensions[index]

  @assign dimension = begin
    local exp::Expression
    local oexp::Option{Expression}
    local var::VariabilityType
    local dim::Dimension
    local b::Binding
    local ty::M_Type
    local ty_err::TypingError
    local parent_dims::Integer
    local dim_index::Integer
    #=  Print an error when a dimension that's currently being processed is
    =#
    #=  found, which indicates a dependency loop. Another way of handling this
    =#
    #=  would be to instead view the dimension as unknown and infer it from the
    =#
    #=  binding, which means that things like x[size(x, 1)] = {...} could be
    =#
    #=  handled. But that is not specified and doesn't seem needed, and can also
    =#
    #=  give different results depending on the declaration order of components.
    =#
    @match dimension begin
      P_Dimension.Dimension.UNTYPED(isProcessing = true) => begin
        #=  Only give an error if we're not in a function.
        =#
        if ORIGIN_flagNotSet(origin, ORIGIN_FUNCTION)
          Error.addSourceMessage(
            Error.CYCLIC_DIMENSIONS,
            list(
              String(index),
              name(component),
              toString(dimension.dimension),
            ),
            info,
          )
          fail()
        end
        #=  TODO: Tell the user which variables are involved in the loop (can be
        =#
        #=        found with DFS on the dimension expression. Maybe have a limit
        =#
        #=        on the output in case there's a lot of dimensions involved.
        =#
        #=  If we are in a functions we allow e.g. size expression of unknown dimensions.
        =#
        @assign dim = P_Dimension.Dimension.UNKNOWN()
        arrayUpdate(dimensions, index, dim)
        dim
      end

      P_Dimension.Dimension.UNTYPED(__) => begin
        #=  If the dimension is not typed, type it.
        =#
        arrayUpdate(
          dimensions,
          index,
          P_Dimension.Dimension.UNTYPED(dimension.dimension, true),
        )
        @assign (exp, ty, var) = typeExp(
          dimension.dimension,
          ORIGIN_setFlag(origin, ORIGIN_DIMENSION),
          info,
        )
        checkDimensionType(exp, ty, info)
        if ORIGIN_flagNotSet(origin, ORIGIN_FUNCTION)
          if var <= Variability.PARAMETER
            @assign exp = Ceval.evalExp(
              exp,
              Ceval.P_EvalTarget.DIMENSION(component, index, exp, info),
            )
          else
            Error.addSourceMessage(
              Error.DIMENSION_NOT_KNOWN,
              list(toString(exp)),
              info,
            )
            fail()
          end
        else
          if var <= Variability.STRUCTURAL_PARAMETER
            @assign exp = Ceval.evalExp(
              exp,
              Ceval.P_EvalTarget.DIMENSION(component, index, exp, info),
            )
          end
        end
        if !arrayAllEqual(exp)
          Error.addSourceMessage(
            Error.RAGGED_DIMENSION,
            list(toString(exp)),
            info,
          )
          fail()
        end
        @assign dim = P_Dimension.Dimension.fromExp(
          arrayFirstScalar(exp),
          var,
        )
        arrayUpdate(dimensions, index, dim)
        dim
      end

      P_Dimension.Dimension.UNKNOWN(
        __,
      ) where {(ORIGIN_flagSet(origin, ORIGIN_FUNCTION))} => begin
        dimension
      end

      P_Dimension.Dimension.UNKNOWN(__) => begin
        #=  If the dimension is unknown in a function, keep it unknown.
        =#
        #=  If the dimension is unknown in a class, try to infer it from the components binding.
        =#
        @assign b = binding
        @assign parent_dims = 0
        if isUnbound(binding)
          @assign (b, parent_dims) = getRecordElementBinding(component)
          if isUnbound(b)
            @assign parent_dims = 0
            @assign b =
              lookupAttributeBinding("start", getClass(component))
          end
        end
        #=  If the component has no binding, try to use its parent's binding
        =#
        #=  (i.e. for record fields where the record instance has a binding).
        =#
        #=  If the component still doesn't have a binding, try to use the start attribute instead.
        =#
        #=  TODO: Any attribute should actually be fine to use here.
        =#
        @assign (dim, ty_err) = begin
          @match b begin
            UNBOUND(__) => begin
              #=  Print an error if there's no binding.
              =#
              Error.addSourceMessage(
                Error.FAILURE_TO_DEDUCE_DIMS_NO_MOD,
                list(String(index), name(component)),
                info,
              )
              fail()
            end

            UNTYPED_BINDING(__) => begin
              #=  An untyped binding, type the expression only as much as is needed
              =#
              #=  to get the dimension we're looking for.
              =#
              @assign dim_index = index + propagatedDimCount(b) + parent_dims
              @assign (dim, oexp, ty_err) = typeExpDim(
                b.bindingExp,
                dim_index,
                ORIGIN_setFlag(origin, ORIGIN_DIMENSION),
                info,
              )
              #=  If the deduced dimension is unknown, evaluate the binding and try again.
              =#
              if P_Dimension.Dimension.isUnknown(dim) && !P_TypingError.isError(ty_err)
                @assign exp = if isSome(oexp)
                  Util.getOption(oexp)
                else
                  b.bindingExp
                end
                @assign exp = Ceval.evalExp(
                  exp,
                  Ceval.P_EvalTarget.DIMENSION(component, index, exp, info),
                )
                @assign (dim, ty_err) = nthDimensionBoundsChecked(
                  typeOf(exp),
                  dim_index,
                )
              end
              (dim, ty_err)
            end

            TYPED_BINDING(__) => begin
              #=  A typed binding, get the dimension from the binding's type.
              =#
              @assign dim_index = index + parent_dims
              @assign (dim, ty_err) = nthDimensionBoundsChecked(b.bindingType, dim_index)
              #=  If the deduced dimension is unknown, evaluate the binding and try again.
              =#
              if P_Dimension.Dimension.isUnknown(dim) && !P_TypingError.isError(ty_err)
                @assign exp = Ceval.evalExp(
                  b.bindingExp,
                  Ceval.P_EvalTarget.DIMENSION(component, index, b.bindingExp, info),
                )
                @assign (dim, ty_err) = nthDimensionBoundsChecked(
                  typeOf(exp),
                  dim_index,
                )
              end
              (dim, ty_err)
            end
          end
        end
        @assign () = begin
          @match ty_err begin
            P_TypingError.OUT_OF_BOUNDS(__) => begin
              Error.addSourceMessage(
                Error.DIMENSION_DEDUCTION_FROM_BINDING_FAILURE,
                list(String(index), name(component), toString(b)),
                info,
              )
              fail()
            end

            _ => begin
              ()
            end
          end
        end
        #=  Make sure the dimension is constant evaluted, and also mark it as structural.
        =#
        @assign dim = begin
          @match dim begin
            P_Dimension.Dimension.EXP(exp = exp) => begin
              Inst.markStructuralParamsExp(exp)
              @assign exp = Ceval.evalExp(
                exp,
                Ceval.P_EvalTarget.DIMENSION(component, index, exp, info),
              )
              P_Dimension.Dimension.fromExp(exp, dim.var)
            end

            P_Dimension.Dimension.UNKNOWN(__) => begin
              Error.addInternalError(
                getInstanceName() + " returned unknown dimension in a non-function context",
                info,
              )
              fail()
            end

            _ => begin
              dim
            end
          end
        end
        arrayUpdate(dimensions, index, dim)
        dim
      end

      _ => begin
        dimension
      end
    end
  end
  #=  Other kinds of dimensions are already typed.
  =#
  verifyDimension(dimension, component, info)
  return dimension
end

function verifyDimension(dimension::Dimension, component::InstNode, info::SourceInfo)
  return @assign () = begin
    @match dimension begin
      DIMENSION_INTEGER(__) => begin
        #=  Check that integer dimensions are not negative.
        =#
        if dimension.size < 0
          Error.addSourceMessage(
            Error.NEGATIVE_DIMENSION_INDEX,
            list(String(dimension.size), name(component)),
            info,
          )
          fail()
        end
        ()
      end

      _ => begin
        ()
      end
    end
  end
end

""" #= Tries to fetch the binding for a given record field by using the binding of
   the record instance. =#"""
function getRecordElementBinding(component::InstNode)::Tuple{Binding, Integer}
  local parentDims::Integer = 0
  local binding::Binding

  local parent::InstNode
  local comp::Component
  local exp::Expression
  local parent_binding::Binding

  @assign parent = derivedParent(component)
  if isComponent(parent)
    @assign comp = component(parent)
    @assign parent_binding = getBinding(comp)
    if isUnbound(parent_binding)
      @assign (binding, parentDims) = getRecordElementBinding(parent)
    else
      @assign binding = typeBinding(parent_binding, ORIGIN_CLASS)
      if !referenceEq(parent_binding, binding)
        componentApply(parent, setBinding, binding)
      end
    end
    @assign parentDims = parentDims + dimensionCount(comp)
    if isBound(binding)
      @assign binding = recordFieldBinding(component, binding)
    end
  else
    @assign binding = EMPTY_BINDING
  end
  return (binding, parentDims)
end

function typeBindings(cls::InstNode, component::InstNode, origin::ORIGIN_Type)
  local c::Class
  local cls_tree::ClassTree
  local node::InstNode

  @assign c = getClass(cls)
  return @assign () = begin
    @match c begin
      INSTANCED_CLASS(elements = cls_tree && CLASS_TREE_FLAT_TREE(__)) => begin
        for c in cls_tree.components
          typeComponentBinding(c, origin)
        end
        ()
      end

      INSTANCED_BUILTIN(elements = cls_tree && CLASS_TREE_FLAT_TREE(__)) => begin
        for c in cls_tree.components
          typeComponentBinding(c, origin)
        end
        ()
      end

      INSTANCED_BUILTIN(__) => begin
        ()
      end

      TYPED_DERIVED(__) => begin
        typeBindings(c.baseClass, component, origin)
        ()
      end

      _ => begin
        Error.assertion(
          false,
          getInstanceName() + " got uninstantiated class " + name(cls),
          sourceInfo(),
        )
        fail()
      end
    end
  end
end

function typeComponentBinding(
  inComponent::InstNode,
  origin::ORIGIN_Type,
  typeChildren::Bool = true,
)
  local node::InstNode = resolveOuter(inComponent)
  local c::Component
  local binding::Binding
  local cls::InstNode
  local matchKind::MatchKind
  local nameStr::String
  local comp_var::VariabilityType
  local comp_eff_var::VariabilityType
  local bind_var::VariabilityType
  local bind_eff_var::VariabilityType
  local attrs::Attributes

  @assign c = component(node)
  () = begin
    @match c begin
      TYPED_COMPONENT(
        binding = UNTYPED_BINDING(__),
        attributes = attrs,
      ) => begin
        @assign nameStr = name(inComponent)
        @assign binding = c.binding
        #ErrorExt.setCheckpoint(getInstanceName())
        #TODO
        @error "ErrorExt.setCheckpoint(getInstanceName())"
        try
          checkBindingEach(c.binding)
          @assign binding =
            typeBinding(binding, setFlag(origin, ORIGIN_BINDING))
          #if !(Config.getGraphicsExpMode() && stringEq(nameStr, "graphics")) TODO
            @assign binding = matchBinding(binding, c.ty, nameStr, node)
          #end
          @assign comp_var = checkComponentBindingVariability(nameStr, c, binding, origin)
          if comp_var != attrs.variability
            @assign attrs.variability = comp_var
            @assign c.attributes = attrs
          end
        catch e
          if isBound(c.condition)
            @assign binding =
              INVALID_BINDING(binding, ErrorExt.getCheckpointMessages())
          else
            #            ErrorExt.delCheckpoint(getInstanceName())
            @error "Error in type componeent binding" # $e"
            fail()
          end
        end
#        ErrorExt.delCheckpoint(getInstanceName()) TODO
        @assign c.binding = binding
        if isBound(c.condition)
          @assign c.condition = typeComponentCondition(c.condition, origin)
        end
        updateComponent(c, node)
        if typeChildren
          typeBindings(c.classInst, inComponent, origin)
        end
        ()
      end

      TYPED_COMPONENT(__) => begin
        #=  A component without a binding, or with a binding that's already been typed.
        =#
        checkBindingEach(c.binding)
        if isTyped(c.binding)
          @assign c.binding =
            matchBinding(c.binding, c.ty, name(inComponent), node)
        end
        if isBound(c.condition)
          @assign c.condition = typeComponentCondition(c.condition, origin)
          updateComponent(c, node)
        end
        if typeChildren
          typeBindings(c.classInst, inComponent, origin)
        end
        ()
      end

      UNTYPED_COMPONENT(
        binding = UNTYPED_BINDING(__),
        attributes = attrs,
      ) => begin
        #=  An untyped component with a binding. This might happen when typing a
        =#
        #=  dimension and having to evaluate the binding of a not yet typed
        =#
        #=  component. Type only the binding and let the case above handle the rest.
        =#
        @assign nameStr = name(inComponent)
        checkBindingEach(c.binding)
        @assign binding =
          typeBinding(c.binding, setFlag(origin, ORIGIN_BINDING))
        @assign comp_var = checkComponentBindingVariability(nameStr, c, binding, origin)
        if comp_var != attrs.variability
          @assign attrs.variability = comp_var
          @assign c.attributes = attrs
        end
        @assign c.binding = binding
        updateComponent(c, node)
        ()
      end

      ENUM_LITERAL_COMPONENT(__) => begin
        ()
      end

      TYPE_ATTRIBUTE(modifier = MODIFIER_NOMOD(__)) => begin
        ()
      end

      TYPE_ATTRIBUTE(__) => begin
        @assign c.modifier =
          typeTypeAttribute(c.modifier, c.ty, parent(inComponent), origin)
        updateComponent(c, node)
        ()
      end

      _ => begin
        Error.assertion(
          false,
          getInstanceName() + " got invalid node " + name(node),
          sourceInfo(),
        )
        fail()
      end
    end
  end
end

function checkComponentBindingVariability(
  name::String,
  component::Component,
  binding::Binding,
  origin::ORIGIN_Type,
)::VariabilityType
  local var::VariabilityType

  local comp_var::VariabilityType
  local comp_eff_var::VariabilityType
  local bind_var::VariabilityType
  local bind_eff_var::VariabilityType

  @assign comp_var = variability(component)
  @assign comp_eff_var = effectiveVariability(comp_var)
  @assign bind_var = variability(binding)
  @assign bind_eff_var = effectiveVariability(bind_var)
  if bind_eff_var > comp_eff_var && flagNotSet(origin, ORIGIN_FUNCTION)
    Error.addSourceMessage(
      Error.HIGHER_VARIABILITY_BINDING,
      list(
        name,
        P_Prefixes.variabilityString(comp_eff_var),
        "'" + toString(getBinding(component)) + "'",
        P_Prefixes.variabilityString(bind_eff_var),
      ),
      getInfo(binding),
    )
    fail()
  end
  #=  Mark parameters that have a structural cref as binding as also
  =#
  #=  structural. This is perhaps not optimal, but is required right now
  =#
  #=  to avoid structural singularity and other issues.
  =#
  if comp_var == Variability.PARAMETER && (
    bind_var == Variability.STRUCTURAL_PARAMETER && isCrefExp(binding) ||
    bind_var == Variability.NON_STRUCTURAL_PARAMETER
  )
    @assign var = bind_var
  else
    @assign var = comp_var
  end
  return var
end

function typeBinding(binding::Binding, origin::ORIGIN_Type)::Binding
  @assign binding = begin
    local exp::Expression
    local ty::M_Type
    local var::VariabilityType
    local info::SourceInfo
    local each_ty::EachTypeType
    @match binding begin
      UNTYPED_BINDING(bindingExp = exp) => begin
        @assign info = getInfo(binding)
        @assign (exp, ty, var) = typeExp(exp, origin, info)
        if binding.isEach
          @assign each_ty = EachType.EACH
        elseif isClassBinding(binding)
          @assign each_ty = EachType.REPEAT
        else
          @assign each_ty = EachType.NOT_EACH
        end
        TYPED_BINDING(exp, ty, var, each_ty, false, false, binding.info)
      end

      TYPED_BINDING(__) => begin
        binding
      end

      UNBOUND(__) => begin
        binding
      end

      _ => begin
        # Error.assertion(
        #   false,
        #   getInstanceName() + " got uninstantiated binding",
        #   sourceInfo(),
        # )
        @error "Uninstantiated binding!"
        fail()
      end
    end
  end
  return binding
end

function checkBindingEach(binding::Binding)
  local parents::List{InstNode}
  if isEach(binding)
    @assign parents = listRest(parents(binding))
    for parent in parents
      if isArray(getType(parent))
        return
      end
    end
    # Error.addStrictMessage(
    #   Error.EACH_ON_NON_ARRAY,
    #   list(name(listHead(parents))),
    #   getInfo(binding),
    # )
  end
end

function typeComponentCondition(condition::Binding, origin::ORIGIN_Type)::Binding

  @assign condition = begin
    local exp::Expression
    local ty::M_Type
    local var::VariabilityType
    local info::SourceInfo
    local mk::MatchKind
    @match condition begin
      UNTYPED_BINDING(bindingExp = exp) => begin
        @assign info = getInfo(condition)
        @assign (exp, ty, var) =
          typeExp(exp, setFlag(origin, ORIGIN_CONDITION), info)
        @assign (exp, _, mk) = matchTypes(ty, TYPE_BOOLEAN(), exp)
        if isIncompatibleMatch(mk)
          Error.addSourceMessage(
            Error.IF_CONDITION_TYPE_ERROR,
            list(toString(exp), Type.toString(ty)),
            info,
          )
          fail()
        end
        if var > Variability.PARAMETER
          Error.addSourceMessage(
            Error.COMPONENT_CONDITION_VARIABILITY,
            list(toString(exp)),
            info,
          )
          fail()
        end
        TYPED_BINDING(
          exp,
          ty,
          var,
          NFBinding.EachType.NOT_EACH,
          false,
          false,
          info,
        )
      end
    end
  end
  return condition
end

function typeTypeAttribute(
  attribute::Modifier,
  ty::NFType,
  component::InstNode,
  origin::ORIGIN_Type,
)::Modifier

  local name::String
  local binding::Binding
  local mod_parent::InstNode
  @assign attribute = begin
    @match attribute begin
      MODIFIER_MODIFIER(__) where {(!ModTable.isEmpty(attribute.subModifiers))} => begin
        #=  Modifier with submodifier, e.g. Real x(start(y = 1)), is an error.
        =#
        #=  Print an error for the first submodifier. The builtin attributes
        =#
        #=  don't have types as such, so for the error message to make sense we
        =#
        #=  join the attribute name and submodifier name together (e.g. start.y).
        =#
        @assign name =
          attribute.name +
          "." +
          Util.tuple21(listHead(ModTable.toList(attribute.subModifiers)))
        Error.addSourceMessage(
          Error.MISSING_MODIFIED_ELEMENT,
          list(name, Type.toString(ty)),
          attribute.info,
        )
        fail()
      end

      MODIFIER_MODIFIER(__) where {(isUnbound(attribute.binding))} => begin
        #=  Modifier with no binding, e.g. Real x(final start).
        =#
        checkBindingEach(attribute.binding)
        NFModifier.NOMOD()
      end

      MODIFIER_MODIFIER(name = name, binding = binding) => begin
        #=  Normal modifier with no submodifiers.
        =#
        #=  Type and type check the attribute.
        =#
        checkBindingEach(binding)
        if isBound(binding)
          @assign binding = typeBinding(binding, origin)
          @assign binding = matchBinding(binding, ty, name, component)
          if variability(binding) > Variability.PARAMETER
            Error.addSourceMessage(
              Error.HIGHER_VARIABILITY_BINDING,
              list(
                name,
                P_Prefixes.variabilityString(Variability.PARAMETER),
                "'" + toString(binding) + "'",
                P_Prefixes.variabilityString(variability(binding)),
              ),
              getInfo(binding),
            )
            fail()
          end
          @assign attribute.binding = binding
        end
        #=  Check the variability. All builtin attributes have parameter variability.
        =#
        attribute
      end
    end
  end
  return attribute
end

""" #= Types an untyped expression, returning the typed expression itself along with
   its type and variability. =#"""
function typeExp(
  exp::Expression,
  origin::ORIGIN_Type,
  info::SourceInfo,
)::Tuple{Expression, NFType, VariabilityType}
  local variability::VariabilityType
  local ty::NFType

  @assign (exp, ty, variability) = begin
    local e1::Expression
    local e2::Expression
    local e3::Expression
    local var1::VariabilityType
    local var2::VariabilityType
    local var3::VariabilityType
    local ty1::M_Type
    local ty2::M_Type
    local ty3::M_Type
    local op::Operator
    local cref::ComponentRef
    local next_origin::ORIGIN_Type
    @match exp begin
      INTEGER_EXPRESSION(__) => begin
        (exp, TYPE_INTEGER(), Variability.CONSTANT)
      end

      REAL_EXPRESSION(__) => begin
        (exp, TYPE_REAL(), Variability.CONSTANT)
      end

      STRING_EXPRESSION(__) => begin
        (exp, TYPE_STRING(), Variability.CONSTANT)
      end

      BOOLEAN_EXPRESSION(__) => begin
        (exp, TYPE_BOOLEAN(), Variability.CONSTANT)
      end

      ENUM_LITERAL_EXPRESSION(__) => begin
        (exp, exp.ty, Variability.CONSTANT)
      end

      CREF_EXPRESSION(__) => begin
        typeCrefExp(exp.cref, origin, info)
      end

      TYPENAME_EXPRESSION(__) => begin
        if flagNotSet(origin, ORIGIN_VALID_TYPENAME_SCOPE)
          Error.addSourceMessage(
            Error.INVALID_TYPENAME_USE,
            list(Type.typenameString(arrayElementType(exp.ty))),
            info,
          )
          fail()
        end
        (exp, exp.ty, Variability.CONSTANT)
      end

      ARRAY_EXPRESSION(__) => begin
        typeArray(exp.elements, origin, info)
      end

      MATRIX_EXPRESSION(__) => begin
        typeMatrix(exp.elements, origin, info)
      end

      RANGE_EXPRESSION(__) => begin
        typeRange(exp, origin, info)
      end

      TUPLE_EXPRESSION(__) => begin
        typeTuple(exp.elements, origin, info)
      end

      SIZE_EXPRESSION(__) => begin
        typeSize(exp, origin, info)
      end

      END_EXPRESSION(__) => begin
        #=  end is replaced in subscripts before we get here, so any end still
        =#
        #=  left should be outside a subscript and thus illegal.
        =#
        Error.addSourceMessage(Error.END_ILLEGAL_USE_ERROR, nil, info)
        fail()
      end

      BINARY_EXPRESSION(__) => begin
        @assign next_origin = setFlag(origin, ORIGIN_SUBEXPRESSION)
        @assign (e1, ty1, var1) = typeExp(exp.exp1, next_origin, info)
        @assign (e2, ty2, var2) = typeExp(exp.exp2, next_origin, info)
        @assign (exp, ty) = checkBinaryOperation(
          e1,
          ty1,
          var1,
          exp.operator,
          e2,
          ty2,
          var2,
          info,
        )
        (exp, ty, variabilityMax(var1, var2))
      end

      UNARY_EXPRESSION(__) => begin
        @assign next_origin = setFlag(origin, ORIGIN_SUBEXPRESSION)
        @assign (e1, ty1, var1) = typeExp(exp.exp, next_origin, info)
        @assign (exp, ty) =
          checkUnaryOperation(e1, ty1, var1, exp.operator, info)
        (exp, ty, var1)
      end

      LBINARY_EXPRESSION(__) => begin
        @assign next_origin = setFlag(origin, ORIGIN_SUBEXPRESSION)
        @assign (e1, ty1, var1) = typeExp(exp.exp1, next_origin, info)
        @assign (e2, ty2, var2) = typeExp(exp.exp2, next_origin, info)
        @assign (exp, ty) = checkLogicalBinaryOperation(
          e1,
          ty1,
          var1,
          exp.operator,
          e2,
          ty2,
          var2,
          info,
        )
        (exp, ty, variabilityMax(var1, var2))
      end

      LUNARY_EXPRESSION(__) => begin
        @assign next_origin = setFlag(origin, ORIGIN_SUBEXPRESSION)
        @assign (e1, ty1, var1) = typeExp(exp.exp, next_origin, info)
        @assign (exp, ty) =
          checkLogicalUnaryOperation(e1, ty1, var1, exp.operator, info)
        (exp, ty, var1)
      end

      RELATION_EXPRESSION(__) => begin
        @assign next_origin = setFlag(origin, ORIGIN_SUBEXPRESSION)
        @assign (e1, ty1, var1) = typeExp(exp.exp1, next_origin, info)
        @assign (e2, ty2, var2) = typeExp(exp.exp2, next_origin, info)
        @assign (exp, ty) = checkRelationOperation(
          e1,
          ty1,
          var1,
          exp.operator,
          e2,
          ty2,
          var2,
          origin,
          info,
        )
        @assign variability = variabilityMax(var1, var2)
        #=  A relation involving continuous expressions which is not inside
        =#
        #=  noEvent is a discrete expression.
        =#
        if flagNotSet(origin, ORIGIN_NOEVENT) &&
           variability == Variability.CONTINUOUS
          @assign variability = Variability.DISCRETE
        end
        (exp, ty, variability)
      end

      IF_EXPRESSION(__) => begin
        typeIfExpression(exp, origin, info)
      end

      CALL_EXPRESSION(__) => begin
        @assign (e1, ty, var1) = typeCall(exp, origin, info)
        #=  If the call has multiple outputs and isn't alone on either side of an
        =#
        #=  equation/algorithm, select the first output.
        =#
        if isTuple(ty) && !isSingleExpression(origin)
          @assign ty = firstTupleType(ty)
          @assign e1 = tupleElement(e1, ty, 1)
        end
        (e1, ty, var1)
      end

      CAST_EXPRESSION(__) => begin
        @assign next_origin = setFlag(origin, ORIGIN_SUBEXPRESSION)
        typeExp(exp.exp, next_origin, info)
      end

      SUBSCRIPTED_EXP_EXPRESSION(__) => begin
        (exp, exp.ty, variability(exp))
      end

      MUTABLE_EXPRESSION(__) => begin
        #=  Subscripted expressions are assumed to already be typed.
        =#
        @assign e1 = P_Pointer.access(exp.exp)
        @assign (e1, ty, variability) = typeExp(e1, origin, info)
        @assign exp.exp = P_Pointer.create(e1)
        (exp, ty, variability)
      end

      PARTIAL_FUNCTION_APPLICATION_EXPRESSION(__) => begin
        typePartialApplication(exp, origin, info)
      end

      BINDING_EXP(__) => begin
        typeBindingExp(exp, origin, info)
      end
      _ => begin
        # Error.assertion(
        #   false,
        #   getInstanceName() +
        #   " got unknown expression: " +
        #   toString(exp),
        #   sourceInfo(),
        # )
        fail()
      end
    end
  end
  #=  Expressions inside when-clauses and initial sections are discrete.
  =#
  if flagSet(origin, ORIGIN_DISCRETE_SCOPE) &&
     variability == Variability.CONTINUOUS
    @assign variability = Variability.DISCRETE
  end
  return (exp, ty, variability)
end

function typeExpl(
  expl::List{<:Expression},
  origin::ORIGIN_Type,
  info::SourceInfo,
)::Tuple{List{Expression}, List{M_Type}, List{Variability}}
  local varl::List{Variability} = nil
  local tyl::List{M_Type} = nil
  local explTyped::List{Expression} = nil

  local exp::Expression
  local var::VariabilityType
  local ty::M_Type

  for e in listReverse(expl)
    @assign (exp, ty, var) = typeExp(e, origin, info)
    @assign explTyped = _cons(exp, explTyped)
    @assign tyl = _cons(ty, tyl)
    @assign varl = _cons(var, varl)
  end
  return (explTyped, tyl, varl)
end

function typeBindingExp(
  exp::Expression,
  origin::ORIGIN_Type,
  info::SourceInfo,
)::Tuple{Expression, M_Type, VariabilityType}
  local variability::VariabilityType
  local ty::M_Type
  local outExp::Expression

  local e::Expression
  local parents::List{InstNode}
  local is_each::Bool
  local exp_ty::M_Type
  local parent_dims::Integer

  @match BINDING_EXP(e, _, _, parents, is_each) = exp
  @assign (e, exp_ty, variability) = typeExp(e, origin, info)
  @assign parent_dims = 0
  if !is_each
    for p in listRest(parents)
      @assign parent_dims = parent_dims + dimensionCount(getType(p))
    end
  end
  if parent_dims == 0
    @assign ty = exp_ty
  else
    if dimensionCount(exp_ty) >= parent_dims
      @assign ty = Type.unliftArrayN(parent_dims, exp_ty)
    end
  end
  #=  If the binding has too few dimensions we can't unlift it, but matchBinding
  =#
  #=  can report the error better so we silently ignore it here.
  =#
  @assign outExp = BINDING_EXP(e, exp_ty, ty, parents, is_each)
  return (outExp, ty, variability)
end

""" #= Returns the requested dimension of the given expression, while doing as
   little typing as possible. This function returns TypingError.OUT_OF_BOUNDS if
   the given index doesn't refer to a valid dimension, in which case the
   returned dimension is undefined. =#"""
function typeExpDim(
  exp::Expression,
  dimIndex::Integer,
  origin::ORIGIN_Type,
  info::SourceInfo,
)::Tuple{Dimension, Option{Expression}, TypingError}
  local error::TypingError
  local typedExp::Option{Expression} = NONE()
  local dim::Dimension

  local ty::M_Type
  local e::Expression

  @assign ty = typeOf(exp)
  if Type.isKnown(ty)
    @assign (dim, error) = nthDimensionBoundsChecked(ty, dimIndex)
    @assign typedExp = SOME(exp)
  else
    @assign e = P_Expression.Expression.getBindingExp(exp)
    @assign (dim, error) = begin
      @match e begin
        ARRAY_EXPRESSION(ty = TYPE_UNKNOWN(__)) => begin
          typeArrayDim(e, dimIndex)
        end

        CREF_EXPRESSION(__) => begin
          typeCrefDim(e.cref, dimIndex, origin, info)
        end

        _ => begin
          #=  If the expression has already been typed, just get the dimension from the type.
          =#
          #=  Otherwise we try to type as little as possible of the expression to get
          =#
          #=  the dimension we need, to avoid introducing unnecessary cycles.
          =#
          #=  An untyped array, use typeArrayDim to get the dimension.
          =#
          #=  A cref, use typeCrefDim to get the dimension.
          =#
          #=  Any other expression, type the whole expression and get the dimension
          =#
          #=  from the type.
          =#
          @assign (e, ty, _) = typeExp(e, origin, info)
          @assign typedExp = SOME(e)
          nthDimensionBoundsChecked(ty, dimIndex)
        end
      end
    end
  end
  return (dim, typedExp, error)
end

""" #= Returns the requested dimension of an array dimension. This function is meant
   to be used on an untyped array, for a typed array it's better to just use
   e.g.  nthDimensionBoundsChecked on its type. =#"""
function typeArrayDim(
  arrayExp::Expression,
  dimIndex::Integer,
)::Tuple{Dimension, TypingError}
  local error::TypingError
  local dim::Dimension

  #=  We don't yet know the number of dimensions, but the index must at least be 1.
  =#
  if dimIndex < 1
    @assign dim = P_Dimension.Dimension.UNKNOWN()
    @assign error =
      P_TypingError.OUT_OF_BOUNDS(P_Expression.Expression.dimensionCount(arrayExp))
  else
    @assign (dim, error) = typeArrayDim2(arrayExp, dimIndex)
  end
  return (dim, error)
end

function typeArrayDim2(
  arrayExp::Expression,
  dimIndex::Integer,
  dimCount::Integer = 0,
)::Tuple{Dimension, TypingError}
  local error::TypingError
  local dim::Dimension

  @assign (dim, error) = begin
    @match (arrayExp, dimIndex) begin
      (ARRAY_EXPRESSION(__), 1) => begin
        (P_Dimension.Dimension.fromExpList(arrayExp.elements), P_TypingError.NO_ERROR())
      end

      (ARRAY_EXPRESSION(__), _) => begin
        typeArrayDim2(listHead(arrayExp.elements), dimIndex - 1, dimCount + 1)
      end

      _ => begin
        #=  Modelica arrays are non-ragged and only the last dimension of an array
        =#
        #=  expression can be empty, so just traverse into the first element.
        =#
        @assign dim = P_Dimension.Dimension.UNKNOWN()
        @assign error = P_TypingError.OUT_OF_BOUNDS(dimCount)
        (dim, error)
      end
    end
  end
  return (dim, error)
end

function typeCrefDim(
  cref::ComponentRef,
  dimIndex::Integer,
  origin::ORIGIN_Type,
  info::SourceInfo,
)::Tuple{Dimension, TypingError}
  local error::TypingError = P_TypingError.NO_ERROR()
  local dim::Dimension

  local crl::List{ComponentRef}
  local subs::List{Subscript}
  local index::Integer
  local dim_count::Integer
  local dim_total::Integer = 0
  local node::InstNode
  local c::Component
  local ty::M_Type

  #=  TODO: If the cref has subscripts it becomes trickier to correctly calculate
  =#
  #=        the dimension. For now we take the easy way out and just type the
  =#
  #=        whole cref, but doing so might introduce unnecessary cycles.
  =#
  if hasSubscripts(cref)
    @assign (_, ty) = typeCref(cref, origin, info)
    @assign (dim, error) = nthDimensionBoundsChecked(ty, dimIndex)
    return (dim, error)
  end
  #=  Loop through the cref in reverse, reducing the index by the number of
  =#
  #=  dimensions each component has until we find a component that the index is
  =#
  #=  valid for. This is done even if the index is 0 or negative, since the loop
  =#
  #=  also sums up the total number of dimensions which is needed to give a good
  =#
  #=  error message.
  =#
  @assign crl = toListReverse(cref)
  @assign index = dimIndex
  for cr in crl
    @assign () = begin
      @match cr begin
        CREF(
          node = COMPONENT_NODE(__),
          subscripts = subs,
        ) => begin
          @assign node = resolveOuter(cr.node)
          @assign c = component(node)
          #=  If the component is untyped it might have an array type whose dimensions
          =#
          #=  we need to take into consideration. To avoid making this more complicated
          =#
          #=  than it already is we make sure that the component is typed in that case.
          =#
          if hasDimensions(getClass(P_Component.classInstance(c)))
            typeComponent(node, origin)
            @assign c = component(node)
          end
          @assign dim_count = begin
            @match c begin
              P_Component.UNTYPED_COMPONENT(__) => begin
                @assign dim_count = arrayLength(c.dimensions)
                if index <= dim_count && index > 0
                  @assign dim = typeDimension(
                    c.dimensions,
                    index,
                    node,
                    c.binding,
                    origin,
                    c.info,
                  )
                  return
                end
                dim_count
              end

              TYPED_COMPONENT(__) => begin
                @assign dim_count = dimensionCount(c.ty)
                if index <= dim_count && index > 0
                  @assign dim = Type.nthDimension(c.ty, index)
                  return
                end
                dim_count
              end

              _ => begin
                0
              end
            end
          end
          @assign index = index - dim_count
          @assign dim_total = dim_total + dim_count
          ()
        end

        _ => begin
          ()
        end
      end
    end
  end
  @assign dim = P_Dimension.Dimension.UNKNOWN()
  @assign error = P_TypingError.OUT_OF_BOUNDS(dim_total)
  return (dim, error)
end

""" #= Returns the requested dimension from the given type, along with a TypingError
   indicating whether the index was valid or not. =#"""
function nthDimensionBoundsChecked(
  ty::M_Type,
  dimIndex::Integer,
  offset::Integer = 0,
)::Tuple{Dimension, TypingError} #= The number of dimensions to skip due to subscripts. =#
  local error::TypingError
  local dim::Dimension

  local dim_size::Integer = dimensionCount(ty)
  local index::Integer = dimIndex + offset

  if index < 1 || index > dim_size
    @assign dim = P_Dimension.Dimension.UNKNOWN()
    @assign error = P_TypingError.OUT_OF_BOUNDS(dim_size - offset)
  else
    @assign dim = Type.nthDimension(ty, index)
    @assign error = P_TypingError.NO_ERROR()
  end
  return (dim, error)
end

function typeCrefExp(
  cref::ComponentRef,
  o::ORIGIN_Type,
  info::SourceInfo,
)::Tuple{Expression, NFType, VariabilityType}
  local variability::VariabilityType
  local ty::NFType
  local exp::Expression
  local cr::ComponentRef
  local node_var::VariabilityType
  local subs_var::VariabilityType
  local eval::Bool
  (cr, ty, node_var, subs_var) = typeCref(cref, o, info)
  exp = CREF_EXPRESSION(ty, cr)
  variability = variabilityMax(node_var, subs_var)
  return (exp, ty, variability)
end

function typeCref(
  cref::ComponentRef,
  origin::ORIGIN_Type,
  info::SourceInfo
)::Tuple{ComponentRef, NFType, VariabilityType, VariabilityType}
  local subsVariability::VariabilityType
  local nodeVariabilityType::VariabilityType
  local ty::NFType
  local subs_var::VariabilityType
  if flagSet(origin, ORIGIN_FUNCTION) &&
     firstName(cref) == "time"
    Error.addSourceMessage(Error.EXP_INVALID_IN_FUNCTION, list("time"), info)
    fail()
  end
  @assign (cref, subsVariability) = typeCref2(cref, origin, info)
  @assign ty = getSubscriptedType(cref)
  @assign nodeVariabilityType = nodeVariability(cref)
  return (cref, ty, nodeVariabilityType, subsVariability)
end

function typeCref2(
  cref::ComponentRef,
  origin::ORIGIN_Type,
  info::SourceInfo,
  firstPart::Bool = true,
)::Tuple{ComponentRef, VariabilityType}
  local subsVariability::VariabilityType
  @assign (cref, subsVariability) = begin
    local rest_cr::ComponentRef
    local node_ty::NFType
    local subs::List{Subscript}
    local subs_var::VariabilityType
    local rest_var::VariabilityType
    local node_origin::ORIGIN_Type
    local fn::M_Function
    @match cref begin
      COMPONENT_REF_CREF(origin = Origin.SCOPE) => begin
        @assign cref.ty = getType(cref.node)
        (cref, Variability.CONSTANT)
      end

      COMPONENT_REF_CREF(node = COMPONENT_NODE(__)) => begin
        if hasCondition(component(cref.node)) && (
          flagNotSet(origin, ORIGIN_CONNECT) ||
          flagSet(origin, ORIGIN_SUBSCRIPT)
        )
          Error.addStrictMessage(
            Error.CONDITIONAL_COMPONENT_INVALID_CONTEXT,
            list(name(cref.node)),
            info,
          )
        end
        #=  The origin used when typing a component node depends on where the
        =#
        #=  component was declared, not where it's used. This can be different to
        =#
        #=  the given origin, e.g. for package constants used in a function.
        =#
        @assign node_origin =
          if isFunction(explicitParent(cref.node))
            ORIGIN_FUNCTION
          else
            ORIGIN_CLASS
          end
        @assign node_ty = typeComponent(cref.node, node_origin)
        @assign (subs, subs_var) =
          typeSubscripts(cref.subscripts, node_ty, cref, origin, info)
        @assign (rest_cr, rest_var) = typeCref2(cref.restCref, origin, info, false)
        @assign subsVariability = variabilityMax(subs_var, rest_var)
        (
          COMPONENT_REF_CREF(cref.node, subs, node_ty, cref.origin, rest_cr),
          subsVariability,
        )
      end

      COMPONENT_REF_CREF(
        node = CLASS_NODE(__),
      ) where {(firstPart && isFunction(cref.node))} => begin
        @match _cons(fn, _) = P_Function.typeNodeCache(cref.node)
        @assign cref.ty = Type.FUNCTION(fn, FunctionType.FUNCTION_REFERENCE)
        @assign cref.restCref = typeCref2(cref.restCref, origin, info, false)
        (cref, Variability.CONTINUOUS)
      end

      COMPONENT_REF_CREF(node = CLASS_NODE(__)) => begin
        @assign cref.ty = getType(cref.node)
        (cref, Variability.CONSTANT)
      end

      _ => begin
        (cref, Variability.CONSTANT)
      end
    end
  end
  return (cref, subsVariability)
end

function typeSubscripts(
  subscripts::List{<:Subscript},
  crefType::NFType,
  cref::ComponentRef,
  origin::ORIGIN_Type,
  info::SourceInfo,
)::Tuple{List{Subscript}, VariabilityType}
  local variability::VariabilityType = Variability.CONSTANT
  local typedSubs::List{Subscript}

  local dims::List{Dimension}
  local dim::Dimension
  local next_origin::Integer
  local i::Integer
  local sub::Subscript
  local var::VariabilityType

  if listEmpty(subscripts)
    @assign typedSubs = subscripts
    return (typedSubs, variability)
  end
  @assign dims = arrayDims(crefType)
  @assign typedSubs = nil
  @assign next_origin = setFlag(origin, ORIGIN_SUBSCRIPT)
  @assign i = 1
  if listLength(subscripts) > listLength(dims)
    Error.addSourceMessage(
      Error.WRONG_NUMBER_OF_SUBSCRIPTS,
      list(
        toString(cref),
        String(listLength(subscripts)),
        String(listLength(dims)),
      ),
      info,
    )
    fail()
  end
  for s in subscripts
    @match _cons(dim, dims) = dims
    @assign (sub, var) = typeSubscript(s, dim, cref, i, next_origin, info)
    @assign typedSubs = _cons(sub, typedSubs)
    @assign variability = variabilityMax(variability, var)
    @assign i = i + 1
    if var == Variability.PARAMETER
      Inst.markStructuralParamsSub(sub)
    end
  end
  #=  Mark parameter subscripts as structural so that they're evaluated.
  =#
  #=  TODO: Ideally this shouldn't be needed, but the old frontend does it and
  =#
  #=        the backend relies on it.
  =#
  @assign typedSubs = listReverseInPlace(typedSubs)
  return (typedSubs, variability)
end

function typeSubscript(
  subscript::Subscript,
  dimension::Dimension,
  cref::ComponentRef,
  index::Integer,
  origin::Type,
  info::SourceInfo,
)::Tuple{Subscript, Variability}
  local variability::VariabilityType = Variability.CONSTANT
  local outSubscript::Subscript = subscript

  local e::Expression
  local ty::M_Type
  local ety::M_Type
  local mk::MatchKind

  @assign (ty, variability) = begin
    @match subscript begin
      SUBSCRIPT_UNTYPED(__) => begin
        #=  An untyped subscript, type the expression and create a typed subscript.
        =#
        @assign e = evaluateEnd(subscript.exp, dimension, cref, index, origin, info)
        @assign (e, ty, variability) = typeExp(e, origin, info)
        if isArray(ty)
          @assign outSubscript = SUBSCRIPT_SLICE(e)
          @assign ty = Type.unliftArray(ty)
          if flagSet(origin, ORIGIN_EQUATION)
            Inst.markStructuralParamsExp(e)
          end
        else
          @assign outSubscript = SUBSCRIPT_INDEX(e)
        end
        (ty, variability)
      end

      SUBSCRIPT_INDEX(index = e) => begin
        (typeOf(e), P_Expression.Expression.variability(e))
      end

      SUBSCRIPT_SLICE(slice = e) => begin
        (
          Type.unliftArray(typeOf(e)),
          P_Expression.Expression.variability(e),
        )
      end

      SUBSCRIPT_WHOLE(__) => begin
        (TYPE_UNKNOWN(), P_Dimension.Dimension.variability(dimension))
      end

      _ => begin
        #=  Other subscripts have already been typed, but still need to be type checked.
        =#
        Error.assertion(false, getInstanceName() + " got unknown subscript", sourceInfo())
        fail()
      end
    end
  end
  #=  Type check the subscript's type against the expected subscript type for the dimension.
  =#
  @assign ety = P_Dimension.Dimension.subscriptType(dimension)
  #=  We can have both : subscripts and : dimensions here, so we need to allow unknowns.
  =#
  @assign (_, _, mk) = matchTypes(ty, ety, e, allowUnknown = true)
  if isIncompatibleMatch(mk)
    Error.addSourceMessage(
      Error.SUBSCRIPT_TYPE_MISMATCH,
      list(
        toString(subscript),
        Type.toString(ty),
        Type.toString(ety),
      ),
      info,
    )
    fail()
  end
  return (outSubscript, variability)
end

function typeArray(
  elements::List{<:Expression},
  origin::ORIGIN_Type,
  info::SourceInfo,
)::Tuple{Expression, M_Type, Variability}
  local variability::VariabilityType = Variability.CONSTANT
  local arrayType::M_Type = TYPE_UNKNOWN()
  local arrayExp::Expression

  local exp::Expression
  local expl::List{Expression} = nil
  local expl2::List{Expression} = nil
  local var::VariabilityType
  local ty1::M_Type = TYPE_UNKNOWN()
  local ty2::M_Type
  local ty3::M_Type
  local tys::List{M_Type} = nil
  local mk::MatchKind
  local n::Integer = 1
  local next_origin::ORIGIN_Type

  @assign next_origin = setFlag(origin, ORIGIN_SUBEXPRESSION)
  for e in elements
    @assign (exp, ty2, var) = typeExp(e, next_origin, info)
    @assign variability = variabilityMax(var, variability)
    @assign (_, ty3, mk) = matchTypes(ty2, ty1, exp, allowUnknown = true)
    if isIncompatibleMatch(mk)
      @assign (_, ty3, mk) = matchTypes(ty1, ty2, exp, allowUnknown = false)
      if isCompatibleMatch(mk)
        @assign ty1 = ty3
      end
    else
      @assign ty1 = ty3
    end
    @assign expl = _cons(exp, expl)
    @assign tys = _cons(ty2, tys)
    @assign n = n + 1
  end
  #=  Try the other way around to get the super-type of the array
  =#
  #=  Give the actual error-messages here after we got the super-type of the array
  =#
  for e in expl
    @match _cons(ty2, tys) = tys
    @assign (exp, _, mk) = matchTypes(ty2, ty1, e)
    @assign expl2 = _cons(exp, expl2)
    @assign n = n - 1
    if !Config.getGraphicsExpMode()
      if isIncompatibleMatch(mk)
        Error.addSourceMessage(
          Error.NF_ARRAY_TYPE_MISMATCH,
          list(
            String(n),
            toString(exp),
            Type.toString(ty2),
            Type.toString(ty1),
          ),
          info,
        )
        fail()
      end
    end
  end
  #=  forget errors when handling annotations
  =#
  @assign arrayType = Type.liftArrayLeft(ty1, P_Dimension.Dimension.fromExpList(expl2))
  @assign arrayExp = P_Expression.Expression.makeArray(arrayType, expl2)
  return (arrayExp, arrayType, variability)
end

""" #= The array concatenation operator =#"""
function typeMatrix(
  elements::List{<:List{<:Expression}},
  origin::ORIGIN_Type,
  info::SourceInfo,
)::Tuple{Expression, M_Type, Variability}
  local variability::VariabilityType = Variability.CONSTANT
  local arrayType::M_Type = TYPE_UNKNOWN()
  local arrayExp::Expression

  local exp::Expression
  local expl::List{Expression} = nil
  local res::List{Expression} = nil
  local var::VariabilityType
  local ty::M_Type = TYPE_UNKNOWN()
  local tys::List{M_Type} = nil
  local resTys::List{M_Type} = nil
  local n::Integer = 2
  local next_origin::ORIGIN_Type = setFlag(origin, ORIGIN_SUBEXPRESSION)

  if listLength(elements) > 1
    for el in elements
      @assign (exp, ty, var) = typeMatrixComma(el, next_origin, info)
      @assign variability = variabilityMax(var, variability)
      @assign expl = _cons(exp, expl)
      @assign tys = _cons(ty, tys)
      @assign n = max(n, dimensionCount(ty))
    end
    for e in expl
      @match _cons(ty, tys) = tys
      @assign (e, ty) = P_Expression.Expression.promote(e, ty, n)
      @assign resTys = _cons(ty, resTys)
      @assign res = _cons(e, res)
    end
    @assign (arrayExp, arrayType) =
      BuiltinCall.makeCatExp(1, res, resTys, variability, info)
  else
    @assign (arrayExp, arrayType, variability) =
      typeMatrixComma(listHead(elements), next_origin, info)
    if dimensionCount(arrayType) < 2
      @assign (arrayExp, arrayType) =
        P_Expression.Expression.promote(arrayExp, arrayType, n)
    end
  end
  return (arrayExp, arrayType, variability)
end

function typeMatrixComma(
  elements::List{<:Expression},
  origin::ORIGIN_Type,
  info::SourceInfo,
)::Tuple{Expression, M_Type, Variability}
  local variability::VariabilityType = Variability.CONSTANT
  local arrayType::M_Type
  local arrayExp::Expression

  local exp::Expression
  local expl::List{Expression} = nil
  local res::List{Expression} = nil
  local var::VariabilityType
  local ty::M_Type = TYPE_UNKNOWN()
  local ty1::M_Type
  local ty2::M_Type
  local ty3::M_Type
  local tys::List{M_Type} = nil
  local tys2::List{M_Type}
  local n::Integer = 2
  local pos::Integer
  local mk::MatchKind

  Error.assertion(
    !listEmpty(elements),
    getInstanceName() + " expected non-empty arguments",
    sourceInfo(),
  )
  if listLength(elements) > 1
    for e in elements
      @assign (exp, ty1, var) = typeExp(e, origin, info)
      @assign expl = _cons(exp, expl)
      if Type.isEqual(ty, TYPE_UNKNOWN())
        @assign ty = ty1
      else
        @assign (_, _, ty2, mk) = matchExpressions(
          INTEGER_EXPRESSION(0),
          arrayElementType(ty1),
          INTEGER_EXPRESSION(0),
          arrayElementType(ty),
        )
        if isCompatibleMatch(mk)
          @assign ty = ty2
        end
      end
      @assign tys = _cons(ty1, tys)
      @assign variability = variabilityMax(variability, var)
      @assign n = max(n, dimensionCount(ty))
    end
    @assign tys2 = nil
    @assign res = nil
    @assign pos = n + 1
    for e in expl
      @match _cons(ty1, tys) = tys
      @assign pos = pos - 1
      if dimensionCount(ty1) != n
        @assign (e, ty1) = P_Expression.Expression.promote(e, ty1, n)
      end
      @assign ty2 = setArrayElementType(ty1, ty)
      @assign (e, ty3, mk) = matchTypes(ty1, ty2, e)
      if isIncompatibleMatch(mk)
        Error.addSourceMessageAndFail(
          Error.ARG_TYPE_MISMATCH,
          list(
            String(pos),
            "matrix constructor ",
            "arg",
            toString(e),
            Type.toString(ty1),
            Type.toString(ty2),
          ),
          info,
        )
      end
      @assign res = _cons(e, res)
      @assign tys2 = _cons(ty3, tys2)
    end
    @assign (arrayExp, arrayType) = BuiltinCall.makeCatExp(2, res, tys2, variability, info)
  else
    @assign (arrayExp, arrayType, variability) = typeExp(listHead(elements), origin, info)
  end
  return (arrayExp, arrayType, variability)
end

function typeRange(
  rangeExp::Expression,
  origin::ORIGIN_Type,
  info::SourceInfo,
)::Tuple{Expression, M_Type, Variability}
  local variability::VariabilityType
  local rangeType::M_Type

  local start_exp::Expression
  local step_exp::Expression
  local stop_exp::Expression
  local start_ty::M_Type
  local step_ty::M_Type
  local stop_ty::M_Type
  local ostep_exp::Option{Expression}
  local ostep_ty::Option{M_Type}
  local start_var::VariabilityType
  local step_var::VariabilityType
  local stop_var::VariabilityType
  local ty_match::MatchKind
  local next_origin::ORIGIN_Type = ORIGIN_setFlag(origin, ORIGIN_SUBEXPRESSION)

  @match RANGE_EXPRESSION(
    start = start_exp,
    step = ostep_exp,
    stop = stop_exp,
  ) = rangeExp
  #=  Type start and stop.
  =#
  @assign (start_exp, start_ty, start_var) = typeExp(start_exp, next_origin, info)
  @assign (stop_exp, stop_ty, stop_var) = typeExp(stop_exp, next_origin, info)
  @assign variability = variabilityMax(start_var, stop_var)
  #=  Type check start and stop.
  =#
  @assign (start_exp, stop_exp, rangeType, ty_match) =
    matchExpressions(start_exp, start_ty, stop_exp, stop_ty)
  if isIncompatibleMatch(ty_match)
    printRangeTypeError(start_exp, start_ty, stop_exp, stop_ty, info)
  end
  if isSome(ostep_exp)
    @match SOME(step_exp) = ostep_exp
    @assign (step_exp, step_ty, step_var) = typeExp(step_exp, next_origin, info)
    @assign variability = variabilityMax(step_var, variability)
    @assign (start_exp, step_exp, rangeType, ty_match) =
      matchExpressions(start_exp, start_ty, step_exp, step_ty)
    if isIncompatibleMatch(ty_match)
      printRangeTypeError(start_exp, start_ty, step_exp, step_ty, info)
    end
    @assign stop_exp = matchTypes_cast(stop_ty, rangeType, stop_exp)
    @assign ostep_exp = SOME(step_exp)
    @assign ostep_ty = SOME(step_ty)
  else
    @assign ostep_exp = NONE()
    @assign ostep_ty = NONE()
  end
  #=  Type step.
  =#
  #=  Type check start and step.
  =#
  #=  We've checked start-stop and start-step now, so step-stop must also be
  =#
  #=  type compatible. Stop might need to be type cast here though.
  =#
  @assign rangeType =
    getRangeType(start_exp, ostep_exp, stop_exp, rangeType, info)
  @assign rangeExp =
    RANGE_EXPRESSION(rangeType, start_exp, ostep_exp, stop_exp)
  if variability <= Variability.PARAMETER && !flagSet(origin, ORIGIN_FUNCTION)
    Inst.markStructuralParamsExp(rangeExp)
  end
  return (rangeExp, rangeType, variability)
end

function typeTuple(
  elements::List{<:Expression},
  origin::ORIGIN_Type,
  info::SourceInfo,
)::Tuple{Expression, M_Type, Variability}
  local variability::VariabilityType
  local tupleType::M_Type
  local tupleExp::Expression

  local expl::List{Expression}
  local tyl::List{M_Type}
  local valr::List{Variability}
  local next_origin::ORIGIN_Type

  #=  Tuples are only allowed on the lhs side of an equality/assignment,
  =#
  #=  and only if they are alone and not part of a larger expression.
  =#
  if flagNotSet(origin, ORIGIN_LHS) ||
     flagSet(origin, ORIGIN_SUBEXPRESSION)
    Error.addSourceMessage(
      Error.RHS_TUPLE_EXPRESSION,
      list(toString(TUPLE_EXPRESSION(
        TYPE_UNKNOWN(),
        elements,
      ))),
      info,
    )
    fail()
  end
  @assign next_origin = setFlag(origin, ORIGIN_SUBEXPRESSION)
  @assign (expl, tyl, valr) = typeExpl(elements, next_origin, info)
  @assign tupleType = TYPE_TUPLE
  @assign tupleExp = TUPLE_EXPRESSION(tupleType, expl)
  @assign variability = if listEmpty(valr)
    Variability.CONSTANT
  else
    listHead(valr)
  end
  return (tupleExp, tupleType, variability)
end

function printRangeTypeError(
  exp1::Expression,
  ty1::M_Type,
  exp2::Expression,
  ty2::M_Type,
  info::SourceInfo,
)
  Error.addSourceMessage(
    Error.RANGE_TYPE_MISMATCH,
    list(
      toString(exp1),
      Type.toString(ty1),
      toString(exp2),
      Type.toString(ty2),
    ),
    info,
  )
  return fail()
end

""" #= Types a size expression. If evaluate is true the size expression is also
   evaluated if the dimension is known and the index is a parameter expression,
   otherwise a typed size expression is returned. =#"""
function typeSize(
  sizeExp::Expression,
  origin::ORIGIN_Type,
  info::SourceInfo,
  evaluate::Bool = true,
)::Tuple{Expression, M_Type, Variability}
  local variability::VariabilityType
  local sizeType::M_Type

  local exp::Expression
  local index::Expression
  local exp_ty::M_Type
  local index_ty::M_Type
  local ty_match::MatchKind
  local iindex::Integer
  local dim_size::Integer
  local dim::Dimension
  local ty_err::TypingError
  local oexp::Option{Expression}
  local next_origin::ORIGIN_Type = setFlag(origin, ORIGIN_SUBEXPRESSION)

  @assign (sizeExp, sizeType, variability) = begin
    @match sizeExp begin
      SIZE_EXPRESSION(exp = exp, dimIndex = SOME(index)) => begin
        @assign (index, index_ty, variability) = typeExp(index, next_origin, info)
        #=  The second argument must be an Integer.
        =#
        @assign (index, _, ty_match) =
          matchTypes(index_ty, TYPE_INTEGER(), index)
        if isIncompatibleMatch(ty_match)
          Error.addSourceMessage(
            Error.ARG_TYPE_MISMATCH,
            list(
              "2",
              "size ",
              "dim",
              toString(index),
              Type.toString(index_ty),
              "Integer",
            ),
            info,
          )
          fail()
        end
        if variability <= Variability.STRUCTURAL_PARAMETER &&
           !P_Expression.Expression.containsIterator(index, origin)
          @assign index = Ceval.evalExp(index, Ceval.P_EvalTarget.IGNORE_ERRORS())
          @match INTEGER_EXPRESSION(iindex) = index
          @assign (dim, oexp, ty_err) = typeExpDim(exp, iindex, next_origin, info)
          checkSizeTypingError(ty_err, exp, iindex, info)
          if P_Dimension.Dimension.isKnown(dim) && evaluate
            @assign exp = P_Dimension.Dimension.sizeExp(dim)
          else
            if isSome(oexp)
              @match SOME(exp) = oexp
            else
              @assign exp = typeExp(exp, next_origin, info)
            end
            @assign exp = SIZE_EXPRESSION(exp, SOME(index))
          end
          if flagNotSet(origin, ORIGIN_FUNCTION) ||
             P_Dimension.Dimension.isKnown(dim)
            @assign variability = Variability.CONSTANT
          else
            @assign variability = Variability.DISCRETE
          end
        else
          @assign (exp, exp_ty) = typeExp(sizeExp.exp, next_origin, info)
          if !isArray(exp_ty)
            Error.addSourceMessage(
              Error.INVALID_ARGUMENT_TYPE_FIRST_ARRAY,
              list("size"),
              info,
            )
            fail()
          end
          @assign exp = SIZE_EXPRESSION(exp, SOME(index))
        end
        #=  Evaluate the index if it's a constant.
        =#
        #=  TODO: Print an error if the index couldn't be evaluated to an int.
        =#
        #=  Get the iindex'd dimension of the expression.
        =#
        #=  If the dimension size is known, return its size.
        =#
        #=  If the dimension size is unknown (e.g. in a function) or
        =#
        #=  evaluation is disabled, return a size expression instead.
        =#
        #=  size is constant outside functions, or for known dimensions inside functions.
        =#
        #=  size is discrete for : in functions.
        =#
        #=  If the index is not a constant, type the whole expression.
        =#
        #=  Check that it's an array.
        =#
        #=  Since we don't know which dimension to take the size of, return a size expression.
        =#
        (exp, TYPE_INTEGER(), variability)
      end

      SIZE_EXPRESSION(__) => begin
        @assign (exp, exp_ty, _) = typeExp(sizeExp.exp, next_origin, info)
        @assign sizeType = Type.sizeType(exp_ty)
        (SIZE_EXPRESSION(exp, NONE()), sizeType, Variability.PARAMETER)
      end
    end
  end
  return (sizeExp, sizeType, variability)
end

function checkSizeTypingError(
  typingError::TypingError,
  exp::Expression,
  index::Integer,
  info::SourceInfo,
)
  return @assign () = begin
    @match typingError begin
      NO_ERROR(__) => begin
        ()
      end

      OUT_OF_BOUNDS(0) => begin
        #=  The first argument wasn't an array.
        =#
        Error.addSourceMessage(Error.INVALID_ARGUMENT_TYPE_FIRST_ARRAY, list("size"), info)
        fail()
      end

      OUT_OF_BOUNDS(__) => begin
        #=  The index referred to an invalid dimension.
        =#
        Error.addSourceMessage(
          Error.INVALID_SIZE_INDEX,
          list(
            String(index),
            toString(exp),
            String(typingError.upperBound),
          ),
          info,
        )
        fail()
      end
    end
  end
end

function evaluateEnd(
  exp::Expression,
  dim::Dimension,
  cref::ComponentRef,
  index::Integer,
  origin::ORIGIN_Type,
  info::SourceInfo,
)::Expression
  local outExp::Expression

  @assign outExp = begin
    local ty::M_Type
    local cr::ComponentRef
    @match exp begin
      P_Expression.Expression.END(__) => begin
        P_Dimension.Dimension.endExp(dim, cref, index)
      end

      CREF_EXPRESSION(__) => begin
        exp
      end

      _ => begin
        mapShallow(
          exp,
          (dim, cref, index, info, origin) -> evaluateEnd(
            dim = dim,
            cref = cref,
            index = index,
            info = info,
            origin = origin,
          ),
        )
      end
    end
  end
  #=  Stop when encountering a cref, any 'end' in a cref expression refers to
  =#
  #=  the cref's dimensions and will be evaluated when the cref is typed.
  =#
  return outExp
end

function typeIfExpression(
  ifExp::Expression,
  origin::ORIGIN_Type,
  info::SourceInfo,
)::Tuple{Expression, M_Type, Variability}
  local var::VariabilityType
  local ty::M_Type

  local cond::Expression
  local tb::Expression
  local fb::Expression
  local tb2::Expression
  local fb2::Expression
  local next_origin::ORIGIN_Type
  local cond_ty::M_Type
  local tb_ty::M_Type
  local fb_ty::M_Type
  local cond_var::VariabilityType
  local tb_var::VariabilityType
  local fb_var::VariabilityType
  local ty_match::MatchKind

  @match IF_EXPRESSION(condition = cond, trueBranch = tb, falseBranch = fb) =
    ifExp
  @assign next_origin = setFlag(origin, ORIGIN_SUBEXPRESSION)
  @assign (cond, cond_ty, cond_var) = typeExp(cond, next_origin, info)
  #=  The condition must be a scalar boolean.
  =#
  @assign (cond, _, ty_match) = matchTypes(cond_ty, TYPE_BOOLEAN(), cond)
  if isIncompatibleMatch(ty_match)
    Error.addSourceMessage(
      Error.IF_CONDITION_TYPE_ERROR,
      list(toString(cond), Type.toString(cond_ty)),
      info,
    )
    fail()
  end
  if cond_var <= Variability.STRUCTURAL_PARAMETER &&
     !P_Expression.Expression.contains(cond, isNonConstantIfCondition)
    if evaluateCondition(cond, origin, info)
      @assign (ifExp, ty, var) = typeExp(tb, next_origin, info)
    else
      @assign (ifExp, ty, var) = typeExp(fb, next_origin, info)
    end
  else
    @assign (tb, tb_ty, tb_var) = typeExp(tb, next_origin, info)
    @assign (fb, fb_ty, fb_var) = typeExp(fb, next_origin, info)
    @assign (tb2, fb2, ty, ty_match) = matchExpressions(tb, tb_ty, fb, fb_ty)
    if isIncompatibleMatch(ty_match)
      if cond_var <= Variability.PARAMETER
        @assign (ifExp, ty, var) = if evaluateCondition(cond, origin, info)
          (tb, tb_ty, tb_var)
        else
          (fb, fb_ty, fb_var)
        end
      else
        Error.addSourceMessage(
          Error.TYPE_MISMATCH_IF_EXP,
          list(
            "",
            toString(tb),
            Type.toString(tb_ty),
            toString(fb),
            Type.toString(fb_ty),
          ),
          info,
        )
        fail()
      end
    else
      @assign ifExp = IF_EXPRESSION(cond, tb2, fb2)
      @assign var =
        variabilityMax(cond_var, variabilityMax(tb_var, fb_var))
    end
  end
  #=  If the condition is constant, always do branch selection.
  =#
  #=  Otherwise type both of the branches.
  =#
  #=  If the branches have different types but the condition is a parameter
  =#
  #=  expression, do branch selection.
  =#
  #=  Otherwise give a type mismatch error.
  =#
  #=  If the types match, return a typed if-expression.
  =#
  return (ifExp, ty, var)
end

function evaluateCondition(
  condExp::Expression,
  origin::ORIGIN_Type,
  info::SourceInfo,
)::Bool
  local condBool::Bool

  local cond_exp::Expression

  @assign cond_exp = Ceval.evalExp(condExp, Ceval.P_EvalTarget.GENERIC(info))
  if arrayAllEqual(cond_exp)
    @assign cond_exp = arrayFirstScalar(cond_exp)
  end
  @assign condBool = begin
    @match cond_exp begin
      BOOLEAN_EXPRESSION(__) => begin
        cond_exp.value
      end

      _ => begin
        Error.assertion(
          false,
          getInstanceName() +
          " failed to evaluate condition `" +
          toString(condExp) +
          "`",
          info,
        )
        fail()
      end
    end
  end
  return condBool
end

function typeClassSections(classNode::InstNode, originArg::ORIGIN_Type)
  local cls::Class
  local typed_cls::Class
  local components::Array{InstNode}
  local sections::Sections
  local info::SourceInfo
  local initial_origin::Integer
  @assign cls = getClass(classNode)
   _ = begin
    @match cls begin
      INSTANCED_CLASS(restriction = RESTRICTION_TYPE(__)) => begin
        ()
      end

      INSTANCED_CLASS(
        elements = CLASS_TREE_FLAT_TREE(components = components),
        sections = sections,
      ) => begin
        @assign sections = begin
          @match sections begin
            SECTIONS(__) => begin
              initial_origin = setFlag(originArg, ORIGIN_INITIAL)
              map(
                sections,
                (x) -> typeEquation(x,
                  setFlag(originArg, ORIGIN_EQUATION),
                ),
                (x) -> typeAlgorithm(x,
                  setFlag(originArg, ORIGIN_ALGORITHM),
                ),
                (x) ->
                typeEquation(x,
                  setFlag(initial_origin, ORIGIN_EQUATION),
                  ),
                (x) ->
                  typeAlgorithm(x,
                    setFlag(initial_origin, ORIGIN_ALGORITHM),
                  )
              )
#              @error "TODO"
            end
            SECTIONS_EXTERNAL(__) => begin
              Error.addSourceMessage(
                Error.TRANS_VIOLATION,
                list(
                  name(classNode),
                  P_Restriction.Restriction.toString(cls.restriction),
                  "external declaration",
                ),
                info(classNode),
              )
              fail()
            end

            _ => begin
              sections
            end
          end
        end
        @assign typed_cls = setSections(sections, cls)
        for c in components
          typeComponentSections(resolveOuter(c), originArg)
        end
        updateClass(typed_cls, classNode)
        ()
      end

      INSTANCED_BUILTIN(__) => begin
        ()
      end

      TYPED_DERIVED(__) => begin
        typeClassSections(cls.baseClass, originArg)
        ()
      end

      _ => begin
        Error.assertion(
          false,
          getInstanceName() + " got uninstantiated class " + name(classNode),
          sourceInfo(),
        )
        fail()
      end
    end
   end
  return
end

function typeFunctionSections(classNode::InstNode, origin::ORIGIN_Type)
  local cls::Class
  local typed_cls::Class
  local sections::Sections
  local info::SourceInfo
  local alg::Algorithm

  @assign cls = getClass(classNode)
  return @assign _ = begin
    @match cls begin
      INSTANCED_CLASS(sections = sections) => begin
        @assign sections = begin
          @match sections begin
            SECTIONS(nil(), nil(), alg <| nil(), nil()) => begin
              @assign sections.algorithms = list(typeAlgorithm(
                alg,
                setFlag(origin, ORIGIN_ALGORITHM),
              ))
              sections
            end
            SECTIONS(__) => begin
              if listLength(sections.equations) > 0 ||
                 listLength(sections.initialEquations) > 0
                Error.addSourceMessage(
                  Error.EQUATION_TRANSITION_FAILURE,
                  list("function"),
                  info(classNode),
                )
              else
                Error.addSourceMessage(
                  Error.MULTIPLE_SECTIONS_IN_FUNCTION,
                  list(name(classNode)),
                  info(classNode),
                )
              end
              fail()
            end

            SECTIONS_EXTERNAL(explicit = true) => begin
              @assign info = info(classNode)
              @assign sections.args =
                List(typeExternalArg(arg, info, classNode) for arg in sections.args)
              @assign sections.outputRef = typeCref(sections.outputRef, origin, info)
              sections
            end

            SECTIONS_EXTERNAL(__) => begin
              makeDefaultExternalCall(sections, classNode)
            end
            _ => begin
              sections
            end
          end
        end
        @assign typed_cls = setSections(sections, cls)
        updateClass(typed_cls, classNode)
        ()
      end

      TYPED_DERIVED(__) => begin
        typeFunctionSections(cls.baseClass, origin)
        ()
      end
      _ => begin
        Error.assertion(
          false,
          getInstanceName() + " got uninstantiated class " + name(classNode),
          sourceInfo(),
        )
        fail()
      end
    end
  end
end

function typeExternalArg(arg::Expression, info::SourceInfo, node::InstNode)::Expression
  local outArg::Expression

  local ty::M_Type
  local var::VariabilityType
  local index::Expression

  @assign outArg = begin
    @match arg begin
      SIZE_EXPRESSION(dimIndex = SOME(_)) => begin
        @assign outArg = typeSize(arg, ORIGIN_FUNCTION, info, evaluate = false)
        @match SIZE_EXPRESSION(dimIndex = SOME(index)) = outArg
        #=  Size expression must have a constant dimension index.
        =#
        if !P_Expression.Expression.isInteger(index)
          Error.addSourceMessage(
            Error.EXTERNAL_ARG_NONCONSTANT_SIZE_INDEX,
            list(toString(arg)),
            info,
          )
          fail()
        end
        outArg
      end

      _ => begin
        @assign (outArg, ty, var) = typeExp(arg, ORIGIN_FUNCTION, info)
        begin
          @match arg begin
            CREF_EXPRESSION(__) => begin
              outArg
            end

            _ => begin
              #=  All kinds of crefs are allowed.
              =#
              #=  The only other kind of expression that's allowed is scalar constants.
              =#
              if Type.isScalarBuiltin(ty) && var == Variability.CONSTANT
                @assign outArg = Ceval.evalExp(outArg, Ceval.P_EvalTarget.GENERIC(info))
              else
                Error.addSourceMessage(
                  Error.EXTERNAL_ARG_WRONG_EXP,
                  list(toString(outArg)),
                  info,
                )
                fail()
              end
              outArg
            end
          end
        end
      end
    end
  end
  return outArg
end

""" #= Constructs a default external call for an external function. If only one
   output exists a call 'output = func(input1, input2, ...)' is generated,
   otherwise a call 'func(param1, param2, ...)' is generated from the function's
   formal parameters and local variables. =#"""
function makeDefaultExternalCall(extDecl::Sections, fnNode::InstNode)::Sections
  @assign extDecl = begin
    local args::List{Expression}
    local output_ref::ComponentRef
    local fn::M_Function
    local single_output::Bool
    local comps::Array{InstNode}
    local comp::Component
    local ty::M_Type
    local node::InstNode
    local exp::Expression
    @match extDecl begin
      SECTIONS_EXTERNAL(__) => begin
        #=  An explicit function call isn't needed for builtin calls. =#
        if extDecl.language == "builtin"
          return extDecl #TODO. Weird to return void in typed function
        end
        #=  Fetch the cached function.=#
        @match C_FUNCTION(funcs = list(fn)) = getFuncCache(fnNode)
        #=  Check whether we have a single output or not.
        =#
        @assign single_output = listLength(fn.outputs) == 1
        #=  When there's a single array output we can't generate a call on the
        =#
        #=  'output = func(inputs)' form, so print a warning and treat is as
        =#
        #=  though it's not a single output.
        =#
        if single_output && isArray(P_Function.returnType(fn))
          @assign single_output = false
          Error.addSourceMessage(
            Error.EXT_FN_SINGLE_RETURN_ARRAY,
            list(extDecl.language),
            info(fnNode),
          )
        end
        #=  If we have a single output, set the external declaration's output to
        =#
        #=  be a reference to the function's output. Otherwise leave it as empty.
        =#
        if single_output
          @match list(node) = fn.outputs
          @assign ty = getType(node)
          @assign extDecl.outputRef = fromNode(node, ty)
        end
        #=  Generate function arguments from the function's components.
        =#
        @assign comps =
          getComponents(classTree(getClass(fn.node)))
        if arrayLength(comps) > 0
          @assign args = nil
          for c in comps
            @assign comp = component(c)
            if !single_output || direction(comp) != Direction.OUTPUT
              @assign ty = getType(comp)
              @assign exp = CREF_EXPRESSION(
                ty,
                fromNode(c, ty),
              )
              @assign args = _cons(exp, args)
              for i = 1:dimensionCount(ty)
                @assign args = _cons(
                  SIZE_EXPRESSION(
                    exp,
                    SOME(INTEGER_EXPRESSION(i)),
                  ),
                  args,
                )
              end
            end
          end
          @assign extDecl.args = listReverse(args)
        end
        extDecl
      end
    end
  end
  return extDecl
end

function typeComponentSections(c::InstNode, origin::ORIGIN_Type)
  local comp::Component

  @assign comp = component(c)
  return @assign () = begin
    @match comp begin
      TYPED_COMPONENT(__) => begin
        typeClassSections(comp.classInst, origin)
        ()
      end

      _ => begin
        Error.assertion(
          false,
          getInstanceName() + " got uninstantiated component " + name(component),
          sourceInfo(),
        )
        fail()
      end
    end
  end
end

function typeEquation(eq::Equation, origin::ORIGIN_Type)::Equation
  @assign eq = begin
    local cond::Expression
    local e1::Expression
    local e2::Expression
    local e3::Expression
    local ty::M_Type
    local ty1::M_Type
    local ty2::M_Type
    local eqs1::List{Equation}
    local body::List{Equation}
    local tybrs::List{Equation}
    local iterator::InstNode
    local mk::MatchKindType
    local var::VariabilityType
    local bvar::VariabilityType
    local next_origin::Integer
    local info::SourceInfo
    @match eq begin
      EQUATION_EQUALITY(__) => begin
        typeEqualityEquation(eq.lhs, eq.rhs, origin, eq.source)
      end

      EQUATION_CONNECT(__) => begin
        typeConnect(eq.lhs, eq.rhs, origin, eq.source)
      end

      EQUATION_FOR(__) => begin
        @assign info = DAE.emptyElementSource
        if isSome(eq.range)
          @match SOME(e1) = eq.range
          @assign e1 = typeIterator(eq.iterator, e1, origin, structural = true)
        else
          Error.assertion(
            false,
            getInstanceName() + ": missing support for implicit iteration range",
            sourceInfo(),
          )
          fail()
        end
        @assign next_origin = setFlag(origin, ORIGIN_FOR)
        @assign body = List(typeEquation(e, next_origin) for e in eq.body)
        FOR(eq.iterator, SOME(e1), body, eq.source)
      end

      EQUATION_IF(__) => begin
        typeIfEquation(eq.branches, origin, eq.source)
      end

      EQUATION_WHEN(__) => begin
        typeWhenEquation(eq.branches, origin, eq.source)
      end

      EQUATION_ASSERT(__) => begin
        @assign info = DAE.emptyElementSource
        @assign next_origin = setFlag(origin, ORIGIN_ASSERT)
        @assign e1 = typeOperatorArg(
          eq.condition,
          TYPE_BOOLEAN(),
          setFlag(next_origin, ORIGIN_CONDITION),
          "assert",
          "condition",
          1,
          info,
        )
        @assign e2 = typeOperatorArg(
          eq.message,
          TYPE_STRING(),
          next_origin,
          "assert",
          "message",
          2,
          info,
        )
        @assign e3 = typeOperatorArg(
          eq.level,
          ASSERTIONLEVEL_TYPE,
          next_origin,
          "assert",
          "level",
          3,
          info,
        )
        ASSERT(e1, e2, e3, eq.source)
      end

      EQUATION_TERMINATE(__) => begin
        @assign info = DAE.emptyElementSource
        @assign e1 = typeOperatorArg(
          eq.message,
          TYPE_STRING(),
          origin,
          "terminate",
          "message",
          1,
          info,
        )
        TERMINATE(e1, eq.source)
      end

      EQUATION_REINIT(__) => begin
        @assign (e1, e2) = typeReinit(eq.cref, eq.reinitExp, origin, eq.source)
        REINIT(e1, e2, eq.source)
      end

      EQUATION_NORETCALL(__) => begin
        @assign e1 = typeExp(eq.exp, origin, DAE.emptyElementSource)
        NORETCALL(e1, eq.source)
      end

      _ => begin
        eq
      end
    end
  end
  return eq
end

function typeConnect(
  lhsConn::Expression,
  rhsConn::Expression,
  origin::ORIGIN_Type,
  source::DAE.ElementSource,
)::Equation
  local connEq::Equation

  local lhs::Expression
  local rhs::Expression
  local lhs_ty::M_Type
  local rhs_ty::M_Type
  local lhs_var::VariabilityType
  local rhs_var::VariabilityType
  local mk::MatchKind
  local next_origin::Integer
  local info::SourceInfo
  local eql::List{Equation}

  @assign info = ElementSource.getInfo(source)
  #=  Connections may not be used in if-equations unless the conditions are
  =#
  #=  parameter expressions.
  =#
  #=  TODO: Also check for cardinality etc. as per 8.3.3.
  =#
  if flagSet(origin, ORIGIN_NONEXPANDABLE)
    Error.addSourceMessage(
      Error.CONNECT_IN_IF,
      list(
        toString(lhsConn),
        toString(rhsConn),
      ),
      info,
    )
    fail()
  end
  @assign next_origin = setFlag(origin, ORIGIN_CONNECT)
  @assign (lhs, lhs_ty) = typeConnector(lhsConn, next_origin, info)
  @assign (rhs, rhs_ty) = typeConnector(rhsConn, next_origin, info)
  #=  Check that the connectors have matching types, but only if they're not expandable.
  =#
  #=  Expandable connectors can only be type checked after they've been augmented during
  =#
  #=  the connection handling.
  =#
  if !(Type.isExpandableConnector(lhs_ty) || Type.isExpandableConnector(rhs_ty))
    @assign (lhs, rhs, _, mk) =
      matchExpressions(lhs, lhs_ty, rhs, rhs_ty, allowUnknown = true)
    if isIncompatibleMatch(mk)
      Error.addSourceMessage(
        Error.INVALID_CONNECTOR_VARIABLE,
        list(
          toString(lhsConn),
          toString(rhsConn),
        ),
        info,
      )
      fail()
    end
  end
  #=  TODO: Better error message.
  =#
  @assign connEq = CONNECT(lhs, rhs, source)
  return connEq
end

function typeConnector(
  connExp::Expression,
  origin::ORIGIN_Type,
  info::SourceInfo,
)::Tuple{Expression, M_Type}
  local ty::M_Type

  @assign (connExp, ty, _) = typeExp(connExp, origin, info)
  checkConnector(connExp, info)
  return (connExp, ty)
end

function checkConnector(connExp::Expression, info::SourceInfo)
  local cr::ComponentRef
  local subs::List{Subscript}

  return @assign () = begin
    @match connExp begin
      CREF_EXPRESSION(
        cref = cr && CREF(origin = Origin.CREF),
      ) => begin
        if !isConnector(cr.node)
          Error.addSourceMessageAndFail(
            Error.INVALID_CONNECTOR_TYPE,
            list(toString(cr)),
            info,
          )
        end
        if !checkConnectorForm(cr)
          Error.addSourceMessageAndFail(
            Error.INVALID_CONNECTOR_FORM,
            list(toString(cr)),
            info,
          )
        end
        if subscriptsVariability(cr) > Variability.PARAMETER
          @assign subs = subscriptsAllFlat(cr)
          for sub in subs
            if variability(sub) > Variability.PARAMETER
              Error.addSourceMessage(
                Error.CONNECTOR_NON_PARAMETER_SUBSCRIPT,
                list(
                  toString(connExp),
                  toString(sub),
                ),
                info,
              )
              fail()
            end
          end
        end
        ()
      end

      _ => begin
        Error.addSourceMessage(
          Error.INVALID_CONNECTOR_TYPE,
          list(toString(connExp)),
          info,
        )
        fail()
      end
    end
  end
end

""" #= Helper function for checkConnector. Checks that a connector cref uses the
   correct form, i.e. either c1.c2...cn or m.c. =#"""
function checkConnectorForm(cref::ComponentRef, isConnector::Bool = true)::Bool
  local valid::Bool

  @assign valid = begin
    @match cref begin
      CREF(origin = Origin.CREF) => begin
        if isConnector
          checkConnectorForm(cref.restCref, isConnector(cref.node))
        else
          false
        end
      end

      _ => begin
        true
      end
    end
  end
  #=  The only part of the connector reference allowed to not be a
  =#
  #=  non-connector is the very last part.
  =#
  return valid
end

function checkLhsInWhen(exp::Expression)::Bool
  local isValid::Bool

  @assign isValid = begin
    @match exp begin
      CREF_EXPRESSION(__) => begin
        true
      end

      TUPLE_EXPRESSION(__) => begin
        for e in exp.elements
          checkLhsInWhen(e)
        end
        true
      end

      _ => begin
        false
      end
    end
  end
  return isValid
end

function typeAlgorithm(alg::Algorithm, origin::ORIGIN_Type)::Algorithm

  @assign alg.statements = List(typeStatement(s, origin) for s in alg.statements)
  return alg
end

function typeStatements(alg::List{<:Statement}, origin::ORIGIN_Type)::List{Statement}

  @assign alg = List(typeStatement(stmt, origin) for stmt in alg)
  return alg
end

function typeStatement(st::Statement, origin::ORIGIN_Type)::Statement

  @assign st = begin
    local cond::Expression
    local e1::Expression
    local e2::Expression
    local e3::Expression
    local ty1::M_Type
    local ty2::M_Type
    local ty3::M_Type
    local sts1::List{Statement}
    local body::List{Statement}
    local tybrs::List{Tuple{Expression, List{Statement}}}
    local iterator::InstNode
    local mk::MatchKind
    local next_origin::ORIGIN_Type
    local cond_origin::ORIGIN_Type
    local info::SourceInfo
    @match st begin
      P_Statement.Statement.ASSIGNMENT(__) => begin
        @assign info = ElementSource.getInfo(st.source)
        @assign (e1, ty1) =
          typeExp(st.lhs, setFlag(origin, ORIGIN_LHS), info)
        @assign (e2, ty2) =
          typeExp(st.rhs, setFlag(origin, ORIGIN_RHS), info)
        #=  TODO: Should probably only be allowUnknown = true if in a function.
        =#
        @assign (e2, ty3, mk) = matchTypes(ty2, ty1, e2, allowUnknown = true)
        if isIncompatibleMatch(mk)
          Error.addSourceMessage(
            Error.ASSIGN_TYPE_MISMATCH_ERROR,
            list(
              toString(e1),
              toString(e2),
              Type.toString(ty1),
              Type.toString(ty2),
            ),
            info,
          )
          fail()
        end
        P_Statement.Statement.ASSIGNMENT(e1, e2, ty3, st.source)
      end

      P_Statement.Statement.FOR(__) => begin
        @assign info = ElementSource.getInfo(st.source)
        if isSome(st.range)
          @match SOME(e1) = st.range
          @assign e1 = typeIterator(st.iterator, e1, origin, structural = false)
        else
          Error.assertion(
            false,
            getInstanceName() + ": missing support for implicit iteration range",
            sourceInfo(),
          )
          fail()
        end
        @assign next_origin = setFlag(origin, ORIGIN_FOR)
        @assign body = typeStatements(st.body, next_origin)
        P_Statement.Statement.FOR(st.iterator, SOME(e1), body, st.source)
      end

      P_Statement.Statement.IF(__) => begin
        @assign next_origin = setFlag(origin, ORIGIN_IF)
        @assign cond_origin = setFlag(next_origin, ORIGIN_CONDITION)
        @assign tybrs = List(
          begin
            @match br begin
              (cond, body) => begin
                @assign e1 = typeCondition(
                  cond,
                  cond_origin,
                  st.source,
                  Error.IF_CONDITION_TYPE_ERROR,
                )
                @assign sts1 = List(typeStatement(bst, next_origin) for bst in body)
                (e1, sts1)
              end
            end
          end for br in st.branches
        )
        P_Statement.Statement.IF(tybrs, st.source)
      end

      P_Statement.Statement.WHEN(__) => begin
        @assign next_origin = setFlag(origin, ORIGIN_WHEN)
        @assign tybrs = List(
          begin
            @match br begin
              (cond, body) => begin
                @assign e1 = typeCondition(
                  cond,
                  origin,
                  st.source,
                  Error.WHEN_CONDITION_TYPE_ERROR,
                  allowVector = true,
                )
                @assign sts1 = List(typeStatement(bst, next_origin) for bst in body)
                (e1, sts1)
              end
            end
          end for br in st.branches
        )
        P_Statement.Statement.WHEN(tybrs, st.source)
      end

      P_Statement.Statement.ASSERT(__) => begin
        @assign info = ElementSource.getInfo(st.source)
        @assign next_origin = setFlag(origin, ORIGIN_ASSERT)
        @assign e1 = typeOperatorArg(
          st.condition,
          TYPE_BOOLEAN,
          setFlag(next_origin, ORIGIN_CONDITION),
          "assert",
          "condition",
          1,
          info,
        )
        @assign e2 = typeOperatorArg(
          st.message,
          TYPE_STRING(),
          next_origin,
          "assert",
          "message",
          2,
          info,
        )
        @assign e3 = typeOperatorArg(
          st.level,
          ASSERTIONLEVEL_TYPE,
          next_origin,
          "assert",
          "level",
          3,
          info,
        )
        P_Statement.Statement.ASSERT(e1, e2, e3, st.source)
      end

      P_Statement.Statement.TERMINATE(__) => begin
        @assign info = ElementSource.getInfo(st.source)
        #=  terminate is not allowed in a function context.
        =#
        if flagSet(origin, ORIGIN_FUNCTION)
          Error.addSourceMessage(Error.EXP_INVALID_IN_FUNCTION, list("terminate"), info)
          fail()
        end
        @assign e1 = typeOperatorArg(
          st.message,
          TYPE_STRING,
          origin,
          "terminate",
          "message",
          1,
          info,
        )
        P_Statement.Statement.TERMINATE(e1, st.source)
      end

      P_Statement.Statement.NORETCALL(__) => begin
        @assign e1 = typeExp(st.exp, origin, ElementSource.getInfo(st.source))
        P_Statement.Statement.NORETCALL(e1, st.source)
      end

      P_Statement.Statement.WHILE(__) => begin
        @assign e1 = typeCondition(
          st.condition,
          origin,
          st.source,
          Error.WHILE_CONDITION_TYPE_ERROR,
        )
        @assign sts1 = List(typeStatement(bst, origin) for bst in st.body)
        P_Statement.Statement.WHILE(e1, sts1, st.source)
      end

      P_Statement.Statement.FAILURE(__) => begin
        @assign sts1 = List(typeStatement(bst, origin) for bst in st.body)
        P_Statement.Statement.FAILURE(sts1, st.source)
      end

      _ => begin
        st
      end
    end
  end
  return st
end

function typeEqualityEquation(
  lhsExp::Expression,
  rhsExp::Expression,
  origin::ORIGIN_Type,
  source::DAE.ElementSource,
)::Equation
  local eq::Equation
  local info::SourceInfo = sourceInfo()
  local e1::Expression
  local e2::Expression
  local ty1::M_Type
  local ty2::M_Type
  local ty::M_Type
  local mk::MatchKindType

  if flagSet(origin, ORIGIN_WHEN) &&
     flagNotSet(origin, ORIGIN_CLOCKED)
    if checkLhsInWhen(lhsExp)
      fold(lhsExp, Inst.markStructuralParamsSubs, 0)
    else
      Error.addSourceMessage(
        Error.WHEN_EQ_LHS,
        list(toString(lhsExp)),
        info,
      )
      fail()
    end
  end
  @assign (e1, ty1) = typeExp(lhsExp, setFlag(origin, ORIGIN_LHS), info)
  @assign (e2, ty2) = typeExp(rhsExp, setFlag(origin, ORIGIN_RHS), info)
  @assign (e1, e2, ty, mk) = matchExpressions(e1, ty1, e2, ty2)
  if isIncompatibleMatch(mk)
    Error.addSourceMessage(
      Error.EQUATION_TYPE_MISMATCH_ERROR,
      list(
        toString(e1) + " = " + toString(e2),
        Type.toString(ty1) + " = " + Type.toString(ty2),
      ),
      info,
    )
    fail()
  end
  @assign eq = EQUATION_EQUALITY(e1, e2, ty, source)
  return eq
end

function typeCondition(
  condition::Expression,
  origin::ORIGIN_Type,
  source::DAE.ElementSource,
  errorMsg,
  allowVector::Bool = false,
  allowClock::Bool = false,
)::Tuple{Expression, M_Type, Variability}
  local variability::VariabilityType
  local ty::M_Type

  local info::SourceInfo
  local ety::M_Type

  @assign info = ElementSource.getInfo(source)
  @assign (condition, ty, variability) = typeExp(condition, origin, info)
  @assign ety = if allowVector
    arrayElementType(ty)
  else
    ty
  end
  if !(Type.isBoolean(ety) || allowClock && Type.isClock(ety))
    # Error.addSourceMessage(
    #   errorMsg,
    #   list(toString(condition), Type.toString(ty)),
    #   info,
    # )
    fail()
  end
  return (condition, ty, variability)
end

function typeIfEquation(
  branches::List{<:Equation},
  origin::ORIGIN_Type,
  source::DAE.ElementSource,
)::Equation
  local ifEq::Equation

  local cond::Expression
  local eql::List{Equation}
  local accum_var::VariabilityType = Variability.CONSTANT
  local var::VariabilityType
  local bl::List{Equation} = nil
  local bl2::List{Equation} = nil
  local next_origin::Type = setFlag(origin, ORIGIN_IF)
  local cond_origin::Type = setFlag(next_origin, ORIGIN_CONDITION)

  #=  Type the conditions of all the branches.
  =#
  for b in branches
    @match BRANCH(cond, _, eql) = b
    @assign (cond, _, var) =
      typeCondition(cond, cond_origin, source, Error.IF_CONDITION_TYPE_ERROR)
    if var > Variability.PARAMETER || Inst.isExpressionNotFixed(cond, maxDepth = 100)
      @assign next_origin = setFlag(next_origin, ORIGIN_NONEXPANDABLE)
    elseif var == Variability.PARAMETER && accum_var <= Variability.PARAMETER
      @assign var = Variability.STRUCTURAL_PARAMETER
      Inst.markStructuralParamsExp(cond)
    end
    @assign accum_var = variabilityMax(accum_var, var)
    @assign bl = _cons(BRANCH(cond, var, eql), bl)
  end

  for b in bl
    @match BRANCH(cond, var, eql) = b
    ErrorExt.setCheckpoint(getInstanceName())
    try
      @assign eql = list(typeEquation(e, next_origin) for e in eql)
      @assign bl2 = _cons(makeBranch(cond, eql, var), bl2)
    catch
      @assign bl2 = _cons(
        INVALID_BRANCH(
          makeBranch(cond, eql, var),
          ErrorExt.getCheckpointMessages(),
        ),
        bl2,
      )
    end
    ErrorExt.delCheckpoint(getInstanceName())
  end
  #=  Do branch selection anyway if -d=-nfScalarize is set, otherwise turning of
  =#
  #=  scalarization breaks currently.
  =#
  if !Flags.isSet(Flags.NF_SCALARIZE)
    @assign bl = bl2
    @assign bl2 = nil
    for b in bl
      @assign bl2 = begin
        @match b begin
          BRANCH(
            __,
          ) where {(b.conditionVar <= Variability.STRUCTURAL_PARAMETER)} => begin
            @assign b.condition = Ceval.evalExp(b.condition)
            if P_Expression.Expression.isFalse(b.condition)
              bl2
            else
              _cons(b, bl2)
            end
          end

          _ => begin
            _cons(b, bl2)
          end
        end
      end
    end
    @assign bl2 = listReverseInPlace(bl2)
  end
  @assign ifEq = IF(bl2, source)
  return ifEq
end

function isNonConstantIfCondition(exp::Expression)::Bool
  local isConstant::Bool

  @assign isConstant = begin
    local fn::M_Function
    @match exp begin
      CREF_EXPRESSION(__) => begin
        isIterator(exp.cref)
      end

      CALL_EXPRESSION(call = TYPED_CALL(fn = fn)) => begin
        begin
          @match AbsynUtil.pathFirstIdent(fn.path) begin
            "Connections" => begin
              true
            end

            "cardinality" => begin
              true
            end

            _ => begin
              P_Call.isImpure(exp.call)
            end
          end
        end
      end

      _ => begin
        false
      end
    end
  end
  return isConstant
end

function typeWhenEquation(
  branches::List{<:Equation},
  origin::ORIGIN_Type,
  source::DAE.ElementSource,
)::Equation
  local whenEq::Equation

  local next_origin::ORIGIN_Type = setFlag(origin, ORIGIN_WHEN)
  local accum_branches::List{Equation} = nil
  local cond::Expression
  local body::List{Equation}
  local ty::M_Type
  local var::VariabilityType

  for branch in branches
    @match BRANCH(cond, _, body) = branch
    @assign (cond, ty, var) = typeCondition(
      cond,
      origin,
      source,
      Error.WHEN_CONDITION_TYPE_ERROR,
      allowVector = true,
      allowClock = true,
    )
    if Type.isClock(ty)
      if listLength(branches) != 1
        if referenceEq(branch, listHead(branches))
          Error.addSourceMessage(Error.ELSE_WHEN_CLOCK, nil, ElementSource.getInfo(source))
        else
          Error.addSourceMessage(
            Error.CLOCKED_WHEN_BRANCH,
            nil,
            ElementSource.getInfo(source),
          )
        end
        fail()
      else
        @assign next_origin = setFlag(origin, ORIGIN_CLOCKED)
      end
    end
    @assign body = list(typeEquation(eq, next_origin) for eq in body)
    @assign accum_branches =
      _cons(makeBranch(cond, body, var), accum_branches)
  end
  @assign whenEq = WHEN(listReverseInPlace(accum_branches), source)
  return whenEq
end

function typeOperatorArg(
  arg::Expression,
  expectedType::M_Type,
  origin::ORIGIN_Type,
  operatorName::String,
  argName::String,
  argIndex::Integer,
  info::SourceInfo,
)::Expression

  local ty::M_Type
  local mk::MatchKind

  @assign (arg, ty, _) = typeExp(arg, origin, info)
  @assign (arg, _, mk) = matchTypes(ty, expectedType, arg)
  if isIncompatibleMatch(mk)
    Error.addSourceMessage(
      Error.ARG_TYPE_MISMATCH,
      list(
        intString(argIndex),
        operatorName,
        argName,
        toString(arg),
        Type.toString(ty),
        Type.toString(expectedType),
      ),
      info,
    )
    fail()
  end
  return arg
end

function typeReinit(
  crefExp::Expression,
  exp::Expression,
  origin::ORIGIN_Type,
  source::DAE.ElementSource,
)::Tuple{Expression, Expression}

  local var::VariabilityType
  local mk::MatchKind
  local ty1::M_Type
  local ty2::M_Type
  local cref::ComponentRef
  local info::SourceInfo

  @assign info = ElementSource.getInfo(source)
  @assign (crefExp, ty1, _) = typeExp(crefExp, origin, info)
  @assign (exp, ty2, _) = typeExp(exp, origin, info)
  #=  The first argument must be a cref.
  =#
  @assign cref = begin
    @match crefExp begin
      CREF_EXPRESSION(__) => begin
        crefExp.cref
      end

      _ => begin
        Error.addSourceMessage(Error.REINIT_MUST_BE_VAR_OR_ARRAY, nil, info)
        fail()
      end
    end
  end
  #=  The first argument must be a continuous time variable.
  =#
  #=  Check the variability of the cref instead of the variability returned by
  =#
  #=  typeExp, since expressions in when-equations count as discrete.
  =#
  if nodeVariability(cref) < Variability.IMPLICITLY_DISCRETE
    Error.addSourceMessage(
      Error.REINIT_MUST_BE_VAR,
      list(
        toString(crefExp),
        P_Prefixes.variabilityString(nodeVariability(cref)),
      ),
      info,
    )
    fail()
  end
  #=  The first argument must be a subtype of Real.
  =#
  @assign (_, _, mk) =
    matchTypes(arrayElementType(ty1), REAL(), crefExp)
  if isIncompatibleMatch(mk)
    Error.addSourceMessage(
      Error.REINIT_MUST_BE_REAL,
      list(
        toString(crefExp),
        toString(arrayElementType(ty1)),
      ),
      info,
    )
    fail()
  end
  #=  The second argument must be type compatible with the first.
  =#
  @assign (exp, _, mk) = matchTypes(ty2, ty1, exp)
  if isIncompatibleMatch(mk)
    Error.addSourceMessage(
      Error.ARG_TYPE_MISMATCH,
      list(
        "2",
        "reinit",
        "",
        toString(exp),
        toString(ty2),
        toString(ty1),
      ),
      info,
    )
    fail()
  end
  return (crefExp, exp)
end
