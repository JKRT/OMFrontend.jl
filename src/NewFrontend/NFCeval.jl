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
FuncT = Function
FuncT = Function
ReductionFn = Function
Expression = NFExpression
Operator = NFOperator
import ..DAE
#import ..ElementSource

@UniontypeDecl EvalTarget
function getInfo(target::EvalTarget)::SourceInfo
  local info::SourceInfo

  @assign info = begin
    @match target begin
      DIMENSION(__) => begin
        target.info
      end

      ATTRIBUTE(__) => begin
        getInfo(target.binding)
      end

      RANGE(__) => begin
        target.info
      end

      CONDITION(__) => begin
        target.info
      end

      GENERIC(__) => begin
        target.info
      end

      STATEMENT(__) => begin
        ElementSource.getInfo(target.source)
      end

      _ => begin
        AbsynUtil.dummyInfo
      end
    end
  end
  return info
end

function hasInfo(target::EvalTarget)::Bool
  local hasInfo::Bool

  @assign hasInfo = begin
    @match target begin
      DIMENSION(__) => begin
        true
      end

      ATTRIBUTE(__) => begin
        true
      end

      RANGE(__) => begin
        true
      end

      CONDITION(__) => begin
        true
      end

      GENERIC(__) => begin
        true
      end

      STATEMENT(__) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return hasInfo
end

function isRange(target::EvalTarget)::Bool
  local isRange::Bool

  @assign isRange = begin
    @match target begin
      RANGE(__) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return isRange
end

@Uniontype EvalTarget begin
  @Record IGNORE_ERRORS begin
  end

  @Record STATEMENT begin
    source::DAE.ElementSource
  end

  @Record GENERIC begin
    info::SourceInfo
  end

  @Record CONDITION begin
    info::SourceInfo
  end

  @Record RANGE begin
    info::SourceInfo
  end

  @Record ATTRIBUTE begin
    binding::Binding
  end

  @Record DIMENSION begin
    component::InstNode
    index::Integer
    exp::Expression
    info::SourceInfo
  end
end


function evalExp(
  exp::Expression,
  target::EvalTarget = P_EvalTarget.IGNORE_ERRORS(),
)::Expression

  @assign exp = P_Expression.Expression.getBindingExp(evalExp_impl(exp, target))
  return exp
end

function evalExp_impl(exp::Expression, target::EvalTarget)::Expression

  @assign exp = begin
    local c::InstNode
    local binding::Binding
    local exp1::Expression
    local exp2::Expression
    local exp3::Expression
    local expl::List{Expression} = nil
    local call::Call
    local comp::Component
    local oexp::Option{Expression}
    local cref::ComponentRef
    local dim::Dimension
    @match exp begin
      CREF_EXPRESSION(__) => begin
        evalCref(exp.cref, exp, target)
      end

      P_Expression.Expression.TYPENAME(__) => begin
        evalTypename(exp.ty, exp, target)
      end

      P_Expression.Expression.ARRAY(__) => begin
        if exp.literal
          exp
        else
          P_Expression.Expression.makeArray(
            exp.ty,
            List(evalExp_impl(e, target) for e in exp.elements),
            literal = true,
          )
        end
      end

      P_Expression.Expression.RANGE(__) => begin
        evalRange(exp, target)
      end

      TUPLE_EXPRESSION(__) => begin
        @assign exp.elements = List(evalExp_impl(e, target) for e in exp.elements)
        exp
      end

      P_Expression.Expression.RECORD(__) => begin
        @assign exp.elements = List(evalExp_impl(e, target) for e in exp.elements)
        exp
      end

      P_Expression.Expression.CALL(__) => begin
        evalCall(exp.call, target)
      end

      P_Expression.Expression.SIZE(__) => begin
        evalSize(exp.exp, exp.dimIndex, target)
      end

      BINARY_EXPRESSION(__) => begin
        @assign exp1 = evalExp_impl(exp.exp1, target)
        @assign exp2 = evalExp_impl(exp.exp2, target)
        evalBinaryOp(exp1, exp.operator, exp2, target)
      end

      P_Expression.Expression.UNARY(__) => begin
        @assign exp1 = evalExp_impl(exp.exp, target)
        evalUnaryOp(exp1, exp.operator)
      end

      P_Expression.Expression.LBINARY(__) => begin
        @assign exp1 = evalExp_impl(exp.exp1, target)
        evalLogicBinaryOp(exp1, exp.operator, exp.exp2, target)
      end

      P_Expression.Expression.LUNARY(__) => begin
        @assign exp1 = evalExp_impl(exp.exp, target)
        evalLogicUnaryOp(exp1, exp.operator)
      end

      P_Expression.Expression.RELATION(__) => begin
        @assign exp1 = evalExp_impl(exp.exp1, target)
        @assign exp2 = evalExp_impl(exp.exp2, target)
        evalRelationOp(exp1, exp.operator, exp2)
      end

      P_Expression.Expression.IF(__) => begin
        evalIfExp(exp, target)
      end

      P_Expression.Expression.CAST(__) => begin
        @assign exp1 = evalExp_impl(exp.exp, target)
        evalCast(exp1, exp.ty)
      end

      P_Expression.Expression.UNBOX(__) => begin
        @assign exp1 = evalExp_impl(exp.exp, target)
        P_Expression.Expression.UNBOX(exp1, exp.ty)
      end

      P_Expression.Expression.SUBSCRIPTED_EXP(__) => begin
        evalSubscriptedExp(exp.exp, exp.subscripts, target)
      end

      TUPLE_EXPRESSION_ELEMENT(__) => begin
        @assign exp1 = evalExp_impl(exp.tupleExp, target)
        P_Expression.Expression.tupleElement(exp1, exp.ty, exp.index)
      end

      P_Expression.Expression.RECORD_ELEMENT(__) => begin
        evalRecordElement(exp, target)
      end

      P_Expression.Expression.MUTABLE(__) => begin
        @assign exp1 = evalExp_impl(Mutable.access(exp.exp), target)
        exp1
      end

      P_Expression.Expression.BINDING_EXP(__) => begin
        @assign exp.exp = evalExp_impl(exp.exp, target)
        exp
      end

      _ => begin
        exp
      end
    end
  end
  return exp
end

function evalExpOpt(
  oexp::Option{<:Expression},
  target::EvalTarget = P_EvalTarget.IGNORE_ERRORS(),
)::Option{Expression}

  @assign oexp = begin
    local e::Expression
    @match oexp begin
      SOME(e) => begin
        SOME(evalExp_impl(e, target))
      end

      _ => begin
        oexp
      end
    end
  end
  return oexp
end

""" #= Evaluates the parts of an expression that are possible to evaluate. This
   means leaving parts of the expression that contains e.g. iterators or mutable
   expressions. This can be used to optimize an expression that is expected to
   be evaluated many times, for example the expression in an array constructor. =#"""
function evalExpPartial(
  exp::Expression,
  target::EvalTarget = P_EvalTarget.IGNORE_ERRORS(),
  evaluated::Bool = true,
)::Tuple{Expression, Bool}
  local outEvaluated::Bool #= True if the whole expression is evaluated, otherwise false. =#
  local outExp::Expression

  local e::Expression
  local e1::Expression
  local e2::Expression
  local eval1::Bool
  local eval2::Bool

  @assign (e, outEvaluated) = P_Expression.Expression.mapFoldShallow(
    exp,
    (target) -> evalExpPartial(target = target),
    true,
  )
  @assign outExp = begin
    @match e begin
      CREF_EXPRESSION(__) => begin
        if isIterator(e.cref)
          @assign outExp = e
          @assign outEvaluated = false
        else
          @assign outExp = evalCref(e.cref, e, target, evalSubscripts = false)
        end
        #=  Don't evaluate iterators.
        =#
        #=  Crefs can be evaluated even if they have non-evaluated subscripts.
        =#
        outExp
      end

      P_Expression.Expression.MUTABLE(__) => begin
        #=  Don't evaluate mutable expressions. While they could technically be
        =#
        #=  evaluated they're usually used as mutable iterators.
        =#
        @assign outEvaluated = false
        e
      end

      _ => begin
        if outEvaluated
          evalExp(e, target)
        else
          e
        end
      end
    end
  end
  @assign outEvaluated = evaluated && outEvaluated
  return (outExp, outEvaluated) #= True if the whole expression is evaluated, otherwise false. =#
end

function evalCref(
  cref::ComponentRef,
  defaultExp::Expression,
  target::EvalTarget,
  evalSubscripts::Bool = true,
)::Expression
  local exp::Expression

  local c::InstNode
  local evaled::Bool
  local subs::List{Subscript}

  @assign exp = begin
    @match cref begin
      CREF(
        node = c && COMPONENT_NODE(__),
      ) where {(!isIterator(cref))} => begin
        evalComponentBinding(c, cref, defaultExp, target, evalSubscripts)
      end

      _ => begin
        defaultExp
      end
    end
  end
  return exp
end

function evalComponentBinding(
  node::InstNode,
  cref::ComponentRef,
  defaultExp::Expression,
  target::EvalTarget,
  evalSubscripts::Bool = true,
)::Expression #= The expression returned if the binding couldn't be evaluated =#
  local exp::Expression

  local exp_origin::ORIGIN_Type
  local comp::Component
  local binding::Binding
  local evaluated::Bool
  local subs::List{Subscript}
  local var::Variability
  local start_exp::Option{Expression}

  @assign exp_origin = if isFunction(explicitParent(node))
    ExpOrigin.FUNCTION
  else
    ExpOrigin.CLASS
  end
  Typing.typeComponentBinding(node, exp_origin, typeChildren = false)
  @assign comp = component(node)
  @assign binding = P_Component.getBinding(comp)
  if isUnbound(binding)
    @assign binding =
      makeComponentBinding(comp, node, P_Expression.Expression.toCref(defaultExp), target)
    if isUnbound(binding)
      @assign start_exp =
        evalComponentStartBinding(node, comp, cref, target, evalSubscripts)
      if isSome(start_exp)
        @match SOME(exp) = start_exp
        return exp
      end
    end
  end
  #=  In some cases we need to construct a binding for the node, for example when
  =#
  #=  a record has bindings on the fields but not on the record instance as a whole.
  =#
  #=  If we couldn't construct a binding, try to use the start value instead.
  =#
  #=  The component had a valid start value. The value has already been
  =#
  #=  evaluated by evalComponentStartBinding, so skip the rest of the function.
  =#
  @assign (exp, evaluated) = begin
    @match binding begin
      TYPED_BINDING(__) => begin
        if binding.evaluated
          @assign exp = binding.bindingExp
        else
          @assign exp = evalExp_impl(binding.bindingExp, target)
          @assign binding.bindingExp = exp
          @assign binding.evaluated = true
          @assign comp = P_Component.setBinding(binding, comp)
          updateComponent(comp, node)
        end
        (exp, true)
      end

      CEVAL_BINDING(__) => begin
        (binding.bindingExp, true)
      end

      UNBOUND(__) => begin
        printUnboundError(comp, target, defaultExp)
        (defaultExp, false)
      end

      _ => begin
        Error.addInternalError(getInstanceName() + " failed on untyped binding", sourceInfo())
        fail()
      end
    end
  end
  #=  Apply subscripts from the cref to the binding expression as needed.
  =#
  if evaluated
    @assign exp = subscriptEvaluatedBinding(exp, cref, evalSubscripts)
  end
  return exp
end

function flattenBindingExp(exp::Expression)::Expression
  local outExp::Expression

  @assign outExp = begin
    @match exp begin
      P_Expression.Expression.BINDING_EXP(exp = P_Expression.Expression.BINDING_EXP(__)) => begin
        flattenBindingExp(exp.exp)
      end

      _ => begin
        exp
      end
    end
  end
  return outExp
end

""" #= Takes subscripts from the given component reference and applies them to an
   evaluated expression. =#"""
function subscriptEvaluatedBinding(
  exp::Expression,
  cref::ComponentRef,
  evalSubscripts::Bool,
)::Expression

  local subs::List{Subscript}
  local cr::ComponentRef

  #=  The subscripts of the first part of the cref are always applied.
  =#
  @assign subs = getSubscripts(cref)
  @assign cr = stripSubscripts(cref)
  if evalSubscripts
    @assign subs = List(eval(s) for s in subs)
  end
  #=  The rest of the cref contributes subscripts based on where the expressions
  =#
  #=  comes from in the instance tree.
  =#
  @assign exp = subscriptEvaluatedBinding2(exp, cr, evalSubscripts, subs, subs)
  return exp
end

function subscriptEvaluatedBinding2(
  exp::Expression,
  cref::ComponentRef,
  evalSubscripts::Bool,
  subscripts::List{<:Subscript} = nil,
  bindingSubs::List{<:Subscript} = nil,
)::Expression

  @assign exp = begin
    local e::Expression
    local exp_ty::M_Type
    local bind_ty::M_Type
    local parents::List{InstNode}
    local accum_subs::List{Subscript}
    local subs::List{Subscript}
    local cr::ComponentRef
    local cr_node::InstNode
    @match exp begin
      P_Expression.Expression.BINDING_EXP(bindingType = bind_ty, parents = parents) =>
        begin
          if exp.isEach
            @assign parents = list(listHead(parents))
          end
          @assign cr = cref
          @assign accum_subs = subscripts
          @assign subs = nil
          if !isEmpty(cr)
            @assign cr_node = node(cr)
            while !(listEmpty(parents) || refEqual(listHead(parents), cr_node))
              @assign parents = listRest(parents)
            end
            while !listEmpty(parents)
              if !refEqual(listHead(parents), cr_node)
                break
              end
              @assign subs =
                listAppend(getSubscripts(cr), subs)
              @assign parents = listRest(parents)
              @assign cr = rest(cr)
              if isEmpty(cr)
                break
              end
              @assign cr_node = node(cr)
            end
            if evalSubscripts
              @assign subs = List(eval(s) for s in subs)
            end
            @assign accum_subs = listAppend(subs, accum_subs)
          end
          #=  Remove binding parents until we find one referring to the first
          =#
          #=  cref node, or we run out of parents.
          =#
          #=  Collect subscripts from the part of the cref corresponding to the
          =#
          #=  remaining parents.
          =#
          #=  Subscript the binding type if bindingSubs was given.
          =#
          if !listEmpty(bindingSubs)
            @assign subs = bindingSubs
            @assign bind_ty = Type.subscript(bind_ty, subs)
          end
          @assign e =
            subscriptEvaluatedBinding2(exp.exp, cr, evalSubscripts, accum_subs, subs)
          @assign exp_ty = P_Expression.Expression.typeOf(e)
          P_Expression.Expression.BINDING_EXP(e, exp_ty, bind_ty, exp.parents, exp.isEach)
        end

      _ => begin
        P_Expression.Expression.applySubscripts(subscripts, exp)
      end
    end
  end
  return exp
end

""" #= Tries to evaluate the given component's start value. NONE() is returned if
   the component isn't a fixed parameter or if it doesn't have a start value.
   Otherwise the evaluated binding expression is returned if it could be
   evaluated, or the function will fail if it couldn't be. =#"""
function evalComponentStartBinding(
  node::InstNode,
  comp::Component,
  cref::ComponentRef,
  target::EvalTarget,
  evalSubscripts::Bool,
)::Option{Expression}
  local outExp::Option{Expression} = NONE()

  local var::Variability
  local start_node::InstNode
  local start_comp::Component
  local binding::Binding
  local exp::Expression
  local subs::List{Subscript}
  local pcount::Integer

  #=  Only use the start value if the component is a fixed parameter.
  =#
  @assign var = P_Component.variability(comp)
  if var != Variability.PARAMETER && var != Variability.STRUCTURAL_PARAMETER ||
     !P_Component.getFixedAttribute(comp)
    return outExp
  end
  #=  Look up \"start\" in the class.
  =#
  try
    @assign start_node = lookupElement("start", getClass(node))
  catch
    return outExp
  end
  #=  Make sure we have an actual start attribute, and didn't just find some
  =#
  #=  other element named start in the class.
  =#
  @assign start_comp = component(start_node)
  if !P_Component.isTypeAttribute(start_comp)
    return outExp
  end
  #=  Try to evaluate the binding if one exists.
  =#
  @assign binding = P_Component.getBinding(start_comp)
  @assign outExp = begin
    @match binding begin
      TYPED_BINDING(__) => begin
        @assign exp = evalExp_impl(binding.bindingExp, target)
        if !referenceEq(exp, binding.bindingExp)
          @assign binding.bindingExp = exp
          @assign start_comp = P_Component.setBinding(binding, start_comp)
          updateComponent(start_comp, start_node)
        end
        SOME(exp)
      end

      _ => begin
        outExp
      end
    end
  end
  return outExp
end

function makeComponentBinding(
  component::Component,
  node::InstNode,
  cref::ComponentRef,
  target::EvalTarget,
)::Binding
  local binding::Binding

  local tree::ClassTree
  local comps::Array{InstNode}
  local fields::List{Expression}
  local ty::M_Type
  local exp_ty::M_Type
  local rec_node::InstNode
  local exp::Expression
  local rest_cr::ComponentRef

  @assign binding = begin
    @matchcontinue (component, cref) begin
      (
        P_Component.TYPED_COMPONENT(
          ty = TYPE_COMPLEX(complexTy = ComplexType.RECORD(rec_node)),
        ),
        _,
      ) => begin
        #=  A record component without an explicit binding, create one from its children.
        =#
        @assign exp =
          makeRecordBindingExp(component.classInst, rec_node, component.ty, cref)
        @assign exp_ty = P_Expression.Expression.typeOf(exp)
        @assign exp =
          P_Expression.Expression.BINDING_EXP(exp, exp_ty, exp_ty, list(node), true)
        @assign binding = CEVAL_BINDING(exp)
        if !hasSubscripts(cref)
          updateComponent(P_Component.setBinding(binding, component), node)
        end
        binding
      end

      (
        P_Component.TYPED_COMPONENT(
          ty = ty && Type.ARRAY(
            elementType = TYPE_COMPLEX(complexTy = ComplexType.RECORD(rec_node)),
          ),
        ),
        _,
      ) => begin
        #=  A record array component without an explicit binding, create one from its children.
        =#
        @assign exp =
          makeRecordBindingExp(component.classInst, rec_node, component.ty, cref)
        @assign exp = splitRecordArrayExp(exp)
        @assign exp_ty = P_Expression.Expression.typeOf(exp)
        @assign exp =
          P_Expression.Expression.BINDING_EXP(exp, exp_ty, exp_ty, list(node), true)
        @assign binding = CEVAL_BINDING(exp)
        if !hasSubscripts(cref)
          updateComponent(P_Component.setBinding(binding, component), node)
        end
        binding
      end

      (_, _) => begin
        #=  A record field without an explicit binding, evaluate the parent's binding
        =#
        #=  if it has one and fetch the binding from it instead.
        =#
        @assign exp = makeRecordFieldBindingFromParent(cref, target)
        CEVAL_BINDING(exp)
      end

      _ => begin
        NFBinding.EMPTY_BINDING
      end
    end
  end
  return binding
end

function makeRecordFieldBindingFromParent(
  cref::ComponentRef,
  target::EvalTarget,
)::Expression
  local exp::Expression

  local parent_cr::ComponentRef
  local parent_ty::M_Type

  @assign parent_cr = rest(cref)
  @assign parent_ty = nodeType(parent_cr)
  @match true = Type.isRecord(Type.arrayElementType(parent_ty))
  try
    @assign exp = evalCref(parent_cr, P_Expression.Expression.EMPTY(parent_ty), target)
  catch
    @assign exp = makeRecordFieldBindingFromParent(parent_cr, target)
  end
  #=  Pass an EMPTY expression here as the default expression instead of the
  =#
  #=  cref. Otherwise evalCref might attempt to make a binding for the parent
  =#
  #=  from its children, which would create an evaluation loop.
  =#
  #=  If the parent didn't have a binding, try the parent's parent.
  =#
  @assign exp =
    P_Expression.Expression.recordElement(firstName(cref), exp)
  return exp
end

function makeRecordBindingExp(
  typeNode::InstNode,
  recordNode::InstNode,
  recordType::M_Type,
  cref::ComponentRef,
)::Expression
  local exp::Expression

  local tree::ClassTree
  local comps::Array{InstNode}
  local args::List{Expression}
  local fields::List{Record.P_Field}
  local ty::M_Type
  local c::InstNode
  local cr::ComponentRef
  local arg::Expression

  @assign tree = classTree(getClass(typeNode))
  @assign comps = getComponents(tree)
  @assign args = nil
  for i = arrayLength(comps):(-1):1
    @assign c = comps[i]
    @assign ty = getType(c)
    @assign cr =
      CREF(c, nil, ty, P_NFComponentRef.Origin.CREF, cref)
    @assign arg = CREF_EXPRESSION(ty, cr)
    if P_Component.variability(component(c)) <= Variability.PARAMETER
      @assign arg = evalExp_impl(arg, P_EvalTarget.IGNORE_ERRORS())
    end
    @assign args = _cons(arg, args)
  end
  @assign exp =
    P_Expression.Expression.makeRecord(scopePath(recordNode), recordType, args)
  return exp
end

function splitRecordArrayExp(exp::Expression)::Expression

  local path::Absyn.Path
  local ty::M_Type
  local expl::List{Expression}

  @match P_Expression.Expression.RECORD(path, ty, expl) = exp
  @assign exp = P_Expression.Expression.makeRecord(path, Type.arrayElementType(ty), expl)
  @assign exp = P_Expression.Expression.fillType(ty, exp)
  return exp
end

function evalTypename(ty::M_Type, originExp::Expression, target::EvalTarget)::Expression
  local exp::Expression

  #=  Only expand the typename into an array if it's used as a range, and keep
  =#
  #=  them as typenames when used as e.g. dimensions.
  =#
  @assign exp = if P_EvalTarget.isRange(target)
    P_ExpandExp.ExpandExp.expandTypename(ty)
  else
    originExp
  end
  return exp
end

function evalRange(rangeExp::Expression, target::EvalTarget)::Expression
  local result::Expression

  local ty::M_Type
  local start_exp::Expression
  local stop_exp::Expression
  local step_exp::Option{Expression}
  local max_prop_exp::Expression
  local max_prop_count::Integer

  @match P_Expression.Expression.RANGE(
    ty = ty,
    start = start_exp,
    step = step_exp,
    stop = stop_exp,
  ) = rangeExp
  @assign start_exp = evalExp(start_exp, target)
  @assign step_exp = evalExpOpt(step_exp, target)
  @assign stop_exp = evalExp(stop_exp, target)
  if P_EvalTarget.isRange(target)
    @assign ty = TypeCheck.getRangeType(
      start_exp,
      step_exp,
      stop_exp,
      Type.arrayElementType(ty),
      P_EvalTarget.getInfo(target),
    )
    @assign result = P_Expression.Expression.RANGE(ty, start_exp, step_exp, stop_exp)
  else
    @assign result = P_Expression.Expression.RANGE(ty, start_exp, step_exp, stop_exp)
    @assign result = P_Expression.Expression.bindingExpMap(result, evalRangeExp)
  end
  return result
end

function evalRangeExp(rangeExp::Expression)::Expression
  local exp::Expression

  local start::Expression
  local step::Expression
  local stop::Expression
  local opt_step::Option{Expression}
  local expl::List{Expression}
  local ty::M_Type
  local literals::List{String}
  local istep::Integer

  @match P_Expression.Expression.RANGE(start = start, step = opt_step, stop = stop) =
    rangeExp
  if isSome(opt_step)
    @match SOME(step) = opt_step
    @assign (ty, expl) = begin
      @match (start, step, stop) begin
        (
          P_Expression.Expression.INTEGER(__),
          P_Expression.Expression.INTEGER(istep),
          P_Expression.Expression.INTEGER(__),
        ) => begin
          #=  The compiler decided to randomly dislike using step.value here, hence istep.
          =#
          @assign expl = List(
            P_Expression.Expression.INTEGER(i) for i = (start.value):istep:(stop.value)
          )
          (TYPE_INTEGER(), expl)
        end

        (
          P_Expression.Expression.REAL(__),
          P_Expression.Expression.REAL(__),
          P_Expression.Expression.REAL(__),
        ) => begin
          @assign expl = evalRangeReal(start.value, step.value, stop.value)
          (TYPE_REAL(), expl)
        end

        _ => begin
          printWrongArgsError(getInstanceName(), list(start, step, stop), sourceInfo())
          fail()
        end
      end
    end
  else
    @assign (ty, expl) = begin
      @match (start, stop) begin
        (P_Expression.Expression.INTEGER(__), P_Expression.Expression.INTEGER(__)) =>
          begin
            @assign expl =
              List(P_Expression.Expression.INTEGER(i) for i = (start.value):(stop.value))
            (TYPE_INTEGER(), expl)
          end

        (P_Expression.Expression.REAL(__), P_Expression.Expression.REAL(__)) => begin
          @assign expl = evalRangeReal(start.value, 1.0, stop.value)
          (TYPE_REAL(), expl)
        end

        (P_Expression.Expression.BOOLEAN(__), P_Expression.Expression.BOOLEAN(__)) =>
          begin
            @assign expl =
              List(P_Expression.Expression.BOOLEAN(b) for b = (start.value):(stop.value))
            (TYPE_BOOLEAN(), expl)
          end

        (
          P_Expression.Expression.ENUM_LITERAL(ty = ty && TYPE_ENUMERATION(__)),
          P_Expression.Expression.ENUM_LITERAL(__),
        ) => begin
          @assign expl = List(
            P_Expression.Expression.ENUM_LITERAL(ty, listGet(ty.literals, i), i)
            for i = (start.index):(stop.index)
          )
          (ty, expl)
        end

        _ => begin
          printWrongArgsError(getInstanceName(), list(start, stop), sourceInfo())
          fail()
        end
      end
    end
  end
  @assign exp = P_Expression.Expression.makeArray(
    Type.ARRAY(ty, list(P_Dimension.Dimension.fromInteger(listLength(expl)))),
    expl,
    literal = true,
  )
  return exp
end

function evalRangeReal(
  start::AbstractFloat,
  step::AbstractFloat,
  stop::AbstractFloat,
)::List{Expression}
  local result::List{Expression}

  local steps::Integer

  @assign steps = Util.realRangeSize(start, step, stop)
  #=  Real ranges are tricky, make sure that start and stop are reproduced
  =#
  #=  exactly if they are part of the range.
  =#
  if steps == 0
    @assign result = nil
  elseif steps == 1
    @assign result = list(P_Expression.Expression.REAL(start))
  else
    @assign result = list(P_Expression.Expression.REAL(stop))
    for i = (steps - 2):(-1):1
      @assign result = _cons(P_Expression.Expression.REAL(start + i * step), result)
    end
    @assign result = _cons(P_Expression.Expression.REAL(start), result)
  end
  return result
end

function printFailedEvalError(name::String, exp::Expression, info::SourceInfo)
  return Error.addInternalError(
    name + " failed to evaluate ‘" + P_Expression.Expression.toString(exp) + "‘",
    info,
  )
end

function evalBinaryOp(
  exp1::Expression,
  op::Operator,
  exp2::Expression,
  target::EvalTarget = P_EvalTarget.IGNORE_ERRORS(),
)::Expression
  local exp::Expression

  local max_prop_exp::Expression
  local max_prop_count::Integer

  @assign (max_prop_exp, max_prop_count) =
    P_Expression.Expression.mostPropagatedSubExpBinary(exp1, exp2)
  if max_prop_count >= 0
    @assign exp = P_Expression.Expression.bindingExpMap2(
      BINARY_EXPRESSION(exp1, op, exp2),
      (target) -> evalBinaryExp(target = target),
      max_prop_count,
      max_prop_exp,
    )
  else
    @assign exp = evalBinaryOp_dispatch(exp1, op, exp2, target)
  end
  return exp
end

function evalBinaryExp(binaryExp::Expression, target::EvalTarget)::Expression
  local result::Expression

  local e1::Expression
  local e2::Expression
  local op::Operator

  @match BINARY_EXPRESSION(exp1 = e1, operator = op, exp2 = e2) = binaryExp
  @assign result = evalBinaryOp_dispatch(e1, op, e2, target)
  return result
end

function evalBinaryOp_dispatch(
  exp1::Expression,
  op::Operator,
  exp2::Expression,
  target::EvalTarget = P_EvalTarget.IGNORE_ERRORS(),
)::Expression
  local exp::Expression

  @assign exp = begin
    @match op.op begin
      Op.ADD => begin
        evalBinaryAdd(exp1, exp2)
      end

      Op.SUB => begin
        evalBinarySub(exp1, exp2)
      end

      Op.MUL => begin
        evalBinaryMul(exp1, exp2)
      end

      Op.DIV => begin
        evalBinaryDiv(exp1, exp2, target)
      end

      Op.POW => begin
        evalBinaryPow(exp1, exp2)
      end

      Op.ADD_SCALAR_ARRAY => begin
        evalBinaryScalarArray(exp1, exp2, evalBinaryAdd)
      end

      Op.ADD_ARRAY_SCALAR => begin
        evalBinaryArrayScalar(exp1, exp2, evalBinaryAdd)
      end

      Op.SUB_SCALAR_ARRAY => begin
        evalBinaryScalarArray(exp1, exp2, evalBinarySub)
      end

      Op.SUB_ARRAY_SCALAR => begin
        evalBinaryArrayScalar(exp1, exp2, evalBinarySub)
      end

      Op.MUL_SCALAR_ARRAY => begin
        evalBinaryScalarArray(exp1, exp2, evalBinaryMul)
      end

      Op.MUL_ARRAY_SCALAR => begin
        evalBinaryArrayScalar(exp1, exp2, evalBinaryMul)
      end

      Op.MUL_VECTOR_MATRIX => begin
        evalBinaryMulVectorMatrix(exp1, exp2)
      end

      Op.MUL_MATRIX_VECTOR => begin
        evalBinaryMulMatrixVector(exp1, exp2)
      end

      Op.SCALAR_PRODUCT => begin
        evalBinaryScalarProduct(exp1, exp2)
      end

      Op.MATRIX_PRODUCT => begin
        evalBinaryMatrixProduct(exp1, exp2)
      end

      Op.DIV_SCALAR_ARRAY => begin
        evalBinaryScalarArray(exp1, exp2, (target) -> evalBinaryDiv(target = target))
      end

      Op.DIV_ARRAY_SCALAR => begin
        evalBinaryArrayScalar(exp1, exp2, (target) -> evalBinaryDiv(target = target))
      end

      Op.POW_SCALAR_ARRAY => begin
        evalBinaryScalarArray(exp1, exp2, evalBinaryPow)
      end

      Op.POW_ARRAY_SCALAR => begin
        evalBinaryArrayScalar(exp1, exp2, evalBinaryPow)
      end

      Op.POW_MATRIX => begin
        evalBinaryPowMatrix(exp1, exp2)
      end

      _ => begin
        Error.addInternalError(
          getInstanceName() +
          ": unimplemented case for " +
          P_Expression.Expression.toString(BINARY_EXPRESSION(exp1, op, exp2)),
          sourceInfo(),
        )
        fail()
      end
    end
  end
  return exp
end

function evalBinaryAdd(exp1::Expression, exp2::Expression)::Expression
  local exp::Expression

  @assign exp = begin
    @match (exp1, exp2) begin
      (P_Expression.Expression.INTEGER(__), P_Expression.Expression.INTEGER(__)) => begin
        P_Expression.Expression.INTEGER(exp1.value + exp2.value)
      end

      (P_Expression.Expression.REAL(__), P_Expression.Expression.REAL(__)) => begin
        P_Expression.Expression.REAL(exp1.value + exp2.value)
      end

      (P_Expression.Expression.STRING(__), P_Expression.Expression.STRING(__)) => begin
        P_Expression.Expression.STRING(exp1.value + exp2.value)
      end

      (
        P_Expression.Expression.ARRAY(__),
        P_Expression.Expression.ARRAY(__),
      ) where {(listLength(exp1.elements) == listLength(exp2.elements))} => begin
        P_Expression.Expression.makeArray(
          exp1.ty,
          List(@do_threaded_for evalBinaryAdd(e1, e2) (e1, e2) (
            exp1.elements,
            exp2.elements,
          )),
          literal = true,
        )
      end

      _ => begin
        @assign exp = BINARY_EXPRESSION(
          exp1,
          P_Operator.Operator.makeAdd(TYPE_UNKNOWN()),
          exp2,
        )
        printFailedEvalError(getInstanceName(), exp, sourceInfo())
        fail()
      end
    end
  end
  return exp
end

function evalBinarySub(exp1::Expression, exp2::Expression)::Expression
  local exp::Expression

  @assign exp = begin
    @match (exp1, exp2) begin
      (P_Expression.Expression.INTEGER(__), P_Expression.Expression.INTEGER(__)) => begin
        P_Expression.Expression.INTEGER(exp1.value - exp2.value)
      end

      (P_Expression.Expression.REAL(__), P_Expression.Expression.REAL(__)) => begin
        P_Expression.Expression.REAL(exp1.value - exp2.value)
      end

      (
        P_Expression.Expression.ARRAY(__),
        P_Expression.Expression.ARRAY(__),
      ) where {(listLength(exp1.elements) == listLength(exp2.elements))} => begin
        P_Expression.Expression.makeArray(
          exp1.ty,
          List(@do_threaded_for evalBinarySub(e1, e2) (e1, e2) (
            exp1.elements,
            exp2.elements,
          )),
          literal = true,
        )
      end

      _ => begin
        @assign exp = BINARY_EXPRESSION(
          exp1,
          P_Operator.Operator.makeSub(TYPE_UNKNOWN()),
          exp2,
        )
        printFailedEvalError(getInstanceName(), exp, sourceInfo())
        fail()
      end
    end
  end
  return exp
end

function evalBinaryMul(exp1::Expression, exp2::Expression)::Expression
  local exp::Expression

  @assign exp = begin
    @match (exp1, exp2) begin
      (P_Expression.Expression.INTEGER(__), P_Expression.Expression.INTEGER(__)) => begin
        P_Expression.Expression.INTEGER(exp1.value * exp2.value)
      end

      (P_Expression.Expression.REAL(__), P_Expression.Expression.REAL(__)) => begin
        P_Expression.Expression.REAL(exp1.value * exp2.value)
      end

      (
        P_Expression.Expression.ARRAY(__),
        P_Expression.Expression.ARRAY(__),
      ) where {(listLength(exp1.elements) == listLength(exp2.elements))} => begin
        P_Expression.Expression.makeArray(
          exp1.ty,
          List(@do_threaded_for evalBinaryMul(e1, e2) (e1, e2) (
            exp1.elements,
            exp2.elements,
          )),
          literal = true,
        )
      end

      _ => begin
        @assign exp = BINARY_EXPRESSION(
          exp1,
          P_Operator.Operator.makeMul(TYPE_UNKNOWN()),
          exp2,
        )
        printFailedEvalError(getInstanceName(), exp, sourceInfo())
        fail()
      end
    end
  end
  return exp
end

function evalBinaryDiv(exp1::Expression, exp2::Expression, target::EvalTarget)::Expression
  local exp::Expression

  @assign exp = begin
    @match (exp1, exp2) begin
      (_, P_Expression.Expression.REAL(0.0)) => begin
        if P_EvalTarget.hasInfo(target)
          Error.addSourceMessage(
            Error.DIVISION_BY_ZERO,
            list(
              P_Expression.Expression.toString(exp1),
              P_Expression.Expression.toString(exp2),
            ),
            P_EvalTarget.getInfo(target),
          )
          fail()
        else
          @assign exp = BINARY_EXPRESSION(
            exp1,
            P_Operator.Operator.makeDiv(TYPE_REAL()),
            exp2,
          )
        end
        exp
      end

      (P_Expression.Expression.REAL(__), P_Expression.Expression.REAL(__)) => begin
        P_Expression.Expression.REAL(exp1.value / exp2.value)
      end

      (
        P_Expression.Expression.ARRAY(__),
        P_Expression.Expression.ARRAY(__),
      ) where {(listLength(exp1.elements) == listLength(exp2.elements))} => begin
        P_Expression.Expression.makeArray(
          exp1.ty,
          List(@do_threaded_for evalBinaryDiv(e1, e2, target) (e1, e2) (
            exp1.elements,
            exp2.elements,
          )),
          literal = true,
        )
      end

      _ => begin
        @assign exp = BINARY_EXPRESSION(
          exp1,
          P_Operator.Operator.makeDiv(TYPE_UNKNOWN()),
          exp2,
        )
        printFailedEvalError(getInstanceName(), exp, sourceInfo())
        fail()
      end
    end
  end
  return exp
end

function evalBinaryPow(exp1::Expression, exp2::Expression)::Expression
  local exp::Expression

  @assign exp = begin
    @match (exp1, exp2) begin
      (P_Expression.Expression.REAL(__), P_Expression.Expression.REAL(__)) => begin
        P_Expression.Expression.REAL(exp1.value^exp2.value)
      end

      (
        P_Expression.Expression.ARRAY(__),
        P_Expression.Expression.ARRAY(__),
      ) where {(listLength(exp1.elements) == listLength(exp2.elements))} => begin
        P_Expression.Expression.makeArray(
          exp1.ty,
          List(@do_threaded_for evalBinaryPow(e1, e2) (e1, e2) (
            exp1.elements,
            exp2.elements,
          )),
          literal = true,
        )
      end

      _ => begin
        @assign exp = BINARY_EXPRESSION(
          exp1,
          P_Operator.Operator.makePow(TYPE_UNKNOWN()),
          exp2,
        )
        printFailedEvalError(getInstanceName(), exp, sourceInfo())
        fail()
      end
    end
  end
  return exp
end

function evalBinaryScalarArray(
  scalarExp::Expression,
  arrayExp::Expression,
  opFunc::FuncT,
)::Expression
  local exp::Expression

  @assign exp = begin
    @match arrayExp begin
      P_Expression.Expression.ARRAY(__) => begin
        P_Expression.Expression.makeArray(
          arrayExp.ty,
          List(evalBinaryScalarArray(scalarExp, e, opFunc) for e in arrayExp.elements),
          literal = true,
        )
      end

      _ => begin
        opFunc(scalarExp, arrayExp)
      end
    end
  end
  return exp
end

function evalBinaryArrayScalar(
  arrayExp::Expression,
  scalarExp::Expression,
  opFunc::FuncT,
)::Expression
  local exp::Expression

  @assign exp = begin
    @match arrayExp begin
      P_Expression.Expression.ARRAY(__) => begin
        P_Expression.Expression.ARRAY(
          arrayExp.ty,
          List(evalBinaryArrayScalar(e, scalarExp, opFunc) for e in arrayExp.elements),
          literal = true,
        )
      end

      _ => begin
        opFunc(arrayExp, scalarExp)
      end
    end
  end
  return exp
end

function evalBinaryMulVectorMatrix(vectorExp::Expression, matrixExp::Expression)::Expression
  local exp::Expression

  local expl::List{Expression}
  local m::Dimension
  local ty::M_Type

  @assign exp = begin
    @match P_Expression.Expression.transposeArray(matrixExp) begin
      P_Expression.Expression.ARRAY(Type.ARRAY(ty, m <| _ <| nil()), expl) => begin
        @assign expl = List(evalBinaryScalarProduct(vectorExp, e) for e in expl)
        P_Expression.Expression.makeArray(Type.ARRAY(ty, list(m)), expl, literal = true)
      end

      _ => begin
        @assign exp = BINARY_EXPRESSION(
          vectorExp,
          P_Operator.Operator.makeMul(TYPE_UNKNOWN()),
          matrixExp,
        )
        printFailedEvalError(getInstanceName(), exp, sourceInfo())
        fail()
      end
    end
  end
  return exp
end

function evalBinaryMulMatrixVector(matrixExp::Expression, vectorExp::Expression)::Expression
  local exp::Expression

  local expl::List{Expression}
  local n::Dimension
  local ty::M_Type

  @assign exp = begin
    @match matrixExp begin
      P_Expression.Expression.ARRAY(Type.ARRAY(ty, n <| _ <| nil()), expl) => begin
        @assign expl = List(evalBinaryScalarProduct(e, vectorExp) for e in expl)
        P_Expression.Expression.makeArray(Type.ARRAY(ty, list(n)), expl, literal = true)
      end

      _ => begin
        @assign exp = BINARY_EXPRESSION(
          matrixExp,
          P_Operator.Operator.makeMul(TYPE_UNKNOWN()),
          vectorExp,
        )
        printFailedEvalError(getInstanceName(), exp, sourceInfo())
        fail()
      end
    end
  end
  return exp
end

function evalBinaryScalarProduct(exp1::Expression, exp2::Expression)::Expression
  local exp::Expression

  @assign exp = begin
    local elem_ty::M_Type
    local e2::Expression
    local rest_e2::List{Expression}
    @match (exp1, exp2) begin
      (
        P_Expression.Expression.ARRAY(ty = Type.ARRAY(elem_ty)),
        P_Expression.Expression.ARRAY(__),
      ) where {(listLength(exp1.elements) == listLength(exp2.elements))} => begin
        @assign exp = P_Expression.Expression.makeZero(elem_ty)
        @assign rest_e2 = exp2.elements
        for e1 in exp1.elements
          @match _cons(e2, rest_e2) = rest_e2
          @assign exp = evalBinaryAdd(exp, evalBinaryMul(e1, e2))
        end
        exp
      end

      _ => begin
        @assign exp = BINARY_EXPRESSION(
          exp1,
          P_Operator.Operator.makeMul(TYPE_UNKNOWN()),
          exp2,
        )
        printFailedEvalError(getInstanceName(), exp, sourceInfo())
        fail()
      end
    end
  end
  return exp
end

function evalBinaryMatrixProduct(exp1::Expression, exp2::Expression)::Expression
  local exp::Expression

  local e2::Expression
  local expl1::List{Expression}
  local expl2::List{Expression}
  local elem_ty::M_Type
  local row_ty::M_Type
  local mat_ty::M_Type
  local n::Dimension
  local p::Dimension

  @assign e2 = P_Expression.Expression.transposeArray(exp2)
  @assign exp = begin
    @match (exp1, e2) begin
      (
        P_Expression.Expression.ARRAY(Type.ARRAY(elem_ty, n <| _ <| nil()), expl1),
        P_Expression.Expression.ARRAY(Type.ARRAY(_, p <| _ <| nil()), expl2),
      ) => begin
        @assign mat_ty = Type.ARRAY(elem_ty, list(n, p))
        if listEmpty(expl2)
          @assign exp = P_Expression.Expression.makeZero(mat_ty)
        else
          @assign row_ty = Type.ARRAY(elem_ty, list(p))
          @assign expl1 = List(
            P_Expression.Expression.makeArray(
              row_ty,
              List(evalBinaryScalarProduct(r, c) for c in expl2),
              literal = true,
            ) for r in expl1
          )
          @assign exp = P_Expression.Expression.makeArray(mat_ty, expl1, literal = true)
        end
        exp
      end

      _ => begin
        @assign exp = BINARY_EXPRESSION(
          exp1,
          P_Operator.Operator.makeMul(TYPE_UNKNOWN()),
          exp2,
        )
        printFailedEvalError(getInstanceName(), exp, sourceInfo())
        fail()
      end
    end
  end
  return exp
end

function evalBinaryPowMatrix(matrixExp::Expression, nExp::Expression)::Expression
  local exp::Expression

  local n::Integer

  @assign exp = begin
    @match (matrixExp, nExp) begin
      (P_Expression.Expression.ARRAY(__), P_Expression.Expression.INTEGER(value = 0)) =>
        begin
          @assign n = P_Dimension.Dimension.size(listHead(Type.arrayDims(matrixExp.ty)))
          P_Expression.Expression.makeIdentityMatrix(n, TYPE_REAL())
        end

      (_, P_Expression.Expression.INTEGER(value = n)) => begin
        evalBinaryPowMatrix2(matrixExp, n)
      end

      _ => begin
        @assign exp = BINARY_EXPRESSION(
          matrixExp,
          P_Operator.Operator.makePow(TYPE_UNKNOWN()),
          nExp,
        )
        printFailedEvalError(getInstanceName(), exp, sourceInfo())
        fail()
      end
    end
  end
  return exp
end

function evalBinaryPowMatrix2(matrix::Expression, n::Integer)::Expression
  local exp::Expression

  @assign exp = begin
    @match n begin
      1 => begin
        matrix
      end

      2 => begin
        evalBinaryMatrixProduct(matrix, matrix)
      end

      _ where {(intMod(n, 2) == 0)} => begin
        #=  A^1 = A
        =#
        #=  A^2 = A * A
        =#
        #=  A^n = A^m * A^m where n = 2*m
        =#
        @assign exp = evalBinaryPowMatrix2(matrix, intDiv(n, 2))
        evalBinaryMatrixProduct(exp, exp)
      end

      _ => begin
        #=  A^n = A * A^(n-1)
        =#
        @assign exp = evalBinaryPowMatrix2(matrix, n - 1)
        evalBinaryMatrixProduct(matrix, exp)
      end
    end
  end
  return exp
end

function evalUnaryOp(exp1::Expression, op::Operator)::Expression
  local exp::Expression

  @assign exp = begin
    @match op.op begin
      Op.UMINUS => begin
        P_Expression.Expression.bindingExpMap(exp1, evalUnaryMinus)
      end

      _ => begin
        Error.addInternalError(
          getInstanceName() +
          ": unimplemented case for " +
          P_Expression.Expression.toString(P_Expression.Expression.UNARY(op, exp1)),
          sourceInfo(),
        )
        fail()
      end
    end
  end
  return exp
end

function evalUnaryMinus(exp1::Expression)::Expression
  local exp::Expression

  @assign exp = begin
    @match exp1 begin
      P_Expression.Expression.INTEGER(__) => begin
        P_Expression.Expression.INTEGER(-exp1.value)
      end

      P_Expression.Expression.REAL(__) => begin
        P_Expression.Expression.REAL(-exp1.value)
      end

      P_Expression.Expression.ARRAY(__) => begin
        @assign exp1.elements = List(evalUnaryMinus(e) for e in exp1.elements)
        exp1
      end

      _ => begin
        @assign exp = P_Expression.Expression.UNARY(
          P_Operator.Operator.makeUMinus(TYPE_UNKNOWN()),
          exp1,
        )
        printFailedEvalError(getInstanceName(), exp, sourceInfo())
        fail()
      end
    end
  end
  return exp
end

function evalLogicBinaryOp(
  exp1::Expression,
  op::Operator,
  exp2::Expression,
  target::EvalTarget = P_EvalTarget.IGNORE_ERRORS(),
)::Expression
  local exp::Expression

  local e1::Expression
  local max_prop_exp::Expression
  local max_prop_count::Integer

  @assign (max_prop_exp, max_prop_count) =
    P_Expression.Expression.mostPropagatedSubExpBinary(exp1, exp2)
  if max_prop_count >= 0
    @assign exp = P_Expression.Expression.bindingExpMap2(
      P_Expression.Expression.LBINARY(exp1, op, exp2),
      (target) -> evalLogicBinaryExp(target = target),
      max_prop_count,
      max_prop_exp,
    )
  else
    @assign exp = evalLogicBinaryOp_dispatch(exp1, op, exp2, target)
  end
  return exp
end

function evalLogicBinaryExp(binaryExp::Expression, target::EvalTarget)::Expression
  local result::Expression

  local e1::Expression
  local e2::Expression
  local op::Operator

  @match P_Expression.Expression.LBINARY(exp1 = e1, operator = op, exp2 = e2) = binaryExp
  @assign result = evalLogicBinaryOp_dispatch(e1, op, e2, target)
  return result
end

function evalLogicBinaryOp_dispatch(
  exp1::Expression,
  op::Operator,
  exp2::Expression,
  target::EvalTarget,
)::Expression
  local exp::Expression

  @assign exp = begin
    @match op.op begin
      Op.AND => begin
        evalLogicBinaryAnd(evalExp(exp1, target), exp2, target)
      end

      Op.OR => begin
        evalLogicBinaryOr(evalExp(exp1, target), exp2, target)
      end

      _ => begin
        Error.addInternalError(
          getInstanceName() +
          ": unimplemented case for " +
          P_Expression.Expression.toString(P_Expression.Expression.LBINARY(exp1, op, exp2)),
          sourceInfo(),
        )
        fail()
      end
    end
  end
  return exp
end

function evalLogicBinaryAnd(
  exp1::Expression,
  exp2::Expression,
  target::EvalTarget,
)::Expression
  local exp::Expression

  @assign exp = begin
    local expl::List{Expression}
    @matchcontinue exp1 begin
      P_Expression.Expression.BOOLEAN(__) => begin
        if exp1.value
          evalExp_impl(exp2, target)
        else
          exp1
        end
      end

      P_Expression.Expression.ARRAY(__) => begin
        @match P_Expression.Expression.ARRAY(elements = expl) = evalExp_impl(exp2, target)
        @assign expl =
          List(@do_threaded_for evalLogicBinaryAnd(e1, e2, target) (e1, e2) (
            exp1.elements,
            expl,
          ))
        P_Expression.Expression.makeArray(
          Type.setArrayElementType(exp1.ty, TYPE_BOOLEAN()),
          expl,
          literal = true,
        )
      end

      _ => begin
        @assign exp = P_Expression.Expression.LBINARY(
          exp1,
          P_Operator.Operator.makeAnd(TYPE_UNKNOWN()),
          exp2,
        )
        printFailedEvalError(getInstanceName(), exp, sourceInfo())
        fail()
      end
    end
  end
  return exp
end

function evalLogicBinaryOr(
  exp1::Expression,
  exp2::Expression,
  target::EvalTarget,
)::Expression
  local exp::Expression

  @assign exp = begin
    local expl::List{Expression}
    @match exp1 begin
      P_Expression.Expression.BOOLEAN(__) => begin
        if exp1.value
          exp1
        else
          evalExp_impl(exp2, target)
        end
      end

      P_Expression.Expression.ARRAY(__) => begin
        @match P_Expression.Expression.ARRAY(elements = expl) = evalExp_impl(exp2, target)
        @assign expl =
          List(@do_threaded_for evalLogicBinaryOr(e1, e2, target) (e1, e2) (
            exp1.elements,
            expl,
          ))
        P_Expression.Expression.makeArray(
          Type.setArrayElementType(exp1.ty, TYPE_BOOLEAN()),
          expl,
          literal = true,
        )
      end

      _ => begin
        @assign exp = P_Expression.Expression.LBINARY(
          exp1,
          P_Operator.Operator.makeOr(TYPE_UNKNOWN()),
          exp2,
        )
        printFailedEvalError(getInstanceName(), exp, sourceInfo())
        fail()
      end
    end
  end
  return exp
end

function evalLogicUnaryOp(exp1::Expression, op::Operator)::Expression
  local exp::Expression

  @assign exp = begin
    @match op.op begin
      Op.NOT => begin
        P_Expression.Expression.bindingExpMap(exp1, evalLogicUnaryNot)
      end

      _ => begin
        Error.addInternalError(
          getInstanceName() +
          ": unimplemented case for " +
          P_Expression.Expression.toString(P_Expression.Expression.LUNARY(op, exp1)),
          sourceInfo(),
        )
        fail()
      end
    end
  end
  return exp
end

function evalLogicUnaryNot(exp1::Expression)::Expression
  local exp::Expression

  @assign exp = begin
    @match exp1 begin
      P_Expression.Expression.BOOLEAN(__) => begin
        P_Expression.Expression.BOOLEAN(!exp1.value)
      end

      P_Expression.Expression.ARRAY(__) => begin
        P_Expression.Expression.mapArrayElements(exp1, evalLogicUnaryNot)
      end

      _ => begin
        @assign exp = P_Expression.Expression.LUNARY(
          P_Operator.Operator.makeNot(TYPE_UNKNOWN()),
          exp1,
        )
        printFailedEvalError(getInstanceName(), exp, sourceInfo())
        fail()
      end
    end
  end
  return exp
end

function evalRelationOp(exp1::Expression, op::Operator, exp2::Expression)::Expression
  local exp::Expression

  local max_prop_exp::Expression
  local max_prop_count::Integer

  @assign (max_prop_exp, max_prop_count) =
    P_Expression.Expression.mostPropagatedSubExpBinary(exp1, exp2)
  if max_prop_count >= 0
    @assign exp = P_Expression.Expression.bindingExpMap2(
      P_Expression.Expression.RELATION(exp1, op, exp2),
      evalRelationExp,
      max_prop_count,
      max_prop_exp,
    )
  else
    @assign exp = evalRelationOp_dispatch(exp1, op, exp2)
  end
  return exp
end

function evalRelationExp(relationExp::Expression)::Expression
  local result::Expression

  local e1::Expression
  local e2::Expression
  local op::Operator

  @match P_Expression.Expression.RELATION(exp1 = e1, operator = op, exp2 = e2) = relationExp
  @assign result = evalRelationOp_dispatch(e1, op, e2)
  return result
end

function evalRelationOp_dispatch(
  exp1::Expression,
  op::Operator,
  exp2::Expression,
)::Expression
  local exp::Expression

  local res::Bool

  @assign res = begin
    @match op.op begin
      Op.LESS => begin
        evalRelationLess(exp1, exp2)
      end

      Op.LESSEQ => begin
        evalRelationLessEq(exp1, exp2)
      end

      Op.GREATER => begin
        evalRelationGreater(exp1, exp2)
      end

      Op.GREATEREQ => begin
        evalRelationGreaterEq(exp1, exp2)
      end

      Op.EQUAL => begin
        evalRelationEqual(exp1, exp2)
      end

      Op.NEQUAL => begin
        evalRelationNotEqual(exp1, exp2)
      end

      _ => begin
        Error.addInternalError(
          getInstanceName() +
          ": unimplemented case for " +
          P_Expression.Expression.toString(P_Expression.Expression.RELATION(
            exp1,
            op,
            exp2,
          )),
          sourceInfo(),
        )
        fail()
      end
    end
  end
  @assign exp = P_Expression.Expression.BOOLEAN(res)
  return exp
end

function evalRelationLess(exp1::Expression, exp2::Expression)::Bool
  local res::Bool

  @assign res = begin
    @match (exp1, exp2) begin
      (P_Expression.Expression.INTEGER(__), P_Expression.Expression.INTEGER(__)) => begin
        exp1.value < exp2.value
      end

      (P_Expression.Expression.REAL(__), P_Expression.Expression.REAL(__)) => begin
        exp1.value < exp2.value
      end

      (P_Expression.Expression.BOOLEAN(__), P_Expression.Expression.BOOLEAN(__)) => begin
        exp1.value < exp2.value
      end

      (P_Expression.Expression.STRING(__), P_Expression.Expression.STRING(__)) => begin
        stringCompare(exp1.value, exp2.value) < 0
      end

      (
        P_Expression.Expression.ENUM_LITERAL(__),
        P_Expression.Expression.ENUM_LITERAL(__),
      ) => begin
        exp1.index < exp2.index
      end

      _ => begin
        printFailedEvalError(
          getInstanceName(),
          P_Expression.Expression.RELATION(
            exp1,
            P_Operator.Operator.makeLess(TYPE_UNKNOWN()),
            exp2,
          ),
          sourceInfo(),
        )
        fail()
      end
    end
  end
  return res
end

function evalRelationLessEq(exp1::Expression, exp2::Expression)::Bool
  local res::Bool

  @assign res = begin
    @match (exp1, exp2) begin
      (P_Expression.Expression.INTEGER(__), P_Expression.Expression.INTEGER(__)) => begin
        exp1.value <= exp2.value
      end

      (P_Expression.Expression.REAL(__), P_Expression.Expression.REAL(__)) => begin
        exp1.value <= exp2.value
      end

      (P_Expression.Expression.BOOLEAN(__), P_Expression.Expression.BOOLEAN(__)) => begin
        exp1.value <= exp2.value
      end

      (P_Expression.Expression.STRING(__), P_Expression.Expression.STRING(__)) => begin
        stringCompare(exp1.value, exp2.value) <= 0
      end

      (
        P_Expression.Expression.ENUM_LITERAL(__),
        P_Expression.Expression.ENUM_LITERAL(__),
      ) => begin
        exp1.index <= exp2.index
      end

      _ => begin
        printFailedEvalError(
          getInstanceName(),
          P_Expression.Expression.RELATION(
            exp1,
            P_Operator.Operator.makeLessEq(TYPE_UNKNOWN()),
            exp2,
          ),
          sourceInfo(),
        )
        fail()
      end
    end
  end
  return res
end

function evalRelationGreater(exp1::Expression, exp2::Expression)::Bool
  local res::Bool

  @assign res = begin
    @match (exp1, exp2) begin
      (P_Expression.Expression.INTEGER(__), P_Expression.Expression.INTEGER(__)) => begin
        exp1.value > exp2.value
      end

      (P_Expression.Expression.REAL(__), P_Expression.Expression.REAL(__)) => begin
        exp1.value > exp2.value
      end

      (P_Expression.Expression.BOOLEAN(__), P_Expression.Expression.BOOLEAN(__)) => begin
        exp1.value > exp2.value
      end

      (P_Expression.Expression.STRING(__), P_Expression.Expression.STRING(__)) => begin
        stringCompare(exp1.value, exp2.value) > 0
      end

      (
        P_Expression.Expression.ENUM_LITERAL(__),
        P_Expression.Expression.ENUM_LITERAL(__),
      ) => begin
        exp1.index > exp2.index
      end

      _ => begin
        printFailedEvalError(
          getInstanceName(),
          P_Expression.Expression.RELATION(
            exp1,
            P_Operator.Operator.makeGreater(TYPE_UNKNOWN()),
            exp2,
          ),
          sourceInfo(),
        )
        fail()
      end
    end
  end
  return res
end

function evalRelationGreaterEq(exp1::Expression, exp2::Expression)::Bool
  local res::Bool

  @assign res = begin
    @match (exp1, exp2) begin
      (P_Expression.Expression.INTEGER(__), P_Expression.Expression.INTEGER(__)) => begin
        exp1.value >= exp2.value
      end

      (P_Expression.Expression.REAL(__), P_Expression.Expression.REAL(__)) => begin
        exp1.value >= exp2.value
      end

      (P_Expression.Expression.BOOLEAN(__), P_Expression.Expression.BOOLEAN(__)) => begin
        exp1.value >= exp2.value
      end

      (P_Expression.Expression.STRING(__), P_Expression.Expression.STRING(__)) => begin
        stringCompare(exp1.value, exp2.value) >= 0
      end

      (
        P_Expression.Expression.ENUM_LITERAL(__),
        P_Expression.Expression.ENUM_LITERAL(__),
      ) => begin
        exp1.index >= exp2.index
      end

      _ => begin
        printFailedEvalError(
          getInstanceName(),
          P_Expression.Expression.RELATION(
            exp1,
            P_Operator.Operator.makeGreaterEq(TYPE_UNKNOWN()),
            exp2,
          ),
          sourceInfo(),
        )
        fail()
      end
    end
  end
  return res
end

function evalRelationEqual(exp1::Expression, exp2::Expression)::Bool
  local res::Bool

  @assign res = begin
    @match (exp1, exp2) begin
      (P_Expression.Expression.INTEGER(__), P_Expression.Expression.INTEGER(__)) => begin
        exp1.value == exp2.value
      end

      (P_Expression.Expression.REAL(__), P_Expression.Expression.REAL(__)) => begin
        exp1.value == exp2.value
      end

      (P_Expression.Expression.BOOLEAN(__), P_Expression.Expression.BOOLEAN(__)) => begin
        exp1.value == exp2.value
      end

      (P_Expression.Expression.STRING(__), P_Expression.Expression.STRING(__)) => begin
        stringCompare(exp1.value, exp2.value) == 0
      end

      (
        P_Expression.Expression.ENUM_LITERAL(__),
        P_Expression.Expression.ENUM_LITERAL(__),
      ) => begin
        exp1.index == exp2.index
      end

      _ => begin
        printFailedEvalError(
          getInstanceName(),
          P_Expression.Expression.RELATION(
            exp1,
            P_Operator.Operator.makeEqual(TYPE_UNKNOWN()),
            exp2,
          ),
          sourceInfo(),
        )
        fail()
      end
    end
  end
  return res
end

function evalRelationNotEqual(exp1::Expression, exp2::Expression)::Bool
  local res::Bool

  @assign res = begin
    @match (exp1, exp2) begin
      (P_Expression.Expression.INTEGER(__), P_Expression.Expression.INTEGER(__)) => begin
        exp1.value != exp2.value
      end

      (P_Expression.Expression.REAL(__), P_Expression.Expression.REAL(__)) => begin
        exp1.value != exp2.value
      end

      (P_Expression.Expression.BOOLEAN(__), P_Expression.Expression.BOOLEAN(__)) => begin
        exp1.value != exp2.value
      end

      (P_Expression.Expression.STRING(__), P_Expression.Expression.STRING(__)) => begin
        stringCompare(exp1.value, exp2.value) != 0
      end

      (
        P_Expression.Expression.ENUM_LITERAL(__),
        P_Expression.Expression.ENUM_LITERAL(__),
      ) => begin
        exp1.index != exp2.index
      end

      _ => begin
        printFailedEvalError(
          getInstanceName(),
          P_Expression.Expression.RELATION(
            exp1,
            P_Operator.Operator.makeNotEqual(TYPE_UNKNOWN()),
            exp2,
          ),
          sourceInfo(),
        )
        fail()
      end
    end
  end
  return res
end

function evalIfExp(ifExp::Expression, target::EvalTarget)::Expression
  local result::Expression

  local cond::Expression
  local btrue::Expression
  local bfalse::Expression

  @match P_Expression.Expression.IF(
    condition = cond,
    trueBranch = btrue,
    falseBranch = bfalse,
  ) = ifExp
  @assign result = P_Expression.Expression.IF(evalExp_impl(cond, target), btrue, bfalse)
  @assign result =
    P_Expression.Expression.bindingExpMap(result, (target) -> evalIfExp2(target = target))
  return result
end

function evalIfExp2(ifExp::Expression, target::EvalTarget)::Expression
  local result::Expression

  local cond::Expression
  local btrue::Expression
  local bfalse::Expression

  @match P_Expression.Expression.IF(
    condition = cond,
    trueBranch = btrue,
    falseBranch = bfalse,
  ) = ifExp
  @assign result = begin
    @match cond begin
      P_Expression.Expression.BOOLEAN(__) => begin
        evalExp_impl(if cond.value
          btrue
        else
          bfalse
        end, target)
      end

      _ => begin
        Error.addInternalError(
          getInstanceName() +
          ": unimplemented case for " +
          P_Expression.Expression.toString(P_Expression.Expression.IF(cond, btrue, bfalse)),
          sourceInfo(),
        )
        fail()
      end
    end
  end
  return result
end

function evalCast(castExp::Expression, castTy::M_Type)::Expression
  local exp::Expression

  @assign exp = P_Expression.Expression.typeCast(castExp, castTy)
  #=  Expression.typeCast will just create a CAST if it can't typecast
  =#
  #=  the expression, so make sure we actually got something else back.
  =#
  @assign () = begin
    @match exp begin
      P_Expression.Expression.CAST(__) => begin
        @assign exp = P_Expression.Expression.CAST(castTy, castExp)
        printFailedEvalError(getInstanceName(), exp, sourceInfo())
        fail()
      end

      _ => begin
        ()
      end
    end
  end
  return exp
end

function evalCall(call::Call, target::EvalTarget)::Expression
  local exp::Expression

  local c::Call = call

  @assign exp = begin
    local args::List{Expression}
    @match c begin
      P_Call.TYPED_CALL(__) => begin
        @assign c.arguments = List(evalExp_impl(arg, target) for arg in c.arguments)
        if P_Function.isBuiltin(c.fn)
          P_Expression.Expression.bindingExpMap(
            P_Expression.Expression.CALL(c),
            (target) -> evalBuiltinCallExp(target = target),
          )
        else
          P_Expression.Expression.bindingExpMap(
            P_Expression.Expression.CALL(c),
            evalNormalCallExp,
          )
        end
      end

      P_Call.TYPED_ARRAY_CONSTRUCTOR(__) => begin
        evalArrayConstructor(c.exp, c.iters)
      end

      P_Call.TYPED_REDUCTION(__) => begin
        evalReduction(c.fn, c.exp, c.iters)
      end

      _ => begin
        Error.addInternalError(getInstanceName() + " got untyped call", sourceInfo())
        fail()
      end
    end
  end
  return exp
end

function evalBuiltinCallExp(callExp::Expression, target::EvalTarget)::Expression
  local result::Expression

  local fn::M_Function
  local args::List{Expression}

  @match P_Expression.Expression.CALL(call = P_Call.TYPED_CALL(fn = fn, arguments = args)) =
    callExp
  @assign result = evalBuiltinCall(fn, args, target)
  return result
end

function evalBuiltinCall(
  fn::M_Function,
  args::List{<:Expression},
  target::EvalTarget,
)::Expression
  local result::Expression

  local fn_path::Absyn.Path = P_Function.nameConsiderBuiltin(fn)

  @assign result = begin
    @match AbsynUtil.pathFirstIdent(fn_path) begin
      "abs" => begin
        evalBuiltinAbs(listHead(args))
      end

      "acos" => begin
        evalBuiltinAcos(listHead(args), target)
      end

      "array" => begin
        evalBuiltinArray(args)
      end

      "asin" => begin
        evalBuiltinAsin(listHead(args), target)
      end

      "atan2" => begin
        evalBuiltinAtan2(args)
      end

      "atan" => begin
        evalBuiltinAtan(listHead(args))
      end

      "cat" => begin
        evalBuiltinCat(listHead(args), listRest(args), target)
      end

      "ceil" => begin
        evalBuiltinCeil(listHead(args))
      end

      "cosh" => begin
        evalBuiltinCosh(listHead(args))
      end

      "cos" => begin
        evalBuiltinCos(listHead(args))
      end

      "der" => begin
        evalBuiltinDer(listHead(args))
      end

      "diagonal" => begin
        evalBuiltinDiagonal(P_Expression.Expression.unbox(listHead(args)))
      end

      "div" => begin
        evalBuiltinDiv(args, target)
      end

      "exp" => begin
        evalBuiltinExp(listHead(args))
      end

      "fill" => begin
        evalBuiltinFill(args)
      end

      "floor" => begin
        evalBuiltinFloor(listHead(args))
      end

      "identity" => begin
        evalBuiltinIdentity(listHead(args))
      end

      "integer" => begin
        evalBuiltinInteger(listHead(args))
      end

      "Integer" => begin
        evalBuiltinIntegerEnum(listHead(args))
      end

      "log10" => begin
        evalBuiltinLog10(listHead(args), target)
      end

      "log" => begin
        evalBuiltinLog(listHead(args), target)
      end

      "matrix" => begin
        evalBuiltinMatrix(listHead(args))
      end

      "max" => begin
        evalBuiltinMax(args, fn)
      end

      "min" => begin
        evalBuiltinMin(args, fn)
      end

      "mod" => begin
        evalBuiltinMod(args, target)
      end

      "noEvent" => begin
        listHead(args)
      end

      "ones" => begin
        evalBuiltinOnes(args)
      end

      "product" => begin
        evalBuiltinProduct(listHead(args))
      end

      "promote" => begin
        evalBuiltinPromote(listGet(args, 1), listGet(args, 2))
      end

      "rem" => begin
        evalBuiltinRem(args, target)
      end

      "scalar" => begin
        evalBuiltinScalar(args)
      end

      "sign" => begin
        evalBuiltinSign(listHead(args))
      end

      "sinh" => begin
        evalBuiltinSinh(listHead(args))
      end

      "sin" => begin
        evalBuiltinSin(listHead(args))
      end

      "skew" => begin
        evalBuiltinSkew(listHead(args))
      end

      "smooth" => begin
        listGet(args, 2)
      end

      "sqrt" => begin
        evalBuiltinSqrt(listHead(args))
      end

      "String" => begin
        evalBuiltinString(args)
      end

      "sum" => begin
        evalBuiltinSum(listHead(args))
      end

      "symmetric" => begin
        evalBuiltinSymmetric(listHead(args))
      end

      "tanh" => begin
        evalBuiltinTanh(listHead(args))
      end

      "tan" => begin
        evalBuiltinTan(listHead(args))
      end

      "transpose" => begin
        evalBuiltinTranspose(listHead(args))
      end

      "vector" => begin
        evalBuiltinVector(listHead(args))
      end

      "zeros" => begin
        evalBuiltinZeros(args)
      end

      "OpenModelica_uriToFilename" => begin
        evalUriToFilename(fn, args, target)
      end

      "intBitAnd" => begin
        evalIntBitAnd(args)
      end

      "intBitOr" => begin
        evalIntBitOr(args)
      end

      "intBitXor" => begin
        evalIntBitXor(args)
      end

      "intBitLShift" => begin
        evalIntBitLShift(args)
      end

      "intBitRShift" => begin
        evalIntBitRShift(args)
      end

      "inferredClock" => begin
        evalInferredClock(args)
      end

      "rationalClock" => begin
        evalRationalClock(args)
      end

      "realClock" => begin
        evalRealClock(args)
      end

      "booleanClock" => begin
        evalBooleanClock(args)
      end

      "solverClock" => begin
        evalSolverClock(args)
      end

      "DynamicSelect" => begin
        evalBuiltinDynamicSelect(fn, args, target)
      end

      _ => begin
        #=  TODO: Fix typing of diagonal so the argument isn't boxed.
        =#
        #=  No events during ceval, just return the argument.
        =#
        Error.addInternalError(
          getInstanceName() + ": unimplemented case for " + AbsynUtil.pathString(fn_path),
          sourceInfo(),
        )
        fail()
      end
    end
  end
  return result
end

function evalNormalCallExp(callExp::Expression)::Expression
  local result::Expression

  local fn::M_Function
  local args::List{Expression}

  @match P_Expression.Expression.CALL(call = P_Call.TYPED_CALL(fn = fn, arguments = args)) =
    callExp
  @assign result = evalNormalCall(fn, args)
  return result
end

function evalNormalCall(fn::M_Function, args::List{<:Expression})::Expression
  local result::Expression = EvalFunction.evaluate(fn, args)
  return result
end

function evalBuiltinAbs(arg::Expression)::Expression
  local result::Expression

  @assign result = begin
    @match arg begin
      P_Expression.Expression.INTEGER(__) => begin
        P_Expression.Expression.INTEGER(abs(arg.value))
      end

      P_Expression.Expression.REAL(__) => begin
        P_Expression.Expression.REAL(abs(arg.value))
      end

      _ => begin
        printWrongArgsError(getInstanceName(), list(arg), sourceInfo())
        fail()
      end
    end
  end
  return result
end

function evalBuiltinAcos(arg::Expression, target::EvalTarget)::Expression
  local result::Expression

  local x::AbstractFloat

  @assign result = begin
    @match arg begin
      P_Expression.Expression.REAL(value = x) => begin
        if x < (-1.0) || x > 1.0
          if P_EvalTarget.hasInfo(target)
            Error.addSourceMessage(
              Error.ARGUMENT_OUT_OF_RANGE,
              list(String(x), "acos", "-1 <= x <= 1"),
              P_EvalTarget.getInfo(target),
            )
          end
          fail()
        end
        P_Expression.Expression.REAL(acos(x))
      end

      _ => begin
        printWrongArgsError(getInstanceName(), list(arg), sourceInfo())
        fail()
      end
    end
  end
  return result
end

function evalBuiltinArray(args::List{<:Expression})::Expression
  local result::Expression

  local ty::M_Type

  @assign ty = P_Expression.Expression.typeOf(listHead(args))
  @assign ty = Type.liftArrayLeft(ty, P_Dimension.Dimension.fromInteger(listLength(args)))
  @assign result = P_Expression.Expression.makeArray(ty, args, literal = true)
  return result
end

function evalBuiltinAsin(arg::Expression, target::EvalTarget)::Expression
  local result::Expression

  local x::AbstractFloat

  @assign result = begin
    @match arg begin
      P_Expression.Expression.REAL(value = x) => begin
        if x < (-1.0) || x > 1.0
          if P_EvalTarget.hasInfo(target)
            Error.addSourceMessage(
              Error.ARGUMENT_OUT_OF_RANGE,
              list(String(x), "asin", "-1 <= x <= 1"),
              P_EvalTarget.getInfo(target),
            )
          end
          fail()
        end
        P_Expression.Expression.REAL(asin(x))
      end

      _ => begin
        printWrongArgsError(getInstanceName(), list(arg), sourceInfo())
        fail()
      end
    end
  end
  return result
end

function evalBuiltinAtan2(args::List{<:Expression})::Expression
  local result::Expression

  local y::AbstractFloat
  local x::AbstractFloat

  @assign result = begin
    @match args begin
      P_Expression.Expression.REAL(value = y) <|
      P_Expression.Expression.REAL(value = x) <| nil() => begin
        P_Expression.Expression.REAL(atan2(y, x))
      end

      _ => begin
        printWrongArgsError(getInstanceName(), args, sourceInfo())
        fail()
      end
    end
  end
  return result
end

function evalBuiltinAtan(arg::Expression)::Expression
  local result::Expression

  @assign result = begin
    @match arg begin
      P_Expression.Expression.REAL(__) => begin
        P_Expression.Expression.REAL(atan(arg.value))
      end

      _ => begin
        printWrongArgsError(getInstanceName(), list(arg), sourceInfo())
        fail()
      end
    end
  end
  return result
end

function evalBuiltinCat(
  argN::Expression,
  args::List{<:Expression},
  target::EvalTarget,
)::Expression
  local result::Expression

  local n::Integer
  local nd::Integer
  local sz::Integer
  local ty::M_Type
  local es::List{Expression}
  local dims::List{Integer}

  @match P_Expression.Expression.INTEGER(n) = argN
  @assign ty = P_Expression.Expression.typeOf(listHead(args))
  @assign nd = Type.dimensionCount(ty)
  if n > nd || n < 1
    if P_EvalTarget.hasInfo(target)
      Error.addSourceMessage(
        Error.ARGUMENT_OUT_OF_RANGE,
        list(String(n), "cat", "1 <= x <= " + String(nd)),
        P_EvalTarget.getInfo(target),
      )
    end
    fail()
  end
  @assign es = List(e for e in args if !P_Expression.Expression.isEmptyArray(e))
  @assign sz = listLength(es)
  if sz == 0
    @assign result = listHead(args)
  elseif sz == 1
    @assign result = listHead(es)
  else
    @assign (es, dims) = ExpressionSimplify.evalCat(
      n,
      es,
      getArrayContents = P_Expression.Expression.arrayElements,
      toString = P_Expression.Expression.toString,
    )
    @assign result = P_Expression.Expression.arrayFromList(
      es,
      P_Expression.Expression.typeOf(listHead(es)),
      List(P_Dimension.Dimension.fromInteger(d) for d in dims),
    )
  end
  return result
end

function evalBuiltinCeil(arg::Expression)::Expression
  local result::Expression

  @assign result = begin
    @match arg begin
      P_Expression.Expression.REAL(__) => begin
        P_Expression.Expression.REAL(ceil(arg.value))
      end

      _ => begin
        printWrongArgsError(getInstanceName(), list(arg), sourceInfo())
        fail()
      end
    end
  end
  return result
end

function evalBuiltinCosh(arg::Expression)::Expression
  local result::Expression

  @assign result = begin
    @match arg begin
      P_Expression.Expression.REAL(__) => begin
        P_Expression.Expression.REAL(cosh(arg.value))
      end

      _ => begin
        printWrongArgsError(getInstanceName(), list(arg), sourceInfo())
        fail()
      end
    end
  end
  return result
end

function evalBuiltinCos(arg::Expression)::Expression
  local result::Expression

  @assign result = begin
    @match arg begin
      P_Expression.Expression.REAL(__) => begin
        P_Expression.Expression.REAL(cos(arg.value))
      end

      _ => begin
        printWrongArgsError(getInstanceName(), list(arg), sourceInfo())
        fail()
      end
    end
  end
  return result
end

function evalBuiltinDer(arg::Expression)::Expression
  local result::Expression

  @assign result = P_Expression.Expression.fillType(
    P_Expression.Expression.typeOf(arg),
    P_Expression.Expression.REAL(0.0),
  )
  return result
end

function evalBuiltinDiagonal(arg::Expression)::Expression
  local result::Expression

  local elem_ty::M_Type
  local row_ty::M_Type
  local zero::Expression
  local elems::List{Expression}
  local row::List{Expression}
  local rows::List{Expression} = nil
  local n::Integer
  local i::Integer = 1
  local e_lit::Bool
  local arg_lit::Bool = true

  @assign result = begin
    @match arg begin
      P_Expression.Expression.ARRAY(elements = nil()) => begin
        arg
      end

      P_Expression.Expression.ARRAY(elements = elems) => begin
        @assign n = listLength(elems)
        @assign elem_ty = P_Expression.Expression.typeOf(listHead(elems))
        @assign row_ty = Type.liftArrayLeft(elem_ty, P_Dimension.Dimension.fromInteger(n))
        @assign zero = P_Expression.Expression.makeZero(elem_ty)
        for e in listReverse(elems)
          @assign row = nil
          for j = 2:i
            @assign row = _cons(zero, row)
          end
          @assign row = _cons(e, row)
          @assign e_lit = P_Expression.Expression.isLiteral(e)
          @assign arg_lit = arg_lit && e_lit
          for j = i:(n - 1)
            @assign row = _cons(zero, row)
          end
          @assign i = i + 1
          @assign rows =
            _cons(P_Expression.Expression.makeArray(row_ty, row, e_lit), rows)
        end
        P_Expression.Expression.makeArray(
          Type.liftArrayLeft(row_ty, P_Dimension.Dimension.fromInteger(n)),
          rows,
          arg_lit,
        )
      end

      _ => begin
        printWrongArgsError(getInstanceName(), list(arg), sourceInfo())
        fail()
      end
    end
  end
  return result
end

function evalBuiltinDiv(args::List{<:Expression}, target::EvalTarget)::Expression
  local result::Expression

  local rx::AbstractFloat
  local ry::AbstractFloat
  local ix::Integer
  local iy::Integer

  @assign result = begin
    @match args begin
      P_Expression.Expression.INTEGER(ix) <| P_Expression.Expression.INTEGER(iy) <| nil() => begin
        if iy == 0
          if P_EvalTarget.hasInfo(target)
            Error.addSourceMessage(
              Error.DIVISION_BY_ZERO,
              list(String(ix), String(iy)),
              P_EvalTarget.getInfo(target),
            )
          end
          fail()
        end
        P_Expression.Expression.INTEGER(intDiv(ix, iy))
      end

      P_Expression.Expression.REAL(rx) <| P_Expression.Expression.REAL(ry) <| nil() =>
        begin
          if ry == 0.0
            if P_EvalTarget.hasInfo(target)
              Error.addSourceMessage(
                Error.DIVISION_BY_ZERO,
                list(String(rx), String(ry)),
                P_EvalTarget.getInfo(target),
              )
            end
            fail()
          end
          @assign rx = rx / ry
          P_Expression.Expression.REAL(if rx < 0.0
            ceil(rx)
          else
            floor(rx)
          end)
        end

      _ => begin
        printWrongArgsError(getInstanceName(), args, sourceInfo())
        fail()
      end
    end
  end
  return result
end

function evalBuiltinExp(arg::Expression)::Expression
  local result::Expression

  @assign result = begin
    @match arg begin
      P_Expression.Expression.REAL(__) => begin
        P_Expression.Expression.REAL(exp(arg.value))
      end

      _ => begin
        printWrongArgsError(getInstanceName(), list(arg), sourceInfo())
        fail()
      end
    end
  end
  return result
end

function evalBuiltinFill(args::List{<:Expression})::Expression
  local result::Expression

  @assign result = evalBuiltinFill2(listHead(args), listRest(args))
  return result
end

function evalBuiltinFill2(fillValue::Expression, dims::List{<:Expression})::Expression
  local result::Expression = fillValue

  local dim_size::Integer
  local arr::List{Expression}
  local arr_ty::M_Type = P_Expression.Expression.typeOf(result)

  for d in listReverse(dims)
    @assign () = begin
      @match d begin
        P_Expression.Expression.INTEGER(value = dim_size) => begin
          ()
        end

        _ => begin
          printWrongArgsError(getInstanceName(), list(d), sourceInfo())
          fail()
        end
      end
    end
    @assign arr = List(result for e = 1:dim_size)
    @assign arr_ty = Type.liftArrayLeft(arr_ty, P_Dimension.Dimension.fromInteger(dim_size))
    @assign result = P_Expression.Expression.makeArray(
      arr_ty,
      arr,
      P_Expression.Expression.isLiteral(fillValue),
    )
  end
  return result
end

function evalBuiltinFloor(arg::Expression)::Expression
  local result::Expression

  @assign result = begin
    @match arg begin
      P_Expression.Expression.REAL(__) => begin
        P_Expression.Expression.REAL(floor(arg.value))
      end

      _ => begin
        printWrongArgsError(getInstanceName(), list(arg), sourceInfo())
        fail()
      end
    end
  end
  return result
end

function evalBuiltinIdentity(arg::Expression)::Expression
  local result::Expression

  @assign result = begin
    @match arg begin
      P_Expression.Expression.INTEGER(__) => begin
        P_Expression.Expression.makeIdentityMatrix(arg.value, TYPE_INTEGER())
      end

      _ => begin
        printWrongArgsError(getInstanceName(), list(arg), sourceInfo())
        fail()
      end
    end
  end
  return result
end

function evalBuiltinInteger(arg::Expression)::Expression
  local result::Expression

  @assign result = begin
    @match arg begin
      P_Expression.Expression.INTEGER(__) => begin
        arg
      end

      P_Expression.Expression.REAL(__) => begin
        P_Expression.Expression.INTEGER(realInt(arg.value))
      end

      _ => begin
        printWrongArgsError(getInstanceName(), list(arg), sourceInfo())
        fail()
      end
    end
  end
  return result
end

function evalBuiltinIntegerEnum(arg::Expression)::Expression
  local result::Expression

  @assign result = begin
    @match arg begin
      P_Expression.Expression.ENUM_LITERAL(__) => begin
        P_Expression.Expression.INTEGER(arg.index)
      end

      _ => begin
        printWrongArgsError(getInstanceName(), list(arg), sourceInfo())
        fail()
      end
    end
  end
  return result
end

function evalBuiltinLog10(arg::Expression, target::EvalTarget)::Expression
  local result::Expression

  local x::AbstractFloat

  @assign result = begin
    @match arg begin
      P_Expression.Expression.REAL(value = x) => begin
        if x <= 0.0
          if P_EvalTarget.hasInfo(target)
            Error.addSourceMessage(
              Error.ARGUMENT_OUT_OF_RANGE,
              list(String(x), "log10", "x > 0"),
              P_EvalTarget.getInfo(target),
            )
          end
          fail()
        end
        P_Expression.Expression.REAL(log10(x))
      end

      _ => begin
        printWrongArgsError(getInstanceName(), list(arg), sourceInfo())
        fail()
      end
    end
  end
  return result
end

function evalBuiltinLog(arg::Expression, target::EvalTarget)::Expression
  local result::Expression

  local x::AbstractFloat

  @assign result = begin
    @match arg begin
      P_Expression.Expression.REAL(value = x) => begin
        if x <= 0.0
          if P_EvalTarget.hasInfo(target)
            Error.addSourceMessage(
              Error.ARGUMENT_OUT_OF_RANGE,
              list(String(x), "log", "x > 0"),
              P_EvalTarget.getInfo(target),
            )
          end
          fail()
        end
        P_Expression.Expression.REAL(log(x))
      end

      _ => begin
        printWrongArgsError(getInstanceName(), list(arg), sourceInfo())
        fail()
      end
    end
  end
  return result
end

function evalBuiltinMatrix(arg::Expression)::Expression
  local result::Expression

  @assign result = begin
    local dim_count::Integer
    local expl::List{Expression}
    local dim1::Dimension
    local dim2::Dimension
    local ty::M_Type
    @match arg begin
      P_Expression.Expression.ARRAY(ty = ty) => begin
        @assign dim_count = Type.dimensionCount(ty)
        if dim_count < 2
          @assign result = P_Expression.Expression.promote(arg, ty, 2)
        elseif dim_count == 2
          @assign result = arg
        else
          @match _cons(dim1, _cons(dim2, _)) = Type.arrayDims(ty)
          @assign ty = Type.liftArrayLeft(Type.arrayElementType(ty), dim2)
          @assign expl = List(evalBuiltinMatrix2(e, ty) for e in arg.elements)
          @assign ty = Type.liftArrayLeft(ty, dim1)
          @assign result = P_Expression.Expression.makeArray(ty, expl)
        end
        result
      end

      _ => begin
        @assign ty = P_Expression.Expression.typeOf(arg)
        if Type.isScalar(ty)
          @assign result = P_Expression.Expression.promote(arg, ty, 2)
        else
          printWrongArgsError(getInstanceName(), list(arg), sourceInfo())
          fail()
        end
        result
      end
    end
  end
  return result
end

function evalBuiltinMatrix2(arg::Expression, ty::M_Type)::Expression
  local result::Expression

  @assign result = begin
    @match arg begin
      P_Expression.Expression.ARRAY(__) => begin
        P_Expression.Expression.makeArray(
          ty,
          List(P_Expression.Expression.toScalar(e) for e in arg.elements),
          arg.literal,
        )
      end

      _ => begin
        printWrongArgsError(getInstanceName(), list(arg), sourceInfo())
        fail()
      end
    end
  end
  return result
end

function evalBuiltinMax(args::List{<:Expression}, fn::M_Function)::Expression
  local result::Expression

  local e1::Expression
  local e2::Expression
  local expl::List{Expression}
  local ty::M_Type

  @assign result = begin
    @match args begin
      e1 <| e2 <| nil() => begin
        evalBuiltinMax2(e1, e2)
      end

      e1 && P_Expression.Expression.ARRAY(ty = ty) <| nil() => begin
        @assign result = P_Expression.Expression.fold(
          e1,
          evalBuiltinMax2,
          P_Expression.Expression.EMPTY(ty),
        )
        if P_Expression.Expression.isEmpty(result)
          @assign result = P_Expression.Expression.CALL(P_Call.makeTypedCall(
            fn,
            list(P_Expression.Expression.makeEmptyArray(ty)),
            Variability.CONSTANT,
            Type.arrayElementType(ty),
          ))
        end
        result
      end

      _ => begin
        printWrongArgsError(getInstanceName(), args, sourceInfo())
        fail()
      end
    end
  end
  return result
end

function evalBuiltinMax2(exp1::Expression, exp2::Expression)::Expression
  local result::Expression

  @assign result = begin
    @match (exp1, exp2) begin
      (P_Expression.Expression.INTEGER(__), P_Expression.Expression.INTEGER(__)) => begin
        if exp1.value < exp2.value
          exp2
        else
          exp1
        end
      end

      (P_Expression.Expression.REAL(__), P_Expression.Expression.REAL(__)) => begin
        if exp1.value < exp2.value
          exp2
        else
          exp1
        end
      end

      (P_Expression.Expression.BOOLEAN(__), P_Expression.Expression.BOOLEAN(__)) => begin
        if exp1.value < exp2.value
          exp2
        else
          exp1
        end
      end

      (
        P_Expression.Expression.ENUM_LITERAL(__),
        P_Expression.Expression.ENUM_LITERAL(__),
      ) => begin
        if exp1.index < exp2.index
          exp2
        else
          exp1
        end
      end

      (P_Expression.Expression.ARRAY(__), _) => begin
        exp2
      end

      (_, P_Expression.Expression.EMPTY(__)) => begin
        exp1
      end

      _ => begin
        printWrongArgsError(getInstanceName(), list(exp1, exp2), sourceInfo())
        fail()
      end
    end
  end
  return result
end

function evalBuiltinMin(args::List{<:Expression}, fn::M_Function)::Expression
  local result::Expression

  local e1::Expression
  local e2::Expression
  local expl::List{Expression}
  local ty::M_Type

  @assign result = begin
    @match args begin
      e1 <| e2 <| nil() => begin
        evalBuiltinMin2(e1, e2)
      end

      e1 && P_Expression.Expression.ARRAY(ty = ty) <| nil() => begin
        @assign result = P_Expression.Expression.fold(
          e1,
          evalBuiltinMin2,
          P_Expression.Expression.EMPTY(ty),
        )
        if P_Expression.Expression.isEmpty(result)
          @assign result = P_Expression.Expression.CALL(P_Call.makeTypedCall(
            fn,
            list(P_Expression.Expression.makeEmptyArray(ty)),
            Variability.CONSTANT,
            Type.arrayElementType(ty),
          ))
        end
        result
      end

      _ => begin
        printWrongArgsError(getInstanceName(), args, sourceInfo())
        fail()
      end
    end
  end
  return result
end

function evalBuiltinMin2(exp1::Expression, exp2::Expression)::Expression
  local result::Expression

  @assign result = begin
    @match (exp1, exp2) begin
      (P_Expression.Expression.INTEGER(__), P_Expression.Expression.INTEGER(__)) => begin
        if exp1.value > exp2.value
          exp2
        else
          exp1
        end
      end

      (P_Expression.Expression.REAL(__), P_Expression.Expression.REAL(__)) => begin
        if exp1.value > exp2.value
          exp2
        else
          exp1
        end
      end

      (P_Expression.Expression.BOOLEAN(__), P_Expression.Expression.BOOLEAN(__)) => begin
        if exp1.value > exp2.value
          exp2
        else
          exp1
        end
      end

      (
        P_Expression.Expression.ENUM_LITERAL(__),
        P_Expression.Expression.ENUM_LITERAL(__),
      ) => begin
        if exp1.index > exp2.index
          exp2
        else
          exp1
        end
      end

      (P_Expression.Expression.ARRAY(__), _) => begin
        exp2
      end

      (_, P_Expression.Expression.EMPTY(__)) => begin
        exp1
      end

      _ => begin
        printWrongArgsError(getInstanceName(), list(exp1, exp2), sourceInfo())
        fail()
      end
    end
  end
  return result
end

function evalBuiltinMod(args::List{<:Expression}, target::EvalTarget)::Expression
  local result::Expression

  local x::Expression
  local y::Expression

  @match list(x, y) = args
  @assign result = begin
    @match (x, y) begin
      (P_Expression.Expression.INTEGER(__), P_Expression.Expression.INTEGER(__)) => begin
        if y.value == 0
          if P_EvalTarget.hasInfo(target)
            Error.addSourceMessage(
              Error.MODULO_BY_ZERO,
              list(String(x.value), String(y.value)),
              P_EvalTarget.getInfo(target),
            )
          end
          fail()
        end
        P_Expression.Expression.INTEGER(mod(x.value, y.value))
      end

      (P_Expression.Expression.REAL(__), P_Expression.Expression.REAL(__)) => begin
        if y.value == 0.0
          if P_EvalTarget.hasInfo(target)
            Error.addSourceMessage(
              Error.MODULO_BY_ZERO,
              list(String(x.value), String(y.value)),
              P_EvalTarget.getInfo(target),
            )
          end
          fail()
        end
        P_Expression.Expression.REAL(mod(x.value, y.value))
      end

      _ => begin
        printWrongArgsError(getInstanceName(), args, sourceInfo())
        fail()
      end
    end
  end
  return result
end

function evalBuiltinOnes(args::List{<:Expression})::Expression
  local result::Expression

  @assign result = evalBuiltinFill2(P_Expression.Expression.INTEGER(1), args)
  return result
end

function evalBuiltinProduct(arg::Expression)::Expression
  local result::Expression

  @assign result = begin
    @match arg begin
      P_Expression.Expression.ARRAY(__) => begin
        begin
          @match Type.arrayElementType(P_Expression.Expression.typeOf(arg)) begin
            TYPE_INTEGER(__) => begin
              P_Expression.Expression.INTEGER(P_Expression.Expression.fold(
                arg,
                evalBuiltinProductInt,
                1,
              ))
            end

            TYPE_REAL(__) => begin
              P_Expression.Expression.REAL(P_Expression.Expression.fold(
                arg,
                evalBuiltinProductReal,
                1.0,
              ))
            end

            _ => begin
              printWrongArgsError(getInstanceName(), list(arg), sourceInfo())
              fail()
            end
          end
        end
      end

      _ => begin
        printWrongArgsError(getInstanceName(), list(arg), sourceInfo())
        fail()
      end
    end
  end
  return result
end

function evalBuiltinProductInt(exp::Expression, result::Integer)::Integer

  @assign result = begin
    @match exp begin
      P_Expression.Expression.INTEGER(__) => begin
        result * exp.value
      end

      P_Expression.Expression.ARRAY(__) => begin
        result
      end

      _ => begin
        fail()
      end
    end
  end
  return result
end

function evalBuiltinProductReal(exp::Expression, result::AbstractFloat)::AbstractFloat

  @assign result = begin
    @match exp begin
      P_Expression.Expression.REAL(__) => begin
        result * exp.value
      end

      P_Expression.Expression.ARRAY(__) => begin
        result
      end

      _ => begin
        fail()
      end
    end
  end
  return result
end

function evalBuiltinPromote(arg::Expression, argN::Expression)::Expression
  local result::Expression

  local n::Integer

  if P_Expression.Expression.isInteger(argN)
    @match P_Expression.Expression.INTEGER(n) = argN
    @assign result =
      P_Expression.Expression.promote(arg, P_Expression.Expression.typeOf(arg), n)
  else
    printWrongArgsError(getInstanceName(), list(arg, argN), sourceInfo())
    fail()
  end
  return result
end

function evalBuiltinRem(args::List{<:Expression}, target::EvalTarget)::Expression
  local result::Expression

  local x::Expression
  local y::Expression

  @match list(x, y) = args
  @assign result = begin
    @match (x, y) begin
      (P_Expression.Expression.INTEGER(__), P_Expression.Expression.INTEGER(__)) => begin
        if y.value == 0
          if P_EvalTarget.hasInfo(target)
            Error.addSourceMessage(
              Error.REM_ARG_ZERO,
              list(String(x.value), String(y.value)),
              P_EvalTarget.getInfo(target),
            )
          end
          fail()
        end
        P_Expression.Expression.INTEGER(x.value - div(x.value, y.value) * y.value)
      end

      (P_Expression.Expression.REAL(__), P_Expression.Expression.REAL(__)) => begin
        if y.value == 0.0
          if P_EvalTarget.hasInfo(target)
            Error.addSourceMessage(
              Error.REM_ARG_ZERO,
              list(String(x.value), String(y.value)),
              P_EvalTarget.getInfo(target),
            )
          end
          fail()
        end
        P_Expression.Expression.REAL(x.value - div(x.value, y.value) * y.value)
      end

      _ => begin
        printWrongArgsError(getInstanceName(), args, sourceInfo())
        fail()
      end
    end
  end
  return result
end

function evalBuiltinScalar(args::List{<:Expression})::Expression
  local result::Expression

  local exp::Expression = listHead(args)

  @assign result = begin
    @match exp begin
      P_Expression.Expression.ARRAY(__) => begin
        evalBuiltinScalar(exp.elements)
      end

      _ => begin
        exp
      end
    end
  end
  return result
end

function evalBuiltinSign(arg::Expression)::Expression
  local result::Expression

  @assign result = begin
    @match arg begin
      P_Expression.Expression.REAL(__) => begin
        P_Expression.Expression.INTEGER(if arg.value > 0
          1
        else
          if arg.value < 0
            -1
          else
            0
          end
        end)
      end

      P_Expression.Expression.INTEGER(__) => begin
        P_Expression.Expression.INTEGER(if arg.value > 0
          1
        else
          if arg.value < 0
            -1
          else
            0
          end
        end)
      end

      _ => begin
        printWrongArgsError(getInstanceName(), list(arg), sourceInfo())
        fail()
      end
    end
  end
  return result
end

function evalBuiltinSinh(arg::Expression)::Expression
  local result::Expression

  @assign result = begin
    @match arg begin
      P_Expression.Expression.REAL(__) => begin
        P_Expression.Expression.REAL(sinh(arg.value))
      end

      _ => begin
        printWrongArgsError(getInstanceName(), list(arg), sourceInfo())
        fail()
      end
    end
  end
  return result
end

function evalBuiltinSin(arg::Expression)::Expression
  local result::Expression

  @assign result = begin
    @match arg begin
      P_Expression.Expression.REAL(__) => begin
        P_Expression.Expression.REAL(sin(arg.value))
      end

      _ => begin
        printWrongArgsError(getInstanceName(), list(arg), sourceInfo())
        fail()
      end
    end
  end
  return result
end

function evalBuiltinSkew(arg::Expression)::Expression
  local result::Expression

  local x1::Expression
  local x2::Expression
  local x3::Expression
  local y1::Expression
  local y2::Expression
  local y3::Expression
  local ty::M_Type
  local zero::Expression
  local literal::Bool

  @assign result = begin
    @match arg begin
      P_Expression.Expression.ARRAY(
        ty = ty,
        elements = x1 <| x2 <| x3 <| nil(),
        literal = literal,
      ) => begin
        @assign zero = P_Expression.Expression.makeZero(Type.arrayElementType(ty))
        @assign y1 = P_Expression.Expression.makeArray(
          ty,
          list(zero, P_Expression.Expression.negate(x3), x2),
          literal,
        )
        @assign y2 = P_Expression.Expression.makeArray(
          ty,
          list(x3, zero, P_Expression.Expression.negate(x1)),
          literal,
        )
        @assign y3 = P_Expression.Expression.makeArray(
          ty,
          list(P_Expression.Expression.negate(x2), x1, zero),
          literal,
        )
        @assign ty = Type.liftArrayLeft(ty, P_Dimension.Dimension.fromInteger(3))
        P_Expression.Expression.makeArray(ty, list(y1, y2, y3), literal)
      end

      _ => begin
        printWrongArgsError(getInstanceName(), list(arg), sourceInfo())
        fail()
      end
    end
  end
  return result
end

function evalBuiltinSqrt(arg::Expression)::Expression
  local result::Expression

  @assign result = begin
    @match arg begin
      P_Expression.Expression.REAL(__) => begin
        P_Expression.Expression.REAL(sqrt(arg.value))
      end

      _ => begin
        printWrongArgsError(getInstanceName(), list(arg), sourceInfo())
        fail()
      end
    end
  end
  return result
end

function evalBuiltinString(args::List{<:Expression})::Expression
  local result::Expression

  @assign result = begin
    local arg::Expression
    local min_len::Integer
    local str_len::Integer
    local significant_digits::Integer
    local idx::Integer
    local c::Integer
    local left_justified::Bool
    local str::String
    local format::String
    local r::AbstractFloat
    @match args begin
      arg <|
      P_Expression.Expression.INTEGER(min_len) <|
      P_Expression.Expression.BOOLEAN(left_justified) <| nil() => begin
        @assign str = begin
          @match arg begin
            P_Expression.Expression.INTEGER(__) => begin
              intString(arg.value)
            end

            P_Expression.Expression.BOOLEAN(__) => begin
              boolString(arg.value)
            end

            P_Expression.Expression.ENUM_LITERAL(__) => begin
              arg.name
            end

            _ => begin
              printWrongArgsError(getInstanceName(), args, sourceInfo())
              fail()
            end
          end
        end
        @assign str_len = stringLength(str)
        if str_len < min_len
          if left_justified
            @assign str = str + stringAppendList(ListUtil.fill(" ", min_len - str_len))
          else
            @assign str = stringAppendList(ListUtil.fill(" ", min_len - str_len)) + str
          end
        end
        P_Expression.Expression.STRING(str)
      end

      P_Expression.Expression.REAL(r) <|
      P_Expression.Expression.INTEGER(significant_digits) <|
      P_Expression.Expression.INTEGER(min_len) <|
      P_Expression.Expression.BOOLEAN(left_justified) <| nil() => begin
        @assign format =
          "%" +
          (
            if left_justified
              "-"
            else
              ""
            end
          ) +
          intString(min_len) +
          "." +
          intString(significant_digits) +
          "g"
        @assign str = System.sprintff(format, r)
        P_Expression.Expression.STRING(str)
      end

      P_Expression.Expression.REAL(r) <| P_Expression.Expression.STRING(format) <| nil() => begin
        @assign str = System.sprintff(format, r)
        P_Expression.Expression.STRING(str)
      end
    end
  end
  return result
end

function evalBuiltinSum(arg::Expression)::Expression
  local result::Expression

  @assign result = begin
    @match arg begin
      P_Expression.Expression.ARRAY(__) => begin
        begin
          @match Type.arrayElementType(P_Expression.Expression.typeOf(arg)) begin
            TYPE_INTEGER(__) => begin
              P_Expression.Expression.INTEGER(P_Expression.Expression.fold(
                arg,
                evalBuiltinSumInt,
                0,
              ))
            end

            TYPE_REAL(__) => begin
              P_Expression.Expression.REAL(P_Expression.Expression.fold(
                arg,
                evalBuiltinSumReal,
                0.0,
              ))
            end

            _ => begin
              printWrongArgsError(getInstanceName(), list(arg), sourceInfo())
              fail()
            end
          end
        end
      end

      _ => begin
        printWrongArgsError(getInstanceName(), list(arg), sourceInfo())
        fail()
      end
    end
  end
  return result
end

function evalBuiltinSumInt(exp::Expression, result::Integer)::Integer

  @assign result = begin
    @match exp begin
      P_Expression.Expression.INTEGER(__) => begin
        result + exp.value
      end

      P_Expression.Expression.ARRAY(__) => begin
        result
      end

      _ => begin
        fail()
      end
    end
  end
  return result
end

function evalBuiltinSumReal(exp::Expression, result::AbstractFloat)::AbstractFloat

  @assign result = begin
    @match exp begin
      P_Expression.Expression.REAL(__) => begin
        result + exp.value
      end

      P_Expression.Expression.ARRAY(__) => begin
        result
      end

      _ => begin
        fail()
      end
    end
  end
  return result
end

function evalBuiltinSymmetric(arg::Expression)::Expression
  local result::Expression

  local mat::Array{Array{Expression}}
  local n::Integer
  local row_ty::M_Type
  local expl::List{Expression}
  local accum::List{Expression} = nil

  @assign result = begin
    @match arg begin
      P_Expression.Expression.ARRAY(__) where {(Type.isMatrix(arg.ty))} => begin
        @assign mat = listArray(List(
          listArray(P_Expression.Expression.arrayElements(row))
          for row in P_Expression.Expression.arrayElements(arg)
        ))
        @assign n = arrayLength(mat)
        @assign row_ty = Type.unliftArray(arg.ty)
        for i = n:(-1):1
          @assign expl = nil
          for j = n:(-1):1
            @assign expl = _cons(if i > j
              arrayGet(mat[j], i)
            else
              arrayGet(mat[i], j)
            end, expl)
          end
          @assign accum = _cons(
            P_Expression.Expression.makeArray(row_ty, expl, literal = true),
            accum,
          )
        end
        P_Expression.Expression.makeArray(arg.ty, accum, literal = true)
      end

      _ => begin
        printWrongArgsError(getInstanceName(), list(arg), sourceInfo())
        fail()
      end
    end
  end
  return result
end

function evalBuiltinTanh(arg::Expression)::Expression
  local result::Expression

  @assign result = begin
    @match arg begin
      P_Expression.Expression.REAL(__) => begin
        P_Expression.Expression.REAL(tanh(arg.value))
      end

      _ => begin
        printWrongArgsError(getInstanceName(), list(arg), sourceInfo())
        fail()
      end
    end
  end
  return result
end

function evalBuiltinTan(arg::Expression)::Expression
  local result::Expression

  @assign result = begin
    @match arg begin
      P_Expression.Expression.REAL(__) => begin
        P_Expression.Expression.REAL(tan(arg.value))
      end

      _ => begin
        printWrongArgsError(getInstanceName(), list(arg), sourceInfo())
        fail()
      end
    end
  end
  return result
end

function evalBuiltinTranspose(arg::Expression)::Expression
  local result::Expression

  local dim1::Dimension
  local dim2::Dimension
  local rest_dims::List{Dimension}
  local ty::M_Type
  local arr::List{Expression}
  local arrl::List{List{Expression}}
  local literal::Bool

  @assign result = begin
    @match arg begin
      P_Expression.Expression.ARRAY(
        ty = Type.ARRAY(elementType = ty, dimensions = dim1 <| dim2 <| rest_dims),
        elements = arr,
        literal = literal,
      ) => begin
        @assign arrl = List(P_Expression.Expression.arrayElements(e) for e in arr)
        @assign arrl = ListUtil.transposeList(arrl)
        @assign ty = Type.liftArrayLeft(ty, dim1)
        @assign arr =
          List(P_Expression.Expression.makeArray(ty, expl, literal) for expl in arrl)
        @assign ty = Type.liftArrayLeft(ty, dim2)
        P_Expression.Expression.makeArray(ty, arr, literal)
      end

      _ => begin
        printWrongArgsError(getInstanceName(), list(arg), sourceInfo())
        fail()
      end
    end
  end
  return result
end

function evalBuiltinVector(arg::Expression)::Expression
  local result::Expression

  local expl::List{Expression}
  local ty::M_Type

  @assign expl = P_Expression.Expression.fold(arg, evalBuiltinVector2, nil)
  @assign ty = Type.liftArrayLeft(
    Type.arrayElementType(P_Expression.Expression.typeOf(arg)),
    P_Dimension.Dimension.fromInteger(listLength(expl)),
  )
  @assign result = P_Expression.Expression.makeArray(ty, listReverse(expl), literal = true)
  return result
end

function evalBuiltinVector2(exp::Expression, expl::List{<:Expression})::List{Expression}

  @assign expl = begin
    @match exp begin
      P_Expression.Expression.ARRAY(__) => begin
        expl
      end

      _ => begin
        _cons(exp, expl)
      end
    end
  end
  return expl
end

function evalBuiltinZeros(args::List{<:Expression})::Expression
  local result::Expression

  @assign result = evalBuiltinFill2(P_Expression.Expression.INTEGER(0), args)
  return result
end

function evalUriToFilename(
  fn::M_Function,
  args::List{<:Expression},
  target::EvalTarget,
)::Expression
  local result::Expression

  local e::Expression
  local arg::Expression
  local s::String
  local f::M_Function

  @assign arg = listHead(args)
  @assign result = begin
    @match arg begin
      P_Expression.Expression.STRING(__) => begin
        @assign s = OpenModelica.Scripting.uriToFilename(arg.value)
        @assign e = P_Expression.Expression.STRING(s)
        e
      end

      _ => begin
        printWrongArgsError(getInstanceName(), list(arg), sourceInfo())
        fail()
      end
    end
  end
  return result
end

function evalIntBitAnd(args::List{<:Expression})::Expression
  local result::Expression

  local i1::Integer
  local i2::Integer

  @assign result = begin
    @match args begin
      P_Expression.Expression.INTEGER(value = i1) <|
      P_Expression.Expression.INTEGER(value = i2) <| nil() => begin
        P_Expression.Expression.INTEGER(intBitAnd(i1, i2))
      end

      _ => begin
        printWrongArgsError(getInstanceName(), args, sourceInfo())
        fail()
      end
    end
  end
  return result
end

function evalIntBitOr(args::List{<:Expression})::Expression
  local result::Expression

  local i1::Integer
  local i2::Integer

  @assign result = begin
    @match args begin
      P_Expression.Expression.INTEGER(value = i1) <|
      P_Expression.Expression.INTEGER(value = i2) <| nil() => begin
        P_Expression.Expression.INTEGER(intBitOr(i1, i2))
      end

      _ => begin
        printWrongArgsError(getInstanceName(), args, sourceInfo())
        fail()
      end
    end
  end
  return result
end

function evalIntBitXor(args::List{<:Expression})::Expression
  local result::Expression

  local i1::Integer
  local i2::Integer

  @assign result = begin
    @match args begin
      P_Expression.Expression.INTEGER(value = i1) <|
      P_Expression.Expression.INTEGER(value = i2) <| nil() => begin
        P_Expression.Expression.INTEGER(intBitXor(i1, i2))
      end

      _ => begin
        printWrongArgsError(getInstanceName(), args, sourceInfo())
        fail()
      end
    end
  end
  return result
end

function evalIntBitLShift(args::List{<:Expression})::Expression
  local result::Expression

  local i1::Integer
  local i2::Integer

  @assign result = begin
    @match args begin
      P_Expression.Expression.INTEGER(value = i1) <|
      P_Expression.Expression.INTEGER(value = i2) <| nil() => begin
        P_Expression.Expression.INTEGER(intBitLShift(i1, i2))
      end

      _ => begin
        printWrongArgsError(getInstanceName(), args, sourceInfo())
        fail()
      end
    end
  end
  return result
end

function evalIntBitRShift(args::List{<:Expression})::Expression
  local result::Expression

  local i1::Integer
  local i2::Integer

  @assign result = begin
    @match args begin
      P_Expression.Expression.INTEGER(value = i1) <|
      P_Expression.Expression.INTEGER(value = i2) <| nil() => begin
        P_Expression.Expression.INTEGER(intBitRShift(i1, i2))
      end

      _ => begin
        printWrongArgsError(getInstanceName(), args, sourceInfo())
        fail()
      end
    end
  end
  return result
end

function evalInferredClock(args::List{<:Expression})::Expression
  local result::Expression

  @assign result = begin
    @match args begin
      nil() => begin
        P_Expression.Expression.CLKCONST(P_Expression.P_ClockKind.Expression.INFERRED_CLOCK())
      end

      _ => begin
        printWrongArgsError(getInstanceName(), args, sourceInfo())
        fail()
      end
    end
  end
  return result
end

function evalRationalClock(args::List{<:Expression})::Expression
  local result::Expression

  @assign result = begin
    local interval::Expression
    local resolution::Expression
    @match args begin
      interval &&
      P_Expression.Expression.INTEGER(__) <| resolution &&
      P_Expression.Expression.INTEGER(__) <| nil() => begin
        P_Expression.Expression.CLKCONST(P_Expression.P_ClockKind.Expression.INTEGER_CLOCK(
          interval,
          resolution,
        ))
      end

      _ => begin
        printWrongArgsError(getInstanceName(), args, sourceInfo())
        fail()
      end
    end
  end
  return result
end

function evalRealClock(args::List{<:Expression})::Expression
  local result::Expression

  @assign result = begin
    local interval::Expression
    @match args begin
      interval && P_Expression.Expression.REAL(__) <| nil() => begin
        P_Expression.Expression.CLKCONST(P_Expression.P_ClockKind.Expression.REAL_CLOCK(
          interval,
        ))
      end

      _ => begin
        printWrongArgsError(getInstanceName(), args, sourceInfo())
        fail()
      end
    end
  end
  return result
end

function evalBooleanClock(args::List{<:Expression})::Expression
  local result::Expression

  @assign result = begin
    local condition::Expression
    local interval::Expression
    @match args begin
      condition &&
      P_Expression.Expression.BOOLEAN(__) <| interval &&
      P_Expression.Expression.REAL(__) <| nil() => begin
        P_Expression.Expression.CLKCONST(P_Expression.P_ClockKind.Expression.BOOLEAN_CLOCK(
          condition,
          interval,
        ))
      end

      _ => begin
        printWrongArgsError(getInstanceName(), args, sourceInfo())
        fail()
      end
    end
  end
  return result
end

function evalSolverClock(args::List{<:Expression})::Expression
  local result::Expression

  @assign result = begin
    local c::Expression
    local solver::Expression
    @match args begin
      c &&
      P_Expression.Expression.CLKCONST(__) <| solver &&
      P_Expression.Expression.STRING(__) <| nil() => begin
        P_Expression.Expression.CLKCONST(P_Expression.P_ClockKind.Expression.SOLVER_CLOCK(
          c,
          solver,
        ))
      end

      _ => begin
        printWrongArgsError(getInstanceName(), args, sourceInfo())
        fail()
      end
    end
  end
  return result
end

function evalBuiltinDynamicSelect(
  fn::M_Function,
  args::List{<:Expression},
  target::EvalTarget,
)::Expression
  local result::Expression

  local s::Expression
  local d::Expression

  @match list(s, d) = List(P_Expression.Expression.unbox(arg) for arg in args)
  @assign s = evalExp(s, target)
  @assign result = s
  return result
end

function evalArrayConstructor(
  exp::Expression,
  iterators::List{<:Tuple{<:InstNode, Expression}},
)::Expression
  local result::Expression

  @assign result = evalExpPartial(exp)
  @assign result = P_Expression.Expression.bindingExpMap(
    result,
    (iterators) -> evalArrayConstructor2(iterators = iterators),
  )
  return result
end

function evalArrayConstructor2(
  exp::Expression,
  iterators::List{<:Tuple{<:InstNode, Expression}},
)::Expression
  local result::Expression

  local e::Expression
  local ranges::List{Expression}
  local iters::List{Mutable{Expression}}
  local types::List{M_Type} = nil
  local ty::M_Type

  @assign (e, ranges, iters) = createIterationRanges(exp, iterators)
  #=  Precompute all the types we're going to need for the arrays created.
  =#
  @assign ty = P_Expression.Expression.typeOf(e)
  for r in ranges
    @assign ty =
      Type.liftArrayLeftList(ty, Type.arrayDims(P_Expression.Expression.typeOf(r)))
    @assign types = _cons(ty, types)
  end
  @assign result = evalArrayConstructor3(e, ranges, iters, types)
  return result
end

function createIterationRanges(
  exp::Expression,
  iterators::List{<:Tuple{<:InstNode, Expression}},
)::Tuple{Expression, List{Expression}, List{Mutable{Expression}}}
  local iters::List{Mutable{Expression}} = nil
  local ranges::List{Expression} = nil

  local node::InstNode
  local range::Expression
  local iter::Mutable{Expression}

  for i in iterators
    @assign (node, range) = i
    @assign iter = Mutable.create(P_Expression.Expression.INTEGER(0))
    @assign exp = P_Expression.Expression.replaceIterator(
      exp,
      node,
      P_Expression.Expression.MUTABLE(iter),
    )
    @assign iters = _cons(iter, iters)
    @assign ranges = _cons(evalExp_impl(range, P_EvalTarget.IGNORE_ERRORS()), ranges)
  end
  return (exp, ranges, iters)
end

function evalArrayConstructor3(
  exp::Expression,
  ranges::List{<:Expression},
  iterators::List{<:Mutable{<:Expression}},
  types::List{<:M_Type},
)::Expression
  local result::Expression

  local range::Expression
  local e::Expression
  local ranges_rest::List{Expression}
  local expl::List{Expression} = nil
  local iter::Mutable{Expression}
  local iters_rest::List{Mutable{Expression}}
  local range_iter::ExpressionIterator
  local value::Expression
  local ty::M_Type
  local rest_ty::List{M_Type}

  if listEmpty(ranges)
    @assign result = evalExp_impl(exp, P_EvalTarget.IGNORE_ERRORS())
  else
    @match _cons(range, ranges_rest) = ranges
    @match _cons(iter, iters_rest) = iterators
    @match _cons(ty, rest_ty) = types
    @assign range_iter = P_ExpressionIterator.ExpressionIterator.fromExp(range)
    while P_ExpressionIterator.ExpressionIterator.hasNext(range_iter)
      @assign (range_iter, value) = P_ExpressionIterator.ExpressionIterator.next(range_iter)
      Mutable.update(iter, value)
      @assign expl =
        _cons(evalArrayConstructor3(exp, ranges_rest, iters_rest, rest_ty), expl)
    end
    @assign result =
      P_Expression.Expression.makeArray(ty, listReverseInPlace(expl), literal = true)
  end
  return result
end

function evalReduction(
  fn::M_Function,
  exp::Expression,
  iterators::List{<:Tuple{<:InstNode, Expression}},
)::Expression
  local result::Expression

  @assign result = evalExpPartial(exp)
  @assign result = P_Expression.Expression.bindingExpMap(
    result,
    (fn, iterators) -> evalReduction2(fn = fn, iterators = iterators),
  )
  return result
end

function evalReduction2(
  fn::M_Function,
  exp::Expression,
  iterators::List{<:Tuple{<:InstNode, Expression}},
)::Expression
  local result::Expression

  local e::Expression
  local default_exp::Expression
  local ranges::List{Expression}
  local iters::List{Mutable{Expression}}
  local red_fn::ReductionFn
  local ty::M_Type

  @assign (e, ranges, iters) = createIterationRanges(exp, iterators)
  @assign ty = P_Expression.Expression.typeOf(e)
  @assign (red_fn, default_exp) = begin
    @match AbsynUtil.pathString(P_Function.name(fn)) begin
      "sum" => begin
        (evalBinaryAdd, P_Expression.Expression.makeZero(ty))
      end

      "product" => begin
        (evalBinaryMul, P_Expression.Expression.makeOne(ty))
      end

      "min" => begin
        (evalBuiltinMin2, P_Expression.Expression.makeMaxValue(ty))
      end

      "max" => begin
        (evalBuiltinMax2, P_Expression.Expression.makeMinValue(ty))
      end

      _ => begin
        Error.assertion(
          false,
          getInstanceName() +
          " got unknown reduction function " +
          AbsynUtil.pathString(P_Function.name(fn)),
          sourceInfo(),
        )
        fail()
      end
    end
  end
  @assign result = evalReduction3(e, ranges, iters, default_exp, red_fn)
  return result
end

function evalReduction3(
  exp::Expression,
  ranges::List{<:Expression},
  iterators::List{<:Mutable{<:Expression}},
  foldExp::Expression,
  fn::ReductionFn,
)::Expression
  local result::Expression

  local range::Expression
  local ranges_rest::List{Expression}
  local expl::List{Expression} = nil
  local iter::Mutable{Expression}
  local iters_rest::List{Mutable{Expression}}
  local range_iter::ExpressionIterator
  local value::Expression
  local el_ty::M_Type

  if listEmpty(ranges)
    @assign result = fn(foldExp, evalExp_impl(exp, P_EvalTarget.IGNORE_ERRORS()))
  else
    @match _cons(range, ranges_rest) = ranges
    @match _cons(iter, iters_rest) = iterators
    @assign range_iter = P_ExpressionIterator.ExpressionIterator.fromExp(range)
    @assign result = foldExp
    while P_ExpressionIterator.ExpressionIterator.hasNext(range_iter)
      @assign (range_iter, value) = P_ExpressionIterator.ExpressionIterator.next(range_iter)
      Mutable.update(iter, value)
      @assign result = evalReduction3(exp, ranges_rest, iters_rest, result, fn)
    end
  end
  return result
end

function evalSize(
  exp::Expression,
  optIndex::Option{<:Expression},
  target::EvalTarget,
)::Expression
  local outExp::Expression

  local index_exp::Expression
  local index::Integer
  local ty_err::TypingError
  local dim::Dimension
  local ty::M_Type
  local expl::List{Expression}
  local info::SourceInfo

  @assign info = P_EvalTarget.getInfo(target)
  if isSome(optIndex)
    @assign index_exp = evalExp_impl(Util.getOption(optIndex), target)
    @assign index = P_Expression.Expression.toInteger(index_exp)
    @assign (dim, _, ty_err) = Typing.typeExpDim(exp, index, ExpOrigin.CLASS, info)
    Typing.checkSizeTypingError(ty_err, exp, index, info)
    @assign outExp = P_Dimension.Dimension.sizeExp(dim)
  else
    @assign (outExp, ty) = Typing.typeExp(exp, ExpOrigin.CLASS, info)
    @assign expl = List(P_Dimension.Dimension.sizeExp(d) for d in Type.arrayDims(ty))
    @assign dim = P_Dimension.Dimension.fromInteger(listLength(expl), Variability.PARAMETER)
    @assign outExp =
      P_Expression.Expression.makeArray(Type.ARRAY(TYPE_INTEGER(), list(dim)), expl)
  end
  #=  Evaluate the index.
  =#
  #=  Get the index'd dimension of the expression.
  =#
  #=  Return the size expression for the found dimension.
  =#
  return outExp
end

function evalSubscriptedExp(
  exp::Expression,
  subscripts::List{<:Subscript},
  target::EvalTarget,
)::Expression
  local result::Expression

  local subs::List{Subscript}

  @assign result = begin
    @match exp begin
      P_Expression.Expression.RANGE(__) => begin
        P_Expression.Expression.RANGE(
          exp.ty,
          evalExp(exp.start, target),
          evalExpOpt(exp.step, target),
          evalExp(exp.stop, target),
        )
      end

      _ => begin
        evalExp_impl(exp, target)
      end
    end
  end
  @assign subs = List(
    mapShallowExp(s, (target) -> evalExp_impl(target = target))
    for s in subscripts
  )
  @assign result = P_Expression.Expression.applySubscripts(subs, result)
  return result
end

function evalRecordElement(exp::Expression, target::EvalTarget)::Expression
  local result::Expression

  local e::Expression
  local index::Integer

  @match P_Expression.Expression.RECORD_ELEMENT(recordExp = e, index = index) = exp
  @assign e = evalExp_impl(e, target)
  try
    @assign result =
      P_Expression.Expression.bindingExpMap(e, (index) -> evalRecordElement2(index = index))
  catch
    Error.assertion(
      false,
      getInstanceName() + " could not evaluate " + P_Expression.Expression.toString(exp),
      sourceInfo(),
    )
  end
  return result
end

function evalRecordElement2(exp::Expression, index::Integer)::Expression
  local result::Expression

  @assign result = begin
    @match exp begin
      P_Expression.Expression.RECORD(__) => begin
        listGet(exp.elements, index)
      end
    end
  end
  return result
end

function printUnboundError(component::Component, target::EvalTarget, exp::Expression)
  return @assign () = begin
    @match target begin
      P_EvalTarget.IGNORE_ERRORS(__) => begin
        ()
      end

      P_EvalTarget.DIMENSION(__) => begin
        Error.addSourceMessage(
          Error.STRUCTURAL_PARAMETER_OR_CONSTANT_WITH_NO_BINDING,
          list(P_Expression.Expression.toString(exp), name(target.component)),
          target.info,
        )
        fail()
      end

      P_EvalTarget.CONDITION(__) => begin
        Error.addSourceMessage(
          Error.CONDITIONAL_EXP_WITHOUT_VALUE,
          list(P_Expression.Expression.toString(exp)),
          target.info,
        )
        fail()
      end

      _ => begin
        #=  check if we have a parameter with (fixed = true), annotation(Evaluate = true) and no binding
        =#
        if listMember(
          P_Component.variability(component),
          list(Variability.STRUCTURAL_PARAMETER, Variability.PARAMETER),
        ) && P_Component.getEvaluateAnnotation(component)
          if P_Component.getFixedAttribute(component)
            Error.addMultiSourceMessage(
              Error.UNBOUND_PARAMETER_EVALUATE_TRUE,
              list(P_Expression.Expression.toString(exp) + "(fixed = true)"),
              list(
                info(node(P_Expression.Expression.toCref(
                  exp,
                ))),
                P_EvalTarget.getInfo(target),
              ),
            )
          end
        else
          Error.addMultiSourceMessage(
            Error.UNBOUND_CONSTANT,
            list(P_Expression.Expression.toString(exp)),
            list(
              info(node(P_Expression.Expression.toCref(
                exp,
              ))),
              P_EvalTarget.getInfo(target),
            ),
          )
          fail()
        end
        #=  only add an error if fixed = true
        =#
        #=  constant with no binding
        =#
        ()
      end
    end
  end
end

function printWrongArgsError(evalFunc::String, args::List{<:Expression}, info::SourceInfo)
  return Error.addInternalError(
    evalFunc +
    " got invalid arguments " +
    ListUtil.toString(args, P_Expression.Expression.toString, "", "(", ", ", ")", true),
    info,
  )
end
