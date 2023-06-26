#= /*
* This file is part of OpenModelica.
*
* Copyright (c) 1998-CurrentYear, Open Source Modelica Consortium (OSMC),
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

@UniontypeDecl EvalTarget
function EvalTarget_getInfo(target::EvalTarget)::SourceInfo
  local info::SourceInfo

  @assign info = begin
    @match target begin
      EVALTARGET_DIMENSION(__) => begin
        target.info
      end

      EVALTARGET_ATTRIBUTE(__) => begin
        Binding_getInfo(target.binding)
      end

      EVALTARGET_RANGE(__) => begin
        target.info
      end

      EVALTARGET_CONDITION(__) => begin
        target.info
      end

      EVALTARGET_GENERIC(__) => begin
        target.info
      end

      EVALTARGET_STATEMENT(__) => begin
        DAE.ElementSource_getInfo(target.source)
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
  local isR::Bool
  @assign isR = begin
    @match target begin
      EVALTARGET_RANGE(__) => begin
        true
      end
      _ => begin
        false
      end
    end
  end
  return isR
end

@Uniontype EvalTarget begin
  @Record EVALTARGET_IGNORE_ERRORS begin
  end

  @Record EVALTARGET_STATEMENT begin
    source::DAE.ElementSource
  end

  @Record EVALTARGET_GENERIC begin
    info::SourceInfo
  end

  @Record EVALTARGET_CONDITION begin
    info::SourceInfo
  end

  @Record EVALTARGET_RANGE begin
    info::SourceInfo
  end

  @Record EVALTARGET_ATTRIBUTE begin
    binding::Binding
  end

  @Record EVALTARGET_DIMENSION begin
    component::InstNode
    index::Int
    exp::Expression
    info::SourceInfo
  end
end

"""
  Evaluates an expression.
"""
function evalExp(
  exp::Expression,
  target::EvalTarget = EVALTARGET_IGNORE_ERRORS(),
)::Expression
  exp = getBindingExp(evalExp_impl(exp, target))
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
      TYPENAME_EXPRESSION(__) => begin
        evalTypename(exp.ty, exp, target)
      end
      ARRAY_EXPRESSION(__) => begin
        if exp.literal
          exp
        else
          makeArray(
            exp.ty,
            list(evalExp_impl(e, target) for e in exp.elements),
            literal = true,
          )
        end
      end
      RANGE_EXPRESSION(__) => begin
        evalRange(exp, target)
      end
      TUPLE_EXPRESSION(__) => begin
        @assign exp.elements = list(evalExp_impl(e, target) for e in exp.elements)
        exp
      end
      RECORD_EXPRESSION(__) => begin
        @assign exp.elements = list(evalExp_impl(e, target) for e in exp.elements)
        exp
      end
      CALL_EXPRESSION(__) => begin
        evalCall(exp.call, target)
      end
      SIZE_EXPRESSION(__) => begin
        evalSize(exp.exp, exp.dimIndex, target)
      end
      BINARY_EXPRESSION(__) => begin
        @assign exp1 = evalExp_impl(exp.exp1, target)
        @assign exp2 = evalExp_impl(exp.exp2, target)
        evalBinaryOp(exp1, exp.operator, exp2, target)
      end
      UNARY_EXPRESSION(__) => begin
        @assign exp1 = evalExp_impl(exp.exp, target)
        evalUnaryOp(exp1, exp.operator)
      end
      LBINARY_EXPRESSION(__) => begin
        @assign exp1 = evalExp_impl(exp.exp1, target)
        evalLogicBinaryOp(exp1, exp.operator, exp.exp2, target)
      end
      LUNARY_EXPRESSION(__) => begin
        @assign exp1 = evalExp_impl(exp.exp, target)
        evalLogicUnaryOp(exp1, exp.operator)
      end
      RELATION_EXPRESSION(__) => begin
        @assign exp1 = evalExp_impl(exp.exp1, target)
        @assign exp2 = evalExp_impl(exp.exp2, target)
        evalRelationOp(exp1, exp.operator, exp2)
      end
      IF_EXPRESSION(__) => begin
        evalIfExp(exp, target)
      end

      CAST_EXPRESSION(__) => begin
        @assign exp1 = evalExp_impl(exp.exp, target)
        evalCast(exp1, exp.ty)
      end

      UNBOX_EXPRESSION(__) => begin
        @assign exp1 = evalExp_impl(exp.exp, target)
        UNBOX_EXPRESSION(exp1, exp.ty)
      end

      SUBSCRIPTED_EXP_EXPRESSION(__) => begin
        evalSubscriptedExp(exp.exp, exp.subscripts, target)
      end

      TUPLE_ELEMENT_EXPRESSION(__) => begin
        @assign exp1 = evalExp_impl(exp.tupleExp, target)
        tupleElement(exp1, exp.ty, exp.index)
      end

      RECORD_ELEMENT_EXPRESSION(__) => begin
        evalRecordElement(exp, target)
      end

      MUTABLE_EXPRESSION(__) => begin
        @assign exp1 = evalExp_impl(P_Pointer.access(exp.exp), target)
        exp1
      end

      BINDING_EXP(__) => begin
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
  oexp::Option{Expression},
  target::EvalTarget = EVALTARGET_IGNORE_ERRORS(),
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

"""
   Evaluates the parts of an expression that are possible to evaluate. This
   means leaving parts of the expression that contains e.g. iterators or mutable
   expressions. This can be used to optimize an expression that is expected to
   be evaluated many times, for example the expression in an array constructor.
"""
function evalExpPartial(
  exp::Expression,
  target::EvalTarget = EVALTARGET_IGNORE_ERRORS(),
  evaluated::Bool = true,
)::Tuple{Expression, Bool}
  local outEvaluated::Bool #= True if the whole expression is evaluated, otherwise false. =#
  local outExp::Expression
  local e::Expression
  local e1::Expression
  local e2::Expression
  local eval1::Bool
  local eval2::Bool
  (e, outEvaluated) = mapFoldShallow(
    exp,
    (expArg, boolArg) -> evalExpPartial(expArg, target, boolArg),
    true,
  )
  outExp = begin
    @match e begin
      CREF_EXPRESSION(__) => begin
        if isIterator(e.cref)
          outExp = e
          outEvaluated = false
        else
          outExp = evalCref(e.cref, e, target, evalSubscripts = false)
        end
        #=  Don't evaluate iterators.
        =#
        #=  Crefs can be evaluated even if they have non-evaluated subscripts.
        =#
        outExp
      end

      MUTABLE_EXPRESSION(__) => begin
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
  target::EvalTarget;
  evalSubscripts::Bool = true,
)::Expression
  local exp::Expression
  local c::InstNode
  local evaled::Bool
  exp = begin
    @match cref begin
      COMPONENT_REF_CREF(
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
  local var::VariabilityType
  local start_exp::Option{Expression}

  @assign exp_origin = if isFunction(explicitParent(node))
    ORIGIN_FUNCTION
  else
    ORIGIN_CLASS
  end
  typeComponentBinding2(node, exp_origin, false)
  @assign comp = component(node)
  @assign binding = getBinding(comp)
  if isUnbound(binding)
    @assign binding =
      makeComponentBinding(comp, node, toCref(defaultExp), target)
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
          @assign comp = setBinding(binding, comp)
          updateComponent!(comp, node)
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
      BINDING_EXP(exp = BINDING_EXP(__)) => begin
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

  #=  The subscripts of the first part of the cref are always applied. =#
  subs = getSubscripts(cref)
  (cr, _) = stripSubscripts(cref)
  if evalSubscripts
    subs = list(eval(s) for s in subs)
  end
  #=  The rest of the cref contributes subscripts based on where the expressions
  =#
  #=  comes from in the instance tree.
  =#
  exp = subscriptEvaluatedBinding2(exp, cr, evalSubscripts, subs, subs)
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
      BINDING_EXP(bindingType = bind_ty, parents = parents) =>
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
              subs = list(eval(s) for s in subs)
            end
            accum_subs = listAppend(subs, accum_subs)
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
            subs = bindingSubs
            bind_ty = subscript(bind_ty, subs)
          end
          e =
            subscriptEvaluatedBinding2(exp.exp, cr, evalSubscripts, accum_subs, subs)
          exp_ty = typeOf(e)
          BINDING_EXP(e, exp_ty, bind_ty, exp.parents, exp.isEach)
        end

      _ => begin
        applySubscripts(subscripts, exp)
      end
    end
  end
  return exp
end

"""
Tries to evaluate the given component's start value. NONE() is returned if
the component isn't a fixed parameter or if it doesn't have a start value.
Otherwise the evaluated binding expression is returned if it could be
evaluated, or the function will fail if it couldn't be.
"""
function evalComponentStartBinding(
  node::InstNode,
  comp::Component,
  cref::ComponentRef,
  target::EvalTarget,
  evalSubscripts::Bool,
)::Option{Expression}
  local outExp::Option{Expression} = NONE()

  local var::VariabilityType
  local start_node::InstNode
  local start_comp::Component
  local binding::Binding
  local exp::Expression
  local subs::List{Subscript}
  local pcount::Int

  #=  Only use the start value if the component is a fixed parameter.
  =#
  @assign var = variability(comp)
  if var != Variability.PARAMETER && var != Variability.STRUCTURAL_PARAMETER ||
     !getFixedAttribute(comp)
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
  binding = getBinding(start_comp)
  outExp = begin
    @match binding begin
      TYPED_BINDING(__) => begin
        @assign exp = evalExp_impl(binding.bindingExp, target)
        if !referenceEq(exp, binding.bindingExp)
          binding.bindingExp = exp
          start_comp = setBinding(binding, start_comp)
          updateComponent!(start_comp, start_node)
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
  local comps::Vector{InstNode}
  local fields::List{Expression}
  local ty::M_Type
  local exp_ty::M_Type
  local rec_node::InstNode
  local exp::Expression
  local rest_cr::ComponentRef

  @assign binding = begin
    @matchcontinue (component, cref) begin
      (
        TYPED_COMPONENT(
          ty = TYPE_COMPLEX(complexTy = COMPLEX_RECORD(rec_node)),
        ),
        _,
      ) => begin
        #=  A record component without an explicit binding, create one from its children.
        =#
        @assign exp =
          makeRecordBindingExp(component.classInst, rec_node, component.ty, cref)
        @assign exp_ty = typeOf(exp)
        @assign exp =
          BINDING_EXP(exp, exp_ty, exp_ty, list(node), true)
        @assign binding = CEVAL_BINDING(exp)
        if !hasSubscripts(cref)
          updateComponent!(P_Component.setBinding(binding, component), node)
        end
        binding
      end

      (
        TYPED_COMPONENT(
          ty = ty && TYPE_ARRAY(
            elementType = TYPE_COMPLEX(complexTy = COMPLEX_RECORD(rec_node)),
          ),
        ),
        _,
      ) => begin
        #=  A record array component without an explicit binding, create one from its children.
        =#
        @assign exp =
          makeRecordBindingExp(component.classInst, rec_node, component.ty, cref)
        @assign exp = splitRecordArrayExp(exp)
        @assign exp_ty = typeOf(exp)
        @assign exp =
          BINDING_EXP(exp, exp_ty, exp_ty, list(node), true)
        @assign binding = CEVAL_BINDING(exp)
        if !hasSubscripts(cref)
          updateComponent!(P_Component.setBinding(binding, component), node)
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
        EMPTY_BINDING
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
  @match true = Type.isRecord(arrayElementType(parent_ty))
  try
    @assign exp = evalCref(parent_cr, EMPTY_EXPRESSION(parent_ty), target)
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
    recordElement(firstName(cref), exp)
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
  local comps::Vector{InstNode}
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
    if variability(component(c)) <= Variability.PARAMETER
      @assign arg = evalExp_impl(arg, EVALTARGET_IGNORE_ERRORS())
    end
    @assign args = _cons(arg, args)
  end
  @assign exp =
    makeRecord(scopePath(recordNode), recordType, args)
  return exp
end

function splitRecordArrayExp(exp::Expression)::Expression

  local path::Absyn.Path
  local ty::M_Type
  local expl::List{Expression}

  @match RECORD_EXPRESSION(path, ty, expl) = exp
  @assign exp = makeRecord(path, arrayElementType(ty), expl)
  @assign exp = fillType(ty, exp)
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
  local max_prop_count::Int

  @match RANGE_EXPRESSION(
    ty = ty,
    start = start_exp,
    step = step_exp,
    stop = stop_exp,
  ) = rangeExp
  @assign start_exp = evalExp(start_exp, target)
  @assign step_exp = evalExpOpt(step_exp, target)
  @assign stop_exp = evalExp(stop_exp, target)
  if isRange(target)
    @assign ty = getRangeType(
      start_exp,
      step_exp,
      stop_exp,
      arrayElementType(ty),
      EvalTarget_getInfo(target),
    )
    @assign result = RANGE_EXPRESSION(ty, start_exp, step_exp, stop_exp)
  else
    @assign result = RANGE_EXPRESSION(ty, start_exp, step_exp, stop_exp)
    @assign result = bindingExpMap(result, evalRangeExp)
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
  local istep::Int

  @match RANGE_EXPRESSION(start = start, step = opt_step, stop = stop) =
    rangeExp
  if isSome(opt_step)
    @match SOME(step) = opt_step
    @assign (ty, expl) = begin
      @match (start, step, stop) begin
        (
          INTEGER_EXPRESSION(__),
          INTEGER_EXPRESSION(istep),
          INTEGER_EXPRESSION(__),
        ) => begin
          #=  The compiler decided to randomly dislike using step.value here, hence istep.
          =#
          @assign expl = list(
            INTEGER_EXPRESSION(i) for i = (start.value):istep:(stop.value)
          )
          (TYPE_INTEGER(), expl)
        end

        (
          REAL_EXPRESSION(__),
          REAL_EXPRESSION(__),
          REAL_EXPRESSION(__),
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
        (INTEGER_EXPRESSION(__), INTEGER_EXPRESSION(__)) =>
          begin
            @assign expl =
              list(INTEGER_EXPRESSION(i) for i = (start.value):(stop.value))
            (TYPE_INTEGER(), expl)
          end

        (REAL_EXPRESSION(__), REAL_EXPRESSION(__)) => begin
          @assign expl = evalRangeReal(start.value, 1.0, stop.value)
          (TYPE_REAL(), expl)
        end

        (BOOLEAN_EXPRESSION(__), BOOLEAN_EXPRESSION(__)) =>
          begin
            @assign expl =
              list(BOOLEAN_EXPRESSION(b) for b = (start.value):(stop.value))
            (TYPE_BOOLEAN(), expl)
          end

        (
          ENUM_LITERAL_EXPRESSION(ty = ty && TYPE_ENUMERATION(__)),
          ENUM_LITERAL_EXPRESSION(__),
        ) => begin
          @assign expl = list(
            ENUM_LITERAL_EXPRESSION(ty, listGet(ty.literals, i), i)
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
  exp = makeArray(
    TYPE_ARRAY(ty, list(fromInteger(listLength(expl)))), expl
    ;literal = true,
  )
  return exp
end

function evalRangeReal(
  start::AbstractFloat,
  step::AbstractFloat,
  stop::AbstractFloat,
)::List{Expression}
  local result::List{Expression}

  local steps::Int

  @assign steps = Util.realRangeSize(start, step, stop)
  #=  Real ranges are tricky, make sure that start and stop are reproduced
  =#
  #=  exactly if they are part of the range.
  =#
  if steps == 0
    @assign result = nil
  elseif steps == 1
    @assign result = list(REAL_EXPRESSION(start))
  else
    @assign result = list(REAL_EXPRESSION(stop))
    for i = (steps - 2):(-1):1
      @assign result = _cons(REAL_EXPRESSION(start + i * step), result)
    end
    @assign result = _cons(REAL_EXPRESSION(start), result)
  end
  return result
end

function printFailedEvalError(name::String, exp::Expression, info::SourceInfo)
  return Error.addInternalError(
    name + " failed to evaluate ‘" + toString(exp) + "‘",
    info,
  )
end

function evalBinaryOp(
  exp1::Expression,
  op::Operator,
  exp2::Expression,
  target::EvalTarget = EVALTARGET_IGNORE_ERRORS(),
)::Expression
  local exp::Expression

  local max_prop_exp::Expression
  local max_prop_count::Int

  @assign (max_prop_exp, max_prop_count) =
    mostPropagatedSubExpBinary(exp1, exp2)
  if max_prop_count >= 0
    @assign exp = bindingExpMap2(
      BINARY_EXPRESSION(exp1, op, exp2),
      (x) -> evalBinaryExp(x, target), #Change by me:) John
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
  target::EvalTarget = EVALTARGET_IGNORE_ERRORS(),
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
        evalBinaryScalarArray(exp1, exp2, (x, y) -> evalBinaryDiv(x, y, target))
      end

      Op.DIV_ARRAY_SCALAR => begin
        evalBinaryArrayScalar(exp1, exp2, (x, y) -> evalBinaryDiv(x, y, target))
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
          toString(BINARY_EXPRESSION(exp1, op, exp2)),
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
      (INTEGER_EXPRESSION(__), INTEGER_EXPRESSION(__)) => begin
        INTEGER_EXPRESSION(exp1.value + exp2.value)
      end

      (REAL_EXPRESSION(__), REAL_EXPRESSION(__)) => begin
        REAL_EXPRESSION(exp1.value + exp2.value)
      end

      (STRING_EXPRESSION(__), STRING_EXPRESSION(__)) => begin
        STRING_EXPRESSION(exp1.value + exp2.value)
      end

      (
        ARRAY_EXPRESSION(__),
        ARRAY_EXPRESSION(__),
      ) where {(listLength(exp1.elements) == listLength(exp2.elements))} => begin
        makeArray(
          exp1.ty,
          list(@do_threaded_for evalBinaryAdd(e1, e2) (e1, e2) (
            exp1.elements,
            exp2.elements,
          )),
          literal = true,
        )
      end

      _ => begin
        @assign exp = BINARY_EXPRESSION(
          exp1,
          makeAdd(TYPE_UNKNOWN()),
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
      (INTEGER_EXPRESSION(__), INTEGER_EXPRESSION(__)) => begin
        INTEGER_EXPRESSION(exp1.value - exp2.value)
      end

      (REAL_EXPRESSION(__), REAL_EXPRESSION(__)) => begin
        REAL_EXPRESSION(exp1.value - exp2.value)
      end

      (
        ARRAY_EXPRESSION(__),
        ARRAY_EXPRESSION(__),
      ) where {(listLength(exp1.elements) == listLength(exp2.elements))} => begin
        makeArray(
          exp1.ty,
          list(@do_threaded_for evalBinarySub(e1, e2) (e1, e2) (
            exp1.elements,
            exp2.elements,
          )),
          literal = true,
        )
      end

      _ => begin
        @assign exp = BINARY_EXPRESSION(
          exp1,
          makeSub(TYPE_UNKNOWN()),
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
      (INTEGER_EXPRESSION(__), INTEGER_EXPRESSION(__)) => begin
        INTEGER_EXPRESSION(exp1.value * exp2.value)
      end

      (REAL_EXPRESSION(__), REAL_EXPRESSION(__)) => begin
        REAL_EXPRESSION(exp1.value * exp2.value)
      end

      (
        ARRAY_EXPRESSION(__),
        ARRAY_EXPRESSION(__),
      ) where {(listLength(exp1.elements) == listLength(exp2.elements))} => begin
        makeArray(
          exp1.ty,
          list(@do_threaded_for evalBinaryMul(e1, e2) (e1, e2) (
            exp1.elements,
            exp2.elements,
          )),
          literal = true,
        )
      end

      _ => begin
        @assign exp = BINARY_EXPRESSION(
          exp1,
          makeMul(TYPE_UNKNOWN()),
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
      (_, REAL_EXPRESSION(0.0)) => begin
        if P_EvalTarget.hasInfo(target)
          Error.addSourceMessage(
            Error.DIVISION_BY_ZERO,
            list(
              toString(exp1),
              toString(exp2),
            ),
            EvalTarget_getInfo(target),
          )
          fail()
        else
          @assign exp = BINARY_EXPRESSION(
            exp1,
            makeDiv(TYPE_REAL()),
            exp2,
          )
        end
        exp
      end

      (REAL_EXPRESSION(__), REAL_EXPRESSION(__)) => begin
        REAL_EXPRESSION(exp1.value / exp2.value)
      end

      (
        ARRAY_EXPRESSION(__),
        ARRAY_EXPRESSION(__),
      ) where {(listLength(exp1.elements) == listLength(exp2.elements))} => begin
        makeArray(
          exp1.ty,
          list(@do_threaded_for evalBinaryDiv(e1, e2, target) (e1, e2) (
            exp1.elements,
            exp2.elements,
          )),
          literal = true,
        )
      end

      _ => begin
        @assign exp = BINARY_EXPRESSION(
          exp1,
          makeDiv(TYPE_UNKNOWN()),
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
      (REAL_EXPRESSION(__), REAL_EXPRESSION(__)) => begin
        REAL_EXPRESSION(exp1.value^exp2.value)
      end

      (
        ARRAY_EXPRESSION(__),
        ARRAY_EXPRESSION(__),
      ) where {(listLength(exp1.elements) == listLength(exp2.elements))} => begin
        makeArray(
          exp1.ty,
          list(@do_threaded_for evalBinaryPow(e1, e2) (e1, e2) (
            exp1.elements,
            exp2.elements,
          )),
          literal = true,
        )
      end

      _ => begin
        @assign exp = BINARY_EXPRESSION(
          exp1,
          makePow(TYPE_UNKNOWN()),
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
      ARRAY_EXPRESSION(__) => begin
        makeArray(
          arrayExp.ty,
          list(evalBinaryScalarArray(scalarExp, e, opFunc) for e in arrayExp.elements),
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
  exp = begin
    @match arrayExp begin
      ARRAY_EXPRESSION(__) => begin
        ARRAY_EXPRESSION(
          arrayExp.ty,
          list(evalBinaryArrayScalar(e, scalarExp, opFunc) for e in arrayExp.elements),
          true,
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
    @match transposeArray(matrixExp) begin
      ARRAY_EXPRESSION(TYPE_ARRAY(ty, m <| _ <| nil()), expl) => begin
        @assign expl = list(evalBinaryScalarProduct(vectorExp, e) for e in expl)
        makeArray(TYPE_ARRAY(ty, list(m)), expl, literal = true)
      end

      _ => begin
        @assign exp = BINARY_EXPRESSION(
          vectorExp,
          makeMul(TYPE_UNKNOWN()),
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
      ARRAY_EXPRESSION(TYPE_ARRAY(ty, n <| _ <| nil()), expl) => begin
        @assign expl = list(evalBinaryScalarProduct(e, vectorExp) for e in expl)
        makeArray(TYPE_ARRAY(ty, list(n)), expl, literal = true)
      end

      _ => begin
        @assign exp = BINARY_EXPRESSION(
          matrixExp,
          makeMul(TYPE_UNKNOWN()),
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
        ARRAY_EXPRESSION(ty = TYPE_ARRAY(elem_ty)),
        ARRAY_EXPRESSION(__),
      ) where {(listLength(exp1.elements) == listLength(exp2.elements))} => begin
        @assign exp = makeZero(elem_ty)
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
          makeMul(TYPE_UNKNOWN()),
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

  @assign e2 = transposeArray(exp2)
  @assign exp = begin
    @match (exp1, e2) begin
      (
        ARRAY_EXPRESSION(TYPE_ARRAY(elem_ty, n <| _ <| nil()), expl1),
        ARRAY_EXPRESSION(TYPE_ARRAY(_, p <| _ <| nil()), expl2),
      ) => begin
        @assign mat_ty = TYPE_ARRAY(elem_ty, list(n, p))
        if listEmpty(expl2)
          @assign exp = makeZero(mat_ty)
        else
          @assign row_ty = TYPE_ARRAY(elem_ty, list(p))
          @assign expl1 = list(
            makeArray(
              row_ty,
              list(evalBinaryScalarProduct(r, c) for c in expl2),
              literal = true,
            ) for r in expl1
          )
          @assign exp = makeArray(mat_ty, expl1, literal = true)
        end
        exp
      end

      _ => begin
        @assign exp = BINARY_EXPRESSION(
          exp1,
          makeMul(TYPE_UNKNOWN()),
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

  local n::Int

  @assign exp = begin
    @match (matrixExp, nExp) begin
      (ARRAY_EXPRESSION(__), INTEGER_EXPRESSION(value = 0)) =>
        begin
          @assign n = P_Dimension.Dimension.size(listHead(arrayDims(matrixExp.ty)))
          makeIdentityMatrix(n, TYPE_REAL())
        end

      (_, INTEGER_EXPRESSION(value = n)) => begin
        evalBinaryPowMatrix2(matrixExp, n)
      end

      _ => begin
        @assign exp = BINARY_EXPRESSION(
          matrixExp,
          makePow(TYPE_UNKNOWN()),
          nExp,
        )
        printFailedEvalError(getInstanceName(), exp, sourceInfo())
        fail()
      end
    end
  end
  return exp
end

function evalBinaryPowMatrix2(matrix::Expression, n::Int)::Expression
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
        bindingExpMap(exp1, evalUnaryMinus)
      end

      _ => begin
        Error.addInternalError(
          getInstanceName() +
          ": unimplemented case for " +
          toString(UNARY_EXPRESSION(op, exp1)),
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
      INTEGER_EXPRESSION(__) => begin
        INTEGER_EXPRESSION(-exp1.value)
      end

      REAL_EXPRESSION(__) => begin
        REAL_EXPRESSION(-exp1.value)
      end

      ARRAY_EXPRESSION(__) => begin
        @assign exp1.elements = list(evalUnaryMinus(e) for e in exp1.elements)
        exp1
      end

      _ => begin
        @assign exp = UNARY_EXPRESSION(
          makeUMinus(TYPE_UNKNOWN()),
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
  target::EvalTarget = EVALTARGET_IGNORE_ERRORS(),
)::Expression
  local exp::Expression

  local e1::Expression
  local max_prop_exp::Expression
  local max_prop_count::Int

  @assign (max_prop_exp, max_prop_count) =
    mostPropagatedSubExpBinary(exp1, exp2)
  if max_prop_count >= 0
    @assign exp = bindingExpMap2(
      LBINARY_EXPRESSION(exp1, op, exp2),
      (binaryExpArg) -> evalLogicBinaryExp(binaryExpArg, target),
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

  @match LBINARY_EXPRESSION(exp1 = e1, operator = op, exp2 = e2) = binaryExp
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
          toString(LBINARY_EXPRESSION(exp1, op, exp2)),
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
      BOOLEAN_EXPRESSION(__) => begin
        if exp1.value
          evalExp_impl(exp2, target)
        else
          exp1
        end
      end

      ARRAY_EXPRESSION(__) => begin
        @match ARRAY_EXPRESSION(elements = expl) = evalExp_impl(exp2, target)
        @assign expl =
          list(@do_threaded_for evalLogicBinaryAnd(e1, e2, target) (e1, e2) (
            exp1.elements,
            expl,
          ))
        makeArray(
          setArrayElementType(exp1.ty, TYPE_BOOLEAN()),
          expl,
          literal = true,
        )
      end

      _ => begin
        @assign exp = LBINARY_EXPRESSION(
          exp1,
          makeAnd(TYPE_UNKNOWN()),
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
      BOOLEAN_EXPRESSION(__) => begin
        if exp1.value
          exp1
        else
          evalExp_impl(exp2, target)
        end
      end

      ARRAY_EXPRESSION(__) => begin
        @match ARRAY_EXPRESSION(elements = expl) = evalExp_impl(exp2, target)
        @assign expl =
          list(@do_threaded_for evalLogicBinaryOr(e1, e2, target) (e1, e2) (
            exp1.elements,
            expl,
          ))
        makeArray(
          setArrayElementType(exp1.ty, TYPE_BOOLEAN()),
          expl,
          literal = true,
        )
      end

      _ => begin
        @assign exp = LBINARY_EXPRESSION(
          exp1,
          makeOr(TYPE_UNKNOWN()),
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
        bindingExpMap(exp1, evalLogicUnaryNot)
      end

      _ => begin
        Error.addInternalError(
          getInstanceName() +
          ": unimplemented case for " +
          toString(LUNARY_EXPRESSION(op, exp1)),
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
      BOOLEAN_EXPRESSION(__) => begin
        BOOLEAN_EXPRESSION(!exp1.value)
      end

      ARRAY_EXPRESSION(__) => begin
        mapArrayElements(exp1, evalLogicUnaryNot)
      end

      _ => begin
        @assign exp = LUNARY_EXPRESSION(
          makeNot(TYPE_UNKNOWN()),
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
  local max_prop_count::Int

  @assign (max_prop_exp, max_prop_count) =
    mostPropagatedSubExpBinary(exp1, exp2)
  if max_prop_count >= 0
    @assign exp = bindingExpMap2(
      RELATION_EXPRESSION(exp1, op, exp2),
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

  @match RELATION_EXPRESSION(exp1 = e1, operator = op, exp2 = e2) = relationExp
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
          toString(RELATION_EXPRESSION(
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
  @assign exp = BOOLEAN_EXPRESSION(res)
  return exp
end

function evalRelationLess(exp1::Expression, exp2::Expression)::Bool
  local res::Bool

  @assign res = begin
    @match (exp1, exp2) begin
      (INTEGER_EXPRESSION(__), INTEGER_EXPRESSION(__)) => begin
        exp1.value < exp2.value
      end

      (REAL_EXPRESSION(__), REAL_EXPRESSION(__)) => begin
        exp1.value < exp2.value
      end

      (BOOLEAN_EXPRESSION(__), BOOLEAN_EXPRESSION(__)) => begin
        exp1.value < exp2.value
      end

      (STRING_EXPRESSION(__), STRING_EXPRESSION(__)) => begin
        stringCompare(exp1.value, exp2.value) < 0
      end

      (
        ENUM_LITERAL_EXPRESSION(__),
        ENUM_LITERAL_EXPRESSION(__),
      ) => begin
        exp1.index < exp2.index
      end

      _ => begin
        printFailedEvalError(
          getInstanceName(),
          RELATION_EXPRESSION(
            exp1,
            makeLess(TYPE_UNKNOWN()),
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
      (INTEGER_EXPRESSION(__), INTEGER_EXPRESSION(__)) => begin
        exp1.value <= exp2.value
      end

      (REAL_EXPRESSION(__), REAL_EXPRESSION(__)) => begin
        exp1.value <= exp2.value
      end

      (BOOLEAN_EXPRESSION(__), BOOLEAN_EXPRESSION(__)) => begin
        exp1.value <= exp2.value
      end

      (STRING_EXPRESSION(__), STRING_EXPRESSION(__)) => begin
        stringCompare(exp1.value, exp2.value) <= 0
      end

      (
        ENUM_LITERAL_EXPRESSION(__),
        ENUM_LITERAL_EXPRESSION(__),
      ) => begin
        exp1.index <= exp2.index
      end

      _ => begin
        printFailedEvalError(
          getInstanceName(),
          RELATION_EXPRESSION(
            exp1,
            makeLessEq(TYPE_UNKNOWN()),
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
      (INTEGER_EXPRESSION(__), INTEGER_EXPRESSION(__)) => begin
        exp1.value > exp2.value
      end

      (REAL_EXPRESSION(__), REAL_EXPRESSION(__)) => begin
        exp1.value > exp2.value
      end

      (BOOLEAN_EXPRESSION(__), BOOLEAN_EXPRESSION(__)) => begin
        exp1.value > exp2.value
      end

      (STRING_EXPRESSION(__), STRING_EXPRESSION(__)) => begin
        stringCompare(exp1.value, exp2.value) > 0
      end

      (
        ENUM_LITERAL_EXPRESSION(__),
        ENUM_LITERAL_EXPRESSION(__),
      ) => begin
        exp1.index > exp2.index
      end

      _ => begin
        printFailedEvalError(
          getInstanceName(),
          RELATION_EXPRESSION(
            exp1,
            makeGreater(TYPE_UNKNOWN()),
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
      (INTEGER_EXPRESSION(__), INTEGER_EXPRESSION(__)) => begin
        exp1.value >= exp2.value
      end

      (REAL_EXPRESSION(__), REAL_EXPRESSION(__)) => begin
        exp1.value >= exp2.value
      end

      (BOOLEAN_EXPRESSION(__), BOOLEAN_EXPRESSION(__)) => begin
        exp1.value >= exp2.value
      end

      (STRING_EXPRESSION(__), STRING_EXPRESSION(__)) => begin
        stringCompare(exp1.value, exp2.value) >= 0
      end

      (
        ENUM_LITERAL_EXPRESSION(__),
        ENUM_LITERAL_EXPRESSION(__),
      ) => begin
        exp1.index >= exp2.index
      end

      _ => begin
        printFailedEvalError(
          getInstanceName(),
          RELATION_EXPRESSION(
            exp1,
            makeGreaterEq(TYPE_UNKNOWN()),
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
      (INTEGER_EXPRESSION(__), INTEGER_EXPRESSION(__)) => begin
        exp1.value == exp2.value
      end

      (REAL_EXPRESSION(__), REAL_EXPRESSION(__)) => begin
        exp1.value == exp2.value
      end

      (BOOLEAN_EXPRESSION(__), BOOLEAN_EXPRESSION(__)) => begin
        exp1.value == exp2.value
      end

      (STRING_EXPRESSION(__), STRING_EXPRESSION(__)) => begin
        stringCompare(exp1.value, exp2.value) == 0
      end

      (
        ENUM_LITERAL_EXPRESSION(__),
        ENUM_LITERAL_EXPRESSION(__),
      ) => begin
        exp1.index == exp2.index
      end

      _ => begin
        printFailedEvalError(
          getInstanceName(),
          RELATION_EXPRESSION(
            exp1,
            makeEqual(TYPE_UNKNOWN()),
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
      (INTEGER_EXPRESSION(__), INTEGER_EXPRESSION(__)) => begin
        exp1.value != exp2.value
      end

      (REAL_EXPRESSION(__), REAL_EXPRESSION(__)) => begin
        exp1.value != exp2.value
      end

      (BOOLEAN_EXPRESSION(__), BOOLEAN_EXPRESSION(__)) => begin
        exp1.value != exp2.value
      end

      (STRING_EXPRESSION(__), STRING_EXPRESSION(__)) => begin
        stringCompare(exp1.value, exp2.value) != 0
      end

      (
        ENUM_LITERAL_EXPRESSION(__),
        ENUM_LITERAL_EXPRESSION(__),
      ) => begin
        exp1.index != exp2.index
      end

      _ => begin
        printFailedEvalError(
          getInstanceName(),
          RELATION_EXPRESSION(
            exp1,
            makeNotEqual(TYPE_UNKNOWN()),
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

  @match IF_EXPRESSION(
    condition = cond,
    trueBranch = btrue,
    falseBranch = bfalse,
  ) = ifExp
  @assign result = IF_EXPRESSION(evalExp_impl(cond, target), btrue, bfalse)
  @assign result =
    bindingExpMap(result, (x) -> evalIfExp2(x, target))
  return result
end

function evalIfExp2(ifExp::Expression, target::EvalTarget)::Expression
  local result::Expression
  local cond::Expression
  local btrue::Expression
  local bfalse::Expression
  @match IF_EXPRESSION(
    condition = cond,
    trueBranch = btrue,
    falseBranch = bfalse,
  ) = ifExp
  result = begin
    @match cond begin
      BOOLEAN_EXPRESSION(__) => begin
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
          toString(IF_EXPRESSION(cond, btrue, bfalse)),
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

  @assign exp = typeCast(castExp, castTy)
  #=  Expression.typeCast will just create a CAST if it can't typecast
  =#
  #=  the expression, so make sure we actually got something else back.
  =#
  @assign () = begin
    @match exp begin
      CAST_EXPRESSION(__) => begin
        @assign exp = CAST_EXPRESSION(castTy, castExp)
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
  exp = begin
    local args::List{Expression}
    @match c begin
      TYPED_CALL(__) => begin
        @assign c.arguments = list(evalExp_impl(arg, target) for arg in c.arguments)
        if isBuiltin(c.fn)
          bindingExpMap(
            CALL_EXPRESSION(c),
            (x) -> evalxp(x, target),
          )
        else
          bindingExpMap(
            CALL_EXPRESSION(c),
            evalNormalCallExp,
          )
        end
      end
      TYPED_ARRAY_CONSTRUCTOR(__) => begin
        evalArrayConstructor(c.exp, c.iters)
      end
      TYPED_REDUCTION(__) => begin
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

#Note was originally called evalBuiltinCallExp, but the translator seems to have butchered the name for this function..
function evalxp(callExp::Expression, target::EvalTarget)::Expression
  local result::Expression
  local fn::M_Function
  local args::List{Expression}
  @match CALL_EXPRESSION(call = TYPED_CALL(fn = fn, arguments = args)) = callExp
  result = eval(fn, args, target)
  return result
end

function eval(
  fn::M_Function,
  args::List{Expression},
  target::EvalTarget,
)::Expression
  local result::Expression

  local fn_path::Absyn.Path = nameConsiderBuiltin(fn)

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
        evalBuiltinDiagonal(unbox(listHead(args)))
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

      "Int" => begin
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
  @match CALL_EXPRESSION(call = TYPED_CALL(fn = fn, arguments = args)) =
    callExp
  result = evalNormalCall(fn, args)
  return result
end

function evalNormalCall(fn::M_Function, args::List{Expression})::Expression
  local result::Expression = evaluate(fn, args)
  return result
end

function evalBuiltinAbs(arg::Expression)::Expression
  local result::Expression

  @assign result = begin
    @match arg begin
      INTEGER_EXPRESSION(__) => begin
        INTEGER_EXPRESSION(abs(arg.value))
      end

      REAL_EXPRESSION(__) => begin
        REAL_EXPRESSION(abs(arg.value))
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
      REAL_EXPRESSION(value = x) => begin
        if x < (-1.0) || x > 1.0
          if P_EvalTarget.hasInfo(target)
            Error.addSourceMessage(
              Error.ARGUMENT_OUT_OF_RANGE,
              list(String(x), "acos", "-1 <= x <= 1"),
              EvalTarget_getInfo(target),
            )
          end
          fail()
        end
        REAL_EXPRESSION(acos(x))
      end

      _ => begin
        printWrongArgsError(getInstanceName(), list(arg), sourceInfo())
        fail()
      end
    end
  end
  return result
end

function evalBuiltinArray(args::List{Expression})::Expression
  local result::Expression

  local ty::M_Type

  @assign ty = typeOf(listHead(args))
  @assign ty = liftArrayLeft(ty, P_Dimension.Dimension.fromInteger(listLength(args)))
  @assign result = makeArray(ty, args, literal = true)
  return result
end

function evalBuiltinAsin(arg::Expression, target::EvalTarget)::Expression
  local result::Expression

  local x::AbstractFloat

  @assign result = begin
    @match arg begin
      REAL_EXPRESSION(value = x) => begin
        if x < (-1.0) || x > 1.0
          if P_EvalTarget.hasInfo(target)
            Error.addSourceMessage(
              Error.ARGUMENT_OUT_OF_RANGE,
              list(String(x), "asin", "-1 <= x <= 1"),
              EvalTarget_getInfo(target),
            )
          end
          fail()
        end
        REAL_EXPRESSION(asin(x))
      end

      _ => begin
        printWrongArgsError(getInstanceName(), list(arg), sourceInfo())
        fail()
      end
    end
  end
  return result
end

function evalBuiltinAtan2(args::List{Expression})::Expression
  local result::Expression

  local y::AbstractFloat
  local x::AbstractFloat

  @assign result = begin
    @match args begin
      REAL_EXPRESSION(value = y) <|
      REAL_EXPRESSION(value = x) <| nil() => begin
        REAL_EXPRESSION(atan2(y, x))
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
      REAL_EXPRESSION(__) => begin
        REAL_EXPRESSION(atan(arg.value))
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
  args::List{Expression},
  target::EvalTarget,
)::Expression
  local result::Expression
  local n::Int
  local nd::Int
  local sz::Int
  local ty::M_Type
  local es::List{Expression}
  local dims::List{Int}
  @match INTEGER_EXPRESSION(n) = argN
  ty = typeOf(listHead(args))
  nd = dimensionCount(ty)
  if n > nd || n < 1
    if hasInfo(target)
      Error.addSourceMessage(
        Error.ARGUMENT_OUT_OF_RANGE,
        list(String(n), "cat", "1 <= x <= " + String(nd)),
        EvalTarget_getInfo(target),
      )
    end
    fail()
  end
  es = list(e for e in args if !isEmptyArray(e))
  sz = listLength(es)
  if sz == 0
    result = listHead(args)
  elseif sz == 1
    result = listHead(es)
  else
    (es, dims) = evalCat(
      n,
      es,
      arrayElements,
      toString,
    )
    result = arrayFromList(
      es,
      typeOf(listHead(es)),
      list(fromInteger(d) for d in dims),
    )
  end
  return result
end

function evalBuiltinCeil(arg::Expression)::Expression
  local result::Expression

  @assign result = begin
    @match arg begin
      REAL_EXPRESSION(__) => begin
        REAL_EXPRESSION(ceil(arg.value))
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
      REAL_EXPRESSION(__) => begin
        REAL_EXPRESSION(cosh(arg.value))
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
      REAL_EXPRESSION(__) => begin
        REAL_EXPRESSION(cos(arg.value))
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

  @assign result = fillType(
    typeOf(arg),
    REAL_EXPRESSION(0.0),
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
  local n::Int
  local i::Int = 1
  local e_lit::Bool
  local arg_lit::Bool = true

  @assign result = begin
    @match arg begin
      ARRAY_EXPRESSION(elements = nil()) => begin
        arg
      end

      ARRAY_EXPRESSION(elements = elems) => begin
        @assign n = listLength(elems)
        @assign elem_ty = typeOf(listHead(elems))
        @assign row_ty = liftArrayLeft(elem_ty, P_Dimension.Dimension.fromInteger(n))
        @assign zero = makeZero(elem_ty)
        for e in listReverse(elems)
          @assign row = nil
          for j = 2:i
            @assign row = _cons(zero, row)
          end
          @assign row = _cons(e, row)
          @assign e_lit = isLiteral(e)
          @assign arg_lit = arg_lit && e_lit
          for j = i:(n - 1)
            @assign row = _cons(zero, row)
          end
          @assign i = i + 1
          @assign rows =
            _cons(makeArray(row_ty, row, e_lit), rows)
        end
        makeArray(
          liftArrayLeft(row_ty, P_Dimension.Dimension.fromInteger(n)),
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

function evalBuiltinDiv(args::List{Expression}, target::EvalTarget)::Expression
  local result::Expression

  local rx::AbstractFloat
  local ry::AbstractFloat
  local ix::Int
  local iy::Int

  @assign result = begin
    @match args begin
      INTEGER_EXPRESSION(ix) <| INTEGER_EXPRESSION(iy) <| nil() => begin
        if iy == 0
          if P_EvalTarget.hasInfo(target)
            Error.addSourceMessage(
              Error.DIVISION_BY_ZERO,
              list(String(ix), String(iy)),
              EvalTarget_getInfo(target),
            )
          end
          fail()
        end
        INTEGER_EXPRESSION(intDiv(ix, iy))
      end

      REAL_EXPRESSION(rx) <| REAL_EXPRESSION(ry) <| nil() =>
        begin
          if ry == 0.0
            if P_EvalTarget.hasInfo(target)
              Error.addSourceMessage(
                Error.DIVISION_BY_ZERO,
                list(String(rx), String(ry)),
                EvalTarget_getInfo(target),
              )
            end
            fail()
          end
          @assign rx = rx / ry
          REAL_EXPRESSION(if rx < 0.0
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
      REAL_EXPRESSION(__) => begin
        REAL_EXPRESSION(exp(arg.value))
      end

      _ => begin
        printWrongArgsError(getInstanceName(), list(arg), sourceInfo())
        fail()
      end
    end
  end
  return result
end

function evalBuiltinFill(args::List{Expression})::Expression
  local result::Expression

  @assign result = evalBuiltinFill2(listHead(args), listRest(args))
  return result
end

function evalBuiltinFill2(fillValue::Expression, dims::List{Expression})::Expression
  local result::Expression = fillValue

  local dim_size::Int
  local arr::List{Expression}
  local arr_ty::M_Type = typeOf(result)

  for d in listReverse(dims)
    dim_size = begin
      @match d begin
        INTEGER_EXPRESSION(value = dim_size) => begin
          dim_size
        end
        _ => begin
          printWrongArgsError(getInstanceName(), list(d), sourceInfo())
          fail()
        end
      end
    end
    arr = list(result for e = 1:dim_size)
    arr_ty = liftArrayLeft(arr_ty, fromInteger(dim_size))
    result = makeArray(
      arr_ty,
      arr;
      literal = isLiteral(fillValue),
    )
  end
  return result
end

function evalBuiltinFloor(arg::Expression)::Expression
  local result::Expression

  @assign result = begin
    @match arg begin
      REAL_EXPRESSION(__) => begin
        REAL_EXPRESSION(floor(arg.value))
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
      INTEGER_EXPRESSION(__) => begin
        makeIdentityMatrix(arg.value, TYPE_INTEGER())
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
      INTEGER_EXPRESSION(__) => begin
        arg
      end

      REAL_EXPRESSION(__) => begin
        INTEGER_EXPRESSION(realInt(arg.value))
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
      ENUM_LITERAL_EXPRESSION(__) => begin
        INTEGER_EXPRESSION(arg.index)
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
      REAL_EXPRESSION(value = x) => begin
        if x <= 0.0
          if P_EvalTarget.hasInfo(target)
            Error.addSourceMessage(
              Error.ARGUMENT_OUT_OF_RANGE,
              list(String(x), "log10", "x > 0"),
              EvalTarget_getInfo(target),
            )
          end
          fail()
        end
        REAL_EXPRESSION(log10(x))
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
      REAL_EXPRESSION(value = x) => begin
        if x <= 0.0
          if P_EvalTarget.hasInfo(target)
            Error.addSourceMessage(
              Error.ARGUMENT_OUT_OF_RANGE,
              list(String(x), "log", "x > 0"),
              EvalTarget_getInfo(target),
            )
          end
          fail()
        end
        REAL_EXPRESSION(log(x))
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
    local dim_count::Int
    local expl::List{Expression}
    local dim1::Dimension
    local dim2::Dimension
    local ty::M_Type
    @match arg begin
      ARRAY_EXPRESSION(ty = ty) => begin
        @assign dim_count = Type.dimensionCount(ty)
        if dim_count < 2
          @assign result = promote(arg, ty, 2)
        elseif dim_count == 2
          @assign result = arg
        else
          @match _cons(dim1, _cons(dim2, _)) = arrayDims(ty)
          @assign ty = liftArrayLeft(arrayElementType(ty), dim2)
          @assign expl = list(evalBuiltinMatrix2(e, ty) for e in arg.elements)
          @assign ty = liftArrayLeft(ty, dim1)
          @assign result = makeArray(ty, expl)
        end
        result
      end

      _ => begin
        @assign ty = typeOf(arg)
        if Type.isScalar(ty)
          @assign result = promote(arg, ty, 2)
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
      ARRAY_EXPRESSION(__) => begin
        makeArray(
          ty,
          list(toScalar(e) for e in arg.elements),
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

function evalBuiltinMax(args::List{Expression}, fn::M_Function)::Expression
  local result::Expression
  local e1::Expression
  local e2::Expression
  local expl::List{Expression}
  local ty::M_Type
  result = begin
    @match args begin
      e1 <| e2 <| nil() => begin
        evalBuiltinMax2(e1, e2)
      end
      e1 <| nil() && ARRAY_EXPRESSION(ty = ty) <| nil() => begin
        result = fold(
          e1,
          evalBuiltinMax2,
          EMPTY_EXPRESSION(ty),
        )
        if isEmpty(result)
          result = CALL_EXPRESSION(makeTypedCall(
            fn,
            list(makeEmptyArray(ty)),
            Variability.CONSTANT,
            arrayElementType(ty),
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
      (INTEGER_EXPRESSION(__), INTEGER_EXPRESSION(__)) => begin
        if exp1.value < exp2.value
          exp2
        else
          exp1
        end
      end

      (REAL_EXPRESSION(__), REAL_EXPRESSION(__)) => begin
        if exp1.value < exp2.value
          exp2
        else
          exp1
        end
      end

      (BOOLEAN_EXPRESSION(__), BOOLEAN_EXPRESSION(__)) => begin
        if exp1.value < exp2.value
          exp2
        else
          exp1
        end
      end

      (
        ENUM_LITERAL_EXPRESSION(__),
        ENUM_LITERAL_EXPRESSION(__),
      ) => begin
        if exp1.index < exp2.index
          exp2
        else
          exp1
        end
      end

      (ARRAY_EXPRESSION(__), _) => begin
        exp2
      end

      (_, EMPTY_EXPRESSION(__)) => begin
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

function evalBuiltinMin(args::List{Expression}, fn::M_Function)::Expression
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

      e1 && ARRAY_EXPRESSION(ty = ty) <| nil() => begin
        @assign result = fold(
          e1,
          evalBuiltinMin2,
          EMPTY(ty),
        )
        if isEmpty(result)
          @assign result = CALL_EXPRESSION(P_Call.makeTypedCall(
            fn,
            list(makeEmptyArray(ty)),
            Variability.CONSTANT,
            arrayElementType(ty),
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
      (INTEGER_EXPRESSION(__), INTEGER_EXPRESSION(__)) => begin
        if exp1.value > exp2.value
          exp2
        else
          exp1
        end
      end

      (REAL_EXPRESSION(__), REAL_EXPRESSION(__)) => begin
        if exp1.value > exp2.value
          exp2
        else
          exp1
        end
      end

      (BOOLEAN_EXPRESSION(__), BOOLEAN_EXPRESSION(__)) => begin
        if exp1.value > exp2.value
          exp2
        else
          exp1
        end
      end

      (
        ENUM_LITERAL_EXPRESSION(__),
        ENUM_LITERAL_EXPRESSION(__),
      ) => begin
        if exp1.index > exp2.index
          exp2
        else
          exp1
        end
      end

      (ARRAY_EXPRESSION(__), _) => begin
        exp2
      end

      (_, EMPTY(__)) => begin
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

function evalBuiltinMod(args::List{Expression}, target::EvalTarget)::Expression
  local result::Expression
  local x::Expression
  local y::Expression
  @match x <| y <| nil = args
  result = begin
    @match (x, y) begin
      (INTEGER_EXPRESSION(__), INTEGER_EXPRESSION(__)) => begin
        if y.value == 0
          if hasInfo(target)
            Error.addSourceMessage(
              Error.MODULO_BY_ZERO,
              list(String(x.value), String(y.value)),
              EvalTarget_getInfo(target),
            )
          end
          fail()
        end
        INTEGER_EXPRESSION(mod(x.value, y.value))
      end

      (REAL_EXPRESSION(__), REAL_EXPRESSION(__)) => begin
        if y.value == 0.0
          if hasInfo(target)
            Error.addSourceMessage(
              Error.MODULO_BY_ZERO,
              list(String(x.value), String(y.value)),
              EvalTarget_getInfo(target),
            )
          end
          fail()
        end
        REAL_EXPRESSION(mod(x.value, y.value))
      end

      _ => begin
        printWrongArgsError(getInstanceName(), args, sourceInfo())
        fail()
      end
    end
  end
  return result
end

function evalBuiltinOnes(args::List{Expression})::Expression
  local result::Expression

  @assign result = evalBuiltinFill2(INTEGER_EXPRESSION(1), args)
  return result
end

function evalBuiltinProduct(arg::Expression)::Expression
  local result::Expression

  @assign result = begin
    @match arg begin
      ARRAY_EXPRESSION(__) => begin
        begin
          @match arrayElementType(typeOf(arg)) begin
            TYPE_INTEGER(__) => begin
              INTEGER_EXPRESSION(fold(
                arg,
                evalBuiltinProductInt,
                1,
              ))
            end

            TYPE_REAL(__) => begin
              REAL_EXPRESSION(fold(
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

function evalBuiltinProductInt(exp::Expression, result::Int)::Int

  @assign result = begin
    @match exp begin
      INTEGER_EXPRESSION(__) => begin
        result * exp.value
      end

      ARRAY_EXPRESSION(__) => begin
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
      REAL_EXPRESSION(__) => begin
        result * exp.value
      end

      ARRAY_EXPRESSION(__) => begin
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
  local n::Int
  if isInteger(argN)
    @match INTEGER_EXPRESSION(n) = argN
    (result, _) = promote(arg, typeOf(arg), n)
  else
    printWrongArgsError(getInstanceName(), list(arg, argN), sourceInfo())
    fail()
  end
  return result
end

function evalBuiltinRem(args::List{Expression}, target::EvalTarget)::Expression
  local result::Expression

  local x::Expression
  local y::Expression

  @match list(x, y) = args
  @assign result = begin
    @match (x, y) begin
      (INTEGER_EXPRESSION(__), INTEGER_EXPRESSION(__)) => begin
        if y.value == 0
          if P_EvalTarget.hasInfo(target)
            Error.addSourceMessage(
              Error.REM_ARG_ZERO,
              list(String(x.value), String(y.value)),
              EvalTarget_getInfo(target),
            )
          end
          fail()
        end
        INTEGER_EXPRESSION(x.value - div(x.value, y.value) * y.value)
      end

      (REAL_EXPRESSION(__), REAL_EXPRESSION(__)) => begin
        if y.value == 0.0
          if P_EvalTarget.hasInfo(target)
            Error.addSourceMessage(
              Error.REM_ARG_ZERO,
              list(String(x.value), String(y.value)),
              EvalTarget_getInfo(target),
            )
          end
          fail()
        end
        REAL_EXPRESSION(x.value - div(x.value, y.value) * y.value)
      end

      _ => begin
        printWrongArgsError(getInstanceName(), args, sourceInfo())
        fail()
      end
    end
  end
  return result
end

function evalBuiltinScalar(args::List{Expression})::Expression
  local result::Expression
  local exp::Expression = listHead(args)
  result = begin
    @match exp begin
      ARRAY_EXPRESSION(__) => begin
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
  result = begin
    @match arg begin
      REAL_EXPRESSION(__) => begin
        INTEGER_EXPRESSION(if arg.value > 0
          1
        else
          if arg.value < 0
            -1
          else
            0
          end
        end)
      end

      INTEGER_EXPRESSION(__) => begin
        INTEGER_EXPRESSION(if arg.value > 0
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
      REAL_EXPRESSION(__) => begin
        REAL_EXPRESSION(sinh(arg.value))
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
      REAL_EXPRESSION(__) => begin
        REAL_EXPRESSION(sin(arg.value))
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
      ARRAY_EXPRESSION(
        ty = ty,
        elements = x1 <| x2 <| x3 <| nil(),
        literal = literal,
      ) => begin
        @assign zero = makeZero(arrayElementType(ty))
        @assign y1 = makeArray(
          ty,
          list(zero, negate(x3), x2),
          literal,
        )
        @assign y2 = makeArray(
          ty,
          list(x3, zero, negate(x1)),
          literal,
        )
        @assign y3 = makeArray(
          ty,
          list(negate(x2), x1, zero),
          literal,
        )
        @assign ty = liftArrayLeft(ty, P_Dimension.Dimension.fromInteger(3))
        makeArray(ty, list(y1, y2, y3), literal)
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
      REAL_EXPRESSION(__) => begin
        REAL_EXPRESSION(sqrt(arg.value))
      end

      _ => begin
        printWrongArgsError(getInstanceName(), list(arg), sourceInfo())
        fail()
      end
    end
  end
  return result
end

function evalBuiltinString(args::List{Expression})::Expression
  local result::Expression

  @assign result = begin
    local arg::Expression
    local min_len::Int
    local str_len::Int
    local significant_digits::Int
    local idx::Int
    local c::Int
    local left_justified::Bool
    local str::String
    local format::String
    local r::AbstractFloat
    @match args begin
      arg <|
      INTEGER_EXPRESSION(min_len) <|
      BOOLEAN_EXPRESSION(left_justified) <| nil() => begin
        @assign str = begin
          @match arg begin
            INTEGER_EXPRESSION(__) => begin
              intString(arg.value)
            end

            BOOLEAN_EXPRESSION(__) => begin
              boolString(arg.value)
            end

            ENUM_LITERAL_EXPRESSION(__) => begin
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
        STRING_EXPRESSION(str)
      end

      REAL_EXPRESSION(r) <|
      INTEGER_EXPRESSION(significant_digits) <|
      INTEGER_EXPRESSION(min_len) <|
      BOOLEAN_EXPRESSION(left_justified) <| nil() => begin
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
        STRING_EXPRESSION(str)
      end

      REAL_EXPRESSION(r) <| STRING_EXPRESSION(format) <| nil() => begin
        @assign str = System.sprintff(format, r)
        STRING_EXPRESSION(str)
      end
    end
  end
  return result
end

function evalBuiltinSum(arg::Expression)::Expression
  local result::Expression

  @assign result = begin
    @match arg begin
      ARRAY_EXPRESSION(__) => begin
        begin
          @match arrayElementType(typeOf(arg)) begin
            TYPE_INTEGER(__) => begin
              INTEGER_EXPRESSION(fold(
                arg,
                evalBuiltinSumInt,
                0,
              ))
            end

            TYPE_REAL(__) => begin
              REAL_EXPRESSION(fold(
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

function evalBuiltinSumInt(exp::Expression, result::Int)::Int

  @assign result = begin
    @match exp begin
      INTEGER_EXPRESSION(__) => begin
        result + exp.value
      end

      ARRAY_EXPRESSION(__) => begin
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
      REAL_EXPRESSION(__) => begin
        result + exp.value
      end

      ARRAY_EXPRESSION(__) => begin
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

  local mat::Vector{Array{Expression}}
  local n::Int
  local row_ty::M_Type
  local expl::List{Expression}
  local accum::List{Expression} = nil

  @assign result = begin
    @match arg begin
      ARRAY_EXPRESSION(__) where {(Type.isMatrix(arg.ty))} => begin
        @assign mat = listArray(List(
          listArray(arrayElements(row))
          for row in arrayElements(arg)
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
            makeArray(row_ty, expl, literal = true),
            accum,
          )
        end
        makeArray(arg.ty, accum, literal = true)
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
      REAL_EXPRESSION(__) => begin
        REAL_EXPRESSION(tanh(arg.value))
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
      REAL_EXPRESSION(__) => begin
        REAL_EXPRESSION(tan(arg.value))
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
  result = begin
    @match arg begin
      ARRAY_EXPRESSION(
        ty = TYPE_ARRAY(elementType = ty, dimensions = dim1 <| dim2 <| rest_dims),
        elements = arr,
        literal = literal,
      ) => begin
        arrl = list(arrayElements(e) for e in arr)
        arrl = ListUtil.transposeList(arrl)
        ty = liftArrayLeft(ty, dim1)
        arr =
          list(makeArray(ty, expl, literal = literal) for expl in arrl)
        ty = liftArrayLeft(ty, dim2)
        makeArray(ty, arr, literal = literal)
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

  @assign expl = fold(arg, evalBuiltinVector2, nil)
  @assign ty = liftArrayLeft(
    arrayElementType(typeOf(arg)),
    P_Dimension.Dimension.fromInteger(listLength(expl)),
  )
  @assign result = makeArray(ty, listReverse(expl), literal = true)
  return result
end

function evalBuiltinVector2(exp::Expression, expl::List{Expression})::List{Expression}

  @assign expl = begin
    @match exp begin
      ARRAY_EXPRESSION(__) => begin
        expl
      end

      _ => begin
        _cons(exp, expl)
      end
    end
  end
  return expl
end

function evalBuiltinZeros(args::List{Expression})::Expression
  local result::Expression

  @assign result = evalBuiltinFill2(INTEGER_EXPRESSION(0), args)
  return result
end

function evalUriToFilename(
  fn::M_Function,
  args::List{Expression},
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
      STRING_EXPRESSION(__) => begin
        @assign s = OpenModelica.Scripting.uriToFilename(arg.value)
        @assign e = STRING_EXPRESSION(s)
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

function evalIntBitAnd(args::List{Expression})::Expression
  local result::Expression

  local i1::Int
  local i2::Int

  @assign result = begin
    @match args begin
      INTEGER_EXPRESSION(value = i1) <|
      INTEGER_EXPRESSION(value = i2) <| nil() => begin
        INTEGER_EXPRESSION(intBitAnd(i1, i2))
      end

      _ => begin
        printWrongArgsError(getInstanceName(), args, sourceInfo())
        fail()
      end
    end
  end
  return result
end

function evalIntBitOr(args::List{Expression})::Expression
  local result::Expression

  local i1::Int
  local i2::Int

  @assign result = begin
    @match args begin
      INTEGER_EXPRESSION(value = i1) <|
      INTEGER_EXPRESSION(value = i2) <| nil() => begin
        INTEGER_EXPRESSION(intBitOr(i1, i2))
      end

      _ => begin
        printWrongArgsError(getInstanceName(), args, sourceInfo())
        fail()
      end
    end
  end
  return result
end

function evalIntBitXor(args::List{Expression})::Expression
  local result::Expression

  local i1::Int
  local i2::Int

  @assign result = begin
    @match args begin
      INTEGER_EXPRESSION(value = i1) <|
      INTEGER_EXPRESSION(value = i2) <| nil() => begin
        INTEGER_EXPRESSION(intBitXor(i1, i2))
      end

      _ => begin
        printWrongArgsError(getInstanceName(), args, sourceInfo())
        fail()
      end
    end
  end
  return result
end

function evalIntBitLShift(args::List{Expression})::Expression
  local result::Expression

  local i1::Int
  local i2::Int

  @assign result = begin
    @match args begin
      INTEGER_EXPRESSION(value = i1) <|
      INTEGER_EXPRESSION(value = i2) <| nil() => begin
        INTEGER_EXPRESSION(intBitLShift(i1, i2))
      end

      _ => begin
        printWrongArgsError(getInstanceName(), args, sourceInfo())
        fail()
      end
    end
  end
  return result
end

function evalIntBitRShift(args::List{Expression})::Expression
  local result::Expression

  local i1::Int
  local i2::Int

  @assign result = begin
    @match args begin
      INTEGER_EXPRESSION(value = i1) <|
      INTEGER_EXPRESSION(value = i2) <| nil() => begin
        INTEGER_EXPRESSION(intBitRShift(i1, i2))
      end

      _ => begin
        printWrongArgsError(getInstanceName(), args, sourceInfo())
        fail()
      end
    end
  end
  return result
end

function evalInferredClock(args::List{Expression})::Expression
  local result::Expression

  @assign result = begin
    @match args begin
      nil() => begin
        CLKCONST(P_Expression.P_ClockKind.Expression.INFERRED_CLOCK())
      end

      _ => begin
        printWrongArgsError(getInstanceName(), args, sourceInfo())
        fail()
      end
    end
  end
  return result
end

function evalRationalClock(args::List{Expression})::Expression
  local result::Expression

  @assign result = begin
    local interval::Expression
    local resolution::Expression
    @match args begin
      interval &&
      INTEGER_EXPRESSION(__) <| resolution &&
      INTEGER_EXPRESSION(__) <| nil() => begin
        CLKCONST(P_Expression.P_ClockKind.Expression.INTEGER_CLOCK(
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

function evalRealClock(args::List{Expression})::Expression
  local result::Expression

  @assign result = begin
    local interval::Expression
    @match args begin
      interval && REAL_EXPRESSION(__) <| nil() => begin
        CLKCONST(P_Expression.P_ClockKind.REAL_EXPRESSION_CLOCK(
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

function evalBooleanClock(args::List{Expression})::Expression
  local result::Expression

  @assign result = begin
    local condition::Expression
    local interval::Expression
    @match args begin
      condition &&
      BOOLEAN_EXPRESSION(__) <| interval &&
      REAL_EXPRESSION(__) <| nil() => begin
        CLKCONST(P_Expression.P_ClockKind.BOOLEAN_EXPRESSION_CLOCK(
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

function evalSolverClock(args::List{Expression})::Expression
  local result::Expression

  @assign result = begin
    local c::Expression
    local solver::Expression
    @match args begin
      c &&
      CLKCONST(__) <| solver &&
      STRING_EXPRESSION(__) <| nil() => begin
        CLKCONST(P_Expression.P_ClockKind.Expression.SOLVER_CLOCK(
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
  args::List{Expression},
  target::EvalTarget,
)::Expression
  local result::Expression
  local s::Expression
  local d::Expression
  @match list(s, d) = list(unbox(arg) for arg in args)
  s = evalExp(s, target)
  result = s
  return result
end

function evalArrayConstructor(
  exp::Expression,
  iterators::List{<:Tuple{<:InstNode, Expression}},
)::Expression
  local result::Expression
  (result, ) = evalExpPartial(exp)
  result = bindingExpMap(
    result,
    (expArg) -> evalArrayConstructor2(expArg, iterators),
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
  local iters::List{Pointer{Expression}}
  local types::List{M_Type} = nil
  local ty::M_Type

  @assign (e, ranges, iters) = createIterationRanges(exp, iterators)
  #=  Precompute all the types we're going to need for the arrays created.
  =#
  @assign ty = typeOf(e)
  for r in ranges
    @assign ty =
      liftArrayLeftList(ty, arrayDims(typeOf(r)))
    @assign types = _cons(ty, types)
  end
  @assign result = evalArrayConstructor3(e, ranges, iters, types)
  return result
end

function createIterationRanges(
  exp::Expression,
  iterators::List{<:Tuple{<:InstNode, Expression}},
)::Tuple{Expression, List{Expression}, List{Pointer{Expression}}}
  local iters::List{Pointer{Expression}} = nil
  local ranges::List{Expression} = nil

  local node::InstNode
  local range::Expression
  local iter::Pointer{Expression}

  for i in iterators
    @assign (node, range) = i
    @assign iter = P_Pointer.create(INTEGER_EXPRESSION(0))
    @assign exp = replaceIterator(
      exp,
      node,
      MUTABLE_EXPRESSION(iter),
    )
    @assign iters = _cons(iter, iters)
    @assign ranges = _cons(evalExp_impl(range, EVALTARGET_IGNORE_ERRORS()), ranges)
  end
  return (exp, ranges, iters)
end

function evalArrayConstructor3(
  exp::Expression,
  ranges::List{Expression},
  iterators::List{<:Pointer{Expression}},
  types::List{<:M_Type},
)::Expression
  local result::Expression
  local range::Expression
  local e::Expression
  local ranges_rest::List{Expression}
  local expl::List{Expression} = nil
  local iter::Pointer{Expression}
  local iters_rest::List{Pointer{Expression}}
  local range_iter::ExpressionIterator
  local value::Expression
  local ty::M_Type
  local rest_ty::List{M_Type}
  if listEmpty(ranges)
    result = evalExp_impl(exp, EVALTARGET_IGNORE_ERRORS())
  else
    @match _cons(range, ranges_rest) = ranges
    @match _cons(iter, iters_rest) = iterators
    @match _cons(ty, rest_ty) = types
    range_iter = fromExpToExpressionIterator(range)
    while hasNext(range_iter)
      (range_iter, value) = next(range_iter)
      P_Pointer.update(iter, value)
      expl =
        _cons(evalArrayConstructor3(exp, ranges_rest, iters_rest, rest_ty), expl)
    end
    result =
      makeArray(ty, listReverseInPlace(expl), literal = true)
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
  @assign result = bindingExpMap(
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
  local iters::List{Pointer{Expression}}
  local red_fn::Function
  local ty::M_Type

  @assign (e, ranges, iters) = createIterationRanges(exp, iterators)
  @assign ty = typeOf(e)
  @assign (red_fn, default_exp) = begin
    @match AbsynUtil.pathString(P_Function.name(fn)) begin
      "sum" => begin
        (evalBinaryAdd, makeZero(ty))
      end

      "product" => begin
        (evalBinaryMul, makeOne(ty))
      end

      "min" => begin
        (evalBuiltinMin2, makeMaxValue(ty))
      end

      "max" => begin
        (evalBuiltinMax2, makeMinValue(ty))
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
  ranges::List{Expression},
  iterators::List{<:Pointer{Expression}},
  foldExp::Expression,
  fn::Function,
)::Expression
  local result::Expression

  local range::Expression
  local ranges_rest::List{Expression}
  local expl::List{Expression} = nil
  local iter::Pointer{Expression}
  local iters_rest::List{Pointer{Expression}}
  local range_iter::ExpressionIterator
  local value::Expression
  local el_ty::M_Type

  if listEmpty(ranges)
    @assign result = fn(foldExp, evalExp_impl(exp, EVALTARGET_IGNORE_ERRORS()))
  else
    @match _cons(range, ranges_rest) = ranges
    @match _cons(iter, iters_rest) = iterators
    @assign range_iter = P_ExpressionIterator.ExpressionIterator.fromExp(range)
    @assign result = foldExp
    while P_ExpressionIterator.ExpressionIterator.hasNext(range_iter)
      @assign (range_iter, value) = P_ExpressionIterator.ExpressionIterator.next(range_iter)
      P_Pointer.update(iter, value)
      @assign result = evalReduction3(exp, ranges_rest, iters_rest, result, fn)
    end
  end
  return result
end

function evalSize(
  exp::Expression,
  optIndex::Option{Expression},
  target::EvalTarget,
)::Expression
  local outExp::Expression
  local index_exp::Expression
  local index::Int
  local ty_err::TypingError
  local dim::Dimension
  local ty::M_Type
  local expl::List{Expression}
  local info::SourceInfo
  info = EvalTarget_getInfo(target)
  if isSome(optIndex)
    index_exp = evalExp_impl(Util.getOption(optIndex), target)
    index = toInteger(index_exp)
    (dim, _, ty_err) = typeExpDim(exp, index, ORIGIN_CLASS, info)
    checkSizeTypingError(ty_err, exp, index, info)
    outExp = sizeExp(dim)
  else
    (outExp, ty) = typeExp(exp, ORIGIN_CLASS, info)
    expl = list(sizeExp(d) for d in arrayDims(ty))
    dim = fromInteger(listLength(expl), Variability.PARAMETER)
    outExp =
      makeArray(TYPE_ARRAY(TYPE_INTEGER(), list(dim)), expl)
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
      RANGE_EXPRESSION(__) => begin
        RANGE_EXPRESSION(
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
  @assign subs = list(
    mapShallowExp(s, (x) -> evalExp_impl(x, target))
    for s in subscripts
  )
  result = applySubscripts(subs, result)
  return result
end

function evalRecordElement(exp::Expression, target::EvalTarget)::Expression
  local result::Expression
  local e::Expression
  local index::Int
  @match RECORD_ELEMENT(recordExp = e, index = index) = exp
  e = evalExp_impl(e, target)
  try
    result =
      bindingExpMap(e, (x) -> evalRecordElement2(x, index))
  catch
    Error.assertion(
      false,
      getInstanceName() + " could not evaluate " + toString(exp),
      sourceInfo(),
    )
  end
  return result
end

function evalRecordElement2(exp::Expression, index::Int)::Expression
  local result::Expression

  @assign result = begin
    @match exp begin
RECORD_EXPRESSION(__) => begin
        listGet(exp.elements, index)
      end
    end
  end
  return result
end

function printUnboundError(component::Component, target::EvalTarget, exp::Expression)
  return @assign () = begin
    @match target begin
      EVALTARGET_IGNORE_ERRORS(__) => begin
        ()
      end

      P_EvalTarget.DIMENSION(__) => begin
        Error.addSourceMessage(
          Error.STRUCTURAL_PARAMETER_OR_CONSTANT_WITH_NO_BINDING,
          list(toString(exp), name(target.component)),
          target.info,
        )
        fail()
      end

      P_EvalTarget.CONDITION(__) => begin
        Error.addSourceMessage(
          Error.CONDITIONAL_EXP_WITHOUT_VALUE,
          list(toString(exp)),
          target.info,
        )
        fail()
      end

      _ => begin
        #=  check if we have a parameter with (fixed = true), annotation(Evaluate = true) and no binding
        =#
        if listMember(
          variability(component),
          list(Variability.STRUCTURAL_PARAMETER, Variability.PARAMETER),
        ) && P_Component.getEvaluateAnnotation(component)
          if getFixedAttribute(component)
            Error.addMultiSourceMessage(
              Error.UNBOUND_PARAMETER_EVALUATE_TRUE,
              list(toString(exp) + "(fixed = true)"),
              list(
                InstNode_info(node(toCref(
                  exp,
                ))),
                EvalTarget_getInfo(target),
              ),
            )
          end
        else
          Error.addMultiSourceMessage(
            Error.UNBOUND_CONSTANT,
            list(toString(exp)),
            list(
              InstNode_info(node(toCref(
                exp,
              ))),
              EvalTarget_getInfo(target),
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

function printWrongArgsError(evalFunc::String, args::List{Expression}, info::SourceInfo)
  return Error.addInternalError(
    evalFunc +
    " got invalid arguments " +
    ListUtil.toString(args, toString, "", "(", ", ", ")", true),
    info,
  )
end

"""
  @author:johti17
  input: The set of initial equations
  output: A mapping between the component references of the variables and the values of the initial equations
(Where the lhs of the equation system is a variable)
"""
function evalInitialEqMapping(ieq::List{Equation})
  local mapping::Dict = Dict()
  for eq in ieq
    var = Variable_fromCref(toCref(eq.lhs))
    push!(mapping, toString(var.name) => eq.rhs)
  end
  return mapping
end

"""
  Custom reimplementation of evalCat
  @author johti17
"""
function evalCat(dim::Int, exps::List{Expression}, getArrayContents::Function, toString::Function)::Tuple{List{Expression}, List{Int}}
  local arr::List{Expression}
  local arrs::List{List{Expression}} = nil
  local dims::List{Int} = nil
  local lastDims::List{Int} = nil
  local firstDims::List{Int} = nil
  local reverseDims::List{Int} = nil
  local dimsLst::List{List{Int}} = nil;
  local j::Int, k::Int, l::Int, thisDim::Int, lastDim::Int
  #= Outputs =#
  local outExps::List{Expression} = nil
  local outDims::List{Int} = nil
  @assert dim >= 1 "Invalid dimension for" * toString(exps)
  @assert false == listEmpty(exps) "Internal error: Empty dimension passed to evalCat"
  if 1 == dim
    #outExps = listAppend(getArrayContents(e) for e in listReverse(exps))#TODO investigate this
    #outExps = arrayList(Base.collect(Iterators.flatten(getArrayContents(e) for e in listReverse(exps))))
    for e in listReverse(exps)
      arrConts = getArrayContents(e)
      outExps = listHead(arrConts) <| outExps
    end
    outDims = list(listLength(outExps));
    return (outExps, outDims)
  end
  for e in listReverse(exps)
    (arr, dims) = evalCatGetFlatArray(e, dim, getArrayContents, toString)
    arrs = arr <| arrs
    dimsLst = dims <| dimsLst
  end
  for i in 1:(dim - 1)
    j = minimum(listHead(d) for d in dimsLst);
    if j != maximum(listHead(d) for d in dimsLst)
      @error "Uneven dimension error for" * toString(exps)
      fail()
    end
    firstDims = j <| firstDims
    dimsLst = list(listRest(d) for d in dimsLst)
  end
    reverseDims = firstDims
    firstDims = listReverse(firstDims)
    lastDims = list(listHead(d) for d in dimsLst)
    lastDim = sum(d for d in lastDims)
    reverseDims = lastDim <| reverseDims
  # Fill in the elements of the new array in the new order; this uses
  # an array structure for random access
  local arrSiz = lastDim*Base.reduce(*, list(d for d in firstDims))
  arrInitVal = listHead(listHead(arrs))
  local expArr::Vector{Expression} = arrayCreate(arrSiz, arrInitVal)
  k = 1
  for exps in arrs
    thisDim = listHead(lastDims)
    lastDims = listRest(lastDims)
    l = 0
    for e in exps
      arrayUpdate(expArr, k+mod(l, thisDim)+(lastDim * div(l, thisDim)), e)
      l = l+1
    end
    k = k + thisDim
  end
  #= Convert the flat array structure to a tree array structure with the correct dimension =#
  outExps = arrayList(expArr)
  outDims = listReverse(reverseDims)
  return (outExps, outDims)
end

"""
Custom reimplementation of evalCatGetFlatArray
@author johti17
"""
function evalCatGetFlatArray(e::Expression, dim::Int, getArrayContents::Function, toString::Function)::Tuple{List{Expression}, List{Int}}
  local arr::List
  local dims::List
  local i::Int
  #= output =#
  local outDims::List{Int} = nil
  local outExps::List{Expression} = nil
  if dim == 1
    outExps = getArrayContents(e)
    outDims = list(listLength(outExps))
    return (outExps, outDims)
  end
  i = 0
  for exp in listReverse(getArrayContents(e))
    (arr, dims) = evalCatGetFlatArray(exp, dim - 1, getArrayContents::Function, toString::Function)
    if listEmpty(outDims)
      outDims = dims
    elseif !(valueEq(dims, outDims))
      @error "Got unbalanced array from" * toString(e)
    else
      continue
    end
    outExps = listAppend(arr, outExps)
    i += 1
  end
  outDims = i <| outDims
  return(outExps, outDims)
end
