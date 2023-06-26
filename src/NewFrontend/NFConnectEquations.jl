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

const potFunc = Function
const EQ_ASSERT_STR = STRING_EXPRESSION("Connected constants/parameters must be equal")

function generateEquations(sets::Vector{<:List{<:Connector}})
  local equations::Vector{Equation} = Equation[]
  local set_eql::List{Equation}
  local potfunc::potFunc
  local flowThreshold::Expression
  local cty::ConnectorType.TYPE
  setGlobalRoot(Global.isInStream, NONE())
  #= potfunc := if Config.orderConnections() then
  =#
  #=   generatePotentialEquationsOrdered else generatePotentialEquations;
  =#
  potfunc = generatePotentialEquations
  #@assign flowThreshold = REAL_EXPRESSION(Flags.getConfigReal(Flags.FLOW_THRESHOLD))
  flowThreshold = REAL_EXPRESSION(1e-7) #=TODO Should be like this.. I think - John. Fix flag memory issue=#
  for set in sets
    cty = getSetType(set)
    if isPotential(cty)
      set_eql = potfunc(set)
    elseif isFlow(cty)
      set_eql = generateFlowEquations(set)
    elseif isStream(cty)
      set_eql = generateStreamEquations(set, flowThreshold)
    else
      Error.addInternalError(
        getInstanceName() +
        " got connection set with invalid type '" +
        ConnectorType.toDebugString(cty) +
        "': " +
        ListUtil.toString(set, Connector.toString, "", "{", ", ", "}", true),
        sourceInfo(),
      )
      fail()
    end
    equations = vcat(listArray(set_eql), equations)
  end
  return equations
end

const CardinalityTable = NFCardinalityTable
function evaluateOperators(
  @nospecialize(exp::Expression),
  @nospecialize(sets::ConnectionSets.Sets),
  setsArray::Vector{<:List{<:Connector}},
  ctable::CardinalityTable.Table,
)::Expression
  local evalExp::Expression
  evalExp = begin
    local call::Call
    local expanded::Bool
    @match exp begin
      CALL_EXPRESSION(call = call) => begin
        begin
          @match call begin
            TYPED_CALL(__) => begin
              begin
                @match name(call.fn) begin
                  Absyn.IDENT("inStream") => begin
                    evaluateInStream(
                      toCref(listHead(call.arguments)),
                      sets,
                      setsArray,
                      ctable,
                    )
                  end

                  Absyn.IDENT("actualStream") => begin
                    evaluateActualStream(
                      toCref(listHead(call.arguments)),
                      sets,
                      setsArray,
                      ctable,
                    )
                  end

                  Absyn.IDENT("cardinality") => begin
                    CardinalityTable.evaluateCardinality(listHead(call.arguments), ctable)
                  end

                  _ => begin
                    mapShallow(
                      exp,
                      (expArg) -> evaluateOperators(
                        expArg,
                        sets,
                        setsArray,
                        ctable,
                      ),
                    )
                  end
                end
              end
            end

            TYPED_REDUCTION(
              __,
            ) where {(contains(call.exp, isStreamCall))} =>
              begin
                evaluateOperatorReductionExp(exp, sets, setsArray, ctable)
              end

            TYPED_ARRAY_CONSTRUCTOR(
              __,
            ) where {(contains(call.exp, isStreamCall))} =>
              begin
                evaluateOperatorArrayConstructorExp(exp, sets, setsArray, ctable)
              end

            _ => begin
              mapShallow(
                exp,
                (expArg) -> evaluateOperators(
                  expArg,
                  sets,
                  setsArray,
                  ctable,
                ),
              )
            end
          end
        end
      end

      BINARY_EXPRESSION(
        exp1 = CREF_EXPRESSION(__),
        operator = OPERATOR(op = Op.MUL),
        exp2 = CALL_EXPRESSION(call = call && TYPED_CALL(__)),
      ) where {(AbsynUtil.isNamedPathIdent(name(call.fn), "actualStream"))} =>
        begin
          evaluateActualStreamMul(
            exp.exp1,
            listHead(call.arguments),
            exp.operator,
            sets,
            setsArray,
            ctable,
          )
        end

      BINARY_EXPRESSION(
        exp1 = CALL_EXPRESSION(call = call && TYPED_CALL(__)),
        operator = OPERATOR(op = Op.MUL),
        exp2 = CREF_EXPRESSION(__),
      ) where {(AbsynUtil.isNamedPathIdent(name(call.fn), "actualStream"))} =>
        begin
          evaluateActualStreamMul(
            exp.exp2,
            listHead(call.arguments),
            exp.operator,
            sets,
            setsArray,
            ctable,
          )
        end
      _ => begin
        mapShallow(
          exp,
          (expArg) ->
            evaluateOperators(expArg, sets, setsArray, ctable),
        )
      end
    end
  end
  #=  inStream/actualStream can't handle non-literal subscripts, so reductions and array =#
  #=  constructors containing such calls needs to be expanded to get rid of the iterators.=#
  return evalExp
end

function getSetType(set::List{<:Connector})::ConnectorType.TYPE
  local cty::ConnectorType.TYPE
  #=  All connectors in a set should have the same type, so pick the first.=#
  @match _cons(CONNECTOR(cty = cty), _) = set
  return cty
end

""" #= Generating the equations for a set of potential variables means equating all
   the components. For n components, this will give n-1 equations. For example,
   if the set contains the components X, Y.A and Z.B, the equations generated
   will be X = Y.A and X = Z.B. =#"""
function generatePotentialEquations(elements::List{<:Connector})::List{Equation}
  local equations::List{Equation}

  local c1::Connector

  @assign c1 = listHead(elements)
  if variability(c1) > Variability.PARAMETER
    @assign equations = list(
      makeEqualityEquation(c1.name, c1.source, c2.name, c2.source)
      for c2 in listRest(elements)
    )
  else
    @assign equations = list(
      makeEqualityAssert(c1.name, c1.source, c2.name, c2.source)
      for c2 in listRest(elements)
    )
  end
  return equations
end

#= function generatePotentialEquationsOrdered
=#
#=   \"Like generatePotentialEquations, but orders the connectors with
=#
#=    shouldFlipPotentialEquation.\"
=#
#=   input list<Connector> elements;
=#
#=   output list<Equation> equations = {};
=#
#= protected
=#
#=   partial function eqFunc
=#
#=     input ComponentRef lhsCref;
=#
#=     input DAE.ElementSource lhsSource;
=#
#=     input ComponentRef rhsCref;
=#
#=     input DAE.ElementSource rhsSource;
=#
#=     output Equation eq;
=#
#=   end eqFunc;
=#
#=
=#
#=   Connector c1;
=#
#=   ComponentRef cr1, cr2;
=#
#=   DAE.ElementSource source;
=#
#=   eqFunc eqfunc;
=#
#= algorithm
=#
#=   if listEmpty(elements) then
=#
#=     return;
=#
#=   end if;
=#
#=
=#
#=   c1 := listHead(elements);
=#
#=   eqfunc := if Connector.variability(c1) > Variability.PARAMETER then
=#
#=     makeEqualityEquation else makeEqualityAssert;
=#
#=
=#
#=   cr1 := c1.name;
=#
#=
=#
#=   for c2 in listRest(elements) loop
=#
#=     cr2 := c2.name;
=#
#=     (cr1, cr2) := Util.swap(shouldFlipPotentialEquation(cr1, c1.source), cr1, cr2);
=#
#=     equations := eqfunc(cr1, c2.source, cr2, c2.source) :: equations;
=#
#=     c1 := c2;
=#
#=     cr1 := cr2;
=#
#=   end for;
=#
#= end generatePotentialEquationsOrdered;
=#

function makeEqualityEquation(
  lhsCref::ComponentRef,
  lhsSource::DAE.ElementSource,
  rhsCref::ComponentRef,
  rhsSource::DAE.ElementSource,
)::Equation
  local equalityEq::Equation

  local source::DAE.ElementSource

  @assign source = DAE.emptyElementSource #ElementSource.mergeSources(lhsSource, rhsSource) TODO
  #= source := ElementSource.addElementSourceConnect(source, (lhsCref, rhsCref));
  =#
  @assign equalityEq = EQUATION_CREF_EQUALITY(lhsCref, rhsCref, source)
  return equalityEq
end

function makeEqualityAssert(
  lhsCref::ComponentRef,
  lhsSource::DAE.ElementSource,
  rhsCref::ComponentRef,
  rhsSource::DAE.ElementSource,
)::Equation
  local equalityAssert::Equation

  local source::DAE.ElementSource
  local lhs_exp::Expression
  local rhs_exp::Expression
  local exp::Expression
  local ty::M_Type

  @assign source = ElementSource.mergeSources(lhsSource, rhsSource)
  #= source := ElementSource.addElementSourceConnect(source, (lhsCref, rhsCref));
  =#
  @assign ty = getComponentType(lhsCref)
  @assign lhs_exp = fromCref(lhsCref)
  @assign rhs_exp = fromCref(rhsCref)
  if isReal(ty)
    @assign exp =
      BINARY_EXPRESSION(lhs_exp, makeSub(ty), rhs_exp)
    @assign exp = CALL_EXPRESSION(P_Call.makeTypedCall(
      NFBuiltinFuncs.ABS_REAL,
      list(exp),
      variability(exp),
    ))
    @assign exp = RELATION_EXPRESSION(
      exp,
      makeLessEq(ty),
      REAL_EXPRESSION(0.0),
    )
  else
    @assign exp =
      RELATION_EXPRESSION(lhs_exp, makeEqual(ty), rhs_exp)
  end
  #=  Modelica doesn't allow == for Reals, so to keep the flat Modelica
  =#
  #=  somewhat valid we use 'abs(lhs - rhs) <= 0' instead.
  =#
  #=  For any other type, generate assertion for 'lhs == rhs'.
  =#
  @assign equalityAssert =
    Equation.ASSERT(exp, EQ_ASSERT_STR, NFBuiltin.ASSERTIONLEVEL_ERROR, source)
  return equalityAssert
end

function generateFlowEquations(elements::List{<:Connector})::List{Equation}
  local equations::List{Equation}
  local c::Connector
  local c_rest::List{Connector}
  local src::DAE.ElementSource
  local sum::Expression
  @match _cons(c, c_rest) = elements
  src = c.source
  if listEmpty(c_rest)
    sum = fromCref(c.name)
  else
    sum = makeFlowExp(c)
    for e in c_rest
      sum = BINARY_EXPRESSION(
        sum,
        makeAdd(TYPE_REAL()),
        makeFlowExp(e),
      )
      src = DAE.emptyElementSource #ElementSource.mergeSources(src, e.source) TODO
    end
  end
  equations = list(EQUATION_EQUALITY(sum, REAL_EXPRESSION(0.0), c.ty, src))
  return equations
end

""" #= Creates an expression from a connector element, which is the element itself
   if it's an inside connector, or the element negated if it's outside. =#"""
function makeFlowExp(element::Connector)::Expression
  local exp::Expression

  local face::FaceType

  @assign exp = fromCref(element.name)
  #=  TODO: Remove unnecessary variable 'face' once #4502 is fixed.
  =#
  @assign face = element.face
  if face == Face.OUTSIDE
    @assign exp =
      UNARY_EXPRESSION(makeUMinus(TYPE_REAL()), exp)
  end
  return exp
end

""" #= Generates the equations for a stream connection set. =#"""
function generateStreamEquations(
  elements::List{<:Connector},
  flowThreshold::Expression,
)::List{Equation}
  local equations::List{Equation}

  @assign equations = begin
    local cr1::ComponentRef
    local cr2::ComponentRef
    local src::DAE.ElementSource
    local src1::DAE.ElementSource
    local src2::DAE.ElementSource
    local cref1::Expression
    local cref2::Expression
    local e1::Expression
    local e2::Expression
    local inside::List{Connector}
    local outside::List{Connector}
    local var1::VariabilityType
    local var2::VariabilityType
    #=  Unconnected stream connector, do nothing.
    =#
    @match elements begin
      Connector.CONNECTOR(face = Face.INSIDE) <| nil() => begin
        nil
      end

      Connector.CONNECTOR(face = Face.INSIDE) <|
      Connector.CONNECTOR(face = Face.INSIDE) <| nil() => begin
        nil
      end

      Connector.CONNECTOR(name = cr1, face = Face.OUTSIDE, source = src1) <|
      Connector.CONNECTOR(name = cr2, face = Face.OUTSIDE, source = src2) <|
      nil() => begin
        #=  Both inside, do nothing.
        =#
        #=  Both outside:
        =#
        #=  cr1 = inStream(cr2);
        =#
        #=  cr2 = inStream(cr1);
        =#
        @assign cref1 = fromCref(cr1)
        @assign cref2 = fromCref(cr2)
        @assign e1 = makeInStreamCall(cref2)
        @assign e2 = makeInStreamCall(cref1)
        @assign src = ElementSource.mergeSources(src1, src2)
        list(
          EQUATION_EQUALITY(cref1, e1, TYPE_REAL(), src),
          EQUATION_EQUALITY(cref2, e2, TYPE_REAL(), src),
        )
      end

      Connector.CONNECTOR(name = cr1, source = src1) <|
      Connector.CONNECTOR(name = cr2, source = src2) <| nil() => begin
        #=  One inside, one outside:
        =#
        #=  cr1 = cr2;
        =#
        @assign src = ElementSource.mergeSources(src1, src2)
        list(EQUATION_CREF_EQUALITY(cr1, cr2, src))
      end

      _ => begin
        #=  The general case with N inside connectors and M outside:
        =#
        @assign (outside, inside) =
          ListUtil.splitOnTrue(elements, Connector.isOutside)
        streamEquationGeneral(outside, inside, flowThreshold)
      end
    end
  end
  return equations
end

""" #= Generates an equation for an outside stream connector element. =#"""
function streamEquationGeneral(
  outsideElements::List{<:Connector},
  insideElements::List{<:Connector},
  flowThreshold::Expression,
)::List{Equation}
  local equations::List{Equation} = nil

  local outside::List{Connector} = outsideElements
  local cref_exp::Expression
  local res::Expression
  local src::DAE.ElementSource

  for e in outsideElements
    @assign cref_exp = fromCref(e.name)
    @assign outside = removeStreamSetElement(e.name, outsideElements)
    @assign res = streamSumEquationExp(outside, insideElements, flowThreshold)
    @assign src = ElementSource.addAdditionalComment(
      e.source,
      " equation generated from stream connection",
    )
    @assign equations =
      _cons(EQUATION_EQUALITY(cref_exp, res, TYPE_REAL(), src), equations)
  end
  return equations
end

""" #= Generates the sum expression used by stream connector equations, given M
  outside connectors and N inside connectors:

    (sum(max(-flow_exp[i], eps) * stream_exp[i] for i in N) +
     sum(max( flow_exp[i], eps) * inStream(stream_exp[i]) for i in M)) /
    (sum(max(-flow_exp[i], eps) for i in N) +
     sum(max( flow_exp[i], eps) for i in M))

  where eps = inFlowThreshold.
   =#"""
function streamSumEquationExp(
  outsideElements::List{<:Connector},
  insideElements::List{<:Connector},
  flowThreshold::Expression,
)::Expression
  local sumExp::Expression

  local outside_sum1::Expression
  local outside_sum2::Expression
  local inside_sum1::Expression
  local inside_sum2::Expression
  local res::Expression

  if listEmpty(outsideElements)
    @assign inside_sum1 = sumMap(insideElements, sumInside1, flowThreshold)
    @assign inside_sum2 = sumMap(insideElements, sumInside2, flowThreshold)
    @assign sumExp = BINARY_EXPRESSION(
      inside_sum1,
      makeDiv(TYPE_REAL()),
      inside_sum2,
    )
  elseif listEmpty(insideElements)
    @assign outside_sum1 = sumMap(outsideElements, sumOutside1, flowThreshold)
    @assign outside_sum2 = sumMap(outsideElements, sumOutside2, flowThreshold)
    @assign sumExp = BINARY_EXPRESSION(
      outside_sum1,
      makeDiv(TYPE_REAL()),
      outside_sum2,
    )
  else
    @assign outside_sum1 = sumMap(outsideElements, sumOutside1, flowThreshold)
    @assign outside_sum2 = sumMap(outsideElements, sumOutside2, flowThreshold)
    @assign inside_sum1 = sumMap(insideElements, sumInside1, flowThreshold)
    @assign inside_sum2 = sumMap(insideElements, sumInside2, flowThreshold)
    @assign sumExp = BINARY_EXPRESSION(
      BINARY_EXPRESSION(
        outside_sum1,
        makeAdd(TYPE_REAL()),
        inside_sum1,
      ),
      makeDiv(TYPE_REAL()),
      BINARY_EXPRESSION(
        outside_sum2,
        makeAdd(TYPE_REAL()),
        inside_sum2,
      ),
    )
  end
  #=  No outside components.
  =#
  #=  No inside components.
  =#
  #=  Both outside and inside components.
  =#
  return sumExp
end

""" #= Creates a sum expression by applying the given function on the list of
  elements and summing up the resulting expressions. =#"""
function sumMap(
  elements::List{<:Connector},
  func::FuncType,
  flowThreshold::Expression,
)::Expression
  local exp::Expression

  @assign exp = func(listHead(elements), flowThreshold)
  for e in listRest(elements)
    @assign exp = BINARY_EXPRESSION(
      func(e, flowThreshold),
      makeAdd(TYPE_REAL()),
      exp,
    )
  end
  return exp
end

""" #= Returns the stream and flow component in a stream set element as expressions. =#"""
function streamFlowExp(element::Connector)::Tuple{Expression, Expression}
  local flowExp::Expression
  local streamExp::Expression

  local stream_cr::ComponentRef

  @assign stream_cr = Connector.name(element)
  @assign streamExp = fromCref(stream_cr)
  @assign flowExp = fromCref(associatedFlowCref(stream_cr))
  return (streamExp, flowExp)
end

""" #= Returns the flow component in a stream set element as an expression. =#"""
function flowExp(element::Connector)::Expression
  local flowExp::Expression

  local flow_cr::ComponentRef

  @assign flow_cr = associatedFlowCref(Connector.name(element))
  @assign flowExp = fromCref(flow_cr)
  return flowExp
end

""" #= Helper function to streamSumEquationExp. Returns the expression
    max(flow_exp, eps) * inStream(stream_exp)
   given a stream set element. =#"""
function sumOutside1(element::Connector, flowThreshold::Expression)::Expression
  local exp::Expression

  local stream_exp::Expression
  local flow_exp::Expression

  @assign (stream_exp, flow_exp) = streamFlowExp(element)
  @assign exp = BINARY_EXPRESSION(
    makePositiveMaxCall(flow_exp, element, flowThreshold),
    makeMul(TYPE_REAL()),
    makeInStreamCall(stream_exp),
  )
  return exp
end

""" #= Helper function to streamSumEquationExp. Returns the expression
    max(-flow_exp, eps) * stream_exp
   given a stream set element. =#"""
function sumInside1(element::Connector, flowThreshold::Expression)::Expression
  local exp::Expression

  local stream_exp::Expression
  local flow_exp::Expression
  local flow_threshold::Expression

  @assign (stream_exp, flow_exp) = streamFlowExp(element)
  @assign flow_exp =
    UNARY_EXPRESSION(makeUMinus(TYPE_REAL()), flow_exp)
  @assign exp = BINARY_EXPRESSION(
    makePositiveMaxCall(flow_exp, element, flowThreshold),
    makeMul(TYPE_REAL()),
    stream_exp,
  )
  return exp
end

""" #= Helper function to streamSumEquationExp. Returns the expression
    max(flow_exp, eps)
   given a stream set element. =#"""
function sumOutside2(element::Connector, flowThreshold::Expression)::Expression
  local exp::Expression

  local flow_exp::Expression

  @assign flow_exp = flowExp(element)
  @assign exp = makePositiveMaxCall(flow_exp, element, flowThreshold)
  return exp
end

""" #= Helper function to streamSumEquationExp. Returns the expression
    max(-flow_exp, eps)
   given a stream set element. =#"""
function sumInside2(element::Connector, flowThreshold::Expression)::Expression
  local exp::Expression

  local flow_exp::Expression

  @assign flow_exp = flowExp(element)
  @assign flow_exp =
    UNARY_EXPRESSION(makeUMinus(TYPE_REAL()), flow_exp)
  @assign exp = makePositiveMaxCall(flow_exp, element, flowThreshold)
  return exp
end

""" #= Creates an inStream call expression. =#"""
function makeInStreamCall(streamExp::Expression)::Expression
  local inStreamCall::Expression

  @assign inStreamCall = CALL_EXPRESSION(P_Call.makeTypedCall(
    NFBuiltinFuncs.IN_STREAM,
    list(streamExp),
    variability(streamExp),
  ))
  return inStreamCall
end

""" #= Generates a max(flow_exp, eps) call. =#"""
function makePositiveMaxCall(
  flowExp::Expression,
  element::Connector,
  flowThreshold::Expression,
)::Expression
  local positiveMaxCall::Expression

  local flow_node::InstNode
  local nominal_oexp::Option{Expression}
  local nominal_exp::Expression
  local flow_threshold::Expression

  @assign flow_node =
    node(associatedFlowCref(Connector.name(
      element,
    )))
  @assign nominal_oexp =
    lookupAttributeValue("nominal", getClass(flow_node))
  if isSome(nominal_oexp)
    @match SOME(nominal_exp) = nominal_oexp
    @assign nominal_exp = getBindingExp(nominal_exp)
    @assign flow_threshold = BINARY_EXPRESSION(
      flowThreshold,
      makeMul(TYPE_REAL()),
      nominal_exp,
    )
  else
    @assign flow_threshold = flowThreshold
  end
  @assign positiveMaxCall = CALL_EXPRESSION(P_Call.makeTypedCall(
    NFBuiltinFuncs.POSITIVE_MAX_REAL,
    list(flowExp, flow_threshold),
    Connector.variability(element),
  ))
  setGlobalRoot(Global.isInStream, SOME(true))
  return positiveMaxCall
end

function isStreamCall(exp::Expression)::Bool
  local streamCall::Bool
  streamCall = begin
    @match exp begin
      CALL_EXPRESSION(__) => begin
        begin
          @match name(typedFunction(exp.call)) begin
            Absyn.IDENT("inStream") => begin
              true
            end
            Absyn.IDENT("actualStream") => begin
              true
            end
            _ => begin
              false
            end
          end
        end
      end
      _ => begin
        false
      end
    end
  end
  return streamCall
end

function evaluateOperatorReductionExp(
  exp::Expression,
  sets::ConnectionSets.Sets,
  setsArray::Vector{<:List{<:Connector}},
  ctable::CardinalityTable.Table,
)::Expression
  local evalExp::Expression

  local call::Call
  local fn::M_Function
  local ty::M_Type
  local arg::Expression
  local iter_exp::Expression
  local iters::List{Tuple{InstNode, Expression}} = nil
  local iter_node::InstNode

  @assign evalExp = begin
    @match exp begin
      CALL_EXPRESSION(call = call && P_Call.TYPED_REDUCTION(__)) => begin
        @assign ty = typeOf(call.exp)
        for iter in call.iters
          @assign (iter_node, iter_exp) = iter
          if variability(component(iter_node)) >
             Variability.PARAMETER
            print("Iteration range in reduction containing connector operator calls must be a parameter expression.")
            fail()
          end
          @assign iter_exp = Ceval.evalExp(iter_exp)
          @assign ty = liftArrayLeftList(
            ty,
            arrayDims(typeOf(iter_exp)),
          )
          @assign iters = _cons((iter_node, iter_exp), iters)
        end
        @assign iters = listReverseInPlace(iters)
        @assign arg = P_ExpandExp.ExpandExp.expandArrayConstructor(call.exp, ty, iters)
        CALL_EXPRESSION(P_Call.makeTypedCall(
          call.fn,
          list(arg),
          call.var,
          call.ty,
        ))
      end
    end
  end
  @assign evalExp = evaluateOperators(evalExp, sets, setsArray, ctable)
  return evalExp
end

function evaluateOperatorArrayConstructorExp(
  exp::Expression,
  sets::ConnectionSets.Sets,
  setsArray::Vector{<:List{<:Connector}},
  ctable::CardinalityTable.Table,
)::Expression
  local evalExp::Expression

  local expanded::Bool

  @assign (evalExp, expanded) = P_ExpandExp.ExpandExp.expand(exp)
  if !expanded
    Error.addInternalError(
      getInstanceName() +
      " failed to expand call containing stream operator: " +
      toString(exp),
      sourceInfo(),
    )
  end
  @assign evalExp = evaluateOperators(evalExp, sets, setsArray, ctable)
  return evalExp
end

""" #= Evaluates the inStream operator with the given cref as argument. =#"""
function evaluateInStream(
  cref::ComponentRef,
  sets::ConnectionSets.Sets,
  setsArray::Vector{<:List{<:Connector}},
  ctable::CardinalityTable.Table,
)::Expression
  local exp::Expression

  local c::Connector
  local sl::List{Connector}
  local set::Int

  @assign c = Connector.CONNECTOR(
    cref,
    TYPE_UNKNOWN(),
    Face.INSIDE,
    ConnectorType.STREAM,
    DAE.emptyElementSource,
  )
  try
    @assign set = ConnectionSets.findSetArrayIndex(c, sets)
    @assign sl = arrayGet(setsArray, set)
  catch
    @assign sl = list(c)
  end
  @assign exp = generateInStreamExp(
    cref,
    sl,
    sets,
    setsArray,
    ctable,
    Flags.getConfigReal(Flags.FLOW_THRESHOLD),
  )
  return exp
end

""" #= Helper function to evaluateInStream. Generates an expression for inStream
   given a connection set. =#"""
function generateInStreamExp(
  streamCref::ComponentRef,
  streams::List{<:Connector},
  sets::ConnectionSets.Sets,
  setsArray::Vector{<:List{<:Connector}},
  ctable::CardinalityTable.Table,
  flowThreshold::AbstractFloat,
)::Expression
  local exp::Expression

  local reducedStreams::List{Connector}
  local inside::List{Connector}
  local outside::List{Connector}
  local cr::ComponentRef
  local f1::FaceType
  local f2::FaceType

  @assign reducedStreams = list(s for s in streams if !isZeroFlowMinMax(s, streamCref))
  @assign exp = begin
    @match reducedStreams begin
      Connector.CONNECTOR(face = Face.INSIDE) <| nil() => begin
        fromCref(streamCref)
      end

      Connector.CONNECTOR(face = Face.INSIDE) <|
      Connector.CONNECTOR(face = Face.INSIDE) <| nil() => begin
        #=  Unconnected stream connector:
        =#
        #=    inStream(c) = c;
        =#
        #=  Two inside connected stream connectors:
        =#
        #=    inStream(c1) = c2;
        =#
        #=    inStream(c2) = c1;
        =#
        @match list(Connector.CONNECTOR(name = cr)) =
          removeStreamSetElement(streamCref, reducedStreams)
        fromCref(cr)
      end

      Connector.CONNECTOR(face = f1) <|
      Connector.CONNECTOR(face = f2) <| nil() where {(f1 != f2)} => begin
        #=  One inside, one outside connected stream connector:
        =#
        #=    inStream(c1) = inStream(c2);
        =#
        @match list(Connector.CONNECTOR(name = cr)) =
          removeStreamSetElement(streamCref, reducedStreams)
        evaluateInStream(cr, sets, setsArray, ctable)
      end

      _ => begin
        #=  The general case:
        =#
        @assign (outside, inside) =
          ListUtil.splitOnTrue(reducedStreams, Connector.isOutside)
        @assign inside = removeStreamSetElement(streamCref, inside)
        @assign exp = streamSumEquationExp(
          outside,
          inside,
          REAL_EXPRESSION(flowThreshold),
        )
        #=  Evaluate any inStream calls that were generated.
        =#
        @assign exp = evaluateOperators(exp, sets, setsArray, ctable)
        exp
      end
    end
  end
  return exp
end

""" #= Returns true if the given flow attribute of a connector is zero. =#"""
function isZeroFlowMinMax(conn::Connector, streamCref::ComponentRef)::Bool
  local isZero::Bool

  if isEqual(streamCref, conn.name)
    @assign isZero = false
  elseif Connector.isOutside(conn)
    @assign isZero = isZeroFlow(conn, "max")
  else
    @assign isZero = isZeroFlow(conn, "min")
  end
  return isZero
end

""" #= Returns true if the given flow attribute of a connector is zero. =#"""
function isZeroFlow(element::Connector, attr::String)::Bool
  local isZero::Bool

  local attr_oexp::Option{Expression}
  local flow_exp::Expression
  local attr_exp::Expression
  local flow_node::InstNode

  @assign flow_exp = flowExp(element)
  @assign flow_node =
    node(toCref(flow_exp))
  @assign attr_oexp = lookupAttributeValue(attr, getClass(flow_node))
  if isSome(attr_oexp)
    @match SOME(attr_exp) = attr_oexp
    @assign isZero =
      isZero(getBindingExp(attr_exp))
  else
    @assign isZero = false
  end
  return isZero
end

""" #= This function evaluates the actualStream operator for a component reference,
   given the connection sets. =#"""
function evaluateActualStream(
  streamCref::ComponentRef,
  sets::ConnectionSets.Sets,
  setsArray::Vector{<:List{<:Connector}},
  ctable::CardinalityTable.Table,
  mulCref::Option{<:ComponentRef} = NONE(),
)::Expression
  local exp::Expression

  local flow_dir::Int
  local flow_cr::ComponentRef
  local flow_exp::Expression
  local stream_exp::Expression
  local instream_exp::Expression
  local op::Operator

  @assign flow_cr = associatedFlowCref(streamCref)
  @assign flow_dir = evaluateFlowDirection(flow_cr)
  #=  Select a branch if we know the flow direction, otherwise generate the whole
  =#
  #=  if-equation.
  =#
  if flow_dir == 1
    @assign exp = evaluateInStream(streamCref, sets, setsArray, ctable)
  elseif flow_dir == (-1)
    @assign exp = fromCref(streamCref)
  else
    @assign flow_exp = fromCref(flow_cr)
    @assign stream_exp = fromCref(streamCref)
    @assign instream_exp = evaluateInStream(streamCref, sets, setsArray, ctable)
    @assign op =
      makeGreater(nodeType(flow_cr))
    @assign exp = IF_EXPRESSION(
      RELATION_EXPRESSION(flow_exp, op, REAL_EXPRESSION(0.0)),
      instream_exp,
      stream_exp,
    )
    if isNone(mulCref) ||
       !isEqual(flow_cr, Util.getOption(mulCref))
      @assign exp = makeSmoothCall(exp, 0)
    end
  end
  #=  actualStream(stream_var) = smooth(0, if flow_var > 0 then inStream(stream_var)
  =#
  #=                                                       else stream_var);
  =#
  return exp
end

""" #= Handles expressions on the form flowCref * actualStream(streamCref) where
   flowCref is associated with streamCref. =#"""
function evaluateActualStreamMul(
  crefExp::Expression,
  actualStreamArg::Expression,
  op::Operator,
  sets::ConnectionSets.Sets,
  setsArray::Vector{<:List{<:Connector}},
  ctable::CardinalityTable.Table,
)::Expression
  local outExp::Expression

  local e1::Expression
  local e2::Expression
  local cr::ComponentRef
  local flow_cr::ComponentRef

  @match (@match CREF_EXPRESSION(cref = cr) = e1) =
    evaluateOperators(crefExp, sets, setsArray, ctable)
  @assign e2 = evaluateActualStream(
    toCref(actualStreamArg),
    sets,
    setsArray,
    ctable,
    SOME(cr),
  )
  @assign outExp = BINARY_EXPRESSION(e1, op, e2)
  #=  Wrap the expression in smooth if the result would be flow_cr * (if flow_cr > 0 then ...)
  =#
  @assign outExp = begin
    @match e2 begin
      IF_EXPRESSION(__) => begin
        makeSmoothCall(outExp, 0)
      end

      _ => begin
        outExp
      end
    end
  end
  return outExp
end

function evaluateFlowDirection(flowCref::ComponentRef)::Int
  local direction::Int = 0

  local flow_cls::Class
  local omin::Option{Expression}
  local omax::Option{Expression}
  local min_val::AbstractFloat
  local max_val::AbstractFloat

  @assign flow_cls = getClass(node(flowCref))
  @assign omin = lookupAttributeValue("min", flow_cls)
  @assign omin =
    simplifyOpt(Util.applyOption(omin, getBindingExp))
  @assign omax = lookupAttributeValue("max", flow_cls)
  @assign omax =
    simplifyOpt(Util.applyOption(omax, getBindingExp))
  @assign direction = begin
    @match (omin, omax) begin
      (NONE(), NONE()) => begin
        0
      end

      (SOME(REAL_EXPRESSION(min_val)), NONE()) => begin
        if min_val >= 0
          1
        else
          0
        end
      end

      (NONE(), SOME(REAL_EXPRESSION(max_val))) => begin
        if max_val <= 0
          -1
        else
          0
        end
      end

      (
        SOME(REAL_EXPRESSION(min_val)),
        SOME(REAL_EXPRESSION(max_val)),
      ) => begin
        if min_val >= 0 && max_val >= min_val
          1
        elseif (max_val <= 0 && min_val <= max_val)
          -1
        else
          0
        end
      end

      _ => begin
        0
      end
    end
  end
  #=  No attributes, flow direction can't be decided.
  =#
  #=  Flow is positive if min is positive.
  =#
  #=  Flow is negative if max is negative.
  =#
  #=  Flow is positive if both min and max are positive, negative if they are
  =#
  #=  both negative, otherwise undecideable.
  =#
  #=  Flow is undecideable if either attribute is not a constant Real value.
  =#
  return direction
end

""" Creates a smooth(order, arg) call. """
function makeSmoothCall(arg::Expression, order::Int)::Expression
  local callExp::Expression

  @assign callExp = CALL_EXPRESSION(P_Call.makeTypedCall(
    NFBuiltinFuncs.SMOOTH,
    list(DAE.INTEGER_EXPRESSION(order), arg),
    variability(arg),
  ))
  return callExp
end

""" This function removes the given cref from a connection set. """
function removeStreamSetElement(
  cref::ComponentRef,
  elements::List{<:Connector},
)::List{Connector}

  @assign elements = ListUtil.deleteMemberOnTrue(cref, elements, compareCrefStreamSet)
  return elements
end

"""
  Helper function to removeStreamSetElement. Checks if the cref in a stream set
  element matches the given cref.
"""
function compareCrefStreamSet(cref::ComponentRef, element::Connector)::Bool
  local matches::Bool

  @assign matches = isEqual(cref, element.name)
  return matches
end

""" #= Returns the flow cref that's declared in the same connector as the given
   stream cref. =#"""
function associatedFlowCref(streamCref::ComponentRef)::ComponentRef
  local flowCref::ComponentRef

  local ty::M_Type
  local rest_cr::ComponentRef
  local flow_node::InstNode

  @match CREF(ty = ty, restCref = rest_cr) = streamCref
  @assign flowCref = begin
    @match arrayElementType(ty) begin
      TYPE_COMPLEX(complexTy = ComplexType.CONNECTOR(flows = flow_node <| nil())) => begin
        prefixCref(
          flow_node,
          getType(flow_node),
          nil,
          streamCref,
        )
      end

      _ => begin
        associatedFlowCref(rest_cr)
      end
    end
  end
  #=  A connector with a single flow, append the flow node to the cref and return it.
  =#
  #=  Otherwise, remove the first part of the cref and try again.
  =#
  return flowCref
end
