function addBindingExpParent(parent::InstNode, exp::Expression) ::Expression
  @assign () = begin
    @match exp begin
      BINDING_EXP(__)  => begin
        @assign exp.parents = _cons(parent, exp.parents)
        ()
      end
      _  => begin
        ()
      end
    end
  end
  exp
end

function mostPropagatedSubExp_traverser(exp::Expression, mostPropagated::Tuple{<:Integer, Expression}) ::Tuple{Integer, Expression}
  local max_prop::Integer
  local exp_prop::Integer
  if isBindingExp(exp)
    @assign (max_prop, _) = mostPropagated
    @assign exp_prop = propagatedDimCount(exp)
    if exp_prop > max_prop
      @assign mostPropagated = (exp_prop, exp)
    end
  end
  mostPropagated
end

""" #= Returns the most propagated subexpression in either of the two given
               expressions, as well as the number of dimensions it's been propagated
               through. Returns the first expression and -1 as the number of dimensions
               if neither expression contains any binding expressions. =#"""
                 function mostPropagatedSubExpBinary(exp1::Expression, exp2::Expression) ::Tuple{Expression, Integer}
                   local maxPropCount::Integer
                   local maxPropExp::Expression

                   #=  TODO: Optimize this, there's no need to check for bindings in e.g. literal arrays.
                   =#
                   @assign (maxPropCount, maxPropExp) = fold(exp1, mostPropagatedSubExp_traverser, (-1, exp1))
                   @assign (maxPropCount, maxPropExp) = fold(exp2, mostPropagatedSubExp_traverser, (maxPropCount, maxPropExp))
                   (maxPropExp, maxPropCount)
                 end

""" #= Returns the most propagated subexpression of the given expression, as well
               as the number of dimensions it's been propagated through. Returns the
               expression itself and -1 as the number of dimensions if it doesn't contain
               any binding expressions. =#"""
                 function mostPropagatedSubExp(exp::Expression) ::Tuple{Expression, Integer}
                   local maxPropCount::Integer
                   local maxPropExp::Expression

                   #=  TODO: Optimize this, there's no need to check for bindings in e.g. literal arrays.
                   =#
                   @assign (maxPropCount, maxPropExp) = fold(exp, mostPropagatedSubExp_traverser, (-1, exp))
                   (maxPropExp, maxPropCount)
                 end

function bindingExpMap4(exp::Expression, subs::List{<:Subscript}) ::Expression
  local outExp::Expression

  @assign outExp = begin
    local prop_count::Integer
    local prop_subs::List{Subscript}
    @match exp begin
      BINDING_EXP(__)  => begin
        @assign prop_count = propagatedDimCount(exp)
        @assign prop_subs = ListUtil.lastN(subs, prop_count)
        applySubscripts(prop_subs, exp.exp)
      end

      _  => begin
        exp
      end
    end
  end
  outExp
end

function bindingExpMap3(exp::Expression, evalFunc::EvalFunc, subs::List{<:Subscript}) ::Expression
  local result::Expression

  local e1::Expression
  local e2::Expression
  local op::Operator

  @assign result = map(exp, (subs) -> bindingExpMap4(subs = subs))
  @assign result = evalFunc(result)
  result
end

function bindingExpMap2(exp::Expression, evalFunc::EvalFunc, mostPropagatedCount::Integer, mostPropagatedExp::Expression) ::Expression
  local result::Expression

  local exp_ty::M_Type
  local bind_ty::M_Type
  local dims::List{Dimension}
  local e::Expression
  local parents::List{InstNode}
  local is_each::Bool

  @match BINDING_EXP(exp = e, expType = exp_ty, parents = parents, isEach = is_each) = mostPropagatedExp
  @assign dims = ListUtil.firstN(arrayDims(exp_ty), mostPropagatedCount)
  @assign result = vectorize(exp, dims, (evalFunc) -> bindingExpMap3(evalFunc = evalFunc))
  @assign (exp_ty, bind_ty) = bindingExpType(result, mostPropagatedCount)
  @assign result = BINDING_EXP(result, exp_ty, bind_ty, parents, is_each)
  result
end

""" #= Calls the given function on each element of a binding expression. =#"""
function bindingExpMap(exp::Expression, evalFunc::EvalFunc) ::Expression
  local result::Expression

  local max_prop_exp::Expression
  local max_prop_count::Integer

  @assign (max_prop_exp, max_prop_count) = mostPropagatedSubExp(exp)
  if max_prop_count >= 0
    @assign result = bindingExpMap2(exp, evalFunc, max_prop_count, max_prop_exp)
  else
    @assign result = evalFunc(exp)
  end
  result
end

""" #= Constructs an array with the given dimensions by calling the given
               function on the given expression for each combination of subscripts defined
               by the dimensions. =#"""
                 function vectorize(exp::Expression, dims::List{<:Dimension}, func::FuncT, accumSubs::List{<:Subscript} = nil) ::Expression
                   local outExp::Expression

                   local iter::RangeIterator
                   local dim::Dimension
                   local rest_dims::List{Dimension}
                   local expl::List{Expression}
                   local e::Expression

                   if listEmpty(dims)
                     @assign outExp = func(exp, listReverse(accumSubs))
                   else
                     @assign expl = nil
                     @match _cons(dim, rest_dims) = dims
                     @assign iter = P_RangeIterator.RangeIterator.fromDim(dim)
                     while P_RangeIterator.RangeIterator.hasNext(iter)
                       @assign (iter, e) = P_RangeIterator.RangeIterator.next(iter)
                       @assign e = vectorize(exp, rest_dims, func, _cons(SUBSCRIPT_INDEX(e), accumSubs))
                       @assign expl = _cons(e, expl)
                     end
                     @assign outExp = makeExpArray(listReverseInPlace(expl))
                   end
                   outExp
                 end

""" #= Calculates the expression type and binding type of an expression given the
               number of dimensions it's been propagated through. =#"""
                 function bindingExpType(exp::Expression, propagatedDimCount::Integer) ::Tuple{M_Type, M_Type}
                   local bindingType::M_Type
                   local expType::M_Type

                   @assign expType = typeOf(exp)
                   @assign bindingType = if propagatedDimCount > 0
                     Type.unliftArrayN(propagatedDimCount, expType)
                   else
                     expType
                   end
                   (expType, bindingType)
                 end

""" #= Returns the number of dimensions a binding expression has been propagated
               through. =#"""
                 function propagatedDimCount(exp::Expression) ::Integer
                   local dimCount::Integer

                   @assign dimCount = begin
                     @match exp begin
                       BINDING_EXP(isEach = false)  => begin
                         if Type.isKnown(exp.expType)
                           @assign dimCount = Type.dimensionCount(exp.expType) - Type.dimensionCount(exp.bindingType)
                         else
                           @assign dimCount = 0
                           for parent in listRest(exp.parents)
                             @assign dimCount = dimCount + Type.dimensionCount(getType(parent))
                           end
                         end
                         dimCount
                       end

                       _  => begin
                         0
                       end
                     end
                   end
                   dimCount
                 end

""" #= Replaces all binding expressions in the given expression with the
               expressions they contain. =#"""
                 function stripBindingInfo(exp::Expression) ::Expression
                   local outExp::Expression

                   @assign outExp = map(exp, getBindingExp)
                   outExp
                 end

""" #= Returns the expression contained in a binding expression, if the given
               expression is a binding expression. =#"""
                 function getBindingExp(bindingExp::Expression) ::Expression
                   local outExp::Expression

                   @assign outExp = begin
                     @match bindingExp begin
                       BINDING_EXP(__)  => begin
                         getBindingExp(bindingExp.exp)
                       end

                       _  => begin
                         bindingExp
                       end
                     end
                   end
                   outExp
                 end

function isBindingExp(exp::Expression) ::Bool
  local isBindingExp::Bool

  @assign isBindingExp = begin
    @match exp begin
      BINDING_EXP(__)  => begin
        true
      end

      _  => begin
        false
      end
    end
  end
  isBindingExp
end

function nthEnumLiteral(ty::M_Type, n::Integer) ::Expression
  local exp::Expression

  @assign exp = ENUM_LITERAL(ty, Type.nthEnumLiteral(ty, n), n)
  exp
end

function retype(exp::Expression) ::Expression


  @assign () = begin
    local dims::List{Dimension}
    @match exp begin
      RANGE_EXPRESSION(__)  => begin
        @assign exp.ty = TypeCheck.getRangeType(exp.start, exp.step, exp.stop, typeOf(exp.start), AbsynUtil.dummyInfo)
        ()
      end

      CALL_EXPRESSION(call = P_Call.TYPED_ARRAY_CONSTRUCTOR(__))  => begin
        @assign exp.call = P_Call.retype(exp.call)
        ()
      end

      _  => begin
        ()
      end
    end
  end
  exp
end

function splitRecordCref(exp::Expression) ::Expression
  local outExp::Expression

  @assign outExp = P_ExpandExp.ExpandExp.expand(exp)
  @assign outExp = begin
    local cls::InstNode
    local comps::Array{InstNode}
    local cr::ComponentRef
    local field_cr::ComponentRef
    local ty::M_Type
    local fields::List{Expression}
    @match outExp begin
      CREF(ty = TYPE_COMPLEX(cls = cls), cref = cr)  => begin
        @assign comps = getComponents(classTree(getClass(cls)))
        @assign fields = nil
        for i in arrayLength(comps):(-1):1
          @assign ty = getType(comps[i])
          @assign field_cr = prefixCref(comps[i], ty, nil, cr)
          @assign fields = _cons(CREF(ty, field_cr), fields)
        end
        makeRecord(scopePath(cls), outExp.ty, fields)
      end

      ARRAY_EXPRESSION(__)  => begin
        @assign outExp.elements = list(splitRecordCref(e) for e in outExp.elements)
        outExp
      end

      _  => begin
        exp
      end
    end
  end
  outExp
end

""" #= Returns the nth field of a record expression. If the expression is an array
               it will return an array with the nth field in each array element. =#"""
                 function nthRecordElement(index::Integer, recordExp::Expression) ::Expression
                   local outExp::Expression

                   @assign outExp = begin
                     local node::InstNode
                     local expl::List{Expression}
                     @match recordExp begin
                       RECORD_EXPRESSION(__)  => begin
                         listGet(recordExp.elements, index)
                       end

                       ARRAY_EXPRESSION(elements =  nil(), ty = ARRAY_TYPE(elementType = TYPE_COMPLEX(cls = node)))  => begin
                         makeEmptyArray(getType(nthComponent(index, getClass(node))))
                       end

                       ARRAY_EXPRESSION(__)  => begin
                         @assign expl = list(nthRecordElement(index, e) for e in recordExp.elements)
                         makeArray(setArrayElementType(recordExp.ty, typeOf(listHead(expl))), expl)
                       end

                       RECORD_ELEMENT_EXPRESSION(ty = ARRAY_TYPE(elementType = TYPE_COMPLEX(cls = node)))  => begin
                         @assign node = nthComponent(index, getClass(node))
                         RECORD_ELEMENT_EXPRESSION(recordExp, index, name(node), Type.liftArrayLeftList(getType(node), arrayDims(recordExp.ty)))
                       end

                       BINDING_EXP(__)  => begin
                         bindingExpMap(recordExp, (index) -> nthRecordElement(index = index))
                       end

                       _  => begin
                         @match TYPE_COMPLEX(cls = node) = typeOf(recordExp)
                         @assign node = nthComponent(index, getClass(node))
                         RECORD_ELEMENT_EXPRESSION(recordExp, index, name(node), getType(node))
                       end
                     end
                   end
                   outExp
                 end

""" #= Returns the field with the given name in a record expression. If the
               expression is an array it will return the equivalent of calling the
               function on each element of the array. =#"""
                 function recordElement(elementName::String, recordExp::Expression) ::Expression
                   local outExp::Expression

                   @assign outExp = begin
                     local node::InstNode
                     local cls::Class
                     local cls_tree::ClassTree
                     local ty::M_Type
                     local index::Integer
                     local expl::List{Expression}
                     local cref::ComponentRef
                     @match recordExp begin
                       RECORD_EXPRESSION(ty = TYPE_COMPLEX(cls = node))  => begin
                         @assign cls = getClass(node)
                         @assign index = lookupComponentIndex(elementName, cls)
                         listGet(recordExp.elements, index)
                       end

                       CREF(ty = TYPE_COMPLEX(cls = node))  => begin
                         @assign cls_tree = classTree(getClass(node))
                         @match (node, false) = lookupElement(elementName, cls_tree)
                         @assign ty = getType(node)
                         @assign cref = prefixCref(node, ty, nil, recordExp.cref)
                         @assign ty = Type.liftArrayLeftList(ty, arrayDims(recordExp.ty))
                         CREF(ty, cref)
                       end

                       ARRAY_EXPRESSION(elements =  nil(), ty = ARRAY_EXPRESSION_TYPE(elementType = TYPE_COMPLEX(cls = node)))  => begin
                         @assign cls = getClass(node)
                         @assign index = lookupComponentIndex(elementName, cls)
                         @assign ty = getType(nthComponent(index, cls))
                         makeArray(ty, nil)
                       end

                       ARRAY_EXPRESSION(ty = ARRAY_TYPE(elementType = TYPE_COMPLEX(cls = node)))  => begin
                         @assign index = lookupComponentIndex(elementName, getClass(node))
                         @assign expl = list(nthRecordElement(index, e) for e in recordExp.elements)
                         @assign ty = Type.liftArrayLeft(typeOf(listHead(expl)), P_Dimension.Dimension.fromInteger(listLength(expl)))
                         makeArray(ty, expl, recordExp.literal)
                       end

                       BINDING_EXP(__)  => begin
                         bindingExpMap(recordExp, (elementName) -> recordElement(elementName = elementName))
                       end

                       SUBSCRIPTED_EXP_EXPRESSION(__)  => begin
                         @assign outExp = recordElement(elementName, recordExp.exp)
                         SUBSCRIPTED_EXP_EXPRESSION(outExp, recordExp.subscripts, Type.lookupRecordFieldType(elementName, recordExp.ty))
                       end

                       EMPTY(__)  => begin
                         fail()
                       end

                       _  => begin
                         @assign ty = typeOf(recordExp)
                         @match TYPE_COMPLEX(cls = node) = arrayElementType(ty)
                         @assign cls = getClass(node)
                         @assign index = lookupComponentIndex(elementName, cls)
                         @assign ty = Type.liftArrayRightList(getType(nthComponent(index, cls)), arrayDims(ty))
                         RECORD_ELEMENT_EXPRESSION(recordExp, index, elementName, ty)
                       end
                     end
                   end
                   outExp
                 end

function tupleElement(exp::Expression, ty::M_Type, index::Integer) ::Expression
  local tupleElem::Expression

  @assign tupleElem = begin
    local ety::M_Type
    @match exp begin
      TUPLE_EXPRESSION(__)  => begin
        listGet(exp.elements, index)
      end

      ARRAY_EXPRESSION(__)  => begin
        @assign ety = Type.unliftArray(ty)
        @assign exp.elements = list(tupleElement(e, ety, index) for e in exp.elements)
        exp
      end

      BINDING_EXP(__)  => begin
        bindingExpMap(exp, (ty, index) -> tupleElement(ty = ty, index = index))
      end

      _  => begin
        TUPLE_EXPRESSION_ELEMENT(exp, index, ty)
      end
    end
  end
  tupleElem
end

function toScalar(exp::Expression) ::Expression
  local outExp::Expression

  @assign outExp = begin
    @match exp begin
      ARRAY_EXPRESSION(elements = outExp <|  nil())  => begin
        toScalar(outExp)
      end

      _  => begin
        exp
      end
    end
  end
  outExp
end

function enumIndexExp(enumExp::Expression) ::Expression
  local indexExp::Expression

  @assign indexExp = begin
    @match enumExp begin
      ENUM_LITERAL(__)  => begin
        INTEGER(enumExp.index)
      end

      _  => begin
        CALL_EXPRESSION(P_Call.makeTypedCall(NFBuiltinFuncs.INTEGER_ENUM, list(enumExp), variability(enumExp)))
      end
    end
  end
  indexExp
end

function isEmpty(exp::Expression) ::Bool
  local empty::Bool

  @assign empty = begin
    @match exp begin
      EMPTY(__)  => begin
        true
      end

      _  => begin
        false
      end
    end
  end
  empty
end

function isMutable(exp::Expression) ::Bool
  local isMutable::Bool

  @assign isMutable = begin
    @match exp begin
      MUTABLE_EXPRESSION(__)  => begin
        true
      end

      _  => begin
        false
      end
    end
  end
  isMutable
end

function makeImmutable(exp::Expression)::Expression
  local outExp::Expression
  @assign outExp = begin
    @match exp begin
      MUTABLE_EXPRESSION(__)  => begin
        P_Pointer.access(exp.exp)
      end

      _  => begin
        exp
      end
    end
  end
  outExp
end

function makeMutable(exp::Expression)::Expression
  local outExp::Expression
  @assign outExp = MUTABLE_EXPRESSION(P_Pointer.create(exp))
  outExp
end

function variabilityList(expl::List{<:Expression}, var::VariabilityType = Variability.CONSTANT)::VariabilityType
  for e in expl
    @assign var = variabilityMax(var, variability(e))
  end
  var
end

function variability(exp::Expression) ::VariabilityType
  local var::VariabilityType
  @assign var = begin
    @match exp begin
      INTEGER(__)  => begin
        Variability.CONSTANT
      end

      REAL(__)  => begin
        Variability.CONSTANT
      end

      STRING(__)  => begin
        Variability.CONSTANT
      end

      BOOLEAN(__)  => begin
        Variability.CONSTANT
      end

      ENUM_LITERAL(__)  => begin
        Variability.CONSTANT
      end

      CLKCONST_EXPRESSION(_)  => begin
        Variability.DISCRETE
      end

      CREF(__)  => begin
        variability(exp.cref)
      end

      TYPENAME(__)  => begin
        Variability.CONSTANT
      end

      ARRAY_EXPRESSION(__)  => begin
        variabilityList(exp.elements)
      end

      MATRIX_EXPRESSION(__)  => begin
        ListUtil.fold(exp.elements, variabilityList, Variability.CONSTANT)
      end

      RANGE_EXPRESSION(__)  => begin
        @assign var = variability(exp.start)
        @assign var = variabilityMax(var, variability(exp.stop))
        if isSome(exp.step)
          @assign var = variabilityMax(var, variability(Util.getOption(exp.step)))
        end
        var
      end

      TUPLE_EXPRESSION(__)  => begin
        variabilityList(exp.elements)
      end

      RECORD_EXPRESSION(__)  => begin
        variabilityList(exp.elements)
      end

      CALL_EXPRESSION(__)  => begin
        P_Call.variability(exp.call)
      end

      SIZE_EXPRESSION(__)  => begin
        if isSome(exp.dimIndex)
          @assign var = variabilityMax(Variability.PARAMETER, variability(Util.getOption(exp.dimIndex)))
        else
          @assign var = Variability.PARAMETER
        end
        var
      end

      END(__)  => begin
        Variability.PARAMETER
      end

      BINARY_EXPRESSION(__)  => begin
        variabilityMax(variability(exp.exp1), variability(exp.exp2))
      end

      UNARY(__)  => begin
        variability(exp.exp)
      end

      LBINARY_EXPRESSION(__)  => begin
        variabilityMax(variability(exp.exp1), variability(exp.exp2))
      end

      LUNARY(__)  => begin
        variability(exp.exp)
      end

      RELATION_EXPRESSION(__)  => begin
        P_Prefixes.variabilityMin(variabilityMax(variability(exp.exp1), variability(exp.exp2)), Variability.DISCRETE)
      end

      IF_EXPRESSION(__)  => begin
        variabilityMax(variability(exp.condition), variabilityMax(variability(exp.trueBranch), variability(exp.falseBranch)))
      end

      CAST_EXPRESSION(__)  => begin
        variability(exp.exp)
      end

      UNBOX_EXPRESSION(__)  => begin
        variability(exp.exp)
      end

      SUBSCRIPTED_EXP_EXPRESSION(__)  => begin
        variabilityMax(variability(exp.exp), variabilityList(exp.subscripts))
      end

      TUPLE_ELEMENT_EXPRESSION(__)  => begin
        variability(exp.tupleExp)
      end

      RECORD_ELEMENT_EXPRESSION(__)  => begin
        variability(exp.recordExp)
      end

      BOX_EXPRESSION(__)  => begin
        variability(exp.exp)
      end

      PARTIAL_FUNCTION_APPLICATION_EXPRESSION(__)  => begin
        Variability.CONTINUOUS
      end

      BINDING_EXP(__)  => begin
        variability(exp.exp)
      end

      _  => begin
        Error.assertion(false, getInstanceName() + " got unknown expression.", sourceInfo())
        fail()
      end
    end
  end
  var
end

function promote2(exp::Expression, isArray::Bool, dims::Integer, types::List{<:M_Type}) ::Expression
  local outExp::Expression

  @assign outExp = begin
    local ty::M_Type
    local rest_ty::List{M_Type}
    local arr_exp::Expression
    local expanded::Bool
    #=  No types left, we're done!
    =#
    @match (exp, types) begin
      (_,  nil())  => begin
        exp
      end

      (ARRAY_EXPRESSION(__), ty <| rest_ty)  => begin
        makeArray(ty, list(promote2(e, false, dims, rest_ty) for e in exp.elements))
      end

      (_, _) where (isArray)  => begin
        #=  An array, promote each element in the array.
        =#
        #=  An expression with array type, but which is not an array expression.
        =#
        #=  Such an expression can't be promoted here, so we create a promote call instead.
        =#
        @assign (outExp, expanded) = P_ExpandExp.ExpandExp.expand(exp)
        if expanded
          @assign outExp = promote2(outExp, true, dims, types)
        else
          @assign outExp = CALL_EXPRESSION(P_Call.makeTypedCall(NFBuiltinFuncs.PROMOTE, list(exp, INTEGER(dims)), variability(exp), listHead(types)))
        end
        outExp
      end

      _  => begin
        #=  A scalar expression, promote it as many times as the number of types given.
        =#
        @assign outExp = exp
        for ty in listReverse(types)
          @assign outExp = makeArray(ty, list(outExp))
        end
        outExp
      end
    end
  end
  outExp
end

function promote(e::Expression, ty::M_Type, n::Integer) ::Tuple{Expression, M_Type}



  local dims::List{Dimension}
  local ety::M_Type
  local tys::List{M_Type} = nil
  local is_array::Bool

  #=  Construct the dimensions that needs to be added.
  =#
  @assign dims = list(P_Dimension.Dimension.fromInteger(1) for i in Type.dimensionCount(ty):n - 1)
  if ! listEmpty(dims)
    @assign dims = listAppend(arrayDims(ty), dims)
    @assign is_array = isArray(ty)
    @assign ety = arrayElementType(ty)
    @assign ty = Type.liftArrayLeftList(ety, dims)
    while ! listEmpty(dims)
      @assign tys = _cons(Type.liftArrayLeftList(ety, dims), tys)
      @assign dims = listRest(dims)
    end
    @assign e = promote2(e, is_array, n, listReverse(tys))
  end
  #=  Concatenate the existing dimensions and the added ones.
  =#
  #=  Construct the result type.
  =#
  #=  Construct the expression types, to avoid having to create a new type
  =#
  #=  for each subexpression that will be created.
  =#
  (e, ty)
end

function makeIdentityMatrix(n::Integer, elementType::M_Type) ::Expression
  local matrix::Expression

  local zero::Expression
  local one::Expression
  local row::List{Expression}
  local rows::List{Expression} = nil
  local row_ty::M_Type

  @assign zero = makeZero(elementType)
  @assign one = makeOne(elementType)
  @assign row_ty = ARRAY_TYPE(elementType, list(P_Dimension.Dimension.fromInteger(n)))
  for i in 1:n
    @assign row = nil
    for j in 2:i
      @assign row = _cons(zero, row)
    end
    @assign row = _cons(one, row)
    for j in i:n - 1
      @assign row = _cons(zero, row)
    end
    @assign rows = _cons(makeArray(row_ty, row, literal = true), rows)
  end
  @assign matrix = makeArray(Type.liftArrayLeft(row_ty, P_Dimension.Dimension.fromInteger(n)), rows, literal = true)
  matrix
end

function transposeArray(arrayExp::Expression) ::Expression
  local outExp::Expression

  local dim1::Dimension
  local dim2::Dimension
  local rest_dims::List{Dimension}
  local ty::M_Type
  local row_ty::M_Type
  local expl::List{Expression}
  local matrix::List{List{Expression}}
  local literal::Bool

  @match ARRAY(ARRAY_TYPE(ty, _cons(dim1, _cons(dim2, rest_dims))), expl, literal) = arrayExp
  if ! listEmpty(expl)
    @assign row_ty = ARRAY_TYPE(ty, _cons(dim1, rest_dims))
    @assign matrix = list(arrayElements(e) for e in expl)
    @assign matrix = ListUtil.transposeList(matrix)
    @assign expl = list(makeArray(row_ty, row, literal) for row in matrix)
  end
  @assign outExp = makeArray(ARRAY_TYPE(ty, _cons(dim2, _cons(dim1, rest_dims))), expl, literal)
  outExp
end

function hasArrayCall2(exp::Expression) ::Bool
  local hasArrayCall::Bool

  local call::Call
  local ty::M_Type

  @assign hasArrayCall = begin
    @match exp begin
      CALL_EXPRESSION(call = call)  => begin
        @assign ty = P_Call.typeOf(call)
        isArray(ty) && P_Call.isVectorizeable(call)
      end

      TUPLE_ELEMENT_EXPRESSION(tupleExp = CALL_EXPRESSION(call = call))  => begin
        @assign ty = Type.nthTupleType(P_Call.typeOf(call), exp.index)
        isArray(ty) && P_Call.isVectorizeable(call)
      end

      _  => begin
        false
      end
    end
  end
  hasArrayCall
end

""" #= Returns true if the given expression contains a function call that returns
               an array, otherwise false. =#"""
                 function hasArrayCall(exp::Expression) ::Bool
                   local hasArrayCall::Bool

                   @assign hasArrayCall = contains(exp, hasArrayCall2)
                   hasArrayCall
                 end

function arrayScalarElement(arrayExp::Expression) ::Expression
  local scalarExp::Expression

  @match ARRAY_EXPRESSION(elements = list(scalarExp)) = arrayExp
  scalarExp
end

function arrayScalarElements_impl(exp::Expression, elements::List{<:Expression}) ::List{Expression}


  @assign elements = begin
    @match exp begin
      ARRAY_EXPRESSION(__)  => begin
        for e in exp.elements
          @assign elements = arrayScalarElements_impl(e, elements)
        end
        elements
      end

      _  => begin
        _cons(exp, elements)
      end
    end
  end
  elements
end

function arrayScalarElements(exp::Expression) ::List{Expression}
  local elements::List{Expression}

  @assign elements = listReverseInPlace(arrayScalarElements_impl(exp, nil))
  elements
end

function arrayElements(Array::Expression) ::List{Expression}
  local elements::List{Expression}

  @match ARRAY_EXPRESSION(elements = elements) = Array
  elements
end

function negate(exp::Expression) ::Expression


  @assign exp = begin
    @match exp begin
      INTEGER(__)  => begin
        INTEGER(-exp.value)
      end

      REAL(__)  => begin
        REAL(-exp.value)
      end

      CAST_EXPRESSION(__)  => begin
        CAST_EXPRESSION(exp.ty, negate(exp.exp))
      end

      UNARY(__)  => begin
        exp.exp
      end

      _  => begin
        UNARY(OPERATOR(typeOf(exp), P_NFOperator.Op.UMINUS), exp)
      end
    end
  end
  exp
end

function isNegated(exp::Expression) ::Bool
  local negated::Bool

  @assign negated = begin
    @match exp begin
      INTEGER(__)  => begin
        exp.value < 0
      end

      REAL(__)  => begin
        exp.value < 0
      end

      CAST_EXPRESSION(__)  => begin
        isNegated(exp.exp)
      end

      UNARY(__)  => begin
        true
      end

      _  => begin
        false
      end
    end
  end
  negated
end

function unbox(boxedExp::Expression) ::Expression
  local exp::Expression

  @assign exp = begin
    local ty::M_Type
    @match boxedExp begin
      BOX_EXPRESSION(__)  => begin
        boxedExp.exp
      end

      _  => begin
        @assign ty = typeOf(boxedExp)
        if isBoxed(ty)
          UNBOX_EXPRESSION(boxedExp, Type.unbox(ty))
        else
          boxedExp
        end
      end
    end
  end
  exp
end

function box(exp::Expression) ::Expression
  local boxedExp::Expression

  @assign boxedExp = begin
    @match exp begin
      BOX_EXPRESSION(__)  => begin
        exp
      end

      _  => begin
        BOX_EXPRESSION(exp)
      end
    end
  end
  boxedExp
end

function makeMinValue(ty::M_Type) ::Expression
  local exp::Expression

  @assign exp = begin
    @match ty begin
      TYPE_REAL(__)  => begin
        REAL(-System.realMaxLit())
      end

      TYPE_INTEGER(__)  => begin
        INTEGER(-System.intMaxLit())
      end

      TYPE_BOOLEAN(__)  => begin
        BOOLEAN(false)
      end

      TYPE_ENUMERATION(__)  => begin
        ENUM_LITERAL(ty, listHead(ty.literals), 1)
      end

      ARRAY_TYPE(__)  => begin
        makeArray(ty, ListUtil.fill(makeMaxValue(Type.unliftArray(ty)), P_Dimension.Dimension.size(listHead(ty.dimensions))), literal = true)
      end
    end
  end
  exp
end

function makeMaxValue(ty::M_Type) ::Expression
  local exp::Expression

  @assign exp = begin
    @match ty begin
      TYPE_REAL(__)  => begin
        REAL(System.realMaxLit())
      end

      TYPE_INTEGER(__)  => begin
        INTEGER(System.intMaxLit())
      end

      TYPE_BOOLEAN(__)  => begin
        BOOLEAN(true)
      end

      TYPE_ENUMERATION(__)  => begin
        ENUM_LITERAL(ty, ListUtil.last(ty.literals), listLength(ty.literals))
      end

      ARRAY_TYPE(__)  => begin
        ARRAY_EXPRESSION(ty, ListUtil.fill(makeMaxValue(Type.unliftArray(ty)), P_Dimension.Dimension.size(listHead(ty.dimensions))), literal = true)
      end
    end
  end
  exp
end

function makeOne(ty::M_Type) ::Expression
  local zeroExp::Expression

  @assign zeroExp = begin
    @match ty begin
      TYPE_REAL(__)  => begin
        REAL(1.0)
      end

      TYPE_INTEGER(__)  => begin
        INTEGER(1)
      end

      ARRAY_TYPE(__)  => begin
        ARRAY_EXPRESSION(ty, ListUtil.fill(makeZero(Type.unliftArray(ty)), P_Dimension.Dimension.size(listHead(ty.dimensions))), literal = true)
      end
    end
  end
  zeroExp
end

function makeOperatorRecordZero(recordNode::InstNode) ::Expression
  local zeroExp::Expression

  local op_node::InstNode
  local fn::P_Function.P_Function

  @assign op_node = lookupElement("'0'", getClass(recordNode))
  P_Function.P_Function.instFunctionNode(op_node)
  @match list(fn) = P_Function.P_Function.typeNodeCache(op_node)
  @assign zeroExp = CALL_EXPRESSION(P_Call.makeTypedCall(fn, nil, Variability.CONSTANT))
  @assign zeroExp = Ceval.evalExp(zeroExp)
  zeroExp
end

function makeZero(ty::M_Type) ::Expression
  local zeroExp::Expression

  @assign zeroExp = begin
    @match ty begin
      TYPE_REAL(__)  => begin
        REAL(0.0)
      end

      TYPE_INTEGER(__)  => begin
        INTEGER(0)
      end

      ARRAY_TYPE(__)  => begin
        ARRAY_EXPRESSION(ty, ListUtil.fill(makeZero(Type.unliftArray(ty)), P_Dimension.Dimension.size(listHead(ty.dimensions))), literal = true)
      end

      TYPE_COMPLEX(__)  => begin
        makeOperatorRecordZero(ty.cls)
      end
    end
  end
  zeroExp
end

""" #= Creates an array from the given list of dimensions, where each element is
               the given expression. Example:
                 liftArrayList([2, 3], 1) => {{1, 1, 1}, {1, 1, 1}} =#"""
                   function liftArrayList(dims::List{<:Dimension}, exp::Expression) ::Tuple{Expression, M_Type}
                     local arrayType::M_Type = typeOf(exp)


                     local expl::List{Expression}
                     local is_literal::Bool = isLiteral(exp)

                     for dim in listReverse(dims)
                       @assign expl = nil
                       for i in 1:P_Dimension.Dimension.size(dim)
                         @assign expl = _cons(exp, expl)
                       end
                       @assign arrayType = Type.liftArrayLeft(arrayType, dim)
                       @assign exp = makeArray(arrayType, expl, literal = is_literal)
                     end
                     (exp, arrayType)
                   end

""" #= Creates an array with the given dimension, where each element is the given
               expression. Example: liftArray([3], 1) => {1, 1, 1} =#"""
                 function liftArray(dim::Dimension, exp::Expression) ::Tuple{Expression, M_Type}
                   local arrayType::M_Type = typeOf(exp)


                   local expl::List{Expression} = nil

                   for i in 1:P_Dimension.Dimension.size(dim)
                     @assign expl = _cons(exp, expl)
                   end
                   @assign arrayType = Type.liftArrayLeft(arrayType, dim)
                   @assign exp = makeArray(arrayType, expl, literal = isLiteral(exp))
                   (exp, arrayType)
                 end

""" #= Creates an array with the given type, filling it with the given scalar
               expression. =#"""
                 function fillType(ty::M_Type, fillExp::Expression) ::Expression
                   local exp::Expression = fillExp

                   local dims::List{Dimension} = arrayDims(ty)
                   local expl::List{Expression}
                   local arr_ty::M_Type = arrayElementType(ty)

                   for dim in listReverse(dims)
                     @assign expl = nil
                     for i in 1:P_Dimension.Dimension.size(dim)
                       @assign expl = _cons(exp, expl)
                     end
                     @assign arr_ty = Type.liftArrayLeft(arr_ty, dim)
                     @assign exp = makeArray(arr_ty, expl, literal = isLiteral(exp))
                   end
                   exp
                 end

function isRecordOrRecordArray(exp::Expression) ::Bool
  local isRecord::Bool

  @assign isRecord = begin
    @match exp begin
      RECORD_EXPRESSION(__)  => begin
        true
      end

      ARRAY_EXPRESSION(__)  => begin
        ListUtil.all(exp.elements, isRecordOrRecordArray)
      end

      _  => begin
        false
      end
    end
  end
  isRecord
end

function isRecord(exp::Expression) ::Bool
  local isRecord::Bool

  @assign isRecord = begin
    @match exp begin
      RECORD_EXPRESSION(__)  => begin
        true
      end

      _  => begin
        false
      end
    end
  end
  isRecord
end

function isBoolean(exp::Expression) ::Bool
  local isBool::Bool

  @assign isBool = begin
    @match exp begin
      BOOLEAN(__)  => begin
        true
      end

      _  => begin
        false
      end
    end
  end
  isBool
end

function isInteger(exp::Expression) ::Bool
  local isInteger::Bool

  @assign isInteger = begin
    @match exp begin
      INTEGER(__)  => begin
        true
      end

      _  => begin
        false
      end
    end
  end
  isInteger
end

function isLiteral(exp::Expression) ::Bool
  local literal::Bool

  @assign literal = begin
    @match exp begin
      INTEGER_EXPRESSION(__)  => begin
        true
      end

      REAL_EXPRESSION(__)  => begin
        true
      end

      STRING_EXPRESSION(__)  => begin
        true
      end

      BOOLEAN_EXPRESSION(__)  => begin
        true
      end

      ENUM_LITERAL_EXPRESSION(__)  => begin
        true
      end

      ARRAY_EXPRESSION(__)  => begin
        ListUtil.all(exp.elements, isLiteral)
      end

      RECORD_EXPRESSION(__)  => begin
        ListUtil.all(exp.elements, isLiteral)
      end

      RANGE_EXPRESSION(__)  => begin
        isLiteral(exp.start) && isLiteral(exp.stop) && Util.applyOptionOrDefault(exp.step, isLiteral, true)
      end
      _  => begin
        false
      end
    end
  end
  literal
end

function isScalarLiteral(exp::Expression) ::Bool
  local literal::Bool

  @assign literal = begin
    @match exp begin
      INTEGER(__)  => begin
        true
      end

      REAL(__)  => begin
        true
      end

      STRING(__)  => begin
        true
      end

      BOOLEAN(__)  => begin
        true
      end

      ENUM_LITERAL(__)  => begin
        true
      end

      _  => begin
        false
      end
    end
  end
  literal
end

function isNegative(exp::Expression) ::Bool
  local negative::Bool

  @assign negative = begin
    @match exp begin
      INTEGER(__)  => begin
        exp.value < 0
      end

      REAL(__)  => begin
        exp.value < 0
      end

      BOOLEAN(__)  => begin
        false
      end

      ENUM_LITERAL(__)  => begin
        false
      end

      CAST_EXPRESSION(__)  => begin
        isNegative(exp.exp)
      end

      UNARY(__)  => begin
        ! isNegative(exp.exp)
      end

      _  => begin
        false
      end
    end
  end
  negative
end

function isOne(exp::Expression) ::Bool
  local isOne::Bool

  @assign isOne = begin
    @match exp begin
      INTEGER(__)  => begin
        exp.value == 1
      end

      REAL(__)  => begin
        exp.value == 1.0
      end

      CAST_EXPRESSION(__)  => begin
        isOne(exp.exp)
      end

      _  => begin
        false
      end
    end
  end
  isOne
end

function isZero(exp::Expression) ::Bool
  local isZero::Bool

  @assign isZero = begin
    @match exp begin
      INTEGER(__)  => begin
        exp.value == 0
      end

      REAL(__)  => begin
        exp.value == 0.0
      end

      CAST_EXPRESSION(__)  => begin
        isZero(exp.exp)
      end

      UNARY(__)  => begin
        isZero(exp.exp)
      end

      _  => begin
        false
      end
    end
  end
  isZero
end

function containsIterator(exp::Expression, origin::ORIGIN_Type) ::Bool
  local iter::Bool

  if ExpOrigin.flagSet(origin, ExpOrigin.FOR)
    @assign iter = contains(exp, isIterator)
  else
    @assign iter = false
  end
  iter
end

function isIterator(exp::Expression) ::Bool
  local isIterator::Bool

  @assign isIterator = begin
    @match exp begin
      CREF_EXPRESSION(__)  => begin
        isIterator(exp.cref)
      end

      _  => begin
        false
      end
    end
  end
  isIterator
end

function toCref(exp::Expression) ::ComponentRef
  local cref::ComponentRef

  @match CREF_EXPRESSION(cref = cref) = exp
  cref
end

function fromCref(cref::ComponentRef) ::Expression
  local exp::Expression

  @assign exp = CREF_EXPRESSION(getSubscriptedType(cref), cref)
  exp
end

function arrayAllEqual2(arrayExp::Expression, element::Expression) ::Bool
  local allEqual::Bool

  @assign allEqual = begin
    @match arrayExp begin
      ARRAY_EXPRESSION(elements = ARRAY_EXPRESSION(__) <| _)  => begin
        ListUtil.map1BoolAnd(arrayExp.elements, arrayAllEqual2, element)
      end

      ARRAY_EXPRESSION(__)  => begin
        ListUtil.map1BoolAnd(arrayExp.elements, isEqual, element)
      end

      _  => begin
        true
      end
    end
  end
  allEqual
end

""" #= Checks if all scalar elements in an array are equal to each other. =#"""
function arrayAllEqual(arrayExp::Expression) ::Bool
  local allEqual::Bool

  @assign allEqual = begin
    @matchcontinue arrayExp begin
      ARRAY_EXPRESSION(__)  => begin
        arrayAllEqual2(arrayExp, arrayFirstScalar(arrayExp))
      end

      _  => begin
        true
      end
    end
  end
  allEqual
end

""" #= Returns the first scalar element of an array. Fails if the array is empty. =#"""
function arrayFirstScalar(arrayExp::Expression) ::Expression
  local exp::Expression

  @assign exp = begin
    @match arrayExp begin
      ARRAY_EXPRESSION(__)  => begin
        arrayFirstScalar(listHead(arrayExp.elements))
      end

      _  => begin
        arrayExp
      end
    end
  end
  exp
end

function callContainsShallow(call::Call, func::ContainsPred) ::Bool
  local res::Bool

  @assign res = begin
    local e::Expression
    @match call begin
      P_Call.UNTYPED_CALL(__)  => begin
        @assign res = listContainsShallow(call.arguments, func)
        if ! res
          for arg in call.named_args
            @assign (_, e) = arg
            if func(e)
              @assign res = true
              break
            end
          end
        end
        res
      end

      P_Call.ARG_TYPED_CALL(__)  => begin
        for arg in call.arguments
          @assign (e, _, _) = arg
          if func(e)
            @assign res = true
            return
          end
        end
        for arg in call.named_args
          @assign (_, e, _, _) = arg
          if func(e)
            @assign res = true
            return
          end
        end
        false
      end

      P_Call.TYPED_CALL(__)  => begin
        listContainsShallow(call.arguments, func)
      end

      P_Call.UNTYPED_ARRAY_CONSTRUCTOR(__)  => begin
        func(call.exp)
      end

      P_Call.TYPED_ARRAY_CONSTRUCTOR(__)  => begin
        func(call.exp)
      end

      P_Call.UNTYPED_REDUCTION(__)  => begin
        func(call.exp)
      end

      P_Call.TYPED_REDUCTION(__)  => begin
        func(call.exp)
      end
    end
  end
  res
end

function listContainsShallow(expl::List{<:Expression}, func::ContainsPred) ::Bool
  local res::Bool

  for e in expl
    if func(e)
      @assign res = true
      return res
    end
  end
  @assign res = false
  res
end

function crefContainsShallow(cref::ComponentRef, func::ContainsPred) ::Bool
  local res::Bool

  @assign res = begin
    @match cref begin
      CREF(__)  => begin
        listContainsExpShallow(cref.subscripts, func) || crefContainsShallow(cref.restCref, func)
      end

      _  => begin
        false
      end
    end
  end
  res
end

function containsShallow(exp::Expression, func::ContainsPred) ::Bool
  local res::Bool

  @assign res = begin
    @match exp begin
      CREF(__)  => begin
        crefContainsShallow(exp.cref, func)
      end

      ARRAY_EXPRESSION(__)  => begin
        listContainsShallow(exp.elements, func)
      end

      MATRIX_EXPRESSION(__)  => begin
        @assign res = false
        for row in exp.elements
          if listContainsShallow(row, func)
            @assign res = true
            break
          end
        end
        res
      end

      RANGE_EXPRESSION(__)  => begin
        func(exp.start) || Util.applyOptionOrDefault(exp.step, func, false) || func(exp.stop)
      end

      TUPLE_EXPRESSION(__)  => begin
        listContainsShallow(exp.elements, func)
      end

      RECORD_EXPRESSION(__)  => begin
        listContainsShallow(exp.elements, func)
      end

      CALL_EXPRESSION(__)  => begin
        callContainsShallow(exp.call, func)
      end

      SIZE_EXPRESSION(__)  => begin
        Util.applyOptionOrDefault(exp.dimIndex, func, false) || func(exp.exp)
      end

      BINARY_EXPRESSION(__)  => begin
        func(exp.exp1) || func(exp.exp2)
      end

      UNARY(__)  => begin
        func(exp.exp)
      end

      LBINARY_EXPRESSION(__)  => begin
        func(exp.exp1) || func(exp.exp2)
      end

      LUNARY(__)  => begin
        func(exp.exp)
      end

      RELATION_EXPRESSION(__)  => begin
        func(exp.exp1) || func(exp.exp2)
      end

      IF_EXPRESSION(__)  => begin
        func(exp.condition) || func(exp.trueBranch) || func(exp.falseBranch)
      end

      CAST_EXPRESSION(__)  => begin
        func(exp.exp)
      end

      UNBOX_EXPRESSION(__)  => begin
        func(exp.exp)
      end

      SUBSCRIPTED_EXP_EXPRESSION(__)  => begin
        func(exp.exp) || listContainsExpShallow(exp.subscripts, func)
      end

      TUPLE_ELEMENT_EXPRESSION(__)  => begin
        func(exp.tupleExp)
      end

      RECORD_ELEMENT_EXPRESSION(__)  => begin
        func(exp.recordExp)
      end

      PARTIAL_FUNCTION_APPLICATION_EXPRESSION(__)  => begin
        listContains(exp.args, func)
      end

      BOX_EXPRESSION(__)  => begin
        func(exp.exp)
      end

      _  => begin
        false
      end
    end
  end
  res
end

function callContains(call::Call, func::ContainsPred) ::Bool
  local res::Bool

  @assign res = begin
    local e::Expression
    @match call begin
      P_Call.UNTYPED_CALL(__)  => begin
        @assign res = listContains(call.arguments, func)
        if ! res
          for arg in call.named_args
            @assign (_, e) = arg
            if contains(e, func)
              @assign res = true
              break
            end
          end
        end
        res
      end

      P_Call.ARG_TYPED_CALL(__)  => begin
        for arg in call.arguments
          @assign (e, _, _) = arg
          if contains(e, func)
            @assign res = true
            return
          end
        end
        for arg in call.named_args
          @assign (_, e, _, _) = arg
          if contains(e, func)
            @assign res = true
            return
          end
        end
        false
      end

      P_Call.TYPED_CALL(__)  => begin
        listContains(call.arguments, func)
      end

      P_Call.UNTYPED_ARRAY_CONSTRUCTOR(__)  => begin
        contains(call.exp, func)
      end

      P_Call.TYPED_ARRAY_CONSTRUCTOR(__)  => begin
        contains(call.exp, func)
      end

      P_Call.UNTYPED_REDUCTION(__)  => begin
        contains(call.exp, func)
      end

      P_Call.TYPED_REDUCTION(__)  => begin
        contains(call.exp, func)
      end
    end
  end
  res
end

function listContains(expl::List{<:Expression}, func::ContainsPred) ::Bool
  local res::Bool

  for e in expl
    if contains(e, func)
      @assign res = true
      return res
    end
  end
  @assign res = false
  res
end

function crefContains(cref::ComponentRef, func::ContainsPred) ::Bool
  local res::Bool

  @assign res = begin
    @match cref begin
      CREF(__)  => begin
        listContainsExp(cref.subscripts, func) || crefContains(cref.restCref, func)
      end

      _  => begin
        false
      end
    end
  end
  res
end

function contains(exp::Expression, func::ContainsPred) ::Bool
  local res::Bool
  if func(exp)
    @assign res = true
    return res
  end
  @assign res = begin
    local e::Expression
    @match exp begin
      CREF(__)  => begin
        crefContains(exp.cref, func)
      end
      ARRAY_EXPRESSION(__)  => begin
        listContains(exp.elements, func)
      end
      MATRIX_EXPRESSION(__)  => begin
        @assign res = false
        for row in exp.elements
          if listContains(row, func)
            @assign res = true
            break
          end
        end
        res
      end
      RANGE_EXPRESSION(__)  => begin
        contains(exp.start, func) || containsOpt(exp.step, func) || contains(exp.stop, func)
      end
      TUPLE_EXPRESSION(__)  => begin
        listContains(exp.elements, func)
      end
      RECORD_EXPRESSION(__)  => begin
        listContains(exp.elements, func)
      end
      CALL_EXPRESSION(__)  => begin
        callContains(exp.call, func)
      end
      SIZE_EXPRESSION(__)  => begin
        containsOpt(exp.dimIndex, func) || contains(exp.exp, func)
      end
      BINARY_EXPRESSION(__)  => begin
        contains(exp.exp1, func) || contains(exp.exp2, func)
      end
      UNARY(__)  => begin
        contains(exp.exp, func)
      end
      LBINARY_EXPRESSION(__)  => begin
        contains(exp.exp1, func) || contains(exp.exp2, func)
      end
      LUNARY(__)  => begin
        contains(exp.exp, func)
      end
      RELATION_EXPRESSION(__)  => begin
        contains(exp.exp1, func) || contains(exp.exp2, func)
      end
      IF_EXPRESSION(__)  => begin
        contains(exp.condition, func) || contains(exp.trueBranch, func) || contains(exp.falseBranch, func)
      end
      CAST_EXPRESSION(__)  => begin
        contains(exp.exp, func)
      end
      UNBOX_EXPRESSION(__)  => begin
        contains(exp.exp, func)
      end
      SUBSCRIPTED_EXP_EXPRESSION(__)  => begin
        contains(exp.exp, func) || listContainsExp(exp.subscripts, func)
      end
      TUPLE_ELEMENT_EXPRESSION(__)  => begin
        contains(exp.tupleExp, func)
      end
      RECORD_ELEMENT_EXPRESSION(__)  => begin
        contains(exp.recordExp, func)
      end
      PARTIAL_FUNCTION_APPLICATION_EXPRESSION(__)  => begin
        listContains(exp.args, func)
      end
      BOX_EXPRESSION(__)  => begin
        contains(exp.exp, func)
      end
      _  => begin
        false
      end
    end
  end
  res
end

function containsOpt(exp::Option{<:Expression}, func::ContainsPred) ::Bool
  local res::Bool

  local e::Expression

  @assign res = begin
    @match exp begin
      SOME(e)  => begin
        contains(e, func)
      end

      _  => begin
        false
      end
    end
  end
  res
end



function mapFoldCrefShallow(cref::ComponentRef, func::MapFunc, arg::ArgT)  where {ArgT}

  local outCref::ComponentRef

  @assign outCref = begin
    local subs::List{Subscript}
    local rest::ComponentRef
    @match cref begin
      COMPONENT_REF_CREF(origin = Origin.CREF)  => begin
        @assign (subs, arg) = ListUtil.map1Fold(cref.subscripts, mapFoldExpShallow, func, arg)
        @assign (rest, arg) = mapFoldCrefShallow(cref.restCref, func, arg)
        COMPONENT_REF_CREF(cref.node, subs, cref.ty, cref.origin, rest)
      end
      _  => begin
        cref
      end
    end
  end
  (outCref, arg)
end

function mapFoldCallIteratorsShallow(iters::List{Tuple{InstNode, Expression}}, func::MapFunc, arg::ArgT)  where {ArgT}

  local outIters::List{Tuple{InstNode, Expression}} = nil

  local node::InstNode
  local exp::Expression
  local new_exp::Expression

  for i in iters
    @assign (node, exp) = i
    @assign (new_exp, arg) = func(exp, arg)
    @assign outIters = _cons(if referenceEq(new_exp, exp)
                             i
                             else
                             (node, new_exp)
                             end, outIters)
  end
  @assign outIters = listReverseInPlace(outIters)
  (outIters, arg)
end

function mapFoldCallShallow(call::Call, func::MapFunc, foldArg::ArgT)  where {ArgT}

  local outCall::Call

  @assign outCall = begin
    local args::List{Expression}
    local nargs::List{P_Function.NamedArg}
    local targs::List{P_Function.TypedArg}
    local tnargs::List{P_Function.TypedNamedArg}
    local s::String
    local e::Expression
    local t::M_Type
    local v::VariabilityType
    local iters::List{Tuple{InstNode, Expression}}
    local default_exp::Option{Expression}
    local fold_exp::Tuple{Option{Expression}, String, String}
    local oe::Option{Expression}
    @match call begin
      P_Call.UNTYPED_CALL(__)  => begin
        @assign (args, foldArg) = ListUtil.mapFold(call.arguments, func, foldArg)
        @assign nargs = nil
        for arg in call.named_args
          @assign (s, e) = arg
          @assign (e, foldArg) = func(e, foldArg)
          @assign nargs = _cons((s, e), nargs)
        end
        P_Call.UNTYPED_CALL(call.ref, args, listReverse(nargs), call.call_scope)
      end

      P_Call.ARG_TYPED_CALL(__)  => begin
        @assign targs = nil
        @assign tnargs = nil
        for arg in call.arguments
          @assign (e, t, v) = arg
          @assign (e, foldArg) = func(e, foldArg)
          @assign targs = _cons((e, t, v), targs)
        end
        for arg in call.named_args
          @assign (s, e, t, v) = arg
          @assign (e, foldArg) = func(e, foldArg)
          @assign tnargs = _cons((s, e, t, v), tnargs)
        end
        P_Call.ARG_TYPED_CALL(call.ref, listReverse(targs), listReverse(tnargs), call.call_scope)
      end

      P_Call.TYPED_CALL(__)  => begin
        @assign (args, foldArg) = ListUtil.mapFold(call.arguments, func, foldArg)
        P_Call.TYPED_CALL(call.fn, call.ty, call.var, args, call.attributes)
      end

      P_Call.UNTYPED_ARRAY_CONSTRUCTOR(__)  => begin
        @assign (e, foldArg) = func(call.exp, foldArg)
        @assign iters = mapFoldCallIteratorsShallow(call.iters, func, foldArg)
        P_Call.UNTYPED_ARRAY_CONSTRUCTOR(e, iters)
      end

      P_Call.TYPED_ARRAY_CONSTRUCTOR(__)  => begin
        @assign (e, foldArg) = func(call.exp, foldArg)
        @assign iters = mapFoldCallIteratorsShallow(call.iters, func, foldArg)
        P_Call.TYPED_ARRAY_CONSTRUCTOR(call.ty, call.var, e, iters)
      end

      P_Call.UNTYPED_REDUCTION(__)  => begin
        @assign (e, foldArg) = func(call.exp, foldArg)
        @assign iters = mapFoldCallIteratorsShallow(call.iters, func, foldArg)
        P_Call.UNTYPED_REDUCTION(call.ref, e, iters)
      end

      P_Call.TYPED_REDUCTION(__)  => begin
        @assign (e, foldArg) = func(call.exp, foldArg)
        @assign iters = mapFoldCallIteratorsShallow(call.iters, func, foldArg)
        @assign (default_exp, foldArg) = mapFoldOptShallow(call.defaultExp, func, foldArg)
        @assign oe = Util.tuple31(call.foldExp)
        if isSome(oe)
          @assign (oe, foldArg) = mapFoldOptShallow(oe, func, foldArg)
          @assign fold_exp = Util.applyTuple31(call.foldExp, (oe) -> Util.replace(arg = oe))
        else
          @assign fold_exp = call.foldExp
        end
        P_Call.TYPED_REDUCTION(call.fn, call.ty, call.var, e, iters, default_exp, fold_exp)
      end
    end
  end
  (outCall, foldArg)
end

function mapFoldOptShallow(exp::Option{Expression}, func::MapFunc, arg::ArgT)  where {ArgT}

  local outExp::Option{Expression}

  local e1::Expression
  local e2::Expression

  @assign outExp = begin
    @match exp begin
      SOME(e1)  => begin
        @assign (e2, arg) = func(e1, arg)
        if referenceEq(e1, e2)
          exp
        else
          SOME(e2)
        end
      end

      _  => begin
        exp
      end
    end
  end
  (outExp, arg)
end

function mapFoldClockShallow(clockExp::Expression, func::MapFunc, arg::ArgT)  where {ArgT}

  local outExp::Expression

  local clk::ClockKind
  local e1::Expression
  local e2::Expression
  local e3::Expression
  local e4::Expression

  @match CLKCONST_EXPRESSION(clk = clk) = clockExp
  @assign outExp = begin
    @match clk begin
      INTEGER_CLOCK(e1, e2)  => begin
        @assign (e3, arg) = func(e1, arg)
        @assign (e4, arg) = func(e2, arg)
        if referenceEq(e1, e3) && referenceEq(e2, e4)
          clockExp
        else
          CLKCONST_EXPRESSION(INTEGER_CLOCK(e3, e4))
        end
      end

      REAL_CLOCK(e1)  => begin
        @assign (e2, arg) = func(e1, arg)
        if referenceEq(e1, e2)
          clockExp
        else
          CLKCONST_EXPRESSION(REAL_CLOCK(e2))
        end
      end

      BOOLEAN_CLOCK(e1, e2)  => begin
        @assign (e3, arg) = func(e1, arg)
        @assign (e4, arg) = func(e2, arg)
        if referenceEq(e1, e3) && referenceEq(e2, e4)
          clockExp
        else
          CLKCONST_EXPRESSION(BOOLEAN_CLOCK(e3, e4))
        end
      end

      SOLVER_CLOCK(e1, e2)  => begin
        @assign (e3, arg) = func(e1, arg)
        @assign (e4, arg) = func(e2, arg)
        if referenceEq(e1, e3) && referenceEq(e2, e4)
          clockExp
        else
          CLKCONST_EXPRESSION(SOLVER_CLOCK(e3, e4))
        end
      end

      _  => begin
        clockExp
      end
    end
  end
  (outExp, arg)
end

function mapFoldShallow(exp::Expression, func::MapFunc, arg::ArgT)  where {ArgT}
  local outExp::Expression
  @assign outExp = begin
    local e1::Expression
    local e2::Expression
    local e3::Expression
    local e4::Expression
    local oe::Option{Expression}
    local cr::ComponentRef
    local expl::List{Expression}
    local call::Call
    local subs::List{Subscript}
    local unchanged::Bool
    @debug "Calling mapFoldShallow"
    @match exp begin
      CLKCONST_EXPRESSION(__)  => begin
        @assign (outExp, arg) = mapFoldClockShallow(exp, func, arg)
        outExp
      end
      CREF_EXPRESSION(__)  => begin
        @assign (cr, arg) = mapFoldCrefShallow(exp.cref, func, arg)
        if referenceEq(exp.cref, cr)
          exp
        else
          CREF_EXPRESSION(exp.ty, cr)
        end
      end
      ARRAY_EXPRESSION(__)  => begin
        @assign (expl, arg) = ListUtil.mapFold(exp.elements, func, arg)
        ARRAY_EXPRESSION(exp.ty, expl, exp.literal)
      end

      RANGE_EXPRESSION(step = oe)  => begin
        @assign (e1, arg) = func(exp.start, arg)
        @assign (oe, arg) = mapFoldOptShallow(exp.step, func, arg)
        @assign (e3, arg) = func(exp.stop, arg)
        if referenceEq(e1, exp.start) && referenceEq(oe, exp.step) && referenceEq(e3, exp.stop)
          exp
        else
          RANGE_EXPRESSION(exp.ty, e1, oe, e3)
        end
      end

      TUPLE_EXPRESSION(__)  => begin
        @assign (expl, arg) = ListUtil.mapFold(exp.elements, func, arg)
        TUPLE_EXPRESSION(exp.ty, expl)
      end

      RECORD_EXPRESSION(__)  => begin
        @assign (expl, arg) = ListUtil.mapFold(exp.elements, func, arg)
        RECORD_EXPRESSION(exp.path, exp.ty, expl)
      end

      CALL_EXPRESSION(__)  => begin
        @assign (call, arg) = mapFoldCallShallow(exp.call, func, arg)
        if referenceEq(exp.call, call)
          exp
        else
          CALL_EXPRESSION(call)
        end
      end

      SIZE_EXPRESSION(__)  => begin
        @assign (e1, arg) = func(exp.exp, arg)
        @assign (oe, arg) = mapFoldOptShallow(exp.dimIndex, func, arg)
        if referenceEq(exp.exp, e1) && referenceEq(exp.dimIndex, oe)
          exp
        else
          SIZE_EXPRESSION(e1, oe)
        end
      end

      BINARY_EXPRESSION(__)  => begin
        @assign (e1, arg) = func(exp.exp1, arg)
        @assign (e2, arg) = func(exp.exp2, arg)
        if referenceEq(exp.exp1, e1) && referenceEq(exp.exp2, e2)
          exp
        else
          BINARY_EXPRESSION(e1, exp.operator, e2)
        end
      end

      UNARY_EXPRESSION(__)  => begin
        @assign (e1, arg) = func(exp.exp, arg)
        if referenceEq(exp.exp, e1)
          exp
        else
          UNARY_EXPRESSION(exp.operator, e1)
        end
      end

      LBINARY_EXPRESSION(__)  => begin
        @assign (e1, arg) = func(exp.exp1, arg)
        @assign (e2, arg) = func(exp.exp2, arg)
        if referenceEq(exp.exp1, e1) && referenceEq(exp.exp2, e2)
          exp
        else
          LBINARY_EXPRESSION(e1, exp.operator, e2)
        end
      end

      LUNARY_EXPRESSION(__)  => begin
        @assign (e1, arg) = func(exp.exp, arg)
        if referenceEq(exp.exp, e1)
          exp
        else
          LUNARY_EXPRESSION(exp.operator, e1)
        end
      end

      RELATION_EXPRESSION(__)  => begin
        @assign (e1, arg) = func(exp.exp1, arg)
        @assign (e2, arg) = func(exp.exp2, arg)
        if referenceEq(exp.exp1, e1) && referenceEq(exp.exp2, e2)
          exp
        else
          RELATION_EXPRESSION(e1, exp.operator, e2)
        end
      end

      IF_EXPRESSION(__)  => begin
        @assign (e1, arg) = func(exp.condition, arg)
        @assign (e2, arg) = func(exp.trueBranch, arg)
        @assign (e3, arg) = func(exp.falseBranch, arg)
        if referenceEq(exp.condition, e1) && referenceEq(exp.trueBranch, e2) && referenceEq(exp.falseBranch, e3)
          exp
        else
          IF_EXPRESSION(e1, e2, e3)
        end
      end

      CAST_EXPRESSION(__)  => begin
        @assign (e1, arg) = func(exp.exp, arg)
        if referenceEq(exp.exp, e1)
          exp
        else
          CAST_EXPRESSION(exp.ty, e1)
        end
      end

      UNBOX_EXPRESSION(__)  => begin
        @assign (e1, arg) = func(exp.exp, arg)
        if referenceEq(exp.exp, e1)
          exp
        else
          UNBOX_EXPRESSION(e1, exp.ty)
        end
      end

      SUBSCRIPTED_EXP_EXPRESSION(__)  => begin
        @assign (e1, arg) = func(exp.exp, arg)
        @assign (subs, arg) = ListUtil.mapFold(exp.subscripts, (func) -> mapFoldExpShallow(func = func), arg)
        SUBSCRIPTED_EXP_EXPRESSION(e1, subs, exp.ty)
      end

      TUPLE_ELEMENT_EXPRESSION(__)  => begin
        @assign (e1, arg) = func(exp.tupleExp, arg)
        if referenceEq(exp.tupleExp, e1)
          exp
        else
          TUPLE_ELEMENT_EXPRESSION(e1, exp.index, exp.ty)
        end
      end

      RECORD_ELEMENT_EXPRESSION(__)  => begin
        @assign (e1, arg) = func(exp.recordExp, arg)
        if referenceEq(exp.recordExp, e1)
          exp
        else
          RECORD_ELEMENT_EXPRESSION(e1, exp.index, exp.fieldName, exp.ty)
        end
      end

      MUTABLE_EXPRESSION(__)  => begin
        @assign (e1, arg) = func(P_Pointer.access(exp.exp), arg)
        P_Pointer.update(exp.exp, e1)
        exp
      end

      PARTIAL_FUNCTION_APPLICATION_EXPRESSION(__)  => begin
        @assign (expl, arg) = ListUtil.mapFold(exp.args, func, arg)
        @assign exp.args = expl
        exp
      end

      BINDING_EXP(__)  => begin
        @assign (e1, arg) = func(exp.exp, arg)
        if referenceEq(exp.exp, e1)
          exp
        else
          BINDING_EXP(e1, exp.expType, exp.bindingType, exp.parents, exp.isEach)
        end
      end

      _  => begin
        exp
      end
    end
end
(outExp, arg)
end

function mapFoldCref(cref::ComponentRef, func::MapFunc, arg::ArgT)  where {ArgT}

  local outCref::ComponentRef

  @assign outCref = begin
    local subs::List{Subscript}
    local rest::ComponentRef
    @match cref begin
      CREF(origin = Origin.CREF)  => begin
        @assign (subs, arg) = ListUtil.map1Fold(cref.subscripts, mapFoldExp, func, arg)
        @assign (rest, arg) = mapFoldCref(cref.restCref, func, arg)
        CREF(cref.node, subs, cref.ty, cref.origin, rest)
      end

      _  => begin
        cref
      end
    end
  end
  (outCref, arg)
end

function mapFoldCallIterators(iters::List{Tuple{InstNode, Expression}}, func::MapFunc, arg::ArgT)  where {ArgT}

  local outIters::List{Tuple{InstNode, Expression}} = nil

  local node::InstNode
  local exp::Expression
  local new_exp::Expression

  for i in iters
    @assign (node, exp) = i
    @assign (new_exp, arg) = mapFold(exp, func, arg)
    @assign outIters = _cons(if referenceEq(new_exp, exp)
                             i
                             else
                             (node, new_exp)
                             end, outIters)
  end
  @assign outIters = listReverseInPlace(outIters)
  (outIters, arg)
end

function mapFoldCall(call::Call, func::MapFunc, foldArg::ArgT)  where {ArgT}

  local outCall::Call

  @assign outCall = begin
    local args::List{Expression}
    local nargs::List{P_Function.NamedArg}
    local targs::List{P_Function.TypedArg}
    local tnargs::List{P_Function.TypedNamedArg}
    local s::String
    local e::Expression
    local t::M_Type
    local v::VariabilityType
    local iters::List{Tuple{InstNode, Expression}}
    local default_exp::Option{Expression}
    local fold_exp::Tuple{Option{Expression}, String, String}
    local oe::Option{Expression}
    @match call begin
      P_Call.UNTYPED_CALL(__)  => begin
        @assign (args, foldArg) = ListUtil.map1Fold(call.arguments, mapFold, func, foldArg)
        @assign nargs = nil
        for arg in call.named_args
          @assign (s, e) = arg
          @assign (e, foldArg) = mapFold(e, func, foldArg)
          @assign nargs = _cons((s, e), nargs)
        end
        P_Call.UNTYPED_CALL(call.ref, args, listReverse(nargs), call.call_scope)
      end

      P_Call.ARG_TYPED_CALL(__)  => begin
        @assign targs = nil
        @assign tnargs = nil
        for arg in call.arguments
          @assign (e, t, v) = arg
          @assign (e, foldArg) = mapFold(e, func, foldArg)
          @assign targs = _cons((e, t, v), targs)
        end
        for arg in call.named_args
          @assign (s, e, t, v) = arg
          @assign (e, foldArg) = mapFold(e, func, foldArg)
          @assign tnargs = _cons((s, e, t, v), tnargs)
        end
        P_Call.ARG_TYPED_CALL(call.ref, listReverse(targs), listReverse(tnargs), call.call_scope)
      end

      P_Call.TYPED_CALL(__)  => begin
        @assign (args, foldArg) = ListUtil.map1Fold(call.arguments, mapFold, func, foldArg)
        P_Call.TYPED_CALL(call.fn, call.ty, call.var, args, call.attributes)
      end

      P_Call.UNTYPED_ARRAY_CONSTRUCTOR(__)  => begin
        @assign (e, foldArg) = mapFold(call.exp, func, foldArg)
        P_Call.UNTYPED_ARRAY_CONSTRUCTOR(e, call.iters)
      end

      P_Call.TYPED_ARRAY_CONSTRUCTOR(__)  => begin
        @assign (e, foldArg) = mapFold(call.exp, func, foldArg)
        P_Call.TYPED_ARRAY_CONSTRUCTOR(call.ty, call.var, e, call.iters)
      end

      P_Call.UNTYPED_REDUCTION(__)  => begin
        @assign (e, foldArg) = mapFold(call.exp, func, foldArg)
        P_Call.UNTYPED_REDUCTION(call.ref, e, call.iters)
      end

      P_Call.TYPED_REDUCTION(__)  => begin
        @assign (e, foldArg) = mapFold(call.exp, func, foldArg)
        @assign (iters, foldArg) = mapFoldCallIterators(call.iters, func, foldArg)
        @assign (default_exp, foldArg) = mapFoldOpt(call.defaultExp, func, foldArg)
        @assign oe = Util.tuple31(call.foldExp)
        if isSome(oe)
          @assign (oe, foldArg) = mapFoldOpt(oe, func, foldArg)
          @assign fold_exp = Util.applyTuple31(call.foldExp, (oe) -> Util.replace(arg = oe))
        else
          @assign fold_exp = call.foldExp
        end
        P_Call.TYPED_REDUCTION(call.fn, call.ty, call.var, e, iters, default_exp, fold_exp)
      end
    end
  end
  (outCall, foldArg)
end

function mapFoldOpt(exp::Option{Expression}, func::MapFunc, arg::ArgT)  where {ArgT}

  local outExp::Option{Expression}

  local e::Expression

  @assign outExp = begin
    @match exp begin
      SOME(e)  => begin
        @assign (e, arg) = mapFold(e, func, arg)
        SOME(e)
      end

      _  => begin
        exp
      end
    end
  end
  (outExp, arg)
end

function mapFold(exp::Expression, func::MapFunc, arg::ArgT)  where {ArgT}

  local outExp::Expression

  @assign outExp = begin
    local e1::Expression
    local e2::Expression
    local e3::Expression
    local e4::Expression
    local cr::ComponentRef
    local expl::List{Expression}
    local call::Call
    local subs::List{Subscript}
    @match exp begin
      CLKCONST_EXPRESSION(INTEGER_CLOCK(e1, e2))  => begin
        @assign (e3, arg) = mapFold(e1, func, arg)
        @assign (e4, arg) = mapFold(e2, func, arg)
        if referenceEq(e1, e3) && referenceEq(e2, e4)
          exp
        else
          CLKCONST_EXPRESSION(INTEGER_CLOCK(e3, e4))
        end
      end

      CLKCONST_EXPRESSION(REAL_CLOCK(e1))  => begin
        @assign (e2, arg) = mapFold(e1, func, arg)
        if referenceEq(e1, e2)
          exp
        else
          CLKCONST_EXPRESSION(REAL_CLOCK(e2))
        end
      end

      CLKCONST_EXPRESSION(BOOLEAN_CLOCK(e1, e2))  => begin
        @assign (e3, arg) = mapFold(e1, func, arg)
        @assign (e4, arg) = mapFold(e2, func, arg)
        if referenceEq(e1, e3) && referenceEq(e2, e4)
          exp
        else
          CLKCONST_EXPRESSION(BOOLEAN_CLOCK(e3, e4))
        end
      end

      CLKCONST_EXPRESSION(SOLVER_CLOCK(e1, e2))  => begin
        @assign (e3, arg) = mapFold(e1, func, arg)
        @assign (e4, arg) = mapFold(e2, func, arg)
        if referenceEq(e1, e3) && referenceEq(e2, e4)
          exp
        else
          CLKCONST_EXPRESSION(SOLVER_CLOCK(e3, e4))
        end
      end

      CREF(__)  => begin
        @assign (cr, arg) = mapFoldCref(exp.cref, func, arg)
        if referenceEq(exp.cref, cr)
          exp
        else
          CREF(exp.ty, cr)
        end
      end

      ARRAY_EXPRESSION(__)  => begin
        @assign (expl, arg) = ListUtil.map1Fold(exp.elements, mapFold, func, arg)
        ARRAY_EXPRESSION(exp.ty, expl, exp.literal)
      end

      RANGE(step = SOME(e2))  => begin
        @assign (e1, arg) = mapFold(exp.start, func, arg)
        @assign (e4, arg) = mapFold(e2, func, arg)
        @assign (e3, arg) = mapFold(exp.stop, func, arg)
        if referenceEq(exp.start, e1) && referenceEq(e2, e4) && referenceEq(exp.stop, e3)
          exp
        else
          RANGE_EXPRESSION(exp.ty, e1, SOME(e4), e3)
        end
      end

      RANGE_EXPRESSION(__)  => begin
        @assign (e1, arg) = mapFold(exp.start, func, arg)
        @assign (e3, arg) = mapFold(exp.stop, func, arg)
        if referenceEq(exp.start, e1) && referenceEq(exp.stop, e3)
          exp
        else
          RANGE_EXPRESSION(exp.ty, e1, NONE(), e3)
        end
      end

      TUPLE_EXPRESSION(__)  => begin
        @assign (expl, arg) = ListUtil.map1Fold(exp.elements, mapFold, func, arg)
        TUPLE_EXPRESSION(exp.ty, expl)
      end

      RECORD_EXPRESSION(__)  => begin
        @assign (expl, arg) = ListUtil.map1Fold(exp.elements, mapFold, func, arg)
        RECORD_EXPRESSION(exp.path, exp.ty, expl)
      end

      CALL_EXPRESSION(__)  => begin
        @assign (call, arg) = mapFoldCall(exp.call, func, arg)
        if referenceEq(exp.call, call)
          exp
        else
          CALL_EXPRESSION(call)
        end
      end

      SIZE_EXPRESSION(dimIndex = SOME(e2))  => begin
        @assign (e1, arg) = mapFold(exp.exp, func, arg)
        @assign (e3, arg) = mapFold(e2, func, arg)
        if referenceEq(exp.exp, e1) && referenceEq(e2, e3)
          exp
        else
          SIZE_EXPRESSION(e1, SOME(e3))
        end
      end

      SIZE_EXPRESSION(__)  => begin
        @assign (e1, arg) = mapFold(exp.exp, func, arg)
        if referenceEq(exp.exp, e1)
          exp
        else
          SIZE_EXPRESSION(e1, NONE())
        end
      end

      BINARY_EXPRESSION(__)  => begin
        @assign (e1, arg) = mapFold(exp.exp1, func, arg)
        @assign (e2, arg) = mapFold(exp.exp2, func, arg)
        if referenceEq(exp.exp1, e1) && referenceEq(exp.exp2, e2)
          exp
        else
          BINARY_EXPRESSION(e1, exp.operator, e2)
        end
      end

      UNARY(__)  => begin
        @assign (e1, arg) = mapFold(exp.exp, func, arg)
        if referenceEq(exp.exp, e1)
          exp
        else
          UNARY(exp.operator, e1)
        end
      end

      LBINARY_EXPRESSION(__)  => begin
        @assign (e1, arg) = mapFold(exp.exp1, func, arg)
        @assign (e2, arg) = mapFold(exp.exp2, func, arg)
        if referenceEq(exp.exp1, e1) && referenceEq(exp.exp2, e2)
          exp
        else
          LBINARY_EXPRESSION(e1, exp.operator, e2)
        end
      end

      LUNARY(__)  => begin
        @assign (e1, arg) = mapFold(exp.exp, func, arg)
        if referenceEq(exp.exp, e1)
          exp
        else
          LUNARY(exp.operator, e1)
        end
      end

      RELATION_EXPRESSION(__)  => begin
        @assign (e1, arg) = mapFold(exp.exp1, func, arg)
        @assign (e2, arg) = mapFold(exp.exp2, func, arg)
        if referenceEq(exp.exp1, e1) && referenceEq(exp.exp2, e2)
          exp
        else
          RELATION_EXPRESSION(e1, exp.operator, e2)
        end
      end

      IF_EXPRESSION(__)  => begin
        @assign (e1, arg) = mapFold(exp.condition, func, arg)
        @assign (e2, arg) = mapFold(exp.trueBranch, func, arg)
        @assign (e3, arg) = mapFold(exp.falseBranch, func, arg)
        if referenceEq(exp.condition, e1) && referenceEq(exp.trueBranch, e2) && referenceEq(exp.falseBranch, e3)
          exp
        else
          IF_EXPRESSION(e1, e2, e3)
        end
      end

      CAST_EXPRESSION(__)  => begin
        @assign (e1, arg) = mapFold(exp.exp, func, arg)
        if referenceEq(exp.exp, e1)
          exp
        else
          CAST_EXPRESSION(exp.ty, e1)
        end
      end

UNBOX_EXPRESSION(__)  => begin
  @assign (e1, arg) = mapFold(exp.exp, func, arg)
  if referenceEq(exp.exp, e1)
    exp
  else
    UNBOX_EXPRESSION(e1, exp.ty)
  end
end

SUBSCRIPTED_EXP_EXPRESSION(__)  => begin
  @assign (e1, arg) = mapFold(exp.exp, func, arg)
  @assign (subs, arg) = ListUtil.mapFold(exp.subscripts, (func) -> mapFoldExp(func = func), arg)
  SUBSCRIPTED_EXP_EXPRESSION(e1, subs, exp.ty)
end

TUPLE_ELEMENT_EXPRESSION(__)  => begin
  @assign (e1, arg) = mapFold(exp.tupleExp, func, arg)
  if referenceEq(exp.tupleExp, e1)
    exp
  else
    TUPLE_ELEMENT_EXPRESSION(e1, exp.index, exp.ty)
  end
end

RECORD_ELEMENT_EXPRESSION(__)  => begin
  @assign (e1, arg) = mapFold(exp.recordExp, func, arg)
  if referenceEq(exp.recordExp, e1)
    exp
  else
    RECORD_ELEMENT_EXPRESSION(e1, exp.index, exp.fieldName, exp.ty)
  end
end

MUTABLE_EXPRESSION(__)  => begin
  @assign (e1, arg) = mapFold(P_Pointer.access(exp.exp), func, arg)
  P_Pointer.update(exp.exp, e1)
  exp
end

PARTIAL_FUNCTION_APPLICATION_EXPRESSION(__)  => begin
  @assign (expl, arg) = ListUtil.map1Fold(exp.args, mapFold, func, arg)
  @assign exp.args = expl
  exp
end

BINDING_EXP(__)  => begin
  @assign (e1, arg) = mapFold(exp.exp, func, arg)
  if referenceEq(exp.exp, e1)
    exp
  else
    BINDING_EXP(e1, exp.expType, exp.bindingType, exp.parents, exp.isEach)
  end
end

_  => begin
  exp
end
end
end
@assign (outExp, arg) = func(outExp, arg)
(outExp, arg)
end

function applyCrefSubscript(subscript::Subscript, func::ApplyFunc)
  @assign () = begin
    @match subscript begin
      SUBSCRIPT_UNTYPED(__)  => begin
        apply(subscript.exp, func)
        ()
      end

      SUBSCRIPT_INDEX(__)  => begin
        apply(subscript.index, func)
        ()
      end

      SUBSCRIPT_SLICE(__)  => begin
        apply(subscript.slice, func)
        ()
      end

      SUBSCRIPT_WHOLE(__)  => begin
        ()
      end
    end
  end
end

function applyCref(cref::ComponentRef, func::ApplyFunc)
  @assign () = begin
    @match cref begin
      CREF(origin = Origin.CREF)  => begin
        for s in cref.subscripts
          applyCrefSubscript(s, func)
        end
        applyCref(cref.restCref, func)
        ()
      end

      _  => begin
        ()
      end
    end
  end
end

function applyCall(call::Call, func::ApplyFunc)
  @assign () = begin
    local e::Expression
    @match call begin
      P_Call.UNTYPED_CALL(__)  => begin
        applyList(call.arguments, func)
        for arg in call.named_args
          @assign (_, e) = arg
          apply(e, func)
        end
        ()
      end

      P_Call.ARG_TYPED_CALL(__)  => begin
        for arg in call.arguments
          @assign (e, _, _) = arg
          apply(e, func)
        end
        for arg in call.named_args
          @assign (_, e, _, _) = arg
          apply(e, func)
        end
        ()
      end

      P_Call.TYPED_CALL(__)  => begin
        applyList(call.arguments, func)
        ()
      end

      P_Call.UNTYPED_ARRAY_CONSTRUCTOR(__)  => begin
        apply(call.exp, func)
        for i in call.iters
          apply(Util.tuple22(i), func)
        end
        ()
      end

      P_Call.TYPED_ARRAY_CONSTRUCTOR(__)  => begin
        apply(call.exp, func)
        for i in call.iters
          apply(Util.tuple22(i), func)
        end
        ()
      end

      P_Call.UNTYPED_REDUCTION(__)  => begin
        apply(call.exp, func)
        for i in call.iters
          apply(Util.tuple22(i), func)
        end
        ()
      end

      P_Call.TYPED_REDUCTION(__)  => begin
        apply(call.exp, func)
        for i in call.iters
          apply(Util.tuple22(i), func)
        end
        Util.applyOption(call.defaultExp, func)
        Util.applyOption(Util.tuple31(call.foldExp), func)
        ()
      end
    end
  end
end

function apply(exp::Expression, func::ApplyFunc)
  @assign () = begin
    local e::Expression
    local e1::Expression
    local e2::Expression
    @match exp begin
      CLKCONST_EXPRESSION(INTEGER_CLOCK(e1, e2))  => begin
        apply(e1, func)
        apply(e2, func)
        ()
      end

      CLKCONST_EXPRESSION(REAL_CLOCK(e1))  => begin
        apply(e1, func)
        ()
      end

      CLKCONST_EXPRESSION(BOOLEAN_CLOCK(e1, e2))  => begin
        apply(e1, func)
        apply(e2, func)
        ()
      end

      CLKCONST_EXPRESSION(SOLVER_CLOCK(e1, e2))  => begin
        apply(e1, func)
        apply(e2, func)
        ()
      end

      CREF(__)  => begin
        applyCref(exp.cref, func)
        ()
      end

      ARRAY_EXPRESSION(__)  => begin
        applyList(exp.elements, func)
        ()
      end

      MATRIX_EXPRESSION(__)  => begin
        for row in exp.elements
          applyList(row, func)
        end
        ()
      end

      RANGE_EXPRESSION(step = SOME(e))  => begin
        apply(exp.start, func)
        apply(e, func)
        apply(exp.stop, func)
        ()
      end

      RANGE_EXPRESSION(__)  => begin
        apply(exp.start, func)
        apply(exp.stop, func)
        ()
      end

      TUPLE_EXPRESSION(__)  => begin
        applyList(exp.elements, func)
        ()
      end

      RECORD_EXPRESSION(__)  => begin
        applyList(exp.elements, func)
        ()
      end

      CALL_EXPRESSION(__)  => begin
        applyCall(exp.call, func)
        ()
      end

      SIZE_EXPRESSION(dimIndex = SOME(e))  => begin
        apply(exp.exp, func)
        apply(e, func)
        ()
      end

      SIZE_EXPRESSION(__)  => begin
        apply(exp.exp, func)
        ()
      end

      BINARY_EXPRESSION(__)  => begin
        apply(exp.exp1, func)
        apply(exp.exp2, func)
        ()
      end

      UNARY_EXPRESSION(__)  => begin
        apply(exp.exp, func)
        ()
      end

      LBINARY_EXPRESSION(__)  => begin
        apply(exp.exp1, func)
        apply(exp.exp2, func)
        ()
      end

      LUNARY_EXPRESSION(__)  => begin
        apply(exp.exp, func)
        ()
      end

      RELATION_EXPRESSION(__)  => begin
        apply(exp.exp1, func)
        apply(exp.exp2, func)
        ()
      end

      IF_EXPRESSION(__)  => begin
        apply(exp.condition, func)
        apply(exp.trueBranch, func)
        apply(exp.falseBranch, func)
        ()
      end

      CAST_EXPRESSION(__)  => begin
        apply(exp.exp, func)
        ()
      end

      UNBOX_EXPRESSION(__)  => begin
        apply(exp.exp, func)
        ()
      end

      SUBSCRIPTED_EXP_EXPRESSION(__)  => begin
        apply(exp.exp, func)
        for s in exp.subscripts
          applyExp(s, func)
        end
        ()
      end

      TUPLE_ELEMENT_EXPRESSION(__)  => begin
        apply(exp.tupleExp, func)
        ()
      end

      RECORD_ELEMENT_EXPRESSION(__)  => begin
        apply(exp.recordExp, func)
        ()
      end

      BOX_EXPRESSION(__)  => begin
        apply(exp.exp, func)
        ()
      end

      MUTABLE_EXPRESSION(__)  => begin
        apply(P_Pointer.access(exp.exp), func)
        ()
      end

      PARTIAL_FUNCTION_APPLICATION_EXPRESSION(__)  => begin
        applyList(exp.args, func)
        ()
      end

      BINDING_EXP(__)  => begin
        apply(exp.exp, func)
        ()
      end

      _  => begin
        ()
      end
    end
  end
  func(exp)
end

function applyList(expl::List{<:Expression}, func::ApplyFunc)
  for e in expl
    apply(e, func)
  end
end

function foldCref(cref::ComponentRef, func::FoldFunc, arg::ArgT)  where {ArgT}

  @assign () = begin
    @match cref begin
      COMPONENT_REF_CREF(origin = Origin.CREF)  => begin
        @assign arg = ListUtil.fold(cref.subscripts, (func) -> foldExp(func = func), arg)
        @assign arg = foldCref(cref.restCref, func, arg)
        ()
      end

      _  => begin
        ()
      end
    end
  end
  arg
end

function foldCall(call::Call, func::FoldFunc, foldArg::ArgT)  where {ArgT}


  @assign () = begin
    local e::Expression
    @match call begin
      P_Call.UNTYPED_CALL(__)  => begin
        @assign foldArg = foldList(call.arguments, func, foldArg)
        for arg in call.named_args
          @assign (_, e) = arg
          @assign foldArg = fold(e, func, foldArg)
        end
        ()
      end

      P_Call.ARG_TYPED_CALL(__)  => begin
        for arg in call.arguments
          @assign (e, _, _) = arg
          @assign foldArg = fold(e, func, foldArg)
        end
        for arg in call.named_args
          @assign (_, e, _, _) = arg
          @assign foldArg = fold(e, func, foldArg)
        end
        ()
      end

      P_Call.TYPED_CALL(__)  => begin
        @assign foldArg = foldList(call.arguments, func, foldArg)
        ()
      end

      P_Call.UNTYPED_ARRAY_CONSTRUCTOR(__)  => begin
        @assign foldArg = fold(call.exp, func, foldArg)
        for i in call.iters
          @assign foldArg = fold(Util.tuple22(i), func, foldArg)
        end
        ()
      end

      P_Call.TYPED_ARRAY_CONSTRUCTOR(__)  => begin
        @assign foldArg = fold(call.exp, func, foldArg)
        for i in call.iters
          @assign foldArg = fold(Util.tuple22(i), func, foldArg)
        end
        ()
      end

      P_Call.UNTYPED_REDUCTION(__)  => begin
        @assign foldArg = fold(call.exp, func, foldArg)
        for i in call.iters
          @assign foldArg = fold(Util.tuple22(i), func, foldArg)
        end
        ()
      end

      P_Call.TYPED_REDUCTION(__)  => begin
        @assign foldArg = fold(call.exp, func, foldArg)
        for i in call.iters
          @assign foldArg = fold(Util.tuple22(i), func, foldArg)
        end
        @assign foldArg = foldOpt(call.defaultExp, func, foldArg)
        @assign foldArg = foldOpt(Util.tuple31(call.foldExp), func, foldArg)
        ()
      end
    end
  end
  foldArg
end

function fold(exp::Expression, func::FoldFunc, arg::ArgT)  where {ArgT}
  local result::ArgT

  @assign result = begin
    local e::Expression
    local e1::Expression
    local e2::Expression
    @match exp begin
      CLKCONST_EXPRESSION(INTEGER_CLOCK(e1, e2))  => begin
        @assign result = fold(e1, func, arg)
        @assign result = fold(e2, func, result)
        result
      end

      CLKCONST_EXPRESSION(REAL_CLOCK(e1))  => begin
        @assign result = fold(e1, func, arg)
        result
      end

      CLKCONST_EXPRESSION(BOOLEAN_CLOCK(e1, e2))  => begin
        @assign result = fold(e1, func, arg)
        @assign result = fold(e2, func, result)
        result
      end

      CLKCONST_EXPRESSION(SOLVER_CLOCK(e1, e2))  => begin
        @assign result = fold(e1, func, arg)
        @assign result = fold(e2, func, result)
        result
      end

      CREF_EXPRESSION(__)  => begin
        foldCref(exp.cref, func, arg)
      end

      ARRAY_EXPRESSION(__)  => begin
        foldList(exp.elements, func, arg)
      end

      MATRIX_EXPRESSION(__)  => begin
        @assign result = arg
        for row in exp.elements
          @assign result = foldList(row, func, result)
        end
        result
      end

      RANGE_EXPRESSION(__)  => begin
        @assign result = fold(exp.start, func, arg)
        @assign result = foldOpt(exp.step, func, result)
        fold(exp.stop, func, result)
      end

      TUPLE_EXPRESSION(__)  => begin
        foldList(exp.elements, func, arg)
      end

      RECORD_EXPRESSION(__)  => begin
        foldList(exp.elements, func, arg)
      end

      CALL_EXPRESSION(__)  => begin
        foldCall(exp.call, func, arg)
      end

      SIZE_EXPRESSION(dimIndex = SOME(e))  => begin
        @assign result = fold(exp.exp, func, arg)
        fold(e, func, result)
      end

      SIZE_EXPRESSION(__)  => begin
        fold(exp.exp, func, arg)
      end

      BINARY_EXPRESSION(__)  => begin
        @assign result = fold(exp.exp1, func, arg)
        fold(exp.exp2, func, result)
      end

      UNARY_EXPRESSION(__)  => begin
        fold(exp.exp, func, arg)
      end

      LBINARY_EXPRESSION(__)  => begin
        @assign result = fold(exp.exp1, func, arg)
        fold(exp.exp2, func, result)
      end

      LUNARY_EXPRESSION(__)  => begin
        fold(exp.exp, func, arg)
      end

      RELATION_EXPRESSION(__)  => begin
        @assign result = fold(exp.exp1, func, arg)
        fold(exp.exp2, func, result)
      end

      IF_EXPRESSION(__)  => begin
        @assign result = fold(exp.condition, func, arg)
        @assign result = fold(exp.trueBranch, func, result)
        fold(exp.falseBranch, func, result)
      end

      CAST_EXPRESSION(__)  => begin
        fold(exp.exp, func, arg)
      end

      UNBOX_EXPRESSION(__)  => begin
        fold(exp.exp, func, arg)
      end

      SUBSCRIPTED_EXP_EXPRESSION(__)  => begin
        @assign result = fold(exp.exp, func, arg)
        ListUtil.fold(exp.subscripts, (func) -> foldExp(func = func), result)
      end

      TUPLE_ELEMENT_EXPRESSION(__)  => begin
        fold(exp.tupleExp, func, arg)
      end

      RECORD_ELEMENT_EXPRESSION(__)  => begin
        fold(exp.recordExp, func, arg)
      end

      BOX_EXPRESSION(__)  => begin
        fold(exp.exp, func, arg)
      end

      MUTABLE_EXPRESSION(__)  => begin
        fold(P_Pointer.access(exp.exp), func, arg)
      end

      PARTIAL_FUNCTION_APPLICATION_EXPRESSION(__)  => begin
        foldList(exp.args, func, arg)
      end

      BINDING_EXP(__)  => begin
        fold(exp.exp, func, arg)
      end

      _  => begin
        arg
      end
    end
  end
  @assign result = func(exp, result)
  result
end

function foldOpt(exp::Option{Expression}, func::FoldFunc, arg::ArgT)  where {ArgT}
  local result::ArgT

  @assign result = begin
    local e::Expression
    @match exp begin
      SOME(e)  => begin
        func(e, arg)
      end

      _  => begin
        arg
      end
    end
  end
  result
end

function foldList(expl::List{Expression}, func::FoldFunc, arg::ArgT)  where {ArgT}
  local result::ArgT = arg

  for e in expl
    @assign result = fold(e, func, result)
  end
  result
end

""" #= Applies the given function to each scalar elements of an array. =#"""
function mapArrayElements(exp::Expression, func::MapFunc) ::Expression
  local outExp::Expression

  @assign outExp = begin
    @match exp begin
      ARRAY_EXPRESSION(__)  => begin
        @assign exp.elements = list(mapArrayElements(e, func) for e in exp.elements)
        exp
      end

      _  => begin
        func(exp)
      end
    end
  end
  outExp
end

function mapCallShallowIterators(iters::List{<:Tuple{<:InstNode, Expression}}, func::MapFunc) ::List{Tuple{InstNode, Expression}}
  local outIters::List{Tuple{InstNode, Expression}} = nil

  local node::InstNode
  local exp::Expression
  local new_exp::Expression

  for i in iters
    @assign (node, exp) = i
    @assign new_exp = func(exp)
    @assign outIters = _cons(if referenceEq(new_exp, exp)
                             i
                             else
                             (node, new_exp)
                             end, outIters)
  end
  @assign outIters = listReverseInPlace(outIters)
  outIters
end

function mapCallShallow(call::Call, func::MapFunc) ::Call
  local outCall::Call

  @assign outCall = begin
    local args::List{Expression}
    local nargs::List{P_Function.NamedArg}
    local targs::List{P_Function.TypedArg}
    local tnargs::List{P_Function.TypedNamedArg}
    local s::String
    local e::Expression
    local t::M_Type
    local v::VariabilityType
    local iters::List{Tuple{InstNode, Expression}}
    local default_exp::Option{Expression}
    local fold_exp::Tuple{Option{Expression}, String, String}
    @match call begin
      P_Call.UNTYPED_CALL(__)  => begin
        @assign args = list(func(arg) for arg in call.arguments)
        @assign nargs = nil
        for arg in call.named_args
          @assign (s, e) = arg
          @assign e = func(e)
          @assign nargs = _cons((s, e), nargs)
        end
        P_Call.UNTYPED_CALL(call.ref, args, listReverse(nargs), call.call_scope)
      end

      P_Call.ARG_TYPED_CALL(__)  => begin
        @assign targs = nil
        @assign tnargs = nil
        for arg in call.arguments
          @assign (e, t, v) = arg
          @assign e = func(e)
          @assign targs = _cons((e, t, v), targs)
        end
        for arg in call.named_args
          @assign (s, e, t, v) = arg
          @assign e = func(e)
          @assign tnargs = _cons((s, e, t, v), tnargs)
        end
        P_Call.ARG_TYPED_CALL(call.ref, listReverse(targs), listReverse(tnargs), call.call_scope)
      end

      P_Call.TYPED_CALL(__)  => begin
        @assign args = list(func(arg) for arg in call.arguments)
        P_Call.TYPED_CALL(call.fn, call.ty, call.var, args, call.attributes)
      end

      P_Call.UNTYPED_ARRAY_CONSTRUCTOR(__)  => begin
        @assign e = func(call.exp)
        P_Call.UNTYPED_ARRAY_CONSTRUCTOR(e, call.iters)
      end

      P_Call.TYPED_ARRAY_CONSTRUCTOR(__)  => begin
        @assign e = func(call.exp)
        P_Call.TYPED_ARRAY_CONSTRUCTOR(call.ty, call.var, e, call.iters)
      end

      P_Call.UNTYPED_REDUCTION(__)  => begin
        @assign e = func(call.exp)
        P_Call.UNTYPED_REDUCTION(call.ref, e, call.iters)
      end

      P_Call.TYPED_REDUCTION(__)  => begin
        @assign e = func(call.exp)
        @assign iters = mapCallShallowIterators(call.iters, func)
        @assign default_exp = mapShallowOpt(call.defaultExp, func)
        @assign fold_exp = Util.applyTuple31(call.foldExp, (func) -> mapShallowOpt(func = func))
        P_Call.TYPED_REDUCTION(call.fn, call.ty, call.var, e, iters, default_exp, fold_exp)
      end
    end
  end
  outCall
end

function mapCrefShallow(cref::ComponentRef, func::MapFunc) ::ComponentRef
  local outCref::ComponentRef

  @assign outCref = begin
    local subs::List{Subscript}
    local rest::ComponentRef
    @match cref begin
      COMPONENT_REF_CREF(origin = Origin.CREF)  => begin
        @assign subs = list(mapShallowExp(s, func) for s in cref.subscripts)
        @assign rest = mapCref(cref.restCref, func)
        COMPONENT_REF_CREF(cref.node, subs, cref.ty, cref.origin, rest)
      end

      _  => begin
        cref
      end
    end
  end
  outCref
end

function mapShallowOpt(exp::Option{<:Expression}, func::MapFunc) ::Option{Expression}
  local outExp::Option{Expression}

  local e::Expression

  @assign outExp = begin
    @match exp begin
      SOME(e)  => begin
        SOME(func(e))
      end

      _  => begin
        exp
      end
    end
  end
  outExp
end

function mapShallow(exp::Expression, func::MapFunc) ::Expression
  local outExp::Expression

  @assign outExp = begin
    local e1::Expression
    local e2::Expression
    local e3::Expression
    local e4::Expression
    @match exp begin
      CLKCONST_EXPRESSION(INTEGER_CLOCK(e1, e2))  => begin
        @assign e3 = func(e1)
        @assign e4 = func(e2)
        if referenceEq(e1, e3) && referenceEq(e2, e4)
          exp
        else
          CLKCONST_EXPRESSION(INTEGER_CLOCK(e3, e4))
        end
      end

      CLKCONST_EXPRESSION(REAL_CLOCK(e1))  => begin
        @assign e2 = func(e1)
        if referenceEq(e1, e2)
          exp
        else
          CLKCONST_EXPRESSION(REAL_CLOCK(e2))
        end
      end

      CLKCONST_EXPRESSION(BOOLEAN_CLOCK(e1, e2))  => begin
        @assign e3 = func(e1)
        @assign e4 = func(e2)
        if referenceEq(e1, e3) && referenceEq(e2, e4)
          exp
        else
          CLKCONST_EXPRESSION(BOOLEAN_CLOCK(e3, e4))
        end
      end

      CLKCONST_EXPRESSION(SOLVER_CLOCK(e1, e2))  => begin
        @assign e3 = func(e1)
        @assign e4 = func(e2)
        if referenceEq(e1, e3) && referenceEq(e2, e4)
          exp
        else
          CLKCONST_EXPRESSION(SOLVER_CLOCK(e3, e4))
        end
      end

      CREF_EXPRESSION(__)  => begin
        CREF_EXPRESSION(exp.ty, mapCrefShallow(exp.cref, func))
      end

      ARRAY_EXPRESSION(__)  => begin
        ARRAY_EXPRESSION(exp.ty, list(func(e) for e in exp.elements), exp.literal)
      end

      MATRIX_EXPRESSION(__)  => begin
        MATRIX_EXPRESSION(list(list(func(e) for e in row) for row in exp.elements))
      end

      RANGE_EXPRESSION(step = SOME(e2))  => begin
        @assign e1 = func(exp.start)
        @assign e4 = func(e2)
        @assign e3 = func(exp.stop)
        if referenceEq(exp.start, e1) && referenceEq(e2, e4) && referenceEq(exp.stop, e3)
          exp
        else
          RANGE_EXPRESSION(exp.ty, e1, SOME(e4), e3)
        end
      end

      RANGE_EXPRESSION(__)  => begin
        @assign e1 = func(exp.start)
        @assign e3 = func(exp.stop)
        if referenceEq(exp.start, e1) && referenceEq(exp.stop, e3)
          exp
        else
          RANGE_EXPRESSION(exp.ty, e1, NONE(), e3)
        end
      end

      TUPLE_EXPRESSION(__)  => begin
        TUPLE_EXPRESSION(exp.ty, list(func(e) for e in exp.elements))
      end

      RECORD_EXPRESSION(__)  => begin
        RECORD_EXPRESSION(exp.path, exp.ty, list(func(e) for e in exp.elements))
      end

      CALL_EXPRESSION(__)  => begin
        CALL_EXPRESSION(mapCallShallow(exp.call, func))
      end

      SIZE_EXPRESSION(dimIndex = SOME(e2))  => begin
        @assign e1 = func(exp.exp)
        @assign e3 = func(e2)
        if referenceEq(exp.exp, e1) && referenceEq(e2, e3)
          exp
        else
          SIZE_EXPRESSION(e1, SOME(e3))
        end
      end

      SIZE_EXPRESSION(__)  => begin
        @assign e1 = func(exp.exp)
        if referenceEq(exp.exp, e1)
          exp
        else
          SIZE_EXPRESSION(e1, NONE())
        end
      end

      BINARY_EXPRESSION(__)  => begin
        @assign e1 = func(exp.exp1)
        @assign e2 = func(exp.exp2)
        if referenceEq(exp.exp1, e1) && referenceEq(exp.exp2, e2)
          exp
        else
          BINARY_EXPRESSION(e1, exp.operator, e2)
        end
      end

      UNARY_EXPRESSION(__)  => begin
        @assign e1 = func(exp.exp)
        if referenceEq(exp.exp, e1)
          exp
        else
          UNARY_EXPRESSION(exp.operator, e1)
        end
      end

      LBINARY_EXPRESSION(__)  => begin
        @assign e1 = func(exp.exp1)
        @assign e2 = func(exp.exp2)
        if referenceEq(exp.exp1, e1) && referenceEq(exp.exp2, e2)
          exp
        else
          LBINARY_EXPRESSION(e1, exp.operator, e2)
        end
      end

      LUNARY_EXPRESSION(__)  => begin
        @assign e1 = func(exp.exp)
        if referenceEq(exp.exp, e1)
          exp
        else
          LUNARY_EXPRESSION(exp.operator, e1)
        end
      end

      RELATION_EXPRESSION(__)  => begin
        @assign e1 = func(exp.exp1)
        @assign e2 = func(exp.exp2)
        if referenceEq(exp.exp1, e1) && referenceEq(exp.exp2, e2)
          exp
        else
          RELATION_EXPRESSION(e1, exp.operator, e2)
        end
      end

      IF_EXPRESSION(__)  => begin
        @assign e1 = func(exp.condition)
        @assign e2 = func(exp.trueBranch)
        @assign e3 = func(exp.falseBranch)
        if referenceEq(exp.condition, e1) && referenceEq(exp.trueBranch, e2) && referenceEq(exp.falseBranch, e3)
          exp
        else
          IF_EXPRESSION(e1, e2, e3)
        end
      end

      CAST_EXPRESSION(__)  => begin
        @assign e1 = func(exp.exp)
        if referenceEq(exp.exp, e1)
          exp
        else
          CAST_EXPRESSION(exp.ty, e1)
        end
      end

      UNBOX_EXPRESSION(__)  => begin
        @assign e1 = func(exp.exp)
        if referenceEq(exp.exp, e1)
          exp
        else
          UNBOX_EXPRESSION(e1, exp.ty)
        end
      end

      SUBSCRIPTED_EXP_EXPRESSION(__)  => begin
        SUBSCRIPTED_EXP_EXPRESSION(func(exp.exp), list(mapShallowExp(e, func) for e in exp.subscripts), exp.ty)
      end

      TUPLE_ELEMENT_EXPRESSION(__)  => begin
        @assign e1 = func(exp.tupleExp)
        if referenceEq(exp.tupleExp, e1)
          exp
        else
          TUPLE_ELEMENT_EXPRESSION(e1, exp.index, exp.ty)
        end
      end

      RECORD_ELEMENT_EXPRESSION(__)  => begin
        @assign e1 = func(exp.recordExp)
        if referenceEq(exp.recordExp, e1)
          exp
        else
          RECORD_ELEMENT_EXPRESSION(e1, exp.index, exp.fieldName, exp.ty)
        end
      end

BOX_EXPRESSION(__)  => begin
  @assign e1 = func(exp.exp)
  if referenceEq(exp.exp, e1)
    exp
  else
    BOX_EXPRESSION(e1)
  end
end

MUTABLE_EXPRESSION(__)  => begin
  P_Pointer.update(exp.exp, func(P_Pointer.access(exp.exp)))
  exp
end

PARTIAL_FUNCTION_APPLICATION_EXPRESSION(__)  => begin
  @assign exp.args = list(func(e) for e in exp.args)
  exp
end

BINDING_EXP(__)  => begin
  @assign e1 = func(exp.exp)
  if referenceEq(exp.exp, e1)
    exp
  else
    BINDING_EXP(e1, exp.expType, exp.bindingType, exp.parents, exp.isEach)
  end
end
_  => begin
  exp
end
end
end
outExp
end

function mapCref(cref::ComponentRef, func::MapFunc) ::ComponentRef
  local outCref::ComponentRef
  @assign outCref = begin
    local subs::List{Subscript}
    local rest::ComponentRef
    @match cref begin
      COMPONENT_REF_CREF(origin = Origin.CREF)  => begin
        @assign subs = list(mapExp(s, func) for s in cref.subscripts)
        @assign rest = mapCref(cref.restCref, func)
        COMPONENT_REF_CREF(cref.node, subs, cref.ty, cref.origin, rest)
      end
      _  => begin
        cref
      end
    end
  end
  outCref
end

function mapCallIterators(iters::List{<:Tuple{<:InstNode, Expression}}, func::MapFunc) ::List{Tuple{InstNode, Expression}}
  local outIters::List{Tuple{InstNode, Expression}} = nil

  local node::InstNode
  local exp::Expression
  local new_exp::Expression

  for i in iters
    @assign (node, exp) = i
    @assign new_exp = map(exp, func)
    @assign outIters = _cons(if referenceEq(new_exp, exp)
                             i
                             else
                             (node, new_exp)
                             end, outIters)
  end
  @assign outIters = listReverseInPlace(outIters)
  outIters
end

function mapCall(call::Call, func::MapFunc) ::Call
  local outCall::Call
  @assign outCall = begin
    local args::List{Expression}
    local nargs::List{P_Function.NamedArg}
    local targs::List{P_Function.TypedArg}
    local tnargs::List{P_Function.TypedNamedArg}
    local s::String
    local e::Expression
    local t::M_Type
    local v::VariabilityType
    local iters::List{Tuple{InstNode, Expression}}
    local default_exp::Option{Expression}
    local fold_exp::Tuple{Option{Expression}, String, String}
    @match call begin
      P_Call.UNTYPED_CALL(__)  => begin
        @assign args = list(map(arg, func) for arg in call.arguments)
        @assign nargs = nil
        for arg in call.named_args
          @assign (s, e) = arg
          @assign e = map(e, func)
          @assign nargs = _cons((s, e), nargs)
        end
        P_Call.UNTYPED_CALL(call.ref, args, listReverse(nargs), call.call_scope)
      end

      P_Call.ARG_TYPED_CALL(__)  => begin
        @assign targs = nil
        @assign tnargs = nil
        for arg in call.arguments
          @assign (e, t, v) = arg
          @assign e = map(e, func)
          @assign targs = _cons((e, t, v), targs)
        end
        for arg in call.named_args
          @assign (s, e, t, v) = arg
          @assign e = map(e, func)
          @assign tnargs = _cons((s, e, t, v), tnargs)
        end
        P_Call.ARG_TYPED_CALL(call.ref, listReverse(targs), listReverse(tnargs), call.call_scope)
      end

      P_Call.TYPED_CALL(__)  => begin
        @assign args = list(map(arg, func) for arg in call.arguments)
        P_Call.TYPED_CALL(call.fn, call.ty, call.var, args, call.attributes)
      end

      P_Call.UNTYPED_ARRAY_CONSTRUCTOR(__)  => begin
        @assign e = map(call.exp, func)
        @assign iters = mapCallIterators(call.iters, func)
        P_Call.UNTYPED_ARRAY_CONSTRUCTOR(e, iters)
      end

      P_Call.TYPED_ARRAY_CONSTRUCTOR(__)  => begin
        @assign e = map(call.exp, func)
        @assign iters = mapCallIterators(call.iters, func)
        P_Call.TYPED_ARRAY_CONSTRUCTOR(call.ty, call.var, e, iters)
      end

      P_Call.UNTYPED_REDUCTION(__)  => begin
        @assign e = map(call.exp, func)
        @assign iters = mapCallIterators(call.iters, func)
        P_Call.UNTYPED_REDUCTION(call.ref, e, iters)
      end

      P_Call.TYPED_REDUCTION(__)  => begin
        @assign e = map(call.exp, func)
        @assign iters = mapCallIterators(call.iters, func)
        @assign default_exp = mapOpt(call.defaultExp, func)
        @assign fold_exp = Util.applyTuple31(call.foldExp, (func) -> mapOpt(func = func))
        P_Call.TYPED_REDUCTION(call.fn, call.ty, call.var, e, iters, default_exp, fold_exp)
      end
    end
  end
  outCall
end

function mapOpt(exp::Option{<:Expression}, func::MapFunc) ::Option{Expression}
  local outExp::Option{Expression}

  local e::Expression

  @assign outExp = begin
    @match exp begin
      SOME(e)  => begin
        SOME(map(e, func))
      end

      _  => begin
        exp
      end
    end
  end
  outExp
end

function map(exp::Expression, func::MapFunc) ::Expression
  local outExp::Expression
  @assign outExp = begin
    local e1::Expression
    local e2::Expression
    local e3::Expression
    local e4::Expression
    @match exp begin
      CLKCONST_EXPRESSION(INTEGER_CLOCK(e1, e2))  => begin
        @assign e3 = map(e1, func)
        @assign e4 = map(e2, func)
        if referenceEq(e1, e3) && referenceEq(e2, e4)
          exp
        else
          CLKCONST_EXPRESSION(INTEGER_CLOCK(e3, e4))
        end
      end

      CLKCONST_EXPRESSION(REAL_CLOCK(e1))  => begin
        @assign e2 = map(e1, func)
        if referenceEq(e1, e2)
          exp
        else
          CLKCONST_EXPRESSION(REAL_CLOCK(e2))
        end
      end
      CLKCONST_EXPRESSION(BOOLEAN_CLOCK(e1, e2))  => begin
        @assign e3 = map(e1, func)
        @assign e4 = map(e2, func)
        if referenceEq(e1, e3) && referenceEq(e2, e4)
          exp
        else
          CLKCONST_EXPRESSION(BOOLEAN_CLOCK(e3, e4))
        end
      end
      CLKCONST_EXPRESSION(SOLVER_CLOCK(e1, e2))  => begin
        @assign e3 = map(e1, func)
        @assign e4 = map(e2, func)
        if referenceEq(e1, e3) && referenceEq(e2, e4)
          exp
        else
          CLKCONST_EXPRESSION(SOLVER_CLOCK(e3, e4))
        end
      end
      CREF_EXPRESSION(__)  => begin
        CREF_EXPRESSION(exp.ty, mapCref(exp.cref, func))
      end

      ARRAY_EXPRESSION(__)  => begin
        ARRAY(exp.ty, list(map(e, func) for e in exp.elements), exp.literal)
      end

      MATRIX_EXPRESSION(__)  => begin
        MATRIX_EXPRESSION(list(list(map(e, func) for e in row) for row in exp.elements))
      end

      RANGE_EXPRESSION(step = SOME(e2))  => begin
        @assign e1 = map(exp.start, func)
        @assign e4 = map(e2, func)
        @assign e3 = map(exp.stop, func)
        if referenceEq(exp.start, e1) && referenceEq(e2, e4) && referenceEq(exp.stop, e3)
          exp
        else
          RANGE_EXPRESSION(exp.ty, e1, SOME(e4), e3)
        end
      end

      RANGE_EXPRESSION(__)  => begin
        @assign e1 = map(exp.start, func)
        @assign e3 = map(exp.stop, func)
        if referenceEq(exp.start, e1) && referenceEq(exp.stop, e3)
          exp
        else
          RANGE_EXPRESSION(exp.ty, e1, NONE(), e3)
        end
      end

      TUPLE_EXPRESSION(__)  => begin
        TUPLE_EXPRESSION(exp.ty, list(map(e, func) for e in exp.elements))
      end

      RECORD_EXPRESSION(__)  => begin
        RECORD_EXPRESSION(exp.path, exp.ty, list(map(e, func) for e in exp.elements))
      end

      CALL_EXPRESSION(__)  => begin
        CALL_EXPRESSION(mapCall(exp.call, func))
      end

      SIZE_EXPRESSION(dimIndex = SOME(e2))  => begin
        @assign e1 = map(exp.exp, func)
        @assign e3 = map(e2, func)
        if referenceEq(exp.exp, e1) && referenceEq(e2, e3)
          exp
        else
          SIZE_EXPRESSION(e1, SOME(e3))
        end
      end

      SIZE_EXPRESSION(__)  => begin
        @assign e1 = map(exp.exp, func)
        if referenceEq(exp.exp, e1)
          exp
        else
          SIZE_EXPRESSION(e1, NONE())
        end
      end

      BINARY_EXPRESSION(__)  => begin
        @assign e1 = map(exp.exp1, func)
        @assign e2 = map(exp.exp2, func)
        if referenceEq(exp.exp1, e1) && referenceEq(exp.exp2, e2)
          exp
        else
          BINARY_EXPRESSION(e1, exp.operator, e2)
        end
      end

      UNARY_EXPRESSION(__)  => begin
        @assign e1 = map(exp.exp, func)
        if referenceEq(exp.exp, e1)
          exp
        else
          UNARY_EXPRESSION(exp.operator, e1)
        end
      end

      LBINARY_EXPRESSION(__)  => begin
        @assign e1 = map(exp.exp1, func)
        @assign e2 = map(exp.exp2, func)
        if referenceEq(exp.exp1, e1) && referenceEq(exp.exp2, e2)
          exp
        else
          LBINARY_EXPRESSION(e1, exp.operator, e2)
        end
      end

      LUNARY_EXPRESSION(__)  => begin
        @assign e1 = map(exp.exp, func)
        if referenceEq(exp.exp, e1)
          exp
        else
          LUNARY_EXPRESSION(exp.operator, e1)
        end
      end

      RELATION_EXPRESSION(__)  => begin
        @assign e1 = map(exp.exp1, func)
        @assign e2 = map(exp.exp2, func)
        if referenceEq(exp.exp1, e1) && referenceEq(exp.exp2, e2)
          exp
        else
          RELATION_EXPRESSION(e1, exp.operator, e2)
        end
      end

      IF_EXPRESSION(__)  => begin
        @assign e1 = map(exp.condition, func)
        @assign e2 = map(exp.trueBranch, func)
        @assign e3 = map(exp.falseBranch, func)
        if referenceEq(exp.condition, e1) && referenceEq(exp.trueBranch, e2) && referenceEq(exp.falseBranch, e3)
          exp
        else
          IF_EXPRESSION(e1, e2, e3)
        end
      end

      CAST_EXPRESSION(__)  => begin
        @assign e1 = map(exp.exp, func)
        if referenceEq(exp.exp, e1)
          exp
        else
          CAST_EXPRESSION(exp.ty, e1)
        end
      end

      UNBOX_EXPRESSION(__)  => begin
        @assign e1 = map(exp.exp, func)
        if referenceEq(exp.exp, e1)
          exp
        else
          UNBOX_EXPRESSION(e1, exp.ty)
        end
      end

      SUBSCRIPTED_EXP_EXPRESSION(__)  => begin
        SUBSCRIPTED_EXP_EXPRESSION(map(exp.exp, func), list(mapExp(s, func) for s in exp.subscripts), exp.ty)
      end

      TUPLE_ELEMENT_EXPRESSION(__)  => begin
        @assign e1 = map(exp.tupleExp, func)
        if referenceEq(exp.tupleExp, e1)
          exp
        else
          TUPLE_ELEMENT_EXPRESSION(e1, exp.index, exp.ty)
        end
      end

      RECORD_ELEMENT_EXPRESSION(__)  => begin
        @assign e1 = map(exp.recordExp, func)
        if referenceEq(exp.recordExp, e1)
          exp
        else
          RECORD_ELEMENT_EXPRESSION(e1, exp.index, exp.fieldName, exp.ty)
        end
      end

BOX_EXPRESSION(__)  => begin
  @assign e1 = map(exp.exp, func)
  if referenceEq(exp.exp, e1)
    exp
  else
    BOX_EXPRESSION(e1)
  end
end

MUTABLE_EXPRESSION(__)  => begin
  P_Pointer.update(exp.exp, map(P_Pointer.access(exp.exp), func))
  exp
end

PARTIAL_FUNCTION_APPLICATION_EXPRESSION(__)  => begin
  @assign exp.args = list(map(e, func) for e in exp.args)
  exp
end

BINDING_EXP(__)  => begin
  @assign e1 = map(exp.exp, func)
  if referenceEq(exp.exp, e1)
    exp
  else
    BINDING_EXP(e1, exp.expType, exp.bindingType, exp.parents, exp.isEach)
  end
end
_  => begin
  exp
end
end
end
@assign outExp = func(outExp)
outExp
end

function dimensionCount(exp::Expression) ::Integer
  local dimCount::Integer
  @assign dimCount = begin
    @match exp begin
      ARRAY_EXPRESSION(ty = TYPE_UNKNOWN(__))  => begin
        1 + dimensionCount(listHead(exp.elements))
      end

      ARRAY_EXPRESSION(__)  => begin
        Type.dimensionCount(exp.ty)
      end

      RANGE_EXPRESSION(__)  => begin
        Type.dimensionCount(exp.ty)
      end

      SIZE_EXPRESSION(dimIndex = NONE())  => begin
        dimensionCount(exp.exp)
      end

      CAST_EXPRESSION(__)  => begin
        dimensionCount(exp.exp)
      end

      SUBSCRIPTED_EXP_EXPRESSION(__)  => begin
        Type.dimensionCount(exp.ty)
      end

      TUPLE_ELEMENT_EXPRESSION(__)  => begin
        Type.dimensionCount(exp.ty)
      end

      _  => begin
        0
      end
    end
  end
  #=  TODO: Add more expressions.
  =#
  dimCount
end

function toDAEValueRecord(ty::M_Type, path::Absyn.Path, args::List{<:Expression}) ::Values.Value
  local value::Values.Value

  local field_names::List{String} = nil
  local arg::Expression
  local rest_args::List{Expression} = args
  local values::List{Values.Value} = nil

  for field in Type.recordFields(ty)
    @match _cons(arg, rest_args) = rest_args
    @assign () = begin
      @match field begin
        Record.P_Field.INPUT(__)  => begin
          @assign field_names = _cons(field.name, field_names)
          @assign values = _cons(toDAEValue(arg), values)
          ()
        end

        _  => begin
          ()
        end
      end
    end
  end
  @assign field_names = listReverseInPlace(field_names)
  @assign values = listReverseInPlace(values)
  @assign value = Values.RECORD(path, values, field_names, -1)
  value
end

function toDAEValue(exp::Expression) ::Values.Value
  local value::Values.Value

  @assign value = begin
    local ty::M_Type
    local vals::List{Values.Value}
    local fields::List{Record.P_Field}
    local field_names::List{String}
    @match exp begin
      INTEGER(__)  => begin
        Values.INTEGER(exp.value)
      end

      REAL(__)  => begin
        Values.REAL(exp.value)
      end

      STRING(__)  => begin
        Values.STRING(exp.value)
      end

      BOOLEAN(__)  => begin
        Values.BOOL(exp.value)
      end

      ENUM_LITERAL(ty = ty && TYPE_ENUMERATION(__))  => begin
        Values.ENUM_LITERAL(AbsynUtil.suffixPath(ty.typePath, exp.name), exp.index)
      end

      ARRAY_EXPRESSION(__)  => begin
        @assign vals = list(toDAEValue(e) for e in exp.elements)
        ValuesUtil.makeArray(vals)
      end

      RECORD_EXPRESSION(__)  => begin
        toDAEValueRecord(exp.ty, exp.path, exp.elements)
      end

      _  => begin
        Error.assertion(false, getInstanceName() + " got unhandled expression " + toString(exp), sourceInfo())
        fail()
      end
    end
  end
  value
end

function toDAEValueOpt(exp::Option{<:Expression}) ::Option{Values.Value}
  local value::Option{Values.Value} = Util.applyOption(exp, toDAEValue)
  value
end

function toDAERecord(ty::M_Type, path::Absyn.Path, args::List{<:Expression}) ::DAE.Exp
  local exp::DAE.Exp

  local field_names::List{String} = nil
  local arg::Expression
  local rest_args::List{Expression} = args
  local dargs::List{DAE.Exp} = nil

  for field in Type.recordFields(ty)
    @match _cons(arg, rest_args) = rest_args
    @assign () = begin
      @match field begin
        Record.P_Field.INPUT(__)  => begin
          @assign field_names = _cons(field.name, field_names)
          @assign dargs = _cons(toDAE(arg), dargs)
          ()
        end

        Record.P_Field.LOCAL(__)  => begin
          #=  TODO: Constants/parameters shouldn't be added to record expressions
          =#
          #=        since that causes issues with the backend, but removing them
          =#
          #=        currently causes even worse issues.
          =#
          @assign field_names = _cons(field.name, field_names)
          @assign dargs = _cons(toDAE(arg), dargs)
          ()
        end

        _  => begin
          ()
        end
      end
    end
  end
  @assign field_names = listReverseInPlace(field_names)
  @assign dargs = listReverseInPlace(dargs)
  @assign exp = DAE.RECORD(path, dargs, field_names, toDAE(ty))
  exp
end

function toDAE(exp::Expression)::DAE.Exp
  local dexp::DAE.Exp
  local changed::Bool = true
  @assign dexp = begin
    local ty::M_Type
    local daeOp::DAE.Operator
    local swap::Bool
    local dae1::DAE.Exp
    local dae2::DAE.Exp
    local names::List{String}
    local fn::P_Function.P_Function
    @match exp begin
      INTEGER_EXPRESSION(__)  => begin
        DAE.ICONST(exp.value)
      end

      REAL_EXPRESSION(__)  => begin
        DAE.RCONST(exp.value)
      end

      STRING_EXPRESSION(__)  => begin
        DAE.SCONST(exp.value)
      end

      BOOLEAN_EXPRESSION(__)  => begin
        DAE.BCONST(exp.value)
      end

      ENUM_LITERAL_EXPRESSION(ty = ty && TYPE_ENUMERATION(__))  => begin
        DAE.ENUM_LITERAL(AbsynUtil.suffixPath(ty.typePath, exp.name), exp.index)
      end

      CLKCONST_EXPRESSION(__)  => begin
        DAE.CLKCONST_EXPRESSION(P_ClockKind.toDAE(exp.clk))
      end

      CREF_EXPRESSION(__)  => begin
        DAE.CREF(toDAE(exp.cref), toDAE(exp.ty))
      end

      TYPENAME_EXPRESSION(__)  => begin
        toDAE(P_ExpandExp.ExpandExp.expandTypename(exp.ty))
      end

      ARRAY_EXPRESSION(__)  => begin
        DAE.ARRAY(toDAE(exp.ty), Type.isScalarArray(exp.ty), list(toDAE(e) for e in exp.elements))
      end

      RECORD_EXPRESSION(__)  => begin
        toDAERecord(exp.ty, exp.path, exp.elements)
      end

      RANGE_EXPRESSION(__)  => begin
        DAE.RANGE(toDAE(exp.ty), toDAE(exp.start), if isSome(exp.step)
                  SOME(toDAE(Util.getOption(exp.step)))
                  else
                  NONE()
                  end, toDAE(exp.stop))
      end

      TUPLE_EXPRESSION(__)  => begin
        DAE.TUPLE(list(toDAE(e) for e in exp.elements))
      end

      CALL_EXPRESSION(__)  => begin
        P_Call.toDAE(exp.call)
      end

      SIZE_EXPRESSION(__)  => begin
        DAE.SIZE(toDAE(exp.exp), if isSome(exp.dimIndex)
                 SOME(toDAE(Util.getOption(exp.dimIndex)))
                 else
                 NONE()
                 end)
      end

      BINARY_EXPRESSION(__)  => begin
        #=  END() doesn't have a DAE representation.
        =#
        @assign daeOp = toDAE(exp.operator)
        swap = false #=TODO. Handle arrays better Implicit stupid metamodelica=#
        @assign dae1 = toDAE(exp.exp1)
        @assign dae2 = toDAE(exp.exp2)
        DAE.BINARY(if swap
                   dae2
                   else
                   dae1
                   end, daeOp, if swap
                   dae1
                   else
                   dae2
                   end)
      end

      UNARY_EXPRESSION(__)  => begin
        DAE.UNARY(toDAE(exp.operator), toDAE(exp.exp))
      end

      LBINARY_EXPRESSION(__)  => begin
        DAE.LBINARY(toDAE(exp.exp1), toDAE(exp.operator), toDAE(exp.exp2))
      end

      LUNARY_EXPRESSION(__)  => begin
        DAE.LUNARY(toDAE(exp.operator), toDAE(exp.exp))
      end

      RELATION_EXPRESSION(__)  => begin
        DAE.RELATION(toDAE(exp.exp1), toDAE(exp.operator), toDAE(exp.exp2), -1, NONE())
      end

      IF_EXPRESSION(__)  => begin
        DAE.IFEXP(toDAE(exp.condition), toDAE(exp.trueBranch), toDAE(exp.falseBranch))
      end

      CAST_EXPRESSION(__)  => begin
        DAE.CAST(toDAE(exp.ty), toDAE(exp.exp))
      end

      BOX_EXPRESSION(__)  => begin
        DAE.BOX(toDAE(exp.exp))
      end

      UNBOX_EXPRESSION(__)  => begin
        DAE.UNBOX(toDAE(exp.exp), toDAE(exp.ty))
      end

      SUBSCRIPTED_EXP_EXPRESSION(__)  => begin
        DAE.ASUB(toDAE(exp.exp), list(toDAEExp(s) for s in exp.subscripts))
      end

      TUPLE_ELEMENT_EXPRESSION(__)  => begin
        DAE.TSUB(toDAE(exp.tupleExp), exp.index, toDAE(exp.ty))
      end

      RECORD_ELEMENT_EXPRESSION(__)  => begin
        DAE.RSUB(toDAE(exp.recordExp), exp.index, exp.fieldName, toDAE(exp.ty))
      end

      PARTIAL_FUNCTION_APPLICATION_EXPRESSION(__)  => begin
        @match _cons(fn, _) = P_Function.P_Function.typeRefCache(exp.fn)
        DAE.PARTEVALFUNCTION(P_Function.P_Function.nameConsiderBuiltin(fn), list(toDAE(arg) for arg in exp.args), toDAE(exp.ty), toDAE(TYPE_FUNCTION(fn, FunctionTYPE_FUNCTIONAL_VARIABLE)))
      end

      BINDING_EXP(__)  => begin
        toDAE(exp.exp)
      end

      _  => begin
        Error.assertion(false, getInstanceName() + " got unknown expression '" + toString(exp) + "'", sourceInfo())
        fail()
      end
    end
  end
  dexp
end

function toDAEOpt(exp::Option{<:Expression}) ::Option{DAE.Exp}
  local dexp::Option{DAE.Exp}

  @assign dexp = begin
    local e::Expression
    @match exp begin
      SOME(e)  => begin
        SOME(toDAE(e))
      end

      _  => begin
        NONE()
      end
    end
  end
  dexp
end

function isNonAssociativeExp(exp::Expression) ::Bool
  local isAssociative::Bool

  @assign isAssociative = begin
    @match exp begin
      BINARY_EXPRESSION(__)  => begin
        P_Operator.Operator.isNonAssociative(exp.operator)
      end

      LBINARY_EXPRESSION(__)  => begin
        true
      end

      _  => begin
        false
      end
    end
  end
  isAssociative
end

function isAssociativeExp(exp::Expression) ::Bool
  local isAssociative::Bool

  @assign isAssociative = begin
    @match exp begin
      BINARY_EXPRESSION(__)  => begin
        P_Operator.Operator.isAssociative(exp.operator)
      end

      LBINARY_EXPRESSION(__)  => begin
        true
      end

      _  => begin
        false
      end
    end
  end
  isAssociative
end

function priority(exp::Expression, lhs::Bool) ::Integer
  local priority::Integer

  @assign priority = begin
    @match exp begin
      INTEGER(__)  => begin
        if exp.value < 0
          4
        else
          0
        end
      end

      REAL(__)  => begin
        if exp.value < 0.0
          4
        else
          0
        end
      end

      BINARY_EXPRESSION(__)  => begin
        P_Operator.Operator.priority(exp.operator, lhs)
      end

      UNARY_EXPRESSION(__)  => begin
        4
      end

      LBINARY_EXPRESSION(__)  => begin
        P_Operator.Operator.priority(exp.operator, lhs)
      end

      LUNARY_EXPRESSION(__)  => begin
        7
      end

      RELATION_EXPRESSION(__)  => begin
        6
      end

      RANGE_EXPRESSION(__)  => begin
        10
      end

      IF_EXPRESSION(__)  => begin
        11
      end

      _  => begin
        0
      end
    end
  end
  priority
end

""" #= Helper function to toString, prints an operator and adds parentheses as needed. =#"""
function operandFlatString(operand::Expression, operator::Expression, lhs::Bool) ::String
  local str::String

  local operand_prio::Integer
  local operator_prio::Integer
  local parenthesize::Bool = false

  @assign str = toFlatString(operand)
  @assign operand_prio = priority(operand, lhs)
  if operand_prio == 4
    @assign parenthesize = true
  else
    @assign operator_prio = priority(operator, lhs)
    if operand_prio > operator_prio
      @assign parenthesize = true
    elseif operand_prio == operator_prio
      @assign parenthesize = if lhs
        isNonAssociativeExp(operand)
      else
        ! isAssociativeExp(operand)
      end
    end
  end
  if parenthesize
    @assign str = "(" + str + ")"
  end
  str
end

""" #= Helper function to toString, prints an operator and adds parentheses as needed. =#"""
function operandString(operand::Expression, operator::Expression, lhs::Bool) ::String
  local str::String

  local operand_prio::Integer
  local operator_prio::Integer
  local parenthesize::Bool = false

  @assign str = toString(operand)
  @assign operand_prio = priority(operand, lhs)
  if operand_prio == 4
    @assign parenthesize = true
  else
    @assign operator_prio = priority(operator, lhs)
    if operand_prio > operator_prio
      @assign parenthesize = true
    elseif operand_prio == operator_prio
      @assign parenthesize = if lhs
        isNonAssociativeExp(operand)
      else
        ! isAssociativeExp(operand)
      end
    end
  end
  if parenthesize
    @assign str = "(" + str + ")"
  end
  str
end

function toFlatSubscriptedString(exp::Expression, subs::List{<:Subscript}) ::String
  local str::String

  local exp_ty::M_Type
  local sub_tyl::List{M_Type}
  local dims::List{Dimension}
  local strl::List{String}
  local name::String

  @assign exp_ty = typeOf(exp)
  @assign dims = ListUtil.firstN(arrayDims(exp_ty), listLength(subs))
  @assign sub_tyl = list(P_Dimension.Dimension.subscriptType(d) for d in dims)
  @assign name = Type.subscriptedTypeName(exp_ty, sub_tyl)
  @assign strl = list(")")
  for s in subs
    @assign strl = _cons(toFlatString(s), strl)
    @assign strl = _cons(",", strl)
  end
  @assign strl = _cons(toFlatString(exp), strl)
  @assign strl = _cons("'(", strl)
  @assign strl = _cons(name, strl)
  @assign strl = _cons("'", strl)
  @assign str = stringAppendList(strl)
  str
end

function toFlatString(exp::Expression) ::String
  local str::String

  local t::M_Type
  local clk::ClockKind

  @assign str = begin
    @match exp begin
      INTEGER(__)  => begin
        intString(exp.value)
      end

      REAL(__)  => begin
        realString(exp.value)
      end

      STRING(__)  => begin
        "\\" + exp.value + "\\"
      end

      BOOLEAN(__)  => begin
        boolString(exp.value)
      end

      ENUM_LITERAL(ty = t && TYPE_ENUMERATION(__))  => begin
        "'" + AbsynUtil.pathString(t.typePath) + "'." + exp.name
      end

      CLKCONST_EXPRESSION(clk)  => begin
        P_ClockKind.toString(clk)
      end

      CREF(__)  => begin
        toFlatString(exp.cref)
      end

      TYPENAME(__)  => begin
        Type.typenameString(arrayElementType(exp.ty))
      end

      ARRAY_EXPRESSION(__)  => begin
        "{" + stringDelimitList(List(toFlatString(e) for e in exp.elements), ", ") + "}"
      end

      MATRIX_EXPRESSION(__)  => begin
        "[" + stringDelimitList(List(stringDelimitList(List(toFlatString(e) for e in el), ", ") for el in exp.elements), "; ") + "]"
      end

      RANGE_EXPRESSION(__)  => begin
        operandFlatString(exp.start, exp, false) + (if isSome(exp.step)
                                                    ":" + operandFlatString(Util.getOption(exp.step), exp, false)
                                                    else
                                                    ""
                                                    end) + ":" + operandFlatString(exp.stop, exp, false)
      end

      TUPLE_EXPRESSION(__)  => begin
        "(" + stringDelimitList(List(toFlatString(e) for e in exp.elements), ", ") + ")"
      end

      RECORD_EXPRESSION(__)  => begin
        ListUtil.toString(exp.elements, toFlatString, "'" + AbsynUtil.pathString(exp.path), "'(", ", ", ")", true)
      end

      CALL_EXPRESSION(__)  => begin
        P_Call.toFlatString(exp.call)
      end

      SIZE_EXPRESSION(__)  => begin
        "size(" + toFlatString(exp.exp) + (if isSome(exp.dimIndex)
                                           ", " + toFlatString(Util.getOption(exp.dimIndex))
                                           else
                                           ""
                                           end) + ")"
      end

      END(__)  => begin
        "end"
      end

      BINARY_EXPRESSION(__)  => begin
        operandFlatString(exp.exp1, exp, true) + P_Operator.Operator.symbol(exp.operator) + operandFlatString(exp.exp2, exp, false)
      end

      UNARY_EXPRESSION(__)  => begin
        P_Operator.Operator.symbol(exp.operator, "") + operandFlatString(exp.exp, exp, false)
      end

      LBINARY_EXPRESSION(__)  => begin
        operandFlatString(exp.exp1, exp, true) + P_Operator.Operator.symbol(exp.operator) + operandFlatString(exp.exp2, exp, false)
      end

      LUNARY_EXPRESSION(__)  => begin
        P_Operator.Operator.symbol(exp.operator, "") + " " + operandFlatString(exp.exp, exp, false)
      end

      RELATION_EXPRESSION(__)  => begin
        operandFlatString(exp.exp1, exp, true) + P_Operator.Operator.symbol(exp.operator) + operandFlatString(exp.exp2, exp, false)
      end

      IF_EXPRESSION(__)  => begin
        "if " + toFlatString(exp.condition) + " then " + toFlatString(exp.trueBranch) + " else " + toFlatString(exp.falseBranch)
      end

      UNBOX_EXPRESSION(__)  => begin
        "UNBOX_EXPRESSION(" + toFlatString(exp.exp) + ")"
      end

      BOX_EXPRESSION(__)  => begin
        "BOX_EXPRESSION(" + toFlatString(exp.exp) + ")"
      end

      CAST_EXPRESSION(__)  => begin
        toFlatString(exp.exp)
      end

      SUBSCRIPTED_EXP_EXPRESSION(__)  => begin
        toFlatSubscriptedString(exp.exp, exp.subscripts)
      end

      TUPLE_ELEMENT_EXPRESSION(__)  => begin
        toFlatString(exp.tupleExp) + "[" + intString(exp.index) + "]"
      end

      RECORD_ELEMENT_EXPRESSION(__)  => begin
        toFlatString(exp.recordExp) + "[field: " + exp.fieldName + "]"
      end

      MUTABLE_EXPRESSION(__)  => begin
        toFlatString(P_Pointer.access(exp.exp))
      end

      EMPTY(__)  => begin
        "#EMPTY#"
      end

      PARTIAL_FUNCTION_APPLICATION_EXPRESSION(__)  => begin
        "function " + toFlatString(exp.fn) + "(" + stringDelimitList(List(@do_threaded_for n + " = " + toFlatString(a) (a, n) (exp.args, exp.argNames)), ", ") + ")"
      end

      BINDING_EXP(__)  => begin
        toFlatString(exp.exp)
      end

      _  => begin
        anyString(exp)
      end
    end
  end
  str
end

function toString(exp::Expression) ::String
  local str::String

  local t::M_Type
  local clk::ClockKind

  @assign str = begin
    @match exp begin
      INTEGER(__)  => begin
        intString(exp.value)
      end

      REAL(__)  => begin
        realString(exp.value)
      end

      STRING(__)  => begin
        "\\" + exp.value + "\\"
      end

      BOOLEAN(__)  => begin
        boolString(exp.value)
      end

      ENUM_LITERAL(ty = t && TYPE_ENUMERATION(__))  => begin
        AbsynUtil.pathString(t.typePath) + "." + exp.name
      end

      CLKCONST_EXPRESSION(clk)  => begin
        P_ClockKind.toString(clk)
      end

      CREF(__)  => begin
        toString(exp.cref)
      end

      TYPENAME(__)  => begin
        Type.typenameString(arrayElementType(exp.ty))
      end

      ARRAY_EXPRESSION(__)  => begin
        "{" + stringDelimitList(List(toString(e) for e in exp.elements), ", ") + "}"
      end

      MATRIX_EXPRESSION(__)  => begin
        "[" + stringDelimitList(List(stringDelimitList(List(toString(e) for e in el), ", ") for el in exp.elements), "; ") + "]"
      end

      RANGE_EXPRESSION(__)  => begin
        operandString(exp.start, exp, false) + (if isSome(exp.step)
                                                ":" + operandString(Util.getOption(exp.step), exp, false)
                                                else
                                                ""
                                                end) + ":" + operandString(exp.stop, exp, false)
      end

      TUPLE_EXPRESSION(__)  => begin
        "(" + stringDelimitList(List(toString(e) for e in exp.elements), ", ") + ")"
      end

      RECORD_EXPRESSION(__)  => begin
        ListUtil.toString(exp.elements, toString, AbsynUtil.pathString(exp.path), "(", ", ", ")", true)
      end

      CALL_EXPRESSION(__)  => begin
        P_Call.toString(exp.call)
      end

      SIZE_EXPRESSION(__)  => begin
        "size(" + toString(exp.exp) + (if isSome(exp.dimIndex)
                                       ", " + toString(Util.getOption(exp.dimIndex))
                                       else
                                       ""
                                       end) + ")"
      end

      END(__)  => begin
        "end"
      end

      BINARY_EXPRESSION(__)  => begin
        operandString(exp.exp1, exp, true) + P_Operator.Operator.symbol(exp.operator) + operandString(exp.exp2, exp, false)
      end

      UNARY_EXPRESSION(__)  => begin
        P_Operator.Operator.symbol(exp.operator, "") + operandString(exp.exp, exp, false)
      end

      LBINARY_EXPRESSION(__)  => begin
        operandString(exp.exp1, exp, true) + P_Operator.Operator.symbol(exp.operator) + operandString(exp.exp2, exp, false)
      end

      LUNARY_EXPRESSION(__)  => begin
        P_Operator.Operator.symbol(exp.operator, "") + " " + operandString(exp.exp, exp, false)
      end

      RELATION_EXPRESSION(__)  => begin
        operandString(exp.exp1, exp, true) + P_Operator.Operator.symbol(exp.operator) + operandString(exp.exp2, exp, false)
      end

      IF_EXPRESSION(__)  => begin
        "if " + toString(exp.condition) + " then " + toString(exp.trueBranch) + " else " + toString(exp.falseBranch)
      end

      UNBOX_EXPRESSION(__)  => begin
        "UNBOX_EXPRESSION(" + toString(exp.exp) + ")"
      end

      BOX_EXPRESSION(__)  => begin
        "BOX_EXPRESSION(" + toString(exp.exp) + ")"
      end

      CAST_EXPRESSION(__)  => begin
        if Flags.isSet(Flags.NF_API)
          toString(exp.exp)
        else
          "CAST_EXPRESSION(" + Type.toString(exp.ty) + ", " + toString(exp.exp) + ")"
        end
      end

      SUBSCRIPTED_EXP_EXPRESSION(__)  => begin
        toString(exp.exp) + toStringList(exp.subscripts)
      end

      TUPLE_ELEMENT_EXPRESSION(__)  => begin
        toString(exp.tupleExp) + "[" + intString(exp.index) + "]"
      end

      RECORD_ELEMENT_EXPRESSION(__)  => begin
        toString(exp.recordExp) + "[field: " + exp.fieldName + "]"
      end

      MUTABLE_EXPRESSION(__)  => begin
        toString(P_Pointer.access(exp.exp))
      end

      EMPTY(__)  => begin
        "#EMPTY#"
      end

      PARTIAL_FUNCTION_APPLICATION_EXPRESSION(__)  => begin
        "function " + toString(exp.fn) + "(" + stringDelimitList(List(@do_threaded_for n + " = " + toString(a) (a, n) (exp.args, exp.argNames)), ", ") + ")"
      end

      BINDING_EXP(__)  => begin
        toString(exp.exp)
      end

      _  => begin
        anyString(exp)
      end
    end
  end
  str
end

function toStringTyped(exp::Expression) ::String
  local str::String

  @assign str = "/*" + Type.toString(typeOf(exp)) + "*/ " + toString(exp)
  str
end

function toInteger(exp::Expression) ::Integer
  local i::Integer

  @assign i = begin
    @match exp begin
      INTEGER(__)  => begin
        exp.value
      end

      BOOLEAN(__)  => begin
        if exp.value
          1
        else
          0
        end
      end

      ENUM_LITERAL(__)  => begin
        exp.index
      end
    end
  end
  i
end

function makeEnumLiterals(enumType::M_Type) ::List{Expression}
  local literals::List{Expression}

  local lits::List{String}

  @match TYPE_ENUMERATION(literals = lits) = enumType
  @assign literals = List(@do_threaded_for ENUM_LITERAL(enumType, l, i) (l, i) (lits, 1:listLength(lits)))
  literals
end

function makeEnumLiteral(enumType::M_Type, index::Integer) ::Expression
  local literal::Expression

  local literals::List{String}

  @match TYPE_ENUMERATION(literals = literals) = enumType
  @assign literal = ENUM_LITERAL(enumType, listGet(literals, index), index)
  literal
end

function arrayFromList_impl(inExps::List{<:Expression}, elemTy::M_Type, inDims::List{<:Dimension}) ::Expression
  local outExp::Expression

  local ldim::Dimension
  local restdims::List{Dimension}
  local ty::M_Type
  local newlst::List{Expression}
  local partexps::List{List{Expression}}
  local dimsize::Integer

  Error.assertion(! listEmpty(inDims), "Empty dimension list given in arrayFromList.", sourceInfo())
  @match _cons(ldim, restdims) = inDims
  @assign dimsize = P_Dimension.Dimension.size(ldim)
  @assign ty = Type.liftArrayLeft(elemTy, ldim)
  if ListUtil.hasOneElement(inDims)
    Error.assertion(dimsize == listLength(inExps), "Length mismatch in arrayFromList.", sourceInfo())
    @assign outExp = makeArray(ty, inExps)
    return outExp
  end
  @assign partexps = ListUtil.partition(inExps, dimsize)
  @assign newlst = nil
  for arrexp in partexps
    @assign newlst = _cons(makeArray(ty, arrexp), newlst)
  end
  @assign newlst = listReverse(newlst)
  @assign outExp = arrayFromList_impl(newlst, ty, restdims)
  outExp
end

function arrayFromList(inExps::List{<:Expression}, elemTy::M_Type, inDims::List{<:Dimension}) ::Expression
  local outExp::Expression

  @assign outExp = arrayFromList_impl(inExps, elemTy, listReverse(inDims))
  outExp
end

function replaceIterator2(exp::Expression, iterator::InstNode, iteratorValue::Expression) ::Expression


  @assign exp = begin
    local node::InstNode
    @match exp begin
      CREF(cref = CREF(node = node))  => begin
        if refEqual(iterator, node)
          iteratorValue
        else
          exp
        end
      end

      _  => begin
        exp
      end
    end
  end
  exp
end

function replaceIterator(exp::Expression, iterator::InstNode, iteratorValue::Expression) ::Expression


  @assign exp = map(exp, (iterator, iteratorValue) -> replaceIterator2(iterator = iterator, iteratorValue = iteratorValue))
  exp
end

function makeSubscriptedExp(subscripts::List{<:Subscript}, exp::Expression) ::Expression
  local outExp::Expression

  local e::Expression
  local subs::List{Subscript}
  local extra_subs::List{Subscript}
  local ty::M_Type
  local dim_count::Integer

  #=  If the expression is already a SUBSCRIPTED_EXP_EXPRESSION we need to concatenate the
  =#
  #=  old subscripts with the new. Otherwise we just create a new SUBSCRIPTED_EXP_EXPRESSION.
  =#
  @assign (e, subs, ty) = begin
    @match exp begin
      SUBSCRIPTED_EXP_EXPRESSION(__)  => begin
        (exp.exp, exp.subscripts, typeOf(exp.exp))
      end

      _  => begin
        (exp, nil, typeOf(exp))
      end
    end
  end
  @assign dim_count = Type.dimensionCount(ty)
  @assign (subs, extra_subs) = mergeList(subscripts, subs, dim_count)
  #=  Check that the expression has enough dimensions to be subscripted.
  =#
  if ! listEmpty(extra_subs)
    Error.assertion(false, getInstanceName() + ": too few dimensions in " + toString(exp) + " to apply subscripts " + toStringList(subscripts), sourceInfo())
  end
  @assign ty = Type.subscript(ty, subs)
  @assign outExp = SUBSCRIPTED_EXP_EXPRESSION(e, subs, ty)
  outExp
end

function applySubscriptIf(subscript::Subscript, exp::Expression, restSubscripts::List{<:Subscript}) ::Expression
  local outExp::Expression

  local cond::Expression
  local tb::Expression
  local fb::Expression

  @match IF_EXPRESSION(cond, tb, fb) = exp
  @assign tb = applySubscript(subscript, tb, restSubscripts)
  @assign fb = applySubscript(subscript, fb, restSubscripts)
  @assign outExp = IF_EXPRESSION(cond, tb, fb)
  outExp
end

function applyIndexSubscriptArrayConstructor(call::Call, index::Subscript) ::Expression
  local subscriptedExp::Expression

  local ty::M_Type
  local var::VariabilityType
  local exp::Expression
  local iter_exp::Expression
  local iters::List{Tuple{InstNode, Expression}}
  local iter::InstNode

  @match P_Call.TYPED_ARRAY_CONSTRUCTOR(ty, var, exp, iters) = call
  @assign ((iter, iter_exp), iters) = ListUtil.splitLast(iters)
  @assign iter_exp = applySubscript(index, iter_exp)
  @assign subscriptedExp = replaceIterator(exp, iter, iter_exp)
  if ! listEmpty(iters)
    @assign subscriptedExp = CALL_EXPRESSION(P_Call.TYPED_ARRAY_CONSTRUCTOR(Type.unliftArray(ty), var, subscriptedExp, iters))
  end
  subscriptedExp
end

function applySubscriptArrayConstructor(subscript::Subscript, call::Call, restSubscripts::List{<:Subscript}) ::Expression
  local outExp::Expression

  if isIndex(subscript) && listEmpty(restSubscripts)
    @assign outExp = applyIndexSubscriptArrayConstructor(call, subscript)
  else
    @assign outExp = makeSubscriptedExp(_cons(subscript, restSubscripts), CALL_EXPRESSION(call))
  end
  #=  TODO: Handle slicing and multiple subscripts better.
  =#
  outExp
end

function applySubscriptCall(subscript::Subscript, exp::Expression, restSubscripts::List{<:Subscript}) ::Expression
  local outExp::Expression

  local call::Call

  @match CALL_EXPRESSION(call = call) = exp
  @assign outExp = begin
    local arg::Expression
    local ty::M_Type
    @match call begin
      P_Call.TYPED_CALL(arguments = arg <|  nil()) where (P_Function.P_Function.isSubscriptableBuiltin(call.fn))  => begin
        @assign arg = applySubscript(subscript, arg, restSubscripts)
        @assign ty = Type.copyDims(typeOf(arg), call.ty)
        CALL(P_Call.TYPED_CALL(call.fn, ty, call.var, list(arg), call.attributes))
      end

      P_Call.TYPED_ARRAY_CONSTRUCTOR(__)  => begin
        applySubscriptArrayConstructor(subscript, call, restSubscripts)
      end

      _  => begin
        makeSubscriptedExp(_cons(subscript, restSubscripts), exp)
      end
    end
  end
  outExp
end

function applyIndexSubscriptRange2(startExp::Expression, stepExp::Option{<:Expression}, stopExp::Expression, index::Integer) ::Expression
  local subscriptedExp::Expression

  local iidx::Integer
  local ridx::AbstractFloat

  @assign subscriptedExp = begin
    @match (startExp, stepExp) begin
      (INTEGER_EXPRESSION(__), SOME(INTEGER_EXPRESSION(iidx)))  => begin
        INTEGER_EXPRESSION(startExp.value + (index - 1) * iidx)
      end

      (INTEGER_EXPRESSION(__), _)  => begin
        INTEGER_EXPRESSION(startExp.value + index - 1)
      end

      (P_Expression.REAL_EXPRESSION(__), SOME(P_Expression.REAL_EXPRESSION(ridx)))  => begin
        P_Expression.REAL_EXPRESSION(startExp.value + (index - 1) * ridx)
      end

      (P_Expression.REAL_EXPRESSION(__), _)  => begin
        P_Expression.REAL_EXPRESSION(startExp.value + index - 1.0)
      end

      (P_Expression.BOOLEAN_EXPRESSION(__), _)  => begin
        if index == 1
          startExp
        else
          stopExp
        end
      end

      (P_Expression.Expression.ENUM_LITERAL(index = iidx), _)  => begin
        @assign iidx = iidx + index - 1
        nthEnumLiteral(startExp.ty, iidx)
      end
    end
  end
  subscriptedExp
end

function applyIndexSubscriptRange(rangeExp::Expression, index::Subscript) ::Expression
  local outExp::Expression

  local index_exp::Expression
  local start_exp::Expression
  local stop_exp::Expression
  local step_exp::Option{Expression}
  local ty::M_Type

  @match SUBSCRIPT_INDEX(index = index_exp) = index
  if isScalarLiteral(index_exp)
    @match RANGE_EXPRESSION(start = start_exp, step = step_exp, stop = stop_exp) = rangeExp
    @assign outExp = applyIndexSubscriptRange2(start_exp, step_exp, stop_exp, toInteger(index_exp))
  else
    @match RANGE_EXPRESSION(ty = ty) = rangeExp
    @assign outExp = SUBSCRIPTED_EXP_EXPRESSION(rangeExp, list(index), ty)
  end
  outExp
end

function applySubscriptRange(subscript::Subscript, exp::Expression) ::Expression
  local outExp::Expression

  local sub::Subscript
  local start_exp::Expression
  local stop_exp::Expression
  local step_exp::Option{Expression}
  local ty::M_Type
  local expl::List{Expression}

  @assign sub = expandSlice(subscript)
  @assign outExp = begin
    @match sub begin
      SUBSCRIPT_INDEX(__)  => begin
        applyIndexSubscriptRange(exp, sub)
      end

      SUBSCRIPT_SLICE(__)  => begin
        @match RANGE_EXPRESSION(ty = ty) = exp
        @assign ty = ARRAY_TYPE(Type.unliftArray(ty), list(toDimension(sub)))
        SUBSCRIPTED_EXP_EXPRESSION(exp, list(subscript), ty)
      end

      SUBSCRIPT_WHOLE(__)  => begin
        exp
      end

      SUBSCRIPT_EXPANDED_SLICE(__)  => begin
        @assign expl = List(applyIndexSubscriptRange(exp, i) for i in sub.indices)
        @match RANGE_EXPRESSION(ty = ty) = exp
        makeArray(Type.liftArrayLeft(ty, P_Dimension.Dimension.fromInteger(listLength(expl))), expl)
      end
    end
  end
  outExp
end

function applyIndexExpArray(exp::Expression, index::Expression, restSubscripts::List{<:Subscript}) ::Expression
  local outExp::Expression

  local expl::List{Expression}

  if isScalarLiteral(index)
    @match ARRAY_EXPRESSION(elements = expl) = exp
    @assign outExp = applySubscripts(restSubscripts, listGet(expl, toInteger(index)))
  elseif isBindingExp(index)
    @assign outExp = bindingExpMap(index, (exp, restSubscripts) -> applyIndexExpArray(exp = exp, restSubscripts = restSubscripts))
  else
    @assign outExp = makeSubscriptedExp(_cons(SUBSCRIPT_INDEX(index), restSubscripts), exp)
  end
  outExp
end

function applyIndexSubscriptArray(exp::Expression, index::Subscript, restSubscripts::List{<:Subscript}) ::Expression
  local outExp::Expression

  @assign outExp = applyIndexExpArray(exp, toExp(index), restSubscripts)
  outExp
end

function applySubscriptArray(subscript::Subscript, exp::Expression, restSubscripts::List{<:Subscript}) ::Expression
  local outExp::Expression

  local sub::Subscript
  local s::Subscript
  local rest_subs::List{Subscript}
  local expl::List{Expression}
  local ty::M_Type
  local el_count::Integer
  local literal::Bool

  @assign sub = expandSlice(subscript)
  @assign outExp = begin
    @match sub begin
      SUBSCRIPT_INDEX(__)  => begin
        applyIndexSubscriptArray(exp, sub, restSubscripts)
      end

      SUBSCRIPT_SLICE(__)  => begin
        makeSubscriptedExp(_cons(subscript, restSubscripts), exp)
      end

      SUBSCRIPT_WHOLE(__)  => begin
        if listEmpty(restSubscripts)
          @assign outExp = exp
        else
          @match ARRAY_EXPRESSION(ty = ty, elements = expl, literal = literal) = exp
          @match _cons(s, rest_subs) = restSubscripts
          @assign expl = List(applySubscript(s, e, rest_subs) for e in expl)
          @assign el_count = listLength(expl)
          @assign ty = if el_count > 0
            typeOf(listHead(expl))
          else
            Type.subscript(Type.unliftArray(ty), restSubscripts)
          end
          @assign ty = Type.liftArrayLeft(ty, P_Dimension.Dimension.fromInteger(el_count))
          @assign outExp = makeArray(ty, expl, literal)
        end
        outExp
      end

      SUBSCRIPT_EXPANDED_SLICE(__)  => begin
        @match ARRAY_EXPRESSION(ty = ty, literal = literal) = exp
        @assign expl = List(applyIndexSubscriptArray(exp, i, restSubscripts) for i in sub.indices)
        @assign el_count = listLength(expl)
        @assign ty = if el_count > 0
          typeOf(listHead(expl))
        else
          Type.subscript(Type.unliftArray(ty), restSubscripts)
        end
        @assign ty = Type.liftArrayLeft(ty, P_Dimension.Dimension.fromInteger(el_count))
        makeArray(ty, expl, literal)
      end
    end
  end
  outExp
end

function applyIndexSubscriptTypename(ty::M_Type, index::Subscript) ::Expression
  local subscriptedExp::Expression

  local idx_exp::Expression
  local idx::Integer

  @assign idx_exp = toExp(index)
  if isScalarLiteral(idx_exp)
    @assign idx = toInteger(idx_exp)
    @assign subscriptedExp = begin
      @match ty begin
        TYPE_BOOLEAN(__) where (idx <= 2)  => begin
          if idx == 1
            P_Expression.BOOLEAN_EXPRESSION(false)
          else
            P_Expression.BOOLEAN_EXPRESSION(true)
          end
        end

        TYPE_ENUMERATION(__)  => begin
          nthEnumLiteral(ty, idx)
        end
      end
    end
  else
    @assign subscriptedExp = SUBSCRIPTED_EXP_EXPRESSION(TYPENAME(ty), list(index), ty)
  end
  subscriptedExp
end

function applySubscriptTypename(subscript::Subscript, ty::M_Type) ::Expression
  local outExp::Expression

  local sub::Subscript
  local index::Integer
  local expl::List{Expression}

  @assign sub = expandSlice(subscript)
  @assign outExp = begin
    @match sub begin
      SUBSCRIPT_INDEX(__)  => begin
        applyIndexSubscriptTypename(ty, sub)
      end

      SUBSCRIPT_SLICE(__)  => begin
        SUBSCRIPTED_EXP_EXPRESSION(TYPENAME(ty), list(subscript), ARRAY_TYPE(ty, list(toDimension(sub))))
      end

      SUBSCRIPT_WHOLE(__)  => begin
        TYPENAME(ty)
      end

      SUBSCRIPT_EXPANDED_SLICE(__)  => begin
        @assign expl = List(applyIndexSubscriptTypename(ty, i) for i in sub.indices)
        makeArray(Type.liftArrayLeft(ty, P_Dimension.Dimension.fromInteger(listLength(expl))), expl, literal = true)
      end
    end
  end
  outExp
end

function applySubscriptCref(subscript::Subscript, cref::ComponentRef, restSubscripts::List{<:Subscript}) ::Expression
  local outExp::Expression

  local cr::ComponentRef
  local ty::M_Type

  @assign cr = applySubscripts(_cons(subscript, restSubscripts), cref)
  @assign ty = getSubscriptedType(cr)
  @assign outExp = CREF(ty, cr)
  outExp
end

""" #= Subscripts an expression with the given subscript, and then applies the
               optional list of subscripts to each element of the subscripted expression. =#"""
                 function applySubscript(subscript::Subscript, exp::Expression, restSubscripts::List{<:Subscript} = nil) ::Expression
                   local outExp::Expression

                   @assign outExp = begin
                     @match exp begin
                       CREF(__)  => begin
                         applySubscriptCref(subscript, exp.cref, restSubscripts)
                       end

                       TYPENAME(__) where (listEmpty(restSubscripts))  => begin
                         applySubscriptTypename(subscript, exp.ty)
                       end

                       ARRAY_EXPRESSION(__)  => begin
                         applySubscriptArray(subscript, exp, restSubscripts)
                       end

                       RANGE_EXPRESSION(__) where (listEmpty(restSubscripts))  => begin
                         applySubscriptRange(subscript, exp)
                       end

                       CALL_EXPRESSION(call = P_Call.TYPED_ARRAY_CONSTRUCTOR(__))  => begin
                         applySubscriptArrayConstructor(subscript, exp.call, restSubscripts)
                       end

                       CALL_EXPRESSION(__)  => begin
                         applySubscriptCall(subscript, exp, restSubscripts)
                       end

                       IF_EXPRESSION(__)  => begin
                         applySubscriptIf(subscript, exp, restSubscripts)
                       end

                       BINDING_EXP(__)  => begin
                         bindingExpMap(exp, (subscript, restSubscripts) -> applySubscript(subscript = subscript, restSubscripts = restSubscripts))
                       end

                       _  => begin
                         makeSubscriptedExp(_cons(subscript, restSubscripts), exp)
                       end
                     end
                   end
                   outExp
                 end

""" #= Subscripts an expression with the given list of subscripts. =#"""
function applySubscripts(subscripts::List{<:Subscript}, exp::Expression) ::Expression
  local outExp::Expression

  if listEmpty(subscripts)
    @assign outExp = exp
  else
    @assign outExp = applySubscript(listHead(subscripts), exp, listRest(subscripts))
  end
  outExp
end

function makeRecord(recordName::Absyn.Path, recordType::M_Type, fields::List{<:Expression}) ::Expression
  local exp::Expression

  @assign exp = RECORD_EXPRESSION(recordName, recordType, fields)
  exp
end

function makeExpArray(elements::List{<:Expression}, isLiteral::Bool = false) ::Expression
  local exp::Expression

  local ty::M_Type

  @assign ty = typeOf(listHead(elements))
  @assign ty = Type.liftArrayLeft(ty, P_Dimension.Dimension.fromInteger(listLength(elements)))
  @assign exp = makeArray(ty, elements, isLiteral)
  exp
end

function makeRealMatrix(values::List{<:List{<:AbstractFloat}}) ::Expression
  local exp::Expression

  local ty::M_Type
  local expl::List{Expression}

  if listEmpty(values)
    @assign ty = ARRAY_TYPE(TYPE_REAL(), list(P_Dimension.Dimension.fromInteger(0), P_Dimension.Dimension.UNKNOWN()))
    @assign exp = makeEmptyArray(ty)
  else
    @assign ty = ARRAY_TYPE(TYPE_REAL(), list(P_Dimension.Dimension.fromInteger(listLength(listHead(values)))))
    @assign expl = List(makeArray(ty, List(REAL(v) for v in row), literal = true) for row in values)
    @assign ty = Type.liftArrayLeft(ty, P_Dimension.Dimension.fromInteger(listLength(expl)))
    @assign exp = makeArray(ty, expl, literal = true)
  end
  exp
end

function makeRealArray(values::List{<:AbstractFloat}) ::Expression
  local exp::Expression

  @assign exp = makeArray(ARRAY_TYPE(TYPE_REAL(), list(P_Dimension.Dimension.fromInteger(listLength(values)))), List(REAL(v) for v in values), literal = true)
  exp
end

function makeIntegerArray(values::List{<:Integer}) ::Expression
  local exp::Expression

  @assign exp = makeArray(ARRAY_TYPE(TYPE_INTEGER(), list(P_Dimension.Dimension.fromInteger(listLength(values)))), List(INTEGER(v) for v in values), literal = true)
  exp
end

function makeEmptyArray(ty::M_Type) ::Expression
  local outExp::Expression

  @assign outExp = ARRAY_EXPRESSION(ty, nil, true)
  outExp
end

function makeArray(ty::M_Type, expl::List{<:Expression}, literal::Bool = false) ::Expression
  local outExp::Expression

  @assign outExp = ARRAY_EXPRESSION(ty, expl, literal)
  outExp
end

function stringValue(exp::Expression) ::String
  local value::String

  @match STRING(value = value) = exp
  value
end

function makeInteger(value::Integer) ::Expression
  local exp::Expression = INTEGER(value)
  exp
end

function integerValue(exp::Expression) ::Integer
  local value::Integer

  @match INTEGER(value = value) = exp
  value
end

function makeReal(value::AbstractFloat) ::Expression
  local exp::Expression = REAL(value)
  exp
end

function realValue(exp::Expression) ::AbstractFloat
  local value::AbstractFloat

  @assign value = begin
    @match exp begin
      REAL(__)  => begin
        exp.value
      end

      INTEGER(__)  => begin
        intReal(exp.value)
      end
    end
  end
  value
end

""" #= Converts an expression to the given type. Dimensions of array types can be
               omitted, and are ignored by this function, since arrays can't be cast to a
               different size. Only the element type of the type is used, so for example:
                 typeCast({1, 2, 3}, TYPE_REAL()) => {1.0, 2.0, 3.0}

               The function does not check that the cast is valid, and expressions that
               can't be converted outright will be wrapped as a CAST expression. =#"""
 function typeCast(exp::Expression, ty::NFType) ::Expression
   local t::NFType
   local t2::NFType
   local ety::NFType
   local el::List{Expression}
   @assign ety = arrayElementType(ty)
   @assign exp = begin
     @match (exp, ety) begin
       (INTEGER_EXPRESSION(__), TYPE_REAL(__))  => begin
         REAL_EXPRESSION(intReal(exp.value))
       end
       (BOOLEAN_EXPRESSION(__), TYPE_REAL(__)) where (Flags.isSet(Flags.NF_API))  => begin
         REAL(if exp.value
              1.0
              else
              0.0
              end)
       end
       (REAL_EXPRESSION(__), TYPE_REAL(__))  => begin
         exp
       end
       (ARRAY_EXPRESSION(ty = t, elements = el), _)  => begin
         #=  Integer can be cast to Real.
         =#
         #=  Boolean can be cast to Real (only if -d=nfAPI is on)
         =#
         #=  as there are annotations having expressions such as Boolean x > 0.5
         =#
         #=  Real doesn't need to be cast to Real, since we convert e.g. array with
         =#
         #=  a mix of Integers and Reals to only Reals.
         =#
         #=  For arrays we typecast each element and update the type of the array.
         =#
         @assign el = list(typeCast(e, ety) for e in el)
         @assign t = setArrayElementType(t, ety)
         ARRAY_EXPRESSION(t, el, exp.literal)
       end
       (RANGE_EXPRESSION(ty = t), _)  => begin
         @assign t = setArrayElementType(t, ety)
         RANGE_EXPRESSION(t, typeCast(exp.start, ety), typeCastOpt(exp.step, ety), typeCast(exp.stop, ety))
       end
       (UNARY_EXPRESSION(__), _)  => begin
         #=  Unary operators (i.e. -) are handled by casting the operand.
         =#
         @assign t = setArrayElementType(P_Operator.Operator.typeOf(exp.operator), ety)
         UNARY_EXPRESSION(setType(t, exp.operator), typeCast(exp.exp, ety))
       end
       (IF_EXPRESSION(__), _)  => begin
         IF(exp.condition, typeCast(exp.trueBranch, ety), typeCast(exp.falseBranch, ety))
       end
       (CALL_EXPRESSION(__), _)  => begin
         typeCast(exp, ety)
       end
       (CAST_EXPRESSION(__), _)  => begin
         typeCast(exp.exp, ty)
       end
       (BINDING_EXP(__), _)  => begin
         #=  If-expressions are handled by casting each of the branches.
         =#
         #=  Calls are handled by Call.typeCast, which has special rules for some functions.
         =#
         #=  Casting a cast expression overrides its current cast type.
         =#
         @assign t = setArrayElementType(exp.expType, ety)
         @assign t2 = setArrayElementType(exp.bindingType, ety)
         BINDING_EXP(typeCast(exp.exp, ety), t, t2, exp.parents, exp.isEach)
       end
       _  => begin
         #=  Other expressions are handled by making a CAST expression.
         =#
         @assign t = typeOf(exp)
         @assign t = setArrayElementType(t, ety)
         CAST_EXPRESSION(t, exp)
       end
     end
   end
   exp
 end

function typeCastOpt(exp::Option{<:Expression}, ty::M_Type) ::Option{Expression}
  local outExp::Option{Expression} = Util.applyOption(exp, (ty) -> typeCast(ty = ty))
  outExp
end

function setType(ty::NFType, exp::Expression) ::Expression

  @assign () = begin
    @match exp begin
      ENUM_LITERAL(__)  => begin
        @assign exp.ty = ty
        ()
      end

      CREF(__)  => begin
        @assign exp.ty = ty
        ()
      end

      TYPENAME(__)  => begin
        @assign exp.ty = ty
        ()
      end

      ARRAY_EXPRESSION(__)  => begin
        @assign exp.ty = ty
        ()
      end

      RANGE_EXPRESSION(__)  => begin
        @assign exp.ty = ty
        ()
      end

      TUPLE_EXPRESSION(__)  => begin
        @assign exp.ty = ty
        ()
      end

      RECORD_EXPRESSION(__)  => begin
        @assign exp.ty = ty
        ()
      end

      CALL_EXPRESSION(__)  => begin
        @assign exp.call = P_Call.setType(exp.call, ty)
        ()
      end

      BINARY_EXPRESSION(__)  => begin
        @assign exp.operator = setType(ty, exp.operator)
        ()
      end

      UNARY_EXPRESSION(__)  => begin
        @assign exp.operator = setType(ty, exp.operator)
        ()
      end

      LBINARY_EXPRESSION(__)  => begin
        @assign exp.operator = setType(ty, exp.operator)
        ()
      end

      LUNARY_EXPRESSION(__)  => begin
        @assign exp.operator = setType(ty, exp.operator)
        ()
      end

      RELATION_EXPRESSION(__)  => begin
        @assign exp.operator = setType(ty, exp.operator)
        ()
      end

      CAST_EXPRESSION(__)  => begin
        @assign exp.ty = ty
        ()
      end

      UNBOX_EXPRESSION(__)  => begin
        @assign exp.ty = ty
        ()
      end

      SUBSCRIPTED_EXP_EXPRESSION(__)  => begin
        @assign exp.ty = ty
        ()
      end

      TUPLE_ELEMENT_EXPRESSION(__)  => begin
        @assign exp.ty = ty
        ()
      end

      RECORD_ELEMENT_EXPRESSION(__)  => begin
        @assign exp.ty = ty
        ()
      end

      PARTIAL_FUNCTION_APPLICATION_EXPRESSION(__)  => begin
        @assign exp.ty = ty
        ()
      end

      _  => begin
        ()
      end
    end
  end
  exp
end

function typeOf(exp::Expression) ::M_Type
  local ty::M_Type

  @assign ty = begin
    @match exp begin
      INTEGER(__)  => begin
        TYPE_INTEGER()
      end

      REAL(__)  => begin
        TYPE_REAL()
      end

      STRING(__)  => begin
        TYPE_STRING()
      end

      BOOLEAN(__)  => begin
        TYPE_BOOLEAN()
      end

      ENUM_LITERAL(__)  => begin
        exp.ty
      end

      CLKCONST_EXPRESSION(__)  => begin
        TYPE_CLOCK()
      end

      CREF(__)  => begin
        exp.ty
      end

      TYPENAME(__)  => begin
        exp.ty
      end

      ARRAY_EXPRESSION(__)  => begin
        exp.ty
      end

      RANGE_EXPRESSION(__)  => begin
        exp.ty
      end

      TUPLE_EXPRESSION(__)  => begin
        exp.ty
      end

      RECORD_EXPRESSION(__)  => begin
        exp.ty
      end

      CALL_EXPRESSION(__)  => begin
        P_Call.typeOf(exp.call)
      end

      SIZE_EXPRESSION(__)  => begin
        if isSome(exp.dimIndex)
          TYPE_INTEGER()
        else
          Type.sizeType(typeOf(exp.exp))
        end
      end

      END(__)  => begin
        TYPE_INTEGER()
      end

      BINARY_EXPRESSION(__)  => begin
        P_Operator.Operator.typeOf(exp.operator)
      end

      UNARY_EXPRESSION(__)  => begin
        P_Operator.Operator.typeOf(exp.operator)
      end

      LBINARY_EXPRESSION(__)  => begin
        P_Operator.Operator.typeOf(exp.operator)
      end

      LUNARY_EXPRESSION(__)  => begin
        P_Operator.Operator.typeOf(exp.operator)
      end

      RELATION_EXPRESSION(__)  => begin
        P_Operator.Operator.typeOf(exp.operator)
      end

      IF_EXPRESSION(__)  => begin
        typeOf(exp.trueBranch)
      end

      CAST_EXPRESSION(__)  => begin
        exp.ty
      end

      UNBOX_EXPRESSION(__)  => begin
        exp.ty
      end

      SUBSCRIPTED_EXP_EXPRESSION(__)  => begin
        exp.ty
      end

      TUPLE_ELEMENT_EXPRESSION(__)  => begin
        exp.ty
      end

      RECORD_ELEMENT_EXPRESSION(__)  => begin
        exp.ty
      end

      BOX_EXPRESSION(__)  => begin
        TYPE_METABOXED(typeOf(exp.exp))
      end

      MUTABLE_EXPRESSION(__)  => begin
        typeOf(P_Pointer.access(exp.exp))
      end

      EMPTY(__)  => begin
        exp.ty
      end

      PARTIAL_FUNCTION_APPLICATION_EXPRESSION(__)  => begin
        exp.ty
      end

      BINDING_EXP(__)  => begin
        exp.bindingType
      end

      _  => begin
        TYPE_UNKNOWN()
      end
    end
  end
  ty
end

function compareList(expl1::List{<:Expression}, expl2::List{<:Expression}) ::Integer
  local comp::Integer

  local e2::Expression
  local rest_expl2::List{Expression} = expl2

  #=  Check that the lists have the same length, otherwise they can't be equal.
  =#
  @assign comp = Util.intCompare(listLength(expl1), listLength(expl2))
  if comp != 0
    return comp
  end
  for e1 in expl1
    @match _cons(e2, rest_expl2) = rest_expl2
    @assign comp = compare(e1, e2)
    if comp != 0
      return comp
    end
  end
  #=  Return if the expressions are not equal.
  =#
  @assign comp = 0
  comp
end

function compareOpt(expl1::Option{<:Expression}, expl2::Option{<:Expression}) ::Integer
  local comp::Integer

  local e1::Expression
  local e2::Expression

  @assign comp = begin
    @match (expl1, expl2) begin
      (NONE(), NONE())  => begin
        0
      end

      (NONE(), _)  => begin
        -1
      end

      (_, NONE())  => begin
        1
      end

      (SOME(e1), SOME(e2))  => begin
        compare(e1, e2)
      end
    end
  end
  comp
end

""" #= Checks whether two expressions are equal, and returns 0 if they are.
               If the first expression is 'less' than the second it returns an integer
               less than 0, otherwise an integer greater than 0. =#"""
                 function compare(exp1::Expression, exp2::Expression) ::Integer
                   local comp::Integer

                   #=  Check if the expressions are the same object.
                   =#
                   if referenceEq(exp1, exp2)
                     @assign comp = 0
                     return comp
                   end
                   #=  Return false if the expressions are of different kinds.
                   =#
                   @assign comp = Util.intCompare(valueConstructor(exp1), valueConstructor(exp2))
                   if comp != 0
                     return comp
                   end
                   @assign comp = begin
                     local i::Integer
                     local r::AbstractFloat
                     local s::String
                     local b::Bool
                     local cr::ComponentRef
                     local ty::M_Type
                     local expl::List{Expression}
                     local e1::Expression
                     local e2::Expression
                     local e3::Expression
                     local oe::Option{Expression}
                     local p::Path
                     local op::Operator
                     local c::Call
                     local subs::List{Subscript}
                     local clk1::ClockKind
                     local clk2::ClockKind
                     local me::Pointer{Expression}
                     @match exp1 begin
                       INTEGER(__)  => begin
                         @match INTEGER(value = i) = exp2
                         Util.intCompare(exp1.value, i)
                       end

                       REAL(__)  => begin
                         @match REAL(value = r) = exp2
                         Util.realCompare(exp1.value, r)
                       end

                       STRING(__)  => begin
                         @match STRING(value = s) = exp2
                         Util.stringCompare(exp1.value, s)
                       end

                       BOOLEAN(__)  => begin
                         @match BOOLEAN(value = b) = exp2
                         Util.boolCompare(exp1.value, b)
                       end

                       ENUM_LITERAL(__)  => begin
                         @match ENUM_LITERAL(ty = ty, index = i) = exp2
                         @assign comp = AbsynUtil.pathCompare(Type.enumName(exp1.ty), Type.enumName(ty))
                         if comp == 0
                           @assign comp = Util.intCompare(exp1.index, i)
                         end
                         comp
                       end

                       CREF(__)  => begin
                         @match CREF(cref = cr) = exp2
                         compare(exp1.cref, cr)
                       end

                       TYPENAME(__)  => begin
                         @match TYPENAME(ty = ty) = exp2
                         valueCompare(exp1.ty, ty)
                       end

                       ARRAY_EXPRESSION(__)  => begin
                         @match ARRAY_EXPRESSION(ty = ty, elements = expl) = exp2
                         @assign comp = valueCompare(ty, exp1.ty)
                         if comp == 0
                           compareList(exp1.elements, expl)
                         else
                           comp
                         end
                       end

                       RANGE_EXPRESSION(__)  => begin
                         @match RANGE_EXPRESSION(start = e1, step = oe, stop = e2) = exp2
                         @assign comp = compare(exp1.start, e1)
                         if comp == 0
                           @assign comp = compare(exp1.stop, e2)
                           if comp == 0
                             @assign comp = compareOpt(exp1.step, oe)
                           end
                         end
                         comp
                       end

                       TUPLE_EXPRESSION(__)  => begin
                         @match TUPLE_EXPRESSION(elements = expl) = exp2
                         compareList(exp1.elements, expl)
                       end

                       RECORD_EXPRESSION(__)  => begin
                         @match RECORD_EXPRESSION(path = p, elements = expl) = exp2
                         @assign comp = AbsynUtil.pathCompare(exp1.path, p)
                         if comp == 0
                           compareList(exp1.elements, expl)
                         else
                           comp
                         end
                       end

                       CALL_EXPRESSION(__)  => begin
                         @match CALL_EXPRESSION(call = c) = exp2
                         P_Call.compare(exp1.call, c)
                       end

                       SIZE_EXPRESSION(__)  => begin
                         @match SIZE_EXPRESSION(exp = e1, dimIndex = oe) = exp2
                         @assign comp = compareOpt(exp1.dimIndex, oe)
                         if comp == 0
                           compare(exp1.exp, e1)
                         else
                           comp
                         end
                       end

                       END(__)  => begin
                         0
                       end

                       BINARY_EXPRESSION(__)  => begin
                         @match BINARY_EXPRESSION(exp1 = e1, operator = op, exp2 = e2) = exp2
                         @assign comp = P_Operator.Operator.compare(exp1.operator, op)
                         if comp == 0
                           @assign comp = compare(exp1.exp1, e1)
                           if comp == 0
                             @assign comp = compare(exp1.exp2, e2)
                           end
                         end
                         comp
                       end

                       UNARY_EXPRESSION(__)  => begin
                         @match UNARY_EXPRESSION(operator = op, exp = e1) = exp2
                         @assign comp = P_Operator.Operator.compare(exp1.operator, op)
                         if comp == 0
                           compare(exp1.exp, e1)
                         else
                           comp
                         end
                       end

                       LBINARY_EXPRESSION(__)  => begin
                         @match LBINARY_EXPRESSION(exp1 = e1, operator = op, exp2 = e2) = exp2
                         @assign comp = P_Operator.Operator.compare(exp1.operator, op)
                         if comp == 0
                           @assign comp = compare(exp1.exp1, e1)
                           if comp == 0
                             @assign comp = compare(exp1.exp2, e2)
                           end
                         end
                         comp
                       end

LUNARY_EXPRESSION(__)  => begin
  @match LUNARY_EXPRESSION(operator = op, exp = e1) = exp2
  @assign comp = P_Operator.Operator.compare(exp1.operator, op)
  if comp == 0
    compare(exp1.exp, e1)
  else
    comp
  end
end

RELATION_EXPRESSION(__)  => begin
  @match RELATION_EXPRESSION(exp1 = e1, operator = op, exp2 = e2) = exp2
  @assign comp = P_Operator.Operator.compare(exp1.operator, op)
  if comp == 0
    @assign comp = compare(exp1.exp1, e1)
    if comp == 0
      @assign comp = compare(exp1.exp2, e2)
    end
  end
  comp
end

IF_EXPRESSION(__)  => begin
  @match IF_EXPRESSION(condition = e1, trueBranch = e2, falseBranch = e3) = exp2
  @assign comp = compare(exp1.condition, e1)
  if comp == 0
    @assign comp = compare(exp1.trueBranch, e2)
    if comp == 0
      @assign comp = compare(exp1.falseBranch, e3)
    end
  end
  comp
end

UNBOX_EXPRESSION(__)  => begin
  @match UNBOX_EXPRESSION(exp = e1) = exp2
  compare(exp1.exp, e1)
end

CAST_EXPRESSION(__)  => begin
  @assign e1 = begin
    @match exp2 begin
      CAST_EXPRESSION(exp = e1)  => begin
        e1
      end

      e1  => begin
        e1
      end
    end
  end
  compare(exp1.exp, e1)
end

SUBSCRIPTED_EXP_EXPRESSION(__)  => begin
  @match SUBSCRIPTED_EXP_EXPRESSION(exp = e1, subscripts = subs) = exp2
  @assign comp = compare(exp1.exp, e1)
  if comp == 0
    @assign comp = compareList(exp1.subscripts, subs)
  end
  comp
end

TUPLE_ELEMENT_EXPRESSION(__)  => begin
  @match TUPLE_ELEMENT_EXPRESSION(tupleExp = e1, index = i) = exp2
  @assign comp = Util.intCompare(exp1.index, i)
  if comp == 0
    @assign comp = compare(exp1.tupleExp, e1)
  end
  comp
end

RECORD_ELEMENT_EXPRESSION(__)  => begin
  @match RECORD_ELEMENT_EXPRESSION(recordExp = e1, index = i) = exp2
  @assign comp = Util.intCompare(exp1.index, i)
  if comp == 0
    @assign comp = compare(exp1.recordExp, e1)
  end
  comp
end

BOX_EXPRESSION(__)  => begin
  @match BOX_EXPRESSION(exp = e2) = exp2
  compare(exp1.exp, e2)
end

MUTABLE_EXPRESSION(__)  => begin
  @match MUTABLE_EXPRESSION(exp = me) = exp2
  compare(P_Pointer.access(exp1.exp), P_Pointer.access(me))
end

EMPTY(__)  => begin
  @match EMPTY(ty = ty) = exp2
  valueCompare(exp1.ty, ty)
end

CLKCONST_EXPRESSION(clk1)  => begin
  @match CLKCONST_EXPRESSION(clk2) = exp2
  P_ClockKind.compare(clk1, clk2)
end

PARTIAL_FUNCTION_APPLICATION_EXPRESSION(__)  => begin
  @match PARTIAL_FUNCTION_APPLICATION_EXPRESSION(fn = cr, args = expl) = exp2
  @assign comp = compare(exp1.fn, cr)
  if comp == 0
    @assign comp = compareList(exp1.args, expl)
  end
  comp
end

BINDING_EXP(__)  => begin
  @match BINDING_EXP(exp = e2) = exp2
  compare(exp1.exp, e2)
end

_  => begin
  Error.assertion(false, getInstanceName() + " got unknown expression.", sourceInfo())
  fail()
end
end
end
comp
end

""" #= Returns true if the two expressions are equal, otherwise false. =#"""
function isEqual(exp1::Expression, exp2::Expression) ::Bool
  local isEqual::Bool

  @assign isEqual = 0 == compare(exp1, exp2)
  isEqual
end

function isFalse(exp::Expression) ::Bool
  local isTrue::Bool

  @assign isTrue = begin
    @match exp begin
      BOOLEAN(false)  => begin
        true
      end

      _  => begin
        false
      end
    end
  end
  isTrue
end

function isAllTrue(exp::Expression) ::Bool
  local isTrue::Bool

  @assign isTrue = begin
    @match exp begin
      BOOLEAN(true)  => begin
        true
      end

      ARRAY_EXPRESSION(__)  => begin
        for e in exp.elements
          if ! isAllTrue(e)
            @assign isTrue = false
            return
          end
        end
        true
      end

      _  => begin
        false
      end
    end
  end
  isTrue
end

function isTrue(exp::Expression) ::Bool
  local isTrue::Bool

  @assign isTrue = begin
    @match exp begin
      BOOLEAN(true)  => begin
        true
      end

      _  => begin
        false
      end
    end
  end
  isTrue
end

function isCall(exp::Expression) ::Bool
  local isCall::Bool
  @assign isCall = begin
    @match exp begin
      CALL_EXPRESSION(__)  => begin
        true
      end
      _  => begin
        false
      end
    end
  end
  isCall
end

function isWildCref(exp::Expression) ::Bool
  local wild::Bool

  @assign wild = begin
    @match exp begin
      CREF(cref = WILD(__))  => begin
        true
      end

      _  => begin
        false
      end
    end
  end
  wild
end

function isCref(exp::Expression) ::Bool
  local isCref::Bool

  @assign isCref = begin
    @match exp begin
      CREF(__)  => begin
        true
      end

      _  => begin
        false
      end
    end
  end
  isCref
end

function isEmptyArray(exp::Expression) ::Bool
  local emptyArray::Bool

  @assign emptyArray = begin
    @match exp begin
      ARRAY_EXPRESSION(elements =  nil())  => begin
        true
      end

      _  => begin
        false
      end
    end
  end
  emptyArray
end

function isArray(exp::Expression) ::Bool
  local isArray::Bool

  @assign isArray = begin
    @match exp begin
      ARRAY_EXPRESSION(__)  => begin
        true
      end

      _  => begin
        false
      end
    end
  end
  isArray
end

@UniontypeDecl ClockKind

function toString(ck::ClockKind) ::String
  local str::String

  @assign str = begin
    local e1::Expression
    local e2::Expression
    @match ck begin
      INFERRED_CLOCK(__)  => begin
        ""
      end

      INTEGER_CLOCK(e1, e2)  => begin
        toString(e1) + ", " + toString(e2)
      end

      REAL_CLOCK(e1)  => begin
        toString(e1)
      end

      BOOLEAN_CLOCK(e1, e2)  => begin
        toString(e1) + ", " + toString(e2)
      end

      SOLVER_CLOCK(e1, e2)  => begin
        toString(e1) + ", " + toString(e2)
      end
    end
  end
  @assign str = "Clock(" + str + ")"
  str
end

function toDebugString(ick::ClockKind) ::String
  local ock::String

  @assign ock = begin
    local i::Expression
    local ic::Expression
    local r::Expression
    local c::Expression
    local si::Expression
    local sm::Expression
    @match ick begin
      INFERRED_CLOCK(__)  => begin
        "INFERRED_CLOCK()"
      end

      INTEGER_CLOCK(i, r)  => begin
        "INTEGER_CLOCK(" + toString(i) + ", " + toString(r) + ")"
      end

      REAL_CLOCK(i)  => begin
        "REAL_CLOCK(" + toString(i) + ")"
      end

      BOOLEAN_CLOCK(c, si)  => begin
        "BOOLEAN_CLOCK(" + toString(c) + ", " + toString(si) + ")"
      end

      SOLVER_CLOCK(c, sm)  => begin
        "SOLVER_CLOCK(" + toString(c) + ", " + toString(sm) + ")"
      end
    end
  end
  ock
end

function toDAE(ick::ClockKind) ::DAE.P_ClockKind
  local ock::DAE.P_ClockKind

  @assign ock = begin
    local i::Expression
    local ic::Expression
    local r::Expression
    local c::Expression
    local si::Expression
    local sm::Expression
    @match ick begin
      INFERRED_CLOCK(__)  => begin
        DAE.INFERRED_CLOCK()
      end

      INTEGER_CLOCK(i, r)  => begin
        DAE.INTEGER_CLOCK(toDAE(i), toDAE(r))
      end

      REAL_CLOCK(i)  => begin
        DAE.REAL_CLOCK(toDAE(i))
      end

      BOOLEAN_CLOCK(c, si)  => begin
        DAE.BOOLEAN_CLOCK(toDAE(c), toDAE(si))
      end

      SOLVER_CLOCK(c, sm)  => begin
        DAE.SOLVER_CLOCK(toDAE(c), toDAE(sm))
      end
    end
  end
  ock
end

function compare(ck1::ClockKind, ck2::ClockKind) ::Integer
  local comp::Integer

  @assign comp = begin
    local i1::Expression
    local ic1::Expression
    local r1::Expression
    local c1::Expression
    local si1::Expression
    local sm1::Expression
    local i2::Expression
    local ic2::Expression
    local r2::Expression
    local c2::Expression
    local si2::Expression
    local sm2::Expression
    @match (ck1, ck2) begin
      (INFERRED_CLOCK(__), INFERRED_CLOCK(__))  => begin
        0
      end

      (INTEGER_CLOCK(i1, r1), INTEGER_CLOCK(i2, r2))  => begin
        @assign comp = compare(i1, i2)
        if comp == 0
          @assign comp = compare(r1, r2)
        end
        comp
      end

      (REAL_CLOCK(i1), REAL_CLOCK(i2))  => begin
        compare(i1, i2)
      end

      (BOOLEAN_CLOCK(c1, si1), BOOLEAN_CLOCK(c2, si2))  => begin
        @assign comp = compare(c1, c2)
        if comp == 0
          @assign comp = compare(si1, si2)
        end
        comp
      end

      (SOLVER_CLOCK(c1, sm2), SOLVER_CLOCK(c2, sm1))  => begin
        @assign comp = compare(c1, c2)
        if comp == 0
          @assign comp = compare(sm1, sm2)
        end
        comp
      end
    end
  end
  comp
end

#= Forward declarations for uniontypes until Julia adds support for mutual recursion =#

PredFunc = Function
FoldFunc = Function
MapFunc = Function
MapFunc = Function
@UniontypeDecl Binding

function containsExp(binding::Binding, predFn::PredFunc)::Bool
  local res::Bool

  @assign res = begin
    @match binding begin
      UNTYPED_BINDING(__) => begin
        contains(binding.bindingExp, predFn)
      end

      TYPED_BINDING(__) => begin
        contains(binding.bindingExp, predFn)
      end

      FLAT_BINDING(__) => begin
        contains(binding.bindingExp, predFn)
      end

      CEVAL_BINDING(__) => begin
        contains(binding.bindingExp, predFn)
      end

      _ => begin
        false
      end
    end
  end
  return res
end

function foldExp(binding::Binding, foldFn::FoldFunc, arg::ArgT) where {ArgT}

  @assign arg = begin
    @match binding begin
      UNTYPED_BINDING(__) => begin
        fold(binding.bindingExp, foldFn, arg)
      end

      TYPED_BINDING(__) => begin
        fold(binding.bindingExp, foldFn, arg)
      end

      FLAT_BINDING(__) => begin
        fold(binding.bindingExp, foldFn, arg)
      end

      CEVAL_BINDING(__) => begin
        fold(binding.bindingExp, foldFn, arg)
      end

      _ => begin
        arg
      end
    end
  end
  return arg
end

function mapExpShallow(binding::Binding, mapFn::MapFunc)::Binding

  local e1::Expression
  local e2::Expression

  @assign () = begin
    @match binding begin
      UNTYPED_BINDING(bindingExp = e1) => begin
        @assign e2 = mapFn(e1)
        if !referenceEq(e1, e2)
          @assign binding.bindingExp = e2
        end
        ()
      end

      TYPED_BINDING(bindingExp = e1) => begin
        @assign e2 = mapFn(e1)
        if !referenceEq(e1, e2)
          @assign binding.bindingExp = e2
        end
        ()
      end

      FLAT_BINDING(bindingExp = e1) => begin
        @assign e2 = mapFn(e1)
        if !referenceEq(e1, e2)
          @assign binding.bindingExp = e2
        end
        ()
      end

      CEVAL_BINDING(bindingExp = e1) => begin
        @assign e2 = mapFn(e1)
        if !referenceEq(e1, e2)
          @assign binding.bindingExp = e2
        end
        ()
      end

      _ => begin
        ()
      end
    end
  end
  return binding
end

function mapExp(binding::Binding, mapFn::MapFunc)::Binding

  local e1::Expression
  local e2::Expression

  @assign () = begin
    @match binding begin
      UNTYPED_BINDING(bindingExp = e1) => begin
        @assign e2 = map(e1, mapFn)
        if !referenceEq(e1, e2)
          @assign binding.bindingExp = e2
        end
        ()
      end

      TYPED_BINDING(bindingExp = e1) => begin
        @assign e2 = map(e1, mapFn)
        if !referenceEq(e1, e2)
          @assign binding.bindingExp = e2
        end
        ()
      end

      FLAT_BINDING(bindingExp = e1) => begin
        @assign e2 = map(e1, mapFn)
        if !referenceEq(e1, e2)
          @assign binding.bindingExp = e2
        end
        ()
      end

      CEVAL_BINDING(bindingExp = e1) => begin
        @assign e2 = map(e1, mapFn)
        if !referenceEq(e1, e2)
          @assign binding.bindingExp = e2
        end
        ()
      end

      _ => begin
        ()
      end
    end
  end
  return binding
end

function toDAEExp(binding::Binding)::Option{DAE.Exp}
  local bindingExp::Option{DAE.Exp}

  @assign bindingExp = begin
    @match binding begin
      UNBOUND(__) => begin
        NONE()
      end

      TYPED_BINDING(__) => begin
        SOME(toDAE(binding.bindingExp))
      end

      FLAT_BINDING(__) => begin
        SOME(toDAE(binding.bindingExp))
      end

      CEVAL_BINDING(__) => begin
        NONE()
      end

      _ => begin
        Error.assertion(false, getInstanceName() + " got untyped binding", sourceInfo())
        fail()
      end
    end
  end
  return bindingExp
end

function makeDAEBinding(exp::Expression, var::VariabilityType)::DAE.P_Binding
  local binding::DAE.Binding
  @assign binding = DAE.EQBOUND(
    toDAE(exp),
    NONE(),
    Variability.variabilityToDAEConst(var),
    DAE.BINDING_FROM_DEFAULT_VALUE(),
  )
  #=  TODO: revise this. =#
  return binding
end

function toDAE(binding::Binding)::DAE.P_Binding
  local outBinding::DAE.P_Binding

  @assign outBinding = begin
    @match binding begin
      UNBOUND(__) => begin
        DAE.UNBOUND()
      end

      TYPED_BINDING(__) => begin
        makeDAEBinding(binding.bindingExp, binding.variability)
      end

      FLAT_BINDING(__) => begin
        makeDAEBinding(binding.bindingExp, binding.variability)
      end

      CEVAL_BINDING(__) => begin
        DAE.UNBOUND()
      end

      INVALID_BINDING(__) => begin
        Error.addTotalMessages(binding.errors)
        fail()
      end

      _ => begin
        Error.assertion(false, getInstanceName() + " got untyped binding", sourceInfo())
        fail()
      end
    end
  end
  return outBinding
end

function isEqual(binding1::Binding, binding2::Binding)::Bool
  local equal::Bool
  @assign equal = begin
    @match (binding1, binding2) begin
      (UNBOUND(__), UNBOUND(__)) => begin
        true
      end
      (RAW_BINDING(__), RAW_BINDING(__)) => begin
        AbsynUtil.expEqual(binding1.bindingExp, binding2.bindingExp)
      end
      (UNTYPED_BINDING(__), UNTYPED_BINDING(__)) => begin
        isEqual(binding1.bindingExp, binding2.bindingExp)
      end
      (TYPED_BINDING(__), TYPED_BINDING(__)) => begin
        isEqual(binding1.bindingExp, binding2.bindingExp)
      end
      _ => begin
        false
      end
    end
  end
  #=  TODO: Handle propagated dims.
  =#
  return equal
end

function toFlatString(binding::Binding, prefix::String = "")::String
  local string::String

  @assign string = begin
    @match binding begin
      UNBOUND(__) => begin
        ""
      end

      RAW_BINDING(__) => begin
        prefix + Dump.printExpStr(binding.bindingExp)
      end

      UNTYPED_BINDING(__) => begin
        prefix + toFlatString(binding.bindingExp)
      end

      TYPED_BINDING(__) => begin
        prefix + toFlatString(binding.bindingExp)
      end

      FLAT_BINDING(__) => begin
        prefix + toFlatString(binding.bindingExp)
      end

      CEVAL_BINDING(__) => begin
        prefix + toFlatString(binding.bindingExp)
      end

      INVALID_BINDING(__) => begin
        toFlatString(binding.binding, prefix)
      end
    end
  end
  return string
end

function toString(binding::Binding, prefix::String = "")::String
  local string::String

  @assign string = begin
    @match binding begin
      UNBOUND(__) => begin
        ""
      end

      RAW_BINDING(__) => begin
        prefix + Dump.printExpStr(binding.bindingExp)
      end

      UNTYPED_BINDING(__) => begin
        prefix + toString(binding.bindingExp)
      end

      TYPED_BINDING(__) => begin
        prefix + toString(binding.bindingExp)
      end

      FLAT_BINDING(__) => begin
        prefix + toString(binding.bindingExp)
      end

      CEVAL_BINDING(__) => begin
        prefix + toString(binding.bindingExp)
      end

      INVALID_BINDING(__) => begin
        toString(binding.binding, prefix)
      end
    end
  end
  return string
end

""" #= Returns the number of dimensions that the binding was propagated through to
     get to the element it belongs to. =#"""
function propagatedDimCount(binding::Binding)::Integer
  local count::Integer

  @assign count = begin
    @match binding begin
      UNTYPED_BINDING(__) => begin
        propagatedDimCount(binding.bindingExp)
      end

      TYPED_BINDING(__) => begin
        propagatedDimCount(binding.bindingExp)
      end

      _ => begin
        0
      end
    end
  end
  return count
end

function isClassBinding(binding::Binding)::Bool
  for parent in parents(binding)
    if isClass(parent)
      return true
    end
  end
  return false
end

function addParent(parent::InstNode, binding::Binding)::Binding

  @assign () = begin
    @match binding begin
      UNBOUND(isEach = true) => begin
        @assign binding.parents = _cons(parent, binding.parents)
        ()
      end

      RAW_BINDING(__) => begin
        @assign binding.parents = _cons(parent, binding.parents)
        ()
      end

      _ => begin
        ()
      end
    end
  end
  return binding
end

function parentCount(binding::Binding)::Integer
  local count::Integer = listLength(parents(binding))
  return count
end

function parents(binding::Binding)::List{InstNode}
  local parents::List{InstNode}

  @assign parents = begin
    @match binding begin
      UNBOUND(__) => begin
        binding.parents
      end

      RAW_BINDING(__) => begin
        binding.parents
      end

      UNTYPED_BINDING(
        bindingExp = BINDING_EXP(parents = parents),
      ) => begin
        parents
      end

      TYPED_BINDING(bindingExp = BINDING_EXP(parents = parents)) => begin
        parents
      end

      CEVAL_BINDING(bindingExp = BINDING_EXP(parents = parents)) => begin
        parents
      end

      _ => begin
        nil
      end
    end
  end
  return parents
end

function isTyped(binding::Binding)::Bool
  local isTyped::Bool

  @assign isTyped = begin
    @match binding begin
      TYPED_BINDING(__) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return isTyped
end

function isEach(binding::Binding)::Bool
  local isEach::Bool

  @assign isEach = begin
    @match binding begin
      UNBOUND(__) => begin
        binding.isEach
      end

      RAW_BINDING(__) => begin
        binding.isEach
      end

      UNTYPED_BINDING(__) => begin
        binding.isEach
      end

      TYPED_BINDING(__) => begin
        binding.eachType == EachType.EACH
      end

      _ => begin
        false
      end
    end
  end
  return isEach
end

function getType(binding::Binding)::NFType
  local ty::NFType
  @assign ty = begin
    @match binding begin
      UNBOUND(__) => begin
        TYPE_UNKNOWN()
      end
      RAW_BINDING(__) => begin
        TYPE_UNKNOWN()
      end

      UNTYPED_BINDING(__) => begin
        TYPE_UNKNOWN()
      end

      TYPED_BINDING(__) => begin
        binding.bindingType
      end

      FLAT_BINDING(__) => begin
        typeOf(binding.bindingExp)
      end

      CEVAL_BINDING(__) => begin
        typeOf(binding.bindingExp)
      end

      INVALID_BINDING(__) => begin
        getType(binding.binding)
      end
    end
  end
  return ty
end

function getInfo(binding::Binding)::SourceInfo
  local info::SourceInfo

  @assign info = begin
    @match binding begin
      UNBOUND(__) => begin
        binding.info
      end

      RAW_BINDING(__) => begin
        binding.info
      end

      UNTYPED_BINDING(__) => begin
        binding.info
      end

      TYPED_BINDING(__) => begin
        binding.info
      end

      _ => begin
        AbsynUtil.dummyInfo
      end
    end
  end
  return info
end

function variability(binding::Binding)::VariabilityType
  local var::VariabilityType

  @assign var = begin
    @match binding begin
      TYPED_BINDING(__) => begin
        binding.variability
      end

      FLAT_BINDING(__) => begin
        binding.variability
      end

      _ => begin
        #Error.assertion(false, getInstanceName() + " got unknown binding", sourceInfo())
        @error "Got Unknown binding!"
        fail()
      end
    end
  end
  return var
end

function recordFieldBinding(fieldNode::InstNode, recordBinding::Binding)::Binding
  local fieldBinding::Binding = recordBinding

  local exp::Expression
  local ty::M_Type
  local var::VariabilityType
  local field_name::String = name(fieldNode)

  @assign fieldBinding = begin
    @match fieldBinding begin
      UNTYPED_BINDING(__) => begin
        @assign fieldBinding.bindingExp =
          recordElement(field_name, fieldBinding.bindingExp)
        fieldBinding
      end

      TYPED_BINDING(__) => begin
        @assign exp =
          recordElement(field_name, fieldBinding.bindingExp)
        @assign exp = addBindingExpParent(fieldNode, exp)
        @assign ty = typeOf(exp)
        @assign var = variability(exp)
        TYPED_BINDING(
          exp,
          ty,
          var,
          fieldBinding.eachType,
          fieldBinding.evaluated,
          fieldBinding.isFlattened,
          fieldBinding.info,
        )
      end

      FLAT_BINDING(__) => begin
        @assign exp =
          recordElement(field_name, fieldBinding.bindingExp)
        @assign var = variability(exp)
        FLAT_BINDING(exp, var)
      end

      CEVAL_BINDING(__) => begin
        @assign fieldBinding.bindingExp =
          recordElement(field_name, fieldBinding.bindingExp)
        fieldBinding
      end
    end
  end
  return fieldBinding
end

function isCrefExp(binding::Binding)::Bool
  local isCref::Bool

  @assign isCref = begin
    @match binding begin
      TYPED_BINDING(__) => begin
        isCref(getBindingExp(binding.bindingExp))
      end

      _ => begin
        false
      end
    end
  end
  return isCref
end

function isRecordExp(binding::Binding)::Bool
  local isRecordExp::Bool

  @assign isRecordExp = begin
    @match binding begin
      TYPED_BINDING(__) => begin
        isRecord(getBindingExp(binding.bindingExp))
      end

      _ => begin
        false
      end
    end
  end
  return isRecordExp
end

function setExp(exp::Expression, binding::Binding)::Binding

  @assign () = begin
    @match binding begin
      UNTYPED_BINDING(__) => begin
        @assign binding.bindingExp = exp
        ()
      end

      TYPED_BINDING(__) => begin
        @assign binding.bindingExp = exp
        ()
      end

      FLAT_BINDING(__) => begin
        @assign binding.bindingExp = exp
        ()
      end
    end
  end
  return binding
end

function getExp(binding::Binding)::Expression
  local exp::Expression

  @assign exp = begin
    @match binding begin
      UNTYPED_BINDING(__) => begin
        binding.bindingExp
      end

      TYPED_BINDING(__) => begin
        binding.bindingExp
      end

      FLAT_BINDING(__) => begin
        binding.bindingExp
      end
    end
  end
  return exp
end

function hasExp(binding::Binding)::Bool
  local hasExp::Bool

  @assign hasExp = begin
    @match binding begin
      UNTYPED_BINDING(__) => begin
        true
      end

      TYPED_BINDING(__) => begin
        true
      end

      FLAT_BINDING(__) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return hasExp
end

function setTypedExp(exp::Expression, binding::Binding)::Binding

  local ty1::M_Type
  local ty2::M_Type

  @assign () = begin
    @match binding begin
      TYPED_BINDING(__) => begin
        @assign binding.bindingExp = exp
        ()
      end

      FLAT_BINDING(__) => begin
        @assign binding.bindingExp = exp
        ()
      end
    end
  end
  return binding
end

function getTypedExp(binding::Binding)::Expression
  local exp::Expression

  @assign exp = begin
    @match binding begin
      TYPED_BINDING(__) => begin
        binding.bindingExp
      end

      FLAT_BINDING(__) => begin
        binding.bindingExp
      end
    end
  end
  return exp
end

function getUntypedExp(binding::Binding)::Expression
  local exp::Expression

  @match UNTYPED_BINDING(bindingExp = exp) = binding
  return exp
end

function typedExp(binding::Binding)::Option{Expression}
  local exp::Option{Expression}

  @assign exp = begin
    @match binding begin
      TYPED_BINDING(__) => begin
        SOME(binding.bindingExp)
      end

      FLAT_BINDING(__) => begin
        SOME(binding.bindingExp)
      end

      _ => begin
        NONE()
      end
    end
  end
  return exp
end

function untypedExp(binding::Binding)::Option{Expression}
  local exp::Option{Expression}

  @assign exp = begin
    @match binding begin
      UNTYPED_BINDING(__) => begin
        SOME(binding.bindingExp)
      end

      _ => begin
        NONE()
      end
    end
  end
  return exp
end

function isUnbound(binding::Binding)::Bool
  local isUnbound::Bool

  @assign isUnbound = begin
    @match binding begin
      UNBOUND(__) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return isUnbound
end

function isExplicitlyBound(binding::Binding)::Bool
  local isBound::Bool

  @assign isBound = begin
    @match binding begin
      UNBOUND(__) => begin
        false
      end

      CEVAL_BINDING(__) => begin
        false
      end

      _ => begin
        true
      end
    end
  end
  return isBound
end

function isBound(binding::Binding)::Bool
  local isBound::Bool

  @assign isBound = begin
    @match binding begin
      UNBOUND(__) => begin
        false
      end

      _ => begin
        true
      end
    end
  end
  return isBound
end

function fromAbsyn(
  bindingExp::Option{<:Absyn.Exp},
  eachPrefix::Bool,
  parents::List{<:InstNode},
  scope::InstNode,
  info::SourceInfo,
)::Binding
  local binding::Binding
  @assign binding = begin
    local exp::Absyn.Exp
    @match bindingExp begin
      SOME(exp) => begin
        RAW_BINDING(exp, scope, parents, eachPrefix, info)
      end
      _ => begin
        if eachPrefix
          UNBOUND(parents, true, info)
        else
          EMPTY_BINDING
        end
      end
    end
  end
  return binding
end
