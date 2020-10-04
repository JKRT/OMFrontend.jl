module P_NFExpandExp

using MetaModelica
using ExportAll
#= Forward declarations for uniontypes until Julia adds support for mutual recursion =#

MakeFn = Function
@UniontypeDecl NFExpandExp

import ..P_NFExpression
P_Expression = P_NFExpression
Expression = P_NFExpression.NFExpression
import ..P_EvalTarget
P_EvalTarget = P_EvalTarget
EvalTarget = P_EvalTarget.EvalTarget
using MetaModelica.Dangerous
import ..NFPrefixes.Variability
import ..NFSimplifyExp
SimplifyExp = NFSimplifyExp
import ..NFInstNode.P_InstNode
import ..NFCeval
Ceval = NFCeval
import ..P_NFOperator
P_Operator = P_NFOperator
Operator = P_NFOperator.NFOperator
import ..NFFunction.P_Function
import ..P_NFComponentRef
P_ComponentRef = P_NFComponentRef
ComponentRef = P_NFComponentRef.NFComponentRef
import ..P_NFDimension
P_Dimension = P_NFDimension
Dimension = P_NFDimension.NFDimension
import ..NFCall.P_Call
import ..P_NFType
P_M_Type = P_NFType
M_Type = NFType
import ..P_NFSubscript
P_Subscript = P_NFSubscript
Subscript = P_NFSubscript.NFSubscript
import ..P_NFExpressionIterator
P_ExpressionIterator = P_NFExpressionIterator
ExpressionIterator = P_NFExpressionIterator.NFExpressionIterator
import ..P_NFRangeIterator
P_RangeIterator = P_NFRangeIterator
RangeIterator = P_NFRangeIterator.NFRangeIterator

function expandGeneric2(
  subs::List{<:List{<:Subscript}},
  exp::Expression,
  ty::M_Type,
  accum::List{<:Subscript} = nil,
)::Expression
  local outExp::Expression

  local t::M_Type
  local sub::List{Subscript}
  local expl::List{Expression}
  local rest_subs::List{List{Subscript}}

  @assign outExp = begin
    @match subs begin
      sub <| rest_subs => begin
        @assign t = Type.unliftArray(ty)
        @assign expl =
          List(expandGeneric2(rest_subs, exp, t, _cons(s, accum)) for s in sub)
        P_Expression.Expression.makeArray(ty, expl)
      end

      nil() => begin
        @assign outExp = exp
        for s in listReverse(accum)
          @assign outExp = P_Expression.Expression.applySubscript(s, outExp)
        end
        outExp
      end
    end
  end
  return outExp
end

function expandGeneric(exp::Expression)::Tuple{Expression, Bool}
  local expanded::Bool
  local outExp::Expression

  local ty::M_Type
  local dims::List{Dimension}
  local subs::List{List{Subscript}}

  @assign ty = typeOf(exp)
  if isArray(ty)
    @assign expanded = Type.hasKnownSize(ty)
    if expanded
      @assign dims = arrayDims(ty)
      @assign subs = List(
        List(
          SUBSCRIPT_INDEX(e)
          for
          e in
          P_RangeIterator.RangeIterator.toList(P_RangeIterator.RangeIterator.fromDim(d))
        ) for d in dims
      )
      @assign outExp = expandGeneric2(subs, exp, ty)
    else
      @assign outExp = exp
    end
  else
    @assign outExp = exp
    @assign expanded = true
  end
  return (outExp, expanded)
end

function expandCast(exp::Expression, ty::M_Type)::Tuple{Expression, Bool}
  local expanded::Bool
  local outExp::Expression

  @assign (outExp, expanded) = expand(exp)
  if expanded
    @assign outExp = typeCast(outExp, ty)
  else
    @assign outExp = exp
  end
  return (outExp, expanded)
end

function makeLogicalUnaryOp(exp1::Expression, op::Operator)::Expression
  local exp::Expression = LUNARY_EXPRESSION(op, exp1)
  return exp
end

function expandLogicalUnary(exp::Expression, op::Operator)::Tuple{Expression, Bool}
  local expanded::Bool
  local outExp::Expression

  local scalar_op::Operator

  @assign (outExp, expanded) = expand(exp)
  @assign scalar_op = P_Operator.Operator.scalarize(op)
  if expanded
    @assign outExp = mapArrayElements(
      outExp,
      (scalar_op) -> makeLogicalUnaryOp(op = scalar_op),
    )
  else
    @assign outExp = exp
  end
  return (outExp, expanded)
end

function makeLBinaryOp(exp1::Expression, op::Operator, exp2::Expression)::Expression
  local exp::Expression

  if P_Expression.Expression.isScalarLiteral(exp1) &&
     P_Expression.Expression.isScalarLiteral(exp2)
    @assign exp = Ceval.evalLogicBinaryOp(exp1, op, exp2)
  else
    @assign exp = LBINARY_EXPRESSION(exp1, op, exp2)
  end
  return exp
end

function expandLogicalBinary(exp::Expression)::Tuple{Expression, Bool}
  local expanded::Bool
  local outExp::Expression

  local exp1::Expression
  local exp2::Expression
  local op::Operator

  @match LBINARY_EXPRESSION(exp1 = exp1, operator = op, exp2 = exp2) = exp
  if isArray(P_Operator.Operator.typeOf(op))
    @assign (exp1, expanded) = expand(exp1)
    if expanded
      @assign (exp2, expanded) = expand(exp2)
    end
    if expanded
      @assign outExp = expandBinaryElementWise2(exp1, op, exp2, makeLBinaryOp)
    else
      @assign outExp = exp
    end
  else
    @assign outExp = exp
    @assign expanded = true
  end
  return (outExp, expanded)
end

function expandUnary(exp::Expression, op::Operator)::Tuple{Expression, Bool}
  local expanded::Bool
  local outExp::Expression

  local scalar_op::Operator

  @assign (outExp, expanded) = expand(exp)
  @assign scalar_op = P_Operator.Operator.scalarize(op)
  if expanded
    @assign outExp = mapArrayElements(
      outExp,
      (scalar_op) -> SimplifyExp.simplifyUnaryOp(op = scalar_op),
    )
  end
  return (outExp, expanded)
end

function expandBinaryPowMatrix2(matrix::Expression, n::Integer)::Expression
  local exp::Expression

  @assign exp = begin
    @match n begin
      1 => begin
        matrix
      end

      2 => begin
        makeBinaryMatrixProduct(matrix, matrix)
      end

      _ where {(intMod(n, 2) == 0)} => begin
        #=  A^1 = A
        =#
        #=  A^2 = A * A
        =#
        #=  A^n = A^m * A^m where n = 2*m
        =#
        @assign exp = expandBinaryPowMatrix2(matrix, intDiv(n, 2))
        makeBinaryMatrixProduct(exp, exp)
      end

      _ => begin
        #=  A^n = A * A^(n-1)
        =#
        @assign exp = expandBinaryPowMatrix2(matrix, n - 1)
        makeBinaryMatrixProduct(matrix, exp)
      end
    end
  end
  return exp
end

function expandBinaryPowMatrix(exp::Expression)::Tuple{Expression, Bool}
  local expanded::Bool
  local outExp::Expression

  local exp1::Expression
  local exp2::Expression
  local op::Operator
  local n::Integer

  @match BINARY_EXPRESSION(exp1 = exp1, operator = op, exp2 = exp2) = exp
  @assign (outExp, expanded) = begin
    @match exp2 begin
      INTEGER_EXPRESSION(0) => begin
        #=  a ^ 0 = identity(size(a, 1))
        =#
        @assign n =
          P_Dimension.Dimension.size(listHead(arrayDims(P_Operator.Operator.typeOf(
            op,
          ))))
        (P_Expression.Expression.makeIdentityMatrix(n, TYPE_REAL()), true)
      end

      INTEGER_EXPRESSION(n) => begin
        #=  a ^ n where n is a literal value.
        =#
        @assign (exp1, expanded) = expand(exp1)
        if expanded
          @assign outExp = expandBinaryPowMatrix2(exp1, n)
        end
        (outExp, expanded)
      end

      _ => begin
        expandGeneric(exp)
      end
    end
  end
  #=  a ^ n where n is unknown, subscript the whole expression.
  =#
  return (outExp, expanded)
end

function makeBinaryMatrixProduct2(
  row::Expression,
  matrix::List{<:Expression},
)::List{Expression}
  local outRow::List{Expression}

  @assign outRow = List(makeScalarProduct(row, e) for e in matrix)
  return outRow
end

function makeBinaryMatrixProduct(exp1::Expression, exp2::Expression)::Expression
  local exp::Expression

  local expl1::List{Expression}
  local expl2::List{Expression}
  local ty::M_Type
  local row_ty::M_Type
  local mat_ty::M_Type
  local n::Dimension
  local p::Dimension

  @match ARRAY_EXPRESSION(ARRAY_TYPE(ty, list(n, _)), expl1) = exp1
  #=  Transpose the second matrix. This makes it easier to do the multiplication,
  =#
  #=  since we can do row-row multiplications instead of row-column.
  =#
  @match ARRAY_EXPRESSION(ARRAY_TYPE(dimensions = list(p, _)), expl2) =
    P_Expression.Expression.transposeArray(exp2)
  @assign mat_ty = ARRAY_TYPE(ty, list(n, p))
  if listEmpty(expl2)
    @assign exp = P_Expression.Expression.makeZero(mat_ty)
  else
    @assign row_ty = ARRAY_TYPE(ty, list(p))
    @assign expl1 = List(
      P_Expression.Expression.makeArray(row_ty, makeBinaryMatrixProduct2(e, expl2))
      for e in expl1
    )
    @assign exp = P_Expression.Expression.makeArray(mat_ty, expl1)
  end
  #=  If any of the matrices' dimensions are zero, the result will be a matrix
  =#
  #=  of zeroes (the default value of sum). Only expl2 needs to be checked here,
  =#
  #=  the normal case can handle expl1 being empty.
  =#
  #=  c[i, j] = a[i, :] * b[:, j] for i in 1:n, j in 1:p.
  =#
  return exp
end

""" #= Expands a matrix*matrix expression, c[n, p] = a[n, m] * b[m, p]. =#"""
function expandBinaryMatrixProduct(exp::Expression)::Tuple{Expression, Bool}
  local expanded::Bool
  local outExp::Expression

  local exp1::Expression
  local exp2::Expression

  @match BINARY_EXPRESSION(exp1 = exp1, exp2 = exp2) = exp
  @assign (exp1, expanded) = expand(exp1)
  if expanded
    @assign (exp2, expanded) = expand(exp2)
  end
  if expanded
    @assign outExp = makeBinaryMatrixProduct(exp1, exp2)
  else
    @assign outExp = exp
  end
  return (outExp, expanded)
end

function makeScalarProduct(exp1::Expression, exp2::Expression)::Expression
  local exp::Expression

  local expl1::List{Expression}
  local expl2::List{Expression}
  local ty::M_Type
  local elem_ty::M_Type
  local mul_op::Operator
  local add_op::Operator

  @match ARRAY_EXPRESSION(ty, expl1) = exp1
  @match ARRAY_EXPRESSION(_, expl2) = exp2
  @assign elem_ty = Type.unliftArray(ty)
  if listEmpty(expl1)
    @assign exp = P_Expression.Expression.makeZero(elem_ty)
  else
    @assign mul_op = P_Operator.Operator.makeMul(elem_ty)
    @assign add_op = P_Operator.Operator.makeAdd(elem_ty)
    @assign expl1 =
      List(@do_threaded_for SimplifyExp.simplifyBinaryOp(e1, mul_op, e2) (e1, e2) (
        expl1,
        expl2,
      ))
    @assign exp =
      ListUtil.reduce(expl1, (add_op) -> SimplifyExp.simplifyBinaryOp(op = add_op))
  end
  #=  Scalar product of two empty arrays. The result is defined in the spec
  =#
  #=  by sum, so we return 0 since that's the default value of sum.
  =#
  return exp
end

""" #= Expands a vector*vector expression, c = a[n] * b[n]. =#"""
function expandBinaryDotProduct(exp::Expression)::Tuple{Expression, Bool}
  local expanded::Bool
  local outExp::Expression

  local exp1::Expression
  local exp2::Expression

  @match BINARY_EXPRESSION(exp1 = exp1, exp2 = exp2) = exp
  @assign (exp1, expanded) = expand(exp1)
  if expanded
    @assign (exp2, expanded) = expand(exp2)
  end
  if expanded
    @assign outExp = makeScalarProduct(exp1, exp2)
  else
    @assign outExp = exp
  end
  return (outExp, expanded)
end

""" #= Expands a matrix*vector expression, c[n] = a[n, m] * b[m]. =#"""
function expandBinaryMatrixVector(exp::Expression)::Tuple{Expression, Bool}
  local expanded::Bool
  local outExp::Expression

  local exp1::Expression
  local exp2::Expression
  local expl::List{Expression}
  local ty::M_Type
  local n::Dimension

  @match BINARY_EXPRESSION(exp1 = exp1, exp2 = exp2) = exp
  @assign (exp1, expanded) = expand(exp1)
  if expanded
    @match ARRAY_EXPRESSION(ARRAY_TYPE(ty, list(n, _)), expl) = exp1
    @assign ty = ARRAY_TYPE(ty, list(n))
    if listEmpty(expl)
      @assign outExp = P_Expression.Expression.makeZero(ty)
    else
      @assign (exp2, expanded) = expand(exp2)
      if expanded
        @assign expl = List(makeScalarProduct(e1, exp2) for e1 in expl)
        @assign outExp = P_Expression.Expression.makeArray(ty, expl)
      else
        @assign outExp = exp
      end
    end
  else
    @assign outExp = exp
  end
  #=  c[i] = a[i, :] * b for i in 1:n
  =#
  return (outExp, expanded)
end

""" #= Expands a vector*matrix expression, c[m] = a[n] * b[n, m]. =#"""
function expandBinaryVectorMatrix(exp::Expression)::Tuple{Expression, Bool}
  local expanded::Bool
  local outExp::Expression

  local exp1::Expression
  local exp2::Expression
  local expl::List{Expression}
  local ty::M_Type
  local m::Dimension

  @match BINARY_EXPRESSION(exp1 = exp1, exp2 = exp2) = exp
  @assign (exp2, expanded) = expand(exp2)
  if expanded
    @match ARRAY_EXPRESSION(ARRAY_TYPE(ty, list(m, _)), expl) =
      P_Expression.Expression.transposeArray(exp2)
    @assign ty = ARRAY_TYPE(ty, list(m))
    if listEmpty(expl)
      @assign outExp = P_Expression.Expression.makeZero(ty)
    else
      @assign (exp1, expanded) = expand(exp1)
      if expanded
        @assign expl = List(makeScalarProduct(exp1, e2) for e2 in expl)
        @assign outExp = P_Expression.Expression.makeArray(ty, expl)
      else
        @assign outExp = exp
      end
    end
  else
    @assign outExp = exp
  end
  #=  c[i] = a * b[:, i] for i in 1:m
  =#
  return (outExp, expanded)
end

function expandBinaryArrayScalar(
  exp::Expression,
  scalarOp::P_NFOperator.Op,
)::Tuple{Expression, Bool}
  local expanded::Bool
  local outExp::Expression

  local exp1::Expression
  local exp2::Expression
  local expl::List{Expression}
  local op::Operator

  @match BINARY_EXPRESSION(exp1 = exp1, operator = op, exp2 = exp2) = exp
  @assign (exp1, expanded) = expand(exp1)
  if expanded
    @assign op = OPERATOR(
      arrayElementType(P_Operator.Operator.typeOf(op)),
      scalarOp,
    )
    @assign outExp = mapArrayElements(
      exp1,
      (op, exp2) -> SimplifyExp.simplifyBinaryOp(op = op, exp2 = exp2),
    )
  else
    @assign outExp = exp
  end
  return (outExp, expanded)
end

function makeScalarArrayBinary_traverser(
  exp1::Expression,
  op::Operator,
  exp2::Expression,
)::Expression
  local exp::Expression

  @assign exp = begin
    @match exp2 begin
      ARRAY_EXPRESSION(__) => begin
        exp2
      end

      _ => begin
        SimplifyExp.simplifyBinaryOp(exp1, op, exp2)
      end
    end
  end
  return exp
end

function expandBinaryScalarArray(
  exp::Expression,
  scalarOp::P_NFOperator.Op,
)::Tuple{Expression, Bool}
  local expanded::Bool
  local outExp::Expression

  local exp1::Expression
  local exp2::Expression
  local expl::List{Expression}
  local op::Operator

  @match BINARY_EXPRESSION(exp1 = exp1, operator = op, exp2 = exp2) = exp
  @assign (exp2, expanded) = expand(exp2)
  if expanded
    @assign op = OPERATOR(
      arrayElementType(P_Operator.Operator.typeOf(op)),
      scalarOp,
    )
    @assign outExp = mapArrayElements(
      exp2,
      (op, exp1) -> SimplifyExp.simplifyBinaryOp(op = op, exp1 = exp1),
    )
  else
    @assign outExp = exp
  end
  return (outExp, expanded)
end

function expandBinaryElementWise2(
  exp1::Expression,
  op::Operator,
  exp2::Expression,
  func::MakeFn,
)::Expression
  local exp::Expression

  local expl1::List{Expression}
  local expl2::List{Expression}
  local expl::List{Expression}
  local ty::M_Type
  local eop::Operator

  @assign expl1 = P_Expression.Expression.arrayElements(exp1)
  @assign expl2 = P_Expression.Expression.arrayElements(exp2)
  @assign ty = P_Operator.Operator.typeOf(op)
  @assign eop = setType(Type.unliftArray(ty), op)
  if Type.dimensionCount(ty) > 1
    @assign expl =
      List(@do_threaded_for expandBinaryElementWise2(e1, eop, e2, func) (e1, e2) (
        expl1,
        expl2,
      ))
  else
    @assign expl = List(@do_threaded_for func(e1, eop, e2) (e1, e2) (expl1, expl2))
  end
  @assign exp = P_Expression.Expression.makeArray(ty, expl)
  return exp
end

function expandBinaryElementWise(exp::Expression)::Tuple{Expression, Bool}
  local expanded::Bool
  local outExp::Expression

  local exp1::Expression
  local exp2::Expression
  local op::Operator

  @match BINARY_EXPRESSION(exp1 = exp1, operator = op, exp2 = exp2) = exp
  if isArray(P_Operator.Operator.typeOf(op))
    @assign (exp1, expanded) = expand(exp1)
    if expanded
      @assign (exp2, expanded) = expand(exp2)
    end
    if expanded
      @assign outExp =
        expandBinaryElementWise2(exp1, op, exp2, SimplifyExp.simplifyBinaryOp)
    else
      @assign outExp = exp
    end
  else
    @assign outExp = exp
    @assign expanded = true
  end
  return (outExp, expanded)
end

function expandBinary(exp::Expression, op::Operator)::Tuple{Expression, Bool}
  local expanded::Bool
  local outExp::Expression

  @assign (outExp, expanded) = begin
    @match op.op begin
      Op.ADD_SCALAR_ARRAY => begin
        expandBinaryScalarArray(exp, Op.ADD)
      end

      Op.ADD_ARRAY_SCALAR => begin
        expandBinaryArrayScalar(exp, Op.ADD)
      end

      Op.SUB_SCALAR_ARRAY => begin
        expandBinaryScalarArray(exp, Op.SUB)
      end

      Op.SUB_ARRAY_SCALAR => begin
        expandBinaryArrayScalar(exp, Op.SUB)
      end

      Op.MUL_SCALAR_ARRAY => begin
        expandBinaryScalarArray(exp, Op.MUL)
      end

      Op.MUL_ARRAY_SCALAR => begin
        expandBinaryArrayScalar(exp, Op.MUL)
      end

      Op.MUL_VECTOR_MATRIX => begin
        expandBinaryVectorMatrix(exp)
      end

      Op.MUL_MATRIX_VECTOR => begin
        expandBinaryMatrixVector(exp)
      end

      Op.SCALAR_PRODUCT => begin
        expandBinaryDotProduct(exp)
      end

      Op.MATRIX_PRODUCT => begin
        expandBinaryMatrixProduct(exp)
      end

      Op.DIV_SCALAR_ARRAY => begin
        expandBinaryScalarArray(exp, Op.DIV)
      end

      Op.DIV_ARRAY_SCALAR => begin
        expandBinaryArrayScalar(exp, Op.DIV)
      end

      Op.POW_SCALAR_ARRAY => begin
        expandBinaryScalarArray(exp, Op.POW)
      end

      Op.POW_ARRAY_SCALAR => begin
        expandBinaryArrayScalar(exp, Op.POW)
      end

      Op.POW_MATRIX => begin
        expandBinaryPowMatrix(exp)
      end

      _ => begin
        expandBinaryElementWise(exp)
      end
    end
  end
  if !expanded
    @assign outExp = exp
  end
  return (outExp, expanded)
end

function expandSize(exp::Expression)::Tuple{Expression, Bool}
  local expanded::Bool = true
  local outExp::Expression

  @assign outExp = begin
    local dims::Integer
    local e::Expression
    local ty::M_Type
    local expl::List{Expression}
    @match exp begin
      SIZE_EXPRESSION(exp = e, dimIndex = NONE()) => begin
        @assign ty = typeOf(e)
        @assign dims = Type.dimensionCount(ty)
        @assign expl = List(
          SIZE_EXPRESSION(e, SOME(INTEGER_EXPRESSION(i)))
          for i = 1:dims
        )
        P_Expression.Expression.makeArray(
          ARRAY_TYPE(ty, list(P_Dimension.Dimension.fromInteger(dims))),
          expl,
        )
      end

      _ => begin
        exp
      end
    end
  end
  #=  Size with an index is scalar, and thus already maximally expanded.
  =#
  return (outExp, expanded)
end

function expandArrayConstructor2(
  exp::Expression,
  ty::M_Type,
  ranges::List{<:Expression},
  iterators::List{<:Pointer{<:Expression}},
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
    @assign result = expand(SimplifyExp.simplify(exp))
  else
    @match _cons(range, ranges_rest) = ranges
    @match _cons(iter, iters_rest) = iterators
    @assign range_iter = P_ExpressionIterator.ExpressionIterator.fromExp(range)
    @assign el_ty = Type.unliftArray(ty)
    while P_ExpressionIterator.ExpressionIterator.hasNext(range_iter)
      @assign (range_iter, value) = P_ExpressionIterator.ExpressionIterator.next(range_iter)
      P_Pointer.update(iter, value)
      @assign expl =
        _cons(expandArrayConstructor2(exp, el_ty, ranges_rest, iters_rest), expl)
    end
    @assign result = P_Expression.Expression.makeArray(ty, listReverseInPlace(expl))
  end
  #=  Normally it wouldn't be the expansion's task to simplify expressions,
  =#
  #=  but we make an exception here since the generated expressions contain
  =#
  #=  MUTABLE expressions that we need to get rid of. Also, expansion of
  =#
  #=  array constructors is often done during the scalarization phase, after
  =#
  #=  the simplification phase, so they wouldn't otherwise be simplified.
  =#
  return result
end

function expandArrayConstructor(
  exp::Expression,
  ty::M_Type,
  iterators::List{<:Tuple{<:InstNode, Expression}},
)::Tuple{Expression, Bool}
  local expanded::Bool = true
  local result::Expression

  local e::Expression = exp
  local range::Expression
  local node::InstNode
  local ranges::List{Expression} = nil
  local expl::List{Expression}
  local iter::Pointer{Expression}
  local iters::List{Pointer{Expression}} = nil

  for i in iterators
    @assign (node, range) = i
    @assign iter = P_Pointer.create(INTEGER_EXPRESSION(0))
    @assign e = P_Expression.Expression.replaceIterator(
      e,
      node,
      MUTABLE_EXPRESSION(iter),
    )
    @assign iters = _cons(iter, iters)
    @match (range, true) = expand(range)
    @assign ranges = _cons(range, ranges)
  end
  @assign result = expandArrayConstructor2(e, ty, ranges, iters)
  return (result, expanded)
end

function expandBuiltinGeneric2(
  exp::Expression,
  fn::M_Function,
  ty::M_Type,
  var::VariabilityType,
  attr::NFCall.P_CallAttributes,
)::Expression

  @assign exp = begin
    local expl::List{Expression}
    @match exp begin
      ARRAY_EXPRESSION(literal = true) => begin
        exp
      end

      ARRAY_EXPRESSION(__) => begin
        @assign expl =
          List(expandBuiltinGeneric2(e, fn, ty, var, attr) for e in exp.elements)
        P_Expression.Expression.makeArray(setArrayElementType(exp.ty, ty), expl)
      end

      _ => begin
        CALL_EXPRESSION(P_Call.TYPED_CALL(fn, ty, var, list(exp), attr))
      end
    end
  end
  return exp
end

function expandBuiltinGeneric(call::Call)::Tuple{Expression, Bool}
  local expanded::Bool = true
  local outExp::Expression

  local fn::M_Function
  local ty::M_Type
  local var::VariabilityType
  local attr::NFCall.P_CallAttributes
  local arg::Expression
  local args::List{Expression}
  local expl::List{Expression}

  @match P_Call.TYPED_CALL(fn, ty, var, list(arg), attr) = call
  @assign ty = arrayElementType(ty)
  @match (arg, true) = expand(arg)
  @assign outExp = expandBuiltinGeneric2(arg, fn, ty, var, attr)
  return (outExp, expanded)
end

function expandBuiltinTranspose(arg::Expression)::Tuple{Expression, Bool}
  local expanded::Bool
  local outExp::Expression

  @assign (outExp, expanded) = expand(arg)
  if expanded
    @assign outExp = P_Expression.Expression.transposeArray(outExp)
  end
  return (outExp, expanded)
end

function expandBuiltinDiagonal(arg::Expression)::Tuple{Expression, Bool}
  local expanded::Bool
  local outExp::Expression

  @assign (outExp, expanded) = expand(arg)
  if expanded
    @assign outExp = Ceval.evalBuiltinDiagonal(outExp)
  end
  return (outExp, expanded)
end

function expandBuiltinPromote(args::List{<:Expression})::Tuple{Expression, Bool}
  local expanded::Bool
  local exp::Expression

  local n::Integer
  local eexp::Expression
  local nexp::Expression

  @match _cons(eexp, _cons(nexp, nil)) = args
  @match INTEGER_EXPRESSION(value = n) = nexp
  @assign (eexp, expanded) = expand(eexp)
  @assign exp =
    P_Expression.Expression.promote(eexp, typeOf(eexp), n)
  return (exp, expanded)
end

function expandBuiltinCat(args::List{<:Expression}, call::Call)::Tuple{Expression, Bool}
  local expanded::Bool
  local exp::Expression

  local expl::List{Expression} = nil

  @assign (expl, expanded) = expandList(listRest(args))
  if expanded
    @assign exp =
      Ceval.evalBuiltinCat(listHead(args), expl, Ceval.P_EvalTarget.IGNORE_ERRORS())
  else
    @assign exp = expandGeneric(CALL_EXPRESSION(call))
  end
  #=  This relies on the fact that Ceval.evalBuiltinCat doesn't actually do any
  =#
  #=  actual constant evaluation, and works on non-constant arrays too as long
  =#
  #=  as they're expanded.
  =#
  return (exp, expanded)
end

function expandBuiltinCall(
  fn::M_Function,
  args::List{<:Expression},
  call::Call,
)::Tuple{Expression, Bool}
  local expanded::Bool
  local outExp::Expression

  local fn_path::Absyn.Path = P_Function.nameConsiderBuiltin(fn)

  @assign (outExp, expanded) = begin
    @match AbsynUtil.pathFirstIdent(fn_path) begin
      "cat" => begin
        expandBuiltinCat(args, call)
      end

      "der" => begin
        expandBuiltinGeneric(call)
      end

      "diagonal" => begin
        expandBuiltinDiagonal(listHead(args))
      end

      "pre" => begin
        expandBuiltinGeneric(call)
      end

      "previous" => begin
        expandBuiltinGeneric(call)
      end

      "promote" => begin
        expandBuiltinPromote(args)
      end

      "transpose" => begin
        expandBuiltinTranspose(listHead(args))
      end
    end
  end
  return (outExp, expanded)
end

function expandCall(call::Call, exp::Expression)::Tuple{Expression, Bool}
  local expanded::Bool
  local outExp::Expression

  @assign (outExp, expanded) = begin
    @matchcontinue call begin
      P_Call.TYPED_CALL(
        __,
      ) where {(P_Function.isBuiltin(call.fn) && !P_Function.isImpure(call.fn))} => begin
        expandBuiltinCall(call.fn, call.arguments, call)
      end

      P_Call.TYPED_ARRAY_CONSTRUCTOR(__) => begin
        expandArrayConstructor(call.exp, call.ty, call.iters)
      end

      _ => begin
        expandGeneric(exp)
      end
    end
  end
  return (outExp, expanded)
end

function expandRange(exp::Expression)::Tuple{Expression, Bool}
  local expanded::Bool
  local outExp::Expression

  local range_iter::RangeIterator
  local ty::M_Type

  @match RANGE_EXPRESSION(ty = ty) = exp
  @assign expanded = Type.hasKnownSize(ty)
  if expanded
    @assign outExp = Ceval.evalExp(exp)
  else
    @assign outExp = exp
  end
  return (outExp, expanded)
end

function expandTypename(ty::M_Type)::Expression
  local outExp::Expression

  @assign outExp = begin
    local lits::List{Expression}
    @match ty begin
      ARRAY_TYPE(elementType = TYPE_BOOLEAN(__)) => begin
        P_Expression.Expression.makeArray(
          ty,
          list(
            P_Expression.BOOLEAN_EXPRESSION(false),
            P_Expression.BOOLEAN_EXPRESSION(true),
          ),
          true,
        )
      end

      ARRAY_TYPE(elementType = TYPE_ENUMERATION(__)) => begin
        @assign lits = P_Expression.Expression.makeEnumLiterals(ty.elementType)
        P_Expression.Expression.makeArray(ty, lits, true)
      end

      _ => begin
        Error.addInternalError(getInstanceName() + " got invalid typename", sourceInfo())
        fail()
      end
    end
  end
  return outExp
end

function expandCref4(
  subs::List{<:Subscript},
  comb::List{<:Subscript} = nil,
  accum::List{<:List{<:Subscript}} = nil,
  restSubs::List{<:List{<:Subscript}},
  cref::ComponentRef,
  crefType::M_Type,
)::Expression
  local arrayExp::Expression

  local expl::List{Expression} = nil
  local arr_ty::M_Type
  local slice::List{Subscript}
  local rest::List{Subscript}

  @assign arrayExp = begin
    @match subs begin
      nil() => begin
        expandCref3(restSubs, cref, crefType, _cons(listReverse(comb), accum))
      end

      SUBSCRIPT_EXPANDED_SLICE(indices = slice) <| rest => begin
        @assign expl = List(
          expandCref4(rest, _cons(idx, comb), accum, restSubs, cref, crefType)
          for idx in slice
        )
        @assign arr_ty = Type.liftArrayLeft(
          typeOf(listHead(expl)),
          P_Dimension.Dimension.fromExpList(expl),
        )
        P_Expression.Expression.makeArray(arr_ty, expl)
      end

      _ => begin
        expandCref4(
          listRest(subs),
          _cons(listHead(subs), comb),
          accum,
          restSubs,
          cref,
          crefType,
        )
      end
    end
  end
  return arrayExp
end

function expandCref3(
  subs::List{<:List{<:Subscript}},
  cref::ComponentRef,
  crefType::M_Type,
  accum::List{<:List{<:Subscript}} = nil,
)::Expression
  local arrayExp::Expression

  @assign arrayExp = begin
    @match subs begin
      nil() => begin
        CREF_EXPRESSION(
          crefType,
          setSubscriptsList(accum, cref),
        )
      end

      _ => begin
        expandCref4(listHead(subs), nil, accum, listRest(subs), cref, crefType)
      end
    end
  end
  return arrayExp
end

function expandCref2(
  cref::ComponentRef,
  subs::List{<:List{<:Subscript}} = nil,
)::List{List{Subscript}}

  local cr_subs::List{Subscript} = nil
  local dims::List{Dimension}
  import ..P_NFComponentRef.Origin

  @assign subs = begin
    @match cref begin
      CREF(origin = Origin.CREF) => begin
        @assign dims = arrayDims(cref.ty)
        @assign cr_subs = expandList(cref.subscripts, dims)
        if listEmpty(cr_subs) && !listEmpty(dims)
          nil
        else
          expandCref2(cref.restCref, _cons(cr_subs, subs))
        end
      end

      _ => begin
        subs
      end
    end
  end
  return subs
end

function expandCref(crefExp::Expression)::Tuple{Expression, Bool}
  local expanded::Bool
  local arrayExp::Expression

  local subs::List{List{Subscript}}

  @assign (arrayExp, expanded) = begin
    @match crefExp begin
      CREF_EXPRESSION(cref = CREF(__)) => begin
        if Type.hasZeroDimension(crefExp.ty)
          @assign arrayExp = P_Expression.Expression.makeEmptyArray(crefExp.ty)
          @assign expanded = true
        elseif Type.hasKnownSize(crefExp.ty)
          @assign subs = expandCref2(crefExp.cref)
          @assign arrayExp =
            expandCref3(subs, crefExp.cref, arrayElementType(crefExp.ty))
          @assign expanded = true
        else
          @assign arrayExp = crefExp
          @assign expanded = false
        end
        (arrayExp, expanded)
      end

      _ => begin
        (crefExp, false)
      end
    end
  end
  return (arrayExp, expanded)
end

""" #= Expands a list of Expressions. If abortOnFailure is true the function will
     stop if it fails to expand an element and the original list will be
     returned unchanged. If abortOnFailure is false it will instead continue and
     try to expand the whole list. In both cases the output 'expanded' indicates
     whether the whole list could be expanded or not. =#"""
function expandList(
  expl::List{<:Expression},
  abortOnFailure::Bool = true,
)::Tuple{List{Expression}, Bool}
  local expanded::Bool = true
  local outExpl::List{Expression} = nil

  local res::Bool

  for exp in expl
    @assign (exp, res) = expand(exp)
    @assign expanded = res && expanded
    if !res && abortOnFailure
      @assign outExpl = expl
      return (outExpl, expanded)
    end
    @assign outExpl = _cons(exp, outExpl)
  end
  @assign outExpl = listReverseInPlace(outExpl)
  return (outExpl, expanded)
end

function expand(exp::Expression)::Tuple{Expression, Bool}
  local expanded::Bool

  @assign (exp, expanded) = begin
    local expl::List{Expression}
    @match exp begin
      CREF_EXPRESSION(ty = ARRAY_TYPE(__)) => begin
        expandCref(exp)
      end

      ARRAY_EXPRESSION(ty = ARRAY_TYPE(dimensions = nil())) => begin
        (exp, true)
      end

      ARRAY_EXPRESSION(__) => begin
        #=  One-dimensional arrays are already expanded.
        =#
        @assign (expl, expanded) = expandList(exp.elements)
        @assign exp.elements = expl
        (exp, expanded)
      end

      P_Expression.Expression.TYPENAME(__) => begin
        (expandTypename(exp.ty), true)
      end

      RANGE_EXPRESSION(__) => begin
        expandRange(exp)
      end

      CALL_EXPRESSION(__) => begin
        expandCall(exp.call, exp)
      end

      SIZE_EXPRESSION(__) => begin
        expandSize(exp)
      end

      BINARY_EXPRESSION(__) => begin
        expandBinary(exp, exp.operator)
      end

      UNARY_EXPRESSION(__) => begin
        expandUnary(exp.exp, exp.operator)
      end

      LBINARY_EXPRESSION(__) => begin
        expandLogicalBinary(exp)
      end

      LUNARY_EXPRESSION(__) => begin
        expandLogicalUnary(exp.exp, exp.operator)
      end

      RELATION_EXPRESSION(__) => begin
        (exp, true)
      end

      CAST_EXPRESSION(__) => begin
        expandCast(exp.exp, exp.ty)
      end

      _ => begin
        expandGeneric(exp)
      end
    end
  end
  return (exp, expanded)
end

@Uniontype NFExpandExp begin end

@exportAll()
end
