
@UniontypeDecl NFOperator
M_Type = NFType

Op = (
  () -> begin #= Enumeration =#
    ADD = 1
    SUB = 2
    MUL = 3
    DIV = 4
    POW = 5
    ADD_EW = 6
    SUB_EW = 7
    MUL_EW = 8
    DIV_EW = 9
    POW_EW = 10
    ADD_SCALAR_ARRAY = 11
    ADD_ARRAY_SCALAR = 12
    SUB_SCALAR_ARRAY = 13
    SUB_ARRAY_SCALAR = 14
    MUL_SCALAR_ARRAY = 15
    MUL_ARRAY_SCALAR = 16
    MUL_VECTOR_MATRIX = 17
    MUL_MATRIX_VECTOR = 18
    SCALAR_PRODUCT = 19
    MATRIX_PRODUCT = 20
    DIV_SCALAR_ARRAY = 21
    DIV_ARRAY_SCALAR = 22
    POW_SCALAR_ARRAY = 23
    POW_ARRAY_SCALAR = 24
    POW_MATRIX = 25
    UMINUS = 26
    AND = 27
    OR = 28
    NOT = 29
    LESS = 30
    LESSEQ = 31
    GREATER = 32
    GREATEREQ = 33
    EQUAL = 34
    NEQUAL = 35
    USERDEFINED = 36
    () -> (
      ADD; SUB; MUL; DIV; POW; ADD_EW; SUB_EW; MUL_EW; DIV_EW; POW_EW; ADD_SCALAR_ARRAY; ADD_ARRAY_SCALAR; SUB_SCALAR_ARRAY; SUB_ARRAY_SCALAR; MUL_SCALAR_ARRAY; MUL_ARRAY_SCALAR; MUL_VECTOR_MATRIX; MUL_MATRIX_VECTOR; SCALAR_PRODUCT; MATRIX_PRODUCT; DIV_SCALAR_ARRAY; DIV_ARRAY_SCALAR; POW_SCALAR_ARRAY; POW_ARRAY_SCALAR; POW_MATRIX; UMINUS; AND; OR; NOT; LESS; LESSEQ; GREATER; GREATEREQ; EQUAL; NEQUAL; USERDEFINED
    )
  end
)()

const OpType = Integer


@Uniontype NFOperator begin
  @Record OPERATOR begin
    ty::M_Type
    op::OpType
  end
end

function negate(op::Operator)::Operator
  local outOp::Operator
  local neg_op::OpType

  @assign neg_op = begin
    @match op.op begin
      Op.ADD => begin
        Op.SUB
      end

      Op.SUB => begin
        Op.ADD
      end

      Op.ADD_EW => begin
        Op.SUB_EW
      end

      Op.SUB_EW => begin
        Op.ADD_EW
      end

      Op.ADD_SCALAR_ARRAY => begin
        Op.SUB_SCALAR_ARRAY
      end

      Op.SUB_SCALAR_ARRAY => begin
        Op.ADD_SCALAR_ARRAY
      end

      Op.ADD_ARRAY_SCALAR => begin
        Op.SUB_ARRAY_SCALAR
      end
    end
  end
  @assign outOp = OPERATOR(op.ty, neg_op)
  return outOp
end

function isElementWise(op::Operator)::Bool
  local ew::Bool

  @assign ew = begin
    @match op.op begin
      Op.ADD_EW => begin
        true
      end

      Op.SUB_EW => begin
        true
      end

      Op.MUL_EW => begin
        true
      end

      Op.DIV_EW => begin
        true
      end

      Op.POW_EW => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return ew
end

function stripEW(op::Operator)::Operator

  @assign () = begin
    @match op.op begin
      Op.ADD_EW => begin
        @assign op.op = Op.ADD
        ()
      end

      Op.SUB_EW => begin
        @assign op.op = Op.SUB
        ()
      end

      Op.MUL_EW => begin
        @assign op.op = Op.MUL
        ()
      end

      Op.DIV_EW => begin
        @assign op.op = Op.DIV
        ()
      end

      Op.POW_EW => begin
        @assign op.op = Op.POW
        ()
      end

      _ => begin
        ()
      end
    end
  end
  return op
end

function makeArrayScalar(ty::M_Type, op::OpType)::Operator
  local outOp::Operator

  local o::OpType

  @assign o = begin
    @match op begin
      Op.ADD => begin
        Op.ADD_ARRAY_SCALAR
      end

      Op.SUB => begin
        Op.SUB_ARRAY_SCALAR
      end

      Op.MUL => begin
        Op.MUL_ARRAY_SCALAR
      end

      Op.DIV => begin
        Op.DIV_ARRAY_SCALAR
      end

      Op.POW => begin
        Op.POW_ARRAY_SCALAR
      end
    end
  end
  @assign outOp = OPERATOR(ty, o)
  return outOp
end

function makeScalarArray(ty::M_Type, op::OpType)::Operator
  local outOp::Operator
  local o::OpType
  @assign o = begin
    @match op begin
      Op.ADD => begin
        Op.ADD_SCALAR_ARRAY
      end

      Op.SUB => begin
        Op.SUB_SCALAR_ARRAY
      end

      Op.MUL => begin
        Op.MUL_SCALAR_ARRAY
      end

      Op.DIV => begin
        Op.DIV_SCALAR_ARRAY
      end

      Op.POW => begin
        Op.POW_SCALAR_ARRAY
      end
    end
  end
  @assign outOp = OPERATOR(ty, o)
  return outOp
end

function makeNotEqual(ty::M_Type)::Operator
  local op::Operator = OPERATOR(ty, Op.NEQUAL)
  return op
end

function makeEqual(ty::M_Type)::Operator
  local op::Operator = OPERATOR(ty, Op.EQUAL)
  return op
end

function makeGreaterEq(ty::M_Type)::Operator
  local op::Operator = OPERATOR(ty, Op.GREATEREQ)
  return op
end

function makeGreater(ty::M_Type)::Operator
  local op::Operator = OPERATOR(ty, Op.GREATER)
  return op
end

function makeLessEq(ty::M_Type)::Operator
  local op::Operator = OPERATOR(ty, Op.LESSEQ)
  return op
end

function makeLess(ty::M_Type)::Operator
  local op::Operator = OPERATOR(ty, Op.LESS)
  return op
end

function makeNot(ty::M_Type)::Operator
  local op::Operator = OPERATOR(ty, Op.NOT)
  return op
end

function makeOr(ty::M_Type)::Operator
  local op::Operator = OPERATOR(ty, Op.OR)
  return op
end

function makeAnd(ty::M_Type)::Operator
  local op::Operator = OPERATOR(ty, Op.AND)
  return op
end

function makeUMinus(ty::M_Type)::Operator
  local op::Operator = OPERATOR(ty, Op.UMINUS)
  return op
end

function makeDivEW(ty::M_Type)::Operator
  local op::Operator = OPERATOR(ty, Op.DIV_EW)
  return op
end

function makeMulEW(ty::M_Type)::Operator
  local op::Operator = OPERATOR(ty, Op.MUL_EW)
  return op
end

function makeSubEW(ty::M_Type)::Operator
  local op::Operator = OPERATOR(ty, Op.SUB_EW)
  return op
end

function makeAddEW(ty::M_Type)::Operator
  local op::Operator = OPERATOR(ty, Op.ADD_EW)
  return op
end

function makePow(ty::M_Type)::Operator
  local op::Operator = OPERATOR(ty, Op.POW)
  return op
end

function makeDiv(ty::M_Type)::Operator
  local op::Operator = OPERATOR(ty, Op.DIV)
  return op
end

function makeMul(ty::M_Type)::Operator
  local op::Operator = OPERATOR(ty, Op.MUL)
  return op
end

function makeSub(ty::M_Type)::Operator
  local op::Operator = OPERATOR(ty, Op.SUB)
  return op
end

function makeAdd(ty::M_Type)::Operator
  local op::Operator = OPERATOR(ty, Op.ADD)
  return op
end

function isNonAssociative(op::Operator)::Bool
  local isNonAssociative::Bool

  @assign isNonAssociative = begin
    @match op.op begin
      Op.POW => begin
        true
      end

      Op.POW_EW => begin
        true
      end

      Op.POW_SCALAR_ARRAY => begin
        true
      end

      Op.POW_ARRAY_SCALAR => begin
        true
      end

      Op.POW_MATRIX => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return isNonAssociative
end

function isAssociative(op::Operator)::Bool
  local isAssociative::Bool

  @assign isAssociative = begin
    @match op.op begin
      Op.ADD => begin
        true
      end

      Op.ADD_EW => begin
        true
      end

      Op.MUL_EW => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  #= case ADD_ARRAY_SCALAR() then true;
  =#
  #= case MUL_ARRAY_SCALAR() then true;
  =#
  return isAssociative
end

function priority(op::Operator, lhs::Bool)::Integer
  local priority::Integer

  @assign priority = begin
    @match op.op begin
      Op.ADD => begin
        if lhs
          5
        else
          6
        end
      end

      Op.SUB => begin
        5
      end

      Op.MUL => begin
        2
      end

      Op.DIV => begin
        2
      end

      Op.POW => begin
        1
      end

      Op.ADD_EW => begin
        if lhs
          5
        else
          6
        end
      end

      Op.SUB_EW => begin
        5
      end

      Op.MUL_EW => begin
        if lhs
          2
        else
          3
        end
      end

      Op.DIV_EW => begin
        2
      end

      Op.POW_EW => begin
        1
      end

      Op.AND => begin
        8
      end

      Op.OR => begin
        9
      end

      _ => begin
        0
      end
    end
  end
  #= case MUL_ARRAY_SCALAR() then if lhs then 2 else 3;
  =#
  #= case ADD_ARRAY_SCALAR() then if lhs then 5 else 6;
  =#
  #= case SUB_SCALAR_ARRAY() then 5;
  =#
  #= case SCALAR_PRODUCT()   then if lhs then 2 else 3;
  =#
  #= case MATRIX_PRODUCT()   then if lhs then 2 else 3;
  =#
  #= case DIV_ARRAY_SCALAR() then 2;
  =#
  #= case DIV_SCALAR_ARRAY() then 2;
  =#
  #= case POW_ARRAY_SCALAR() then 1;
  =#
  #= case POW_SCALAR_ARRAY() then 1;
  =#
  #= case POW_ARR()          then 1;
  =#
  return priority
end

function symbol(op::Operator, spacing::String = " ")::String
  local symbol::String

  @assign symbol = begin
    @match op.op begin
      Op.ADD => begin
        "+"
      end

      Op.SUB => begin
        "-"
      end

      Op.MUL => begin
        "*"
      end

      Op.DIV => begin
        "/"
      end

      Op.POW => begin
        "^"
      end

      Op.ADD_EW => begin
        ".+"
      end

      Op.SUB_EW => begin
        ".-"
      end

      Op.MUL_EW => begin
        ".*"
      end

      Op.DIV_EW => begin
        "./"
      end

      Op.POW_EW => begin
        ".^"
      end

      Op.ADD_SCALAR_ARRAY => begin
        ".+"
      end

      Op.ADD_ARRAY_SCALAR => begin
        ".+"
      end

      Op.SUB_SCALAR_ARRAY => begin
        ".-"
      end

      Op.SUB_ARRAY_SCALAR => begin
        ".-"
      end

      Op.MUL_SCALAR_ARRAY => begin
        "*"
      end

      Op.MUL_ARRAY_SCALAR => begin
        ".*"
      end

      Op.MUL_VECTOR_MATRIX => begin
        "*"
      end

      Op.MUL_MATRIX_VECTOR => begin
        "*"
      end

      Op.SCALAR_PRODUCT => begin
        "*"
      end

      Op.MATRIX_PRODUCT => begin
        "*"
      end

      Op.DIV_SCALAR_ARRAY => begin
        "./"
      end

      Op.DIV_ARRAY_SCALAR => begin
        "/"
      end

      Op.POW_SCALAR_ARRAY => begin
        ".^"
      end

      Op.POW_ARRAY_SCALAR => begin
        ".^"
      end

      Op.POW_MATRIX => begin
        "^"
      end

      Op.UMINUS => begin
        "-"
      end

      Op.AND => begin
        "and"
      end

      Op.OR => begin
        "or"
      end

      Op.NOT => begin
        "not"
      end

      Op.LESS => begin
        "<"
      end

      Op.LESSEQ => begin
        "<="
      end

      Op.GREATER => begin
        ">"
      end

      Op.GREATEREQ => begin
        ">="
      end

      Op.EQUAL => begin
        "=="
      end

      Op.NEQUAL => begin
        "<>"
      end

      _ => begin
        #= case Op.USERDEFINED      then \"Userdefined:\" + AbsynUtil.pathString(op.fqName);
        =#
        Error.assertion(false, getInstanceName() + " got unknown type.", sourceInfo())
        fail()
      end
    end
  end
  @assign symbol = spacing + symbol + spacing
  return symbol
end

function unlift(op::Operator)::Operator

  @assign op.ty = Type.unliftArray(op.ty)
  return op
end

function scalarize(op::Operator)::Operator

  @assign op.ty = Type.arrayElementType(op.ty)
  return op
end

function setType(ty::M_Type, op::Operator)::Operator

  @assign op.ty = ty
  return op
end

function typeOf(op::Operator)::M_Type
  local ty::M_Type = op.ty
  return ty
end

function toDAE(op::Operator)::Tuple{DAE.P_Operator.Operator, Bool}
  local swapArguments::Bool = false #= The DAE structure only has array*scalar, not scalar*array, etc =#
  local daeOp::DAE.P_Operator.Operator

  local ty::DAE.Type

  @assign ty = Type.toDAE(op.ty)
  @assign daeOp = begin
    @match op.op begin
      Op.ADD => begin
        if Type.isArray(op.ty)
          DAE.ADD_ARR(ty)
        else
          DAE.ADD(ty)
        end
      end

      Op.SUB => begin
        if Type.isArray(op.ty)
          DAE.SUB_ARR(ty)
        else
          DAE.SUB(ty)
        end
      end

      Op.MUL => begin
        if Type.isArray(op.ty)
          DAE.MUL_ARR(ty)
        else
          DAE.MUL(ty)
        end
      end

      Op.DIV => begin
        if Type.isArray(op.ty)
          DAE.DIV_ARR(ty)
        else
          DAE.DIV(ty)
        end
      end

      Op.POW => begin
        if Type.isArray(op.ty)
          DAE.POW_ARR2(ty)
        else
          DAE.POW(ty)
        end
      end

      Op.ADD_SCALAR_ARRAY => begin
        @assign swapArguments = true
        DAE.ADD_ARRAY_SCALAR(ty)
      end

      Op.ADD_ARRAY_SCALAR => begin
        DAE.ADD_ARRAY_SCALAR(ty)
      end

      Op.SUB_SCALAR_ARRAY => begin
        DAE.SUB_SCALAR_ARRAY(ty)
      end

      Op.SUB_ARRAY_SCALAR => begin
        Error.addInternalError(
          getInstanceName() + ": Don't know how to handle " + String(op.op),
          sourceInfo(),
        )
        DAE.SUB(ty)
      end

      Op.MUL_SCALAR_ARRAY => begin
        @assign swapArguments = true
        DAE.MUL_ARRAY_SCALAR(ty)
      end

      Op.MUL_ARRAY_SCALAR => begin
        DAE.MUL_ARRAY_SCALAR(ty)
      end

      Op.MUL_VECTOR_MATRIX => begin
        DAE.MUL_MATRIX_PRODUCT(ty)
      end

      Op.MUL_MATRIX_VECTOR => begin
        DAE.MUL_MATRIX_PRODUCT(ty)
      end

      Op.SCALAR_PRODUCT => begin
        DAE.MUL_SCALAR_PRODUCT(ty)
      end

      Op.MATRIX_PRODUCT => begin
        DAE.MUL_MATRIX_PRODUCT(ty)
      end

      Op.DIV_SCALAR_ARRAY => begin
        DAE.DIV_SCALAR_ARRAY(ty)
      end

      Op.DIV_ARRAY_SCALAR => begin
        DAE.DIV_ARRAY_SCALAR(ty)
      end

      Op.POW_SCALAR_ARRAY => begin
        DAE.POW_SCALAR_ARRAY(ty)
      end

      Op.POW_ARRAY_SCALAR => begin
        DAE.POW_ARRAY_SCALAR(ty)
      end

      Op.POW_MATRIX => begin
        DAE.POW_ARR(ty)
      end

      Op.UMINUS => begin
        if Type.isArray(op.ty)
          DAE.UMINUS_ARR(ty)
        else
          DAE.UMINUS(ty)
        end
      end

      Op.AND => begin
        DAE.AND(ty)
      end

      Op.OR => begin
        DAE.OR(ty)
      end

      Op.NOT => begin
        DAE.NOT(ty)
      end

      Op.LESS => begin
        DAE.LESS(ty)
      end

      Op.LESSEQ => begin
        DAE.LESSEQ(ty)
      end

      Op.GREATER => begin
        DAE.GREATER(ty)
      end

      Op.GREATEREQ => begin
        DAE.GREATEREQ(ty)
      end

      Op.EQUAL => begin
        DAE.EQUAL(ty)
      end

      Op.NEQUAL => begin
        DAE.NEQUAL(ty)
      end

      _ => begin
        Error.assertion(false, getInstanceName() + " got unknown type.", sourceInfo())
        fail()
      end
    end
  end
  return (daeOp, swapArguments) #= The DAE structure only has array*scalar, not scalar*array, etc =#
end

function fromAbsyn(inOperator::Absyn.Operator)::Operator
  local outOperator::Operator
  local op::OpType

  @assign op = begin
    @match inOperator begin
      Absyn.ADD(__) => begin
        Op.ADD
      end

      Absyn.SUB(__) => begin
        Op.SUB
      end

      Absyn.MUL(__) => begin
        Op.MUL
      end

      Absyn.DIV(__) => begin
        Op.DIV
      end

      Absyn.POW(__) => begin
        Op.POW
      end

      Absyn.ADD_EW(__) => begin
        Op.ADD_EW
      end

      Absyn.SUB_EW(__) => begin
        Op.SUB_EW
      end

      Absyn.MUL_EW(__) => begin
        Op.MUL_EW
      end

      Absyn.DIV_EW(__) => begin
        Op.DIV_EW
      end

      Absyn.POW_EW(__) => begin
        Op.POW_EW
      end

      Absyn.UPLUS(__) => begin
        Op.ADD
      end

      Absyn.UPLUS_EW(__) => begin
        Op.ADD
      end

      Absyn.UMINUS(__) => begin
        Op.UMINUS
      end

      Absyn.UMINUS_EW(__) => begin
        Op.UMINUS
      end

      Absyn.AND(__) => begin
        Op.AND
      end

      Absyn.OR(__) => begin
        Op.OR
      end

      Absyn.NOT(__) => begin
        Op.NOT
      end

      Absyn.LESS(__) => begin
        Op.LESS
      end

      Absyn.LESSEQ(__) => begin
        Op.LESSEQ
      end

      Absyn.GREATER(__) => begin
        Op.GREATER
      end

      Absyn.GREATEREQ(__) => begin
        Op.GREATEREQ
      end

      Absyn.EQUAL(__) => begin
        Op.EQUAL
      end

      Absyn.NEQUAL(__) => begin
        Op.NEQUAL
      end
    end
  end
  @assign outOperator = OPERATOR(TYPE_UNKNOWN(), op)
  return outOperator
end

function compare(op1::Operator, op2::Operator)::Integer
  local comp::Integer

  local o1::OpType = op1.op
  local o2::OpType = op2.op

  #=  TODO: Compare the types instead if both operators are USERDEFINED.
  =#
  @assign comp = Util.intCompare(Integer(o1), Integer(o2))
  return comp
end
