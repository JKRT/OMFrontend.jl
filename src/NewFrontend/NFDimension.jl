@UniontypeDecl NFDimension
@Uniontype NFDimension begin
  @Record DIMENSION_UNKNOWN begin
  end
  @Record DIMENSION_EXP begin
    exp::Expression
    var::VariabilityType
  end
  @Record DIMENSION_ENUM begin
    enumType
  end
  @Record DIMENSION_BOOLEAN begin
  end
  @Record DIMENSION_INTEGER begin
    size::Int
    var::VariabilityType
  end
  @Record DIMENSION_UNTYPED begin
    dimension::Expression
    isProcessing::Bool
  end
  @Record DIMENSION_RAW_DIM begin
    dim::Absyn.Subscript
  end
end

using Absyn: Exp, Path #, Subscript

function foldExpList(dims::List{Dimension}, func::FoldFunc, arg::ArgT) where {ArgT}
  for dim in dims
    @assign arg = foldExp(dim, func, arg)
  end
  return arg
end

function foldExp(dim::Dimension, func::FoldFunc, arg::ArgT) where {ArgT}
  local outArg::ArgT
  @assign outArg = begin
    @match dim begin
      DIMENSION_UNTYPED(__) => begin
        fold(dim.dimension, func, arg)
      end
      DIMENSION_EXP(__) => begin
        fold(dim.exp, func, arg)
      end
      _ => begin
        arg
      end
    end
  end
  return outArg
end

function mapExp(dim::Dimension, func::MapFunc)::Dimension
  local outDim::Dimension
  @assign outDim = begin
    local e1::Expression
    local e2::Expression
    @match dim begin
      DIMENSION_UNTYPED(dimension = e1) => begin
        @assign e2 = map(e1, func)
        if referenceEq(e1, e2)
          dim
        else
          DIMENSION_UNTYPED(e2, dim.isProcessing)
        end
      end

      DIMENSION_EXP(exp = e1) => begin
        @assign e2 = map(e1, func)
        if referenceEq(e1, e2)
          dim
        else
          DIMENSION_EXP(e2, dim.var)
        end
      end

      _ => begin
        dim
      end
    end
  end
  return outDim
end

function variability(dim::Dimension)::VariabilityType
  local var::VariabilityType

  @assign var = begin
    @match dim begin
      DIMENSION_INTEGER(__) => begin
        dim.var
      end

      DIMENSION_BOOLEAN(__) => begin
        Variability.CONSTANT
      end

      DIMENSION_ENUM(__) => begin
        Variability.CONSTANT
      end

      DIMENSION_EXP(__) => begin
        dim.var
      end

      DIMENSION_UNKNOWN(__) => begin
        Variability.CONTINUOUS
      end
    end
  end
  return var
end

""" #= Returns the size of a dimension as an Expression. =#"""
function sizeExp(dim::Dimension)::Expression
  local sizeExp::Expression

  @assign sizeExp = begin
    local ty::M_Type
    @match dim begin
      DIMENSION_INTEGER(__) => begin
        INTEGER_EXPRESSION(dim.size)
      end

      DIMENSION_BOOLEAN(__) => begin
        INTEGER_EXPRESSION(2)
      end

      DIMENSION_ENUM(enumType = ty && TYPE_ENUMERATION(__)) => begin
        INTEGER_EXPRESSION(listLength(ty.literals))
      end

      DIMENSION_EXP(__) => begin
        dim.exp
      end
    end
  end
  return sizeExp
end

""" #= Returns an expression for the last index in a dimension. =#"""
function endExp(dim::Dimension, cref::ComponentRef, index::Int)::Expression
  local sizeExp::Expression

  @assign sizeExp = begin
    local ty::M_Type
    @match dim begin
      DIMENSION_INTEGER(__) => begin
        INTEGER_EXPRESSION(dim.size)
      end

      DIMENSION_BOOLEAN(__) => begin
        BOOLEAN_EXPRESSION(true)
      end

      DIMENSION_ENUM(enumType = ty && TYPE_ENUMERATION(__)) => begin
        makeEnumLiteral(ty, listLength(ty.literals))
      end

      DIMENSION_EXP(__) => begin
        dim.exp
      end

      DIMENSION_UNKNOWN(__) => begin
        SIZE_EXPRESSION(
          CREF_EXPRESSION(
            TYPE_UNKNOWN(),
            stripSubscripts(cref),
          ),
          SOME(INTEGER_EXPRESSION(index)),
        )
      end
    end
  end
  return sizeExp
end

function toStringList(dims::List{<:Dimension})::String
  local str::String = "[" + stringDelimitList(ListUtil.map(dims, toString), ", ") + "]"
  return str
end

function toString(dim::Dimension)::String
  local str::String

  @assign str = begin
    local ty::M_Type
    @match dim begin
      DIMENSION_INTEGER(__) => begin
        String(dim.size)
      end

      DIMENSION_BOOLEAN(__) => begin
        "Boolean"
      end

      DIMENSION_ENUM(enumType = ty && TYPE_ENUMERATION(__)) => begin
        AbsynUtil.pathString(ty.typePath)
      end

      DIMENSION_EXP(__) => begin
        toString(dim.exp)
      end

      DIMENSION_UNKNOWN(__) => begin
        ":"
      end

      DIMENSION_UNTYPED(__) => begin
        toString(dim.dimension)
      end
    end
  end
  return str
end

""" #= Returns the expected type of a subscript for the given dimension. =#"""
function subscriptType(dim::Dimension)::M_Type
  local ty::M_Type
  ty = begin
    @match dim begin
      DIMENSION_INTEGER(__) => begin
        TYPE_INTEGER()
      end
      DIMENSION_BOOLEAN(__) => begin
        TYPE_BOOLEAN()
      end
      DIMENSION_ENUM(__) => begin
        dim.enumType
      end
      DIMENSION_EXP(__) => begin
        typeOf(dim.exp)
      end
      _ => begin
        TYPE_UNKNOWN()
      end
    end
  end
  return ty
end

function isOne(dim::Dimension)::Bool
  local isOne::Bool

  @assign isOne = begin
    @match dim begin
      INTEGER_EXPRESSION(__) => begin
        dim.size == 1
      end
      DIMENSION_ENUM(__) => begin
        Type.enumSize(dim.enumType) == 1
      end

      _ => begin
        false
      end
    end
  end
  return isOne
end

function isZero(dim::Dimension)::Bool
  local isZ::Bool
  @assign isZ = begin
    @match dim begin
      INTEGER_EXPRESSION(__) => begin
        dim.size == 0
      end
      DIMENSION_ENUM(__) => begin
        Type.enumSize(dim.enumType) == 0
      end
      _ => begin
        false
      end
    end
  end
  return isZ
end

function isUnknown(dim::Dimension)::Bool
  local isUnk::Bool

  @assign isUnk = begin
    @match dim begin
      UNKNOWN(__) => begin
        true
      end
      _ => begin
        false
      end
    end
  end
  return isUnk
end

function isKnown(dim::Dimension, allowExp::Bool = false)::Bool
  local known::Bool
  @assign known = begin
    @match dim begin
      DIMENSION_INTEGER(__) => begin
        true
      end
      DIMENSION_BOOLEAN(__) => begin
        true
      end
      DIMENSION_ENUM(__) => begin
        true
      end
      DIMENSION_EXP(__) => begin
        allowExp
      end
      _ => begin
        false
      end
    end
  end
  return known
end

function allEqualKnown(dims1::List{<:Dimension}, dims2::List{<:Dimension})::Bool
  local allEqual::Bool = ListUtil.isEqualOnTrue(dims1, dims2, isEqualKnown)
  return allEqual
end

function isEqualKnown(dim1::Dimension, dim2::Dimension)::Bool
  local isEqual::Bool

  @assign isEqual = begin
    @match (dim1, dim2) begin
      (UNKNOWN(__), _) => begin
        false
      end

      (_, UNKNOWN(__)) => begin
        false
      end

      (DIMENSION_EXP(__), DIMENSION_EXP(__)) => begin
        isEqual(dim1.exp, dim2.exp)
      end

      (DIMENSION_EXP(__), _) => begin
        false
      end

      (_, DIMENSION_EXP(__)) => begin
        false
      end

      _ => begin
        P_Dimension.Dimension.size(dim1) == P_Dimension.Dimension.size(dim2)
      end
    end
  end
  return isEqual
end

function isEqual(dim1::Dimension, dim2::Dimension)::Bool
  local isEq::Bool
  isEq = begin
    @match (dim1, dim2) begin
      (DIMENSION_UNKNOWN(__), _) => begin
        true
      end
      (_, DIMENSION_UNKNOWN(__)) => begin
        true
      end
      (DIMENSION_EXP(__), DIMENSION_EXP(__)) => begin
        isEqual(dim1.exp, dim2.exp)
      end
      (DIMENSION_EXP(__), _) => begin
        true
      end
      (_, DIMENSION_EXP(__)) => begin
        true
      end
      _ => begin
        size(dim1) == size(dim2)
      end
    end
  end
  return isEq
end

function size(dim::Dimension)::Int
  local sz::Int
  @assign sz = begin
    local ty::M_Type
    @match dim begin
      DIMENSION_INTEGER(__) => begin
        dim.size
      end

      DIMENSION_BOOLEAN(__) => begin
        2
      end
      DIMENSION_ENUM(enumType = ty && TYPE_ENUMERATION(__)) => begin
        listLength(ty.literals)
      end
    end
  end
  return sz
end

function add(a::Dimension, b::Dimension)::Dimension
  local c::Dimension
  @assign c = begin
    @match (a, b) begin
      (UNKNOWN(__), _) => begin
        UNKNOWN()
      end

      (_, UNKNOWN(__)) => begin
        UNKNOWN()
      end

      (INTEGER_EXPRESSION(__), INTEGER_EXPRESSION(__)) => begin
        INTEGER_EXPRESSION(a.size + b.size, variabilityMax(a.var, b.var))
      end

      (INTEGER_EXPRESSION(__), DIMENSION_EXP(__)) => begin
        DIMENSION_EXP(
          BINARY_EXPRESSION(
            b.exp,
            OPERATOR(TYPE_INTEGER(), P_NFOperator.Op.ADD),
            INTEGER_EXPRESSION(a.size),
          ),
          b.var,
        )
      end

      (DIMENSION_EXP(__), INTEGER_EXPRESSION(__)) => begin
        DIMENSION_EXP(
          BINARY_EXPRESSION(
            a.exp,
            OPERATOR(TYPE_INTEGER(), P_NFOperator.Op.ADD),
            INTEGER_EXPRESSION(b.size),
          ),
          a.var,
        )
      end

      (DIMENSION_EXP(__), DIMENSION_EXP(__)) => begin
        DIMENSION_EXP(
          BINARY_EXPRESSION(
            a.exp,
            OPERATOR(TYPE_INTEGER(), P_NFOperator.Op.ADD),
            b.exp,
          ),
          variabilityMax(a.var, b.var),
        )
      end

      _ => begin
        UNKNOWN()
      end
    end
  end
  return c
end

function toDAE(dim::Dimension)::DAE.Dimension
  local daeDim::DAE.Dimension

  @assign daeDim = begin
    local ty::M_Type
    @match dim begin
      DIMENSION_INTEGER(__) => begin
        DAE.DIM_INTEGER(dim.size)
      end
      DIMENSION_BOOLEAN(__) => begin
        DAE.DIM_BOOLEAN()
      end
      DIMENSION_ENUM(enumType = ty && TYPE_ENUMERATION(__)) => begin
        DAE.DIM_ENUM(ty.typePath, ty.literals, listLength(ty.literals))
      end
      DIMENSION_EXP(__) => begin
        DAE.DIM_EXP(toDAE(dim.exp))
      end
      DIMENSION_UNKNOWN(__) => begin
        DAE.DIM_UNKNOWN()
      end
    end
  end
  return daeDim
end

function fromExpList(expl::List{<:Expression})::Dimension
  local dim::Dimension = DIMENSION_INTEGER(listLength(expl), Variability.CONSTANT)
  return dim
end

function fromInteger(n::Int, var::VariabilityType = Variability.CONSTANT)::Dimension
  local dim::Dimension = DIMENSION_INTEGER(n, var)
  return dim
end

function fromExp(exp::Expression, var::VariabilityType)::Dimension
  local dim::Dimension
  @assign dim = begin
    local cls::Class
    local cref::ComponentRef
    local ty::M_Type
    @match exp begin
      INTEGER_EXPRESSION(__) => begin
        DIMENSION_INTEGER(exp.value, var)
      end
      TYPENAME_EXPRESSION(ty = TYPE_ARRAY(elementType = ty)) => begin
        begin
          @match ty begin
            TYPE_BOOLEAN(__) => begin
              BOOLEAN()
            end
            TYPE_ENUMERATION(__) => begin
              ENUM(ty)
            end
            _ => begin
              Error.assertion(
                false,
                getInstanceName() + " got invalid typename",
                sourceInfo(),
              )
              fail()
            end
          end
        end
      end
      _ => begin
        DIMENSION_EXP(exp, var)
      end
    end
  end
  return dim
end
