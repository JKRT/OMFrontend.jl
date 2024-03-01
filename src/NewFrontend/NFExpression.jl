@Uniontype ClockKind begin
  @Record SOLVER_CLOCK begin
    c::Expression
    solverMethod #=  string type  =#::Expression
  end
  @Record BOOLEAN_CLOCK begin
    condition::Expression
    startInterval #=  real type >= 0.0  =#::Expression
  end
  @Record REAL_CLOCK begin
    interval::Expression
  end
  @Record INTEGER_CLOCK begin
    intervalCounter::Expression
    resolution #=  integer type >= 1  =#::Expression
  end
  @Record INFERRED_CLOCK begin
  end
end

abstract type NFExpression end

mutable struct BINDING_EXP <: NFExpression
  exp::Expression
  expType #= The actual type of exp. =#::NFType
  bindingType #= The type of the propagated binding. =#::NFType
  parents::List{InstNode}
  isEach::Bool
end

struct PARTIAL_FUNCTION_APPLICATION_EXPRESSION <: NFExpression
  fn::ComponentRef
  args::List{Expression}
  argNames::List{String}
  ty::NFType
end

struct CLKCONST_EXPRESSION <: NFExpression
  clk #= Clock kinds =#::ClockKind
end

struct EMPTY_EXPRESSION <: NFExpression
  ty::NFType
end

struct MUTABLE_EXPRESSION <: NFExpression
  exp::Pointer{Expression}
end

struct BOX_EXPRESSION <: NFExpression
  exp::Expression
end

struct RECORD_ELEMENT_EXPRESSION <: NFExpression
  recordExp::Expression
  index::Int
  fieldName::String
  ty::NFType
end

struct TUPLE_ELEMENT_EXPRESSION <: NFExpression
  tupleExp::Expression
  index::Int
  ty::NFType
end

struct SUBSCRIPTED_EXP_EXPRESSION <: NFExpression
  exp::Expression
  subscripts::List{Subscript}
  ty::NFType
end

struct UNBOX_EXPRESSION <: NFExpression
  exp::Expression
  ty::NFType
end

struct CAST_EXPRESSION <: NFExpression
  ty::NFType
  exp::Expression
end

struct IF_EXPRESSION <: NFExpression
  condition::Expression
  trueBranch::Expression
  falseBranch::Expression
end

mutable struct RELATION_EXPRESSION <: NFExpression
  exp1::Expression
  operator::Operator
  exp2::Expression
end

mutable struct LUNARY_EXPRESSION <: NFExpression
  operator::Operator
  exp::Expression
end

mutable struct LBINARY_EXPRESSION <: NFExpression
  exp1::Expression
  operator::Operator
  exp2::Expression
end

mutable struct UNARY_EXPRESSION <: NFExpression
  operator::Operator
  exp::Expression
end

mutable struct BINARY_EXPRESSION <: NFExpression
  exp1::Expression
  operator::Operator
  exp2::Expression
end

struct END_EXPRESSION <: NFExpression
end

struct SIZE_EXPRESSION <: NFExpression
  exp::Expression
  dimIndex::Option{Expression}
end

struct CALL_EXPRESSION <: NFExpression
  call::Call
end

mutable struct RECORD_EXPRESSION <: NFExpression
  #=  Maybe not needed since the type contains the name. Prefix? =#
  const path::Absyn.Path
  const ty::NFType
  elements::Vector{Expression}
end

struct TUPLE_EXPRESSION <: NFExpression
  ty::NFType
  elements::List{Expression}
end

struct RANGE_EXPRESSION <: NFExpression
  ty::NFType
  start::Expression
  step::Option{Expression}
  stop::Expression
end

struct MATRIX_EXPRESSION <: NFExpression
  #=  Does not have a type since we only keep this operator before type-checking =#
  elements::List{List{Expression}}
end

struct ARRAY_EXPRESSION <: NFExpression
  ty::NFType
  elements::List{Expression}
  literal #= True if the array is known to only contain literal expressions. =#::Bool
end

struct TYPENAME_EXPRESSION <: NFExpression
  ty::NFType
end

struct CREF_EXPRESSION <: NFExpression
  ty::NFType
  cref::ComponentRef
end

struct ENUM_LITERAL_EXPRESSION{T0 <: NFType, T1 <: String, T2 <: Int} <: NFExpression
  ty::NFType
  name::String
  index::Int
end

struct BOOLEAN_EXPRESSION{T0 <: Bool} <: NFExpression
  value::T0
end

struct STRING_EXPRESSION{T0 <: String} <: NFExpression
  value::T0
end

struct REAL_EXPRESSION{T0 <: AbstractFloat} <: NFExpression
  value::T0
end

struct INTEGER_EXPRESSION{T0 <: Integer} <: NFExpression
  value::T0
end
