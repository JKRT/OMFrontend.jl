EvalFunc = Function
FuncT = Function
ContainsPred = Function
MapFunc = Function
ApplyFunc = Function
FoldFunc = Function
MapFunc = Function

@UniontypeDecl NFExpression
Expression = NFExpression
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

@Uniontype NFExpression begin
  @Record BINDING_EXP begin
    exp::Expression
    expType #= The actual type of exp. =#::NFType
    bindingType #= The type of the propagated binding. =#::NFType
    parents::List{InstNode}
    isEach::Bool
  end

  @Record PARTIAL_FUNCTION_APPLICATION_EXPRESSION begin
    fn::ComponentRef
    args::List{Expression}
    argNames::List{String}
    ty::NFType
  end

  @Record CLKCONST_EXPRESSION begin
    clk #= Clock kinds =#::ClockKind
  end

  @Record EMPTY_EXPRESSION begin
    ty::NFType
  end

  @Record MUTABLE_EXPRESSION begin
    exp::Pointer{Expression}
  end

  @Record BOX_EXPRESSION begin
    exp::Expression
  end

  @Record RECORD_ELEMENT_EXPRESSION begin
    recordExp::Expression
    index::Integer
    fieldName::String
    ty::NFType
  end

  @Record TUPLE_ELEMENT_EXPRESSION begin
    tupleExp::Expression
    index::Integer
    ty::NFType
  end

  @Record SUBSCRIPTED_EXP_EXPRESSION begin
    exp::Expression
    subscripts::List{Subscript}
    ty::NFType
  end

  @Record UNBOX_EXPRESSION begin
    exp::Expression
    ty::NFType
  end

  @Record CAST_EXPRESSION begin
    ty::NFType
    exp::Expression
  end

  @Record IF_EXPRESSION begin
    condition::Expression
    trueBranch::Expression
    falseBranch::Expression
  end

  @Record RELATION_EXPRESSION begin
    exp1::Expression
    operator::Operator
    exp2::Expression
  end

  @Record LUNARY_EXPRESSION begin
    operator::Operator
    exp::Expression
  end

  @Record LBINARY_EXPRESSION begin
    exp1::Expression
    operator::Operator
    exp2::Expression
  end

  @Record UNARY_EXPRESSION begin
    operator::Operator
    exp::Expression
  end

  @Record BINARY_EXPRESSION begin
    exp1::Expression
    operator::Operator
    exp2::Expression
  end

  @Record END_EXPRESSION begin
  end

  @Record SIZE_EXPRESSION begin
    exp::Expression
    dimIndex::Option{Expression}
  end

  @Record CALL_EXPRESSION begin
    call::Call
  end

  @Record RECORD_EXPRESSION begin
    path
    #=  Maybe not needed since the type contains the name. Prefix? =#
    ty::NFType
    elements::List{Expression}
  end

  @Record TUPLE_EXPRESSION begin
    ty::NFType
    elements::List{Expression}
  end

  @Record RANGE_EXPRESSION begin
    ty::NFType
    start::Expression
    step::Option{Expression}
    stop::Expression
  end

  @Record MATRIX_EXPRESSION begin
    #=  Does not have a type since we only keep this operator before type-checking
    =#
    elements::List{List{Expression}}
  end

  @Record ARRAY_EXPRESSION begin
    ty::NFType
    elements::List{Expression}
    literal #= True if the array is known to only contain literal expressions. =#::Bool
  end

  @Record TYPENAME_EXPRESSION begin
    ty::NFType
  end

  @Record CREF_EXPRESSION begin
    ty::NFType
    cref::ComponentRef
  end

  @Record ENUM_LITERAL_EXPRESSION begin
    ty::NFType
    name::String
    index::Integer
  end

  @Record BOOLEAN_EXPRESSION begin
    value::Bool
  end

  @Record STRING_EXPRESSION begin
    value::String
  end

  @Record REAL_EXPRESSION begin
    value::AbstractFloat
  end

  @Record INTEGER_EXPRESSION begin
    value::Integer
  end
end
