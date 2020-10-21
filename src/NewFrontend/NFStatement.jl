@UniontypeDecl NFStatement
@Uniontype NFStatement begin
  @Record ALG_FAILURE begin
    body::List{Statement}
    source::DAE.ElementSource
  end

  @Record ALG_BREAK begin
    source::DAE.ElementSource
  end

  @Record ALG_RETURN begin
    source::DAE.ElementSource
  end

  @Record ALG_WHILE begin
    condition::Expression
    body::List{Statement}
    source::DAE.ElementSource
  end

  @Record ALG_NORETCALL begin
    exp::Expression
    source::DAE.ElementSource
  end

  @Record ALG_TERMINATE begin
    message::Expression #= The message to display if the terminate triggers. =#
    source::DAE.ElementSource
  end

  @Record ALG_ASSERT begin
    condition::Expression #= The assert condition. =#
    message::Expression #= The message to display if the assert fails. =#
    level::Expression
    source::DAE.ElementSource
  end

  @Record ALG_WHEN begin
    branches::List{Tuple{Expression, List{Statement}}} #= List of branches, where each branch is a tuple of a condition and a body. =#
    source::DAE.ElementSource
  end

  @Record ALG_IF begin
    branches::List{Tuple{Expression, List{Statement}}} #= List of branches, where each branch is a tuple of a condition and a body. =#
    source::DAE.ElementSource
  end

  @Record ALG_FOR begin
    iterator::InstNode
    range::Option{Expression}
    body::List{Statement} #= The body of the for loop. =#
    source::DAE.ElementSource
  end

  @Record ALG_FUNCTION_ARRAY_INIT begin
    name::String
    ty::M_Type
    source::DAE.ElementSource
  end

  @Record ALG_ASSIGNMENT begin
    lhs::Expression #= The asignee =#
    rhs::Expression #= The expression =#
    ty::NFType
    source::DAE.ElementSource
  end
end

function isMultiLine(stmt::Statement)::Bool
  local multiLine::Bool
  @assign multiLine = begin
    @match stmt begin
      ALG_FOR(__) => begin
        true
      end
      ALG_IF(__) => begin
        true
      end
      ALG_WHEN(__) => begin
        true
      end
      ALG_WHILE(__) => begin
        true
      end
      _ => begin
        false
      end
    end
  end
  return multiLine
end

function toFlatStreamList(
  stmtl::List{<:Statement},
  indent::String,
  s,
)

  local prev_multi_line::Bool = false
  local multi_line::Bool
  local first::Bool = true

  for stmt in stmtl
    @assign multi_line = isMultiLine(stmt)
    if first
      @assign first = false
    elseif prev_multi_line || multi_line
      @assign s = IOStream.append(s, "\\n")
    end
    @assign prev_multi_line = multi_line
    @assign s = toFlatStream(stmt, indent, s)
    @assign s = IOStream.append(s, ";\\n")
  end
  #=  Improve human parsability by separating statements that spans multiple
  =#
  #=  lines (like if-statements) with newlines.
  =#
  return s
end

function toFlatStream(
  stmt::Statement,
  indent::String,
  s,
)
  local str::String
  @assign s = IOStream.append(s, indent)
  @assign s = begin
    @match stmt begin
      ALG_ASSIGNMENT(__) => begin
        @assign s = IOStream.append(s, toFlatString(stmt.lhs))
        @assign s = IOStream.append(s, " := ")
        @assign s = IOStream.append(s, toFlatString(stmt.rhs))
        s
      end

      ALG_FUNCTION_ARRAY_INIT(__) => begin
        @assign s = IOStream.append(s, "array init")
        @assign s = IOStream.append(s, stmt.name)
        s
      end

      ALG_FOR(__) => begin
        @assign s = IOStream.append(s, "for ")
        @assign s = IOStream.append(s, name(stmt.iterator))
        if isSome(stmt.range)
          @assign s = IOStream.append(s, " in ")
          @assign s = IOStream.append(
            s,
            toFlatString(Util.getOption(stmt.range)),
          )
        end
        @assign s = IOStream.append(s, " loop\\n")
        @assign s = toFlatStreamList(stmt.body, indent + "  ", s)
        @assign s = IOStream.append(s, indent)
        @assign s = IOStream.append(s, "end for")
        s
      end

      ALG_IF(__) => begin
        @assign str = "if "
        for b in stmt.branches
          @assign s = IOStream.append(s, str)
          @assign s =
            IOStream.append(s, toFlatString(Util.tuple21(b)))
          @assign s = IOStream.append(s, " then\\n")
          @assign s = toFlatStreamList(Util.tuple22(b), indent + "  ", s)
          @assign s = IOStream.append(s, indent)
          @assign str = "elseif "
        end
        @assign s = IOStream.append(s, "end if")
        s
      end

      ALG_WHEN(__) => begin
        @assign str = "when "
        for b in stmt.branches
          @assign s = IOStream.append(s, str)
          @assign s =
            IOStream.append(s, toFlatString(Util.tuple21(b)))
          @assign s = IOStream.append(s, " then\\n")
          @assign s = toFlatStreamList(Util.tuple22(b), indent + "  ", s)
          @assign s = IOStream.append(s, indent)
          @assign str = "elsewhen "
        end
        @assign s = IOStream.append(s, "end when")
        s
      end

      ALG_ASSERT(__) => begin
        @assign s = IOStream.append(s, "assert(")
        @assign s =
          IOStream.append(s, toFlatString(stmt.condition))
        @assign s = IOStream.append(s, ", ")
        @assign s = IOStream.append(s, toFlatString(stmt.message))
        @assign s = IOStream.append(s, ", ")
        @assign s = IOStream.append(s, toFlatString(stmt.level))
        @assign s = IOStream.append(s, ")")
        s
      end

      ALG_TERMINATE(__) => begin
        @assign s = IOStream.append(s, "terminate(")
        @assign s = IOStream.append(s, toFlatString(stmt.message))
        @assign s = IOStream.append(s, ")")
        s
      end

      ALG_NORETCALL(__) => begin
        IOStream.append(s, toFlatString(stmt.exp))
      end

      ALG_WHILE(__) => begin
        @assign s = IOStream.append(s, "while ")
        @assign s =
          IOStream.append(s, toFlatString(stmt.condition))
        @assign s = IOStream.append(s, " then\\n")
        @assign s = toFlatStreamList(stmt.body, indent + "  ", s)
        @assign s = IOStream.append(s, indent)
        @assign s = IOStream.append(s, "end while")
        s
      end

      ALG_RETURN(__) => begin
        IOStream.append(s, "return")
      end

      ALG_RETURN(__) => begin
        IOStream.append(s, "break")
      end

      _ => begin
        IOStream.append(s, "#UNKNOWN STATEMENT#")
      end
    end
  end
  return s
end

function toStreamList(
  stmtl::List{<:Statement},
  indent::String,
  s,
)

  local prev_multi_line::Bool = false
  local multi_line::Bool
  local first::Bool = true

  for stmt in stmtl
    @assign multi_line = isMultiLine(stmt)
    if first
      @assign first = false
    elseif prev_multi_line || multi_line
      @assign s = IOStream.append(s, "\\n")
    end
    @assign prev_multi_line = multi_line
    @assign s = toStream(stmt, indent, s)
    @assign s = IOStream.append(s, ";\\n")
  end
  #=  Improve human parsability by separating statements that spans multiple
  =#
  #=  lines (like if-statements) with newlines.
  =#
  return s
end

function toStream(stmt::Statement, indent::String, s)

  local str::String

  @assign s = IOStream.append(s, indent)
  @assign s = begin
    @match stmt begin
      ASSIGNMENT(__) => begin
        @assign s = IOStream.append(s, toString(stmt.lhs))
        @assign s = IOStream.append(s, " := ")
        @assign s = IOStream.append(s, toString(stmt.rhs))
        s
      end

      FUNCTION_ARRAY_INIT(__) => begin
        @assign s = IOStream.append(s, "array init")
        @assign s = IOStream.append(s, stmt.name)
        s
      end

      FOR(__) => begin
        @assign s = IOStream.append(s, "for ")
        @assign s = IOStream.append(s, name(stmt.iterator))
        if isSome(stmt.range)
          @assign s = IOStream.append(s, " in ")
          @assign s = IOStream.append(
            s,
            toString(Util.getOption(stmt.range)),
          )
        end
        @assign s = IOStream.append(s, " loop\\n")
        @assign s = toStreamList(stmt.body, indent + "  ", s)
        @assign s = IOStream.append(s, indent)
        @assign s = IOStream.append(s, "end for")
        s
      end

      IF(__) => begin
        @assign str = "if "
        for b in stmt.branches
          @assign s = IOStream.append(s, str)
          @assign s =
            IOStream.append(s, toString(Util.tuple21(b)))
          @assign s = IOStream.append(s, " then\\n")
          @assign s = toStreamList(Util.tuple22(b), indent + "  ", s)
          @assign s = IOStream.append(s, indent)
          @assign str = "elseif "
        end
        @assign s = IOStream.append(s, "end if")
        s
      end

      WHEN(__) => begin
        @assign str = "when "
        for b in stmt.branches
          @assign s = IOStream.append(s, str)
          @assign s =
            IOStream.append(s, toString(Util.tuple21(b)))
          @assign s = IOStream.append(s, " then\\n")
          @assign s = toStreamList(Util.tuple22(b), indent + "  ", s)
          @assign s = IOStream.append(s, indent)
          @assign str = "elsewhen "
        end
        @assign s = IOStream.append(s, "end when")
        s
      end

      ASSERT(__) => begin
        @assign s = IOStream.append(s, "assert(")
        @assign s = IOStream.append(s, toString(stmt.condition))
        @assign s = IOStream.append(s, ", ")
        @assign s = IOStream.append(s, toString(stmt.message))
        @assign s = IOStream.append(s, ", ")
        @assign s = IOStream.append(s, toString(stmt.level))
        @assign s = IOStream.append(s, ")")
        s
      end

      TERMINATE(__) => begin
        @assign s = IOStream.append(s, "terminate(")
        @assign s = IOStream.append(s, toString(stmt.message))
        @assign s = IOStream.append(s, ")")
        s
      end

      NORETCALL(__) => begin
        IOStream.append(s, toString(stmt.exp))
      end

      WHILE(__) => begin
        @assign s = IOStream.append(s, "while ")
        @assign s = IOStream.append(s, toString(stmt.condition))
        @assign s = IOStream.append(s, " then\\n")
        @assign s = toStreamList(stmt.body, indent + "  ", s)
        @assign s = IOStream.append(s, indent)
        @assign s = IOStream.append(s, "end while")
        s
      end

      RETURN(__) => begin
        IOStream.append(s, "return")
      end

      RETURN(__) => begin
        IOStream.append(s, "break")
      end

      _ => begin
        IOStream.append(s, "#UNKNOWN STATEMENT#")
      end
    end
  end
  return s
end

function toStringList(stmtl::List{<:Statement}, indent::String = "")::String
  local str::String

  local s

  @assign s = IOStream.create(getInstanceName(), IOStream.IOStreamType.LIST())
  @assign s = toStreamList(stmtl, indent, s)
  @assign str = IOStream.string(s)
  IOStream.delete(s)
  return str
end

function toString(stmt::Statement, indent::String = "")::String
  local str::String

  local s

  @assign s = IOStream.create(getInstanceName(), IOStream.IOStreamType.LIST())
  @assign s = toStream(stmt, indent, s)
  @assign str = IOStream.string(s)
  IOStream.delete(s)
  return str
end

function foldExp(stmt::Statement, func::FoldFunc, arg::ArgT) where {ArgT}

  @assign () = begin
    @match stmt begin
      P_Statement.Statement.ASSIGNMENT(__) => begin
        @assign arg = func(stmt.lhs, arg)
        @assign arg = func(stmt.rhs, arg)
        ()
      end

      P_Statement.Statement.FOR(__) => begin
        @assign arg = foldExpList(stmt.body, func, arg)
        if isSome(stmt.range)
          @assign arg = func(Util.getOption(stmt.range), arg)
        end
        ()
      end

      P_Statement.Statement.IF(__) => begin
        for b in stmt.branches
          @assign arg = func(Util.tuple21(b), arg)
          @assign arg = foldExpList(Util.tuple22(b), func, arg)
        end
        ()
      end

      P_Statement.Statement.WHEN(__) => begin
        for b in stmt.branches
          @assign arg = func(Util.tuple21(b), arg)
          @assign arg = foldExpList(Util.tuple22(b), func, arg)
        end
        ()
      end

      P_Statement.Statement.ASSERT(__) => begin
        @assign arg = func(stmt.condition, arg)
        @assign arg = func(stmt.message, arg)
        @assign arg = func(stmt.level, arg)
        ()
      end

      P_Statement.Statement.TERMINATE(__) => begin
        @assign arg = func(stmt.message, arg)
        ()
      end

      P_Statement.Statement.NORETCALL(__) => begin
        @assign arg = func(stmt.exp, arg)
        ()
      end

      P_Statement.Statement.WHILE(__) => begin
        @assign arg = func(stmt.condition, arg)
        @assign arg = foldExpList(stmt.body, func, arg)
        ()
      end

      _ => begin
        ()
      end
    end
  end
  return arg
end

function foldExpList(stmt::List{Statement}, func::FoldFunc, arg::ArgT) where {ArgT}

  for s in stmt
    @assign arg = foldExp(s, func, arg)
  end
  return arg
end
MapFunc = Function
function mapExp(stmt::Statement, func::MapFunc)::Statement
  @assign stmt = begin
    local e1::Expression
    local e2::Expression
    local e3::Expression
    @match stmt begin
      ASSIGNMENT(__) => begin
        @assign e1 = func(stmt.lhs)
        @assign e2 = func(stmt.rhs)
        if referenceEq(e1, stmt.lhs) && referenceEq(e2, stmt.rhs)
          stmt
        else
          ASSIGNMENT(e1, e2, stmt.ty, stmt.source)
        end
      end

      FOR(__) => begin
        @assign stmt.body = mapExpList(stmt.body, func)
        @assign stmt.range = Util.applyOption(stmt.range, func)
        stmt
      end

      IF(__) => begin
        @assign stmt.branches = list(
          (func(Util.tuple21(b)), mapExpList(Util.tuple22(b), func)) for b in stmt.branches
        )
        stmt
      end

      WHEN(__) => begin
        @assign stmt.branches = list(
          (func(Util.tuple21(b)), mapExpList(Util.tuple22(b), func)) for b in stmt.branches
        )
        stmt
      end

      ASSERT(__) => begin
        @assign e1 = func(stmt.condition)
        @assign e2 = func(stmt.message)
        @assign e3 = func(stmt.level)
        if referenceEq(e1, stmt.condition) &&
           referenceEq(e2, stmt.message) &&
           referenceEq(e3, stmt.level)
          stmt
        else
          ASSERT(e1, e2, e3, stmt.source)
        end
      end

      TERMINATE(__) => begin
        @assign e1 = func(stmt.message)
        if referenceEq(e1, stmt.message)
          stmt
        else
          TERMINATE(e1, stmt.source)
        end
      end

      NORETCALL(__) => begin
        @assign e1 = func(stmt.exp)
        if referenceEq(e1, stmt.exp)
          stmt
        else
          NORETCALL(e1, stmt.source)
        end
      end

      WHILE(__) => begin
        WHILE(func(stmt.condition), mapExpList(stmt.body, func), stmt.source)
      end

      _ => begin
        stmt
      end
    end
  end
  return stmt
end

function mapExpList(stmtl::List{<:Statement}, func::MapFunc)::List{Statement}

  @assign stmtl = list(mapExp(s, func) for s in stmtl)
  return stmtl
end

function map(stmt::Statement, func::MapFn)::Statement

  @assign () = begin
    @match stmt begin
      FOR(__) => begin
        @assign stmt.body = list(map(s, func) for s in stmt.body)
        ()
      end

      IF(__) => begin
        @assign stmt.branches = list(
          (Util.tuple21(b), list(map(s, func) for s in Util.tuple22(b)))
          for b in stmt.branches
        )
        ()
      end

      WHEN(__) => begin
        @assign stmt.branches = list(
          (Util.tuple21(b), list(map(s, func) for s in Util.tuple22(b)))
          for b in stmt.branches
        )
        ()
      end

      WHILE(__) => begin
        @assign stmt.body = list(map(s, func) for s in stmt.body)
        ()
      end

      _ => begin
        ()
      end
    end
  end
  @assign stmt = func(stmt)
  return stmt
end

function apply(stmt::Statement, func::ApplyFn)
  @assign () = begin
    @match stmt begin
      FOR(__) => begin
        for e in stmt.body
          apply(e, func)
        end
        ()
      end

      IF(__) => begin
        for b in stmt.branches
          for e in Util.tuple22(b)
            apply(e, func)
          end
        end
        ()
      end

      WHEN(__) => begin
        for b in stmt.branches
          for e in Util.tuple22(b)
            apply(e, func)
          end
        end
        ()
      end

      WHILE(__) => begin
        for e in stmt.body
          apply(e, func)
        end
        ()
      end

      ALG_FAILURE(__) => begin
        for e in stmt.body
          apply(e, func)
        end
        ()
      end

      _ => begin
        ()
      end
    end
  end
  return func(stmt)
end

function info(stmt::Statement)::SourceInfo
  local info::SourceInfo = ElementSource.getInfo(source(stmt))
  return info
end

function source(stmt::Statement)::DAE.ElementSource
  local source::DAE.ElementSource
  return stmt.source
end

function makeIf(
  branches::List{<:Tuple{<:Expression, List{<:Statement}}},
  src::DAE.ElementSource,
)::Statement
  local stmt::Statement

  @assign stmt = IF(branches, src)
  return stmt
end

function makeAssignment(
  lhs::Expression,
  rhs::Expression,
  ty::M_Type,
  src::DAE.ElementSource,
)::Statement
  local stmt::Statement

  @assign stmt = ASSIGNMENT(lhs, rhs, ty, src)
  return stmt
end

function updateImplicitVariabilityAlg(alg::Algorithm)
  updateImplicitVariabilityStmts(alg.statements)
end

function updateImplicitVariabilityStmts(stmtl::List{<:Statement}, inWhen::Bool = false)
  for s in stmtl
    updateImplicitVariabilityStmt(s, inWhen)
  end
end

function updateImplicitVariabilityStmt(stmt::Statement, inWhen::Bool)
  @assign () = begin
    @match stmt begin
     ASSIGNMENT(__)  => begin
        if inWhen
          markImplicitWhenExp(stmt.lhs)
        end
        ()
      end

     FOR(__)  => begin
        #=  'when' is not allowed in 'for', so we only need to keep going if
        =#
        #=  we're already in a 'when'.
        =#
        if inWhen
          updateImplicitVariabilityStmts(stmt.body, true)
        end
        ()
      end
     IF(__)  => begin
        #=  'when' is not allowed in 'if', so we only need to keep going if
        =#
        #=  we're already in a 'when.
        =#
        if inWhen
          for branch in stmt.branches
            updateImplicitVariabilityStmts(Util.tuple22(branch), true)
          end
        end
        ()
      end
      WHEN(__)  => begin
        for branch in stmt.branches
          updateImplicitVariabilityStmts(Util.tuple22(branch), true)
        end
        ()
      end
     WHILE(__)  => begin
        if inWhen
          updateImplicitVariabilityStmts(stmt.body, true)
        end
        ()
      end
      _  => begin
        ()
      end
    end
  end
end

function markStructuralParamsSubs(exp::Expression, dummy::Integer) ::Integer


  @assign () = begin
    @match exp begin
      CREF_EXPRESSION(__)  => begin
        ComponentRef.foldSubscripts(exp.cref, markStructuralParamsSub, 0)
        ()
      end

      _  => begin
        ()
      end
    end
  end
  dummy
end



function instStatement(scodeStmt::SCode.Statement, scope::InstNode, origin::ORIGIN_Type)::Statement
  local statement::Statement
  @assign statement = begin
    local exp1::Expression
    local exp2::Expression
    local exp3::Expression
    local oexp::Option{Expression}
    local stmtl::List{Statement}
    local branches::List{Tuple{Expression, List{Statement}}}
    local info::SourceInfo
    local for_scope::InstNode
    local iter::InstNode
    local next_origin::ORIGIN_Type
    @match scodeStmt begin
      SCode.ALG_ASSIGN(info = info)  => begin
        @assign exp1 = instExp(scodeStmt.assignComponent, scope, info)
        @assign exp2 = instExp(scodeStmt.value, scope, info)
        ALG_ASSIGNMENT(exp1, exp2, TYPE_UNKNOWN(), makeSource(scodeStmt.comment, info))
      end

      SCode.ALG_FOR(info = info)  => begin
        @assign oexp = instExpOpt(scodeStmt.range, scope, info)
        @assign (for_scope, iter) = addIteratorToScope(scodeStmt.index, scope, info)
        @assign next_origin = setFlag(origin, ORIGIN_FOR)
        @assign stmtl = instStatements(scodeStmt.forBody, for_scope, next_origin)
        ALG_FOR(iter, oexp, stmtl, makeSource(scodeStmt.comment, info))
      end

      SCode.ALG_IF(info = info)  => begin
        @assign branches = nil
        @assign next_origin = setFlag(origin, ORIGIN_FOR)
        for branch in _cons((scodeStmt.boolExpr, scodeStmt.trueBranch), scodeStmt.elseIfBranch)
          @assign exp1 = instExp(Util.tuple21(branch), scope, info)
          @assign stmtl = instStatements(Util.tuple22(branch), scope, next_origin)
          @assign branches = _cons((exp1, stmtl), branches)
        end
        if ! listEmpty(scodeStmt.elseBranch)
          @assign stmtl = instStatements(scodeStmt.elseBranch, scope, next_origin)
          @assign branches = _cons((P_Expression.BOOLEAN_EXPRESSION(true), stmtl), branches)
        end
        ALG_IF(listReverse(branches), makeSource(scodeStmt.comment, info))
      end

      SCode.ALG_WHEN_A(info = info)  => begin
        if origin > 0
          if flagSet(origin, ORIGIN_WHEN)
            Error.addSourceMessageAndFail(Error.NESTED_WHEN, nil, info)
          elseif flagSet(origin, ORIGIN_INITIAL)
            Error.addSourceMessageAndFail(Error.INITIAL_WHEN, nil, info)
          else
            Error.addSourceMessageAndFail(Error.INVALID_WHEN_STATEMENT_CONTEXT, nil, info)
          end
        end
        @assign branches = nil
        for branch in scodeStmt.branches
          @assign exp1 = instExp(Util.tuple21(branch), scope, info)
          @assign next_origin = setFlag(origin, ORIGIN_WHEN)
          @assign stmtl = instStatements(Util.tuple22(branch), scope, next_origin)
          @assign branches = _cons((exp1, stmtl), branches)
        end
        ALG_WHEN(listReverse(branches), makeSource(scodeStmt.comment, info))
      end

      SCode.ALG_ASSERT(info = info)  => begin
        @assign exp1 = instExp(scodeStmt.condition, scope, info)
        @assign exp2 = instExp(scodeStmt.message, scope, info)
        @assign exp3 = instExp(scodeStmt.level, scope, info)
       ALG_ASSERT(exp1, exp2, exp3, makeSource(scodeStmt.comment, info))
      end

      SCode.ALG_TERMINATE(info = info)  => begin
        @assign exp1 = instExp(scodeStmt.message, scope, info)
       ALG_TERMINATE(exp1, makeSource(scodeStmt.comment, info))
      end

      SCode.ALG_REINIT(info = info)  => begin
        Error.addSourceMessage(Error.REINIT_NOT_IN_WHEN, nil, info)
        fail()
      end

      SCode.ALG_NORETCALL(info = info)  => begin
        @assign exp1 = instExp(scodeStmt.exp, scope, info)
       ALG_NORETCALL(exp1, makeSource(scodeStmt.comment, info))
      end

      SCode.ALG_WHILE(info = info)  => begin
        @assign exp1 = instExp(scodeStmt.boolExpr, scope, info)
        @assign next_origin = setFlag(origin, ORIGIN_WHILE)
        @assign stmtl = instStatements(scodeStmt.whileBody, scope, next_origin)
       ALG_WHILE(exp1, stmtl, makeSource(scodeStmt.comment, info))
      end

      SCode.ALG_RETURN(__)  => begin
       ALG_RETURN(makeSource(scodeStmt.comment, scodeStmt.info))
      end

      SCode.ALG_BREAK(__)  => begin
       ALG_BREAK(makeSource(scodeStmt.comment, scodeStmt.info))
      end

      SCode.ALG_FAILURE(__)  => begin
        @assign stmtl = instStatements(scodeStmt.stmts, scope, origin)
       ALG_FAILURE(stmtl, makeSource(scodeStmt.comment, scodeStmt.info))
      end

      _  => begin
        Error.assertion(false, getInstanceName() + " got unknown statement", sourceInfo())
        fail()
      end
    end
  end
statement
end

function instAlgorithmSections(algorithmSections::List{<:SCode.AlgorithmSection}, scope::InstNode, origin::ORIGIN_Type) ::List{Algorithm}
  local algs::List{Algorithm}
  @assign algs = list(instAlgorithmSection(alg, scope, origin) for alg in algorithmSections)
  algs
end

function instAlgorithmSection(algorithmSection::SCode.AlgorithmSection, scope::InstNode, origin::ORIGIN_Type) ::Algorithm
  local alg::Algorithm
  @assign alg = ALGORITHM(instStatements(algorithmSection.statements, scope, origin), DAE.emptyElementSource)
  alg
end

function instStatements(scodeStmtl::List{<:SCode.Statement}, scope::InstNode, origin::ORIGIN_Type)::List{Statement}
  local statements::List{Statement}
  @assign statements = list(instStatement(stmt, scope, origin) for stmt in scodeStmtl)
  statements
end
