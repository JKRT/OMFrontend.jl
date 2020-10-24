PredFn = Function
PredFn = Function
FoldFunc = Function
FoldFunc = Function
MapExpFn = Function
MapFn = Function
ApplyFn = Function
@UniontypeDecl NFEquation
@UniontypeDecl Equation_Branch
ComponentRef = NFComponentRef
import ..Util
Equation = NFEquation

@Uniontype NFEquation begin
  @Record EQUATION_NORETCALL begin
    exp::Expression
    source::DAE.ElementSource
  end

  @Record EQUATION_REINIT begin
    cref::Expression #= The variable to reinitialize. =#
    reinitExp::Expression #= The new value of the variable. =#
    source::DAE.ElementSource
  end

  @Record EQUATION_TERMINATE begin
    message::Expression #= The message to display if the terminate triggers. =#
    source::DAE.ElementSource
  end

  @Record EQUATION_ASSERT begin
    condition::Expression #= The assert condition. =#
    message::Expression #= The message to display if the assert fails. =#
    level::Expression #= Error or warning =#
    source::DAE.ElementSource
  end

  @Record EQUATION_WHEN begin
    branches::List{Equation_Branch}
    source::DAE.ElementSource
  end

  @Record EQUATION_IF begin
    branches::List{Equation_Branch}
    source::DAE.ElementSource
  end

  @Record EQUATION_FOR begin
    iterator::InstNode
    range::Option{Expression}
    body::List{Equation} #= The body of the for loop. =#
    source::DAE.ElementSource
  end

  @Record EQUATION_CONNECT begin
    lhs::Expression
    rhs::Expression
    source::DAE.ElementSource
  end

  @Record EQUATION_ARRAY_EQUALITY begin
    lhs::Expression
    rhs::Expression
    ty::NFType
    source::DAE.ElementSource
  end

  @Record EQUATION_CREF_EQUALITY begin
    lhs::ComponentRef
    rhs::ComponentRef
    source::DAE.ElementSource
  end

  @Record EQUATION_EQUALITY begin
    lhs::Expression #= The left hand side expression. =#
    rhs::Expression #= The right hand side expression. =#
    ty::NFType
    source::DAE.ElementSource
  end
end


@UniontypeDecl Equation_Branch
@Uniontype Equation_Branch begin
  @Record EQUATION_INVALID_BRANCH begin
    branch::Equation_Branch
    errors::List
  end
  @Record EQUATION_BRANCH begin
    condition::Expression
    conditionVar
    body::List
  end
end

function isMultiLine(eq::Equation)::Bool
  local singleLine::Bool
  @assign singleLine = begin
    @match eq begin
      EQUATION_FOR(__) => begin
        true
      end
      EQUATION_IF(__) => begin
        true
      end
      EQUATION_WHEN(__) => begin
        true
      end
      _ => begin
        false
      end
    end
  end
  return singleLine
end

function toFlatStreamList(
  eql::List{<:Equation},
  indent::String,
  s,
)

  local prev_multi_line::Bool = false
  local multi_line::Bool
  local first::Bool = true

  for eq in eql
    @assign multi_line = isMultiLine(eq)
    if first
      @assign first = false
    elseif prev_multi_line || multi_line
      @assign s = IOStream.append(s, "\\n")
    end
    @assign prev_multi_line = multi_line
    @assign s = toFlatStream(eq, indent, s)
    @assign s = IOStream.append(s, ";\\n")
  end
  #=  Improve human parsability by separating statements that spans multiple
  =#
  #=  lines (like if-equations) with newlines.
  =#
  return s
end

function toFlatStream(eq::Equation, indent::String, s)
  @assign s = IOStream.append(s, indent)
  @assign s = begin
    @match eq begin
      EQUALITY(__) => begin
        @assign s = IOStream.append(s, toFlatString(eq.lhs))
        @assign s = IOStream.append(s, " = ")
        @assign s = IOStream.append(s, toFlatString(eq.rhs))
        s
      end

      CREF_EQUALITY(__) => begin
        @assign s = IOStream.append(s, toFlatString(eq.lhs))
        @assign s = IOStream.append(s, " = ")
        @assign s = IOStream.append(s, toFlatString(eq.rhs))
        s
      end

      ARRAY_EQUALITY(__) => begin
        @assign s = IOStream.append(s, toFlatString(eq.lhs))
        @assign s = IOStream.append(s, " = ")
        @assign s = IOStream.append(s, toFlatString(eq.rhs))
        s
      end

      CONNECT(__) => begin
        @assign s = IOStream.append(s, "connect(")
        @assign s = IOStream.append(s, toFlatString(eq.lhs))
        @assign s = IOStream.append(s, " = ")
        @assign s = IOStream.append(s, toFlatString(eq.rhs))
        @assign s = IOStream.append(s, ")")
        s
      end

      FOR(__) => begin
        @assign s = IOStream.append(s, "for ")
        @assign s = IOStream.append(s, name(eq.iterator))
        if isSome(eq.range)
          @assign s = IOStream.append(s, " in ")
          @assign s = IOStream.append(
            s,
            toFlatString(Util.getOption(eq.range)),
          )
        end
        @assign s = IOStream.append(s, " loop\\n")
        @assign s = toFlatStreamList(eq.body, indent + "  ", s)
        @assign s = IOStream.append(s, indent)
        @assign s = IOStream.append(s, "end for")
        s
      end

      IF(__) => begin
        @assign s = IOStream.append(s, "if ")
        @assign s = toFlatStream(listHead(eq.branches), indent, s)
        for b in listRest(eq.branches)
          @assign s = IOStream.append(s, indent)
          @assign s = IOStream.append(s, "elseif ")
          @assign s = toFlatStream(b, indent, s)
        end
        @assign s = IOStream.append(s, indent)
        @assign s = IOStream.append(s, "end if")
        s
      end

      WHEN(__) => begin
        @assign s = IOStream.append(s, "when ")
        @assign s = toFlatStream(listHead(eq.branches), indent, s)
        for b in listRest(eq.branches)
          @assign s = IOStream.append(s, indent)
          @assign s = IOStream.append(s, "elsewhen ")
          @assign s = toFlatStream(b, indent, s)
        end
        @assign s = IOStream.append(s, indent)
        @assign s = IOStream.append(s, "end when")
        s
      end

      ASSERT(__) => begin
        @assign s = IOStream.append(s, "assert(")
        @assign s = IOStream.append(s, toFlatString(eq.condition))
        @assign s = IOStream.append(s, ", ")
        @assign s = IOStream.append(s, toFlatString(eq.message))
        @assign s = IOStream.append(s, ", ")
        @assign s = IOStream.append(s, toFlatString(eq.level))
        @assign s = IOStream.append(s, ")")
        s
      end

      TERMINATE(__) => begin
        @assign s = IOStream.append(s, "terminate(")
        @assign s = IOStream.append(s, toFlatString(eq.message))
        @assign s = IOStream.append(s, ")")
        s
      end

      REINIT(__) => begin
        @assign s = IOStream.append(s, "reinit(")
        @assign s = IOStream.append(s, toFlatString(eq.cref))
        @assign s = IOStream.append(s, ", ")
        @assign s = IOStream.append(s, toFlatString(eq.reinitExp))
        @assign s = IOStream.append(s, ")")
        s
      end

      NORETCALL(__) => begin
        IOStream.append(s, toFlatString(eq.exp))
      end

      _ => begin
        IOStream.append(s, "#UNKNOWN EQUATION#")
      end
    end
  end
  return s
end

function toStreamList(
  eql::List{<:Equation},
  indent::String,
  s,
)

  local prev_multi_line::Bool = false
  local multi_line::Bool
  local first::Bool = true

  for eq in eql
    @assign multi_line = isMultiLine(eq)
    if first
      @assign first = false
    elseif prev_multi_line || multi_line
      @assign s = IOStream.append(s, "\\n")
    end
    @assign prev_multi_line = multi_line
    @assign s = toStream(eq, indent, s)
    @assign s = IOStream.append(s, ";\\n")
  end
  #=  Improve human parsability by separating statements that spans multiple
  =#
  #=  lines (like if-equations) with newlines.
  =#
  return s
end

function toStream(eq::Equation, indent::String, s)

  @assign s = IOStream.append(s, indent)
  @assign s = begin
    @match eq begin
      EQUALITY(__) => begin
        @assign s = IOStream.append(s, toString(eq.lhs))
        @assign s = IOStream.append(s, " = ")
        @assign s = IOStream.append(s, toString(eq.rhs))
        s
      end

      CREF_EQUALITY(__) => begin
        @assign s = IOStream.append(s, toString(eq.lhs))
        @assign s = IOStream.append(s, " = ")
        @assign s = IOStream.append(s, toString(eq.rhs))
        s
      end

      ARRAY_EQUALITY(__) => begin
        @assign s = IOStream.append(s, toString(eq.lhs))
        @assign s = IOStream.append(s, " = ")
        @assign s = IOStream.append(s, toString(eq.rhs))
        s
      end

      CONNECT(__) => begin
        @assign s = IOStream.append(s, "connect(")
        @assign s = IOStream.append(s, toString(eq.lhs))
        @assign s = IOStream.append(s, " = ")
        @assign s = IOStream.append(s, toString(eq.rhs))
        @assign s = IOStream.append(s, ")")
        s
      end

      FOR(__) => begin
        @assign s = IOStream.append(s, "for ")
        @assign s = IOStream.append(s, name(eq.iterator))
        if isSome(eq.range)
          @assign s = IOStream.append(s, " in ")
          @assign s = IOStream.append(
            s,
            toString(Util.getOption(eq.range)),
          )
        end
        @assign s = IOStream.append(s, " loop\\n")
        @assign s = toStreamList(eq.body, indent + "  ", s)
        @assign s = IOStream.append(s, indent)
        @assign s = IOStream.append(s, "end for")
        s
      end

      IF(__) => begin
        @assign s = IOStream.append(s, "if ")
        @assign s = toStream(listHead(eq.branches), indent, s)
        for b in listRest(eq.branches)
          @assign s = IOStream.append(s, indent)
          @assign s = IOStream.append(s, "elseif ")
          @assign s = toStream(b, indent, s)
        end
        @assign s = IOStream.append(s, indent)
        @assign s = IOStream.append(s, "end if")
        s
      end

      WHEN(__) => begin
        @assign s = IOStream.append(s, "when ")
        @assign s = toStream(listHead(eq.branches), indent, s)
        for b in listRest(eq.branches)
          @assign s = IOStream.append(s, indent)
          @assign s = IOStream.append(s, "elsewhen ")
          @assign s = toStream(b, indent, s)
        end
        @assign s = IOStream.append(s, indent)
        @assign s = IOStream.append(s, "end when")
        s
      end

      ASSERT(__) => begin
        @assign s = IOStream.append(s, "assert(")
        @assign s = IOStream.append(s, toString(eq.condition))
        @assign s = IOStream.append(s, ", ")
        @assign s = IOStream.append(s, toString(eq.message))
        @assign s = IOStream.append(s, ", ")
        @assign s = IOStream.append(s, toString(eq.level))
        @assign s = IOStream.append(s, ")")
        s
      end

      TERMINATE(__) => begin
        @assign s = IOStream.append(s, "terminate(")
        @assign s = IOStream.append(s, toString(eq.message))
        @assign s = IOStream.append(s, ")")
        s
      end

      REINIT(__) => begin
        @assign s = IOStream.append(s, "reinit(")
        @assign s = IOStream.append(s, toString(eq.cref))
        @assign s = IOStream.append(s, ", ")
        @assign s = IOStream.append(s, toString(eq.reinitExp))
        @assign s = IOStream.append(s, ")")
        s
      end

      NORETCALL(__) => begin
        IOStream.append(s, toString(eq.exp))
      end

      _ => begin
        IOStream.append(s, "#UNKNOWN EQUATION#")
      end
    end
  end
  return s
end

function toStringList(eql::List{<:Equation}, indent::String = "")::String
  local str::String

  local s

  @assign s = IOStream.create(getInstanceName(), IOStream.IOStreamType.LIST())
  @assign s = toStreamList(eql, indent, s)
  @assign str = IOStream.string(s)
  IOStream.delete(s)
  return str
end

function toString(eq::Equation, indent::String = "")::String
  local str::String

  local s

  @assign s = IOStream.create(getInstanceName(), IOStream.IOStreamType.LIST())
  @assign s = toStream(eq, indent, s)
  @assign str = IOStream.string(s)
  IOStream.delete(s)
  return str
end

function isConnect(eq::Equation)::Bool
  local isConnect::Bool

  @assign isConnect = begin
    @match eq begin
      CONNECT(__) => begin
        true
      end

      _ => begin
        false
      end
    end
  end
  return isConnect
end

function containsList(eql::List{<:Equation}, func::PredFn)::Bool
  local res::Bool

  for eq in eql
    if contains(eq, func)
      @assign res = true
      return res
    end
  end
  @assign res = false
  return res
end

function contains(eq::Equation, func::PredFn)::Bool
  local res::Bool
  if func(eq)
    @assign res = true
    return res
  end
  @assign res = begin
    @match eq begin
      EQUATION_FOR(__) => begin
        containsList(eq.body, func)
      end
      EQUATION_IF(__) => begin
        for b in eq.branches
          @assign () = begin
            @match b begin
              EQUATION_BRANCH(__) => begin
                if containsList(b.body, func)
                  @assign res = true
                  return
                end
                ()
              end

              _ => begin
                ()
              end
            end
          end
        end
        false
      end
      EQUATION_WHEN(__) => begin
        for b in eq.branches
          @assign () = begin
            @match b begin
              EQUATION_BRANCH(__) => begin
                if containsList(b.body, func)
                  @assign res = true
                  return
                end
                ()
              end

              _ => begin
                ()
              end
            end
          end
        end
        false
      end

      _ => begin
        false
      end
    end
  end
  return res
end

function foldExp(eq::Equation, func::FoldFunc, arg::ArgT) where {ArgT}

  @assign () = begin
    @match eq begin
      EQUATION_EQUALITY(__) => begin
        @assign arg = func(eq.lhs, arg)
        @assign arg = func(eq.rhs, arg)
        ()
      end

      EQUATION_ARRAY_EQUALITY(__) => begin
        @assign arg = func(eq.lhs, arg)
        @assign arg = func(eq.rhs, arg)
        ()
      end

      EQUATION_CONNECT(__) => begin
        @assign arg = func(eq.lhs, arg)
        @assign arg = func(eq.rhs, arg)
        ()
      end

      EQUATION_FOR(__) => begin
        @assign arg = foldExpList(eq.body, func, arg)
        if isSome(eq.range)
          @assign arg = func(Util.getOption(eq.range), arg)
        end
        ()
      end

      EQUATION_IF(__) => begin
        for b in eq.branches
          @assign () = begin
            @match b begin
              EQUATION_BRANCH(__) => begin
                @assign arg = func(b.condition, arg)
                @assign arg = foldExpList(b.body, func, arg)
                ()
              end

              _ => begin
                ()
              end
            end
          end
        end
        ()
      end

      EQUATION_WHEN(__) => begin
        for b in eq.branches
          @assign () = begin
            @match b begin
              EQUATION_BRANCH(__) => begin
                @assign arg = func(b.condition, arg)
                @assign arg = foldExpList(b.body, func, arg)
                ()
              end

              _ => begin
                ()
              end
            end
          end
        end
        ()
      end

      EQUATION_ASSERT(__) => begin
        @assign arg = func(eq.condition, arg)
        @assign arg = func(eq.message, arg)
        @assign arg = func(eq.level, arg)
        ()
      end

      EQUATION_TERMINATE(__) => begin
        @assign arg = func(eq.message, arg)
        ()
      end

      EQUATION_REINIT(__) => begin
        @assign arg = func(eq.cref, arg)
        @assign arg = func(eq.reinitExp, arg)
        ()
      end

      EQUATION_NORETCALL(__) => begin
        @assign arg = func(eq.exp, arg)
        ()
      end

      _ => begin
        ()
      end
    end
  end
  return arg
end

function foldExpList(eq::List{Equation}, func::FoldFunc, arg::ArgT) where {ArgT}
  for e in eq
    @assign arg = foldExp(e, func, arg)
  end
  return arg
end

function foldExpList(eq::Nil{Any}, func::FoldFunc, arg::ArgT) where {ArgT}
  return arg
end

function mapExpBranch(branch::Equation_Branch, func::MapExpFn)::Equation_Branch

  local cond::Expression
  local eql::List{Equation}

  @assign branch = begin
    @match branch begin
      EQUATION_BRANCH(__) => begin
        @assign cond = func(branch.condition)
        @assign eql = list(mapExp(e, func) for e in branch.body)
        EQUATION_BRANCH(cond, branch.conditionVar, eql)
      end

      _ => begin
        branch
      end
    end
  end
  return branch
end

function mapExp(eq::Equation, func::MapExpFn)::Equation

  @assign eq = begin
    local e1::Expression
    local e2::Expression
    local e3::Expression
    @match eq begin
      EQUALITY(__) => begin
        @assign e1 = func(eq.lhs)
        @assign e2 = func(eq.rhs)
        if referenceEq(e1, eq.lhs) && referenceEq(e2, eq.rhs)
          eq
        else
          EQUALITY(e1, e2, eq.ty, eq.source)
        end
      end

      ARRAY_EQUALITY(__) => begin
        @assign e1 = func(eq.lhs)
        @assign e2 = func(eq.rhs)
        if referenceEq(e1, eq.lhs) && referenceEq(e2, eq.rhs)
          eq
        else
          ARRAY_EQUALITY(e1, e2, eq.ty, eq.source)
        end
      end

      CONNECT(__) => begin
        @assign e1 = func(eq.lhs)
        @assign e2 = func(eq.rhs)
        if referenceEq(e1, eq.lhs) && referenceEq(e2, eq.rhs)
          eq
        else
          CONNECT(e1, e2, eq.source)
        end
      end

      FOR(__) => begin
        @assign eq.body = list(mapExp(e, func) for e in eq.body)
        @assign eq.range = Util.applyOption(eq.range, func)
        eq
      end

      IF(__) => begin
        @assign eq.branches = list(mapExpBranch(b, func) for b in eq.branches)
        eq
      end

      WHEN(__) => begin
        @assign eq.branches = list(mapExpBranch(b, func) for b in eq.branches)
        eq
      end

      ASSERT(__) => begin
        @assign e1 = func(eq.condition)
        @assign e2 = func(eq.message)
        @assign e3 = func(eq.level)
        if referenceEq(e1, eq.condition) &&
           referenceEq(e2, eq.message) &&
           referenceEq(e3, eq.level)
          eq
        else
          ASSERT(e1, e2, e3, eq.source)
        end
      end

      TERMINATE(__) => begin
        @assign e1 = func(eq.message)
        if referenceEq(e1, eq.message)
          eq
        else
          TERMINATE(e1, eq.source)
        end
      end

      REINIT(__) => begin
        @assign e1 = func(eq.cref)
        @assign e2 = func(eq.reinitExp)
        if referenceEq(e1, eq.cref) && referenceEq(e2, eq.reinitExp)
          eq
        else
          REINIT(e1, e2, eq.source)
        end
      end

      NORETCALL(__) => begin
        @assign e1 = func(eq.exp)
        if referenceEq(e1, eq.exp)
          eq
        else
          NORETCALL(e1, eq.source)
        end
      end

      _ => begin
        eq
      end
    end
  end
  return eq
end

function mapExpList(eql::List{<:Equation}, func::MapExpFn)::List{Equation}

  @assign eql = list(mapExp(eq, func) for eq in eql)
  return eql
end

function map(eq::Equation, func::MapFn)::Equation

  @assign () = begin
    @match eq begin
      EQUATION_FOR(__) => begin
        @assign eq.body = list(map(e, func) for e in eq.body)
        ()
      end
      EQUATION_IF(__) => begin
        @assign eq.branches = List(
          begin
            @match b begin
              EQUATION_BRANCH(__) => begin
                @assign b.body = list(map(e, func) for e in b.body)
                b
              end

              _ => begin
                b
              end
            end
          end for b in eq.branches
        )
        ()
      end
      EQUATION_WHEN(__) => begin
        @assign eq.branches = List(
          begin
            @match b begin
              BRANCH(__) => begin
                @assign b.body = list(map(e, func) for e in b.body)
                b
              end

              _ => begin
                b
              end
            end
          end for b in eq.branches
        )
        ()
      end

      _ => begin
        ()
      end
    end
  end
  @assign eq = func(eq)
  return eq
end

function apply(eq::Equation, func::ApplyFn)
  @assign () = begin
    @match eq begin
      FOR(__) => begin
        for e in eq.body
          apply(e, func)
        end
        ()
      end

      IF(__) => begin
        for b in eq.branches
          @assign () = begin
            @match b begin
              BRANCH(__) => begin
                for e in b.body
                  apply(e, func)
                end
                ()
              end

              _ => begin
                ()
              end
            end
          end
        end
        ()
      end

      WHEN(__) => begin
        for b in eq.branches
          @assign () = begin
            @match b begin
              BRANCH(__) => begin
                for e in b.body
                  apply(e, func)
                end
                ()
              end

              _ => begin
                ()
              end
            end
          end
        end
        ()
      end

      _ => begin
        ()
      end
    end
  end
  return func(eq)
end

function applyList(eql::List{<:Equation}, func::ApplyFn)
  return for eq in eql
    apply(eq, func)
  end
end

function info(eq::Equation)::SourceInfo
  local info::SourceInfo = ElementSource.getInfo(source(eq))
  return info
end

function source(eq::Equation)::DAE.ElementSource
  local source::DAE.ElementSource

  @assign source = begin
    @match eq begin
      EQUATION_EQUALITY(__) => begin
        eq.source
      end
      EQUATION_CREF_EQUALITY(__) => begin
        eq.source
      end
      EQUATION_ARRAY_EQUALITY(__) => begin
        eq.source
      end
      EQUATION_CONNECT(__) => begin
        eq.source
      end
      EQUATION_FOR(__) => begin
        eq.source
      end
      EQUATION_IF(__) => begin
        eq.source
      end
      EQUATION_WHEN(__) => begin
        eq.source
      end
      EQUATION_ASSERT(__) => begin
        eq.source
      end
      EQUATION_TERMINATE(__) => begin
        eq.source
      end
      EQUATION_REINIT(__) => begin
        eq.source
      end
      EQUATION_NORETCALL(__) => begin
        eq.source
      end
    end
  end
  return source
end

function makeIf(branches::List{<:Equation_Branch}, src::DAE.ElementSource)::Equation
  local eq::Equation
  @assign eq = IF(branches, src)
  return eq
end

function makeBranch(
  condition::Expression,
  body::List{<:Equation},
  condVar = Variability.CONTINUOUS,
)::Equation_Branch
  local branch::Equation_Branch
  @assign branch = EQUATION_BRANCH(condition, condVar, body)
  return branch
end

function makeEquality(
  lhs::Expression,
  rhs::Expression,
  ty::M_Type,
  src::DAE.ElementSource,
)::Equation
  local eq::Equation
  @assign eq = EQUALITY(lhs, rhs, ty, src)
  return eq
end


function triggerErrors(branch::Equation_Branch)
  return @assign () = begin
    @match branch begin
      INVALID_BRANCH(__) => begin
        Error.addTotalMessages(branch.errors)
        fail()
      end

      _ => begin
        ()
      end
    end
  end
end

function toFlatStream(
  branch::Equation_Branch,
  indent::String,
  s,
)
  @assign s = begin
    @match branch begin
      EQUATION_BRANCH(__) => begin
        @assign s =
          IOStream.append(s, toFlatString(branch.condition))
        @assign s = IOStream.append(s, " then\\n")
        @assign s = toFlatStreamList(branch.body, indent + "  ", s)
        s
      end

      INVALID_BRANCH(__) => begin
        toFlatStream(branch.branch, indent, s)
      end
    end
  end
  return s
end

function toStream(branch::Equation_Branch, indent::String, s)
  @assign s = begin
    @match branch begin
      EQUATION_BRANCH(__) => begin
        s = IOStream.append(s, toString(branch.condition))
        s = IOStream.append(s, " then\\n")
        s = toStreamList(branch.body, indent + "  ", s)
        s
      end
      INVALID_BRANCH(__) => begin
        toStream(branch.branch, indent, s)
      end
    end
  end
  return s
end
