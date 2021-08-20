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
  singleLine = begin
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
    multi_line = isMultiLine(eq)
    if first
      first = false
    elseif prev_multi_line || multi_line
      s = IOStream.append(s, "\\n")
    end
    prev_multi_line = multi_line
    s = toFlatStream(eq, indent, s)
    s = IOStream.append(s, ";\\n")
  end
  #=  Improve human parsability by separating statements that spans multiple
  =#
  #=  lines (like if-equations) with newlines.
  =#
  return s
end

function toFlatStream(eq::Equation, indent::String, s)
  s = IOStream.append(s, indent)
  s = begin
    @match eq begin
      EQUATION_EQUALITY(__) => begin
        s = IOStream.append(s, toFlatString(eq.lhs))
        #= Complex assign=#@assign s = IOStream.append(s, " = ")
        s = IOStream.append(s, toFlatString(eq.rhs))
        s
      end

      CREF_EQUALITY(__) => begin
        s = IOStream.append(s, toFlatString(eq.lhs))
        #= Complex assign=#@assign s = IOStream.append(s, " = ")
        s = IOStream.append(s, toFlatString(eq.rhs))
        s
      end

      ARRAY_EQUALITY(__) => begin
        s = IOStream.append(s, toFlatString(eq.lhs))
        #= Complex assign=#@assign s = IOStream.append(s, " = ")
        s = IOStream.append(s, toFlatString(eq.rhs))
        s
      end

      CONNECT(__) => begin
        s = IOStream.append(s, "connect(")
        s = IOStream.append(s, toFlatString(eq.lhs))
        #= Complex assign=#@assign s = IOStream.append(s, " = ")
        s = IOStream.append(s, toFlatString(eq.rhs))
        s = IOStream.append(s, ")")
        s
      end

      FOR(__) => begin
        s = IOStream.append(s, "for ")
        s = IOStream.append(s, name(eq.iterator))
        if isSome(eq.range)
          s = IOStream.append(s, " in ")
          s = IOStream.append(
            s,
            toFlatString(Util.getOption(eq.range)),
          )
        end
        s = IOStream.append(s, " loop\\n")
        s = toFlatStreamList(eq.body, indent + "  ", s)
        s = IOStream.append(s, indent)
        s = IOStream.append(s, "end for")
        s
      end

      IF(__) => begin
        s = IOStream.append(s, "if ")
        s = toFlatStream(listHead(eq.branches), indent, s)
        for b in listRest(eq.branches)
          s = IOStream.append(s, indent)
          s = IOStream.append(s, "elseif ")
          s = toFlatStream(b, indent, s)
        end
        s = IOStream.append(s, indent)
        s = IOStream.append(s, "end if")
        s
      end

      WHEN(__) => begin
        s = IOStream.append(s, "when ")
        s = toFlatStream(listHead(eq.branches), indent, s)
        for b in listRest(eq.branches)
          s = IOStream.append(s, indent)
          s = IOStream.append(s, "elsewhen ")
          s = toFlatStream(b, indent, s)
        end
        s = IOStream.append(s, indent)
        s = IOStream.append(s, "end when")
        s
      end

      ASSERT(__) => begin
        s = IOStream.append(s, "assert(")
        s = IOStream.append(s, toFlatString(eq.condition))
        s = IOStream.append(s, ", ")
        s = IOStream.append(s, toFlatString(eq.message))
        s = IOStream.append(s, ", ")
        s = IOStream.append(s, toFlatString(eq.level))
        s = IOStream.append(s, ")")
        s
      end

      TERMINATE(__) => begin
        s = IOStream.append(s, "terminate(")
        s = IOStream.append(s, toFlatString(eq.message))
        s = IOStream.append(s, ")")
        s
      end

      REINIT(__) => begin
        s = IOStream.append(s, "reinit(")
        s = IOStream.append(s, toFlatString(eq.cref))
        s = IOStream.append(s, ", ")
        s = IOStream.append(s, toFlatString(eq.reinitExp))
        s = IOStream.append(s, ")")
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
    multi_line = isMultiLine(eq)
    if first
      first = false
    elseif prev_multi_line || multi_line
      s = IOStream.append(s, "\\n")
    end
    prev_multi_line = multi_line
    s = toStream(eq, indent, s)
    s = IOStream.append(s, ";\\n")
  end
  #=  Improve human parsability by separating statements that spans multiple
  =#
  #=  lines (like if-equations) with newlines.
  =#
  return s
end

function toStream(eq::Equation, indent::String, s)

  s = IOStream.append(s, indent)
  s = begin
    @match eq begin
      EQUATION_EQUALITY(__) => begin
        s = IOStream.append(s, toString(eq.lhs))
        #= Complex assign=#@assign s = IOStream.append(s, " = ")
        s = IOStream.append(s, toString(eq.rhs))
        s
      end

      CREF_EQUALITY(__) => begin
        s = IOStream.append(s, toString(eq.lhs))
        #= Complex assign=#@assign s = IOStream.append(s, " = ")
        s = IOStream.append(s, toString(eq.rhs))
        s
      end

      ARRAY_EQUALITY(__) => begin
        s = IOStream.append(s, toString(eq.lhs))
        #= Complex assign=#@assign s = IOStream.append(s, " = ")
        s = IOStream.append(s, toString(eq.rhs))
        s
      end

      CONNECT(__) => begin
        s = IOStream.append(s, "connect(")
        s = IOStream.append(s, toString(eq.lhs))
        #= Complex assign=#@assign s = IOStream.append(s, " = ")
        s = IOStream.append(s, toString(eq.rhs))
        s = IOStream.append(s, ")")
        s
      end

      FOR(__) => begin
        s = IOStream.append(s, "for ")
        s = IOStream.append(s, name(eq.iterator))
        if isSome(eq.range)
          s = IOStream.append(s, " in ")
          s = IOStream.append(
            s,
            toString(Util.getOption(eq.range)),
          )
        end
        s = IOStream.append(s, " loop\\n")
        s = toStreamList(eq.body, indent + "  ", s)
        s = IOStream.append(s, indent)
        s = IOStream.append(s, "end for")
        s
      end

      IF(__) => begin
        s = IOStream.append(s, "if ")
        s = toStream(listHead(eq.branches), indent, s)
        for b in listRest(eq.branches)
          s = IOStream.append(s, indent)
          s = IOStream.append(s, "elseif ")
          s = toStream(b, indent, s)
        end
        s = IOStream.append(s, indent)
        s = IOStream.append(s, "end if")
        s
      end

      WHEN(__) => begin
        s = IOStream.append(s, "when ")
        s = toStream(listHead(eq.branches), indent, s)
        for b in listRest(eq.branches)
          s = IOStream.append(s, indent)
          s = IOStream.append(s, "elsewhen ")
          s = toStream(b, indent, s)
        end
        s = IOStream.append(s, indent)
        s = IOStream.append(s, "end when")
        s
      end

      ASSERT(__) => begin
        s = IOStream.append(s, "assert(")
        s = IOStream.append(s, toString(eq.condition))
        s = IOStream.append(s, ", ")
        s = IOStream.append(s, toString(eq.message))
        s = IOStream.append(s, ", ")
        s = IOStream.append(s, toString(eq.level))
        s = IOStream.append(s, ")")
        s
      end

      TERMINATE(__) => begin
        s = IOStream.append(s, "terminate(")
        s = IOStream.append(s, toString(eq.message))
        s = IOStream.append(s, ")")
        s
      end

      REINIT(__) => begin
        s = IOStream.append(s, "reinit(")
        s = IOStream.append(s, toString(eq.cref))
        s = IOStream.append(s, ", ")
        s = IOStream.append(s, toString(eq.reinitExp))
        s = IOStream.append(s, ")")
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

  s = IOStream.create(getInstanceName(), IOStream.IOStreamType.LIST())
  s = toStreamList(eql, indent, s)
  str = IOStream.string(s)
  IOStream.delete(s)
  return str
end

function toString(eq::Equation, indent::String = "")::String
  local str::String

  local s

  s = IOStream.create(getInstanceName(), IOStream.IOStreamType.LIST())
  s = toStream(eq, indent, s)
  str = IOStream.string(s)
  IOStream.delete(s)
  return str
end

function isConnect(eq::Equation)::Bool
  local isConnect::Bool

  isConnect = begin
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
      res = true
      return res
    end
  end
  res = false
  return res
end

function contains(eq::Equation, func::PredFn)::Bool
  local res::Bool
  if func(eq)
    res = true
    return res
  end
  res = begin
    @match eq begin
      EQUATION_FOR(__) => begin
        containsList(eq.body, func)
      end
      EQUATION_IF(__) => begin
        for b in eq.branches
          () = begin
            @match b begin
              EQUATION_BRANCH(__) => begin
                if containsList(b.body, func)
                  res = true
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
          () = begin
            @match b begin
              EQUATION_BRANCH(__) => begin
                if containsList(b.body, func)
                  res = true
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

  () = begin
    @match eq begin
      EQUATION_EQUALITY(__) => begin
        arg = func(eq.lhs, arg)
        arg = func(eq.rhs, arg)
        ()
      end

      EQUATION_ARRAY_EQUALITY(__) => begin
        arg = func(eq.lhs, arg)
        arg = func(eq.rhs, arg)
        ()
      end

      EQUATION_CONNECT(__) => begin
        arg = func(eq.lhs, arg)
        arg = func(eq.rhs, arg)
        ()
      end

      EQUATION_FOR(__) => begin
        arg = foldExpList(eq.body, func, arg)
        if isSome(eq.range)
          arg = func(Util.getOption(eq.range), arg)
        end
        ()
      end

      EQUATION_IF(__) => begin
        for b in eq.branches
          () = begin
            @match b begin
              EQUATION_BRANCH(__) => begin
                arg = func(b.condition, arg)
                arg = foldExpList(b.body, func, arg)
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
          () = begin
            @match b begin
              EQUATION_BRANCH(__) => begin
                arg = func(b.condition, arg)
                arg = foldExpList(b.body, func, arg)
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
        arg = func(eq.condition, arg)
        arg = func(eq.message, arg)
        arg = func(eq.level, arg)
        ()
      end

      EQUATION_TERMINATE(__) => begin
        arg = func(eq.message, arg)
        ()
      end

      EQUATION_REINIT(__) => begin
        arg = func(eq.cref, arg)
        arg = func(eq.reinitExp, arg)
        ()
      end

      EQUATION_NORETCALL(__) => begin
        arg = func(eq.exp, arg)
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
    arg = foldExp(e, func, arg)
  end
  return arg
end

function foldExpList(eq::Nil{Any}, func::FoldFunc, arg::ArgT) where {ArgT}
  return arg
end

function mapExpBranch(branch::Equation_Branch, func::MapExpFn)::Equation_Branch

  local cond::Expression
  local eql::List{Equation}

  branch = begin
    @match branch begin
      EQUATION_BRANCH(__) => begin
        cond = func(branch.condition)
        eql = list(mapExp(e, func) for e in branch.body)
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
  eq = begin
    local e1::Expression
    local e2::Expression
    local e3::Expression
    @match eq begin
      EQUATION_EQUALITY(__) => begin
        e1 = func(eq.lhs)
        e2 = func(eq.rhs)
        if referenceEq(e1, eq.lhs) && referenceEq(e2, eq.rhs)
          eq
        else
          EQUATION_EQUALITY(e1, e2, eq.ty, eq.source)
        end
      end

      EQUATION_ARRAY_EQUALITY(__) => begin
        e1 = func(eq.lhs)
        e2 = func(eq.rhs)
        if referenceEq(e1, eq.lhs) && referenceEq(e2, eq.rhs)
          eq
        else
          EQUATION_ARRAY_EQUALITY(e1, e2, eq.ty, eq.source)
        end
      end

       EQUATION_CONNECT(__) => begin
        e1 = func(eq.lhs)
        e2 = func(eq.rhs)
        if referenceEq(e1, eq.lhs) && referenceEq(e2, eq.rhs)
          eq
        else
           EQUATION_CONNECT(e1, e2, eq.source)
        end
      end

       EQUATION_FOR(__) => begin
        #= complex assign=#@assign eq.body = list(mapExp(e, func) for e in eq.body)
        #= complex assign=#@assign eq.range = Util.applyOption(eq.range, func)
        eq
      end

       EQUATION_IF(__) => begin
        #= complex assign=#@assign eq.branches = list(mapExpBranch(b, func) for b in eq.branches)
        eq
      end

       EQUATION_WHEN(__) => begin
        #= complex assign=#@assign eq.branches = list(mapExpBranch(b, func) for b in eq.branches)
        eq
      end

       EQUATION_ASSERT(__) => begin
        e1 = func(eq.condition)
        e2 = func(eq.message)
        e3 = func(eq.level)
        if referenceEq(e1, eq.condition) &&
           referenceEq(e2, eq.message) &&
           referenceEq(e3, eq.level)
          eq
        else
          EQUATION_ASSERT(e1, e2, e3, eq.source)
        end
      end

       EQUATION_TERMINATE(__) => begin
        e1 = func(eq.message)
        if referenceEq(e1, eq.message)
          eq
        else
          EQUATION_TERMINATE(e1, eq.source)
        end
      end

       EQUATION_REINIT(__) => begin
        e1 = func(eq.cref)
        e2 = func(eq.reinitExp)
        if referenceEq(e1, eq.cref) && referenceEq(e2, eq.reinitExp)
          eq
        else
           EQUATION_REINIT(e1, e2, eq.source)
        end
      end

       EQUATION_NORETCALL(__) => begin
        e1 = func(eq.exp)
        if referenceEq(e1, eq.exp)
          eq
        else
          EQUATION_NORETCALL(e1, eq.source)
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

  eql = list(mapExp(eq, func) for eq in eql)
  return eql
end

function map(eq::Equation, func::MapFn)::Equation

  () = begin
    @match eq begin
      EQUATION_FOR(__) => begin
        #= complex assign=#@assign eq.body = list(map(e, func) for e in eq.body)
        ()
      end
      EQUATION_IF(__) => begin
        #= complex assign=#@assign eq.branches = List(
          begin
            @match b begin
              EQUATION_BRANCH(__) => begin
                #= complex assign=#@assign b.body = list(map(e, func) for e in b.body)
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
        #= complex assign=#@assign eq.branches = List(
          begin
            @match b begin
              BRANCH(__) => begin
                #= complex assign=#@assign b.body = list(map(e, func) for e in b.body)
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
  eq = func(eq)
  return eq
end

function apply(eq::Equation, func::ApplyFn)
  () = begin
    @match eq begin
      FOR(__) => begin
        for e in eq.body
          apply(e, func)
        end
        ()
      end

      IF(__) => begin
        for b in eq.branches
          () = begin
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
          () = begin
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

function Equation_info(eq::Equation)::SourceInfo
  local info::SourceInfo = sourceInfo() #DAE.ElementSource_getInfo(source(eq))
  return info
end

function source(eq::Equation)::DAE.ElementSource
  local sourceVar::DAE.ElementSource

  sourceVar = begin
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
  return sourceVar
end

function makeIf(branches::List{<:Equation_Branch}, src::DAE.ElementSource)::Equation
  local eq::Equation
  eq = EQUATION_IF(branches, src)
  return eq
end

function makeBranch(
  condition::Expression,
  body::List{<:Equation},
  condVar = Variability.CONTINUOUS,
)::Equation_Branch
  local branch::Equation_Branch
  branch = EQUATION_BRANCH(condition, condVar, body)
  return branch
end

function makeEquality(
  lhs::Expression,
  rhs::Expression,
  ty::M_Type,
  src::DAE.ElementSource,
)::Equation
  local eq::Equation
  eq = EQUATION_EQUALITY(lhs, rhs, ty, src)
  return eq
end


function triggerErrors(branch::Equation_Branch)
  return () = begin
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
  s = begin
    @match branch begin
      EQUATION_BRANCH(__) => begin
        s =
          IOStream.append(s, toFlatString(branch.condition))
        s = IOStream.append(s, " then\\n")
        s = toFlatStreamList(branch.body, indent + "  ", s)
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
  s = begin
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
