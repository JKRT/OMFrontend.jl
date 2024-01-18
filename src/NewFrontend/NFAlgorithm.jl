abstract type NFAlgorithm end

mutable struct ALGORITHM <: NFAlgorithm
    statements::Vector{Statement}
    source::DAE.ElementSource
end

function toString(alg::ALGORITHM)::String
  local str::String
  @assign str = toStringList(alg.statements)
  return str
end

function foldExpList(algs::Vector{Algorithm}, func::FoldFunc, arg::ArgT) where {ArgT}
  for alg in algs
    arg = foldExp(alg, func, arg)
  end
  return arg
end

function foldExp(alg::ALGORITHM, func::FoldFunc, arg::ArgT) where {ArgT}
  for s in alg.statements
    arg = foldExp(s, func, arg)
  end
  return arg
end

"""
Declare with! to keep with the idioms of Julia.
"""
function mapExpList(algs::Vector{Algorithm}, func::MapFunc)
  local f = @closure (alg) -> mapExp(alg, func)
  local mAlgs::Vector{Algorithm} = map!(f, algs, algs)
  return mAlgs
end

function mapExp(alg::ALGORITHM, func::MapFunc)::Algorithm
  @assign alg.statements = mapExpList(alg.statements, func)
  return alg
end

function apply(alg::ALGORITHM, func::ApplyFn)
  for s in alg.statements
    Statement.apply(s, func)
  end
end

function applyList(algs::Vector{Algorithm}, func::ApplyFn)
  return for alg in algs
    for s in alg.statements
      Statement.apply(s, func)
    end
  end
end
