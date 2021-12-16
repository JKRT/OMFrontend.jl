@Uniontype NFAlgorithm begin
  @Record ALGORITHM begin
    statements::List{Statement}
    source::DAE.ElementSource
  end
end

function toString(alg::Algorithm)::String
  local str::String
  @assign str = toStringList(alg.statements)
  return str
end

function foldExpList(algs::List{Algorithm}, func::FoldFunc, arg::ArgT) where {ArgT}
  for alg in algs
    @assign arg = foldExp(alg, func, arg)
  end
  return arg
end

function foldExp(alg::Algorithm, func::FoldFunc, arg::ArgT) where {ArgT}
  for s in alg.statements
    @assign arg = P_Statement.Statement.foldExp(s, func, arg)
  end
  return arg
end

function mapExpList(algs::List{<:Algorithm}, func::MapFunc)::List{Algorithm}
  @assign algs = list(mapExp(alg, func) for alg in algs)
  return algs
end

function mapExp(alg::Algorithm, func::MapFunc)::Algorithm
  @assign alg.statements = mapExpList(alg.statements, func)
  return alg
end

function apply(alg::Algorithm, func::ApplyFn)
  return for s in alg.statements
    Statement.apply(s, func)
  end
end

function applyList(algs::List{<:Algorithm}, func::ApplyFn)
  return for alg in algs
    for s in alg.statements
      Statement.apply(s, func)
    end
  end
end
