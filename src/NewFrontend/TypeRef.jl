#= TODO: Merge this file with the others.. =#


function typeMatrixCommaRef(
  elements::Vector{Expression},
  origin::ORIGIN_Type,
  info::SourceInfo,
  arrayTypeRef::Ref{NFType},
  variabilityTypeRef::Ref{VariabilityType})
  local variability::VariabilityType = Variability.CONSTANT
  local arrayType::NFType
  local arrayExp::Expression
  local exp::Expression
  local expl::Vector{Expression} = Vector{Expression}(undef, length(elements))
  local res::Vector{Expression} = Vector{Expression}(undef, length(elements))
  local var::VariabilityType
  local ty::NFType = TYPE_UNKNOWN()
  local ty1::NFType
  local ty2::NFType
  local ty3::NFType
  local tys::List{NFType} = nil
  local tys2::Vector{NFType} = Vector{NFType}(undef, length(elements))
  local n::Int = 2
  local pos::Int
  local mk::MatchKindType
  @assert !isempty(elements)
  if length(elements) > 1
    local varTyRef = Ref{VariabilityType}(Variability.CONSTANT)
    local refMatchKind = Ref{MatchKindType}(MatchKind.NOT_COMPATIBLE)
    local tyRef = Ref{NFType}(TYPE_UNKNOWN())
    for (i,e) in enumerate(elements)
      exp = typeExp2(e, origin, info, tyRef, varTyRef)
      ty1 = tyRef.x
      var = varTyRef.x
      expl[i] = exp
      if isEqual(ty, TYPE_UNKNOWN())
         ty = ty1
      else
         (_, _, ty2, mk) = matchExpressions(
           INTEGER_EXPRESSION_0,
           arrayElementType(ty1),
           INTEGER_EXPRESSION_0,
           arrayElementType(ty),
         )
        if isCompatibleMatch(mk)
           ty = ty2
        end
      end
       tys = Cons{NFType}(ty1, tys)
       variability = variabilityMax(variability, var)
       n = max(n, dimensionCount(ty))
    end
    pos = n + 1
    local i = 1
    for e in expl
      @match Cons{NFType}(ty1, tys) = tys
       pos = pos - 1
      if dimensionCount(ty1) != n
         (e, ty1) = promote(e, ty1, n)
      end
       ty2 = setArrayElementType(ty1, ty)
      e = matchTypesRef(ty1, ty2, e, tyRef, refMatchKind)
      ty3 = tyRef.x
      mk = refMatchKind.x
      if isIncompatibleMatch(mk)
        Error.addSourceMessageAndFail(
          Error.ARG_TYPE_MISMATCH,
          list(
            String(pos),
            "matrix constructor ",
            "arg",
            toString(e),
            toString(ty1),
            toString(ty2),
          ),
          info,
        )
      end
      res[i] = e
      tys2[i] = ty3
      i += 1
    end
    (arrayExp, arrayType) = makeCatExp(2, res, tys2, variability, info)
  else
    local varTyRef = Ref{VariabilityType}(Variability.CONSTANT)
    local tyRef = Ref{NFType}(TYPE_UNKNOWN())
    arrayExp = typeExp2(elements[1], origin, info, tyRef, varTyRef)
    arrayType = tyRef.x
    variability = varTyRef.x
  end
  variabilityTypeRef.x = variability
  arrayTypeRef.x = arrayType
  return arrayExp
end
