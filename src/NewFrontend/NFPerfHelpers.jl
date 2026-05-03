"""
  mapPreservingEq(arr::Vector{T}, f) -> Vector{T}

Map `f` over each element of `arr`. If `f` returns the same reference for
every element (`referenceEq`), return the original `arr` without allocating.
Otherwise lazily copy the array on the first changed element and write
mapped elements into the copy.
"""
@inline function mapPreservingEq(arr::Vector{T}, f::F) where {T, F}
  newArr = arr
  for i in eachindex(arr)
    orig = arr[i]
    mapped = f(orig)::T
    if !referenceEq(orig, mapped)
      if newArr === arr
        newArr = copy(arr)
      end
      newArr[i] = mapped
    end
  end
  return newArr
end

"""
  reuseIfRefEqual(unchanged, orig, new, makeNew)

If `orig` and `new` are the same reference, return `unchanged` (typically the
outer/parent value). Otherwise call `makeNew(new)` to construct a fresh
wrapper. Eliminates the wrapper allocation when the sub-expression was
unchanged.

No type constraints on the arguments: traversal functions over abstract
unions (e.g. `map(::Expression, ...)`) commonly return a different concrete
subtype than the input, and the wrapper produced by `makeNew` may also use a
different parameterisation than `unchanged`. The identity check
(`referenceEq`) is by pointer and does not require type equality. Callers
sit inside an `@match` arm whose enclosing block coerces the union return
to a concrete type.
"""
@inline function reuseIfRefEqual(unchanged, orig, new, makeNew::F) where {F}
  referenceEq(orig, new) ? unchanged : makeNew(new)
end
