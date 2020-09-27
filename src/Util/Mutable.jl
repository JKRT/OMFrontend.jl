mutable struct Mutable{T}
  data::T #=Can be anything really..=#
  function access(m)
    return data
  end
  function update(x)
    data = x
  end
end
