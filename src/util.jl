"""
It is possible to enable execstat by passing the following command line parameters

```
ENABLE_EXECSTAT=true julia yourscript.jl
```
"""
const ENABLE_EXECSTAT::Bool = get(ENV, "ENABLE_EXECSTAT", "false") == "true"
macro EXECSTAT(msg, expr)
  if ENABLE_EXECSTAT
    return quote
      @time $(esc(msg)) __result = $(esc(expr))
      __result
    end
  else
    return esc(expr)
  end
end
