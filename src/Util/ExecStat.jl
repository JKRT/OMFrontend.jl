module ExecStat

using MetaModelica
using ExportAll

import ..System
import ..Flags

function execStatReset()
  System.realtimeTick(ClockIndexes.RT_CLOCK_EXECSTAT)
  System.realtimeTick(ClockIndexes.RT_CLOCK_EXECSTAT_CUMULATIVE)
  return setGlobalRoot(Global.gcProfilingIndex, GC.getProfStats())
end

global startTime::Float64 = time()
global memoryInBytes::Int = 0
global const MODELICA_GC_NUM = Base.gc_num()
"""
Prints an execution stat on the format:
  *** %name% -> time: %time%, memory %memory%
  Where you provide name, and time is the time since the last call using this
  index (the clock is reset after each call). The memory is the total memory
  consumed by the compiler at this point in time.
"""
function execStat(name::String)
  local prevAllocs::Int
  local prevTime::Int
  if Flags.isSet(Flags.EXEC_STAT)
    prevAllocs = Base.gc_alloc_count
    tTime = time() - startTime
    local diffMem = GC_Diff(gc_num(), MODELICA_GC_NUM)
    startTime = time()
    printf("Execstat %s -> time:%d, memory %d", name, tTime, diffMem)
  else
    return
  end
end

@exportAll()
end
