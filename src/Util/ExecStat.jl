module ExecStat

using MetaModelica
using ExportAll

import ..System
import ..Flags

global startTime::Float64 = time()
global memoryInBytes::Int = 0
global const MODELICA_GC_NUM = Base.gc_num()

function execStatReset()
  System.realtimeTick(ClockIndexes.RT_CLOCK_EXECSTAT)
  System.realtimeTick(ClockIndexes.RT_CLOCK_EXECSTAT_CUMULATIVE)
  return setGlobalRoot(Global.gcProfilingIndex, GC.getProfStats())
end

"""
Reset only the wall-time baseline used by `execStat` so the next `execStat`
call reports time relative to now, not relative to module load. Cheap, no
allocation.
"""
function execStatResetTimer()
  global startTime
  startTime = time()
  return nothing
end

"""
Prints an execution stat on the format:
  Execstat <name> -> time: <seconds>s, alloc'd: <bytes> bytes (cumulative)
where `time` is the wall time since the last `execStat` call (the timer is
reset per call so successive entries give per-phase wall time), and `alloc'd`
is the cumulative allocated bytes since module load (a running total — the
GC baseline `MODELICA_GC_NUM` is captured at module load and is `const`, so
we cannot reset it per-call without further changes).

No-op when `Flags.EXEC_STAT` is not set.
"""
function execStat(name::String)
  if !Flags.isSet(Flags.EXEC_STAT)
    return
  end
  global startTime
  local now = time()
  local elapsed = now - startTime
  startTime = now
  local diff = Base.GC_Diff(Base.gc_num(), MODELICA_GC_NUM)
  println("Execstat ", name,
          " -> time: ", round(elapsed; digits = 4), "s",
          ", alloc'd: ", Int(diff.allocd), " bytes (cumulative)")
  return
end

@exportAll()
end
