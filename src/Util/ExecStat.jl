module ExecStat

using MetaModelica
using ExportAll

import Main.ClockIndexes
import Main.Error
import Main.GC
import Main.Global
import Main.Flags
import Main.System
import Main.StringUtil

function execStatReset()
  System.realtimeTick(ClockIndexes.RT_CLOCK_EXECSTAT)
  System.realtimeTick(ClockIndexes.RT_CLOCK_EXECSTAT_CUMULATIVE)
  return setGlobalRoot(Global.gcProfilingIndex, GC.getProfStats())
end

""" #= Prints an execution stat on the format:
  *** %name% -> time: %time%, memory %memory%
  Where you provide name, and time is the time since the last call using this
  index (the clock is reset after each call). The memory is the total memory
  consumed by the compiler at this point in time.
   =#"""
function execStat(name::String)
  local t::AbstractFloat
  local total::AbstractFloat
  local timeStr::String
  local totalTimeStr::String
  local gcStr::String
  local memory::Integer
  local oldMemory::Integer
  local heapsize_full::Integer
  local free_bytes_full::Integer
  local since::Integer
  local before::Integer
  local stats::GC.ProfStats
  local oldStats::GC.ProfStats

  return if Flags.isSet(Flags.EXEC_STAT)
    for i in if Flags.isSet(Flags.EXEC_STAT_EXTRA_GC)
      list(1, 2)
    else
      list(1)
    end
      if i == 2
        GC.gcollect()
      end
      @match (@match GC.PROFSTATS(
        bytes_allocd_since_gc = since,
        allocd_bytes_before_gc = before,
        heapsize_full = heapsize_full,
        free_bytes_full = free_bytes_full,
      ) = stats) = GC.getProfStats()
      @assign memory = since + before
      @assign oldStats = getGlobalRoot(Global.gcProfilingIndex)
      @match GC.PROFSTATS(bytes_allocd_since_gc = since, allocd_bytes_before_gc = before) =
        oldStats
      @assign oldMemory = since + before
      @assign t = System.realtimeTock(ClockIndexes.RT_CLOCK_EXECSTAT)
      @assign total = System.realtimeTock(ClockIndexes.RT_CLOCK_EXECSTAT_CUMULATIVE)
      @assign timeStr = System.snprintff("%.4g", 20, t)
      @assign totalTimeStr = System.snprintff("%.4g", 20, total)
      if Flags.isSet(Flags.GC_PROF)
        @assign gcStr = GC.profStatsStr(stats, head = "", delimiter = " / ")
        Error.addMessage(
          Error.EXEC_STAT_GC,
          list(name + (
            if i == 2
              " GC"
            else
              ""
            end
          ), timeStr, totalTimeStr, gcStr),
        )
      else
        Error.addMessage(
          Error.EXEC_STAT,
          list(
            name + (
              if i == 2
                " GC"
              else
                ""
              end
            ),
            timeStr,
            totalTimeStr,
            StringUtil.bytesToReadableUnit(
              memory - oldMemory,
              maxSizeInUnit = 500,
              significantDigits = 4,
            ),
            StringUtil.bytesToReadableUnit(
              memory,
              maxSizeInUnit = 500,
              significantDigits = 4,
            ),
            StringUtil.bytesToReadableUnit(
              free_bytes_full,
              maxSizeInUnit = 500,
              significantDigits = 4,
            ),
            StringUtil.bytesToReadableUnit(
              heapsize_full,
              maxSizeInUnit = 500,
              significantDigits = 4,
            ),
          ),
        )
      end
      System.realtimeTick(ClockIndexes.RT_CLOCK_EXECSTAT)
      setGlobalRoot(Global.gcProfilingIndex, stats)
    end
  end
end

@exportAll()
end
