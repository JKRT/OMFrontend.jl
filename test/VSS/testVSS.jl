using Revise
using Test

import Absyn
import OM

@testset "Testing VSS models" begin
  @test typeof(OM.parseFile("BreakingPendulum.mo")) == Absyn.PROGRAM
  @test typeof(OM.parseFile("SimpleSingleMode.mo")) == Absyn.PROGRAM

  res = OM.flattenFM("SimpleSingleMode", "SimpleSingleMode.mo")
  res2 = OM.string(first(res))
  println(res2)
  
  res = OM.flattenFM("BreakingPendulum", "BreakingPendulum.mo")
  res2 = OM.string(first(res))
  println(res2)
  
end
