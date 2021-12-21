using Revise
using Test

import Absyn
import OM

@testset "Testing VSS models" begin
  @test typeof(OM.parseFile("BreakingPendulum.mo")) == Absyn.PROGRAM
  OM.flattenFM("BreakingPendulum", "BreakingPendulum.mo")
end
