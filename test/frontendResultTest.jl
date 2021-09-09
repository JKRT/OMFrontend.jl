include("connect.jl")
import ..ConnectTests
#= How one test case is defined =#
ctst1 = (ConnectTests.Connect1, "Connect1", "./Connectors/Connect1.mo")
ctst5 = (ConnectTests.Connect5, "Connect5", "./Connectors/Connect5.mo")
heattank = (ConnectTests.HeatTank, "HeatTank", "./Connectors/HeatTank.mo")
heatTankExpanded = (ConnectTests.HeatTankExpanded, "HeatTankExpanded", "./Connectors/HeatTankExpanded.mo")

#= Add new tests here=#
tst = [ctst1, ctst5, heattank, heatTankExpanded]
for mf in tst
  try
    res = flattenFM(mf[2], mf[3])
    println("We have a result")
    #= Convert the result to a string =#
    res = OMFrontend.toString(first(res))
    println(res)
    @test res == mf[1]
  catch e
    @warn "An exception was thrown: $(e)"
    throw(e)
  end
end
