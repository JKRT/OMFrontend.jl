include("connect.jl")
import ..ConnectTests
#= How one test case is defined =#
ctst1 = (ConnectTests.Connect1, "Connect1", "./Connectors/Connect1.mo")
ctst5 = (ConnectTests.Connect5, "Connect5", "./Connectors/Connect5.mo")
tank = (ConnectTests.Tank, "Tank", "./Connectors/Tank.mo")
heattank = (ConnectTests.HeatTank, "HeatTank", "./Connectors/HeatTank.mo")
heatTankExpanded = (ConnectTests.HeatTankExpanded, "HeatTankExpanded", "./Connectors/HeatTankExpanded.mo")
multipleinheritanceconnect = (ConnectTests.MultipleInheritanceConnect
                              , "MultipleInheritanceConnect"
                              , "./Connectors/MutipleInheritanceConnect.mo")
#= Add new tests here=#

@testset "Connector test. Testing the handling of connectors" begin
  tst = [ctst1, ctst5, tank, heattank, heatTankExpanded, multipleinheritanceconnect]
  for mf in tst
    try
      res = flattenFM(mf[2], mf[3])
      #= Convert the result to a string =#
      res = OMFrontend.toString(first(res))
      @test res == mf[1]
    catch e
      @warn "An exception was thrown: $(e)"
    end
  end
end
