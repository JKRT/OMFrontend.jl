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
resistorCircuit0 = (ConnectTests.ResistorCircuit0, "ElectricalComponentTest.ResistorCircuit0", "./Connectors/EletricalComponentTest.mo")
resistorCircuit1 = (ConnectTests.ResistorCircuit1, "ElectricalComponentTest.ResistorCircuit1", "./Connectors/EletricalComponentTest.mo")
simpleCircuit = (ConnectTests.SimpleCircuit, "ElectricalComponentTest.SimpleCircuit", "./Connectors/EletricalComponentTest.mo")

#= Add new tests here=#

@testset "Connector test. Testing the handling of connectors" begin
  tst = [ctst1, ctst5, tank, heattank, heatTankExpanded, multipleinheritanceconnect, resistorCircuit0, resistorCircuit1, simpleCircuit]
  for mf in tst
    try
      #= Get the flat model =#
      res = flattenFM(mf[2], mf[3])
      #= Convert the flat model to a string =#
      res = OMFrontend.toString(first(res))
      @test res == mf[1]
    catch e
      @test true == false
      @warn "An exception was thrown: $(e)"      
    end
  end
end
