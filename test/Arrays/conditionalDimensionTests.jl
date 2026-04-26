#=
Tests for conditional array dimensions using if-expressions.
Covers the pattern used in Modelica.Fluid.Vessels.BaseClasses.PartialLumpedVessel
where portsData has dimension [if use_portsData then nPorts else 0].

These tests verify that the frontend correctly evaluates if-expressions
in array dimensions and propagates the resulting size through record field
access (e.g., portsData.diameter should have size [nPorts] when use_portsData=true).

Root cause of MSL failure (RoomCO2): The model sets use_portsData=false with nPorts=2.
This makes portsData dimension [0], but portsData_diameter_internal (dimension [nPorts]=[2])
has a binding to portsData.diameter. The binding is guarded by a conditional
"if use_portsData and nPorts > 0" which is false, so the component should be inactive.
But matchBinding checks dimension compatibility before evaluating the conditional guard.
See ConditionalDimension9 for the minimal reproducer.
=#

const COND_DIM_DIR = joinpath(@__DIR__)

@testset "Conditional array dimensions" begin

  @testset "ConditionalDimension3: if-expression as array dimension" begin
    (flatModel, _) = flattenFM("ConditionalDimension3",
                                joinpath(COND_DIM_DIR, "ConditionalDimension3.mo"))
    @test flatModel !== nothing
  end

  @testset "ConditionalDimension1: record array with conditional dimension + field binding" begin
    (flatModel, _) = flattenFM("ConditionalDimension1",
                                joinpath(COND_DIM_DIR, "ConditionalDimension1.mo"))
    @test flatModel !== nothing
  end

  @testset "ConditionalDimension2: conditional component with conditional record array" begin
    (flatModel, _) = flattenFM("ConditionalDimension2",
                                joinpath(COND_DIM_DIR, "ConditionalDimension2.mo"))
    @test flatModel !== nothing
  end

  @testset "ConditionalDimension4: inheritance from partial model" begin
    (flatModel, _) = flattenFM("ConditionalDimension4",
                                joinpath(COND_DIM_DIR, "ConditionalDimension4.mo"))
    @test flatModel !== nothing
  end

  @testset "ConditionalDimension5: component instantiation with nPorts modification" begin
    (flatModel, _) = flattenFM("ConditionalDimension5",
                                joinpath(COND_DIM_DIR, "ConditionalDimension5.mo"))
    @test flatModel !== nothing
  end

  @testset "ConditionalDimension6: deep inheritance + Evaluate annotation" begin
    (flatModel, _) = flattenFM("ConditionalDimension6",
                                joinpath(COND_DIM_DIR, "ConditionalDimension6.mo"))
    @test flatModel !== nothing
  end

  @testset "ConditionalDimension7: shared class tree with multiple instantiations" begin
    (flatModel, _) = flattenFM("ConditionalDimension7",
                                joinpath(COND_DIM_DIR, "ConditionalDimension7.mo"))
    @test flatModel !== nothing
  end

  @testset "ConditionalDimension8: separate library + model (MSL-like loading)" begin
    # Load library and model as separate SCode programs, then combine.
    # This mimics how flattenModelWithMSL works: the library SCode is pre-loaded
    # and the model SCode is appended. The shared class tree may cache the
    # conditional dimension with default nPorts=0 before the modification is applied.
    libFile = joinpath(COND_DIM_DIR, "ConditionalDimension8_Lib.mo")
    modelFile = joinpath(COND_DIM_DIR, "ConditionalDimension8.mo")
    libAbsyn = OMFrontend.parseFile(libFile)
    libSCode = OMFrontend.translateToSCode(libAbsyn)
    modelAbsyn = OMFrontend.parseFile(modelFile)
    modelSCode = OMFrontend.translateToSCode(modelAbsyn)
    combined = listAppend(modelSCode, libSCode)
    (flatModel, _) = OMFrontend.instantiateSCodeToFM("ConditionalDimension8", combined)
    @test flatModel !== nothing
  end

  @testset "ConditionalDimension9: use_portsData=false with nPorts>0 (RoomCO2 pattern)" begin
    # This is the actual bug reproducer. When use_portsData=false and nPorts=2:
    #   portsData dimension = [if false then 2 else 0] = [0]
    #   portsData_diameter_internal dimension = [nPorts] = [2]
    #   binding = portsData.diameter (guarded by "if use_portsData and nPorts > 0")
    # The conditional guard is false, so the component should be inactive,
    # but matchBinding checks dimensions before evaluating the guard.
    @test_broken begin
      (flatModel, _) = flattenFM("ConditionalDimension9",
                                  joinpath(COND_DIM_DIR, "ConditionalDimension9.mo"))
      flatModel !== nothing
    end
  end

end
