// Decomposition of BranchingDynamicPipes into isolated sub-patterns.
// Each test model isolates one structural feature to identify which
// causes the stack overflow.

// ==========================================================================
// Test A: Single DynamicPipe with MoistAir, no heat transfer.
// If this overflows, the problem is in DynamicPipe + MoistAir instantiation.
// ==========================================================================

model FluidPatternA_PipeOnly
  extends Modelica.Icons.Example;
  replaceable package Medium = Modelica.Media.Air.MoistAir
    constrainedby Modelica.Media.Interfaces.PartialMedium;
  inner Modelica.Fluid.System system;
  Modelica.Fluid.Pipes.DynamicPipe pipe1(
    redeclare package Medium = Medium,
    nNodes = 5,
    diameter = 2.54e-2,
    length = 50,
    modelStructure = Modelica.Fluid.Types.ModelStructure.a_v_b);
end FluidPatternA_PipeOnly;


// ==========================================================================
// Test B: DynamicPipe with LocalPipeFlowHeatTransfer (the redeclare model).
// If A passes but B overflows, the problem is LocalPipeFlowHeatTransfer.
// ==========================================================================

model FluidPatternB_PipeWithHT
  extends Modelica.Icons.Example;
  replaceable package Medium = Modelica.Media.Air.MoistAir
    constrainedby Modelica.Media.Interfaces.PartialMedium;
  inner Modelica.Fluid.System system;
  Modelica.Fluid.Pipes.DynamicPipe pipe2(
    redeclare package Medium = Medium,
    nNodes = 5,
    redeclare model HeatTransfer =
      Modelica.Fluid.Pipes.BaseClasses.HeatTransfer.LocalPipeFlowHeatTransfer,
    use_HeatTransfer = true,
    diameter = 2.54e-2,
    length = 50,
    modelStructure = Modelica.Fluid.Types.ModelStructure.av_vb);
end FluidPatternB_PipeWithHT;


// ==========================================================================
// Test C: FixedHeatFlow array sized by pipe parameter (cross-component ref).
// If A and B pass but C overflows, the problem is the cross-component sizing.
// ==========================================================================

model FluidPatternC_CrossRef
  extends Modelica.Icons.Example;
  replaceable package Medium = Modelica.Media.Air.MoistAir
    constrainedby Modelica.Media.Interfaces.PartialMedium;
  inner Modelica.Fluid.System system;
  Modelica.Fluid.Pipes.DynamicPipe pipe2(
    redeclare package Medium = Medium,
    nNodes = 5,
    redeclare model HeatTransfer =
      Modelica.Fluid.Pipes.BaseClasses.HeatTransfer.LocalPipeFlowHeatTransfer,
    use_HeatTransfer = true,
    diameter = 2.54e-2,
    length = 50,
    modelStructure = Modelica.Fluid.Types.ModelStructure.av_vb);
  Modelica.Thermal.HeatTransfer.Sources.FixedHeatFlow[pipe2.nNodes] heat2(
    Q_flow = 200 * pipe2.dxs,
    alpha = -1e-2 * ones(pipe2.n));
end FluidPatternC_CrossRef;


// ==========================================================================
// Test D: Multiple pipes (the branching), no heat transfer.
// Tests whether 4 pipes cause excessive depth.
// ==========================================================================

model FluidPatternD_MultiplePipes
  extends Modelica.Icons.Example;
  replaceable package Medium = Modelica.Media.Air.MoistAir
    constrainedby Modelica.Media.Interfaces.PartialMedium;
  inner Modelica.Fluid.System system;
  Modelica.Fluid.Pipes.DynamicPipe pipe1(
    redeclare package Medium = Medium, nNodes = 5,
    diameter = 2.54e-2, length = 50,
    modelStructure = Modelica.Fluid.Types.ModelStructure.a_v_b);
  Modelica.Fluid.Pipes.DynamicPipe pipe2(
    redeclare package Medium = Medium, nNodes = 5,
    diameter = 2.54e-2, length = 50,
    modelStructure = Modelica.Fluid.Types.ModelStructure.av_vb);
  Modelica.Fluid.Pipes.DynamicPipe pipe3(
    redeclare package Medium = Medium, nNodes = 5,
    diameter = 2.54e-2, length = 25,
    modelStructure = Modelica.Fluid.Types.ModelStructure.a_v_b);
  Modelica.Fluid.Pipes.DynamicPipe pipe4(
    redeclare package Medium = Medium, nNodes = 5,
    diameter = 2.54e-2, length = 50,
    modelStructure = Modelica.Fluid.Types.ModelStructure.a_v_b);
end FluidPatternD_MultiplePipes;


// ==========================================================================
// Test E: Single pipe with StandardWater instead of MoistAir.
// If A overflows but E passes, the problem is MoistAir specifically.
// ==========================================================================

model FluidPatternE_WaterPipe
  extends Modelica.Icons.Example;
  replaceable package Medium = Modelica.Media.Water.StandardWater
    constrainedby Modelica.Media.Interfaces.PartialMedium;
  inner Modelica.Fluid.System system;
  Modelica.Fluid.Pipes.DynamicPipe pipe1(
    redeclare package Medium = Medium,
    nNodes = 5,
    diameter = 2.54e-2,
    length = 50,
    modelStructure = Modelica.Fluid.Types.ModelStructure.a_v_b);
end FluidPatternE_WaterPipe;


// ==========================================================================
// Test F: Pipe with IdealFlowHeatTransfer (simpler HT, known to work in
// HeatingSystem example). If B overflows but F passes, the problem is
// specific to LocalPipeFlowHeatTransfer.
// ==========================================================================

model FluidPatternF_IdealHT
  extends Modelica.Icons.Example;
  replaceable package Medium = Modelica.Media.Air.MoistAir
    constrainedby Modelica.Media.Interfaces.PartialMedium;
  inner Modelica.Fluid.System system;
  Modelica.Fluid.Pipes.DynamicPipe pipe2(
    redeclare package Medium = Medium,
    nNodes = 5,
    redeclare model HeatTransfer =
      Modelica.Fluid.Pipes.BaseClasses.HeatTransfer.IdealFlowHeatTransfer,
    use_HeatTransfer = true,
    diameter = 2.54e-2,
    length = 50,
    modelStructure = Modelica.Fluid.Types.ModelStructure.av_vb);
end FluidPatternF_IdealHT;
