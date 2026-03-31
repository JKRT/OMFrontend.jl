// Extended decomposition of BranchingDynamicPipes.
// Builds incrementally on FluidPatternD (4 pipes, no connects)
// to find which structural feature triggers the stack overflow.

// ==========================================================================
// Test G: 4 pipes + 2 Boundary_pT sources (no connect equations).
// If D passes but G overflows, the problem is Boundary_pT instantiation.
// ==========================================================================

model FluidPatternG_PipesWithBoundaries
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
  Modelica.Fluid.Sources.Boundary_pT boundary1(nPorts=1,
    redeclare package Medium = Medium, p=150000);
  Modelica.Fluid.Sources.Boundary_pT boundary4(nPorts=1,
    redeclare package Medium = Medium, p=100000);
end FluidPatternG_PipesWithBoundaries;


// ==========================================================================
// Test H: 4 pipes + 2 boundaries + connect equations (stream connectors).
// If G passes but H overflows, the problem is connect/stream handling.
// ==========================================================================

model FluidPatternH_PipesConnected
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
  Modelica.Fluid.Sources.Boundary_pT boundary1(nPorts=1,
    redeclare package Medium = Medium, p=150000);
  Modelica.Fluid.Sources.Boundary_pT boundary4(nPorts=1,
    redeclare package Medium = Medium, p=100000);
equation
  connect(boundary1.ports[1], pipe1.port_a);
  connect(pipe1.port_b, pipe2.port_a);
  connect(pipe1.port_b, pipe3.port_a);
  connect(pipe2.port_b, pipe4.port_a);
  connect(pipe3.port_b, pipe4.port_a);
  connect(pipe4.port_b, boundary4.ports[1]);
end FluidPatternH_PipesConnected;


// ==========================================================================
// Test I: Full BranchingDynamicPipes reconstruction.
// H + HeatTransfer redeclare on pipe2 + FixedHeatFlow cross-component ref.
// If H passes but I overflows, the problem is HT redeclare + cross-ref.
// ==========================================================================

model FluidPatternI_FullBranching
  extends Modelica.Icons.Example;
  replaceable package Medium = Modelica.Media.Air.MoistAir
    constrainedby Modelica.Media.Interfaces.PartialMedium;
  inner Modelica.Fluid.System system;
  Modelica.Fluid.Pipes.DynamicPipe pipe1(
    redeclare package Medium = Medium, nNodes = 5,
    diameter = 2.54e-2, length = 50, height_ab = 50,
    m_flow_start = 0.02,
    p_a_start = 150000, p_b_start = 130000,
    modelStructure = Modelica.Fluid.Types.ModelStructure.a_v_b);
  Modelica.Fluid.Pipes.DynamicPipe pipe2(
    redeclare package Medium = Medium, nNodes = 5,
    redeclare model HeatTransfer =
      Modelica.Fluid.Pipes.BaseClasses.HeatTransfer.LocalPipeFlowHeatTransfer,
    use_HeatTransfer = true,
    diameter = 2.54e-2, length = 50, height_ab = 25,
    m_flow_start = 0.01,
    p_a_start = 130000, p_b_start = 120000,
    modelStructure = Modelica.Fluid.Types.ModelStructure.av_vb);
  Modelica.Fluid.Pipes.DynamicPipe pipe3(
    redeclare package Medium = Medium, nNodes = 5,
    diameter = 2.54e-2, length = 25, height_ab = 25,
    m_flow_start = 0.01,
    p_a_start = 130000, p_b_start = 120000,
    modelStructure = Modelica.Fluid.Types.ModelStructure.a_v_b);
  Modelica.Fluid.Pipes.DynamicPipe pipe4(
    redeclare package Medium = Medium, nNodes = 5,
    diameter = 2.54e-2, length = 50, height_ab = 50,
    m_flow_start = 0.02,
    p_a_start = 120000, p_b_start = 100000,
    modelStructure = Modelica.Fluid.Types.ModelStructure.a_v_b);
  Modelica.Fluid.Sources.Boundary_pT boundary1(nPorts=1,
    redeclare package Medium = Medium, p=150000);
  Modelica.Fluid.Sources.Boundary_pT boundary4(nPorts=1,
    redeclare package Medium = Medium, use_p_in=true, p=100000);
  Modelica.Blocks.Sources.Ramp ramp1(
    offset=1e5, startTime=2, height=1e5, duration=0);
  Modelica.Thermal.HeatTransfer.Sources.FixedHeatFlow[pipe2.nNodes] heat2(
    Q_flow = 200 * pipe2.dxs, alpha = -1e-2 * ones(pipe2.n));
equation
  connect(ramp1.y, boundary4.p_in);
  connect(boundary1.ports[1], pipe1.port_a);
  connect(pipe1.port_b, pipe2.port_a);
  connect(pipe1.port_b, pipe3.port_a);
  connect(pipe2.port_b, pipe4.port_a);
  connect(pipe3.port_b, pipe4.port_a);
  connect(pipe4.port_b, boundary4.ports[1]);
  connect(heat2.port, pipe2.heatPorts);
end FluidPatternI_FullBranching;


// ==========================================================================
// Test Ia: H + HeatTransfer redeclare on pipe2 only (no FixedHeatFlow).
// If Ia overflows, the HT redeclare alone is enough to trigger it.
// ==========================================================================

model FluidPatternIa_HTRedeclareOnly
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
    redeclare model HeatTransfer =
      Modelica.Fluid.Pipes.BaseClasses.HeatTransfer.LocalPipeFlowHeatTransfer,
    use_HeatTransfer = true,
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
  Modelica.Fluid.Sources.Boundary_pT boundary1(nPorts=1,
    redeclare package Medium = Medium, p=150000);
  Modelica.Fluid.Sources.Boundary_pT boundary4(nPorts=1,
    redeclare package Medium = Medium, p=100000);
equation
  connect(boundary1.ports[1], pipe1.port_a);
  connect(pipe1.port_b, pipe2.port_a);
  connect(pipe1.port_b, pipe3.port_a);
  connect(pipe2.port_b, pipe4.port_a);
  connect(pipe3.port_b, pipe4.port_a);
  connect(pipe4.port_b, boundary4.ports[1]);
end FluidPatternIa_HTRedeclareOnly;


// ==========================================================================
// Test Ib: H + FixedHeatFlow cross-ref only (no HT redeclare on pipe2).
// pipe2 uses default HeatTransfer but we still have FixedHeatFlow[pipe2.nNodes].
// If Ib overflows, the cross-component FixedHeatFlow is enough.
// ==========================================================================

model FluidPatternIb_FixedHeatFlowOnly
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
    use_HeatTransfer = true,
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
  Modelica.Fluid.Sources.Boundary_pT boundary1(nPorts=1,
    redeclare package Medium = Medium, p=150000);
  Modelica.Fluid.Sources.Boundary_pT boundary4(nPorts=1,
    redeclare package Medium = Medium, p=100000);
  Modelica.Thermal.HeatTransfer.Sources.FixedHeatFlow[pipe2.nNodes] heat2(
    Q_flow = 200 * pipe2.dxs);
equation
  connect(boundary1.ports[1], pipe1.port_a);
  connect(pipe1.port_b, pipe2.port_a);
  connect(pipe1.port_b, pipe3.port_a);
  connect(pipe2.port_b, pipe4.port_a);
  connect(pipe3.port_b, pipe4.port_a);
  connect(pipe4.port_b, boundary4.ports[1]);
  connect(heat2.port, pipe2.heatPorts);
end FluidPatternIb_FixedHeatFlowOnly;
