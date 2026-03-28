// ==========================================================================
// Test 1: Deep extends chain
// Tests stack depth with MSL-like inheritance depth (10 levels).
// ==========================================================================

model Base1
  Real x;
equation
  x = time;
end Base1;

model Derived1 extends Base1; end Derived1;
model Derived2 extends Derived1; end Derived2;
model Derived3 extends Derived2; end Derived3;
model Derived4 extends Derived3; end Derived4;
model Derived5 extends Derived4; end Derived5;
model Derived6 extends Derived5; end Derived6;
model Derived7 extends Derived6; end Derived7;
model Derived8 extends Derived7; end Derived8;
model Derived9 extends Derived8; end Derived9;
model Derived10 extends Derived9; end Derived10;

model DeepExtendsChain
  Derived10 comp;
end DeepExtendsChain;


// ==========================================================================
// Test 2: Replaceable model with redeclare
// The BranchingDynamicPipes pattern: component has a replaceable model
// that is redeclared at instantiation site.
// ==========================================================================

model HeatTransferBase
  parameter Integer n;
  Real[n] Q;
equation
  for i in 1:n loop
    Q[i] = 0;
  end for;
end HeatTransferBase;

model AdvancedHeatTransfer
  extends HeatTransferBase;
end AdvancedHeatTransfer;

model Pipe
  parameter Integer nNodes = 5;
  parameter Real[nNodes] dxs = ones(nNodes) / nNodes;
  replaceable model HeatTransfer = HeatTransferBase
    constrainedby HeatTransferBase;
  HeatTransfer ht(n = nNodes);
  Real[nNodes] T;
equation
  for i in 1:nNodes loop
    der(T[i]) = ht.Q[i];
  end for;
end Pipe;

model RedeclareModelTest
  Pipe p1(nNodes = 3);
  Pipe p2(nNodes = 4, redeclare model HeatTransfer = AdvancedHeatTransfer);
end RedeclareModelTest;


// ==========================================================================
// Test 3: Cross-component parameter references in array size and modifiers
// Exact pattern from BranchingDynamicPipes:
//   FixedHeatFlow[pipe2.nNodes] heat2(Q_flow = 200 * pipe2.dxs)
// ==========================================================================

model Source
  parameter Integer n;
  parameter Real[n] scale;
  Real[n] y;
equation
  for i in 1:n loop
    y[i] = scale[i] * time;
  end for;
end Source;

model CrossComponentRef
  Pipe pipe1(nNodes = 3);
  Source src(n = pipe1.nNodes, scale = 2 * pipe1.dxs);
end CrossComponentRef;


// ==========================================================================
// Test 4: Derived type chain (type aliases)
// Tests the EXPANDED_DERIVED path in instClassDef which does NOT
// increment instLevel.
// ==========================================================================

type Voltage = Real(unit = "V");
type EMF = Voltage;
type MotorVoltage = EMF;

model DerivedTypeChain
  MotorVoltage v;
equation
  v = time;
end DerivedTypeChain;


// ==========================================================================
// Test 5: Nested replaceable with medium-like pattern
// Mimics Fluid's pattern: replaceable package Medium propagated through
// multiple component levels, with sub-records accessed via Medium.
// ==========================================================================

record MediumState
  Real p;
  Real T;
end MediumState;

partial model PartialMedium
  constant Integer nX = 1;
  replaceable record ThermodynamicState = MediumState;
end PartialMedium;

model SimpleMedium
  extends PartialMedium;
end SimpleMedium;

model FluidPort
  replaceable model Medium = SimpleMedium constrainedby PartialMedium;
  Medium.ThermodynamicState state;
end FluidPort;

model FluidComponent
  replaceable model Medium = SimpleMedium constrainedby PartialMedium;
  FluidPort port_a(redeclare model Medium = Medium);
  FluidPort port_b(redeclare model Medium = Medium);
  Medium.ThermodynamicState state;
end FluidComponent;

model NestedRedeclareTest
  FluidComponent comp(redeclare model Medium = SimpleMedium);
end NestedRedeclareTest;


// ==========================================================================
// Test 6: Inner/outer combined with replaceable model
// ==========================================================================

model SystemDefaults
  parameter Real g = 9.81;
end SystemDefaults;

model ComponentWithOuter
  outer SystemDefaults system;
  Real y;
equation
  y = system.g * time;
end ComponentWithOuter;

model InnerOuterWithRedeclare
  inner SystemDefaults system(g = 10.0);
  Pipe pipe1(nNodes = 2, redeclare model HeatTransfer = AdvancedHeatTransfer);
  ComponentWithOuter sub;
end InnerOuterWithRedeclare;


// ==========================================================================
// Test 7: Deep hierarchy with replaceable package propagation
// Mimics MSL Fluid: PartialTwoPort -> PartialTwoPortFlow -> DynamicPipe
// Each level propagates "redeclare package Medium" down.
// ==========================================================================

package WaterMedium
  constant Integer nX = 1;
  record State
    Real p;
    Real h;
  end State;
end WaterMedium;

package AirMedium
  constant Integer nX = 2;
  record State
    Real p;
    Real T;
  end State;
end AirMedium;

model PartialComponent
  replaceable package Medium = WaterMedium;
  parameter Integer n = 1;
end PartialComponent;

model PartialTwoPort
  extends PartialComponent;
  Real port_a_p;
  Real port_b_p;
equation
  port_a_p = 1e5;
  port_b_p = 1e5;
end PartialTwoPort;

model PartialTwoPortFlow
  extends PartialTwoPort;
  parameter Integer nNodes = 3;
  Real[nNodes] m_flow;
equation
  for i in 1:nNodes loop
    m_flow[i] = 0;
  end for;
end PartialTwoPortFlow;

model MyDynamicPipe
  extends PartialTwoPortFlow;
  replaceable model HeatTransfer = HeatTransferBase
    constrainedby HeatTransferBase;
  parameter Boolean use_HeatTransfer = false;
  parameter Real[nNodes] dxs = ones(nNodes) / nNodes;
  HeatTransfer heatTransfer(n = nNodes);
  Real[nNodes] T(each start = 293.15);
equation
  for i in 1:nNodes loop
    der(T[i]) = heatTransfer.Q[i];
  end for;
end MyDynamicPipe;

model DeepHierarchyRedeclare
  replaceable package Medium = AirMedium;
  MyDynamicPipe pipe1(
    redeclare package Medium = Medium,
    nNodes = 3);
  MyDynamicPipe pipe2(
    redeclare package Medium = Medium,
    nNodes = 5,
    use_HeatTransfer = true,
    redeclare model HeatTransfer = AdvancedHeatTransfer);
  Source heat(n = pipe2.nNodes, scale = 200 * pipe2.dxs);
end DeepHierarchyRedeclare;


// ==========================================================================
// Test 8: Multiple redeclares on the same component (package + model)
// The BranchingDynamicPipes pattern: a component has both
// "redeclare package Medium" and "redeclare model HeatTransfer"
// applied simultaneously.
// ==========================================================================

model PipeWithMediumHT
  extends PartialTwoPortFlow;
  replaceable package Medium = WaterMedium;
  replaceable model HeatTransfer = HeatTransferBase
    constrainedby HeatTransferBase;
  parameter Real[nNodes] dxs = ones(nNodes) / nNodes;
  HeatTransfer ht(n = nNodes);
  Real[nNodes] T(each start = 293.15);
equation
  for i in 1:nNodes loop
    der(T[i]) = ht.Q[i];
  end for;
end PipeWithMediumHT;

model DualRedeclareTest
  PipeWithMediumHT pipe(
    redeclare package Medium = AirMedium,
    nNodes = 4,
    redeclare model HeatTransfer = AdvancedHeatTransfer);
end DualRedeclareTest;


// ==========================================================================
// Test 9: Multiple pipes with branching connections and heat source array
// Closest structural match to BranchingDynamicPipes.
// ==========================================================================

model BranchingPipeTest
  replaceable package Medium = AirMedium;
  inner SystemDefaults system;
  MyDynamicPipe pipe1(
    redeclare package Medium = Medium,
    nNodes = 5);
  MyDynamicPipe pipe2(
    redeclare package Medium = Medium,
    nNodes = 5,
    use_HeatTransfer = true,
    redeclare model HeatTransfer = AdvancedHeatTransfer);
  MyDynamicPipe pipe3(
    redeclare package Medium = Medium,
    nNodes = 5);
  MyDynamicPipe pipe4(
    redeclare package Medium = Medium,
    nNodes = 5);
  Source heat2(n = pipe2.nNodes, scale = 200 * pipe2.dxs);
end BranchingPipeTest;
