// Minimal test for cross-component parameter references in array dimensions.
// Reproduces the pattern from Modelica.Fluid.Examples.BranchingDynamicPipes:
//   FixedHeatFlow[pipe2.nNodes] heat2(Q_flow = 200 * pipe2.dxs)
// where one component's array size depends on another component's parameter.

model InnerComponent
  parameter Integer n = 3;
  parameter Real[n] weights = ones(n) / n;
  Real[n] x;
equation
  for i in 1:n loop
    x[i] = weights[i] * time;
  end for;
end InnerComponent;

model CrossComponentArrayDim
  InnerComponent comp(n = 5);
  Real[comp.n] y(each start = 0);
  parameter Real[comp.n] scale = 2 * comp.weights;
equation
  for i in 1:comp.n loop
    der(y[i]) = scale[i] * comp.x[i];
  end for;
end CrossComponentArrayDim;
