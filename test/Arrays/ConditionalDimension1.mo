// Test: parameter-dependent array dimension with if-expression
// This is the pattern used by Modelica.Fluid.Vessels.BaseClasses.PartialLumpedVessel
// for portsData: the array size depends on a boolean parameter.
model ConditionalDimension1
  parameter Integer n = 2;
  parameter Boolean useData = true;
  record Data
    Real diameter = 0.01;
    Real height = 0.0;
  end Data;
  parameter Data[if useData then n else 0] data;
  // When useData=true, data has size n, so data.diameter has size [n].
  // This internal variable should pick up size [n] from data.diameter.
  protected
    Real[n] data_diameter_internal = data.diameter;
equation
  // nothing needed, just check typing
end ConditionalDimension1;
