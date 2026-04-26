// Test: conditional component with parameter-dependent array dimension
// The conditional component only exists when useData=true, at which point
// data also has size [n]. Binding should type-check.
model ConditionalDimension2
  parameter Integer n = 3;
  parameter Boolean useData = true;
  record PortData
    Real diameter = 0.01;
    Real height = 0.0;
    Real zeta_in = 0.0;
    Real zeta_out = 0.0;
  end PortData;
  parameter PortData[if useData then n else 0] portsData;
  // Conditional component: only exists when useData and n > 0
  protected
    Real[n] portsData_diameter_internal = portsData.diameter if useData and n > 0;
    Real[n] portsData_height_internal = portsData.height if useData and n > 0;
    // Unconditional fallback (always exists)
    Real[n] portsData_diameter;
equation
  if useData then
    portsData_diameter = portsData_diameter_internal;
  else
    portsData_diameter = fill(0.0, n);
  end if;
end ConditionalDimension2;
