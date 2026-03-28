// Test: conditional dimension through inheritance (mirrors PartialLumpedVessel pattern)
// The partial model declares portsData with conditional dimension.
// The concrete model extends it and connects ports, setting nPorts.

record VesselPortsData
  Real diameter = 0.01;
  Real height = 0.0;
  Real zeta_in = 0.0;
  Real zeta_out = 0.0;
end VesselPortsData;

partial model PartialVessel
  parameter Integer nPorts = 0 "Number of ports";
  parameter Boolean use_portsData = true;
  parameter VesselPortsData[if use_portsData then nPorts else 0] portsData;
protected
  Real[nPorts] portsData_diameter_internal = portsData.diameter if use_portsData and nPorts > 0;
  Real[nPorts] portsData_diameter;
equation
  if use_portsData then
    portsData_diameter = portsData_diameter_internal;
  else
    portsData_diameter = fill(0.04, nPorts);
  end if;
end PartialVessel;

model ConditionalDimension4
  extends PartialVessel(nPorts = 2);
end ConditionalDimension4;
